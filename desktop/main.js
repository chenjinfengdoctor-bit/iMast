const { app, BrowserWindow, Menu, shell, ipcMain } = require('electron');
const path = require('path');
const { exec, spawn } = require('child_process');
const http = require('http');

// iMast backend URL (Docker container)
const IMAST_URL = 'http://127.0.0.1:41337';
const DOCKER_IMAGE = 'jamovi/jamovi:2.7-imast';
const CONTAINER_NAME = 'jamovi';
const DOCKER_DESKTOP_EXE = 'C:\\Program Files\\Docker\\Docker\\Docker Desktop.exe';

let mainWindow = null;
let helpWindows = [];

function openHelpPage(fileName, title) {
  const win = new BrowserWindow({
    width: 1000,
    height: 700,
    parent: mainWindow || undefined,
    title: title,
    icon: path.join(__dirname, 'assets', 'icon.png'),
    autoHideMenuBar: true,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      webSecurity: true
    }
  });
  win.loadFile(path.join(__dirname, 'help', fileName));
  win.on('closed', () => {
    helpWindows = helpWindows.filter(w => w !== win);
  });
  helpWindows.push(win);
  return win;
}

function showAboutDialog() {
  const { dialog } = require('electron');
  dialog.showMessageBox(mainWindow, {
    type: 'info',
    title: 'About iMast',
    message: 'iMast Statistical Analysis Software',
    detail:
      'Version 2.7.2\n' +
      'Based on jamovi 2.7.0.0\n' +
      '\n' +
      '153 statistical methods across 6 categories\n' +
      'IVD-focused statistical software\n' +
      '\n' +
      'Architecture: Docker backend + Electron desktop shell\n' +
      '\n' +
      'Key features:\n' +
      '  \u2022 Professional IVD menu order (CLSI EP series, YY/T 1709)\n' +
      '  \u2022 Precision, accuracy, linearity & commutability evaluation\n' +
      '  \u2022 Reference material characterization & uncertainty\n' +
      '  \u2022 Full inferential & regression toolkit\n' +
      '  \u2022 Integrated AI assistant (LLM) for result interpretation\n' +
      '  \u2022 One-click PDF report export',
    buttons: ['OK']
  });
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// Run a shell command; resolves with { ok, stdout, stderr } (never rejects).
function runCmd(cmd, timeoutMs) {
  return new Promise((resolve) => {
    exec(cmd, { timeout: timeoutMs || 30000, windowsHide: true }, (error, stdout, stderr) => {
      resolve({
        ok: !error,
        stdout: (stdout || '').trim(),
        stderr: (stderr || '').trim()
      });
    });
  });
}

// Push a status update to the loading screen renderer.
function sendStatus(status, detail) {
  if (mainWindow && !mainWindow.isDestroyed()) {
    mainWindow.webContents.send('loading-status', { status, detail });
  }
}

// Push an error to the loading screen renderer.
function sendError(message) {
  if (mainWindow && !mainWindow.isDestroyed()) {
    mainWindow.webContents.send('loading-error', { message });
  }
}

// Probe the backend over HTTP; resolves true on any response.
function probeBackend(url) {
  return new Promise((resolve) => {
    const req = http.get(url, { timeout: 8000 }, () => resolve(true));
    req.on('error', () => resolve(false));
    req.on('timeout', () => { req.destroy(); resolve(false); });
    req.end();
  });
}

// ---------------------------------------------------------------------------
// Backend readiness pipeline
// ---------------------------------------------------------------------------

async function ensureDockerDaemon() {
  sendStatus('checking_docker', 'Checking Docker daemon...');
  let r = await runCmd('docker info --format "{{.ServerVersion}}"', 10000);
  if (r.ok) return;

  // Docker daemon not running -> launch Docker Desktop
  sendStatus('starting_docker', 'Starting Docker Desktop...');
  try {
    const child = spawn(DOCKER_DESKTOP_EXE, [], { detached: true, stdio: 'ignore' });
    child.unref();
  } catch (e) {
    throw new Error('Could not launch Docker Desktop automatically. Please start Docker Desktop manually and retry.');
  }

  // Poll until the daemon responds (up to 120s)
  const deadline = Date.now() + 120000;
  while (Date.now() < deadline) {
    await sleep(4000);
    r = await runCmd('docker info --format "{{.ServerVersion}}"', 10000);
    if (r.ok) return;
  }
  throw new Error('Docker Desktop did not become ready within 120 seconds. Please ensure Docker is running and retry.');
}

async function ensureImage() {
  sendStatus('checking_image', 'Checking iMast Docker image...');
  let r = await runCmd(`docker image inspect ${DOCKER_IMAGE} --format "{{.Id}}"`, 15000);
  if (r.ok) return;

  // Image missing -> look for the bundled tar
  sendStatus('loading_image', 'Loading iMast image from bundle (this may take a few minutes)...');
  const candidates = [
    path.join(__dirname, 'imast-image.tar'),
    path.join(process.resourcesPath || '', 'imast-image.tar'),
    path.join(process.resourcesPath || '', 'app', 'imast-image.tar')
  ];
  const tarPath = candidates.find((p) => {
    try { return require('fs').existsSync(p); } catch (e) { return false; }
  });
  if (!tarPath) {
    throw new Error(`Docker image "${DOCKER_IMAGE}" not found and no bundled imast-image.tar was located. Please reinstall iMast.`);
  }

  r = await runCmd(`docker load -i "${tarPath}"`, 600000);
  if (!r.ok) {
    throw new Error(`Failed to load Docker image from bundle. ${r.stderr || r.stdout || 'Unknown error'}`);
  }
}

async function ensureContainer() {
  sendStatus('checking_container', 'Checking iMast container...');
  let r = await runCmd(`docker ps -a --filter name=${CONTAINER_NAME} --format "{{.Names}}"`, 10000);
  if (!r.ok || !r.stdout) {
    throw new Error(`Container "${CONTAINER_NAME}" does not exist. Please reinstall iMast so the container can be created.`);
  }

  // Already running?
  r = await runCmd(`docker inspect -f "{{.State.Running}}" ${CONTAINER_NAME}`, 10000);
  if (r.ok && r.stdout === 'true') return;

  sendStatus('starting_container', 'Starting iMast container...');
  r = await runCmd(`docker start ${CONTAINER_NAME}`, 30000);
  if (!r.ok) {
    throw new Error(`Failed to start container "${CONTAINER_NAME}". ${r.stderr || 'Unknown error'}`);
  }
}

async function waitForBackend() {
  sendStatus('connecting', 'Connecting to iMast backend...');
  const deadline = Date.now() + 120000;
  while (Date.now() < deadline) {
    if (await probeBackend(IMAST_URL)) return;
    await sleep(2000);
  }
  throw new Error('iMast backend did not respond within 120 seconds after the container started.');
}

async function startBackend() {
  try {
    await ensureDockerDaemon();
    await ensureImage();
    await ensureContainer();
    await waitForBackend();
    sendStatus('ready', 'Ready! Loading iMast...');
    await sleep(600);
    if (mainWindow && !mainWindow.isDestroyed()) {
      await mainWindow.loadURL(IMAST_URL);
    }
  } catch (e) {
    sendError((e && e.message) ? e.message : String(e));
  }
}

// ---------------------------------------------------------------------------
// Window
// ---------------------------------------------------------------------------

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    minWidth: 800,
    minHeight: 600,
    title: 'iMast Statistical Analysis Software',
    icon: path.join(__dirname, 'assets', 'icon.png'),
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      webSecurity: true,
      preload: path.join(__dirname, 'preload.js')
    },
    show: false
  });

  // Set window position and size
  mainWindow.setBounds({ x: 0, y: 0, width: 1600, height: 900 });
  mainWindow.show();
  mainWindow.focus();

  // Load the friendly loading screen first; the backend pipeline runs after it finishes.
  mainWindow.loadFile(path.join(__dirname, 'loading.html'));
  mainWindow.webContents.once('did-finish-load', () => {
    startBackend();
  });

  // Open external links in system browser
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });

  // Custom menu (all English)
  const template = [
    {
      label: 'File',
      submenu: [
        { label: 'New', accelerator: 'CmdOrCtrl+N', click: () => mainWindow.loadURL(IMAST_URL) },
        { label: 'Reload', accelerator: 'CmdOrCtrl+R', click: () => mainWindow.reload() },
        { type: 'separator' },
        { label: 'Quit', accelerator: 'CmdOrCtrl+Q', click: () => app.quit() }
      ]
    },
    {
      label: 'Edit',
      submenu: [
        { label: 'Undo', accelerator: 'CmdOrCtrl+Z', role: 'undo' },
        { label: 'Redo', accelerator: 'CmdOrCtrl+Y', role: 'redo' },
        { type: 'separator' },
        { label: 'Cut', accelerator: 'CmdOrCtrl+X', role: 'cut' },
        { label: 'Copy', accelerator: 'CmdOrCtrl+C', role: 'copy' },
        { label: 'Paste', accelerator: 'CmdOrCtrl+V', role: 'paste' },
        { label: 'Select All', accelerator: 'CmdOrCtrl+A', role: 'selectAll' }
      ]
    },
    {
      label: 'View',
      submenu: [
        { label: 'Zoom In', accelerator: 'CmdOrCtrl+=', role: 'zoomIn' },
        { label: 'Zoom Out', accelerator: 'CmdOrCtrl+-', role: 'zoomOut' },
        { label: 'Reset Zoom', accelerator: 'CmdOrCtrl+0', role: 'resetZoom' },
        { type: 'separator' },
        { label: 'Full Screen', accelerator: 'F11', role: 'togglefullscreen' },
        { label: 'Developer Tools', accelerator: 'F12', role: 'toggleDevTools' }
      ]
    },
    {
      label: 'Help',
      submenu: [
        { label: 'User Guide', accelerator: 'F1', click: () => openHelpPage('userguide.html', 'iMast User Guide') },
        { label: 'IVD Methods Documentation', click: () => openHelpPage('methods.html', 'iMast IVD Methods Documentation') },
        { label: 'Statistical Methods Genealogy', click: () => openHelpPage('genealogy.html', 'iMast Statistical Methods Genealogy') },
        { type: 'separator' },
        { label: 'About iMast', click: () => showAboutDialog() }
      ]
    }
  ];
  Menu.setApplicationMenu(Menu.buildFromTemplate(template));

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
}

// IPC: retry button on the error screen
ipcMain.on('retry-backend', () => {
  startBackend();
});

// Set user data directory to C drive to avoid F drive permission issues
app.setPath('userData', path.join(process.env.LOCALAPPDATA || process.env.APPDATA, 'iMast'));

app.whenReady().then(() => {
  createWindow();
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});
