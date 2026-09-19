const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('imastBridge', {
  onStatus: (cb) => ipcRenderer.on('loading-status', (_e, data) => cb(data)),
  onError: (cb) => ipcRenderer.on('loading-error', (_e, data) => cb(data)),
  retry: () => ipcRenderer.send('retry-backend')
});
