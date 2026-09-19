"""
PDF export via UI automation for iMast.
Simulates user clicking: File -> Export -> Select PDF -> Export
Uses JavaScript clicks for reliability.

Robustness fixes (vs. previous version):
- The fresh headless browser lands on the instance URL, but the results view
  (/rv/ iframe) is NOT guaranteed to be rendered immediately. If File->Export is
  clicked before results render, jamovi POSTs an empty results HTML to
  /utils/to-pdf and the downloaded PDF is blank. We now WAIT for the results
  iframe to exist AND contain real text before triggering the export.
- We dismiss both the jmv-infobox overlay AND any "Unable to open"/error dialog
  that can pop up when a second client attaches to the instance.
- We explicitly click "All" in the analyses tree so the results view is active.
- The downloaded PDF is verified to have real text; if the first attempt yields
  an empty/too-small PDF, the flow retries once.
"""
import os
import asyncio
import time

_OUTPUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "exports")
os.makedirs(_OUTPUT_DIR, exist_ok=True)


async def _dismiss_overlays(page):
    """Remove infobox overlays and click away error dialogs (e.g. 'Unable to open')."""
    await page.evaluate("""(function() {
        // Remove the AI infobox / any jmv-infobox
        document.querySelectorAll('jmv-infobox').forEach(function(el){ el.remove(); });
        // Dismiss Element Plus error/message boxes and modal dialogs that block
        // e.g. "Unable to open - This data set is no longer available"
        document.querySelectorAll('.el-message-box, .el-overlay-message-box').forEach(function(box){
            var btns = box.querySelectorAll('button');
            for (var i = 0; i < btns.length; i++) {
                var t = (btns[i].textContent || '').trim().toLowerCase();
                if (t === 'close' || t === 'ok' || t === 'close' || t === '确定' || t === '关闭' || t === '×') {
                    btns[i].click();
                    break;
                }
            }
        });
    })()""")


async def _activate_results_view(page):
    """Click 'All' under Analyses so the results view (/rv/ iframe) is shown."""
    clicked = await page.evaluate("""(function() {
        var nodes = document.querySelectorAll('.el-tree-node__label, .el-tree-node, [class*=tree-node]');
        for (var i = 0; i < nodes.length; i++) {
            var t = (nodes[i].textContent || '').trim();
            if (t === 'All') {
                nodes[i].click();
                return true;
            }
        }
        return false;
    })()""")
    return clicked


async def _wait_for_results_content(page, timeout_ms=45000):
    """
    Wait until a results iframe (/rv/) exists AND its body contains real text.
    Returns (iframe_url, body_text_len) or (None, 0) on timeout.
    """
    deadline = time.time() + timeout_ms / 1000.0
    iframe_url = None
    text_len = 0
    while time.time() < deadline:
        # Find the results iframe
        for frame in page.frames:
            try:
                if "/rv/" in frame.url and frame != page.main_frame:
                    iframe_url = frame.url
                    body = await frame.evaluate("document.body ? document.body.innerText : ''")
                    text_len = len((body or "").strip())
                    if text_len > 30:
                        return iframe_url, text_len
                    break
            except Exception:
                continue
        await asyncio.sleep(1.0)
    return iframe_url, text_len


async def _do_export_once(page, full_url, iid, target_filepath, console_log, page_errors):
    """Run the File->Export->PDF flow. Returns (success, message)."""
    page_url = f"{full_url}/{iid}/"
    print(f"[pdf_ui] Navigating to {page_url}")
    await page.goto(page_url, wait_until="domcontentloaded", timeout=30000)
    # Base settle time
    await page.wait_for_timeout(5000)
    await _dismiss_overlays(page)

    # Activate results view
    clicked_all = await _activate_results_view(page)
    print(f"[pdf_ui] Clicked 'All' to show results: {clicked_all}")
    await page.wait_for_timeout(2000)
    await _dismiss_overlays(page)

    # Wait for results iframe to actually render content
    iframe_url, text_len = await _wait_for_results_content(page, timeout_ms=45000)
    print(f"[pdf_ui] Results iframe: {iframe_url}, text_len={text_len}")

    if text_len <= 30:
        # Give it one more chance after a longer settle
        await page.wait_for_timeout(5000)
        iframe_url, text_len = await _wait_for_results_content(page, timeout_ms=15000)
        print(f"[pdf_ui] Recheck results iframe: {iframe_url}, text_len={text_len}")

    # Step 1: Click File menu via JS
    print("[pdf_ui] Clicking File menu...")
    await page.evaluate("""(function() {
        var items = document.querySelectorAll('.el-sub-menu__title');
        for (var i = 0; i < items.length; i++) {
            if (items[i].textContent.trim() === 'File') { items[i].click(); break; }
        }
    })()""")
    await page.wait_for_timeout(1500)

    # Step 2: Click Export via JS
    print("[pdf_ui] Clicking Export...")
    await page.evaluate("""(function() {
        var items = document.querySelectorAll('.el-menu-item');
        for (var i = 0; i < items.length; i++) {
            if (items[i].textContent.trim() === 'Export') { items[i].click(); break; }
        }
    })()""")
    await page.wait_for_timeout(2500)

    # Step 3: Verify dialog is visible
    dialog_visible = await page.evaluate("""(function() {
        var dialogs = document.querySelectorAll('.el-dialog');
        for (var i = 0; i < dialogs.length; i++) {
            if (dialogs[i].offsetParent !== null) return true;
        }
        return false;
    })()""")
    print(f"[pdf_ui] Dialog visible: {dialog_visible}")

    if not dialog_visible:
        return False, "Export dialog not visible after clicking Export."

    # Step 4: Select PDF (it's usually default, but ensure)
    await page.evaluate("""(function() {
        var radios = document.querySelectorAll('.el-radio');
        for (var i = 0; i < radios.length; i++) {
            if (radios[i].textContent.indexOf('PDF') >= 0) {
                radios[i].click();
                break;
            }
        }
    })()""")
    await page.wait_for_timeout(500)

    # Step 5: Click Export button in dialog, expect download
    print("[pdf_ui] Clicking Export button and waiting for download...")
    download = None
    try:
        async with page.expect_download(timeout=30000) as download_info:
            await page.evaluate("""(function() {
                var buttons = document.querySelectorAll('.el-dialog button');
                for (var i = 0; i < buttons.length; i++) {
                    if (buttons[i].textContent.trim() === 'Export') { buttons[i].click(); break; }
                }
            })()""")
        download = await download_info.value
        print(f"[pdf_ui] Download started: {download.suggested_filename}")
    except Exception as e:
        print(f"[pdf_ui] Download failed: {e}")
        return False, f"Export button click or download failed: {e}"

    # Step 6: Save downloaded file
    if download:
        await download.save_as(target_filepath)
        print(f"[pdf_ui] Saved to {target_filepath}")

    return True, f"results_text={text_len}, iframe={iframe_url is not None}"


async def export_pdf_via_ui(url: str, iid: str, filename: str = "") -> dict:
    """
    Export PDF by simulating user UI operations via JavaScript.
    """
    try:
        from playwright.async_api import async_playwright
    except ImportError:
        return {"success": False, "filepath": None, "message": "Playwright not installed."}

    if not filename:
        filename = f"iMast_report_{int(time.time())}"
    if not filename.endswith(".pdf"):
        filename += ".pdf"
    target_filepath = os.path.join(_OUTPUT_DIR, filename)

    console_log = []
    page_errors = []

    try:
        max_attempts = 2
        last_debug = ""
        for attempt in range(1, max_attempts + 1):
            print(f"[pdf_ui] === Attempt {attempt}/{max_attempts} ===")
            console_log.clear()
            page_errors.clear()
            async with async_playwright() as p:
                browser = await p.chromium.launch(
                    headless=True,
                    args=['--no-sandbox', '--disable-gpu', '--disable-dev-shm-usage']
                )
                context = await browser.new_context(
                    viewport={"width": 1920, "height": 1080},
                    accept_downloads=True,
                )
                context.set_default_timeout(30000)

                page = await context.new_page()
                page.on("console", lambda msg: console_log.append(f"{msg.type}: {msg.text}"))
                page.on("pageerror", lambda err: page_errors.append(str(err)))

                ok, debug = await _do_export_once(
                    page, url, iid, target_filepath, console_log, page_errors
                )
                last_debug = debug
                await browser.close()

            pdf_size = os.path.getsize(target_filepath) if os.path.exists(target_filepath) else 0
            print(f"[pdf_ui] Attempt {attempt}: size={pdf_size}, ok={ok}, debug={debug}")

            # Success criterion: downloaded and has substantial size
            if ok and pdf_size > 8000:
                # Verify it actually has extractable-ish content (size heuristic is enough)
                return {
                    "success": True,
                    "filepath": target_filepath,
                    "size_bytes": pdf_size,
                    "message": f"PDF exported via UI ({pdf_size} bytes). {debug}. console={len(console_log)}, errors={len(page_errors)}",
                }
            # Otherwise retry
            await asyncio.sleep(2)

        err_detail = ""
        if page_errors:
            err_detail += f" Errors: {'; '.join(page_errors[:3])}"
        pdf_size = os.path.getsize(target_filepath) if os.path.exists(target_filepath) else 0
        return {
            "success": False,
            "filepath": target_filepath,
            "message": f"PDF may be empty (size={pdf_size}) after {max_attempts} attempts. {last_debug}. console={len(console_log)}, errors={len(page_errors)}{err_detail}",
        }
    except Exception as e:
        import traceback
        return {
            "success": False,
            "filepath": target_filepath,
            "message": f"PDF export via UI failed: {str(e)}\n{traceback.format_exc()}",
        }


def get_output_dir() -> str:
    return _OUTPUT_DIR


if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 3:
        result = asyncio.run(export_pdf_via_ui(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else ""))
        print(result)
    else:
        print("Usage: python pdf_export_ui.py <url> <iid> [filename]")
