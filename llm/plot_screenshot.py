"""
Plot screenshot via UI automation for iMast.

Strategy (following pdf_export_ui.py which is known to work):
1. Load the main instance page
2. Dismiss overlays and activate results view (click "All")
3. Wait for the /rv/ result frames to render
4. Wait for a real plot to render (jamovi draws plots as a CSS background-image
   on a .jmv-results-image-image div, NOT as an <svg>/<img>/<canvas> element)
5. Pick the newest plot (highest result-item frame index / last in DOM) and
   screenshot ONLY that div's bounding box -> tight crop of chart + axes + labels
6. Fallback: generic svg/canvas/img, then the results content area

Notes on this jamovi version's results DOM:
- Each result item lives in its own sandboxed /rv/<iid>/<N>/ iframe (N = item index).
- The newest analysis / plot has the largest frame index.
- A plot item is:  .jmv-results-image  >  .jmv-results-image-image
  where the inner div has style="background-image: url(...svg); width: W; height: H".
- The inner div's box (W x H) tightly wraps the chart with no title bar or chrome.
"""
import os
import re
import asyncio
import time

_OUTPUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "exports")
os.makedirs(_OUTPUT_DIR, exist_ok=True)

# Minimum on-screen size (px) for an element to be considered a real plot.
_MIN_PLOT_W = 80
_MIN_PLOT_H = 80


async def _dismiss_overlays(page):
    """Remove infobox overlays and click away error dialogs."""
    await page.evaluate("""(function() {
        document.querySelectorAll('jmv-infobox').forEach(function(el){ el.remove(); });
        document.querySelectorAll('.el-message-box, .el-overlay-message-box').forEach(function(box){
            var btns = box.querySelectorAll('button');
            for (var i = 0; i < btns.length; i++) {
                var t = (btns[i].textContent || '').trim().toLowerCase();
                if (t === 'close' || t === 'ok' || t === '确定' || t === '关闭' || t === '×') {
                    btns[i].click();
                    break;
                }
            }
        });
    })()""")


def _frame_index(url: str) -> int:
    """Extract the result-item index N from a /rv/<iid>/<N>/ frame URL."""
    m = re.search(r"/rv/[^/]+/(\d+)/?$", url)
    return int(m.group(1)) if m else 0


async def _collect_plot_candidates(page):
    """
    Scan every /rv/ frame for renderable plot elements.
    Returns a list of dicts sorted newest-first (highest frame index, then
    last-in-DOM). Each item: {frame, handle, kind, w, h, frame_index, fp}.
    `fp` is a stable fingerprint used to distinguish newly rendered plots from
    ones that already existed before the current analysis ran.
    """
    found = []
    for frame in page.frames:
        if "/rv/" not in frame.url or frame == page.main_frame:
            continue
        idx = _frame_index(frame.url)

        # 1) Primary: jamovi's CSS background-image plot div.
        try:
            image_handles = await frame.query_selector_all(".jmv-results-image-image")
        except Exception:
            image_handles = []
        for n, h in enumerate(image_handles):
            try:
                bi = await h.evaluate("e => getComputedStyle(e).backgroundImage")
                box = await h.bounding_box()
            except Exception:
                continue
            if bi and bi != "none" and box and box["width"] >= _MIN_PLOT_W and box["height"] >= _MIN_PLOT_H:
                found.append({
                    "frame": frame, "handle": h, "kind": "bgimage",
                    "w": box["width"], "h": box["height"], "frame_index": idx,
                    "fp": f"{frame.url}|bg|{bi}",
                })

        # 2) Secondary: actual svg / canvas / img elements (older or other modules).
        for sel in ["svg", "canvas", "img"]:
            try:
                handles = await frame.query_selector_all(sel)
            except Exception:
                handles = []
            for h in handles:
                try:
                    box = await h.bounding_box()
                    src = await h.evaluate(
                        "e => (e.tagName==='IMG' ? e.src : '')"
                    )
                except Exception:
                    continue
                if box and box["width"] >= _MIN_PLOT_W and box["height"] >= _MIN_PLOT_H:
                    found.append({
                        "frame": frame, "handle": h, "kind": sel,
                        "w": box["width"], "h": box["height"], "frame_index": idx,
                        "fp": f"{frame.url}|{sel}|{src}",
                    })

    # Newest first: highest frame index, then later-in-DOM (already appended in
    # DOM order, so stable sort keeps the last one first).
    found.sort(key=lambda c: c["frame_index"], reverse=True)
    return found


async def _wait_and_pick_plot(page, timeout_s: int = 45):
    """
    Wait for the plot produced by the most recent analysis to render.

    Existing plots (from earlier analyses) are fingerprinted first; we then wait
    for a NEW plot fingerprint to appear and become stable. This avoids
    re-capturing a stale plot when the results view already contains older
    figures. Falls back to the newest existing plot if nothing new appears.
    """
    # Snapshot plots that already exist (e.g. from previous analyses).
    existing = {c["fp"] for c in await _collect_plot_candidates(page)}

    deadline = time.time() + timeout_s
    last_sig = None
    while time.time() < deadline:
        candidates = await _collect_plot_candidates(page)
        new_ones = [c for c in candidates if c["fp"] not in existing]
        pool = new_ones if new_ones else candidates
        if pool:
            cand = pool[0]
            try:
                box = await cand["handle"].bounding_box()
            except Exception:
                box = None
            sig = (cand["fp"], box)
            if last_sig == sig:
                return cand
            last_sig = sig
        else:
            last_sig = None
        await asyncio.sleep(1.0)
    return None


async def _capture_via_pdf(page, target_filepath: str) -> bool:
    """
    PDF export fallback for interactive plots (e.g. scatr scatter plots) that
    don't render as CSS background-image in headless mode.

    Strategy:
    1. Export the results view to PDF via page.pdf()
    2. Open PDF with PyMuPDF and render pages to PNG
    3. Save the page containing the plot
    """
    try:
        import fitz  # PyMuPDF
    except ImportError:
        print("[plot_ui] PyMuPDF not available, skipping PDF fallback")
        return False

    pdf_path = target_filepath.replace(".png", "_temp.pdf")
    try:
        # Export to PDF
        await page.pdf(
            path=pdf_path,
            format="A4",
            print_background=True,
            margin={"top": "10mm", "bottom": "10mm", "left": "10mm", "right": "10mm"}
        )
        print(f"[plot_ui] PDF exported: {os.path.getsize(pdf_path)} bytes")

        # Open PDF and find the page with the plot
        doc = fitz.open(pdf_path)
        best_page = None
        best_area = 0
        for page_num in range(len(doc)):
            page = doc[page_num]
            # Get images on this page
            images = page.get_images(full=True)
            if images:
                area = sum(img[2] * img[3] for img in images)
                if area > best_area:
                    best_area = area
                    best_page = page_num

        if best_page is None:
            # No embedded images, render the page with most content
            best_page = 0
            max_text = 0
            for page_num in range(len(doc)):
                text_len = len(doc[page_num].get_text())
                if text_len > max_text:
                    max_text = text_len
                    best_page = page_num

        # Render the best page to PNG
        page = doc[best_page]
        mat = fitz.Matrix(2, 2)  # 2x zoom for better quality
        pix = page.get_pixmap(matrix=mat)
        pix.save(target_filepath)
        doc.close()
        print(f"[plot_ui] PDF fallback saved page {best_page} as PNG")

        # Clean up temp PDF
        if os.path.exists(pdf_path):
            os.remove(pdf_path)

        return True
    except Exception as e:
        print(f"[plot_ui] PDF fallback failed: {e}")
        if os.path.exists(pdf_path):
            try:
                os.remove(pdf_path)
            except Exception:
                pass
        return False


async def capture_plot_via_ui(url: str, iid: str, filename: str = "") -> dict:
    """
    Open the iMast results view and screenshot the targeted plot element.
    """
    try:
        from playwright.async_api import async_playwright
    except ImportError:
        return {"success": False, "filepath": None, "message": "Playwright not installed."}

    if not filename:
        filename = f"iMast_plot_{int(time.time())}"
    if not filename.endswith(".png"):
        filename += ".png"
    target_filepath = os.path.join(_OUTPUT_DIR, filename)

    try:
        async with async_playwright() as p:
            browser = await p.chromium.launch(
                headless=True,
                args=['--no-sandbox', '--disable-gpu', '--disable-dev-shm-usage']
            )
            context = await browser.new_context(
                viewport={"width": 1600, "height": 1200},
            )
            context.set_default_timeout(30000)
            page = await context.new_page()

            # Step 1: Load main page
            page_url = f"{url}/{iid}/"
            print(f"[plot_ui] Navigating to {page_url}")
            await page.goto(page_url, wait_until="domcontentloaded", timeout=30000)
            await page.wait_for_timeout(8000)
            await _dismiss_overlays(page)

            # Step 2: Expand Analyses tree and click "All"
            await page.evaluate("""(function() {
                var nodes = document.querySelectorAll('.el-tree-node');
                for (var i = 0; i < nodes.length; i++) {
                    var label = nodes[i].querySelector('.el-tree-node__label');
                    if (label && label.textContent.trim() === 'Analyses') {
                        var icon = nodes[i].querySelector('.el-tree-node__expand-icon');
                        if (icon) icon.click();
                        break;
                    }
                }
            })()""")
            await page.wait_for_timeout(2000)
            await page.evaluate("""(function() {
                var nodes = document.querySelectorAll('.el-tree-node__label, .el-tree-node, [class*=tree-node]');
                for (var i = 0; i < nodes.length; i++) {
                    if (nodes[i].textContent.trim() === 'All') {
                        nodes[i].click();
                        return true;
                    }
                }
                return false;
            })()""")
            await page.wait_for_timeout(3000)
            await _dismiss_overlays(page)

            # Step 3: Wait until at least one /rv/ result frame has content.
            deadline = time.time() + 50
            frame_ready = False
            while time.time() < deadline:
                for frame in page.frames:
                    try:
                        if "/rv/" in frame.url and frame != page.main_frame:
                            body_text = await frame.evaluate(
                                "document.body ? document.body.innerText.trim() : ''"
                            )
                            if len(body_text) > 5:
                                frame_ready = True
                                break
                    except Exception:
                        continue
                if frame_ready:
                    break
                await asyncio.sleep(1.0)

            # Step 4: Wait for the plot to render across ALL rv frames, then capture.
            plot_found = False
            # Nudge lazy loading: scroll main page to bottom a few times.
            for _ in range(3):
                try:
                    await page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
                except Exception:
                    pass
                await asyncio.sleep(1.0)

            chosen = await _wait_and_pick_plot(page, timeout_s=45)
            if chosen:
                handle = chosen["handle"]
                try:
                    await handle.scroll_into_view_if_needed()
                    await asyncio.sleep(0.8)
                except Exception:
                    pass
                await handle.screenshot(path=target_filepath)
                plot_found = True
                print(f"[plot_ui] Captured {chosen['kind']} "
                      f"({chosen['w']:.0f}x{chosen['h']:.0f}) from frame "
                      f"index {chosen['frame_index']}")

            # Step 5: If no precise plot found, try PDF export fallback first
            # (especially for interactive plots like scatr scatter plots)
            if not plot_found:
                print("[plot_ui] No precise plot found, trying PDF export fallback")
                pdf_ok = await _capture_via_pdf(page, target_filepath)
                if pdf_ok and os.path.exists(target_filepath):
                    pdf_size = os.path.getsize(target_filepath)
                    if pdf_size > 3000:
                        plot_found = True
                        print(f"[plot_ui] PDF fallback succeeded ({pdf_size} bytes)")

            # Step 6: Last resort - plain viewport screenshot
            if not plot_found:
                try:
                    await page.screenshot(path=target_filepath, full_page=False)
                    plot_found = True
                    print("[plot_ui] Used viewport screenshot fallback")
                except Exception as e:
                    print(f"[plot_ui] Screenshot fallback failed: {e}")

            await browser.close()

        if plot_found and os.path.exists(target_filepath):
            size = os.path.getsize(target_filepath)
            if size > 3000:
                return {
                    "success": True,
                    "filepath": target_filepath,
                    "size_bytes": size,
                    "message": f"Plot captured ({size} bytes)."
                }
            # Screenshot too small - try PDF fallback for interactive plots
            print(f"[plot_ui] Screenshot too small ({size} bytes), trying PDF fallback")
            pdf_ok = await _capture_via_pdf(page, target_filepath)
            if pdf_ok and os.path.exists(target_filepath):
                pdf_size = os.path.getsize(target_filepath)
                if pdf_size > 3000:
                    return {
                        "success": True,
                        "filepath": target_filepath,
                        "size_bytes": pdf_size,
                        "message": f"Plot captured via PDF export ({pdf_size} bytes)."
                    }
            return {
                "success": False,
                "filepath": target_filepath,
                "message": f"Screenshot too small ({size} bytes), PDF fallback also failed."
            }
        # No plot found - try PDF fallback
        print("[plot_ui] No plot found, trying PDF fallback")
        pdf_ok = await _capture_via_pdf(page, target_filepath)
        if pdf_ok and os.path.exists(target_filepath):
            pdf_size = os.path.getsize(target_filepath)
            if pdf_size > 3000:
                return {
                    "success": True,
                    "filepath": target_filepath,
                    "size_bytes": pdf_size,
                    "message": f"Plot captured via PDF export ({pdf_size} bytes)."
                }
        return {
            "success": False,
            "filepath": target_filepath,
            "message": "No plot found in results view after waiting. The plot may not have rendered."
        }
    except Exception as e:
        import traceback
        return {
            "success": False,
            "filepath": target_filepath,
            "message": f"Plot screenshot failed: {str(e)}\n{traceback.format_exc()}",
        }


def get_output_dir() -> str:
    return _OUTPUT_DIR


if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 3:
        result = asyncio.run(capture_plot_via_ui(sys.argv[1], sys.argv[2],
                                                   sys.argv[3] if len(sys.argv) > 3 else ""))
        print(result)
    else:
        print("Usage: python plot_screenshot.py <url> <iid> [filename]")
