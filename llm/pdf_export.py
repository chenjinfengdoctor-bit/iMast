"""
PDF export module for iMast.
Uses Playwright to render the jamovi analysis results and export as PDF.

Strategy:
1. Load the main app page to find the results iframe URL
2. Open the iframe URL directly in a new page (Frame has no pdf() method)
3. Wait for analysis results to render
4. Export the page as PDF
"""
import os
import asyncio
import time

_OUTPUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "exports")
os.makedirs(_OUTPUT_DIR, exist_ok=True)


async def export_results_pdf(url: str, iid: str, filename: str = "") -> dict:
    """
    Export the current iMast analysis results to PDF.

    Args:
        url: Base URL like http://127.0.0.1:41337
        iid: Instance ID
        filename: Output filename (without .pdf).

    Returns:
        dict with success, filepath, and message.
    """
    try:
        from playwright.async_api import async_playwright
    except ImportError:
        return {"success": False, "filepath": None, "message": "Playwright not installed."}

    if not filename:
        filename = f"iMast_report_{int(time.time())}"
    if not filename.endswith(".pdf"):
        filename += ".pdf"
    filepath = os.path.join(_OUTPUT_DIR, filename)

    console_log = []
    page_errors = []
    text_len = 0
    iframe_url = None

    try:
        async with async_playwright() as p:
            browser = await p.chromium.launch(
                headless=True,
                args=['--no-sandbox', '--disable-gpu', '--disable-dev-shm-usage']
            )
            context = await browser.new_context(
                viewport={"width": 1920, "height": 1080},
            )

            # Step 1: Load main page to find results iframe URL
            page = await context.new_page()
            page.on("console", lambda msg: console_log.append(f"{msg.type}: {msg.text}"))
            page.on("pageerror", lambda err: page_errors.append(str(err)))

            page_url = f"{url}/{iid}/"
            print(f"[pdf_export] Navigating to {page_url}")
            await page.goto(page_url, wait_until="domcontentloaded", timeout=30000)
            await page.wait_for_timeout(8000)

            # Find results iframe URL
            for frame in page.frames:
                if "/rv/" in frame.url and frame != page.main_frame:
                    iframe_url = frame.url
                    print(f"[pdf_export] Found results iframe: {iframe_url}")
                    break

            if iframe_url is None:
                # Try clicking "All" to switch to results view
                print("[pdf_export] No iframe found, trying 'All'...")
                try:
                    all_labels = await page.get_by_text("All", exact=True).all()
                    for label in all_labels:
                        cls = await label.get_attribute("class") or ""
                        if "tree-node" in cls:
                            await label.click()
                            await page.wait_for_timeout(5000)
                            break
                except Exception as e:
                    console_log.append(f"Click All failed: {e}")

                for frame in page.frames:
                    if "/rv/" in frame.url and frame != page.main_frame:
                        iframe_url = frame.url
                        print(f"[pdf_export] Found iframe after click: {iframe_url}")
                        break

            await page.close()

            # Step 2: Open iframe URL directly in a new page (Page has pdf(), Frame does not)
            if iframe_url:
                print(f"[pdf_export] Opening iframe directly: {iframe_url}")
                result_page = await context.new_page()
                result_page.on("console", lambda msg: console_log.append(f"{msg.type}: {msg.text}"))
                result_page.on("pageerror", lambda err: page_errors.append(str(err)))

                await result_page.goto(iframe_url, wait_until="domcontentloaded", timeout=30000)
                await result_page.wait_for_timeout(6000)
            else:
                # Fallback: use main page
                print("[pdf_export] No iframe URL found, using main page")
                result_page = await context.new_page()
                await result_page.goto(page_url, wait_until="domcontentloaded", timeout=30000)
                await result_page.wait_for_timeout(8000)

            # Step 3: Wait for result content
            result_selectors = [
                "[class*='rich-text']", "[class*='jmv-results']",
                "[class*='results-view']", "math", "table",
                "h1", "h2", "h3", "[class*='jmv']",
            ]
            found_selector = None
            for sel in result_selectors:
                try:
                    await result_page.wait_for_selector(sel, timeout=5000, state="visible")
                    found_selector = sel
                    print(f"[pdf_export] Found selector: {sel}")
                    break
                except Exception:
                    continue

            await result_page.wait_for_timeout(3000)

            # Get text for verification
            try:
                page_text = await result_page.inner_text("body")
                text_len = len(page_text.strip())
            except Exception:
                text_len = 0
            print(f"[pdf_export] Text length: {text_len}")

            # Step 4: Export PDF (always on Page object, never Frame)
            print(f"[pdf_export] Exporting PDF to {filepath}")
            await result_page.pdf(
                path=filepath,
                format="A4",
                print_background=True,
                margin={"top": "15mm", "bottom": "15mm", "left": "12mm", "right": "12mm"},
            )
            await browser.close()

        pdf_size = os.path.getsize(filepath) if os.path.exists(filepath) else 0
        debug = f"iframe={iframe_url is not None}, selector={found_selector}, text={text_len}, console={len(console_log)}, errors={len(page_errors)}"

        if pdf_size > 2000 and text_len > 30:
            return {
                "success": True,
                "filepath": filepath,
                "size_bytes": pdf_size,
                "message": f"PDF exported successfully ({pdf_size} bytes). {debug}",
            }
        else:
            err_detail = ""
            if page_errors:
                err_detail += f" Errors: {'; '.join(page_errors[:3])}"
            return {
                "success": False,
                "filepath": filepath,
                "message": f"PDF may be empty (size={pdf_size}, text={text_len}). {debug}{err_detail}",
            }
    except Exception as e:
        import traceback
        return {
            "success": False,
            "filepath": filepath,
            "message": f"PDF export failed: {str(e)}\n{traceback.format_exc()}",
        }


def get_output_dir() -> str:
    return _OUTPUT_DIR


if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 3:
        result = asyncio.run(export_results_pdf(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else ""))
        print(result)
    else:
        print("Usage: python pdf_export.py <url> <iid> [filename]")
