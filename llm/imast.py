from __future__ import annotations
import websockets
import asyncio
import json
import os
import typing
import requests as _requests
from pydantic import BaseModel


class AnalysisOption(BaseModel):
    key: str
    value: typing.Union[str, float, bool, int, AnalysisOptions]
    type: typing.Literal["s", "d", "o", "c"]


class AnalysisOptions(BaseModel):
    hasNames: bool
    options: list[AnalysisOption]


class AnalysisRequest(BaseModel):
    type: str = "ANALYSIS_CREATE_REQ"
    instance_id: str = ""
    analysis_name: str
    ns: str
    options: AnalysisOptions


class InstanceNotFoundError(Exception):
    pass


class NoDataError(Exception):
    pass


# Maximum wait for a single WebSocket reply. The bridge may silently fail to
# answer (e.g. unknown message type, instance already removed); without a timeout
# the client would block on recv() forever.
RECV_TIMEOUT = 30


class IMAST:
    ws_url: str
    raw_url: str
    url: str
    ak: str
    instance_id: str = None

    async def _recv(self, ws, timeout: float = RECV_TIMEOUT):
        """Receive one text frame with a timeout; raises asyncio.TimeoutError on no reply."""
        return await asyncio.wait_for(ws.recv(), timeout)

    def __init__(self, url: str):
        self.raw_url = url
        url, ak = url.split("/", 2)
        self.ak = ak.split("=")[-1]
        self.base_url = url  # e.g. "127.0.0.1:41337"
        self.url = f"{url}/llm/"
        self.ws_url = f"ws://{self.url}"

    async def _gii(self, ws):
        iid = self.instance_id
        if iid is None:
            iid = (await self._gai(ws))[0][1]
        return iid

    async def _gai(self, ws):
        await ws.send(json.dumps({"type": "INSTANCE_REQ"}))
        response = await self._recv(ws)
        iinfo = json.loads(response)
        return [(i[0], i[1]) for i in iinfo]

    async def _gam(self, ws):
        await ws.send(
            json.dumps({"instance_id": await self._gii(ws), "type": "MODULE_REQ"})
        )
        return json.loads(await self._recv(ws))

    async def _gr(self, ws):
        await ws.send(
            json.dumps({"instance_id": await self._gii(ws), "type": "RESULT_REQ"})
        )
        return json.loads(await self._recv(ws))

    async def _gdi(self, ws):
        await ws.send(
            json.dumps({"instance_id": await self._gii(ws), "type": "INFO_REQ"})
        )
        return json.loads(await self._recv(ws))

    async def _ca(self, ws, analysisRequest):
        areq = analysisRequest
        if isinstance(analysisRequest, AnalysisRequest):
            areq = analysisRequest.model_dump()
        areq["instance_id"] = await self._gii(ws)
        await ws.send(json.dumps(areq))
        return json.loads(await self._recv(ws))

    async def get_all_instances(self):
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            return await self._gai(ws)

    def set_instance_id(self, instance_id: str):
        self.instance_id = instance_id

    async def close_instance(self, instance_id: str):
        """Close and remove a single instance via the bridge's CLOSE_INSTANCE_REQ.
        Resets self.instance_id if it pointed at the closed instance."""
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            await ws.send(
                json.dumps({"instance_id": instance_id, "type": "CLOSE_INSTANCE_REQ"})
            )
            resp = json.loads(await self._recv(ws))
        if self.instance_id == instance_id:
            self.instance_id = None
        return resp

    async def auto_detect_column_types(self, instance_id: str = None):
        """Auto-detect and set measure_type for all columns based on data content.
        Sends AUTO_DETECT_COLUMN_TYPES_REQ to the bridge."""
        if instance_id is None:
            instance_id = self.instance_id
        if not instance_id:
            return {"success": False, "message": "No instance_id set"}
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            await ws.send(
                json.dumps({"instance_id": instance_id, "type": "AUTO_DETECT_COLUMN_TYPES_REQ"})
            )
            resp = json.loads(await self._recv(ws))
        return resp

    async def set_column_type(self, columns: list, instance_id: str = None):
        """Manually set measure_type for specified columns.
        columns: list of {"name": str, "measure_type": "NOMINAL"|"ORDINAL"|"CONTINUOUS"|"ID"}
        Sends SET_COLUMN_TYPE_REQ to the bridge."""
        if instance_id is None:
            instance_id = self.instance_id
        if not instance_id:
            return {"success": False, "message": "No instance_id set"}
        if not columns:
            return {"success": False, "message": "No columns specified"}
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            await ws.send(
                json.dumps({"instance_id": instance_id, "type": "SET_COLUMN_TYPE_REQ", "columns": columns})
            )
            resp = json.loads(await self._recv(ws))
        return resp

    async def check_instance(self):
        """
        Check if current instance has data. If not, auto-switch to one that does.
        Returns (is_valid, message).
        """
        try:
            instances = await self.get_all_instances()
        except Exception as e:
            return False, f"Cannot get instances: {str(e)}"

        if not instances:
            return False, "No instances found. Please open a dataset in iMast first."

        # Helper: safely read instance info; returns None if the instance is
        # gone or the bridge fails to answer (do not let one bad instance break
        # the whole check).
        async def _safe_info(iid):
            try:
                return await self._get_info_for_instance(iid)
            except Exception:
                return None

        # Check current instance first
        if self.instance_id:
            for _, iid in instances:
                if iid == self.instance_id:
                    info = await _safe_info(iid)
                    if info is not None:
                        columns = info.get("columns", [])
                        total_rows = max((c.get("row_count", 0) for c in columns), default=0)
                        if columns and total_rows > 0:
                            return True, f"Dataset: {info.get('title', 'Untitled')}, {len(columns)} vars, {total_rows} rows."
                    break

        # Find any instance with data
        for title, iid in instances:
            info = await _safe_info(iid)
            if info is None:
                continue
            columns = info.get("columns", [])
            total_rows = max((c.get("row_count", 0) for c in columns), default=0)
            if columns and total_rows > 0:
                old = self.instance_id
                self.instance_id = iid
                if old and old != iid:
                    return True, f"Switched to '{info.get('title', 'Untitled')}' ({len(columns)} vars, {total_rows} rows)."
                return True, f"Dataset: {info.get('title', 'Untitled')}, {len(columns)} vars, {total_rows} rows."

        return False, "No dataset loaded. Please open a data file in iMast first."

    async def _get_info_for_instance(self, iid):
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            await ws.send(json.dumps({"instance_id": iid, "type": "INFO_REQ"}))
            return json.loads(await self._recv(ws))

    async def get_analyses_method(self):
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            return await self._gam(ws)

    async def get_results(self):
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            return await self._gr(ws)

    async def get_dataframe_info(self):
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            return await self._gdi(ws)

    async def create_analysis(self, analysisRequest, wait_for_completion=True, timeout=120):
        is_valid, msg = await self.check_instance()
        if not is_valid:
            raise NoDataError(msg)

        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            result = await self._ca(ws, analysisRequest)
            if wait_for_completion:
                analysis_id = result.get("analysisId") or result.get("id")
                start = asyncio.get_event_loop().time()
                while asyncio.get_event_loop().time() - start < timeout:
                    await asyncio.sleep(1)
                    try:
                        results = await self._gr(ws)
                        analyses = results if isinstance(results, list) else results.get("analyses", [])
                        for a in analyses:
                            a_id = a.get("id") or a.get("analysisId")
                            if a_id == analysis_id:
                                status = a.get("status", "").lower()
                                if "complete" in status or "error" in status:
                                    return a
                    except Exception:
                        pass
            return result

    async def get_all(self):
        async with websockets.connect(
            self.ws_url, additional_headers={"Cookie": f"access_key={self.ak}"}
        ) as ws:
            return {
                "analyses_method": await self._gam(ws),
                "dataframe_info": await self._gdi(ws),
                "results": await self._gr(ws),
            }

    def import_data_file(self, file_path: str) -> dict:
        """Upload a data file (CSV/OMV/XLSX/SPSS) to jamovi server via HTTP POST to /open.
        Creates a new instance with the data loaded. Returns instance info.

        Args:
            file_path: Absolute path to the data file (accessible from the container).

        Returns:
            dict with keys: success, instance_id, title, message, url
        """
        if not os.path.exists(file_path):
            return {"success": False, "message": f"File not found: {file_path}"}

        filename = os.path.basename(file_path)
        # The /open endpoint is at the server root (not under /llm/)
        http_url = f"http://{self.base_url}/open"

        try:
            with open(file_path, "rb") as f:
                files = {"file": (filename, f)}
                # jamovi /open endpoint accepts file upload
                resp = _requests.post(
                    http_url,
                    files=files,
                    timeout=60,
                    allow_redirects=False,
                )

            # Response is newline-delimited JSON: progress lines then final OK/error
            resp_text = resp.text.strip()
            lines = [l for l in resp_text.split("\n") if l.strip()]
            final_line = lines[-1] if lines else ""

            try:
                data = json.loads(final_line)
            except Exception:
                data = {}

            status = data.get("status", "")

            if status == "OK":
                redirect_url = data.get("url", "")
                # Extract instance ID from URL like "<uuid>/"
                instance_id = redirect_url.strip("/").split("/")[-1] if redirect_url else ""
                if instance_id:
                    self.instance_id = instance_id
                return {
                    "success": True,
                    "instance_id": instance_id,
                    "title": filename,
                    "message": f"File '{filename}' imported successfully. New instance: {instance_id}",
                    "url": f"http://{self.base_url}/{redirect_url}",
                }

            elif status == "error":
                return {
                    "success": False,
                    "message": f"Import error: {data.get('message', 'Unknown error')}",
                }

            elif status == "in-progress":
                return {
                    "success": False,
                    "message": f"Import still in progress (last update: {data})",
                }

            return {
                "success": False,
                "message": f"Unexpected response (HTTP {resp.status_code}): {resp_text[:300]}",
            }

        except Exception as e:
            return {"success": False, "message": f"Upload error: {str(e)}"}


if __name__ == "__main__":
    import sys
    import compress_as_markdown
    imast = IMAST(sys.argv[1])
    all = asyncio.run(imast.get_all())
    open("all.json", "w", encoding="utf-8").write(json.dumps(all))
    open("all.md", "w", encoding="utf-8").write(compress_as_markdown.compress_all(all))
