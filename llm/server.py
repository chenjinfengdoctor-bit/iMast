import os
import csv
import io
import time
import json
import asyncio
from fastapi import FastAPI, UploadFile, File
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
import requests
import uvicorn
import pandas as pd
import imast
import compress_as_markdown
import pdf_export
import pdf_export_ui
import plot_screenshot
_LLM_DIR = os.path.dirname(os.path.abspath(__file__))
# Load workflows
_workflows_file = os.path.join(_LLM_DIR, "workflows", "all_workflows.json")
WORKFLOWS = {}
if os.path.exists(_workflows_file):
    with open(_workflows_file, "r", encoding="utf-8") as f:
        _wf_data = json.load(f)
    for _m in _wf_data.get("methods", []):
        _key = f"{_m.get('ns', '')}/{_m.get('name', '')}"
        WORKFLOWS[_key] = _m
        WORKFLOWS[_m.get('name', '')] = _m
    print(f"Loaded {len(WORKFLOWS)} workflow entries")
# API key and model: prefer environment variable, fall back to config file
deepseek_api_key = os.environ.get("DEEPSEEK_API_KEY", "").strip()
deepseek_model = os.environ.get("DEEPSEEK_MODEL", "deepseek-chat").strip()
_key_file = os.path.join(_LLM_DIR, "deepseek_api_key.txt")
if not deepseek_api_key and os.path.exists(_key_file):
    with open(_key_file, "r", encoding="utf-8") as _f:
        _raw = _f.read().strip()
    if _raw:
        try:
            # New JSON format: {"apiKey": "...", "model": "..."}
            _config = json.loads(_raw)
            deepseek_api_key = (_config.get("apiKey") or "").strip()
            if _config.get("model"):
                deepseek_model = _config["model"].strip()
        except (json.JSONDecodeError, ValueError):
            # Legacy plain-text format: just the API key
            deepseek_api_key = _raw
# API base URL
_DEEPSEEK_BASE = "https://api.deepseek.com"
# Shared requests session (no proxy - direct connection works in container)
_requests_session = requests.Session()
_requests_session.trust_env = False  # Ignore proxy env vars
def _chat_completion_sync(messages: list, model: str, api_key: str, max_tokens: int = 4096, timeout: int = 120, thinking: bool = True) -> dict:
    """Synchronous chat completion using requests (works around async HTTP issues in container)."""
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    payload = {
        "model": model,
        "messages": messages,
        "max_tokens": max_tokens,
        "stream": False
    }
    # Enable thinking mode for deeper reasoning (DeepSeek supports this)
    if thinking:
        payload["thinking"] = {"type": "enabled"}
    resp = _requests_session.post(
        f"{_DEEPSEEK_BASE}/chat/completions",
        headers=headers,
        json=payload,
        timeout=timeout
    )
    resp.raise_for_status()
    return resp.json()
def _update_client(api_key: str):
    """Update the API key at runtime (no client object needed with requests)."""
    global deepseek_api_key
    deepseek_api_key = api_key
def _mask_key(api_key: str) -> str:
    """Return a masked version of the API key, e.g. sk-****abcd."""
    if not api_key:
        return ""
    if len(api_key) <= 8:
        return "*" * len(api_key)
    return api_key[:4] + "****" + api_key[-4:]
def _infer_type(value) -> str:
    """Infer the option type from Python value: s=string, d=double, o=bool, c=list."""
    if isinstance(value, bool):
        return "o"
    elif isinstance(value, (int, float)):
        return "d"
    elif isinstance(value, list):
        return "c"
    else:
        return "s"
def _format_analysis_methods(methods) -> str:
    """Format available analysis methods into a readable list grouped by module."""
    lines = ["Available statistical analyses in iMast:"]
    lines.append("IMPORTANT: Use the 'name' field as analysis_name and 'ns' field as ns when calling run_analysis.")
    try:
        if isinstance(methods, dict) and "modules" in methods:
            modules = methods["modules"]
        else:
            modules = methods
        if isinstance(modules, list):
            for module in modules:
                if isinstance(module, dict):
                    mod_name = module.get("name", "Unknown")
                    mod_title = module.get("title", "")
                    analyses = module.get("analyses", [])
                    if analyses:
                        lines.append(f"\n## {mod_name} ({mod_title})")
                        for a in analyses:
                            if isinstance(a, dict):
                                name = a.get("name", "?")
                                ns = a.get("ns", mod_name)
                                title = a.get("title", "")
                                lines.append(f"  - name: {name}, ns: {ns}, title: {title}")
        elif isinstance(modules, dict):
            for mod_name, analyses in modules.items():
                lines.append(f"\n## {mod_name}")
                if isinstance(analyses, list):
                    for a in analyses:
                        if isinstance(a, dict):
                            name = a.get("name", "?")
                            ns = a.get("ns", mod_name)
                            title = a.get("title", "")
                            lines.append(f"  - name: {name}, ns: {ns}, title: {title}")
    except Exception as e:
        lines.append(f"Error formatting methods: {e}")
        lines.append(f"Raw: {json.dumps(methods, ensure_ascii=False)[:2000]}")
    return "\n".join(lines)
def _find_and_describe_method(methods, method_name: str, ns: str = "") -> str:
    """Find a specific analysis method and describe it professionally."""
    lines = [f"Analysis method: {method_name}"]
    if ns:
        lines.append(f"Module: {ns}")
    found = None
    try:
        if isinstance(methods, dict) and "modules" in methods:
            modules = methods["modules"]
        else:
            modules = methods
        if isinstance(modules, list):
            for module in modules:
                if isinstance(module, dict):
                    mod_name = module.get("name", "")
                    if ns and ns.lower() != mod_name.lower():
                        continue
                    for a in module.get("analyses", []):
                        if isinstance(a, dict):
                            a_name = a.get("name", "")
                            a_title = a.get("title", "")
                            if method_name.lower() in a_name.lower() or method_name.lower() in a_title.lower():
                                found = (mod_name, a)
                                break
                if found:
                    break
        elif isinstance(modules, dict):
            search_modules = {ns: modules[ns]} if ns and ns in modules else modules
            for mod_name, analyses in search_modules.items():
                if isinstance(analyses, list):
                    for a in analyses:
                        if isinstance(a, dict) and method_name.lower() in a.get("name", "").lower():
                            found = (mod_name, a)
                            break
        if found:
            mod_name, info = found
            lines.append(f"Found in module: {mod_name}")
            if isinstance(info, dict):
                for k, v in info.items():
                    if k not in ("name", "ns"):
                        lines.append(f"  {k}: {v}")
        else:
            lines.append("Method not found in the available analyses list.")
            lines.append("Use list_analyses to see all available methods.")
    except Exception as e:
        lines.append(f"Error: {e}")
    return "\n".join(lines)
def _resolve_analysis_name(methods, analysis_name: str, ns: str = ""):
    """Resolve analysis name: if AI used title instead of name, find the correct name and ns."""
    try:
        if isinstance(methods, dict) and "modules" in methods:
            modules = methods["modules"]
        else:
            modules = methods
        if isinstance(modules, list):
            for module in modules:
                if isinstance(module, dict):
                    mod_name = module.get("name", "")
                    for a in module.get("analyses", []):
                        if isinstance(a, dict):
                            a_name = a.get("name", "")
                            a_title = a.get("title", "")
                            a_ns = a.get("ns", mod_name)
                            # Exact match on name
                            if a_name.lower() == analysis_name.lower():
                                return a_name, a_ns or ns
                            # Match on title
                            if a_title.lower() == analysis_name.lower():
                                return a_name, a_ns or ns
                            # Partial match on title
                            if analysis_name.lower() in a_title.lower() or a_title.lower() in analysis_name.lower():
                                return a_name, a_ns or ns
    except Exception as e:
        print(f"Error resolving analysis name: {e}")
    return analysis_name, ns
def _extract_tables_as_csv(results: list, analysis_names: list = None) -> str:
    """
    Parse analysis results JSON and extract all visible tables as CSV text.
    Walks the results tree (groups -> tables) following the same structure as
    compress_as_markdown.recursive_parse_results.  Each table becomes a CSV
    block with section headers; multiple tables are separated by blank lines.
    """
    output = io.StringIO()
    writer = csv.writer(output)
    table_count = 0
    for result in results:
        # Skip the empty placeholder result
        if (result.get("instanceId") == ""
                and result.get("name") == "empty"
                and result.get("ns") == "jmv"):
            continue
        rname = result.get("name", "")
        rns = result.get("ns", "")
        # Filter by analysis names if requested
        if analysis_names:
            if not any(n.lower() in rname.lower() for n in analysis_names):
                continue
        # Walk the results tree to collect tables
        tables = []  # list of (breadcrumb_title, table_node)
        def _walk(node, breadcrumb):
            if not isinstance(node, dict):
                return
            if node.get("visible") not in ("DEFAULT_YES", "YES"):
                return
            node_title = node.get("title", "")
            current_path = f"{breadcrumb} > {node_title}" if (breadcrumb and node_title) else (node_title or breadcrumb)
            ntype = node.get("type", "")
            if ntype == "group":
                for child in node.get("content", []):
                    _walk(child, current_path)
            elif ntype == "table":
                tables.append((current_path, node))
        _walk(result.get("results", {}), "")
        for table_title, table_node in tables:
            table_count += 1
            writer.writerow([f"=== Analysis: {rname} (ns={rns}) ==="])
            if table_title:
                writer.writerow([f"Table: {table_title}"])
            content = table_node.get("content", {})
            columns = content.get("columns", [])
            if not columns:
                writer.writerow(["(no data)"])
                writer.writerow([])
                continue
            # Header row: combine superTitle + title
            headers = []
            for col in columns:
                st = col.get("superTitle", "")
                ct = col.get("title", "")
                headers.append(f"{st} - {ct}" if st else ct)
            writer.writerow(headers)
            # Data rows: columns hold cells, transpose to rows
            row_values = {}
            empty_row = [""] * len(columns)
            for ci, col in enumerate(columns):
                for ri, cell in enumerate(col.get("cells", [])):
                    if ri not in row_values:
                        row_values[ri] = empty_row.copy()
                    val = cell.get("value", "")
                    if isinstance(val, float):
                        row_values[ri][ci] = f"{val:.6g}"
                    else:
                        row_values[ri][ci] = str(val)
            for ri in sorted(row_values.keys()):
                writer.writerow(row_values[ri])
            writer.writerow([])  # blank separator between tables
    if table_count == 0:
        return ""
    return output.getvalue()
# Mapping from high-level plot_type to jamovi analysis definition
# Verified against the live analyses_method list:
#   - descriptives (ns=jmv): vars (Variables), hist/dens/box/qq (Bool)
#   - QQPlot (ns=Estimation): x (Variable)
_PLOT_TYPE_MAP = {
    "histogram": {
        "analysis_name": "descriptives",
        "ns": "jmv",
        "options_template": lambda vars, extra: {
            "vars": vars, "hist": True, "n": True,
        },
    },
    "qqplot": {
        "analysis_name": "QQPlot",
        "ns": "Estimation",
        "options_template": lambda vars, extra: {
            "x": vars[0] if len(vars) > 0 else "",
        },
    },
    "boxplot": {
        "analysis_name": "descriptives",
        "ns": "jmv",
        "options_template": lambda vars, extra: {
            "vars": vars, "box": True, "n": True,
        },
    },
    "density": {
        "analysis_name": "descriptives",
        "ns": "jmv",
        "options_template": lambda vars, extra: {
            "vars": vars, "dens": True, "n": True,
        },
    },
    "scatter": {
        "analysis_name": "scat",
        "ns": "scatr",
        "options_template": lambda vars, extra: {
            "x": vars[0] if len(vars) > 0 else "",
            "y": vars[1] if len(vars) > 1 else "",
        },
    },
}
def _extract_analysis_error(result: dict) -> str:
    """Extract a human-readable error message from a failed jamovi analysis result.
    jamovi stores the real R error in results.error_message (not in the top-level
    error.message, which is often empty).  Falls back to the Debug preformatted
    node, then to the top-level error field.
    """
    # 1. results.error_message — the primary location in jamovi 2.x
    results_node = result.get("results", {})
    if isinstance(results_node, dict):
        msg = results_node.get("error_message") or results_node.get("errorMessage")
        if msg:
            return str(msg).strip()
    # 2. Top-level error.message / error.cause
    err = result.get("error", {})
    if isinstance(err, dict):
        msg = err.get("message") or err.get("cause")
        if msg:
            return str(msg).strip()
    # 3. Search the results tree for a "debug" preformatted node
    def _find_debug(node):
        if isinstance(node, dict):
            if node.get("name") == "debug" and node.get("type") == "preformatted":
                content = node.get("content", "")
                if content:
                    # First line is usually the actual error; rest is stack trace
                    return content.split("\n")[0].strip()
            for v in node.values():
                found = _find_debug(v)
                if found:
                    return found
        elif isinstance(node, list):
            for item in node:
                found = _find_debug(item)
                if found:
                    return found
        return None
    debug_msg = _find_debug(results_node)
    if debug_msg:
        return debug_msg
    return "Unknown analysis error (open the analysis in iMast and check the Debug panel for details)"
def _jamovi_convert_options(options: dict) -> dict:
    """
    Convert a flat options dict (or pre-formatted {options: [...]}) into the
    jamovi AnalysisOptions envelope.  Reuses the same logic as run_analysis.
    Returns {"hasNames": True, "options": [...]}.
    """
    def _convert_option(key, value):
        if isinstance(value, list):
            if len(value) > 0 and all(isinstance(item, dict) for item in value):
                nested_items = []
                for i, group in enumerate(value):
                    group_opts = []
                    for gk, gv in group.items():
                        if isinstance(gv, bool):
                            group_opts.append({"key": gk, "value": int(gv), "type": "o"})
                        elif isinstance(gv, int):
                            group_opts.append({"key": gk, "value": gv, "type": "i"})
                        elif isinstance(gv, float):
                            group_opts.append({"key": gk, "value": gv, "type": "d"})
                        else:
                            group_opts.append({"key": gk, "value": str(gv), "type": "s"})
                    nested_items.append({
                        "key": str(i),
                        "type": "c",
                        "value": {"hasNames": True, "options": group_opts}
                    })
                return {"key": key, "value": {"hasNames": False, "options": nested_items}, "type": "c"}
            else:
                nested = {
                    "hasNames": False,
                    "options": [{"key": str(i), "type": "s", "value": str(v)} for i, v in enumerate(value)]
                }
                return {"key": key, "value": nested, "type": "c"}
        elif isinstance(value, bool):
            return {"key": key, "value": int(value), "type": "o"}
        elif isinstance(value, int):
            return {"key": key, "value": value, "type": "i"}
        elif isinstance(value, float):
            return {"key": key, "value": value, "type": "d"}
        elif isinstance(value, dict):
            if "options" in value and isinstance(value["options"], list):
                for i, item in enumerate(value["options"]):
                    if isinstance(item, dict) and "key" not in item:
                        item["key"] = str(i)
            return {"key": key, "value": value, "type": "c"}
        else:
            return {"key": key, "value": str(value), "type": "s"}
    if isinstance(options, dict) and "options" in options and isinstance(options["options"], list):
        opts_list = []
        for item in options["options"]:
            if isinstance(item, dict) and "key" in item and "value" in item:
                opts_list.append(_convert_option(item["key"], item["value"]))
            else:
                opts_list.append(item)
    else:
        opts_list = [_convert_option(k, v) for k, v in options.items()]
    return {"hasNames": True, "options": opts_list}
async def execute_tool(tool_name: str, parameters: dict, url: str, iid: str) -> str:
    """Execute a tool and return the result as a string."""
    try:
        iclient = imast.IMAST(f"{url}/IMAST")
        iclient.set_instance_id(iid)
        # IMPORTANT: Do NOT auto-switch instances.
        # AI should only operate on the current instance that the user sees.
        # This prevents the "gap" problem where AI operates on a different
        # instance than what the user sees.
        if tool_name == "get_dataset":
            # Only return the CURRENT instance's data (what the user sees)
            # Do NOT search all instances - that causes the "gap" problem
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
                return compress_as_markdown.compress_dataframe_info(df_info)
            except Exception:
                pass
            return "No dataset found. Please open a data file in iMast first."
        elif tool_name == "run_workflow":
            print(f"[DEBUG] run_workflow called: params={parameters}")
            # One-click workflow: import data (optional) -> fix variable types -> run analysis
            workflow_name = parameters.get("workflow_name", "")
            data_file = parameters.get("data_file", "")
            ns = parameters.get("ns", "")
            steps = []
            current_iid = iid
            # Step 1: Import data if specified AND current instance has no data
            # First check if current instance already has data
            current_has_data = False
            try:
                df_check = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=5)
                if df_check and df_check.get("columns"):
                    row_count = df_check["columns"][0].get("row_count", 0) if df_check["columns"] else 0
                    if row_count > 0:
                        current_has_data = True
                        steps.append(f"Step 1: Current dataset already has {row_count} rows - skipping import")
            except Exception:
                pass
            if data_file and not current_has_data:
                steps.append(f"Step 1: Importing data from {data_file}...")
                try:
                    import_result = await asyncio.wait_for(
                        asyncio.to_thread(iclient.import_data_file, data_file),
                        timeout=60
                    )
                    if not import_result.get("success"):
                        return f"Workflow failed at import: {import_result.get('message', 'Unknown error')}"
                    new_iid = import_result.get("instance_id", "")
                    if new_iid:
                        # Wait for the new instance to fully load (lazy loading)
                        steps.append("  Waiting for instance to load...")
                        await asyncio.sleep(3)
                        new_iclient = imast.IMAST(f"{url}/IMAST")
                        new_iclient.set_instance_id(new_iid)
                        iclient = new_iclient
                        current_iid = new_iid
                        steps.append("  OK Data imported successfully")
                except Exception as e:
                    return f"Workflow failed at import: {str(e)}"
            elif data_file and current_has_data:
                steps.append(f"Step 1: Using existing dataset (no need to re-import)")
            # Step 2: Check dataset
            steps.append("Step 2: Checking dataset...")
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=10)
                ncols = len(df_info.get("columns", []))
                row_count = df_info["columns"][0].get("row_count", 0) if df_info.get("columns") else 0
                steps.append(f"  OK Dataset: {ncols} columns, {row_count} rows")
            except Exception as e:
                return f"Workflow failed at dataset check: {str(e)}"
            # Step 2.1: Analyze data characteristics (missing values, outliers)
            steps.append("Step 2.1: Analyzing data characteristics...")
            data_quality_report = ""
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=10)
                cols = df_info.get("columns", [])
                missing_details = []
                continuous_vars = []
                nominal_vars = []
                for col in cols:
                    col_name = col.get("name", "")
                    missing = col.get("missing_count", 0)
                    measure_type = col.get("measure_type", "")
                    if missing > 0:
                        missing_details.append(f"{col_name}: {missing} missing")
                    if measure_type == "continuous":
                        continuous_vars.append(col_name)
                    elif measure_type == "nominal":
                        nominal_vars.append(col_name)
                steps.append(f"  OK Continuous variables: {len(continuous_vars)}")
                steps.append(f"  OK Nominal variables: {len(nominal_vars)}")
                if missing_details:
                    steps.append(f"  WARN Missing values in {len(missing_details)} columns")
                    data_quality_report = "Missing values: " + "; ".join(missing_details[:5])
                else:
                    steps.append(f"  OK No missing values detected")
                    data_quality_report = "No missing values detected"
            except Exception as e:
                steps.append(f"  WARN Data analysis skipped: {str(e)}")
            # Step 2.2: Data quality assessment
            steps.append("Step 2.2: Data quality assessment...")
            try:
                if "Missing values" in data_quality_report:
                    steps.append("  INFO Data quality issues detected. Review recommended before analysis.")
                    steps.append("  INFO Automatic cleaning not applied - check with user first.")
                else:
                    steps.append("  OK Data quality looks good, no cleaning needed.")
            except Exception as e:
                steps.append(f"  WARN Data quality check skipped: {str(e)}")
            # Step 3: Infer and set variable types (via execute_tool)
            steps.append("Step 3: Setting variable types...")
            try:
                await asyncio.wait_for(
                    execute_tool("infer_and_set_variable_types", 
                        {"workflow_name": workflow_name, "ns": ns}, url, current_iid),
                    timeout=30
                )
                steps.append("  OK Variable types set")
            except Exception as e:
                steps.append(f"  WARN Variable type step skipped: {str(e)}")
            # Step 4: Get workflow parameters (via execute_tool)
            steps.append("Step 4: Getting workflow parameters...")
            try:
                await asyncio.wait_for(
                    execute_tool("get_workflow",
                        {"workflow_name": workflow_name, "ns": ns}, url, current_iid),
                    timeout=15
                )
                steps.append(f"  OK Workflow retrieved: {workflow_name}")
            except Exception as e:
                return f"Workflow failed at get_workflow: {str(e)}"
            # Step 5: Auto-match parameters based on variable names
            steps.append("Step 5: Auto-matching parameters...")
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=10)
                var_names = [col.get("name", "") for col in df_info.get("columns", [])]
                options = []
                # Select parameter map based on workflow name
                wf_lower = workflow_name.lower()
                if "commutability" in wf_lower or "ep14" in wf_lower:
                    # Commutability EP14-A3
                    param_map = {
                        "csid": ["CS ID", "临床样本", "样本编号", "csid"],
                        "csx": ["CS MPx", "CS X", "测量X", "csx"],
                        "csy": ["CS MPy", "CS Y", "测量Y", "csy"],
                        "rmid": ["RM ID", "参考物质", "物质编号", "rmid"],
                        "rmp": ["RM Pos", "RM Position", "物质位置", "rmp"],
                        "rmx": ["RM MPx", "RM X", "rmx"],
                        "rmy": ["RM MPy", "RM Y", "rmy"],
                    }
                elif "precision" in wf_lower or "ep15" in wf_lower:
                    # Precision EP15-A3
                    param_map = {
                        "sample": ["Sample", "样本", "sample_id", "id"],
                        "run": ["Run", "批次", "run"],
                        "level": ["Level", "水平", "浓度水平", "level"],
                        "result": ["Result", "测量值", "结果", "meas", "value"],
                    }
                elif "methodcomparison" in wf_lower or "ep9" in wf_lower or "comparison" in wf_lower:
                    # Method Comparison EP9-A3
                    param_map = {
                        "sample": ["Sample", "样本", "sample_id", "id"],
                        "x": ["X", "方法1", "Method1", "method_x"],
                        "y": ["Y", "方法2", "Method2", "method_y"],
                    }
                elif "linearity" in wf_lower or "ep6" in wf_lower or "linear" in wf_lower:
                    # Linearity EP6-A2
                    param_map = {
                        "sample": ["Sample", "样本", "sample_id"],
                        "concentration": ["Conc", "浓度", "理论值", "expected"],
                        "result": ["Result", "测量值", "实测值", "measured", "response"],
                    }
                else:
                    # Generic fallback: try to match common patterns
                    param_map = {
                        "id": ["ID", "编号", "id"],
                        "group": ["Group", "分组", "group"],
                        "x": ["X", "x"],
                        "y": ["Y", "y"],
                    }
                matched_params = {}
                for param, patterns in param_map.items():
                    for pattern in patterns:
                        matches = [v for v in var_names if pattern.lower() in v.lower()]
                        if matches:
                            if len(matches) == 1:
                                matched_params[param] = matches[0]
                            else:
                                matched_params[param] = matches
                            break
                steps.append(f"  OK Matched parameters: {list(matched_params.keys())}")
                for key, value in matched_params.items():
                    if isinstance(value, list):
                        options.append({"key": key, "value": value, "type": "c"})
                    else:
                        options.append({"key": key, "value": value, "type": "s"})
            except Exception as e:
                steps.append(f"  WARN Parameter matching failed: {str(e)}")
                options = []
            # Step 6: Run analysis (via execute_tool)
            steps.append("Step 6: Running analysis...")
            try:
                # Wrap options list into dict format expected by run_analysis
                run_options = {"options": options} if options else {}
                await asyncio.wait_for(
                    execute_tool("run_analysis",
                        {"analysis_name": workflow_name, "ns": ns, "options": run_options}, url, current_iid),
                    timeout=60
                )
                steps.append("  OK Analysis completed")
            except Exception as e:
                return f"Workflow failed at run_analysis: {str(e)}\n\nSteps so far:\n" + "\n".join(steps)
            # Step 7: Get results
            steps.append("Step 7: Getting results...")
            try:
                result = await asyncio.wait_for(
                    execute_tool("interpret_result", {}, url, current_iid),
                    timeout=15
                )
                results = result
            except Exception as e:
                results = f"Results retrieval warning: {str(e)}"
            # Build final output
            output = "Workflow completed successfully!\n\n"
            output += "\n".join(steps)
            output += "\n\n---\n\n"
            output += f"Analysis results for {workflow_name}:\n"
            output += str(results)[:2000]
            # Add navigate marker if we imported new data
            if data_file and import_result.get("url"):
                output += "\n\n[NAVIGATE_TO: " + import_result["url"] + "]"
            return output
        elif tool_name == "interpret_result":
            # Validate instance has data
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot interpret results: {check_msg}"
            results = await asyncio.wait_for(iclient.get_results(), timeout=8)
            if not results:
                return "No analysis results found. Please run an analysis first."
            md = compress_as_markdown.compress_results(results)
            return f"Current analysis results:\n{md}\n\nPlease interpret these results professionally."
        elif tool_name == "generate_report":
            # Validate instance has data
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot generate report: {check_msg}"
            title = parameters.get("title", "Statistical Analysis Report")
            all_data = await asyncio.wait_for(iclient.get_all(), timeout=8)
            md = compress_as_markdown.compress_all(all_data)
            return f"Please generate a complete statistical analysis report titled '{title}' based on the following data:\n{md}"
        elif tool_name == "list_analyses":
            methods = await asyncio.wait_for(iclient.get_analyses_method(), timeout=8)
            return _format_analysis_methods(methods)
        elif tool_name == "run_analysis":
            analysis_name = parameters.get("analysis_name", "")
            ns = parameters.get("ns", "")
            options = parameters.get("options", {})
            if not analysis_name or not ns:
                return "Error: analysis_name and ns are required. Use list_analyses to see available methods."
            # Pre-flight: validate instance and data (following jamovi source behavior)
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot run analysis: {check_msg}\n\nPlease make sure:\n1. A dataset is open in iMast\n2. The AI panel was opened from the current dataset page\n3. The page has not been refreshed since opening the AI panel"
            # Auto-resolve analysis name: if AI used title instead of name, find the correct name
            methods = await asyncio.wait_for(iclient.get_analyses_method(), timeout=8)
            resolved_name, resolved_ns = _resolve_analysis_name(methods, analysis_name, ns)
            if resolved_name != analysis_name or resolved_ns != ns:
                print(f"Resolved analysis name: '{analysis_name}' -> '{resolved_name}' (ns: '{ns}' -> '{resolved_ns}')")
            analysis_name = resolved_name
            ns = resolved_ns
            # === PARAMETER VALIDATION & FILTERING ===
            # Remove parameters not in the workflow's parameter list to avoid
            # AI mixing in parameters from other analysis methods.
            # Supports both simple format {"csid": "CS ID"} and jamovi format
            # {"options": [{"key": "csid", "value": "CS ID", "type": "s"}, ...]}.
            try:
                _wf_for_filter = WORKFLOWS.get(f"{ns}/{analysis_name}") or WORKFLOWS.get(analysis_name)
                if _wf_for_filter:
                    _valid_params = {p.get('name') for p in _wf_for_filter.get('parameters', [])}
                    # Detect format: jamovi nested format has "options" key with a list
                    if isinstance(options, dict) and 'options' in options and isinstance(options.get('options'), list):
                        # Jamovi format: filter the list items by "key"
                        _filtered_list = []
                        _removed = []
                        for _item in options['options']:
                            if isinstance(_item, dict) and _item.get('key') in _valid_params:
                                _filtered_list.append(_item)
                            else:
                                _removed.append(_item.get('key', '?') if isinstance(_item, dict) else '?')
                        if _removed:
                            print(f"Filtered out unknown parameters for {analysis_name}: {_removed} (valid: {sorted(_valid_params)})")
                            options = {'options': _filtered_list}
                    else:
                        # Simple format: filter dict keys
                        _filtered = {}
                        _removed = []
                        for k, v in options.items():
                            if k in _valid_params:
                                _filtered[k] = v
                            else:
                                _removed.append(k)
                        if _removed:
                            print(f"Filtered out unknown parameters for {analysis_name}: {_removed} (valid: {sorted(_valid_params)})")
                            options = _filtered
            except Exception as _filter_err:
                print(f"Parameter filtering skipped: {_filter_err}")
            # === END PARAMETER VALIDATION ===
            # Convert options to jamovi format
            def _convert_option(key, value):
                """Convert a single option to jamovi AnalysisOption format."""
                if isinstance(value, list):
                    if len(value) > 0 and all(isinstance(item, dict) for item in value):
                        # Array of Groups (e.g., rs parameter with l/r/u sub-fields)
                        nested_items = []
                        for i, group in enumerate(value):
                            group_opts = []
                            for gk, gv in group.items():
                                if isinstance(gv, bool):
                                    group_opts.append({"key": gk, "value": int(gv), "type": "o"})
                                elif isinstance(gv, int):
                                    group_opts.append({"key": gk, "value": gv, "type": "i"})
                                elif isinstance(gv, float):
                                    group_opts.append({"key": gk, "value": gv, "type": "d"})
                                else:
                                    group_opts.append({"key": gk, "value": str(gv), "type": "s"})
                            nested_items.append({
                                "key": str(i),
                                "type": "c",
                                "value": {"hasNames": True, "options": group_opts}
                            })
                        nested = {"hasNames": False, "options": nested_items}
                        return {"key": key, "value": nested, "type": "c"}
                    else:
                        # Variables type: wrap as nested AnalysisOptions with string items
                        nested = {
                            "hasNames": False,
                            "options": [{"key": str(i), "type": "s", "value": str(v)} for i, v in enumerate(value)]
                        }
                        return {"key": key, "value": nested, "type": "c"}
                elif isinstance(value, bool):
                    return {"key": key, "value": int(value), "type": "o"}
                elif isinstance(value, int):
                    return {"key": key, "value": value, "type": "i"}
                elif isinstance(value, float):
                    return {"key": key, "value": value, "type": "d"}
                elif isinstance(value, dict):
                    # Already nested AnalysisOptions - ensure each item has key
                    if "options" in value and isinstance(value["options"], list):
                        for i, item in enumerate(value["options"]):
                            if isinstance(item, dict) and "key" not in item:
                                item["key"] = str(i)
                    return {"key": key, "value": value, "type": "c"}
                else:
                    # String or other
                    return {"key": key, "value": str(value), "type": "s"}
            if isinstance(options, dict) and "options" in options and isinstance(options["options"], list):
                # AI already provided the full options list - convert each item
                opts_list = []
                for item in options["options"]:
                    if isinstance(item, dict) and "key" in item and "value" in item:
                        opts_list.append(_convert_option(item["key"], item["value"]))
                    else:
                        opts_list.append(item)
            else:
                # Flat format - convert each key-value pair
                opts_list = [_convert_option(k, v) for k, v in options.items()]
            request = {
                "type": "ANALYSIS_CREATE_REQ",
                "analysis_name": analysis_name,
                "ns": ns,
                "options": {
                    "hasNames": True,
                    "options": opts_list
                }
            }
            # === AUTO VARIABLE TYPE CORRECTION (pre-flight) ===
            # Before running analysis, auto-check and fix variable measure types
            # based on the workflow's parameter definitions. This ensures ID/category
            # variables are NOMINAL and measurement variables are CONTINUOUS.
            try:
                _wf = WORKFLOWS.get(f"{ns}/{analysis_name}") or WORKFLOWS.get(analysis_name)
                print(f"Auto-correct: workflow found={_wf is not None}, params={len(_wf.get('parameters', [])) if _wf else 0}")
                if _wf:
                    _nominal_kw = ['id', 'group', 'subject', 'method', 'site', 'position', 'category', 'class', 'type', 'level', 'factor', 'batch', 'lot', 'sample', 'specimen', 'run', 'reagent', 'calibrator', 'operator', 'instrument', 'plate', 'well']
                    _continuous_kw = ['value', 'result', 'measurement', 'concentration', 'reading', 'score', 'amount', 'signal', 'absorbance', 'optical', 'density', 'mp', 'mean', 'sd', 'cv', 'ratio', 'difference', 'delta', 'x', 'y', 'z', 'time', 'age', 'weight', 'height']
                    # Build expected type map: variable_name -> expected measure_type
                    _expected = {}
                    for _p in _wf.get('parameters', []):
                        _ptype = _p.get('type', '')
                        _pname = _p.get('name', '')
                        _pname_lower = _pname.lower()
                        _ptitle = _p.get('title', '').lower()
                        _pdesc = _p.get('description', '').lower()
                        _combined = f"{_pname_lower} {_ptitle} {_pdesc}"
                        if _ptype in ('Pairs', 'Variables'):
                            _exp = 'CONTINUOUS'
                        elif _ptype == 'Variable':
                            if any(kw in _combined for kw in _nominal_kw):
                                _exp = 'NOMINAL'
                            elif any(kw in _combined for kw in _continuous_kw):
                                _exp = 'CONTINUOUS'
                            else:
                                _exp = 'NOMINAL'
                        else:
                            continue
                        # Get the variable name(s) from options
                        _opt_val = options.get(_pname)
                        if isinstance(_opt_val, str) and _opt_val:
                            _expected[_opt_val] = _exp
                        elif isinstance(_opt_val, list):
                            for _v in _opt_val:
                                if isinstance(_v, str) and _v:
                                    _expected[_v] = _exp
                    if _expected:
                        print(f"Auto-correct: expected types for {len(_expected)} variables: {json.dumps(_expected, ensure_ascii=False)}")
                        # Get current dataset column types
                        _df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
                        _current_types = {}
                        for _c in _df_info.get('columns', []):
                            _cname = _c.get('name', '')
                            _ctype = _c.get('measureType', '') or _c.get('measure_type', '') or _c.get('measuretype', '')
                            if _cname:
                                _current_types[_cname] = _ctype.upper() if _ctype else ''
                        print(f"Auto-correct: current dataset has {len(_current_types)} columns")
                        # Find mismatches
                        _to_fix = []
                        for _vname, _exp_type in _expected.items():
                            _cur = _current_types.get(_vname, '')
                            if _cur and _cur != _exp_type:
                                _to_fix.append({"name": _vname, "measure_type": _exp_type})
                            elif not _cur:
                                print(f"Auto-correct: variable '{_vname}' not found in dataset")
                        print(f"Auto-correct: {len(_to_fix)} variables need correction")
                        if _to_fix:
                            print(f"Auto-correcting variable types: {json.dumps(_to_fix, ensure_ascii=False)}")
                            await asyncio.wait_for(iclient.set_column_type(_to_fix), timeout=30)
                            print(f"Auto-corrected {len(_to_fix)} variable(s)")
            except Exception as _auto_err:
                print(f"Auto variable type correction skipped: {_auto_err}")
            # === END AUTO VARIABLE TYPE CORRECTION ===
            print(f"run_analysis request: {json.dumps(request, ensure_ascii=False)[:500]}")
            try:
                result = await asyncio.wait_for(iclient.create_analysis(request, wait_for_completion=True), timeout=120)
                # Check for analysis errors
                status = result.get("status", "")
                if "error" in status.lower():
                    error_msg = _extract_analysis_error(result)
                    return f"Analysis '{analysis_name}' failed with error: {error_msg}\n\nPlease check your data and parameter settings."
                md = compress_as_markdown.compress_results([result])
                return f"Analysis '{analysis_name}' completed. Results:\n{md}\n\nPlease interpret these results for the user."
            except imast.NoDataError as e:
                return f"Cannot run analysis: {str(e)}"
            except asyncio.TimeoutError:
                return f"Analysis '{analysis_name}' timed out after 120 seconds. The analysis may still be running - check the results panel in iMast."
            except Exception as e:
                return f"Analysis '{analysis_name}' failed: {str(e)}\n\nPlease check your data and parameter settings, or try a different analysis method."
        elif tool_name == "describe_method":
            method_name = parameters.get("method_name", "") or parameters.get("analysis_name", "")
            ns = parameters.get("ns", "")
            methods = await asyncio.wait_for(iclient.get_analyses_method(), timeout=8)
            return _find_and_describe_method(methods, method_name, ns)
        elif tool_name == "get_workflow":
            method_name = parameters.get("method_name", "") or parameters.get("analysis_name", "")
            ns = parameters.get("ns", "")
            # Try to find workflow
            workflow = None
            matched_key = None
            if ns:
                key = f"{ns}/{method_name}"
                workflow = WORKFLOWS.get(key)
                if workflow:
                    matched_key = key
            if not workflow:
                workflow = WORKFLOWS.get(method_name)
                if workflow:
                    matched_key = method_name
            # Fuzzy match by title if exact name lookup fails
            if not workflow:
                import re as _re
                _search_lower = method_name.lower().strip()
                # Critical version tokens: epXX (e.g. ep14, ep30) and year (e.g. 2018)
                # These MUST match - clsi/ifcc are org names, not versions
                _crit_version_pattern = _re.compile(r'(ep[-]?\d+|^\d{4}|ifcc\s*\d{4})')
                _search_crit_versions = set(v.replace('-', '') for v in _crit_version_pattern.findall(_search_lower))
                _best_match = None
                _best_score = 0
                for _wk, _wv in WORKFLOWS.items():
                    if not isinstance(_wv, dict):
                        continue
                    _wtitle = str(_wv.get('title', '')).lower()
                    _wname = str(_wv.get('name', '')).lower()
                    _wns = str(_wv.get('ns', '')).lower()
                    _w_crit_versions = set(v.replace('-', '') for v in (set(_crit_version_pattern.findall(_wtitle)) | set(_crit_version_pattern.findall(_wname))))
                    # Critical version check: if search has epXX or year, target MUST share it
                    _crit_mismatch = False
                    if _search_crit_versions:
                        if not (_search_crit_versions & _w_crit_versions):
                            _crit_mismatch = True
                    if _crit_mismatch:
                        continue  # Skip - wrong version (e.g. ep14 vs ep30)
                    # Score: exact title match > title contains > name contains > keyword overlap
                    _score = 0
                    if _search_lower == _wtitle:
                        _score = 100
                    elif _search_lower in _wtitle or _wtitle in _search_lower:
                        _score = 80
                    elif _search_lower == _wname:
                        _score = 90
                    elif _search_lower in _wname:
                        _score = 70
                    else:
                        _kws = [k for k in _search_lower.replace('(', ' ').replace(')', ' ').replace(',', ' ').replace('-', ' ').split() if len(k) > 2]
                        _hits = sum(1 for k in _kws if k in _wtitle or k in _wname)
                        _score = _hits * 10
                    # Version match bonus
                    if _search_crit_versions and _w_crit_versions:
                        _score += len(_search_crit_versions & _w_crit_versions) * 20
                    # If ns specified, boost matches in that namespace
                    if ns and _wns == ns.lower():
                        _score += 5
                    if _score > _best_score and _score >= 20:
                        _best_score = _score
                        _best_match = _wk
                        workflow = _wv
                        matched_key = _wk
                if workflow:
                    print(f"get_workflow fuzzy matched: '{method_name}' -> '{matched_key}' (score={_best_score})")
            if workflow:
                lines = [f"=== Workflow: {workflow.get('title', method_name)} ==="]
                lines.append(f"Module: {workflow.get('ns', ns)}")
                lines.append(f"Analysis name: {workflow.get('name', method_name)}")
                lines.append(f"Purpose: {workflow.get('purpose', '')}")
                lines.append(f"\nParameters:")
                for p in workflow.get('parameters', []):
                    req = "REQUIRED" if p.get('required') else "optional"
                    default = f", default={p.get('default')}" if 'default' in p else ""
                    lines.append(f"  - {p['name']} ({p['type']}, {req}{default}): {p.get('title', '')}")
                # Auto-infer variable type requirements for Variable/Pairs/Variables params
                _nominal_keywords = ['id', 'group', 'subject', 'method', 'site', 'position', 'category', 'class', 'type', 'level', 'factor', 'batch', 'lot', 'sample', 'specimen', 'run', 'reagent', 'calibrator', 'operator', 'instrument', 'plate', 'well', 'row', 'col']
                _continuous_keywords = ['value', 'result', 'measurement', 'concentration', 'reading', 'score', 'amount', 'signal', 'absorbance', 'optical', 'density', 'mp', 'mean', 'sd', 'cv', 'ratio', 'difference', 'delta', 'x', 'y', 'z', 'time', 'age', 'weight', 'height', 'temperature', 'volume']
                _var_type_reqs = []
                for p in workflow.get('parameters', []):
                    ptype = p.get('type', '')
                    pname = p.get('name', '').lower()
                    ptitle = p.get('title', '').lower()
                    pdesc = p.get('description', '').lower()
                    combined = f"{pname} {ptitle} {pdesc}"
                    if ptype in ('Pairs', 'Variables'):
                        expected = 'CONTINUOUS'
                        reason = '多列测量值/配对数据'
                    elif ptype == 'Variable':
                        if any(kw in combined for kw in _nominal_keywords):
                            expected = 'NOMINAL'
                            reason = '标识/分类/分组变量'
                        elif any(kw in combined for kw in _continuous_keywords):
                            expected = 'CONTINUOUS'
                            reason = '测量值/数值变量'
                        else:
                            expected = 'NOMINAL'
                            reason = '单列Variable默认按分类变量处理（如不确定请检查数据内容）'
                    else:
                        continue
                    _var_type_reqs.append((p['name'], expected, reason, p.get('title', '')))
                if _var_type_reqs:
                    lines.append(f"\n=== Variable Type Requirements (MUST set before run_analysis) ===")
                    lines.append(f"以下变量在执行分析前必须设置为指定的测量级别，否则分析可能失败或结果错误：")
                    for vname, vtype, vreason, vtitle in _var_type_reqs:
                        lines.append(f"  - {vname} ({vtitle}): MUST be {vtype} — {vreason}")
                    lines.append(f"操作：先调用 get_dataset 查看当前变量类型，然后用 set_column_type 将不符合的变量修正为上述类型，修正完成后再执行 run_analysis。")
                lines.append(f"\nSteps:")
                # Prepend variable type check step if not already present
                _steps = workflow.get('steps', [])
                _has_type_step = any('variable type' in s.lower() or 'column type' in s.lower() or 'set_column' in s.lower() or '测量级别' in s or '变量类型' in s for s in _steps)
                if not _has_type_step and _var_type_reqs:
                    lines.append(f"  0. 【前置】调用 get_dataset 查看变量类型，用 set_column_type 将上表中列出的变量设置为指定测量级别")
                for s in _steps:
                    lines.append(f"  {s}")
                lines.append(f"\nExample:")
                lines.append(f"  {json.dumps(workflow.get('example', {}), ensure_ascii=False)}")
                # Enhanced v2.0 fields
                if workflow.get('data_requirements'):
                    dr = workflow['data_requirements']
                    lines.append(f"\nData Requirements:")
                    lines.append(f"  {dr.get('description', '')}")
                    for col in dr.get('required_columns', []):
                        lines.append(f"  - REQUIRED {col['name']} ({col['type']}): {col.get('description','')}")
                    for col in dr.get('optional_columns', []):
                        lines.append(f"  - optional {col['name']} ({col['type']}): {col.get('description','')}")
                    if dr.get('row_count'):
                        lines.append(f"  Row count: {dr['row_count']}")
                    if dr.get('replicates'):
                        lines.append(f"  Replicates: {dr['replicates']}")
                    if dr.get('typical_pattern'):
                        lines.append(f"  Pattern: {dr['typical_pattern']}")
                if workflow.get('key_parameter_guide'):
                    lines.append(f"\nKey Parameter Guide:")
                    for g in workflow['key_parameter_guide']:
                        lines.append(f"  - {g}")
                if workflow.get('verified_example'):
                    ve = workflow['verified_example']
                    lines.append(f"\nVerified Example (tested & working):")
                    lines.append(f"  options: {json.dumps(ve.get('options',{}), ensure_ascii=False)}")
                    if ve.get('test_data'):
                        lines.append(f"  test_data: {ve['test_data']}")
                    if ve.get('expected_result'):
                        lines.append(f"  expected: {ve['expected_result']}")
                if workflow.get('result_tables'):
                    lines.append(f"\nResult Tables:")
                    for rt in workflow['result_tables']:
                        lines.append(f"  - {rt.get('table','?')}: {rt.get('description','')}")
                        if rt.get('fields'):
                            lines.append(f"    fields: {rt['fields']}")
                        if rt.get('interpretation'):
                            lines.append(f"    interpret: {rt['interpretation']}")
                if workflow.get('common_pitfalls'):
                    lines.append(f"\nCommon Pitfalls:")
                    for p in workflow['common_pitfalls']:
                        lines.append(f"  - {p}")
                if workflow.get('notes'):
                    lines.append(f"\nNotes:")
                    for n in workflow.get('notes', []):
                        lines.append(f"  - {n}")
                return "\n".join(lines)
            else:
                # List available methods with name->title mapping so AI can pick the correct name
                _avail = []
                for _wk, _wv in WORKFLOWS.items():
                    if isinstance(_wv, dict) and _wv.get('name'):
                        _avail.append(f"{_wv['name']} = {_wv.get('title','')}")
                return f"No workflow found for '{method_name}'. You MUST use the 'name' field (not title) as analysis_name.\nAvailable methods (name = title):\n" + "\n".join(_avail[:40])
        elif tool_name == "auto_select_method":
            # Smart method recommendation: gather dataset info + all methods with workflow purpose
            user_intent = parameters.get("user_intent", "")
            # Get dataset info
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
                df_md = compress_as_markdown.compress_dataframe_info(df_info)
            except Exception:
                df_md = "(Dataset info unavailable)"
            # Get all methods
            methods = await asyncio.wait_for(iclient.get_analyses_method(), timeout=8)
            # Build method catalog with workflow purpose
            catalog_lines = ["=== Available Methods with Purpose ==="]
            try:
                modules = methods.get("modules", methods) if isinstance(methods, dict) else methods
                if isinstance(modules, list):
                    for module in modules:
                        mod_name = module.get("name", "")
                        analyses = module.get("analyses", [])
                        if analyses:
                            catalog_lines.append(f"\n## {mod_name}")
                            for a in analyses:
                                a_name = a.get("name", "")
                                a_title = a.get("title", "")
                                a_ns = a.get("ns", mod_name)
                                # Look up workflow purpose
                                wf = WORKFLOWS.get(a_name) or WORKFLOWS.get(f"{a_ns}/{a_name}")
                                purpose = wf.get("purpose", "") if wf else ""
                                scenario = ""
                                if wf:
                                    notes = wf.get("notes", [])
                                    if notes:
                                        scenario = notes[0][:120]
                                catalog_lines.append(f"  - {a_name} (ns={a_ns}): {a_title}")
                                if purpose:
                                    catalog_lines.append(f"    Purpose: {purpose[:150]}")
                                if scenario:
                                    catalog_lines.append(f"    Scenario: {scenario}")
            except Exception as e:
                catalog_lines.append(f"Error building catalog: {e}")
            intent_line = f"\n=== User Intent ===\n{user_intent}" if user_intent else ""
            return f"{df_md}\n{''.join(catalog_lines)}{intent_line}\n\nBased on the dataset and user intent above, recommend the most appropriate statistical method(s). For each recommendation, provide: method name, ns, why it fits, and required parameters."
        elif tool_name == "batch_analysis":
            # Execute multiple analyses in sequence
            analyses = parameters.get("analyses", [])
            if not analyses or not isinstance(analyses, list):
                return "Error: 'analyses' must be a non-empty list. Each item needs analysis_name, ns, and options."
            # Pre-flight: validate instance and data
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot run batch analysis: {check_msg}"
            results = []
            methods = await asyncio.wait_for(iclient.get_analyses_method(), timeout=8)
            for idx, ana in enumerate(analyses):
                analysis_name = ana.get("analysis_name", "")
                ns = ana.get("ns", "")
                options = ana.get("options", {})
                if not analysis_name or not ns:
                    results.append(f"[{idx+1}] {analysis_name or '?'}: SKIPPED (missing analysis_name or ns)")
                    continue
                # Resolve name
                resolved_name, resolved_ns = _resolve_analysis_name(methods, analysis_name, ns)
                # Convert options (reuse the same conversion logic as run_analysis)
                def _convert_option(key, value):
                    if isinstance(value, list):
                        if len(value) > 0 and all(isinstance(item, dict) for item in value):
                            nested_items = []
                            for i, group in enumerate(value):
                                group_opts = []
                                for gk, gv in group.items():
                                    if isinstance(gv, bool):
                                        group_opts.append({"key": gk, "value": int(gv), "type": "o"})
                                    elif isinstance(gv, int):
                                        group_opts.append({"key": gk, "value": gv, "type": "i"})
                                    elif isinstance(gv, float):
                                        group_opts.append({"key": gk, "value": gv, "type": "d"})
                                    else:
                                        group_opts.append({"key": gk, "value": str(gv), "type": "s"})
                                nested_items.append({"key": str(i), "type": "c", "value": {"hasNames": True, "options": group_opts}})
                            return {"key": key, "value": {"hasNames": False, "options": nested_items}, "type": "c"}
                        else:
                            nested = {"hasNames": False, "options": [{"key": str(i), "type": "s", "value": str(v)} for i, v in enumerate(value)]}
                            return {"key": key, "value": nested, "type": "c"}
                    elif isinstance(value, bool):
                        return {"key": key, "value": int(value), "type": "o"}
                    elif isinstance(value, int):
                        return {"key": key, "value": value, "type": "i"}
                    elif isinstance(value, float):
                        return {"key": key, "value": value, "type": "d"}
                    elif isinstance(value, dict):
                        if "options" in value and isinstance(value["options"], list):
                            for i, item in enumerate(value["options"]):
                                if isinstance(item, dict) and "key" not in item:
                                    item["key"] = str(i)
                        return {"key": key, "value": value, "type": "c"}
                    else:
                        return {"key": key, "value": str(value), "type": "s"}
                if isinstance(options, dict) and "options" in options and isinstance(options["options"], list):
                    opts_list = []
                    for item in options["options"]:
                        if isinstance(item, dict) and "key" in item and "value" in item:
                            opts_list.append(_convert_option(item["key"], item["value"]))
                        else:
                            opts_list.append(item)
                else:
                    opts_list = [_convert_option(k, v) for k, v in options.items()]
                request = {
                    "type": "ANALYSIS_CREATE_REQ",
                    "analysis_name": resolved_name,
                    "ns": resolved_ns,
                    "options": {"hasNames": True, "options": opts_list}
                }
                try:
                    result = await asyncio.wait_for(iclient.create_analysis(request), timeout=120)
                    md = compress_as_markdown.compress_results([result])
                    status = "OK" if result else "EMPTY"
                    results.append(f"[{idx+1}] {resolved_name} ({resolved_ns}): {status}\n{md[:500]}")
                except Exception as e:
                    results.append(f"[{idx+1}] {resolved_name} ({resolved_ns}): FAILED - {str(e)[:200]}")
            summary = f"Batch analysis complete: {len(results)} analyses executed.\n\n" + "\n---\n".join(results)
            return summary + "\n\nPlease summarize the batch results and highlight any failures or notable findings."
        elif tool_name == "compare_results":
            # Validate instance has data
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot compare results: {check_msg}"
            # Compare multiple analysis results side by side
            analysis_names = parameters.get("analysis_names", [])
            all_results = await asyncio.wait_for(iclient.get_results(), timeout=8)
            if not all_results:
                return "No analysis results available to compare. Run some analyses first."
            # Filter if specific names given
            if analysis_names and isinstance(analysis_names, list):
                filtered = []
                for r in all_results:
                    name = r.get("name", "") or r.get("analysisName", "")
                    if any(n.lower() in name.lower() for n in analysis_names):
                        filtered.append(r)
                compare_set = filtered if filtered else all_results
            else:
                compare_set = all_results
            # Build comparison
            lines = [f"=== Comparing {len(compare_set)} analysis results ===\n"]
            for idx, r in enumerate(compare_set):
                name = r.get("name", f"Analysis {idx+1}")
                ns = r.get("ns", "")
                lines.append(f"\n--- Analysis {idx+1}: {name} ({ns}) ---")
                md = compress_as_markdown.compress_results([r])
                lines.append(md[:800])
            lines.append("\n=== Comparison Guidance ===")
            lines.append("Compare the analyses above. For each key metric (p-value, effect size, confidence interval, etc.):")
            lines.append("1. List the value from each analysis in a table")
            lines.append("2. Identify which analysis gives the strongest/most significant result")
            lines.append("3. Note any contradictions or inconsistencies")
            lines.append("4. Provide a professional conclusion about which method is most appropriate")
            return "\n".join(lines)
        elif tool_name == "export_pdf":
            # Validate instance has data and results
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot export PDF: {check_msg}"
            # Use the active instance id (check_instance may have auto-switched
            # away from a stale/closed iid — using the original iid here would
            # navigate playwright to a non-existent page and time out).
            active_iid = iclient.instance_id or iid
            # Check if there are analysis results
            results = await asyncio.wait_for(iclient.get_results(), timeout=8)
            if not results:
                return "No analysis results to export. Please run an analysis first."
            # Export current results to PDF via UI automation (File -> Export -> PDF)
            filename = parameters.get("filename", "")
            full_url = f"http://{url}" if not url.startswith("http") else url
            result = await asyncio.wait_for(
                pdf_export_ui.export_pdf_via_ui(full_url, active_iid, filename),
                timeout=120
            )
            if result.get("success"):
                pdf_path = result['filepath']
                pdf_name = os.path.basename(pdf_path)
                # Construct download URL (LLM service is mapped to host port 8001)
                download_url = f"http://127.0.0.1:8001/exports/{pdf_name}"
                return f"PDF exported successfully!\n\n**Download link:** {download_url}\n\n**Filename:** {pdf_name}\n**Size:** {result.get('size_bytes', 0)} bytes\n\nClick the link above to download the PDF report to your computer. The PDF contains all current analysis results."
            else:
                return f"PDF export failed: {result.get('message', 'Unknown error')}\nYou can try manually using File -> Export in the iMast interface."
        elif tool_name == "export_results_csv":
            # Validate instance has data and results
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot export results CSV: {check_msg}"
            results = await asyncio.wait_for(iclient.get_results(), timeout=8)
            if not results:
                return "No analysis results to export. Please run an analysis first."
            analysis_names = parameters.get("analysis_names", [])
            filename = parameters.get("filename", "")
            csv_text = _extract_tables_as_csv(results, analysis_names if analysis_names else None)
            if not csv_text.strip():
                return "No tables found in the current analysis results to export as CSV."
            if not filename:
                filename = f"iMast_results_{int(time.time())}.csv"
            elif not filename.endswith(".csv"):
                filename += ".csv"
            exports_dir = os.path.join(_LLM_DIR, "exports")
            os.makedirs(exports_dir, exist_ok=True)
            filepath = os.path.join(exports_dir, filename)
            with open(filepath, "w", encoding="utf-8-sig", newline="") as f:
                f.write(csv_text)
            file_size = os.path.getsize(filepath)
            download_url = f"http://127.0.0.1:8001/exports/{filename}"
            line_count = csv_text.count("\n")
            return (
                f"Results exported to CSV successfully!\n\n"
                f"**Download link:** {download_url}\n\n"
                f"**Filename:** {filename}\n"
                f"**Size:** {file_size} bytes\n"
                f"**Approx. lines:** {line_count}\n\n"
                f"The CSV file contains all analysis result tables with section headers. "
                f"Open it in Excel or any spreadsheet application."
            )
        elif tool_name == "generate_plot":
            # Validate instance has data
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot generate plot: {check_msg}"
            # Use the active instance id (check_instance may have auto-switched)
            active_iid = iclient.instance_id or iid
            plot_type = parameters.get("plot_type", "").lower().strip()
            variables = parameters.get("variables", [])
            filename = parameters.get("filename", "")
            extra_options = parameters.get("extra_options", {})
            if not plot_type:
                return "Error: plot_type is required. Supported types: histogram, qqplot, boxplot, density, scatter."
            if not variables or not isinstance(variables, list):
                return "Error: 'variables' must be a non-empty list of column names."
            # Look up the plot type mapping
            plot_def = _PLOT_TYPE_MAP.get(plot_type)
            if not plot_def:
                supported = ", ".join(sorted(_PLOT_TYPE_MAP.keys()))
                return f"Error: Unsupported plot_type '{plot_type}'. Supported types: {supported}."
            # For scatter plot we need at least 2 variables
            if plot_type == "scatter" and len(variables) < 2:
                return "Error: scatter plot requires at least 2 variables (x and y)."
            # Build options from template
            base_options = plot_def["options_template"](variables, extra_options)
            # Merge extra options
            if isinstance(extra_options, dict):
                base_options.update({k: v for k, v in extra_options.items() if k not in base_options})
            # Resolve analysis name / ns
            analysis_name = plot_def["analysis_name"]
            ns = plot_def["ns"]
            try:
                methods = await asyncio.wait_for(iclient.get_analyses_method(), timeout=8)
                resolved_name, resolved_ns = _resolve_analysis_name(methods, analysis_name, ns)
                analysis_name, ns = resolved_name, resolved_ns
            except Exception:
                pass  # Fall back to defaults
            # Build the jamovi request
            opts_envelope = _jamovi_convert_options(base_options)
            request = {
                "type": "ANALYSIS_CREATE_REQ",
                "analysis_name": analysis_name,
                "ns": ns,
                "options": opts_envelope,
            }
            print(f"generate_plot request: {json.dumps(request, ensure_ascii=False)[:500]}")
            # Run the analysis
            try:
                result = await asyncio.wait_for(
                    iclient.create_analysis(request, wait_for_completion=True),
                    timeout=120
                )
            except imast.NoDataError as e:
                return f"Cannot generate plot: {str(e)}"
            except asyncio.TimeoutError:
                return f"Plot analysis timed out after 120 seconds."
            except Exception as e:
                return f"Plot analysis failed: {str(e)}"
            status = result.get("status", "")
            if "error" in status.lower():
                error_obj = result.get("error", {})
                error_msg = error_obj.get("message", "") or "Unknown plot error"
                error_cause = error_obj.get("cause", "")
                stacktrace = result.get("stacktrace", "")
                detail = f"Plot generation failed (status={status})."
                if error_msg:
                    detail += f" Message: {error_msg}"
                if error_cause:
                    detail += f" Cause: {error_cause}"
                if stacktrace:
                    detail += f"\nDetails: {str(stacktrace)[:300]}"
                detail += "\nPlease check variable assignments (variable exists and is correct type) and plot type."
                return detail
            # Now screenshot the plot from the results view
            full_url = f"http://{url}" if not url.startswith("http") else url
            if not filename:
                filename = f"iMast_{plot_type}_{int(time.time())}"
            plot_result = await asyncio.wait_for(
                plot_screenshot.capture_plot_via_ui(full_url, active_iid, filename),
                timeout=90
            )
            if plot_result.get("success"):
                png_name = os.path.basename(plot_result["filepath"])
                download_url = f"http://127.0.0.1:8001/exports/{png_name}"
                return (
                    f"Plot '{plot_type}' generated successfully!\n\n"
                    f"**Download link:** {download_url}\n\n"
                    f"**Filename:** {png_name}\n"
                    f"**Size:** {plot_result.get('size_bytes', 0)} bytes\n\n"
                    f"Variables: {', '.join(variables)}. Click the link above to view the plot image."
                )
            else:
                return (
                    f"Plot analysis ran, but screenshot failed: {plot_result.get('message', 'Unknown error')}\n"
                    f"The plot should be visible in the iMast results panel. You can also try export_pdf to capture it in a report."
                )
        elif tool_name == "export_data_csv":
            # Validate instance has data
            is_valid, check_msg = await asyncio.wait_for(iclient.check_instance(), timeout=15)
            if not is_valid:
                return f"Cannot export data CSV: {check_msg}"
            # Use the active instance id (check_instance may have auto-switched)
            active_iid = iclient.instance_id or iid
            filename = parameters.get("filename", "")
            if not filename:
                filename = f"iMast_data_{int(time.time())}"
            if not filename.endswith(".csv"):
                filename += ".csv"
            # Use UI automation to export data as CSV (File -> Export -> CSV data)
            full_url = f"http://{url}" if not url.startswith("http") else url
            # Reuse the PDF export UI module's browser infrastructure but
            # trigger CSV export.  We implement a lightweight version here.
            try:
                from playwright.async_api import async_playwright
            except ImportError:
                return "Playwright not installed; cannot export data CSV via UI."
            exports_dir = os.path.join(_LLM_DIR, "exports")
            os.makedirs(exports_dir, exist_ok=True)
            target_filepath = os.path.join(exports_dir, filename)
            try:
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
                    page_url = f"{full_url}/{active_iid}/"
                    await page.goto(page_url, wait_until="domcontentloaded", timeout=30000)
                    await page.wait_for_timeout(5000)
                    # Dismiss overlays
                    await page.evaluate("""(function() {
                        document.querySelectorAll('jmv-infobox').forEach(function(el){ el.remove(); });
                        document.querySelectorAll('.el-message-box, .el-overlay-message-box').forEach(function(box){
                            var btns = box.querySelectorAll('button');
                            for (var i = 0; i < btns.length; i++) {
                                var t = (btns[i].textContent || '').trim().toLowerCase();
                                if (t === 'close' || t === 'ok' || t === '确定' || t === '关闭' || t === '×') {
                                    btns[i].click(); break;
                                }
                            }
                        });
                    })()""")
                    await page.wait_for_timeout(1000)
                    # Click File menu
                    await page.evaluate("""(function() {
                        var items = document.querySelectorAll('.el-sub-menu__title');
                        for (var i = 0; i < items.length; i++) {
                            if (items[i].textContent.trim() === 'File') { items[i].click(); break; }
                        }
                    })()""")
                    await page.wait_for_timeout(1500)
                    # Click Export
                    await page.evaluate("""(function() {
                        var items = document.querySelectorAll('.el-menu-item');
                        for (var i = 0; i < items.length; i++) {
                            if (items[i].textContent.trim() === 'Export') { items[i].click(); break; }
                        }
                    })()""")
                    await page.wait_for_timeout(2500)
                    # Select CSV option in the export dialog (look for CSV / Data radio)
                    csv_selected = await page.evaluate("""(function() {
                        var radios = document.querySelectorAll('.el-radio, .el-radio-button');
                        for (var i = 0; i < radios.length; i++) {
                            var t = (radios[i].textContent || '').trim().toLowerCase();
                            if (t.indexOf('csv') >= 0 || t.indexOf('data') >= 0) {
                                radios[i].click();
                                return true;
                            }
                        }
                        return false;
                    })()""")
                    await page.wait_for_timeout(500)
                    # Click Export button in dialog
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
                    except Exception as e:
                        await browser.close()
                        return (f"Data CSV export failed: no download triggered. "
                                f"CSV option found: {csv_selected}. "
                                f"You may need to export data manually from the spreadsheet view.")
                    if download:
                        await download.save_as(target_filepath)
                    await browser.close()
                if os.path.exists(target_filepath):
                    size = os.path.getsize(target_filepath)
                    if size > 100:
                        download_url = f"http://127.0.0.1:8001/exports/{filename}"
                        return (
                            f"Dataset exported to CSV successfully!\n\n"
                            f"**Download link:** {download_url}\n\n"
                            f"**Filename:** {filename}\n"
                            f"**Size:** {size} bytes"
                        )
                    return f"Data CSV file seems empty (size={size}). The export may have failed."
                return "Data CSV file was not saved."
            except Exception as e:
                return f"Data CSV export failed: {str(e)}"
        elif tool_name == "import_data":
            # Import a data file into iMast via HTTP upload (bypasses native file dialog)
            file_path = parameters.get("file_path", "")
            file_name = parameters.get("file_name", "")
            if not file_path:
                # If no path given, try /data/ directory with the file_name
                if file_name:
                    file_path = os.path.join("/data", file_name)
                else:
                    return "Please specify file_path (absolute path in container, e.g. /data/iris.csv) or file_name (must exist in /data/ directory)."
            # Security: restrict to /data/ directory to prevent arbitrary file access
            if not file_path.startswith("/data/") and not file_path.startswith("/tmp/"):
                return f"Security: file_path must be under /data/ or /tmp/ directory. Got: {file_path}"
            if not os.path.exists(file_path):
                # List available files to help user
                data_dir = "/data"
                if os.path.isdir(data_dir):
                    files = os.listdir(data_dir)
                    file_list = "\n".join(f"  - {f}" for f in sorted(files))
                    return f"File not found: {file_path}\n\nAvailable files in /data/:\n{file_list}"
                return f"File not found: {file_path}"
            # Upload via HTTP to jamovi server
            try:
                result = await asyncio.wait_for(
                    asyncio.to_thread(iclient.import_data_file, file_path),
                    timeout=60
                )
            except Exception as e:
                return f"Import failed: {str(e)}"
            if result.get("success"):
                new_iid = result.get("instance_id", "")
                title = result.get("title", file_name or os.path.basename(file_path))
                msg = result.get("message", "Import successful")
                new_url = result.get("url", "")
                info_line = ""
                if new_iid:
                    info_line = f"\n\n**Dataset title:** {title}\n\nYou can now run analyses on this dataset. Use get_dataset to view variables."
                    # Add navigation marker for frontend to auto-switch to new instance
                    if new_url:
                        info_line += f"\n\n[NAVIGATE_TO: {new_url}]"
                return f"✅ {msg}{info_line}"
            else:
                return f"❌ Import failed: {result.get('message', 'Unknown error')}"
        elif tool_name == "list_available_data":
            # Only return current instance info, do NOT list all files in filesystem.
            # This prevents AI from getting confused by too many files.
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
                if df_info and df_info.get("columns"):
                    ncols = len(df_info["columns"])
                    row_count = 0
                    if df_info["columns"] and isinstance(df_info["columns"][0], dict):
                        row_count = df_info["columns"][0].get("row_count", 0)
                    return f"Current dataset: {row_count} rows × {ncols} columns. This is what you see in the data interface. Use get_dataset to see details."
                else:
                    return "No dataset currently loaded. Please open or upload a data file first."
            except Exception as e:
                return f"Cannot access current dataset: {str(e)}"
        elif tool_name == "auto_detect_column_types":
            # Auto-detect and set measure_type for all columns
            try:
                result = await asyncio.wait_for(
                    iclient.auto_detect_column_types(), timeout=30
                )
                if result.get("success"):
                    total = result.get("total_columns", 0)
                    changed = result.get("changed_columns", 0)
                    details = result.get("details", [])
                    lines = [f"Column type auto-detection completed.",
                             f"Total columns: {total}",
                             f"Columns changed: {changed}",
                             ""]
                    for d in details:
                        status = "changed" if d.get("changed") else "unchanged"
                        lines.append(f"- {d.get('name')}: {d.get('old_type')} -> {d.get('new_type')} ({status})")
                    return "\n".join(lines)
                else:
                    return f"Auto-detect failed: {result.get('message', 'Unknown error')}"
            except Exception as e:
                return f"Failed to auto-detect column types: {str(e)}"
        elif tool_name == "set_column_type":
            # Manually set measure_type for specified columns
            columns = parameters.get("columns", [])
            if not columns:
                return "Error: No columns specified. Provide a list of {\"name\": \"column_name\", \"measure_type\": \"NOMINAL|ORDINAL|CONTINUOUS|ID\"}."
            try:
                result = await asyncio.wait_for(
                    iclient.set_column_type(columns), timeout=30
                )
                if result.get("success"):
                    details = result.get("details", [])
                    changed = result.get("changed_columns", 0)
                    lines = [f"Column type setting completed. Columns changed: {changed}", ""]
                    for d in details:
                        status = "changed" if d.get("changed") else "unchanged"
                        err = f" (error: {d.get('error')})" if d.get("error") else ""
                        lines.append(f"- {d.get('name')}: {d.get('old_type')} -> {d.get('new_type')} ({status}){err}")
                    return "\n".join(lines)
                else:
                    return f"Failed to set column types: {result.get('message', 'Unknown error')}"
            except Exception as e:
                return f"Failed to set column types: {str(e)}"
        elif tool_name == "infer_and_set_variable_types":
            # Infer variable types based on analysis workflow and auto-set them
            analysis_name = parameters.get("analysis_name", "")
            ns = parameters.get("ns", "")
            variable_hints = parameters.get("variable_hints", {})
            if not analysis_name or not ns:
                return "Error: analysis_name and ns are required."
            # Step 1: Get workflow
            wf = WORKFLOWS.get(f"{ns}/{analysis_name}") or WORKFLOWS.get(analysis_name)
            if not wf:
                return f"Error: Workflow not found for {ns}/{analysis_name}. Use get_workflow to verify the method name."
            # Step 2: Get dataset info
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
            except Exception as e:
                return f"Failed to get dataset info: {str(e)}"
            columns = df_info.get("columns", [])
            if not columns:
                return "Error: No dataset loaded. Please import data first."
            # Step 3: Extract expected variable types from workflow
            expected_types = {}  # workflow_param_name -> expected measure_type
            for p in wf.get('parameters', []):
                ptype = p.get('type', '')
                ptitle = p.get('title', '').lower()
                pname = p.get('name', '').lower()
                if ptype in ('Variable', 'Variables'):
                    if 'id' in ptitle or 'id' in pname:
                        expected_types[pname] = 'NOMINAL'
                    elif 'position' in ptitle or 'pos' in pname:
                        expected_types[pname] = 'CONTINUOUS'
                    elif 'group' in ptitle or 'factor' in ptitle or 'category' in ptitle:
                        expected_types[pname] = 'NOMINAL'
                # Pairs type parameters (e.g., csxy, rmxy) contain continuous measurement values
                # Their variable names are specified in options at analysis time, not pre-inferred here
            # Step 4: Infer target type for each column
            type_mapping = {}  # column_name -> {old_type, new_type, reason}
            for col in columns:
                col_name = col.get('name', '')
                if not col_name:
                    continue
                col_name_lower = col_name.lower().replace(' ', '_').replace('-', '_')
                current_type = col.get('measure_type', 'CONTINUOUS')
                data_type = col.get('data_type', 'TEXT')
                # levels count indicates number of unique values for categorical columns
                unique_count = len(col.get('levels', [])) if col.get('levels') else 0
                target_type = current_type
                reason = "保持原类型"
                # 4.1 User hints have highest priority
                if col_name in variable_hints:
                    hint_type = variable_hints[col_name]
                    if hint_type in ('NOMINAL', 'ORDINAL', 'CONTINUOUS', 'ID'):
                        target_type = hint_type
                        reason = "用户指定"
                # 4.2 Match workflow parameter names (fuzzy matching)
                else:
                    for wf_param, wf_type in expected_types.items():
                        param_keywords = [k for k in wf_param.split('_') if len(k) > 1]
                        if wf_param in col_name_lower or any(k in col_name_lower for k in param_keywords):
                            target_type = wf_type
                            reason = f"匹配工作流参数'{wf_param}'"
                            break
                # 4.3 Fallback: heuristic rules based on data characteristics
                if target_type == current_type and reason == "保持原类型":
                    if data_type.upper() == 'TEXT':
                        # Text columns are typically categorical (NOMINAL) or identifiers (ID)
                        if unique_count > 50 and any(k in col_name_lower for k in ['id', 'code', 'no', 'number', 'sample', 'specimen']):
                            target_type = 'ID'
                            reason = "文本+ID关键词+多唯一值→ID"
                        else:
                            target_type = 'NOMINAL'
                            reason = "文本类型→NOMINAL"
                    elif data_type.upper() in ('INTEGER', 'DECIMAL', 'NUMBER'):
                        # Numeric columns: check if they look like IDs
                        if unique_count > 50 and any(k in col_name_lower for k in ['id', 'code', 'no', 'number']):
                            target_type = 'ID'
                            reason = "数值+ID关键词+多唯一值→ID"
                        else:
                            target_type = 'CONTINUOUS'
                            reason = "数值类型→CONTINUOUS"
                # 4.4 Only record columns that need changing
                if target_type != current_type:
                    type_mapping[col_name] = {
                        'old_type': current_type,
                        'new_type': target_type,
                        'reason': reason
                    }
            # Step 5: If nothing to change, return early
            if not type_mapping:
                return f"All variable types are already appropriate for {analysis_name}. No changes needed.\n\nCurrent types:\n" + "\n".join(f"- {c.get('name')}: {c.get('measure_type')}" for c in columns)
            # Step 6: Batch set column types
            columns_to_set = [{'name': name, 'measure_type': info['new_type']} for name, info in type_mapping.items()]
            try:
                result = await asyncio.wait_for(iclient.set_column_type(columns_to_set), timeout=30)
            except Exception as e:
                return f"Failed to set variable types: {str(e)}"
            # Step 7: Format and return result
            if result.get("success"):
                lines = [
                    f"Variable type auto-inference completed for analysis: {analysis_name} ({ns})",
                    f"Columns analyzed: {len(columns)}",
                    f"Columns changed: {len(type_mapping)}",
                    "",
                    "Changes:"
                ]
                for name, info in type_mapping.items():
                    lines.append(f"  - {name}: {info['old_type']} → {info['new_type']}  [{info['reason']}]")
                lines.append("")
                lines.append("All variable types are now set according to the analysis requirements. You may proceed with run_analysis.")
                return "\n".join(lines)
            else:
                return f"Failed to set variable types: {result.get('message', 'Unknown error')}"
        elif tool_name == "get_variable_detail":
            # Get detailed info for a specific variable
            var_name = parameters.get("variable_name", "")
            df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
            columns = df_info.get("columns", [])
            if not var_name:
                # Return summary of all variables
                lines = [f"Dataset: {df_info.get('title', 'Unknown')}", f"Total variables: {len(columns)}", ""]
                for col in columns:
                    lines.append(f"- {col.get('name', '?')}: type={col.get('measure_type', '?')}, data_type={col.get('data_type', '?')}, rows={col.get('row_count', '?')}")
                    if col.get("levels"):
                        lines.append(f"  levels: {', '.join(str(l) for l in col['levels'][:10])}")
                    if col.get("description"):
                        lines.append(f"  description: {col['description']}")
                return "\n".join(lines)
            # Find specific variable
            for col in columns:
                if col.get("name", "").lower() == var_name.lower():
                    lines = [f"Variable: {col.get('name', '?')}"]
                    lines.append(f"  column_type: {col.get('column_type', '?')}")
                    lines.append(f"  data_type: {col.get('data_type', '?')}")
                    lines.append(f"  measure_type: {col.get('measure_type', '?')}")
                    lines.append(f"  row_count: {col.get('row_count', '?')}")
                    lines.append(f"  active: {col.get('active', '?')}")
                    if col.get("levels"):
                        lines.append(f"  levels: {', '.join(str(l) for l in col['levels'])}")
                    if col.get("description"):
                        lines.append(f"  description: {col['description']}")
                    if col.get("formula"):
                        lines.append(f"  formula: {col['formula']}")
                    return "\n".join(lines)
            return f"Variable '{var_name}' not found. Available variables: {', '.join(c.get('name', '?') for c in columns)}"
        elif tool_name == "check_missing_values":
            # Check missing values in the dataset
            df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=8)
            columns = df_info.get("columns", [])
            total_rows = max((col.get("row_count", 0) for col in columns), default=0)
            lines = [f"Missing Value Check for: {df_info.get('title', 'Unknown')}", f"Total rows: {total_rows}", ""]
            # Note: We can't directly get missing count from INFO_REQ,
            # but we can report variable types and suggest running descriptives
            lines.append("Note: To get exact missing value counts, run Descriptives analysis.")
            lines.append("")
            for col in columns:
                name = col.get("name", "?")
                mtype = col.get("measure_type", "?")
                lines.append(f"- {name} ({mtype}): {col.get('row_count', '?')} rows")
            return "\n".join(lines)
        elif tool_name == "get_analysis_by_name":
            # Get results for a specific analysis by name
            analysis_name = parameters.get("analysis_name", "")
            results = await asyncio.wait_for(iclient.get_results(), timeout=8)
            if not analysis_name:
                # List all analyses
                lines = [f"Total analyses: {len(results)}", ""]
                for i, r in enumerate(results):
                    lines.append(f"{i+1}. {r.get('name', '?')} (ns={r.get('ns', '?')}) - status={r.get('status', '?')}")
                return "\n".join(lines)
            # Find specific analysis
            for r in results:
                if r.get("name", "").lower() == analysis_name.lower():
                    md = compress_as_markdown.compress_results([r])
                    return f"Analysis: {r.get('name', '?')} (ns={r.get('ns', '?')})\nStatus: {r.get('status', '?')}\n\n{md}"
            available = [r.get("name", "?") for r in results]
            return f"Analysis '{analysis_name}' not found. Available analyses: {', '.join(available)}"
        elif tool_name == "match_parameters":
            # Auto-match analysis parameters based on variable names
            workflow_name = parameters.get("workflow_name", "")
            # Get current dataset variables
            try:
                df_info = await asyncio.wait_for(iclient.get_dataframe_info(), timeout=10)
                var_names = [col.get("name", "") for col in df_info.get("columns", [])]
            except Exception as e:
                return f"Cannot get dataset variables: {str(e)}"
            # Select parameter map based on workflow name
            wf_lower = workflow_name.lower()
            options = []
            if "commutability" in wf_lower or "ep14" in wf_lower:
                param_map = {
                    "csid": ["CS ID", "临床样本", "样本编号", "csid"],
                    "csx": ["CS MPx", "CS X", "测量X", "csx"],
                    "csy": ["CS MPy", "CS Y", "测量Y", "csy"],
                    "rmid": ["RM ID", "参考物质", "物质编号", "rmid"],
                    "rmp": ["RM Pos", "RM Position", "物质位置", "rmp"],
                    "rmx": ["RM MPx", "RM X", "rmx"],
                    "rmy": ["RM MPy", "RM Y", "rmy"],
                }
                method_desc = "Commutability (EP14-A3)"
            elif "precision" in wf_lower or "ep15" in wf_lower:
                param_map = {
                    "sample": ["Sample", "样本", "sample_id", "id"],
                    "run": ["Run", "批次", "run"],
                    "level": ["Level", "水平", "浓度水平", "level"],
                    "result": ["Result", "测量值", "结果", "meas", "value"],
                }
                method_desc = "Precision (EP15-A3)"
            elif "methodcomparison" in wf_lower or "ep9" in wf_lower or "comparison" in wf_lower:
                param_map = {
                    "sample": ["Sample", "样本", "sample_id", "id"],
                    "x": ["X", "方法1", "Method1", "method_x"],
                    "y": ["Y", "方法2", "Method2", "method_y"],
                }
                method_desc = "Method Comparison (EP9-A3)"
            elif "linearity" in wf_lower or "ep6" in wf_lower or "linear" in wf_lower:
                param_map = {
                    "sample": ["Sample", "样本", "sample_id"],
                    "concentration": ["Conc", "浓度", "理论值", "expected"],
                    "result": ["Result", "测量值", "实测值", "measured", "response"],
                }
                method_desc = "Linearity (EP6-A2)"
            else:
                param_map = {
                    "id": ["ID", "编号", "id"],
                    "group": ["Group", "分组", "group"],
                    "x": ["X", "x"],
                    "y": ["Y", "y"],
                }
                method_desc = "Generic"
            matched_params = {}
            for param, patterns in param_map.items():
                for pattern in patterns:
                    matches = [v for v in var_names if pattern.lower() in v.lower()]
                    if matches:
                        if len(matches) == 1:
                            matched_params[param] = matches[0]
                        else:
                            matched_params[param] = matches
                        break
            for key, value in matched_params.items():
                if isinstance(value, list):
                    options.append({"key": key, "value": value, "type": "c"})
                else:
                    options.append({"key": key, "value": value, "type": "s"})
            return (
                f"Auto-matched parameters for {method_desc}:\n"
                f"Matched: {list(matched_params.keys())}\n"
                f"Options: {options}\n\n"
                f"Use these options with run_analysis to run the analysis."
            )
        else:
            return f"Unknown tool: {tool_name}"
    except Exception as e:
        import traceback
        print(f"Tool execution error for {tool_name}: {e}")
        print(traceback.format_exc())
        return f"Tool '{tool_name}' failed: {str(e)}. Please make sure you have an open dataset and analysis in iMast."
prompt = (
    "【重要】请始终用中文回答用户的问题，包括思考过程和最终回答。\n"
        "你是 iMAST（可靠的医学分析与统计工具集，Intelligent Medical Analysis and Statistics Toolkits）中的专业AI助手，"
    "专注于体外诊断（IVD）领域的统计分析，包括精密度、正确度、互通性、携带污染、线性、检出限等IVD专用评估方法。"
    "你是iMast统计分析软件的AI助手。你具备深厚的统计学知识和IVD行业经验。\n\n"
    "【工具调用格式】当需要调用工具时，输出一行 #FunctionCall，下一行输出JSON：\n"
    '{"name": "工具名", "parameters": {"参数名": "参数值"}}\n\n'
    "【重要规则——必须遵守】\n"
    "1. 如果工具结果中包含 [NAVIGATE_TO: xxx] 标记，必须原样输出到你的回复中，不要删除、不要转换、不要省略。这个标记用于让界面自动跳转到新数据集。\n"
    "2. 不要说自己无法做某事——先检查工具列表，有对应工具就直接调用\n"
    "3. 工具调用后如果需要继续，就继续调用下一个工具，不要停下来问用户\n\n"
    "【分析工作流——你是项目经理，不是操作工】\n"
    "你可以自己组合工具，根据实际情况灵活调整，而不是机械走流程。\n"
    "每一步都要做深度智能判断：这一步的结果怎么样？符不符合预期？下一步该怎么做？\n\n"
    "【智能判断指南】\n"
    "不要机械地走流程。每一步做完后，停下来想一想：\n"
    "- 这个结果对不对？符合我的预期吗？\n"
    "- 有没有什么问题？比如数据质量差、变量类型不对、参数匹配错了？\n"
    "- 如果有问题，我应该怎么调整？要不要先修复再继续？\n"
    "- 如果没问题，下一步应该做什么？\n\n"
    "【推荐分析流程】（根据实际情况灵活调整，不要机械执行）\n"
    "步骤1：了解当前数据 → 调用 get_dataset\n"
    "  → 智能判断：\n"
    "    - 数据有没有加载成功？行数对不对？\n"
    "    - 变量名是什么样的？是不是我要的那种数据？\n"
    "    - 变量类型对不对？ID 类变量是 nominal 还是 ID？测量值是 continuous 吗？\n"
    "    - 如果变量类型不对，要不要先修正？\n\n"
    "步骤2：数据质量检查 → 调用 check_missing_values 或 infer_and_set_variable_types\n"
    "  → 智能判断：\n"
    "    - 有没有缺失值？缺失多不多？要不要处理？\n"
    "    - 变量类型设置对不对？CS ID 是 nominal 吗？CS MPx1 是 continuous 吗？\n"
    "    - 如果变量类型不对，要不要手动修正？\n\n"
    "步骤3：选择分析方法 → 调用 get_workflow\n"
    "  → 智能判断：\n"
    "    - 用户要做什么分析？是不是这个方法？\n"
    "    - 这个方法需要哪些参数？我的数据里有没有对应的变量？\n"
    "    - 如果没有对应的变量，要不要告诉用户？\n\n"
    "步骤4：匹配参数 → 调用 match_parameters\n"
    "  → 智能判断：\n"
    "    - 自动匹配的参数对不对？CS ID 对应对了吗？\n"
    "    - 有没有匹配错的变量？比如把 RM ID 当成了 CS ID？\n"
    "    - 需不需要手动调整参数？\n\n"
    "步骤5：运行分析 → 调用 run_analysis\n"
    "  → 智能判断：\n"
    "    - 分析成功了吗？有没有报错？\n"
    "    - 如果报错了，是什么原因？要不要调整参数重新运行？\n"
    "    - 如果成功了，结果是什么样的？\n\n"
    "步骤6：解读结果 → 调用 interpret_result\n"
    "  → 智能判断：\n"
    "    - 结果是什么意思？符合预期吗？\n"
    "    - 有没有异常的结果？比如 R 值特别低？\n"
    "    - 需不需要告诉用户注意什么？\n\n"
    "【重要】\n"
    "不要机械地走流程！每一步都要思考，根据实际情况调整。\n"
    "如果发现问题，先停下来修复问题，再继续下一步。\n"
    "这样用户才能得到准确的分析结果。\n\n"
    "【核心工具列表】\n"
    "数据相关：get_dataset(查看当前数据)、list_available_data(查看当前数据状态)、check_missing_values(检查缺失值)\n"
    "变量相关：infer_and_set_variable_types(自动设置变量类型)、set_column_type(手动设置变量类型)\n"
    "分析相关：list_analyses(列出分析方法)、get_workflow(获取工作流)、match_parameters(自动匹配参数)、run_analysis(执行分析)、interpret_result(解读结果)\n\n"
    "【重要规则】\n"
    "1. 不要说自己无法做某事——先检查工具列表，有对应工具就直接调用\n"
    "2. 执行分析前必须先调用 get_workflow 获取参数清单\n"
    "3. run_analysis 的 analysis_name 用 name 字段，不用 title\n"
    "4. 工具调用后如果需要继续，就继续调用下一个工具，不要停下来问用户\n"
    "5. 只有当信息不足（比如不知道用什么分析方法）时，才向用户提问\n"
)
# Tool definitions
TOOLS = [
    {
        "name": "get_dataset",
        "description": "获取当前数据集的信息，包括变量名、数据类型、测量类型、样本量、变量描述和分类水平",
        "parameters": {}
    },
    {
        "name": "run_workflow",
        "description": "一键执行完整分析工作流：自动导入数据（可选）、自动修改变量类型、自动匹配参数、执行分析。这是推荐的分析方式，只需调用一次即可完成完整分析流程。参数：workflow_name（分析方法名）、data_file（数据文件路径，可选）、ns（模块名，可选）",
        "parameters": {
            "workflow_name": {"type": "string", "description": "分析方法名称，如 CommutabilityEP14A3, PrecisionA15, MethodComparisonA9"},
            "data_file": {"type": "string", "description": "数据文件路径（可选），如 /data/u_1.csv 或 /tmp/imast_uploads/e_15.csv"},
            "ns": {"type": "string", "description": "模块名（可选），如 ReferenceMaterial, Precision"}
        }
    },
    {
        "name": "interpret_result",
        "description": "解读当前页面的统计分析结果，包括P值、置信区间、效应量等的专业解释，以及结果是否显著、是否符合预期的判断",
        "parameters": {}
    },
    {
        "name": "generate_report",
        "description": "生成完整的统计分析报告，包含分析方法描述、结果表格、专业解读和结论建议",
        "parameters": {
            "title": {"type": "string", "description": "报告标题，可选", "default": "Statistical Analysis Report"}
        }
    },
    {
        "name": "list_analyses",
        "description": "列出iMast中所有可用的统计分析方法，按模块分类显示，帮助用户了解可以做哪些分析",
        "parameters": {}
    },
    {
        "name": "run_analysis",
        "description": "执行指定的统计分析。需要提供分析名称(analysis_name)、模块名(ns)和分析选项(options)。options中包含变量名和参数值，例如{'x': 'Var1', 'y': 'Var2', 'alpha': 0.05}",
        "parameters": {
            "analysis_name": {"type": "string", "description": "分析方法名称，例如 SpearmanRankCorrelation, Descriptives, TTestIndSamples"},
            "ns": {"type": "string", "description": "模块/命名空间，例如 Regression, Descriptive, ANOVA"},
            "options": {"type": "object", "description": "分析选项，包含变量分配和参数设置，例如 {'x': 'Age', 'y': 'Score', 'alpha': 0.05}"}
        }
    },
    {
        "name": "match_parameters",
        "description": "自动匹配分析参数。根据当前数据集的变量名和指定的分析方法，自动匹配对应的参数（如 CS ID、CS MPx、RM Pos 等），返回可以直接用于 run_analysis 的 options。",
        "parameters": {
            "workflow_name": {"type": "string", "description": "分析方法名称，例如 CommutabilityEP14A3, PrecisionEP15A3, MethodComparisonEP9A3, LinearityEP6A2"}
        }
    },
    {
        "name": "describe_method",
        "description": "描述某个统计分析方法的用途、适用场景和参数说明，帮助用户选择合适的分析方法",
        "parameters": {
            "method_name": {"type": "string", "description": "分析方法名称"},
            "ns": {"type": "string", "description": "模块/命名空间，可选"}
        }
    },
    {
        "name": "get_workflow",
        "description": "获取指定统计方法的详细操作工作流，包括参数清单、操作步骤、结果解读和调用示例。执行任何分析前必须先调用此工具获取工作流",
        "parameters": {
            "method_name": {"type": "string", "description": "分析方法名称，例如 ArithmeticMeanDirect, ttestIS"},
            "ns": {"type": "string", "description": "模块/命名空间，例如 Descriptive, jmv, Regression"}
        }
    },
    {
        "name": "auto_select_method",
        "description": "智能推荐统计方法：根据用户的分析需求和当前数据集，自动推荐最合适的统计方法。返回数据集摘要和所有可用方法的用途说明，帮助选择最佳方法",
        "parameters": {
            "user_intent": {"type": "string", "description": "用户的分析需求描述，例如'比较两组均值是否有差异'、'分析变量间的相关性'"}
        }
    },
    {
        "name": "batch_analysis",
        "description": "批量执行多个统计分析。一次调用可执行多个分析，每个分析需指定analysis_name、ns和options。适用于需要同时运行多种方法对比的场景",
        "parameters": {
            "analyses": {"type": "array", "description": "分析列表，每个元素为 {'analysis_name': '...', 'ns': '...', 'options': {...}}", "items": {"type": "object"}}
        }
    },
    {
        "name": "compare_results",
        "description": "对比多个分析结果：提取各分析的关键指标（p值、效应量、置信区间等），生成对比表格，帮助判断哪种方法最适合",
        "parameters": {
            "analysis_names": {"type": "array", "description": "要对比的分析名称列表，可选。为空则对比当前所有分析结果", "items": {"type": "string"}}
        }
    },
    {
        "name": "export_pdf",
        "description": "将当前所有统计分析结果导出为PDF报告文件。导出的PDF包含完整的分析表格和结果",
        "parameters": {
            "filename": {"type": "string", "description": "输出PDF文件名（不含.pdf后缀），可选。默认自动生成时间戳文件名"}
        }
    },
    {
        "name": "get_variable_detail",
        "description": "获取数据集中特定变量的详细信息，包括变量类型、测量级别、分类水平、描述、行数等。不指定variable_name时返回所有变量的摘要",
        "parameters": {
            "variable_name": {"type": "string", "description": "变量名称，可选。为空时返回所有变量摘要"}
        }
    },
    {
        "name": "check_missing_values",
        "description": "检查数据集的缺失值情况，列出所有变量及其类型和行数。建议结合Descriptives分析获取精确的缺失值统计",
        "parameters": {}
    },
    {
        "name": "get_analysis_by_name",
        "description": "获取特定分析的详细结果。不指定analysis_name时列出所有已运行的分析。用于查看某个分析的具体结果表格",
        "parameters": {
            "analysis_name": {"type": "string", "description": "分析名称，可选。为空时列出所有分析"}
        }
    },
    {
        "name": "export_results_csv",
        "description": "将当前所有统计分析结果中的表格导出为CSV文件。解析分析结果JSON中的表格数据，转为CSV格式保存，可直接用Excel打开。可指定导出特定分析的结果",
        "parameters": {
            "filename": {"type": "string", "description": "输出CSV文件名（不含.csv后缀），可选。默认自动生成时间戳文件名"},
            "analysis_names": {"type": "array", "description": "要导出的分析名称列表，可选。为空则导出所有分析结果", "items": {"type": "string"}}
        }
    },
    {
        "name": "generate_plot",
        "description": "生成统计图表并导出为PNG图片。支持直方图(histogram)、Q-Q图(qqplot)、箱线图(boxplot)、密度图(density)和散点图(scatter)。自动运行对应的绘图分析并截图",
        "parameters": {
            "plot_type": {"type": "string", "description": "图表类型：histogram（直方图）、qqplot（Q-Q图）、boxplot（箱线图）、density（密度图）、scatter（散点图）"},
            "variables": {"type": "array", "description": "变量名列表。histogram/qqplot/boxplot/density指定1个或多个变量；scatter需要2个变量（x和y）", "items": {"type": "string"}},
            "filename": {"type": "string", "description": "输出PNG文件名（不含.png后缀），可选。默认自动生成"},
            "extra_options": {"type": "object", "description": "额外的绘图选项，可选。如散点图可加group变量等"}
        }
    },
    {
        "name": "export_data_csv",
        "description": "将当前数据集的原始数据导出为CSV文件。通过UI自动化操作File->Export导出数据",
        "parameters": {
            "filename": {"type": "string", "description": "输出CSV文件名（不含.csv后缀），可选。默认自动生成时间戳文件名"}
        }
    },
    {
        "name": "import_data",
        "description": "导入数据文件到iMast中，创建新的分析实例并加载数据。支持CSV、OMV、XLSX、SPSS(.sav)等格式。文件必须位于容器内可访问的路径，如/data/目录下的测试数据。导入后自动切换到新实例。",
        "parameters": {
            "file_path": {"type": "string", "description": "容器内数据文件的绝对路径，例如 /data/iris.csv 或 /data/precision_study.omv"},
            "file_name": {"type": "string", "description": "文件名（可选，用于显示），例如 iris.csv"}
        }
    },
    {
        "name": "list_available_data",
        "description": "列出容器内/data/目录下所有可用的测试数据文件，包括文件名、大小和格式。帮助用户了解可以导入哪些数据。",
        "parameters": {}
    },
    {
        "name": "auto_detect_column_types",
        "description": "自动识别当前数据集中所有列的数据类型（连续型、名义型、ID型）并设置测量类型。在导入数据后调用此工具，确保每列都有正确的数据类型，避免后续分析出错。",
        "parameters": {}
    },
    {
        "name": "set_column_type",
        "description": "手动将指定变量设置为指定的测量级别（类型）。当需要精确控制某个变量的类型时使用，例如把ID列改为NOMINAL、把连续变量改为分类变量等。支持 NOMINAL（名义/分类）、ORDINAL（有序）、CONTINUOUS（连续）、ID 四种类型。",
        "parameters": {
            "type": "object",
            "properties": {
                "columns": {
                    "type": "array",
                    "description": "要修改的列列表，每列包含 name（列名）和 measure_type（目标类型：NOMINAL/ORDINAL/CONTINUOUS/ID）",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string", "description": "变量名"},
                            "measure_type": {"type": "string", "description": "目标测量级别：NOMINAL、ORDINAL、CONTINUOUS 或 ID"}
                        },
                        "required": ["name", "measure_type"]
                    }
                }
            },
            "required": ["columns"]
        }
    },
    {
        "name": "infer_and_set_variable_types",
        "description": "根据指定分析方法的工作流，自动推断数据集中每个变量应有的测量类型，并批量设置。在导入数据后、执行分析前调用此工具，确保变量类型符合分析方法要求（如ID列改为NOMINAL、测量值保持CONTINUOUS）。推断依据：工作流参数定义 + 变量名语义 + 数据特征。",
        "parameters": {
            "type": "object",
            "properties": {
                "analysis_name": {
                    "type": "string",
                    "description": "分析方法名称，例如 CommutabilityEP14A3, TTestIndSamples"
                },
                "ns": {
                    "type": "string",
                    "description": "模块/命名空间，例如 ReferenceMaterial, jmv, ANOVA"
                },
                "variable_hints": {
                    "type": "object",
                    "description": "可选。用户指定的变量类型提示，例如 {'CS ID': 'NOMINAL', 'Age': 'CONTINUOUS'}。推断时优先参考这些提示"
                }
            },
            "required": ["analysis_name", "ns"]
        }
    }
]
app = FastAPI()
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=False,
    allow_methods=["*"],
    allow_headers=["*"],
    expose_headers=["X-Navigate-To"],
)
app.mount("/page/", StaticFiles(directory=_LLM_DIR))
_exports_dir = os.path.join(_LLM_DIR, "exports")
os.makedirs(_exports_dir, exist_ok=True)
app.mount("/exports", StaticFiles(directory=_exports_dir), name="exports")
iid_context = dict[str, list[dict[str, str]]]()
@app.get("/api-key")
async def get_api_key():
    """Return whether an API key is configured, its masked form, full key, and the selected model."""
    if deepseek_api_key:
        return {"configured": True, "masked": _mask_key(deepseek_api_key), "apiKey": deepseek_api_key, "model": deepseek_model}
    return {"configured": False, "masked": "", "apiKey": "", "model": deepseek_model}
@app.post("/api-key")
async def set_api_key(data: dict):
    """Save API key and model to config file (JSON). Empty key clears the configuration."""
    global deepseek_api_key, deepseek_model
    api_key = (data.get("apiKey") or "").strip()
    model = (data.get("model") or "").strip() or "deepseek-chat"
    _key_file = os.path.join(_LLM_DIR, "deepseek_api_key.txt")
    if not api_key:
        # Clear configuration
        deepseek_api_key = ""
        deepseek_model = model
        if os.path.exists(_key_file):
            os.remove(_key_file)
        return {"success": True, "masked": "", "model": model, "cleared": True}
    deepseek_api_key = api_key
    deepseek_model = model
    with open(_key_file, "w", encoding="utf-8") as _f:
        json.dump({"apiKey": api_key, "model": model}, _f)
    _update_client(api_key)
    return {"success": True, "masked": _mask_key(api_key), "model": model, "cleared": False}
@app.post("/test-api-key")
async def test_api_key(data: dict):
    """Test the API key and model by sending a simple request."""
    api_key = (data.get("apiKey") or "").strip()
    model = (data.get("model") or "").strip() or "deepseek-chat"
    if not api_key:
        return {"success": False, "message": "API key cannot be empty"}
    try:
        result = await asyncio.to_thread(
            _chat_completion_sync,
            [{"role": "user", "content": "hi"}],
            model,
            api_key,
            10,
            60
        )
        reply = result.get("choices", [{}])[0].get("message", {}).get("content", "")
        return {"success": True, "message": f"Connection successful! Model: {model}"}
    except Exception as e:
        return {"success": False, "message": str(e)}
@app.post("/export-pdf")
async def export_pdf_endpoint(data: dict):
    """Export current analysis results to PDF and return download URL."""
    url = data.get("url", "http://127.0.0.1:41337")
    iid = data.get("iid", "")
    filename = data.get("filename", "")
    if not iid:
        return {"success": False, "message": "Missing instance ID (iid)"}
    try:
        result = await pdf_export.export_results_pdf(url, iid, filename)
        if result.get("success"):
            pdf_path = result["filepath"]
            pdf_name = os.path.basename(pdf_path)
            # Serve via static files mounted at /exports
            download_url = f"/exports/{pdf_name}"
            return {
                "success": True,
                "download_url": download_url,
                "filename": pdf_name,
                "size_bytes": result.get("size_bytes", 0),
                "message": result.get("message", "")
            }
        else:
            return {"success": False, "message": result.get("message", "PDF export failed")}
    except Exception as e:
        return {"success": False, "message": str(e)}
def _parse_document(file_path: str, filename: str) -> dict:
    """Parse uploaded document and extract text/data content.
    Supports: CSV, Excel (.xlsx/.xls), PDF, Word (.docx), plain text.
    Returns: {"success": bool, "content": str, "file_type": str, "message": str}
    """
    ext = filename.lower().rsplit('.', 1)[-1] if '.' in filename else ''
    try:
        if ext in ('csv', 'txt', 'tsv'):
            # CSV / TSV / plain text
            if ext == 'tsv':
                df = pd.read_csv(file_path, sep='\t')
            else:
                df = pd.read_csv(file_path)
            content = f"Data file: {filename}\n"
            content += f"Shape: {df.shape[0]} rows x {df.shape[1]} columns\n"
            content += f"Columns: {', '.join(df.columns.tolist())}\n\n"
            content += "First 20 rows:\n"
            content += df.head(20).to_string(index=False)
            return {"success": True, "content": content, "file_type": "csv", "message": f"CSV parsed: {df.shape[0]} rows, {df.shape[1]} columns"}
        elif ext in ('xlsx', 'xls'):
            # Excel
            xls = pd.ExcelFile(file_path)
            content = f"Excel file: {filename}\n"
            content += f"Sheets: {', '.join(xls.sheet_names)}\n\n"
            for sheet in xls.sheet_names[:3]:  # First 3 sheets
                df = pd.read_excel(file_path, sheet_name=sheet)
                content += f"=== Sheet: {sheet} ({df.shape[0]} rows x {df.shape[1]} cols) ===\n"
                content += f"Columns: {', '.join(df.columns.tolist())}\n"
                content += df.head(15).to_string(index=False)
                content += "\n\n"
            return {"success": True, "content": content, "file_type": "excel", "message": f"Excel parsed: {len(xls.sheet_names)} sheets"}
        elif ext == 'pdf':
            # PDF
            import pdfplumber
            content = f"PDF file: {filename}\n\n"
            with pdfplumber.open(file_path) as pdf:
                content += f"Total pages: {len(pdf.pages)}\n\n"
                for i, page in enumerate(pdf.pages[:10]):  # First 10 pages
                    text = page.extract_text() or ""
                    content += f"=== Page {i+1} ===\n{text[:3000]}\n\n"
                    # Also extract tables
                    tables = page.extract_tables()
                    for j, table in enumerate(tables[:2]):
                        content += f"--- Table {j+1} on page {i+1} ---\n"
                        for row in table[:20]:
                            content += " | ".join(str(c) if c else "" for c in row) + "\n"
                        content += "\n"
            return {"success": True, "content": content, "file_type": "pdf", "message": f"PDF parsed: {len(pdf.pages)} pages"}
        elif ext == 'docx':
            # Word
            from docx import Document
            doc = Document(file_path)
            content = f"Word file: {filename}\n\n"
            content += f"Paragraphs: {len(doc.paragraphs)}\n"
            content += f"Tables: {len(doc.tables)}\n\n"
            for i, para in enumerate(doc.paragraphs[:100]):
                if para.text.strip():
                    content += para.text + "\n"
            for j, table in enumerate(doc.tables[:5]):
                content += f"\n=== Table {j+1} ===\n"
                for row in table.rows[:20]:
                    content += " | ".join(cell.text for cell in row.cells) + "\n"
            return {"success": True, "content": content, "file_type": "word", "message": f"Word parsed: {len(doc.paragraphs)} paragraphs, {len(doc.tables)} tables"}
        else:
            return {"success": False, "content": "", "file_type": "unknown", "message": f"Unsupported file type: .{ext}. Supported: CSV, Excel, PDF, Word, TXT"}
    except Exception as e:
        return {"success": False, "content": "", "file_type": ext, "message": f"Parse error: {str(e)}"}
# Temporary upload directory
UPLOAD_DIR = "/tmp/imast_uploads"
os.makedirs(UPLOAD_DIR, exist_ok=True)
@app.post("/upload")
async def upload_file(file: UploadFile = File(...)):
    """Upload a data file directly to jamovi, show in data interface."""
    try:
        # Save uploaded file
        file_path = os.path.join(UPLOAD_DIR, file.filename)
        with open(file_path, "wb") as f:
            content = await file.read()
            f.write(content)
        # Check if it's a data file (CSV/Excel)
        ext = os.path.splitext(file.filename)[1].lower()
        is_data_file = ext in ['.csv', '.xlsx', '.xls', '.tsv', '.txt']
        if is_data_file:
            # Directly import to jamovi, show in data interface
            try:
                import imast as imast_module
                # IMAST expects url WITHOUT http:// prefix (it parses it internally)
                iclient = imast_module.IMAST("127.0.0.1:41337/IMAST")
                result = await asyncio.wait_for(
                    asyncio.to_thread(iclient.import_data_file, file_path),
                    timeout=60
                )
                if result.get("success"):
                    new_url = result.get("url", "")
                    title = result.get("title", file.filename)
                    return {
                        "success": True,
                        "filename": file.filename,
                        "file_type": "data",
                        "message": f"Imported: {title}",
                        "file_path": file_path,
                        "navigate_url": new_url
                    }
                else:
                    return {"success": False, "filename": file.filename, "message": f"Import failed: {result.get('message', 'Unknown error')}"}
            except Exception as e:
                return {"success": False, "filename": file.filename, "message": f"Import error: {str(e)}"}
        else:
            # Document file (PDF/Word) - parse and return content for AI
            result = _parse_document(file_path, file.filename)
            if result["success"]:
                content = result["content"]
                if len(content) > 15000:
                    content = content[:15000] + "\n... (content truncated, full file saved on server)"
                return {
                    "success": True,
                    "filename": file.filename,
                    "file_type": result["file_type"],
                    "content": content,
                    "file_path": file_path,
                    "message": result["message"]
                }
            else:
                return {"success": False, "filename": file.filename, "message": result["message"]}
    except Exception as e:
        return {"success": False, "message": f"Upload error: {str(e)}"}
# Skills system
SKILLS_DIR = os.path.join(_LLM_DIR, "skills")
_skills_cache = {}
def _load_skills():
    """Load all skill definitions from skills directory."""
    global _skills_cache
    _skills_cache = {}
    if not os.path.exists(SKILLS_DIR):
        return
    for filename in os.listdir(SKILLS_DIR):
        if filename.endswith('.json'):
            try:
                with open(os.path.join(SKILLS_DIR, filename), 'r', encoding='utf-8') as f:
                    skill = json.load(f)
                    _skills_cache[skill['name']] = skill
            except Exception as e:
                print(f"Failed to load skill {filename}: {e}")
_load_skills()
print(f"Loaded {len(_skills_cache)} skills")
@app.get("/skills")
async def list_skills():
    """List all available AI skills with brief info."""
    skill_list = []
    for name, skill in _skills_cache.items():
        skill_list.append({
            "name": skill.get("name", ""),
            "title": skill.get("title", ""),
            "description": skill.get("description", ""),
            "category": skill.get("category", ""),
            "applicable_scenarios": skill.get("applicable_scenarios", []),
            "tools_to_use": skill.get("tools_to_use", []),
            "key_outputs": skill.get("key_outputs", []),
            "workflow": skill.get("workflow", [])
        })
    # Sort by category then title
    skill_list.sort(key=lambda x: (x.get("category", ""), x.get("title", "")))
    return {"skills": skill_list, "count": len(skill_list)}
@app.get("/skill/{skill_name}")
async def get_skill(skill_name: str):
    """Get detailed definition of a specific skill."""
    if skill_name in _skills_cache:
        return {"success": True, "skill": _skills_cache[skill_name]}
    else:
        return {"success": False, "message": f"Skill '{skill_name}' not found. Available: {', '.join(_skills_cache.keys())}"}
@app.post("/optimize-prompt")
async def optimize_prompt_endpoint(data: dict):
    """Optimize user's raw question into a standardized, context-aware prompt using LLM."""
    user_input = data.get("user_input", "").strip()
    dataset_context = data.get("dataset_context", "").strip()
    if not user_input:
        return {"optimized_prompt": None, "error": "Empty user input"}
    if not deepseek_api_key:
        return {"optimized_prompt": None, "error": "API key not configured. Please set it in Setup first."}
    system_msg = (
        "You are a prompt optimizer for the iMast IVD statistical analysis software. "
        "Your job is to rewrite the user's question into a precise, unambiguous instruction "
        "that another AI assistant can execute correctly. Rules:\n"
        "1. If the user refers to 'this data' or 'current data', make it explicit that the analysis "
        "must be performed on the currently loaded dataset (not all possible methods in the software).\n"
        "2. Clarify ambiguous terms (e.g., 'what can we do' -> 'list statistical methods applicable to the current dataset').\n"
        "3. Keep the optimized prompt concise and in English.\n"
        "4. Do NOT add analysis or explanation — return ONLY the optimized prompt text, nothing else.\n"
        "5. Preserve the user's original intent; do not change what they are asking for."
    )
    if dataset_context:
        system_msg += f"\n\nContext available: {dataset_context}"
    messages = [
        {"role": "system", "content": system_msg},
        {"role": "user", "content": f"Optimize this question: {user_input}"}
    ]
    try:
        result = await asyncio.to_thread(
            _chat_completion_sync,
            messages,
            deepseek_model,
            deepseek_api_key,
            512,
            30
        )
        optimized = result.get("choices", [{}])[0].get("message", {}).get("content", "").strip()
        # Remove any surrounding quotes the model might add
        optimized = optimized.strip('"').strip("'").strip()
        if not optimized:
            return {"optimized_prompt": None, "error": "Model returned empty response"}
        return {"optimized_prompt": optimized}
    except Exception as e:
        return {"optimized_prompt": None, "error": str(e)}
@app.post("/clear-context")
async def clear_context(data: dict):
    """Clear the conversation history for a specific instance."""
    iid = data.get("iid", "")
    if iid and iid in iid_context:
        del iid_context[iid]
        return {"success": True, "message": f"Context cleared for instance {iid}"}
    return {"success": True, "message": "No context to clear"}
@app.post("/input")
async def inputRequest(data: dict[str, str]):
    navigate_url = None  # Will be set if any tool returns NAVIGATE_TO
    url = data["url"]
    iid = data["iid"]
    uinput = data["data"]
    if url.startswith("https://"):
        url = url.replace("https://", "")
    elif url.startswith("http://"):
        url = url.replace("http://", "")
    context = iid_context.setdefault(iid, list())
    print(url, iid)
    md_info = ""
    active_iid = iid
    try:
        iclient = imast.IMAST(f"{url}/IMAST")
        iclient.set_instance_id(iid)
        md_info = compress_as_markdown.compress_all(await asyncio.wait_for(iclient.get_all(), timeout=10))
    except Exception as e:
        print(f"Failed to get page context with iid={iid}: {e}")
        # iid 可能失效，自动查找当前活动的实例
        try:
            iclient2 = imast.IMAST(f"{url}/IMAST")
            instances = await asyncio.wait_for(iclient2.get_all_instances(), timeout=5)
            if instances:
                active_iid = instances[0][1]
                print(f"Auto-resolved to active instance: {active_iid}")
                # 迁移 context 到新的 iid
                if active_iid != iid:
                    iid_context.setdefault(active_iid, []).extend(context)
                    context = iid_context[active_iid]
                iclient2.set_instance_id(active_iid)
                md_info = compress_as_markdown.compress_all(await asyncio.wait_for(iclient2.get_all(), timeout=10))
            else:
                md_info = "(No active analysis instance. User may need to open a dataset first.)"
        except Exception as e2:
            print(f"Failed to resolve active instance: {e2}")
            md_info = "(No active analysis instance. User may need to open a dataset first.)"
    context.append({"role": "user", "content": uinput})
    # New architecture: tool calling loop INSIDE the SSE stream
    # so users see progress in real-time
    max_iterations = 15
    async def event_stream():
        # If there's a navigate URL, send it as a special marker at the start
        if navigate_url:
            nav_marker = f"<!--NAVIGATE: {navigate_url}-->\n"
            for char in nav_marker:
                yield char
            print(f"Sending NAVIGATE marker in stream: {navigate_url}")
        # Function Calling loop (inside SSE stream)
        for iteration in range(max_iterations):
            messages = [
                {"role": "system", "content": f"{prompt}\n{md_info}"},
            ] + context
            result = await asyncio.to_thread(
                _chat_completion_sync,
                messages,
                deepseek_model,
                deepseek_api_key,
                4096,
                120,
                True  # Enable thinking mode for deeper reasoning
            )
            message_obj = result.get("choices", [{}])[0].get("message", {})
            reply = message_obj.get("content", "") or ""
            reasoning = message_obj.get("reasoning_content", "") or ""
            # In thinking mode, content may be empty on first call - retry once
            if not reply.strip() and reasoning and iteration == 0:
                print("Thinking mode: content empty, retrying...")
                continue
            # Build assistant message with reasoning_content
            assistant_msg = {"role": "assistant", "content": reply}
            if reasoning:
                assistant_msg["reasoning_content"] = reasoning
            # Check if AI wants to call a tool
            if "#FunctionCall" in reply:
                # Output thinking content first
                if reasoning:
                    yield f"[THINKING_START]\n{reasoning}\n[THINKING_END]\n\n"
                
                lines = reply.split("\n")
                fc_idx = None
                for i, line in enumerate(lines):
                    if line.strip() == "#FunctionCall":
                        fc_idx = i
                        break
                if fc_idx is not None:
                    json_line = None
                    for j in range(fc_idx + 1, len(lines)):
                        if lines[j].strip():
                            json_line = lines[j].strip()
                            break
                    if json_line:
                        try:
                            tool_call = json.loads(json_line)
                            tool_name = tool_call.get("name", "")
                            tool_params = tool_call.get("parameters", {})
                            print(f"Tool call: {tool_name} {tool_params}")
                            # Show progress to user
                            yield f"[ACTION_START]\n\U0001f504 正在执行 {tool_name}...\n[ACTION_END]\n\n"
                            tool_result = await asyncio.wait_for(execute_tool(tool_name, tool_params, url, active_iid), timeout=180)
                            # Show completion
                            yield f"[ACTION_START]\n\u2705 {tool_name} 完成\n[ACTION_END]\n\n"
                            # Extract NAVIGATE_TO URL from tool result
                            import re
                            nav_match = re.search(r'\[NAVIGATE_TO:\s*(.+?)\]', tool_result)
                            if nav_match:
                                new_nav_url = nav_match.group(1).strip()
                                print(f"Navigation URL captured: {new_nav_url}")
                                yield f"<!--NAVIGATE: {new_nav_url}-->\n"
                            context.append(assistant_msg)
                            context.append({"role": "user", "content": f"Tool result for {tool_name}:\n{tool_result}"})
                            continue
                        except (json.JSONDecodeError, KeyError) as e:
                            print(f"Failed to parse tool call: {e}")
                        except asyncio.TimeoutError:
                            timeout_msg = f"Tool '{tool_name}' timed out after 180 seconds."
                            print(f"Tool timeout: {tool_name}")
                            yield f"\u26a0\ufe0f {tool_name} 超时\n\n"
                            context.append(assistant_msg)
                            context.append({"role": "user", "content": f"Tool result for {tool_name}:\n{timeout_msg}"})
                            continue
                        except Exception as e:
                            error_msg = f"Tool '{tool_name}' failed with error: {str(e)}."
                            print(f"Tool execution error for {tool_name}: {e}")
                            yield f"\u274c {tool_name} 失败: {str(e)}\n\n"
                            context.append(assistant_msg)
                            context.append({"role": "user", "content": f"Tool result for {tool_name}:\n{error_msg}"})
                            continue
            # No tool call, this is the final answer
            context.append(assistant_msg)
            
            # Output thinking content if any
            if reasoning:
                yield f"[THINKING_START]\n{reasoning}\n[THINKING_END]\n\n"
            
            yield "\n---\n\n"
            for char in reply:
                yield char
            yield "\n"
            return
        # Fallback if max iterations reached
        fallback = "I apologize, but I was unable to complete the request after multiple tool calls. Please try again."
        yield fallback
        yield "\n"
    return StreamingResponse(event_stream(), media_type="text/event-stream")
if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
