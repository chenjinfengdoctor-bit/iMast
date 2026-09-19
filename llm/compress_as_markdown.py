import json
import typing


def recursive_parse_results(
    result: dict[str, typing.Any], markdown_items: list[str], level: int = 1
):
    if result["visible"] not in ["DEFAULT_YES", "YES"]:
        return

    if result["title"] != "":
        markdown_items.append(f'#{"#"*min(level,5)} {result["title"]}')
    if result["type"] == "group":
        for content in result["content"]:
            recursive_parse_results(content, markdown_items, level + 1)
    elif result["type"] == "table":
        columns = list[str]()
        top_rules = list[str]()
        row_values = dict[int, list[typing.Any]]()
        empty_row = [""] * len(result["content"]["columns"])
        for ci, column in enumerate(result["content"]["columns"]):
            if column["superTitle"] == "":
                columns.append(column["title"])
            else:
                columns.append(f'{column["superTitle"]}<br>{column["title"]}')
            top_rules.append("--")
            for ri in range(len(column["cells"])):
                if ri not in row_values:
                    row_values[ri] = empty_row.copy()
                cell = column["cells"][ri]
                if isinstance(cell["value"], float):
                    row_values[ri][ci] = f"{cell["value"]:.4f}"
                else:
                    row_values[ri][ci] = str(cell["value"])
        markdown_items.append("|" + "|".join(columns) + "|")
        markdown_items.append("|" + "|".join(top_rules) + "|")
        for k in row_values:
            markdown_items.append("|" + "|".join(row_values[k]) + "|")
    elif result["type"] == "preformatted":
        if result["name"] == "syntax":
            markdown_items.append(f'```\n{result["content"]}\n```')
        else:
            markdown_items.append(result["content"].replace("\n", "\n\n"))


def compress_results(results: list[dict[str, typing.Any]]) -> str:
    markdown_items = list[str]()
    for result in results:
        if (
            result["instanceId"] == ""
            and result["name"] == "empty"
            and result["ns"] == "jmv"
        ):
            continue
        recursive_parse_results(result["results"], markdown_items)
    return "\n".join(markdown_items)


def compress_analyses_method(
    analyses_method: list[dict[str, typing.Any]],
    exclude_ns: set[str] = {"jmv", "scatr"},
) -> str:
    markdown_items = list[str]()
    counter = 0
    sub_counter = 1
    counter += 1
    markdown_items.append(f"## {counter}. How To Invoke")
    markdown_items.append(
        "- specify each `name`, `value` and `type` of the following options\n"
        '- **Variables**: one or more column names, its type is `"s"`\n'
        "  - if **permitted** is not empty, the column type must be one of the allowed types\n"
        "  - if **suggested** is not empty, the column type should be one of the allowed types\n"
        '- **Variable**: a column name, its type is `"s"`\n'
        "  - if **permitted** is not empty, the column type must be one of the allowed types\n"
        "  - if **suggested** is not empty, the column type should be one of the allowed types\n"
        '- **Number**: a float number, its type is `"d"`\n'
        '- **Integer**: a integer number, its type is `"d"`\n'
        '- **List**: specify one of the name in options, its type is `"s"`\n'
        '- **Bool**: specify true or false, its type is `"o"`\n'
    )
    markdown_items.append(
        "- A invoke json is just like below:\n\n"
        """
```json
{
    "analysis_name":"SpearmanRankCorrelation",
    "ns":"Regression",
    "options:{
        "options":[
            {"key": "x", "value": "Appraisal", "type": "s"},
            {"key": "y", "value": "Price", "type": "s"},
            {"key": "alpha", "value": 0.05, "type": "d"},
            {"key": "correct", "value": true, "type": "o"}
        ]
}
```
        """
    )
    for module in analyses_method:
        sub_counter = 0
        title = module["title"]
        if title == "What the Package Does (Title Case)":
            title = module["name"]
        counter += 1
        markdown_items.append(f"## {counter}. {title}")
        description = module["description"]
        if (
            description
            != "More about what it does (maybe more than one line) Use four spaces when indenting paragraphs within the Description."
        ):
            markdown_items.append(f"{description}")
        # markdown_items.append("\nauthors:")
        # for author in module["authors"]:
        #     markdown_items.append(f"  - {author}")
        for analysis in module["analyses"]:
            ns = analysis["ns"]
            if ns in exclude_ns:
                continue
            sub_counter += 1
            markdown_items.append(f'### {counter}.{sub_counter}. {analysis["title"]}')
            markdown_items.append(
                f'#### {counter}.{sub_counter}.1. Where\n`{analysis["menuGroup"]}`/`{analysis["menuTitle"]}`'
            )
            markdown_items.append(
                f'#### {counter}.{sub_counter}.2. Descrption\n{analysis["defn"].get("description","")}'
            )
            markdown_items.append(
                f'#### {counter}.{sub_counter}.3. Namespace (ns)\n{analysis["ns"]}'
            )
            markdown_items.append(
                f"\n#### {counter}.{sub_counter}.4. Options"
                "\n|Name|Title|Type|Permitted|Suggested|Description|Options|"
                "\n|----|-----|----|---------|---------|-----------|-------|"
            )
            for option in analysis["defn"]["options"]:
                if option["name"] == "data":
                    continue
                description = ""
                if "description" in option:
                    if isinstance(option["description"], str):
                        description = option["description"]
                    else:
                        description = option["description"]["R"]
                description = description.replace("\n", "<br>")
                markdown_items.append(
                    f'|{option["name"]}'
                    f'|{option.get("title","").replace("\\{","{").replace("\\}","}")}'
                    f'|{option["type"]}'
                    f'|{"; ".join(option.get("permitted",[]))}'
                    f'|{"; ".join(option.get("suggested",[]))}'
                    f"|{description}"
                    f'|{"; ".join([item["name"] if isinstance(item,dict) else item for item in option.get("options",[])])}'
                    f"|"
                )
    return "\n".join(markdown_items)


def compress_dataframe_info(dataframe_info: dict[str, typing.Any]) -> str:
    """Compress dataframe info to markdown. Robust against missing fields."""
    try:
        if not isinstance(dataframe_info, dict):
            return f"Dataset info returned unexpected type: {type(dataframe_info).__name__}. Raw: {str(dataframe_info)[:200]}"

        title = dataframe_info.get("title", "Untitled Dataset")
        columns = dataframe_info.get("columns", [])

        if not columns:
            row_count = dataframe_info.get("row_count", "unknown")
            return f"## {title}\n\nNo columns found. Row count: {row_count}. The dataset may be empty or not fully loaded."

        markdown_items = list[str]()
        markdown_items.append(f'## {title}')

        # Helper to safely get column field
        def _col_val(col, key, default=""):
            val = col.get(key, default)
            return str(val) if val is not None else default

        col_names = [_col_val(col, "name", "?") for col in columns]
        markdown_items.append("|RowName|" + "|".join(col_names) + "|")
        markdown_items.append("|-------|" + "|".join(["--" for _ in columns]) + "|")
        markdown_items.append("|ColumnType|" + "|".join([_col_val(col, "column_type") for col in columns]) + "|")
        markdown_items.append("|DataType|" + "|".join([_col_val(col, "data_type") for col in columns]) + "|")
        markdown_items.append("|MeasureType|" + "|".join([_col_val(col, "measure_type") for col in columns]) + "|")
        markdown_items.append("|RowCount|" + "|".join([_col_val(col, "row_count") for col in columns]) + "|")
        markdown_items.append("|Description|" + "|".join([_col_val(col, "description") for col in columns]) + "|")

        # levels - safely handle
        level_strs = []
        for col in columns:
            levels = col.get("levels", [])
            if isinstance(levels, list):
                level_labels = []
                for level in levels:
                    if isinstance(level, (list, tuple)) and len(level) >= 3:
                        level_labels.append(str(level[2]))
                    elif isinstance(level, dict):
                        level_labels.append(str(level.get("label", level.get("value", ""))))
                    else:
                        level_labels.append(str(level))
                level_strs.append("; ".join(level_labels))
            else:
                level_strs.append("")
        markdown_items.append("|levels|" + "|".join(level_strs) + "|")

        return "\n".join(markdown_items)
    except Exception as e:
        return f"Failed to compress dataframe info: {type(e).__name__}: {e}\nRaw keys: {list(dataframe_info.keys()) if isinstance(dataframe_info, dict) else 'not a dict'}"


def compress_all(content: dict[str, typing.Any]) -> str:
    items = []
    if "dataframe_info" in content:
        items.append("# Dataframe Info")
        items.append(compress_dataframe_info(content["dataframe_info"]))
    if "analyses_method" in content:
        items.append("# Analyses Method")
        items.append(compress_analyses_method(content["analyses_method"]))
    if "results" in content:
        items.append("# Results")
        items.append(compress_results(content["results"]))
    return "\n".join(items)


if __name__ == "__main__":
    results = json.load(open("./result.json", encoding="utf-8"))
    open("result.md", "w", encoding="utf-8").write(compress_all(results))
