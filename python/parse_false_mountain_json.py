#!/usr/bin/env python3
"""Parse false_mountain_report.md -> false_mountain_data.json.

Centralizes the regex parsing of false mountain entries, consumed by
classification_audit.
"""

import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
FM_MD = ROOT / "outputs" / "false_mountain_report.md"
FM_JSON = ROOT / "outputs" / "false_mountain_data.json"

RE_ENTRY = re.compile(
    r'###\s*\d+\.\s*False Mountain:\s*`([^`]+)`\s*\n(.*?)(?=###\s*\d+\.|$)',
    re.DOTALL,
)
RE_SEVERITY = re.compile(r'\*\*Severity:\*\*\s*`(\w+)`')


def parse(text):
    """Parse false mountain report markdown into structured data."""
    entries = []
    for cid, body in RE_ENTRY.findall(text):
        severity = None
        sev_m = RE_SEVERITY.search(body)
        if sev_m:
            severity = sev_m.group(1)

        if 'Snare" is masked' in body:
            gap_pattern = "snare_masked_as_rope"
        elif 'Rope" appears as' in body:
            gap_pattern = "rope_appears_as_mountain"
        else:
            gap_pattern = "unknown"

        entries.append({
            "id": cid,
            "severity": severity,
            "gap_pattern": gap_pattern,
        })
    return {"false_mountains": entries}


def main():
    if not FM_MD.exists():
        data = {"false_mountains": []}
    else:
        text = FM_MD.read_text(encoding="utf-8", errors="replace")
        data = parse(text)
    FM_JSON.parent.mkdir(parents=True, exist_ok=True)
    with open(FM_JSON, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)
    print(f"false_mountain_data.json: {len(data['false_mountains'])} entries")


if __name__ == "__main__":
    main()
