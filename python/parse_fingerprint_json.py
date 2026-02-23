#!/usr/bin/env python3
"""Parse fingerprint_report.md -> fingerprint_data.json.

Centralizes the regex parsing of shift families from the fingerprint report
into a JSON sidecar, consumed by conflict_map, reform_threshold_report, and
powerless_blind_diagnostic.
"""

import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
FINGERPRINT_MD = ROOT / "outputs" / "fingerprint_report.md"
FINGERPRINT_JSON = ROOT / "outputs" / "fingerprint_data.json"

# Regex patterns (centralized from 3 scripts)
RE_HEADER = re.compile(r'^###\s+`(shift\([^)]+\))`\s+.*?(\d+)\s+constraints?')
RE_MEMBER = re.compile(r'^- `([^`]+)`')
RE_TUPLE = re.compile(r'shift\(([^,]+),\s*([^,]+),\s*([^,]+),\s*([^)]+)\)')


def parse(text):
    """Parse fingerprint report markdown into structured data."""
    families = []
    current_pattern = None
    current_count = 0
    current_members = []

    for line in text.splitlines():
        m = RE_HEADER.match(line)
        if m:
            if current_pattern is not None:
                families.append(_make_family(current_pattern, current_count, current_members))
            current_pattern = m.group(1)
            current_count = int(m.group(2))
            current_members = []
            continue

        m2 = RE_MEMBER.match(line)
        if m2 and current_pattern is not None:
            current_members.append(m2.group(1))
            continue

        if line.startswith("## ") and current_pattern is not None:
            families.append(_make_family(current_pattern, current_count, current_members))
            current_pattern = None
            current_members = []

    if current_pattern is not None:
        families.append(_make_family(current_pattern, current_count, current_members))

    return {"shift_families": families}


def _make_family(pattern, count, members):
    components = {}
    m = RE_TUPLE.match(pattern)
    if m:
        components = {
            "powerless": m.group(1).strip(),
            "moderate": m.group(2).strip(),
            "institutional": m.group(3).strip(),
            "analytical": m.group(4).strip(),
        }
    return {
        "pattern": pattern,
        "components": components,
        "count": count,
        "members": members,
    }


def main():
    text = FINGERPRINT_MD.read_text(encoding="utf-8", errors="replace")
    data = parse(text)
    FINGERPRINT_JSON.parent.mkdir(parents=True, exist_ok=True)
    with open(FINGERPRINT_JSON, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)
    n_families = len(data["shift_families"])
    n_total = sum(len(fam["members"]) for fam in data["shift_families"])
    print(f"fingerprint_data.json: {n_families} families, {n_total} constraints")


if __name__ == "__main__":
    main()
