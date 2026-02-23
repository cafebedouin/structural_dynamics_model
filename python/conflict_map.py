#!/usr/bin/env python3
"""
Conflict Map Generator — Perspectival Gap Analysis by Domain

Thin wrapper around the registry report. Original computation logic
moved to reports/queries/conflict_map.py, template in
reports/templates/conflict_map.md.j2.

Usage: python3 python/conflict_map.py
"""

import sys
from pathlib import Path

if str(Path(__file__).resolve().parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent))

from reports.registry import run_report, REPORTS


def main():
    if not run_report(REPORTS["conflict_map"]):
        sys.exit(1)


if __name__ == '__main__':
    main()
