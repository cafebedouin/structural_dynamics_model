#!/usr/bin/env python3
"""
Reform Threshold Report — Energy Triage for Snare-Classified Constraints

Thin wrapper around the registry report. Original computation logic
moved to reports/queries/reform_threshold_report.py, template in
reports/templates/reform_threshold_report.md.j2.

Usage: python3 python/reform_threshold_report.py
"""

import sys
from pathlib import Path

if str(Path(__file__).resolve().parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent))

from reports.registry import run_report, REPORTS


def main():
    if not run_report(REPORTS["reform_threshold_report"]):
        sys.exit(1)


if __name__ == '__main__':
    main()
