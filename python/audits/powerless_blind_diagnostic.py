#!/usr/bin/env python3
"""
Powerless Blind Diagnostic

Thin wrapper around the registry report. Original computation logic
moved to reports/queries/powerless_blind_diagnostic.py, template in
reports/templates/powerless_blind_diagnostic.md.j2.

Usage: python3 python/powerless_blind_diagnostic.py
"""

import sys
from pathlib import Path

if str(Path(__file__).resolve().parent.parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from reports.registry import run_report, REPORTS


def main():
    if not run_report(REPORTS["powerless_blind_diagnostic"]):
        sys.exit(1)


if __name__ == '__main__':
    main()
