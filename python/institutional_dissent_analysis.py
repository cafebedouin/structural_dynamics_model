#!/usr/bin/env python3
"""Institutional Dissent Analysis (v1.0)

Thin wrapper — delegates to reports.queries.institutional_dissent via registry.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from reports.registry import run_report, REPORTS


def main():
    run_report(REPORTS["institutional_dissent"])


if __name__ == "__main__":
    main()
