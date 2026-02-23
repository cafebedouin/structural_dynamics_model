#!/usr/bin/env python3
"""
Omega Enricher — Cross-references omega data with corpus metrics,
orbit signatures, and conflict map classifications to produce
triaged, actionable omega reports.

Thin wrapper — delegates to reports.queries.omega_enricher via registry.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from reports.registry import run_report, REPORTS


def main():
    run_report(REPORTS["omega_enricher"])


if __name__ == '__main__':
    main()
