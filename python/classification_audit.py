#!/usr/bin/env python3
"""
Classification Audit Engine — Triages Engine 1 findings into actionable categories.

Thin wrapper around the registry report. Original computation logic
moved to reports/queries/classification_audit.py, template in
reports/templates/classification_audit.md.j2.

Preserves CLI arg compatibility for --output override.

Usage: python3 python/classification_audit.py [--output PATH]
"""

import argparse
import sys
from pathlib import Path

if str(Path(__file__).resolve().parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent))

from reports.registry import run_report, REPORTS


def main():
    parser = argparse.ArgumentParser(
        description='Classification Audit Engine — triages Engine 1 findings')
    parser.add_argument(
        '--corpus-data',
        default=None,
        help='Path to corpus_data.json (ignored, uses registry data source)')
    parser.add_argument(
        '--false-mountains',
        default=None,
        help='Path to false_mountain_report.md (ignored, uses JSON sidecar)')
    parser.add_argument(
        '--testsets',
        default=None,
        help='Path to prolog testsets directory (ignored, uses internal paths)')
    parser.add_argument(
        '--output',
        default=None,
        help='Output report path (overrides default)')
    args = parser.parse_args()

    report = REPORTS["classification_audit"]

    output_override = None
    if args.output:
        project_root = Path(__file__).resolve().parent.parent
        output_path = Path(args.output)
        if not output_path.is_absolute():
            output_path = project_root / output_path
        output_override = output_path

    success = run_report(report, output_override=output_override)
    return 0 if success else 1


if __name__ == '__main__':
    sys.exit(main())
