#!/usr/bin/env python3
"""
Parameterized type reporter — replaces snare/piton/scaffold/rope/true_mountain/
tangled_rope/false_mountain reporters plus count_computed_classifications and
high_friction.

Thin wrapper — delegates to reports.queries.type_reporter.
Exports load_pipeline_data() and run_type_report() for run_pipeline.py.
"""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from shared.loader import load_json, PIPELINE_JSON
from orbit_utils import load_orbit_data
from reports.queries.type_reporter import (
    TYPE_CONFIGS, run_type_report, summary_counts, summary_friction,
)


def load_pipeline_data():
    """Exported for run_pipeline.py compatibility."""
    return load_json(PIPELINE_JSON, label="pipeline")


def main():
    parser = argparse.ArgumentParser(
        description='Parameterized type reporter for structural dynamics model')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--type', choices=list(TYPE_CONFIGS.keys()),
                       help='Generate report for a single type')
    group.add_argument('--all', action='store_true',
                       help='Generate reports for all types')
    group.add_argument('--summary', choices=['counts', 'friction'],
                       help='Print summary to stdout (no file output)')

    args = parser.parse_args()
    pipeline_data = load_pipeline_data()
    if not pipeline_data:
        print(f"Error: Pipeline output not found at {PIPELINE_JSON}", file=sys.stderr)
        sys.exit(1)

    if args.summary:
        if args.summary == 'counts':
            summary_counts(pipeline_data)
        else:
            summary_friction(pipeline_data)
        return

    orbit_data = load_orbit_data()

    if args.all:
        for type_key in TYPE_CONFIGS:
            run_type_report(type_key, pipeline_data, orbit_data)
            print()
    else:
        run_type_report(args.type, pipeline_data, orbit_data)


if __name__ == '__main__':
    main()
