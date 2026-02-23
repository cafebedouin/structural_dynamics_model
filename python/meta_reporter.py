#!/usr/bin/env python3
"""
Meta-reporter — pipeline health dashboard.

Thin wrapper — delegates to reports.queries.meta_reporter via registry.
Preserves MetaReporter class API for run_pipeline.py compatibility.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from reports.registry import run_report, REPORTS


class MetaReporter:
    """Compatibility class for run_pipeline.py which creates MetaReporter(),
    calls parse() then generate_report() inside redirect_stdout()."""

    def parse(self):
        pass  # no-op; data loading happens inside run_report

    def generate_report(self):
        run_report(REPORTS["meta_report"])


def main():
    reporter = MetaReporter()
    reporter.parse()
    reporter.generate_report()


if __name__ == '__main__':
    main()
