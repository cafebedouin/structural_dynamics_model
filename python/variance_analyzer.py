"""Variance analysis — thin wrapper around reports registry."""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))

# Backward-compatible class for pipeline imports
from reports.queries.variance_analysis import VarianceAnalyzer

from reports.registry import run_report, REPORTS


def main():
    run_report(REPORTS["variance_analysis"])


if __name__ == "__main__":
    main()
