"""Sufficiency tester — thin wrapper around reports registry."""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))

# Backward-compatible class for pipeline imports
from reports.queries.sufficiency_test import SufficiencyTester

from reports.registry import run_report, REPORTS


def main():
    run_report(REPORTS["sufficiency_test"])


if __name__ == "__main__":
    main()
