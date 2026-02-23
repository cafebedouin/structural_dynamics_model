"""Red spot check — thin wrapper around reports registry."""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
from reports.registry import run_report, REPORTS


def main():
    run_report(REPORTS["red_spot_check"])


if __name__ == "__main__":
    main()
