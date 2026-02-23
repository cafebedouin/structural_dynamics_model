"""Corpus profile — thin wrapper around reports registry."""
import json
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
from reports.registry import run_report, REPORTS
from shared.loader import OUTPUT_DIR


def main():
    if not run_report(REPORTS["corpus_profile"]):
        sys.exit(1)

    # Print summary to stdout (matching original behavior)
    profile_path = OUTPUT_DIR / "corpus_profile.json"
    with open(profile_path, "r") as f:
        profile = json.load(f)

    print(f"Wrote {profile_path} ({profile['corpus_size']} constraints)")

    print(f"\nVerdict distribution:")
    for k, v in profile["verdict_distribution"].items():
        pct = 100 * v / profile["corpus_size"]
        print(f"  {k}: {v} ({pct:.1f}%)")
    print(f"\nSignal base rates:")
    for k, v in profile["signal_base_rates"].items():
        print(f"  {k}: {v}%")
    print(f"\nSubsystems available: {profile['subsystems_available']}")
    print(f"Subsystems unavailable: {profile['subsystems_unavailable']}")
    print(f"Abductive tensions: {profile['abductive_tensions']}")


if __name__ == "__main__":
    main()
