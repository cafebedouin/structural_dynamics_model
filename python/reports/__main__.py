"""CLI entry point: python -m reports <name> | --all | --list"""

import sys
from reports.registry import run_report, REPORTS


def main():
    if len(sys.argv) < 2 or sys.argv[1] in ("-h", "--help"):
        print("Usage: python -m reports <name> | --all | --list")
        print("       python -m reports --list          List registered reports")
        print("       python -m reports --all           Run all reports")
        print("       python -m reports <name>          Run a single report")
        sys.exit(0)

    arg = sys.argv[1]

    if arg == "--list":
        for name in sorted(REPORTS.keys()):
            r = REPORTS[name]
            dest = str(r.output_path) if r.output_path else "stdout"
            print(f"  {name:25s} -> {dest}")
        return

    if arg == "--all":
        ok = 0
        fail = 0
        for name in sorted(REPORTS.keys()):
            print(f"--- {name} ---")
            if run_report(REPORTS[name]):
                ok += 1
            else:
                fail += 1
            print()
        print(f"Done: {ok} succeeded, {fail} failed")
        sys.exit(1 if fail else 0)

    if arg not in REPORTS:
        print(f"Unknown report: {arg}", file=sys.stderr)
        print(f"Available: {', '.join(sorted(REPORTS.keys()))}", file=sys.stderr)
        sys.exit(1)

    if not run_report(REPORTS[arg]):
        sys.exit(1)


if __name__ == "__main__":
    main()
