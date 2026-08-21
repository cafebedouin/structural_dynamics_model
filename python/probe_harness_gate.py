#!/usr/bin/env python3
"""probe_harness_gate.py — gate row for the OQ-326 install-witness suite.

Runs prolog/tests/test_probe_harness.pl and asserts the suite actually EXECUTED
tests. A bare `run ... swipl -g "... run_tests ..."` row would exit 0 when the
suite loads zero tests or the file is not found under the load path — which is
clause 1 of OQ-326's own acceptance criterion (snapshot non-empty) reappearing
one layer up as *test count non-zero*. Guarding the install checks with a row
that has the install-check defect is not acceptable, so this parses the count.

Three ways to be RED:
  - swipl exits non-zero (a test failed, or the load chain broke)
  - swipl is unavailable            -> RED, never "skip". An unenforced channel
                                       is the filtered channel (OQ-96).
  - zero tests executed             -> RED, however green the exit code looked.

--check runs the fixture selftest FIRST, then the live suite, so a clean live
result is never a didn't-look. Wired into scripts/gate.sh.
"""
from __future__ import annotations

import re
import shutil
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
PROLOG = REPO / "prolog"
SUITE = PROLOG / "tests" / "test_probe_harness.pl"

# plunit's summary lines. Both spellings occur across SWI versions.
PASSED_RE = re.compile(r"All (\d+) tests passed")
COUNT_RE = re.compile(r"(\d+) tests? (?:passed|failed)")
FAILED_RE = re.compile(r"(\d+) tests? failed")


def parse_outcome(text: str) -> tuple[int, int]:
    """-> (n_executed, n_failed). n_executed == 0 means the suite did not run."""
    n_failed = 0
    m = FAILED_RE.search(text)
    if m:
        n_failed = int(m.group(1))
    m = PASSED_RE.search(text)
    if m:
        return int(m.group(1)), n_failed
    # a failing run reports "N tests failed" without an "All N passed" line;
    # recover the executed count from the per-test progress lines.
    idx = re.findall(r"^%\s*\[\d+/(\d+)\]", text, re.M)
    if idx:
        return int(idx[-1]), n_failed
    m = COUNT_RE.search(text)
    if m:
        return int(m.group(1)), n_failed
    return 0, n_failed


def run_suite() -> tuple[int, int, int, str]:
    """-> (rc, n_executed, n_failed, output)"""
    if shutil.which("swipl") is None:
        return 127, 0, 0, "swipl not on PATH"
    if not SUITE.exists():
        return 2, 0, 0, f"suite not found: {SUITE}"
    proc = subprocess.run(
        ["swipl", "-g", "[stack], [tests/test_probe_harness], run_tests, halt",
         "-t", "halt(1)"],
        cwd=str(PROLOG), capture_output=True, text=True, timeout=300)
    out = proc.stdout + proc.stderr
    n_exec, n_failed = parse_outcome(out)
    return proc.returncode, n_exec, n_failed, out


def selftest() -> None:
    """The parser is the thing that can silently lie, so it carries the controls:
    a planted zero-test output must read RED, a planted failing-test output must
    read RED, and a realistic passing output must read GREEN."""
    zero = "% No tests to run\n"
    n, f = parse_outcome(zero)
    assert n == 0, f"selftest: planted zero-test output read as {n} executed"

    failing = ("% [12/47] probe_harness:c2_fires .... passed (0.000 sec)\n"
               "ERROR: test probe_harness:c4_declines: wrong error\n"
               "% 2 tests failed\n")
    n, f = parse_outcome(failing)
    assert n == 47, f"selftest: planted failing output read as {n} executed"
    assert f == 2, f"selftest: planted failing output read as {f} failures"

    passing = ("% [47/47] probe_harness:rep..checked .... passed (0.000 sec)\n"
               "% All 47 tests passed in 0.025 seconds (0.025 cpu)\n")
    n, f = parse_outcome(passing)
    assert n == 47 and f == 0, f"selftest: planted passing output read as {n}/{f}"

    # and a control that the RED path is reachable at all: an empty transcript
    # must not read as a passing suite.
    n, f = parse_outcome("")
    assert n == 0, "selftest: empty output read as a run"
    print("selftest: OK (4 parser controls: 3 red-capable, 1 green)")


def main(argv: list[str]) -> int:
    mode = argv[1] if len(argv) > 1 else "--check"
    if mode == "--selftest":
        selftest()
        return 0
    if mode != "--check":
        print(__doc__)
        return 2
    selftest()  # controls ride with every live check
    rc, n_exec, n_failed, out = run_suite()
    if rc == 127:
        print("probe harness gate: RED — swipl unavailable. Not skipped: an "
              "unenforced channel is the filtered channel (OQ-96).")
        return 1
    if n_exec == 0:
        print(f"probe harness gate: RED — ZERO tests executed (swipl rc={rc}). "
              "A green exit code with no tests run is exactly the install-check "
              "defect this suite exists to close.")
        for line in out.strip().splitlines()[-6:]:
            print(f"  | {line}")
        return 1
    if rc != 0 or n_failed:
        print(f"probe harness gate: RED — {n_failed} of {n_exec} tests failed "
              f"(swipl rc={rc})")
        for line in out.strip().splitlines()[-12:]:
            print(f"  | {line}")
        return 1
    print(f"probe harness gate: GREEN — {n_exec} install-witness tests executed, "
          "0 failed; two-sided pair per check (2, 3, 1, 4, 4', 5)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
