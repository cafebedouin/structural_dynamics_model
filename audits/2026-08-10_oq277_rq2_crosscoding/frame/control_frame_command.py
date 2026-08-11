#!/usr/bin/env python3
"""control_frame_command.py — the positive control the §4.5 census command never had.

Operator, 2026-08-10: "'174 = 73 + 101, partition exact' is an arithmetic check on a
possibly-miscounting instrument." The partition identity holds by construction — `comm -23`
cannot make it fail — so it witnesses nothing about whether the census command CLASSIFIES
correctly. Same argument that produced the payload-count check: a consistency check over an
instrument's output is not evidence the instrument discriminates.

This builds a fixture tree of planted directories with KNOWN correct classifications and runs
the pinned command form against it. Every fixture is a two-sided control: it must land in the
bucket it belongs in AND stay out of the other.

Fixtures (want -> bucket):
  F1 empty directory                          -> population, NOT incident-bearing
       the found defect: an "audit directory" that is not an audit, inflating the denominator
  F2 .md containing a census keyword          -> incident-bearing            (the true positive)
  F3 .md with no census keyword               -> population, NOT incident-bearing
  F4 keyword present but in a NON-.md file    -> NOT incident-bearing        (--include='*.md')
  F5 keyword in a nested subdirectory .md     -> incident-bearing            (grep -r recurses)
  F6 directory holding only non-.md files     -> population, NOT incident-bearing

Plus a REGRESSION control for the historical unit error: run from inside `audits/` instead of
the repository root, `cut -d/ -f2` extracts FILENAMES rather than directory names. That is the
exact defect that produced 77/175 before v0.3 corrected it to 73/175. The control asserts the
wrong-cwd invocation gives a DIFFERENT and wrong answer — if it ever agrees with the right one,
the command has stopped being cwd-sensitive and this control has gone silent.

Exit 0 = all controls fired as pre-registered. Exit 1 = the census instrument does not
discriminate, and no census figure computed with it may be cited.
"""
from __future__ import annotations

import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

KEYWORD_PATTERN = (
    r'for its whole life\|never fired\|never ran\|read.*0 for\|was never\|silently'
)

FIXTURES = {
    "d1_empty":            {"files": {},                                              "want_incident": False},
    "d2_keyword_md":       {"files": {"WRITEUP.md": "the gate never fired at all\n"},  "want_incident": True},
    "d3_plain_md":         {"files": {"WRITEUP.md": "a routine comparison, all green\n"}, "want_incident": False},
    "d4_keyword_wrong_ext":{"files": {"notes.txt": "this never fired for its whole life\n"}, "want_incident": False},
    "d5_keyword_nested":   {"files": {"sub/FINDINGS.md": "the value was silently dropped\n"}, "want_incident": True},
    "d6_no_md_at_all":     {"files": {"data.json": "{}\n"},                            "want_incident": False},
}


def build(root: Path) -> None:
    for name, spec in FIXTURES.items():
        d = root / name
        d.mkdir(parents=True, exist_ok=True)
        for rel, content in spec["files"].items():
            f = d / rel
            f.parent.mkdir(parents=True, exist_ok=True)
            f.write_text(content)


def census(cwd: Path, target: str) -> set[str]:
    """The pinned Appendix B command form, run from `cwd` against `target`.

    Uses /usr/bin/grep EXPLICITLY. This session's interactive shell carries a `grep`
    shell FUNCTION (`type grep` -> "grep is a function") that alters output relative to
    the real binary: with target `.` the wrapper emits `dir/file.md` while the binary
    emits `./dir/file.md`. Since the census pipes grep into a POSITIONAL parse
    (`cut -d/ -f2`), that one-component shift moves the extracted field from directory
    name to FILENAME — silently. A control whose verdict depends on which grep happened
    to be in scope is not a control, so the binary is pinned here.
    """
    cmd = (
        f"/usr/bin/grep -rl '{KEYWORD_PATTERN}' --include='*.md' {target} "
        f"| cut -d/ -f2 | sort -u"
    )
    out = subprocess.run(["bash", "-c", cmd], cwd=cwd, capture_output=True, text=True)
    return {ln for ln in out.stdout.splitlines() if ln.strip()}


def population(cwd: Path, target: str) -> set[str]:
    cmd = f"ls -d {target}/*/ | sed 's#^{target}/##; s#/$##' | sort"
    out = subprocess.run(["bash", "-c", cmd], cwd=cwd, capture_output=True, text=True)
    return {ln for ln in out.stdout.splitlines() if ln.strip()}


def main() -> int:
    tmp = Path(tempfile.mkdtemp(prefix="oq277_frame_control_"))
    try:
        fake_root = tmp / "repo"
        audits = fake_root / "audits"
        build(audits)

        pop = population(fake_root, "audits")
        inc = census(fake_root, "audits")

        failures: list[str] = []

        # --- control 1: population completeness -------------------------------
        if pop != set(FIXTURES):
            failures.append(f"population != fixtures: got {sorted(pop)}")

        # --- control 2: per-fixture two-sided classification -------------------
        print(f"{'fixture':<24}{'want':>10}{'got':>10}   verdict")
        for name, spec in FIXTURES.items():
            want = spec["want_incident"]
            got = name in inc
            ok = want == got
            if not ok:
                failures.append(f"{name}: want incident={want}, got {got}")
            print(f"  {name:<22}{str(want):>10}{str(got):>10}   {'ok' if ok else 'FAIL'}")

        # --- control 3: the empty dir specifically ----------------------------
        # This is the defect actually found on 2026-08-10. It must be demonstrable,
        # not merely asserted: an empty dir enters the DENOMINATOR and can never
        # enter the numerator, so it silently deflates the incidence ratio.
        print()
        if "d1_empty" in pop and "d1_empty" not in inc:
            print("  [control 3] empty dir enters population, never the numerator — "
                  "denominator inflation is REPRODUCED")
        else:
            failures.append("control 3: the empty-directory defect did not reproduce")

        # --- control 4: the historical unit error is still reproducible ---------
        # The 77/175 defect: run from INSIDE audits/ with an unprefixed target, grep
        # emits 'dirname/file.md' and `cut -d/ -f2` takes the FILENAME. The pinned form
        # (repo root, target 'audits/') emits 'audits/dirname/file.md' so -f2 is the
        # directory. The command is correct only relative to a path shape it does not
        # check. If this control ever stops reproducing the error, the pipeline has
        # changed shape and every historical figure needs re-deriving.
        wrong = census(audits, "*")
        print()
        print(f"  [control 4] pinned form (root, 'audits/') : {sorted(inc)}")
        print(f"  [control 4] broken form (inside, '*')     : {sorted(wrong)}")
        # The broken form does not simply yield "filenames": with a nested hit it yields a
        # SUBDIRECTORY name too ('sub'), so the honest property is not "everything ends in
        # .md" but "nothing extracted is an audit directory at all". Asserting the narrower
        # shape made this control fail on its own fixture — the first version of this file
        # did exactly that, which is why the property is stated structurally now.
        extracts_no_real_dirs = bool(wrong) and not (wrong & set(FIXTURES))
        if wrong == inc or not extracts_no_real_dirs:
            failures.append(
                "control 4: the 77/175 unit error no longer reproduces — the pipeline's "
                "output shape has changed and this regression control has gone SILENT"
            )
        else:
            print("  [control 4] broken form yields FILENAMES, not directories — the "
                  "77/175 unit error is still reproducible, so the control is live")

        # --- control 5: the positional-parse fragility itself -------------------
        # `cut -d/ -f2` is a POSITIONAL parse of another tool's output, and its correct
        # field index depends on the target's path shape. That is precisely the mechanism
        # Wu's Class B names (positional parsing of tool output is a latent failure) and
        # that our own discipline warns about. Demonstrated, not asserted: the same
        # command text against the same tree, differing only in target shape, disagrees.
        print()
        dotted = census(audits, ".")          # './dir/file.md'  -> -f2 = dir  (accidentally right)
        starred = census(audits, "*")         # 'dir/file.md'    -> -f2 = file (wrong)
        print(f"  [control 5] target '.' -> {sorted(dotted)}")
        print(f"  [control 5] target '*' -> {sorted(starred)}")
        if dotted == starred:
            failures.append(
                "control 5: target shape no longer changes the extracted field — the "
                "fragility this control documents has been fixed or has moved"
            )
        else:
            print("  [control 5] same command, same tree, different target SHAPE -> "
                  "different extracted field. The census is a positional parse whose "
                  "field index is unchecked. Use an explicit 'audits/' target, never '.'")

        print()
        if failures:
            print("CONTROL FAILED — the census instrument does not discriminate as "
                  "pre-registered. No figure computed with it may be cited.")
            for f in failures:
                print(f"  - {f}")
            return 1
        print(f"CONTROL PASSED — {len(FIXTURES)} fixtures classified correctly, "
              "empty-dir defect reproduced, cwd regression control live.")
        return 0
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
