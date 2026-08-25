#!/usr/bin/env python3
"""OQ-356 — read-site checker for the Phase-3 Contamination Collapse table.

Parses `### Contamination Collapse Analysis` out of a giant_component_analysis.md
artifact and checks the acceptance criteria the plan pre-committed:

  criterion 2 (conservation), TWO identities, never one:
      (1) NS + NB + NW + ND == NKept          band coverage of the filtered domain
      (2) NKept + NExcluded == |Members|      the guard's partition
    Split deliberately: the four bands cover [-0.01, 1.01) while the filter admits
    ANY numeric EP >= 0.0, so a numeric value at/above 1.01 lands in no band and
    breaks (1). Conflated into one identity that would read as a GUARD bug.

  criterion 3 (monotonicity across the cap sweep):
      cap rises 0.10 -> 1.00, so contamination only increases:
      ND non-decreasing, NS non-increasing across the ten rows.

PRE-FIX artifacts carry no coverage line. That is not an error — the coverage
line is what this OQ adds. With --require-coverage absent, only criterion 3 runs
and criterion 2 is reported UNCHECKED (never silently "passed": an absent input
must not satisfy a gate).

Exit 0 = all requested checks pass. Exit 1 = a check FAILED. Exit 2 = the input
could not be parsed (no section / no rows) — distinguished from a failure so a
missing section can never read as a pass.
"""
import argparse, re, sys

SECTION = "### Contamination Collapse Analysis"
# data row: | 0.10 | 12 | 3 | 4 | 5 |   (header + separator rejected by int())
ROW = re.compile(r"^\|\s*([0-9]*\.?[0-9]+)\s*\|\s*(\d+)\s*\|\s*(\d+)\s*\|\s*(\d+)\s*\|\s*(\d+)\s*\|\s*$")
# Coverage line emitted by report_contamination_collapse_analysis/2 (OQ-356).
# Anchored on the STABLE part of the line (the two counts and the word "excluded"),
# deliberately not on the prose: the label was corrected once already, before first
# publication, because it named only the non-numeric cause while NExcluded counts
# the complement of the whole conjunction. A parser keyed on the prose would have
# gone silently blind at that correction rather than loud.
COV = re.compile(r"\*\*Purity coverage\*\*:\s*(\d+)\s*of\s*(\d+)\s*giant-component members.*?"
                 r"(\d+)\s*excluded", re.S)


def parse(path):
    text = open(path, encoding="utf-8", errors="replace").read()
    i = text.find(SECTION)
    if i < 0:
        return None, None, "section %r not found" % SECTION
    body = text[i:]
    # stop at the next section header so we never absorb a later table
    j = body.find("\n### ", len(SECTION))
    k = body.find("\n## ", len(SECTION))
    ends = [e for e in (j, k) if e > 0]
    if ends:
        body = body[:min(ends)]
    rows = []
    for ln in body.splitlines():
        m = ROW.match(ln.rstrip())
        if m:
            rows.append((float(m.group(1)), int(m.group(2)), int(m.group(3)),
                         int(m.group(4)), int(m.group(5))))
    cm = COV.search(body)
    cov = (int(cm.group(1)), int(cm.group(2)), int(cm.group(3))) if cm else None
    if not rows:
        return None, None, "section found but no data rows parsed"
    return rows, cov, None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("artifact")
    ap.add_argument("--require-coverage", action="store_true",
                    help="fail if the OQ-356 coverage line is absent (post-fix runs)")
    ap.add_argument("--expect-excluded", type=int, default=None,
                    help="assert NExcluded equals this exactly (invariance legs: 0)")
    ap.add_argument("--compare-rows", metavar="OTHER.md",
                    help="V6c invariance oracle: assert the TEN COUNT ROWS are identical to "
                         "those of OTHER.md. SCOPED TO THE COUNT ROWS ON PURPOSE — a whole-table "
                         "or whole-file diff is GUARANTEED to differ, because the post-fix "
                         "artifact carries the coverage line this OQ deliberately adds, and that "
                         "difference would false-alarm on a change the fix itself introduced.")
    ap.add_argument("--label", default="")
    a = ap.parse_args()

    rows, cov, err = parse(a.artifact)
    if err:
        print("PARSE-FAIL %s: %s" % (a.artifact, err))
        return 2

    tag = a.label or a.artifact
    print("== %s ==" % tag)
    print("rows parsed: %d" % len(rows))
    fails = []

    if len(rows) != 10:
        fails.append("expected 10 cap rows (0.10..1.00), parsed %d" % len(rows))

    # --- criterion 3: monotonicity -----------------------------------------
    print("\n-- criterion 3: monotonicity across the cap sweep --")
    print("  %-6s %8s %10s %8s %9s %8s" % ("Cap", "Sound", "Borderline", "Warning", "Degraded", "sum"))
    for cap, ns, nb, nw, nd in rows:
        print("  %-6.2f %8d %10d %8d %9d %8d" % (cap, ns, nb, nw, nd, ns + nb + nw + nd))
    for idx in range(1, len(rows)):
        if rows[idx][4] < rows[idx - 1][4]:
            fails.append("ND DECREASED at cap %.2f: %d -> %d"
                         % (rows[idx][0], rows[idx - 1][4], rows[idx][4]))
        if rows[idx][1] > rows[idx - 1][1]:
            fails.append("NS INCREASED at cap %.2f: %d -> %d"
                         % (rows[idx][0], rows[idx - 1][1], rows[idx][1]))
    mono = not any(f.startswith(("ND DECREASED", "NS INCREASED")) for f in fails)
    print("  ND non-decreasing AND NS non-increasing : %s" % ("HOLDS" if mono else "FAILS"))

    # --- criterion 2: the two identities -----------------------------------
    print("\n-- criterion 2: conservation (two identities) --")
    if cov is None:
        if a.require_coverage:
            fails.append("coverage line ABSENT but --require-coverage was given")
            print("  coverage line: ABSENT -> FAIL (required)")
        else:
            print("  coverage line: ABSENT -> criterion 2 UNCHECKED "
                  "(pre-fix artifact; absence must not read as a pass)")
    else:
        nkept, nmembers, nexcl = cov
        print("  coverage line: NKept=%d  |Members|=%d  NExcluded=%d" % (nkept, nmembers, nexcl))
        # (1) per row -- also tests that the kept set is cap-invariant
        bad = [(c, s + b + w + d) for c, s, b, w, d in rows if s + b + w + d != nkept]
        if bad:
            fails.append("identity (1) NS+NB+NW+ND == NKept FAILED at caps: %s (NKept=%d)"
                         % (", ".join("%.2f->%d" % t for t in bad), nkept))
        print("  (1) NS+NB+NW+ND == NKept on all %d rows : %s"
              % (len(rows), "HOLDS" if not bad else "FAILS"))
        # (2) once
        ok2 = (nkept + nexcl == nmembers)
        if not ok2:
            fails.append("identity (2) NKept+NExcluded == |Members| FAILED: %d+%d != %d"
                         % (nkept, nexcl, nmembers))
        print("  (2) NKept+NExcluded == |Members|        : %s"
              % ("HOLDS" if ok2 else "FAILS"))
        if a.expect_excluded is not None:
            ok3 = (nexcl == a.expect_excluded)
            if not ok3:
                fails.append("NExcluded == %d expected, got %d" % (a.expect_excluded, nexcl))
            print("  NExcluded == %d (expected)             : %s"
                  % (a.expect_excluded, "HOLDS" if ok3 else "FAILS"))
        # criterion 5 tell: a zero subtrahend is a DEGENERATE pass, not a witness
        if nexcl == 0 and a.expect_excluded is None:
            print("  NOTE: NExcluded == 0 -> identity (2) is DEGENERATE on this leg "
                  "(criterion 5: a pass here is not a witness for criterion 2)")

    # --- V6c: exact invariance against a pre-fix capture ---------------------
    if a.compare_rows:
        print("\n-- V6c: count-row invariance vs %s --" % a.compare_rows)
        other, _ocov, oerr = parse(a.compare_rows)
        if oerr:
            print("  PARSE-FAIL on the comparison artifact: %s" % oerr)
            return 2
        if other == rows:
            print("  all %d count rows IDENTICAL (Cap/Sound/Borderline/Warning/Degraded)" % len(rows))
            print("  the coverage line is NEW and EXPECTED; it is deliberately not diffed")
        else:
            fails.append("count rows DIFFER from %s" % a.compare_rows)
            for idx in range(max(len(rows), len(other))):
                r = rows[idx] if idx < len(rows) else None
                o = other[idx] if idx < len(other) else None
                if r != o:
                    print("  row %d differs: post=%s  pre=%s" % (idx, r, o))

    print("\nVERDICT: %s" % ("PASS" if not fails else "FAIL"))
    for f in fails:
        print("  FAIL: %s" % f)
    return 1 if fails else 0


# ---------------------------------------------------------------------------
# SELFTEST — this parser is an INTRODUCED INSTRUMENT, so it owes its own
# discrimination record: not "does it run" but "does it DECLINE a case it must
# decline". Planted fixtures, two-sided: one clean table that must PASS, and one
# fixture per failure mode that must FAIL, plus a no-section input that must
# come back UNPARSEABLE (exit 2) rather than passing on absence.
# ---------------------------------------------------------------------------
_CLEAN = """## Phase 3

### Contamination Collapse Analysis

Current settings: cap=0.30, attenuation=0.50
**Purity coverage**: 8 of 10 giant-component members are banded below; 2 excluded
from the bands (no effective purity, non-numeric, or numeric below the 0.0 floor).

| Cap | Sound (>=0.70) | Borderline | Warning | Degraded (<0.30) |
|-----|--------|------------|---------|---------|
| 0.10 | 5 | 1 | 1 | 1 |
| 0.20 | 5 | 1 | 1 | 1 |
| 0.30 | 4 | 2 | 1 | 1 |
| 0.40 | 4 | 1 | 2 | 1 |
| 0.50 | 3 | 2 | 2 | 1 |
| 0.60 | 3 | 1 | 2 | 2 |
| 0.70 | 2 | 2 | 2 | 2 |
| 0.80 | 2 | 1 | 2 | 3 |
| 0.90 | 1 | 1 | 3 | 3 |
| 1.00 | 0 | 1 | 3 | 4 |

## Phase 4
"""

def _mut(text, old, new):
    assert text.count(old) == 1, "fixture anchor not unique: %r" % old
    return text.replace(old, new)

def _selftest():
    import tempfile, os, io, contextlib
    cases = []
    cases.append(("clean table", _CLEAN, 0, True))
    # monotonicity: ND drops between two rows
    # NOTE: this mutation must make ND genuinely DECREASE against the PREVIOUS
    # row, and must keep the band sum at NKept so it isolates criterion 3 from
    # identity (1). A first draft moved ND 2->1 at cap 0.60, where the previous
    # row already read 1 — no decrease, and the selftest correctly declined it.
    # ND runs 1,1,1,1,1,2,2,3,3,4; dropping the 0.80 row's 3 to 1 is a real fall.
    cases.append(("ND decreases (criterion 3)",
                  _mut(_CLEAN, "| 0.80 | 2 | 1 | 2 | 3 |", "| 0.80 | 2 | 3 | 2 | 1 |"), 1, True))
    # monotonicity: NS rises between two rows
    cases.append(("NS increases (criterion 3)",
                  _mut(_CLEAN, "| 0.50 | 3 | 2 | 2 | 1 |", "| 0.50 | 5 | 0 | 2 | 1 |"), 1, True))
    # identity (1): a row's bands do not sum to NKept (a band-coverage bug)
    cases.append(("band sum != NKept (identity 1)",
                  _mut(_CLEAN, "| 0.30 | 4 | 2 | 1 | 1 |", "| 0.30 | 4 | 1 | 1 | 1 |"), 1, True))
    # identity (2): NKept + NExcluded != |Members| (a partition bug)
    cases.append(("NKept+NExcluded != |Members| (identity 2)",
                  _mut(_CLEAN, "**Purity coverage**: 8 of 10 giant-component members",
                               "**Purity coverage**: 8 of 11 giant-component members"), 1, True))
    # coverage line absent + --require-coverage
    cases.append(("coverage line absent, required",
                  _CLEAN[:_CLEAN.index("**Purity coverage**")] +
                  _CLEAN[_CLEAN.index("| Cap |"):], 1, True))
    # no section at all -> exit 2, NOT a pass
    cases.append(("no section (must be UNPARSEABLE, not a pass)",
                  "# nothing here\n\n## Phase 4\n", 2, True))

    print("=== parse_collapse_table.py SELFTEST (planted fixtures) ===")
    bad = 0
    for name, text, want, require_cov in cases:
        fd, path = tempfile.mkstemp(suffix=".md"); os.close(fd)
        open(path, "w", encoding="utf-8").write(text)
        argv = [path]
        if name.startswith("coverage line absent"):
            argv.append("--require-coverage")
        buf = io.StringIO()
        old = sys.argv
        sys.argv = ["parse_collapse_table.py"] + argv
        try:
            with contextlib.redirect_stdout(buf):
                got = main()
        finally:
            sys.argv = old
            os.unlink(path)
        ok = (got == want)
        bad += (0 if ok else 1)
        print("  %-46s want exit %d, got %d  %s" % (name, want, got, "ok" if ok else "*** MISMATCH ***"))
    print("=== selftest %s (%d mismatches) ===" % ("GREEN" if bad == 0 else "RED", bad))
    return 1 if bad else 0


if __name__ == "__main__":
    if "--selftest" in sys.argv:
        sys.exit(_selftest())
    sys.exit(main())
