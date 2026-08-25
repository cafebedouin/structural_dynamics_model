#!/usr/bin/env python3
"""Grammar checker for `.claude/skills/plan-review/RUNS.md` — the plan-review cost meter.

WHAT THIS CHECKS, AND WHAT IT DELIBERATELY DOES NOT (OQ-337, D2).

It checks CONSISTENCY PROPERTIES DECIDABLE FROM THE FILE ALONE: id uniqueness, id grammar,
date agreement, row arity, the mandatory last-field prefix, the line vocabulary, and the
absence of unfilled placeholders. That is why the gate row is named `ledger grammar` and not
`runs ledger` — a row named for the ledger reading green would be skimmed as "the ledger is in
good order", which is exactly the claim this checker REFUSES to make. It cannot see a missing
append. A run that never landed a row leaves this file perfectly grammatical.

The missing-append remedy is the PROCEDURE in `.claude/skills/plan-review/SKILL.md` (the
landing chain: planner composes, first write-capable session lands, evaluator registers
retroactively), not this checker.

CONTROL BURDEN, DECLARED UNMET. OQ-337 specifies a two-sided ABSENCE detector: fires on a
referenced-but-absent run-id, declines on a registered one. This is a grammar/uniqueness
checker and is not that detector. Under the SKILL.md grammar the referenced-but-absent class is
removed by construction (executor prompts carry no ids — the id is allocated at landing), but
that prevention claim is itself uncontrolled. The burden is recorded UNMET in OQ-337, not
claimed covered here.

ONE THING BEYOND GRAMMAR, ADDED 2026-08-25: annotation debt. The `post-impl gaps` column is the
only outcome-bearing field in the ledger and the only one that has ever produced a finding the
review loop could not (the "check that cannot fail / name that does not match what it counts"
species, discovered across rows 2026-08-21-3 and 2026-08-24-1 and promoted into CLAUDE.md). It
was 40% empty when measured (4 of 10 rows, 2026-08-25) under a prose-only obligation. Empty is
indistinguishable from "no gaps were found", so the column was silently unreadable. It is now a
FINDING once a row is 7 days past its planning date with nothing written. This arm is
clock-dependent and therefore NOT decidable from the file alone — it is kept separate from the
grammar arm for that reason, and carries its own date-relative fixtures.

DISCRIMINATION RECORD (this checker's own two-sided control, in --check's selftest):
  FIRES    on a planted duplicate `2026-08-21-1` — the reconstructed OQ-337 instance-3 state,
           the collision that actually occurred and was caught only by a human re-derivation.
  FIRES    on a 30-day-old row with an empty `post-impl gaps:`; DECLINES on the same row dated
           today (inside grace), annotated, or annotated `UNRECORDED` (the escape).
  FIRES    on a missing file, a not-in-index file, a placeholder-bearing row, a wrong-arity
           row, `UNRECORDED` in field 1/2, and a bare `UNRECORDED` in the last field.
  DECLINES on a clean fixture AND on the live file at its current state.
  DISCRIMINATES exit 1 (finding: path not in index) from other nonzero (instrument failure:
           git fatal / no git binary) — a fixture outside the repository exercises the second.
"""

import argparse
import datetime
import os
import re
import shutil
import subprocess
import sys
import tempfile

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LEDGER = ".claude/skills/plan-review/RUNS.md"

# Row anchor is YEAR-AGNOSTIC by construction and uses EXPLICIT REPETITION, never `{4}`.
# A year-scoped anchor (`^2026-`) returns an empty set from the next January, which reads
# exactly like a file with no rows; `{4}` is not portable across awk implementations, and the
# same notation habit carried into a regex dialect without it produces the same empty set.
ROW_RE = re.compile(r"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] \|")
ID_RE = re.compile(r"^([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9])-([0-9]+)$")
DATE_RE = re.compile(r"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$")
BRACKET_RE = re.compile(r"<[^>]*>")
LAST_PREFIX = "post-impl gaps:"
UNRECORDED = "UNRECORDED"

# The live head note's first line reads `Note:` (colon), not `Note ` (space). The four-form
# vocabulary in the design is written `Note ...`, which that line satisfies; the space-suffixed
# literal governs only what a head-note REWRITE must compose. Accepting `^Note` is faithful to
# the vocabulary and is not a widening: it admits no row and no unclassifiable line.
NOTE_RE = re.compile(r"^Note")
HEADER_RE = re.compile(r"^#")

DECLARATION = """\
  enforced here : line vocabulary (# / Note.. / blank / date-led row); row arity EXACTLY 10 or
                  11 fields; field 1 a real YYYY-MM-DD; field 2 a `<date>-<n>` run-id whose
                  embedded date equals field 1; run-id UNIQUENESS; the last field's mandatory
                  `post-impl gaps:` prefix; `UNRECORDED` never in fields 1-2; no `<...>` pair
                  in any row; and no-trailing-separator (enforced through its consequences --
                  a trailing `|` makes a 10-field row's last field empty, failing the prefix,
                  and a 12-field row, failing arity).
  field-2-only  : id grammar, date-agreement and uniqueness are evaluated on FIELD 2 ONLY. No
                  other field is scanned for id-shaped tokens. This is a SCOPE decision, not a
                  scan left unbuilt: id-shaped tokens in prose (a provenance note naming a
                  reassigned id) are prose, and a token-scanning implementation would flag
                  exactly the record-keeping the grammar mandates.
  permitted     : bare `UNRECORDED` in fields 3 through n-1 (the retroactive-registration
                  rule). In the last field only as `post-impl gaps: UNRECORDED`, behind the
                  prefix. In fields 1-2 never -- the date and the id are what the lander
                  allocates against, so both are always writable at landing time.
  procedure     : the annotation-position rule (the provenance field sits immediately before
                  the last field) and the no-literal-`|`-in-values rule live in SKILL.md and
                  bind the LANDER, not this gate. An annotation in the wrong middle field
                  passes here.
  residue       : a stray `|` that yields exactly 11 fields PASSES -- it is indistinguishable
                  from a legitimate provenance row. Declared, not hidden.
  also enforced : ANNOTATION DEBT -- a landed row whose `post-impl gaps:` is still empty more
                  than 7 days after its PLANNING date. Separate arm, own date-relative
                  fixtures, clock-dependent (so not decidable from the file alone). Discharge
                  with the real gaps or `post-impl gaps: UNRECORDED`; both always available.
  not checked   : whether any run is MISSING from the ledger. See this file's docstring."""


def index_status(path):
    """Return (state, detail). Discriminates a FINDING from an INSTRUMENT failure.

    exit 0  -> OK          : the path is in the git index.
    exit 1  -> FINDING     : the path is not in the index (`git rm --cached`, or a recreated
                             file never `git add`ed). This is the checked condition.
    other   -> INSTRUMENT  : git fatal (not a repository / path outside it) or no git binary.
                             Reported as instrument failure, NEVER as an untracked ledger.
    """
    try:
        p = subprocess.run(
            ["git", "ls-files", "--error-unmatch", "--", path],
            cwd=REPO, capture_output=True, text=True,
        )
    except (FileNotFoundError, OSError) as exc:
        return "INSTRUMENT", "git could not be executed: %s" % exc
    if p.returncode == 0:
        return "OK", ""
    if p.returncode == 1:
        return "FINDING", "path is not in the git index"
    return "INSTRUMENT", "git exited %d: %s" % (p.returncode, (p.stderr or "").strip())


def check_ledger(path, check_index=True):
    """Return (findings, instrument_errors). Both are lists of strings."""
    findings, instrument = [], []
    disp = os.path.relpath(path, REPO) if path.startswith(REPO) else path

    # File-absent is a FAILURE, never a skip: a checker that skips on absence reports green on
    # the one state that most needs to be loud.
    if not os.path.isfile(path):
        findings.append("%s: ledger file is ABSENT from the working tree "
                        "(absence is a failure, never a skip)" % disp)
        return findings, instrument

    if check_index:
        state, detail = index_status(path)
        if state == "FINDING":
            findings.append("%s: %s -- appends to an untracked ledger are not "
                            "git-witnessable" % (disp, detail))
        elif state == "INSTRUMENT":
            instrument.append("%s: %s" % (disp, detail))

    with open(path, "r", encoding="utf-8") as fh:
        lines = fh.read().split("\n")
    if lines and lines[-1] == "":
        lines.pop()  # trailing newline, not a line

    seen_ids = {}
    for n, line in enumerate(lines, 1):
        where = "%s:%d" % (disp, n)
        if ROW_RE.match(line):
            findings.extend(check_row(where, line, seen_ids))
        elif HEADER_RE.match(line) or NOTE_RE.match(line) or line.strip() == "":
            continue
        else:
            findings.append("%s: line matches none of the four permitted forms "
                            "(`#` header / `Note...` / blank / date-led row): %r"
                            % (where, line[:70]))
    return findings, instrument


def check_row(where, line, seen_ids):
    out = []
    fields = [f.strip() for f in line.split("|")]

    if len(fields) not in (10, 11):
        out.append("%s: row has %d pipe-delimited fields; exactly 10 (no annotation) or 11 "
                   "(base + one provenance field) are legitimate. Most likely cause: a literal "
                   "`|` inside a composed value -- values may not contain one (substitute `/` "
                   "or `;`); a pipe inside free text shifts every positional read after it. "
                   "Other cause: a trailing separator, which adds a spurious empty final field."
                   % (where, len(fields)))
        return out  # positional reads below are meaningless once arity is wrong

    if BRACKET_RE.search(line):
        out.append("%s: row contains an angle-bracket pair. Cause 1: an UNFILLED PLACEHOLDER "
                   "(`<allocated-at-append>`, `<rounds -- planner supplies>`) -- a composed row "
                   "was landed without substituting its values, which is the defect this rule "
                   "exists for. Cause 2: legitimate prose that happens to use brackets (`n<3`, "
                   "a generic). The ban rests on a prediction about future content, so if the "
                   "text is genuine prose, that is a finding about THIS RULE -- route it, do "
                   "not rephrase the row to satisfy the checker." % where)

    date, run_id, last = fields[0], fields[1], fields[-1]

    if not DATE_RE.match(date) or not _real_date(date):
        out.append("%s: field 1 is not a real YYYY-MM-DD date: %r" % (where, date))
    if date == UNRECORDED:
        out.append("%s: field 1 is `UNRECORDED`; the planning date is never UNRECORDED "
                   "(it is what the lander allocates against)" % where)

    if run_id == UNRECORDED:
        out.append("%s: field 2 is `UNRECORDED`; the run-id is never UNRECORDED "
                   "(it is allocated at landing time)" % where)
    else:
        m = ID_RE.match(run_id)
        if not m:
            out.append("%s: field 2 is not a `<date>-<n>` run-id: %r" % (where, run_id))
        else:
            if m.group(1) != date:
                out.append("%s: run-id %r embeds date %s but field 1 is %s -- both are the "
                           "PLANNING date and must agree" % (where, run_id, m.group(1), date))
            if run_id in seen_ids:
                out.append("%s: DUPLICATE run-id %r (first seen at line %d). Two targets under "
                           "one id is OQ-337's collision class." % (where, run_id, seen_ids[run_id]))
            else:
                seen_ids[run_id] = int(where.rsplit(":", 1)[1])

    if not last.startswith(LAST_PREFIX):
        out.append("%s: the last field must begin `%s` unconditionally (an empty value after "
                   "the prefix is permitted; an unreconstructible status is written "
                   "`%s %s`, never a bare `%s`). Found: %r"
                   % (where, LAST_PREFIX, LAST_PREFIX, UNRECORDED, UNRECORDED, last[:50]))
    return out


def _real_date(s):
    try:
        datetime.date.fromisoformat(s)
        return True
    except ValueError:
        return False


ANNOTATION_GRACE_DAYS = 7


def check_annotation_debt(path, today=None, grace_days=ANNOTATION_GRACE_DAYS):
    """Rows whose `post-impl gaps:` is still EMPTY more than `grace_days` after planning.

    Separate from the grammar arm on purpose. Grammar is decidable from the file alone and its
    fixtures pin literal dates; this arm is decidable only against a clock, so a shared fixture
    set would rot the moment those literal dates aged past the window.

    Why it is a FINDING and not a report: the obligation already existed in SKILL.md prose and
    was 40% unmet (4 of 10 rows, measured 2026-08-25). An empty column is indistinguishable
    from `no gaps were found`, which makes the ledger's one outcome-bearing column silently
    unreadable -- and that column is where every novel finding this apparatus has produced came
    from. The discharge is cheap and always available: annotate with the real gaps, or write
    `post-impl gaps: UNRECORDED` when they cannot be reconstructed. Neither is ever blocked.
    """
    findings = []
    if not os.path.isfile(path):
        return findings  # absence is the grammar arm's finding; not double-reported here
    today = today or datetime.date.today()
    disp = os.path.relpath(path, REPO) if path.startswith(REPO) else path

    with open(path, "r", encoding="utf-8") as fh:
        lines = fh.read().split("\n")

    for n, line in enumerate(lines, 1):
        if not ROW_RE.match(line):
            continue
        fields = [f.strip() for f in line.split("|")]
        if len(fields) not in (10, 11):
            continue  # arity is the grammar arm's finding; positional reads are meaningless
        last = fields[-1]
        if not last.startswith(LAST_PREFIX):
            continue  # likewise
        if last[len(LAST_PREFIX):].strip():
            continue  # annotated (any value, including UNRECORDED, discharges the obligation)
        if not DATE_RE.match(fields[0]) or not _real_date(fields[0]):
            continue
        age = (today - datetime.date.fromisoformat(fields[0])).days
        if age > grace_days:
            findings.append(
                "%s:%d: run %s (%s) planned %d days ago and its `post-impl gaps:` is still "
                "EMPTY -- empty is indistinguishable from `no gaps were found`. Annotate it "
                "with the real gaps, or `post-impl gaps: UNRECORDED` if they cannot be "
                "reconstructed. Grace is %d days."
                % (disp, n, fields[1], fields[2][:40], age, grace_days))
    return findings


# --------------------------------------------------------------------------------------
# Selftest. Rides every --check run (the audit_writeup_gate.py pattern): a checker whose
# controls are only run on request is a checker whose red-capability is a memory.
# --------------------------------------------------------------------------------------

CLEAN = (
    "# ledger\n"
    "\n"
    "Note: a head note.\n"
    "Note (2026-08-21, with parens): another.\n"
    "\n"
    "2026-08-21 | 2026-08-21-1 | target A | 3 rounds | 2 agents | f: 1 | d: 0 | rulings 1 | "
    "fresh-pass finds: 0 | post-impl gaps:\n"
    "2026-08-21 | 2026-08-21-2 | target B | 3 rounds | UNRECORDED | f: 1 | d: 0 | rulings 1 | "
    "fresh-pass finds: 0 | prov: reassigned from -1 | post-impl gaps: 2 (things)\n"
)


def _fixtures():
    """(name, content-or-None, expect_fire, note). None content => file never created."""
    dup = CLEAN.replace("2026-08-21-2 | target B", "2026-08-21-1 | target B")
    ph = CLEAN.replace("| target A |", "| <target -- planner supplies> |")
    arity = CLEAN.replace("| target B |", "| target | B |")
    unrec12 = CLEAN.replace("2026-08-21 | 2026-08-21-2 |", "2026-08-21 | UNRECORDED |")
    bare = CLEAN.replace("| post-impl gaps: 2 (things)", "| UNRECORDED")
    skew = CLEAN.replace("2026-08-21 | 2026-08-21-2 |", "2026-08-22 | 2026-08-21-2 |")
    vocab = CLEAN.replace("Note: a head note.", "a stray unclassifiable line")
    trail = CLEAN.rstrip("\n") + " |\n"
    return [
        ("clean", CLEAN, False, "declines on a well-formed ledger (10- and 11-field rows)"),
        ("duplicate-id", dup, True, "reconstructed OQ-337 instance-3 collision"),
        ("missing-file", None, True, "absence is a failure, never a skip"),
        ("placeholder-row", ph, True, "an unfilled `<...>` placeholder was landed"),
        ("wrong-arity", arity, True, "a literal `|` inside a value (12 fields)"),
        ("unrecorded-field2", unrec12, True, "`UNRECORDED` in field 2"),
        ("bare-unrecorded-last", bare, True, "bare `UNRECORDED` without the mandatory prefix"),
        ("date-skew", skew, True, "field 1 disagrees with the id's embedded date"),
        ("bad-vocabulary", vocab, True, "a line matching none of the four forms"),
        ("trailing-separator", trail, True, "trailing `|` -> empty final field, prefix fails"),
    ]


def selftest(verbose=True):
    tmp = tempfile.mkdtemp(prefix=".runs_ledger_selftest_", dir=REPO)
    failures = []
    try:
        for name, content, expect_fire, note in _fixtures():
            p = os.path.join(tmp, "RUNS.md")
            if os.path.exists(p):
                os.remove(p)
            if content is not None:
                with open(p, "w", encoding="utf-8") as fh:
                    fh.write(content)
            # check_index=False here: these fixtures test the GRAMMAR arm.
            f, inst = check_ledger(p, check_index=False)
            fired = bool(f)
            ok = fired == expect_fire
            if not ok:
                failures.append("%s: expected %s, got %d finding(s): %s"
                                % (name, "FIRE" if expect_fire else "DECLINE", len(f), f[:2]))
            if verbose:
                print("    %-22s %-8s %s  (%s)"
                      % (name, "FIRES" if fired else "declines", "ok" if ok else "MISMATCH", note))

        # --- the index arm, two-sided on the EXIT-CODE discrimination ---
        # F8: the not-in-index fixture is created INSIDE the work tree and deliberately never
        # `git add`ed. A /tmp file is outside the repository and exits through git's FATAL
        # path -- it would witness instrument failure, not untracked-ness, and the non-firing
        # would read as a checker bug.
        inside = os.path.join(tmp, "never_added.md")
        with open(inside, "w", encoding="utf-8") as fh:
            fh.write(CLEAN)
        st, _ = index_status(inside)
        ok = st == "FINDING"
        if not ok:
            failures.append("not-in-index (inside work tree): expected FINDING, got %s" % st)
        if verbose:
            print("    %-22s %-8s %s  (inside work tree, never `git add`ed)"
                  % ("not-in-index", st, "ok" if ok else "MISMATCH"))

        outside = os.path.join(tempfile.gettempdir(), "runs_ledger_outside_repo.md")
        with open(outside, "w", encoding="utf-8") as fh:
            fh.write(CLEAN)
        st2, _ = index_status(outside)
        ok2 = st2 == "INSTRUMENT"
        if not ok2:
            failures.append("outside-repo: expected INSTRUMENT, got %s" % st2)
        if verbose:
            print("    %-22s %-8s %s  (outside the repository -- git fatal, NOT untracked)"
                  % ("outside-repo", st2, "ok" if ok2 else "MISMATCH"))
        os.remove(outside)

        # --- the annotation-debt arm, two-sided, on DATE-RELATIVE fixtures ---
        # Its own fixtures because the grammar fixtures pin literal dates: a shared set would
        # start firing here the moment those dates aged past the window, and the failure would
        # read as a checker bug rather than as fixture rot.
        today = datetime.date.today()
        old = (today - datetime.timedelta(days=30)).isoformat()
        row = ("%s | %s-1 | target A | 3 rounds | 2 agents | f: 1 | d: 0 | rulings 1 | "
               "fresh-pass finds: 0 | post-impl gaps:%s\n")
        debt_cases = [
            ("debt-stale-empty", row % (old, today.isoformat(), ""), True,
             "planned 30 days ago, still unannotated"),
            ("debt-fresh-empty", row % (today.isoformat(), today.isoformat(), ""), False,
             "planned today -- inside grace, not yet owed"),
            ("debt-stale-filled", row % (old, today.isoformat(), " 3 (things)"), False,
             "annotated: obligation discharged"),
            ("debt-stale-unrecorded", row % (old, today.isoformat(), " UNRECORDED"), False,
             "the always-available escape discharges it too"),
        ]
        for name, content, expect_fire, note in debt_cases:
            p = os.path.join(tmp, "RUNS.md")
            # the id embeds today's date while field 1 may be `old`: date-skew is the GRAMMAR
            # arm's finding and is irrelevant here -- this arm is called directly, not through
            # check_ledger, so the two arms cannot mask one another.
            with open(p, "w", encoding="utf-8") as fh:
                fh.write("# ledger\n\n" + content)
            d = check_annotation_debt(p, today=today)
            fired = bool(d)
            ok = fired == expect_fire
            if not ok:
                failures.append("%s: expected %s, got %d finding(s)"
                                % (name, "FIRE" if expect_fire else "DECLINE", len(d)))
            if verbose:
                print("    %-22s %-8s %s  (%s)"
                      % (name, "FIRES" if fired else "declines", "ok" if ok else "MISMATCH", note))

        # --- and it must DECLINE on the live file: a control that only fires is one-sided ---
        live = os.path.join(REPO, LEDGER)
        lf, _li = check_ledger(live, check_index=True)
        ok3 = not lf
        if not ok3:
            failures.append("live-file: expected DECLINE, got %d finding(s)" % len(lf))
        if verbose:
            print("    %-22s %-8s %s  (the naturally-arising negative)"
                  % ("live-file", "FIRES" if lf else "declines", "ok" if ok3 else "MISMATCH"))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    return failures


def main(argv):
    ap = argparse.ArgumentParser(add_help=False)
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--reporting", action="store_true")
    ap.add_argument("-h", "--help", action="store_true")
    args, extra = ap.parse_known_args(argv)

    if args.help or not args.check or extra:
        sys.stderr.write(
            "usage: runs_ledger_check.py --check [--reporting]\n"
            "\n"
            "  --check      validate %s (selftest rides along).\n"
            "  --reporting  R-B first-run mode: print findings but ALWAYS exit 0.\n"
            "\n"
            "No default action: invoked without --check this exits 2 rather than guessing.\n"
            % LEDGER)
        return 2

    print("runs_ledger_check: %s" % LEDGER)
    print("  -- what this enforces, and what it does not --")
    print(DECLARATION)

    print("  -- selftest (discrimination record) --")
    st_failures = selftest(verbose=True)

    findings, instrument = check_ledger(os.path.join(REPO, LEDGER), check_index=True)
    findings.extend(check_annotation_debt(os.path.join(REPO, LEDGER)))

    print("  -- live file --")
    for i in instrument:
        print("    INSTRUMENT FAILURE: %s" % i)
    for f in findings:
        print("    FINDING: %s" % f)

    bad = bool(st_failures or findings or instrument)
    if st_failures:
        for s in st_failures:
            print("    SELFTEST FAILURE: %s" % s)

    if bad:
        summary = "ledger grammar: %d finding(s), %d instrument failure(s), %d selftest failure(s)" % (
            len(findings), len(instrument), len(st_failures))
        if args.reporting:
            print("REPORTING-ONLY (R-B, first live run): " + summary + " -- NOT blocking")
            return 0
        print(summary)
        return 1

    n_rows = sum(1 for ln in open(os.path.join(REPO, LEDGER), encoding="utf-8")
                 if ROW_RE.match(ln))
    tail = " [reporting-only mode]" if args.reporting else ""
    print("ledger grammar OK: %d rows, ids unique, grammar clean%s" % (n_rows, tail))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
