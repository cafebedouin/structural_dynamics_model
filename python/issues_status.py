#!/usr/bin/env python3
"""issues_status.py — parse and check ISSUES.md status grammar.

Canonical grammar (one line per OQ, first Status line of the section):
    **Status:** <token>[ — <free-text detail>]
    token ∈ {open, investigating, mitigated, partial, resolved, disposed, future}
    (future = closed-but-searchable: a real question deliberately not slated for
     work — won't realistically get done — kept full-bodied so it can be revived;
     not in omega_resolver's ACTIVE set, so it drops out of the workable frontier)

Disposition sub-field (OQ-291 ruling, 2026-08-19) — disposed entries only:
    **Disposition:** <kind>[ — <free-text detail>]
    kind ∈ {retracted, wont_build, superseded, reframed}
    `disposed` conflated four epistemically different closes; the sub-field makes them
    machine-readable without touching the status token set (disposed stays disposed for
    every existing consumer). REQUIRED on disposed entries, forward-only: the 2026-08-19
    backfill labeled every then-disposed entry whose prose was clear and recorded the
    residue — BACKFILL_UNLABELED below — which is exempt. A new disposed entry without a
    Disposition line is malformed. A FIFTH kind is a deliberate grammar amendment (this
    file + the ISSUES.md footer + OQ-291), never a stretch of `reframed`.
    Retraction-count caveat (pre-committed for OQ-288's census): any count over
    Disposition values is a count over TWO labeling regimes — prose-clear backfill +
    enforced forward — and must say so.

Usage:
    python3 python/issues_status.py            # table + counts
    python3 python/issues_status.py --check    # selftest + exit 1 on any malformed entry
    python3 python/issues_status.py open       # list only that token

The checker is the grammar's consumer: if a new entry deviates (old `**Status: x**`
form, missing line, unknown token, DUPLICATE OQ label), --check fails loudly instead
of the deviation silently breaking the next census. See KNOWN_STATE.md 2026-06-04
ledger-sweep entry. Duplicate detection added 2026-06-10: under parallel worktree
instances, two clean-merging entries can claim the same OQ-NN; pre-fix, the second
header's whole entry was silently SKIPPED (`seen` is label-keyed), so the census
showed the first entry and the checker passed — witnessed with a constructed
duplicate before the fix.
"""
import re
import sys
from pathlib import Path

TOKENS = {"open", "investigating", "mitigated", "partial", "resolved", "disposed",
          "future"}
DISPO_TOKENS = {"retracted", "wont_build", "superseded", "reframed"}
# The 2026-08-19 backfill residue: examined, left unlabeled because the prose straddles
# two kinds (OQ-59: "preserve-and-diff, not merge" reads as both wont_build and
# reframed). Enumerable on purpose — an unlabeled-because-ambiguous entry must stay
# distinguishable from unlabeled-because-nobody-looked (OQ-291 ruling). Adding to this
# set is a recorded adjudication, not a convenience.
BACKFILL_UNLABELED = {"OQ-59"}
ISSUES = Path(__file__).resolve().parents[1] / "ISSUES.md"

HEADER = re.compile(r"^## (OQ-\d+) ")
CANON = re.compile(r"^\*\*Status:\*\* (\w+)(?: — .*)?$")
ANY_STATUS = re.compile(r"^\*\*Status\b")
DISPO = re.compile(r"^\*\*Disposition:\*\* (\w+)(?: — .*)?$")
# Exact-form match on purpose: ISSUES.md carries five LEGACY prose headings starting
# `**Disposition (...)` / `**Disposition policy` / `**Disposition record` /
# `**Disposition of` (witnessed on this checker's first live run, 2026-08-19) — those are
# narrative, not the sub-field. A new-style line that typos itself out of this form is
# still caught by the disposed-without-Disposition rule (fail-closed side).
ANY_DISPO = re.compile(r"^\*\*Disposition:\*\*")


def scan(text=None):
    if text is None:
        text = ISSUES.read_text()
    entries, problems = [], []
    cur, seen = None, set()
    headers_seen = set()
    status_of = {}
    dispo_of = {}
    dispo_line = {}
    for lineno, line in enumerate(text.splitlines(), 1):
        m = HEADER.match(line)
        if m:
            if cur and cur not in seen:
                problems.append(f"{cur}: no Status line found")
            cur = m.group(1)
            if cur in headers_seen:
                problems.append(
                    f"{cur} (line {lineno}): duplicate OQ label — a second `## {cur}` "
                    f"header exists (parallel-worktree merge artifact?); the duplicate "
                    f"entry is otherwise INVISIBLE to census and checker"
                )
            headers_seen.add(cur)
            continue
        if cur and cur not in seen and ANY_STATUS.match(line):
            seen.add(cur)
            cm = CANON.match(line)
            if not cm:
                problems.append(f"{cur} (line {lineno}): malformed Status line: {line[:80]!r}")
            elif cm.group(1) not in TOKENS:
                problems.append(f"{cur} (line {lineno}): unknown token {cm.group(1)!r}")
            else:
                entries.append((cur, cm.group(1)))
                status_of[cur] = cm.group(1)
        if cur and cur not in dispo_of and ANY_DISPO.match(line):
            dm = DISPO.match(line)
            if not dm:
                problems.append(
                    f"{cur} (line {lineno}): malformed Disposition line: {line[:80]!r}")
                dispo_of[cur] = None
            else:
                dispo_of[cur] = dm.group(1)
                dispo_line[cur] = lineno
    if cur and cur not in seen:
        problems.append(f"{cur}: no Status line found")
    # Disposition validation (OQ-291): only on disposed; valid kind; required
    # forward-only (backfill residue exempt).
    for oq, kind in dispo_of.items():
        if kind is None:
            continue
        if kind not in DISPO_TOKENS:
            problems.append(
                f"{oq} (line {dispo_line[oq]}): unknown Disposition kind {kind!r} — a "
                f"fifth kind is a grammar amendment (OQ-291), not a new word")
        if status_of.get(oq) != "disposed":
            problems.append(
                f"{oq}: Disposition line on a {status_of.get(oq, '?')} entry — the "
                f"sub-field is defined for disposed entries only (OQ-291)")
    for oq, tok in status_of.items():
        if tok == "disposed" and oq not in dispo_of and oq not in BACKFILL_UNLABELED:
            problems.append(
                f"{oq}: disposed without a **Disposition:** line (required forward-only "
                f"since 2026-08-19; kinds: {'/'.join(sorted(DISPO_TOKENS))} — OQ-291)")
    return entries, problems


def selftest():
    """Five two-sided controls over constructed fixtures. Returns count passed."""
    ok = 0
    good = "## OQ-1 x\n**Status:** disposed — done\n**Disposition:** retracted — why\n"
    _, p = scan(good)
    ok += 1 if not p else 0
    missing = "## OQ-2 x\n**Status:** disposed — done\n"
    _, p = scan(missing)
    ok += 1 if any("without a **Disposition:**" in x for x in p) else 0
    badkind = "## OQ-3 x\n**Status:** disposed — d\n**Disposition:** abandoned\n"
    _, p = scan(badkind)
    ok += 1 if any("unknown Disposition kind" in x for x in p) else 0
    wrongstatus = "## OQ-4 x\n**Status:** open\n**Disposition:** retracted\n"
    _, p = scan(wrongstatus)
    ok += 1 if any("disposed entries only" in x for x in p) else 0
    exempt = "## OQ-59 x\n**Status:** disposed — straddles\n"
    _, p = scan(exempt)
    ok += 1 if not p else 0
    return ok


def main():
    args = sys.argv[1:]
    if "--check" in args:
        st = selftest()
        if st != 5:
            print(f"SELFTEST RED ({st}/5 controls) — live result not trustworthy",
                  file=sys.stderr)
            sys.exit(1)
        entries, problems = scan()
        for p in problems:
            print(f"MALFORMED: {p}", file=sys.stderr)
        print(f"{len(entries)} parsed, {len(problems)} malformed")
        sys.exit(1 if problems else 0)
    entries, problems = scan()
    want = next((a for a in args if a in TOKENS), None)
    counts = {}
    for oq, tok in entries:
        counts[tok] = counts.get(tok, 0) + 1
        if want is None or tok == want:
            print(f"{oq}\t{tok}")
    print("---", " ".join(f"{t}:{n}" for t, n in sorted(counts.items())),
          f"total:{len(entries)}", file=sys.stderr)
    if problems:
        for p in problems:
            print(f"MALFORMED: {p}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
