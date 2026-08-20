#!/usr/bin/env python3
"""amnesiac_carriage_check.py — an INVARIANT-asserting carriage check for the amnesiac paper.

Operator ruling, 2026-08-18. The 2026-08-18 discharge pass closed with a verification block
built from pattern greps, and it mis-fired twice: once reporting a present phrase absent (the
document is hard-wrapped AND blockquoted, so a flattened probe still missed), once reporting a
withdrawn number as an asserted one. Both are VALUE CHECKS, and §7.4's restated property says
exactly why they missed: *a gate catches this class only when it asserts a structural invariant
rather than checking a value.* A probe that can return "absent" for a present phrase cannot
distinguish "the sweep worked" from "the probe is broken", which is the two-sided requirement
§7.3 imposes on every other instrument in the programme.

So this checker does not grep for staleness. It asserts, per enumerated site, **the expected
number of hits**, and fails on a MISS as loudly as on an EXTRA. A normalisation bug now turns
the check RED instead of green, because the expected count stops being met.

That is also §5.1's P8 rider applied to itself: the expected value is published beside the
command, so running it without comparing is impossible rather than merely discouraged.

Editing the paper's carriage sites without updating EXPECTATIONS turns this red, deliberately —
the same opt-in-with-teeth shape as `spec_enum_check.py`.

Exit 0 = every expectation met. Exit 1 = a carriage invariant is violated, or this checker's
own selftest failed.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PAPER = ROOT / "docs/amnesiac_institution/amnesiac_institution_v0_6.md"

# --- normalisation ---------------------------------------------------------------
# Two traps, both witnessed on this document on 2026-08-18:
#   (1) it is HARD-WRAPPED, so a line-oriented probe misses phrases crossing a newline;
#   (2) phrases sit inside BLOCKQUOTES, so continuation lines begin "> " and a
#       whitespace-flatten leaves those markers embedded mid-phrase.
# Strip blockquote markers FIRST, then flatten. The selftest plants both traps.

def normalise(text: str) -> str:
    text = re.sub(r"(?m)^[ \t]*>[ \t]?", "", text)
    return re.sub(r"\s+", " ", text)


def plant(raw: str, phrase: str, replacement: str) -> str:
    """Replace *phrase* in RAW text even though the document hard-wraps and blockquotes it.

    The first selftest written for this file planted its fixtures with a plain
    `str.replace` and every plant SILENTLY NO-OPPED, because the target phrases are
    wrapped across lines with `> ` markers in between — so `raw.replace(...)` matched
    nothing, the document came back unchanged, and the "did the check go red?" assertions
    passed a document that had never been damaged. A selftest whose plants do not land is
    the exact defect this checker exists to catch, committed inside the checker's own
    control. It is fixed here and kept in the comment, because the fix is the finding:
    **a control must be shown to change the thing it perturbs.**
    """
    pat = re.compile(r"\s*>?\s*".join(map(re.escape, phrase.split())))
    out, n = pat.subn(replacement, raw, count=1)
    if n != 1:
        raise AssertionError(f"plant did not land: {phrase!r}")
    return out


def section(text: str, start: str, end: str) -> str:
    i = text.index(start)
    j = text.index(end, i + len(start))
    return text[i:j]


# --- the expectation manifest ----------------------------------------------------
# (label, what it protects, callable(raw) -> actual, expected)
#
# THE POOLED SCALAR IS WITHDRAWN (§5.4, 2026-08-18). It may appear ONLY where the
# surrounding clause withdraws it or records its history. It may not be asserted at any
# carriage site the document itself enumerates (§0 W2, Appendix B, §2.A).

POOLED = re.compile(r"\b(?:73\s*/\s*174|83\s*/\s*185|4[245](?:\.\d)?%)")
WINDOW = re.compile(r"\b(?:36\.7|37|57\.8|58|80(?:\.0)?)\s*%")


def w2_row(raw: str) -> str:
    return next(l for l in raw.splitlines() if l.startswith("| W2 |"))


def _count(pat, s):
    return len(pat.findall(s))


EXPECTATIONS = [
    # --- the three enumerated carriage sites ------------------------------------
    ("W2 window rates",
     "§0's W2 row reports per-window rates",
     lambda raw: _count(WINDOW, w2_row(raw)), 3),
    ("W2 withdrawal clause",
     "§0's W2 row names the withdrawn pooled scalar exactly once, as withdrawn",
     lambda raw: normalise(w2_row(raw)).count("is **withdrawn**"), 1),
    ("2.A no pooled scalar",
     "§2.A's worked example asserts no pooled scalar",
     lambda raw: _count(POOLED, section(raw, "### 2.A ", "### 2.B ")), 0),
    ("abstract no pooled scalar",
     "the ABSTRACT asserts no pooled scalar",
     lambda raw: _count(POOLED, section(raw, "## ABSTRACT", "## 0. ")), 0),

    # --- §5.4's lead: the per-window table IS the headline ----------------------
    ("5.4 window table rows",
     "§5.4 leads with a three-window table",
     lambda raw: len(re.findall(r"(?m)^\| 2026-0[678][^|]*\|", raw)), 3),
    ("5.4 pooled row marked",
     "§5.4's table marks the pooled row not-a-reportable-summary",
     lambda raw: normalise(raw).count("not a reportable summary"), 1),
    ("5.4 correction count",
     "§5.4 states the restated correction count of six",
     lambda raw: normalise(raw).count("The correct count is six"), 1),

    # --- §7.4: eleven instances, exactly one caught by a gate -------------------
    ("7.4 numbered rows",
     "§7.4's recursion table enumerates eleven instances — ANY integer row counts, so a "
     "twelfth is caught rather than ignored by a too-narrow alternation",
     lambda raw: len(re.findall(r"(?m)^\| \d+ \| ",
                                section(raw, "### 7.4 The recursion", "#### 7.4.1 "))), 12),
    ("7.4 gate catch",
     "exactly one §7.4 instance names a gate as its catcher",
     lambda raw: len(re.findall(r"(?m)^\| 11 \| .*partition_check", raw)), 1),
    ("7.4 property restated",
     "§7.4 states the invariant-vs-value property, not the withdrawn zero-catch streak",
     lambda raw: normalise(raw).count("asserts a structural invariant rather than checking a value"), 2),
    ("W3 no zero-catch claim",
     "§0's W3 row no longer claims none was caught by a gate",
     lambda raw: normalise(w3_row(raw)).count("none caught by a gate"), 0),

    # --- Appendix D.3: the 73/174 row is no longer listed as settled ------------
    ("D.3 unsettled",
     "Appendix D.3 records that its 73/174 line was wrong to call the matter settled",
     lambda raw: normalise(section(raw, "### D.3 ", "### D.4 "))
                 .count("was itself wrong to call the matter settled"), 1),

    # --- the trial is reported unpooled -----------------------------------------
    ("8.2 no pooled ratio",
     "§8.2 states no ratio for the promotion-test trial",
     lambda raw: normalise(raw).count("1 of 2 draws") + normalise(raw).count("0 of 2 draws"), 0),
    ("8.2 registered framing",
     "§8.2 itself reports one registered draw per arm (the phrase also appears in "
     "Appendix B and the amendment block; this assertion is scoped to §8.2)",
     lambda raw: normalise(section(raw, "### 8.2 The promotion economy", "### 8.3 "))
                 .count("One registered draw per arm"), 1),
    ("8.2 framing carried",
     "the same framing is carried at every site that reports the trial, so a reader "
     "cannot meet the result without it",
     lambda raw: normalise(raw).count("One registered draw per arm"), 3),
]


def w3_row(raw: str) -> str:
    return next(l for l in raw.splitlines() if l.startswith("| W3 |"))


def run(raw: str, label_filter=None):
    rows, failures = [], []
    for label, why, fn, expected in EXPECTATIONS:
        if label_filter and label != label_filter:
            continue
        try:
            actual = fn(raw)
        except Exception as e:                       # a site that vanished is a FAILURE,
            actual, e_note = None, f"{type(e).__name__}: {e}"   # never a silent skip
            rows.append((label, expected, "ERROR", why, e_note))
            failures.append(f"{label}: site not found — {e_note}")
            continue
        ok = actual == expected
        rows.append((label, expected, actual, why, ""))
        if not ok:
            failures.append(f"{label}: expected {expected}, found {actual} — {why}")
    return rows, failures


def selftest() -> list[str]:
    """Plant each trap this checker exists to survive; every one must turn it RED."""
    raw = PAPER.read_text(encoding="utf-8")
    problems = []
    # every plant below goes through plant(), which RAISES if it does not land — so a
    # fixture that silently fails to perturb the document can no longer be mistaken for
    # a control that fired.

    # NOTE (2026-08-18): this selftest used to open with "the clean document must be
    # green". That assertion is redundant with main()'s own run AND it was actively
    # harmful: when a real carriage invariant broke, the selftest failed first and
    # main() printed "selftest failed: clean document is not green", swallowing the one
    # thing the operator needed — WHICH invariant. A check that reports its own
    # machinery instead of its finding is a value check wearing an invariant's clothes.
    # Document failures are now reported by main() before selftest results.

    # (2) the hard-wrap trap: break a phrase across a newline. A line-oriented probe
    #     would report it absent; this checker must still find it (count unchanged).
    wrapped = plant(raw, "The correct count is six", "The correct\n> count is\n> six")
    _, f = run(wrapped, "5.4 correction count")
    if f:
        problems.append("selftest: hard-wrap + blockquote trap defeats the normaliser")

    # (3) a real deletion must FAIL — the check must not pass on absence
    deleted = plant(raw, "The correct count is six", "The correct count is three")
    _, f = run(deleted, "5.4 correction count")
    if not f:
        problems.append("selftest: a deleted expectation did not turn the check red")

    # (4) undeclared carriage: re-assert a pooled scalar in §2.A
    recarried = plant(raw, "### 2.B ", "An incidence figure of 42% travels here.\n\n### 2.B ")
    _, f = run(recarried, "2.A no pooled scalar")
    if not f:
        problems.append("selftest: a re-asserted pooled scalar was not caught")

    # (5) a vanished SITE must fail, not skip
    gone = plant(raw, "| W2 |", "| W2X |")
    _, f = run(gone, "W2 window rates")
    if not f:
        problems.append("selftest: a vanished site was silently skipped instead of failing")

    # (6) two-sided on the row counts: adding a §7.4 row must fail
    extra = plant(raw, "#### 7.4.1", "| 12 | a planted row | planted | planted |\n\n#### 7.4.1")
    _, f = run(extra, "7.4 numbered rows")
    if not f:
        problems.append("selftest: an extra §7.4 row was not caught")

    return problems


def main() -> int:
    check = "--check" in sys.argv
    raw = PAPER.read_text(encoding="utf-8")
    rows, failures = run(raw)
    st = selftest()

    if not check:
        w = max(len(r[0]) for r in rows)
        for label, expected, actual, why, note in rows:
            mark = "ok " if actual == expected else "FAIL"
            print(f"  {mark} {label:<{w}}  expected={expected:<3} actual={actual}  # {why}{note}")
        print()

    # Document findings FIRST — they are what a reader needs; selftest problems are
    # about the instrument and are reported after, never in place of.
    if failures:
        print(f"amnesiac carriage check: RED — {len(failures)} carriage invariant(s) violated")
        for f in failures:
            print(f"  - {f}")
        if st:
            print(f"  (and {len(st)} selftest problem(s) — the instrument is also suspect)")
            for s2 in st:
                print(f"  - {s2}")
        return 1
    if st:
        print(f"amnesiac carriage check: RED — the DOCUMENT passes but the CHECKER's own "
              f"selftest failed ({len(st)}); its green may not be trusted")
        for s2 in st:
            print(f"  - {s2}")
        return 1
    print(f"amnesiac carriage check: GREEN — {len(rows)} carriage invariants asserted "
          f"(expected counts published in the manifest); selftest 6/6, every plant verified to land")
    return 0


if __name__ == "__main__":
    sys.exit(main())
