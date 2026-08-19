#!/usr/bin/env python3
"""sunset_check.py — dated obligations turn the gate RED on their day (OQ-317 ruling, 2026-08-19).

The rule this mechanizes (build_discipline.md → "A passive trigger never fires"): a DATED
obligation gets gate enforcement; an undated one gets a named read-site. Before this checker,
REVIEW-BY dates were prose — "when someone remembers to look" was the failure mode.

Two surfaces, deliberately independent so neither obligation dies with the other's carrier
(operator amendment, 2026-08-19: the OQ-303 conversion question lives in the allowlist row and
the socket disposition lives in ISSUES OQ-317; if the row is removed in October, the OQ's own
Sunset line still fires in November):

  1. prolog/codewalk_caller_allowlist.txt — any `REVIEW-BY YYYY-MM-DD` token.
  2. ISSUES.md — any `**Sunset:** YYYY-MM-DD` line, attributed to the nearest preceding
     `## OQ-NN` header; counted only while that entry's status token is ACTIVE
     (open/investigating/mitigated/partial). A resolved/disposed/future entry's Sunset is
     inert history, not an obligation.

An obligation is DUE when today >= its date (it fires ON the day — a sunset that fires a day
late fires on a disposition that has already renewed itself). A malformed date is RED, not
skipped: an unparseable obligation is an absence presenting as a presence.

LICENSED RESPONSES to a red (operator ruling 2026-08-19, recorded at ISSUES OQ-317): conduct
the review, OR extend the date — but an extension is an OPERATOR RULING recorded in the owning
ISSUES entry, never a silent edit to the date token. The gate's job is to summon; the entry's
job is to hold the argument.

Selftest (rides --check): planted past date FIRES; planted same-day date FIRES (the boundary —
off-by-one on a date comparison is the classic defect); planted future date DECLINES; planted
malformed date FIRES; inactive-entry Sunset DECLINES.
"""
from __future__ import annotations

import datetime
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ALLOWLIST = ROOT / "prolog" / "codewalk_caller_allowlist.txt"
ISSUES = ROOT / "ISSUES.md"

REVIEW_RE = re.compile(r"REVIEW-BY\s+(\S+)")
SUNSET_RE = re.compile(r"^\*\*Sunset:\*\*\s+(\S+)")
HEADER_RE = re.compile(r"^## (OQ-\d+)")
STATUS_RE = re.compile(r"^\*\*Status:\*\*\s+(\w+)")
DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
ACTIVE = {"open", "investigating", "mitigated", "partial"}

MSG = ("extension requires an operator ruling recorded in the owning ISSUES entry "
       "(OQ-317 ruling, 2026-08-19) — never a silent edit to the date token")


def scan_allowlist(text: str, today: datetime.date):
    """Return (tracked, due, malformed) for REVIEW-BY tokens."""
    tracked, due, malformed = [], [], []
    for lineno, line in enumerate(text.splitlines(), 1):
        for m in REVIEW_RE.finditer(line):
            tok = m.group(1).rstrip(",;:.")
            item = f"allowlist:{lineno} REVIEW-BY {tok}"
            if not DATE_RE.match(tok):
                malformed.append(item)
                continue
            tracked.append(item)
            if datetime.date.fromisoformat(tok) <= today:
                due.append(item)
    return tracked, due, malformed


def scan_issues(text: str, today: datetime.date):
    """Return (tracked, due, malformed) for **Sunset:** lines on ACTIVE entries."""
    tracked, due, malformed = [], [], []
    header, status = None, None
    for line in text.splitlines():
        h = HEADER_RE.match(line)
        if h:
            header, status = h.group(1), None
            continue
        s = STATUS_RE.match(line)
        if s and status is None:
            status = s.group(1)
            continue
        m = SUNSET_RE.match(line)
        if m:
            tok = m.group(1).rstrip(",;:.")
            item = f"{header or 'NO-HEADER'} Sunset {tok}"
            if not DATE_RE.match(tok):
                malformed.append(item)
                continue
            if header is None or status is None:
                malformed.append(item + " (no attributable active entry)")
                continue
            if status not in ACTIVE:
                continue  # inert history on a closed entry
            tracked.append(item)
            if datetime.date.fromisoformat(tok) <= today:
                due.append(item)
    return tracked, due, malformed


def selftest() -> int:
    today = datetime.date(2026, 11, 17)
    ok = 0
    # 1. past date fires
    _, due, _ = scan_allowlist("x REVIEW-BY 2026-11-16 y", today)
    ok += 1 if due else 0
    # 2. same-day date fires (the boundary)
    _, due, _ = scan_allowlist("x REVIEW-BY 2026-11-17 y", today)
    ok += 1 if due else 0
    # 3. future date declines
    _, due, _ = scan_allowlist("x REVIEW-BY 2026-11-18 y", today)
    ok += 1 if not due else 0
    # 4. malformed date fires loud
    _, _, mal = scan_allowlist("x REVIEW-BY tomorrow y", today)
    ok += 1 if mal else 0
    # 5. Sunset on an ACTIVE entry fires; on a resolved entry declines
    active = "## OQ-1\n**Status:** open\n**Sunset:** 2026-11-01\n"
    closed = "## OQ-2\n**Status:** resolved — done\n**Sunset:** 2026-11-01\n"
    _, due_a, _ = scan_issues(active, today)
    _, due_c, _ = scan_issues(closed, today)
    ok += 1 if (due_a and not due_c) else 0
    return ok


def main() -> int:
    st = selftest()
    if st != 5:
        print(f"sunset check: SELFTEST RED ({st}/5 controls)")
        return 1
    today = datetime.date.today()
    a_t, a_d, a_m = scan_allowlist(ALLOWLIST.read_text(encoding="utf-8"), today)
    i_t, i_d, i_m = scan_issues(ISSUES.read_text(encoding="utf-8"), today)
    problems = a_d + i_d + a_m + i_m
    if problems:
        for p in a_m + i_m:
            print(f"MALFORMED: {p}")
        for p in a_d + i_d:
            print(f"DUE: {p} — review due; {MSG}")
        print(f"sunset check: RED — {len(a_d + i_d)} due, {len(a_m + i_m)} malformed")
        return 1
    print(f"sunset check: GREEN — {len(a_t)} allowlist REVIEW-BY + {len(i_t)} active ISSUES "
          f"Sunset tracked, 0 due; selftest 5/5")
    return 0


if __name__ == "__main__":
    sys.exit(main())
