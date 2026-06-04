#!/usr/bin/env python3
"""issues_status.py — parse and check ISSUES.md status grammar.

Canonical grammar (one line per OQ, first Status line of the section):
    **Status:** <token>[ — <free-text detail>]
    token ∈ {open, investigating, mitigated, partial, resolved, disposed}

Usage:
    python3 python/issues_status.py            # table + counts
    python3 python/issues_status.py --check    # exit 1 on any malformed entry
    python3 python/issues_status.py open       # list only that token

The checker is the grammar's consumer: if a new entry deviates (old `**Status: x**`
form, missing line, unknown token), --check fails loudly instead of the deviation
silently breaking the next census. See KNOWN_STATE.md 2026-06-04 ledger-sweep entry.
"""
import re
import sys
from pathlib import Path

TOKENS = {"open", "investigating", "mitigated", "partial", "resolved", "disposed"}
ISSUES = Path(__file__).resolve().parents[1] / "ISSUES.md"

HEADER = re.compile(r"^## (OQ-\d+) ")
CANON = re.compile(r"^\*\*Status:\*\* (\w+)(?: — .*)?$")
ANY_STATUS = re.compile(r"^\*\*Status\b")


def scan():
    entries, problems = [], []
    cur, seen = None, set()
    for lineno, line in enumerate(ISSUES.read_text().splitlines(), 1):
        m = HEADER.match(line)
        if m:
            if cur and cur not in seen:
                problems.append(f"{cur}: no Status line found")
            cur = m.group(1)
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
    if cur and cur not in seen:
        problems.append(f"{cur}: no Status line found")
    return entries, problems


def main():
    entries, problems = scan()
    args = sys.argv[1:]
    if "--check" in args:
        for p in problems:
            print(f"MALFORMED: {p}", file=sys.stderr)
        print(f"{len(entries)} parsed, {len(problems)} malformed")
        sys.exit(1 if problems else 0)
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
