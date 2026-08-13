#!/usr/bin/env python3
"""known_state_status.py — parse and check the KNOWN_STATE.md entry grammar.

Canonical grammar (sibling of issues_status.py; see ISSUES.md footer for that one).
Each entry is:
    ## YYYY-MM-DD — <title>
    **Files:** <comma-separated paths the entry concerns>
    **Tier:** <token>
    token ∈ {tripwire, correction-key, landed, history}

The Files: line makes the "read KNOWN_STATE.md before touching file X" contract
queryable: instead of reading the whole changelog, enumerate the entries whose
Files: line names the file you are about to touch.

Tiers: tripwire = standing do-not / silent-mistake warning; correction-key =
corrects prior claims or qualifies how results may be cited; landed = change or
audit shipped and witnessed; history = narrative/archival (roll-off candidate).

Usage:
    python3 python/known_state_status.py                # table: date, tier, title
    python3 python/known_state_status.py --check        # exit 1 on malformed entry
    python3 python/known_state_status.py tripwire       # filter by tier token
    python3 python/known_state_status.py --file <path>  # entries whose Files: mentions <path>
"""
import re
import sys
from pathlib import Path

TIERS = {"tripwire", "correction-key", "landed", "history"}
KNOWN_STATE = Path(__file__).resolve().parents[1] / "KNOWN_STATE.md"

HEADER = re.compile(r"^## (\d{4}-\d{2}-\d{2}) — (.+)$")
FILES = re.compile(r"^\*\*Files:\*\* (.+)$")
TIER = re.compile(r"^\*\*Tier:\*\* (\S+)$")


def scan():
    """Return (entries, problems). Each entry: dict(date, title, files, tier, lineno)."""
    entries, problems = [], []
    cur = None
    for lineno, line in enumerate(KNOWN_STATE.read_text().splitlines(), 1):
        m = HEADER.match(line)
        if m:
            if cur is not None:
                _close(cur, problems)
                entries.append(cur)
            cur = {"date": m.group(1), "title": m.group(2), "files": None,
                   "tier": None, "lineno": lineno}
            continue
        if cur is None:
            continue
        fm = FILES.match(line)
        if fm and cur["files"] is None:
            cur["files"] = [f.strip() for f in fm.group(1).split(",")]
            continue
        tm = TIER.match(line)
        if tm and cur["tier"] is None:
            tok = tm.group(1)
            if tok not in TIERS:
                problems.append(f"{cur['date']} (line {lineno}): unknown tier {tok!r}")
            cur["tier"] = tok
    if cur is not None:
        _close(cur, problems)
        entries.append(cur)
    return entries, problems


def entries_for_file(entries, target):
    """Entries whose **Files:** line names `target`. THE canonical match rule.

    Substring in both directions, so an absolute path from a tool payload
    ('/repo/prolog/config.pl') matches a repo-relative Files: entry
    ('prolog/config.pl') and vice versa. Deliberately loose: over-delivery on
    this channel is noise, under-delivery is a missed tripwire. Any consumer
    (--file, the PreToolUse hook) calls THIS — a second copy of the predicate
    would be a silent fork of the matching rule (Build Discipline Pattern 2).
    """
    return [e for e in entries if e["files"]
            and any(target in f or f in target for f in e["files"])]


def _close(entry, problems):
    where = f"{entry['date']} \"{entry['title'][:50]}\" (line {entry['lineno']})"
    if entry["files"] is None:
        problems.append(f"{where}: no **Files:** line")
    if entry["tier"] is None:
        problems.append(f"{where}: no **Tier:** line")


def main():
    entries, problems = scan()
    args = sys.argv[1:]
    if "--check" in args:
        for p in problems:
            print(f"MALFORMED: {p}", file=sys.stderr)
        print(f"{len(entries)} entries parsed, {len(problems)} problems")
        sys.exit(1 if problems else 0)
    if "--file" in args:
        try:
            target = args[args.index("--file") + 1]
        except IndexError:
            print("usage: known_state_status.py --file <path>", file=sys.stderr)
            sys.exit(2)
        hits = entries_for_file(entries, target)
        for e in hits:
            print(f"line {e['lineno']}\t{e['date']}\t{e['tier']}\t{e['title']}")
        print(f"--- {len(hits)} entries mention {target!r}", file=sys.stderr)
        sys.exit(0)
    want = next((a for a in args if a in TIERS), None)
    counts = {}
    for e in entries:
        counts[e["tier"]] = counts.get(e["tier"], 0) + 1
        if want is None or e["tier"] == want:
            print(f"{e['date']}\t{e['tier']}\t{e['title']}")
    print("---", " ".join(f"{t}:{n}" for t, n in sorted(counts.items(), key=lambda kv: str(kv[0]))),
          f"total:{len(entries)}", file=sys.stderr)
    if problems:
        for p in problems:
            print(f"MALFORMED: {p}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
