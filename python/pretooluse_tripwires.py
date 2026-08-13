#!/usr/bin/env python3
"""pretooluse_tripwires.py — deliver per-file KNOWN_STATE tripwires at edit time.

Wired as a PreToolUse hook on Edit|Write (`.claude/settings.json`). Reads the
hook payload on stdin, and injects the KNOWN_STATE.md entries whose **Files:**
line names the file about to be edited, via
`hookSpecificOutput.additionalContext`.

WHY A HOOK AND NOT A DOC LINE. `known_state_status.py --file <path>` has existed
since the Files:/Tier: grammar landed, and its only consumers were
`scripts/gate.sh` and `audit_citation_status.py` — so a worker had to REMEMBER
to run it before editing, which is the one thing a non-persistent worker cannot
be relied upon to do. The query was right; its delivery time was wrong. This
moves it to the moment of the action. Producer with a human habit for a
consumer is Build Discipline Pattern 1 wearing ordinary clothes.

RUNG DISCIPLINE. Only `tripwire` and `correction-key` are delivered — the two
tiers that change what an editor would do. `landed` and `history` are narrative
and stay in KNOWN_STATE.md, read on demand. Widening the tier set turns a
tripwire channel into a changelog and it will be ignored accordingly.

SILENCE MEANS EXACTLY ONE THING. An empty injection means "the query ran and
matched nothing." A query that could not run emits a loud DELIVERY FAILED
context instead — never silence. A broken instrument that emits nothing is
byte-identical at the read site to a working one that found nothing (Build
Discipline Pattern 6), and a context channel is precisely where that collapse
would never be noticed: nobody sees the injection that did not happen.

MATCHING is not implemented here. It calls
`known_state_status.entries_for_file`, the canonical predicate that `--file`
also uses; a second copy would be a silent fork of the matching rule (Pattern 2).

Controls: `python3 python/pretooluse_tripwires.py --selftest` — four cases,
two-sided, including a decline that comes from the tier filter rather than from
a path miss, and the failure path. Wired into `scripts/gate.sh`.
"""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

DELIVER_TIERS = ("tripwire", "correction-key")

# Cap on delivered entries. Overflow is REPORTED in the injected text, never
# silently dropped — a truncated list that does not say it was truncated reads
# as a complete one.
MAX_ENTRIES = 8


def collect(target, scanner=None):
    """Return (hits, unfiltered_count, failure).

    `failure` is None on success, else a reason string. `unfiltered_count` is
    how many entries matched the path BEFORE the tier filter — it is what lets
    a caller (and the selftest) tell "no entry names this file" apart from
    "entries name it but none is a tripwire."

    `scanner` is the injection point for the failure control: a callable
    (target) -> list[entry].
    """
    try:
        if scanner is None:
            import known_state_status as kss
            entries, _problems = kss.scan()
            matched = kss.entries_for_file(entries, target)
        else:
            matched = scanner(target)
        hits = [e for e in matched if e.get("tier") in DELIVER_TIERS]
        return hits, len(matched), None
    except Exception as exc:  # noqa: BLE001 — any failure must surface, not vanish
        return [], 0, f"{type(exc).__name__}: {exc}"


def render(target, hits, failure):
    """Injected text, or None for 'queried, nothing to say'."""
    if failure:
        return (
            f"KNOWN_STATE tripwire delivery FAILED for {target} — {failure}\n"
            "This is NOT a report that the file has no tripwires; the query did "
            "not run. Before editing, run:\n"
            f"  python3 python/known_state_status.py --file {target}"
        )
    if not hits:
        return None
    shown, dropped = hits[:MAX_ENTRIES], max(0, len(hits) - MAX_ENTRIES)
    lines = [
        f"KNOWN_STATE.md has {len(hits)} standing entr"
        f"{'y' if len(hits) == 1 else 'ies'} for {target}. "
        "Read the ones you have not already accounted for before editing — "
        "these are the silent-mistake warnings, not the changelog."
    ]
    for e in shown:
        lines.append(
            f"  KNOWN_STATE.md:{e['lineno']}  [{e['tier']}]  {e['date']} — {e['title']}"
        )
    if dropped:
        lines.append(
            f"  … {dropped} further entr{'y' if dropped == 1 else 'ies'} NOT shown "
            f"(cap {MAX_ENTRIES}). Full list: "
            f"python3 python/known_state_status.py --file {target}"
        )
    return "\n".join(lines)


def emit(context):
    if context:
        print(json.dumps({
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "additionalContext": context,
            }
        }))


def main():
    raw = sys.stdin.read()
    try:
        payload = json.loads(raw) if raw.strip() else {}
    except json.JSONDecodeError as exc:
        emit(f"KNOWN_STATE tripwire delivery FAILED: unparseable hook payload — {exc}")
        return
    target = (payload.get("tool_input") or {}).get("file_path") or ""
    if not target:
        return  # no file in this call; nothing this hook can speak to
    hits, _n, failure = collect(target)
    emit(render(target, hits, failure))


# ---------------------------------------------------------------- selftest

def _selftest():
    """Four controls. Two fire, two decline, and one of the declines is caused
    by the tier filter rather than by a path miss — a control that can only
    fire is not a control.
    """
    ok = True

    def check(label, cond, detail=""):
        nonlocal ok
        ok = ok and cond
        print(f"  {'PASS' if cond else 'FAIL'}  {label}" + (f"  [{detail}]" if detail else ""))

    print("pretooluse_tripwires selftest")

    # 1. POSITIVE — a file with real tripwire entries must produce context.
    t = "prolog/signature_detection.pl"
    hits, n, fail = collect(t)
    ctx = render(t, hits, fail)
    check("positive: signature_detection.pl delivers", bool(ctx) and fail is None,
          f"{len(hits)} of {n} matched entries pass the tier filter")

    # 2. NEGATIVE (path) — a real file no entry names must produce silence.
    t = "python/cli.py"
    hits, n, fail = collect(t)
    check("decline: cli.py (no entry names it)", render(t, hits, fail) is None and fail is None,
          f"matched={n}")

    # 3. NEGATIVE (rung) — the discriminating one. The path DOES match, and the
    #    tier filter is what declines. Asserting matched>0 is what separates
    #    "the filter worked" from "the path lookup missed", which would look
    #    identical at the output.
    t = "python/spec_enum_check.py"
    hits, n, fail = collect(t)
    check("decline: spec_enum_check.py declined BY THE TIER FILTER, not a path miss",
          render(t, hits, fail) is None and n > 0 and fail is None,
          f"matched={n}, tier-passing={len(hits)}")

    # 4. FAILURE — a query that cannot run must be LOUD, never silent. This is
    #    the control that makes silence in cases 2 and 3 mean something.
    def _boom(_target):
        raise RuntimeError("simulated KNOWN_STATE read failure")

    t = "prolog/signature_detection.pl"
    hits, n, fail = collect(t, scanner=_boom)
    ctx = render(t, hits, fail)
    check("failure is loud: unreadable KNOWN_STATE emits DELIVERY FAILED, not silence",
          ctx is not None and "FAILED" in ctx and fail is not None)

    # 5. Shape — what is emitted must be the JSON the harness actually reads.
    import io
    import contextlib
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        emit("x")
    try:
        shape = json.loads(buf.getvalue())
        good = shape["hookSpecificOutput"]["hookEventName"] == "PreToolUse" and \
            shape["hookSpecificOutput"]["additionalContext"] == "x"
    except Exception:
        good = False
    check("emitted payload is valid PreToolUse hook JSON", good)

    # 6. NORMALIZATION — `Files:` tokens must carry no markdown backticks after
    #    scan. A backticked token still matches a RELATIVE target (the plain name
    #    is a substring of the ticked one), so `--file CLAUDE.md` looked fine
    #    while the hook — which passes an ABSOLUTE path — matched nothing. The
    #    style entered ~2026-08-10 and silenced every entry after it; two files
    #    went from 0 delivered entries to non-zero when it was fixed. Red-capable:
    #    the same assertion is run against a tick-retaining parse and must FAIL
    #    there, or it is a check that cannot fail.
    import known_state_status as kss
    entries, _ = kss.scan()
    live_ticks = sum(1 for e in entries for f in (e["files"] or []) if "`" in f)
    seeded = [{"files": ["`python/x.py`", "plain/y.py"]}]
    seeded_ticks = sum(1 for e in seeded for f in e["files"] if "`" in f)
    check("Files: tokens carry no backticks after scan (absolute-path blindness)",
          live_ticks == 0 and seeded_ticks > 0,
          f"live={live_ticks}, red-capable control detects {seeded_ticks}")

    print("SELFTEST", "GREEN" if ok else "RED")
    return 0 if ok else 1


if __name__ == "__main__":
    if "--selftest" in sys.argv:
        sys.exit(_selftest())
    main()
