#!/usr/bin/env python3
"""mode_adjudication.py — the direct question, per row: does the LAST argument carry an answer out?

THE REPLACEMENT LICENCE (operator ruling, 2026-08-19). "Mechanical per a witnessed template"
was falsified because its unstated premise — every `latent-B` row's last argument is an OUTPUT —
is false at 2 rows, unadjudicated at 21, and UNEXAMINED at the rest. Absence of the cut-first
tell is not presence of an output. So the licence is rebuilt on the direct question, asked of
every remaining row rather than of a screen-defined subset.

AND THE ANSWER WAS ALREADY IN THE SOURCE. Both misfiled rows carry an authored mode line
directly above the predicate:

    %% seat_overrides(+C, +Signature)
    %% expected_power_divergence(+P1, +P2, +T1, +T2)

Last argument `+`. INPUT. The fact the registry was supposed to encode was written by the
author, sitting three lines above the clauses, and nothing read it — which is the whole reason
`dispatch_head_check.pl:9-11` could state the last-arg-is-output assumption in a header while
two violations sat inside its own worklist. This checker reads it.

  -Last / ?Last  -> OUTPUT-bearing; the class-B template applies
  +Last          -> INPUT-keyed; the template is INVALID (registry class `input-key`)
  no mode line   -> UNDECLARED; owed a per-row read, and the absence is itself a finding

CROSS-CHECK, not clearance (the two-instrument discipline the partition used). This is run
against `inputkey_screen.py`'s cut-first tell and DISAGREEMENT IN EITHER DIRECTION IS THE
FINDING: a `+Last` the screen missed is a false negative in the screen; a screen hit with an
authored `-Last` is the screen over-flagging (characterize_family/2 selects on argument 1).
Neither instrument clears a row on its own.

CONTROLS (naturally arising, asserted before any table prints):
  FIRES    seat_overrides/2 and expected_power_divergence/4 read `+` on the last argument —
           the two the corpus attributed real changes to.
  DECLINES signature_grade/2 reads `-Grade` — converted 2026-08-19, six-leg pair zero-diff
           over 5,311 constraints.
"""
from __future__ import annotations

import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
PROLOG = REPO / "prolog"
sys.path.insert(0, str(REPO / "python"))
from dispatch_head_check import DECLARED  # noqa: E402

MUST_BE_INPUT = {("abductive_helpers.pl", "seat_overrides/2"),
                 ("boltzmann_compliance.pl", "expected_power_divergence/4")}
MUST_BE_OUTPUT = {("signature_detection.pl", "signature_grade/2")}


def split_args(s: str) -> list[str]:
    args, depth, cur = [], 0, []
    for c in s:
        if c in "([{":
            depth += 1
        elif c in ")]}":
            depth -= 1
        if c == "," and depth == 0:
            args.append("".join(cur)); cur = []; continue
        cur.append(c)
    args.append("".join(cur))
    return [a.strip() for a in args]


def mode_of(fname: str, name: str, arity: int) -> tuple[str | None, str | None]:
    """(last-arg mode, the authored line) from a `%% name(...)` doc comment.

    The argument list is found by BALANCED PAREN SCAN, not by anchoring the regex to
    end-of-line. The first version required the line to end at the closing paren, so
    `%% stance_member_provenance(+C, +Stance, -Prov)  — morphology_suggested | hand_declared.`
    read as UNDECLARED — an authored fact discarded because of trailing prose, which would
    have inflated the "owed a per-row read" set with rows whose answer was already written.
    """
    head = re.compile(r"^%%+\s*" + re.escape(name) + r"\s*\(")
    for raw in (PROLOG / fname).read_text().splitlines():
        line = raw.strip()
        m = head.match(line)
        if not m:
            continue
        depth, start, end = 0, m.end() - 1, None
        for i in range(start, len(line)):
            if line[i] in "([{":
                depth += 1
            elif line[i] in ")]}":
                depth -= 1
                if depth == 0:
                    end = i
                    break
        if end is None:
            continue
        args = split_args(line[start + 1:end])
        if len(args) != arity:
            continue
        last = args[-1]
        if last[:1] in "+-?":
            return last[0], line
        return "none", line
    return None, None


def main() -> int:
    latentb = sorted(k for k, v in DECLARED.items() if v == "latent-B")
    try:
        screen = json.loads((HERE / "inputkey_screen.json").read_text())
        flagged = {(r["file"], r["pi"]) for r in screen["flagged"]}
    except FileNotFoundError:
        raise SystemExit("mode_adjudication: RED — run inputkey_screen.py first; this checker "
                         "is a CROSS-CHECK and reports nothing on its own")

    rows = []
    for f, pi in latentb + sorted(MUST_BE_OUTPUT | MUST_BE_INPUT):
        name, ar = pi.rsplit("/", 1)
        mode, line = mode_of(f, name, int(ar))
        rows.append({"file": f, "pi": pi, "last_mode": mode, "authored_line": line,
                     "screen_flagged": (f, pi) in flagged})

    by_key = {(r["file"], r["pi"]): r for r in rows}
    problems = []
    for key in MUST_BE_INPUT:
        r = by_key.get(key)
        if not r or r["last_mode"] != "+":
            problems.append(f"CONTROL {key[1]}: expected an authored `+` last argument, got "
                            f"{r and r['last_mode']!r} — this checker is not reading the fact "
                            f"that distinguishes the two rows the corpus attributed")
    for key in MUST_BE_OUTPUT:
        r = by_key.get(key)
        if not r or r["last_mode"] != "-":
            problems.append(f"CONTROL {key[1]}: expected an authored `-` last argument, got "
                            f"{r and r['last_mode']!r} — it converted with a zero-diff six-leg "
                            f"pair, so a checker calling it input-keyed cannot separate the class")
    if problems:
        for p in problems:
            print(f"  {p}")
        print("mode_adjudication: RED — discrimination controls failed")
        return 1

    live = [r for r in rows if (r["file"], r["pi"]) in set(latentb)]
    out = [r for r in live if r["last_mode"] in ("-", "?")]
    inp = [r for r in live if r["last_mode"] == "+"]
    undeclared = [r for r in live if r["last_mode"] in (None, "none")]

    disagree_screen_only = [r for r in live if r["screen_flagged"] and r["last_mode"] in ("-", "?")]
    disagree_mode_only = [r for r in live if not r["screen_flagged"] and r["last_mode"] == "+"]

    (HERE / "mode_adjudication.json").write_text(json.dumps(rows, indent=1))
    print(f"mode_adjudication: controls OK "
          f"(seat_overrides/2 +, expected_power_divergence/4 +, signature_grade/2 -)")
    print(f"  {len(live)} latent-B rows: {len(out)} authored OUTPUT (-/?), "
          f"{len(inp)} authored INPUT (+), {len(undeclared)} UNDECLARED")
    if inp:
        print(f"\n  AUTHORED INPUT — template invalid, must be reclassified `input-key`:")
        for r in inp:
            print(f"    {r['file']:32} {r['pi']:34} {r['authored_line']}")
    if undeclared:
        print(f"\n  UNDECLARED — no mode line; owed a per-row read (the absence is the finding):")
        for r in undeclared:
            print(f"    {r['file']:32} {r['pi']:34} screen_flagged={r['screen_flagged']}")
    print(f"\n  CROSS-CHECK vs the cut-first screen (disagreement either way is the finding):")
    print(f"    screen flags but author says OUTPUT (screen over-flags): {len(disagree_screen_only)}")
    for r in disagree_screen_only:
        print(f"      {r['file']:30} {r['pi']:32} {r['authored_line']}")
    print(f"    author says INPUT but screen missed it (screen false negative): "
          f"{len(disagree_mode_only)}")
    for r in disagree_mode_only:
        print(f"      {r['file']:30} {r['pi']:32} {r['authored_line']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
