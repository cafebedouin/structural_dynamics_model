#!/usr/bin/env python3
"""inputkey_screen.py — which `latent-B` rows are actually INPUT-KEYED, and so unconvertible?

THE DEFECT THIS EXISTS FOR (witnessed 2026-08-19). The class-B template — fresh-variable heads
+ unify-after-cut — is valid only when the last argument is an OUTPUT. `dispatch_head_check.pl`
says so in its header ("OUTPUT ARGUMENT is taken to be the LAST argument, by engine convention.
This is a declared assumption, not a fact about every predicate"), and the registry has a class
for the exception (`input-key`). Two rows were nonetheless filed `latent-B`:

    seat_overrides(C, false_ci_rope) :- !, \\+ signature_detection:fcr_routed(C).
    expected_power_divergence(powerless, institutional, _, _) :- !.

Converted, the first becomes `seat_overrides(C, T) :- !, \\+ fcr_routed(C), T = false_ci_rope.`
— which matches ANY second argument, cuts, and makes every later clause unreachable. The batch
six-leg pair caught it (129 and 17 live constraints moved), and a per-file bisect attributed it
to exactly these two files out of 29.

THE TELL IS MECHANICAL: a clause whose body's FIRST goal is `!`. A clause that commits before
testing anything cannot be computing its head arguments — it is SELECTING on them, so they are
inputs. `seat_overrides/2` cuts before its guard; `expected_power_divergence/4`'s body IS a cut.

DISCRIMINATION RECORD — naturally arising, no plant, asserted in-process below: the screen must
FIRE on exactly the two predicates the corpus attributed, and must DECLINE on `signature_grade/2`
(converted 2026-08-19, six-leg pair zero-diff over 5,311 constraints). Anything else it flags is
the latent version of the same defect — an input-keyed row whose conversion happens not to move
THIS corpus — which is the more valuable half of the catch.
"""
from __future__ import annotations

import json
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
PROLOG = REPO / "prolog"

MUST_FIRE = {("abductive_helpers.pl", "seat_overrides/2"),
             ("boltzmann_compliance.pl", "expected_power_divergence/4")}
MUST_DECLINE = {("signature_detection.pl", "signature_grade/2")}

SHAPE_RE = re.compile(r"^SHAPE (\S+/\d+) (true|false) (true|false)$")


def worklist() -> dict[str, list[str]]:
    """The latent-B rows as of the pre-batch registry, read from git."""
    import ast
    src = subprocess.run(["git", "show", "6c1bfa44:python/dispatch_head_check.py"],
                         cwd=REPO, capture_output=True, text=True, timeout=120).stdout
    tree = ast.parse(src)
    declared = None
    for node in ast.walk(tree):
        if isinstance(node, (ast.AnnAssign, ast.Assign)):
            tgt = node.target if isinstance(node, ast.AnnAssign) else node.targets[0]
            if getattr(tgt, "id", "") == "DECLARED":
                declared = node.value
    by_file: dict[str, list[str]] = {}
    for k, v in zip(declared.keys, declared.values):
        f, pi = (ast.literal_eval(e) for e in k.elts)
        if isinstance(v, ast.Constant) and v.value == "latent-B":
            by_file.setdefault(f, []).append(pi)
    return by_file


def shapes(fname: str) -> list[tuple[str, bool, bool]]:
    goal = (f"scan_file_clause_shapes('{PROLOG / fname}', S), "
            "forall(member(PI-shape(_,C,F), S), "
            "  ( PI = N/A, format('SHAPE ~w/~w ~w ~w~n', [N,A,C,F]) ))")
    proc = subprocess.run(
        ["swipl", "-q", "-l", str(PROLOG / "dispatch_head_check.pl"), "-g", f"{goal}, halt",
         "-t", "halt(1)"], cwd=REPO, capture_output=True, text=True, timeout=300)
    if proc.returncode != 0:
        raise SystemExit(f"inputkey_screen: RED — shape scan failed on {fname}: "
                         f"{proc.stderr[-300:]}")
    out = []
    for ln in proc.stdout.splitlines():
        if (m := SHAPE_RE.match(ln)):
            out.append((m.group(1), m.group(2) == "true", m.group(3) == "true"))
    if not out:
        raise SystemExit(f"inputkey_screen: RED — 0 clauses read from {fname}")
    return out


def main() -> int:
    by_file = worklist()
    flagged, clean = [], []
    for f in sorted(by_file):
        sh = shapes(f)
        for pi in sorted(by_file[f]):
            mine = [s for s in sh if s[0] == pi]
            if not mine:
                raise SystemExit(f"inputkey_screen: RED — {f} {pi} produced no clauses")
            cutfirst = [i for i, s in enumerate(mine, 1) if s[2]]
            (flagged if cutfirst else clean).append(
                {"file": f, "pi": pi, "cut_first_clauses": cutfirst, "n_clauses": len(mine)})

    fset = {(r["file"], r["pi"]) for r in flagged}
    problems = []
    for key in MUST_FIRE:
        if key not in fset:
            problems.append(f"CONTROL: screen did NOT fire on {key[1]}, which the corpus "
                            f"attributed a real change to — the screen is not detecting the "
                            f"mechanism it was built for")
    for key in MUST_DECLINE:
        if key in fset:
            problems.append(f"CONTROL: screen fired on {key[1]}, which converted with a "
                            f"zero-diff six-leg pair over 5,311 constraints — the screen "
                            f"cannot separate the class it is meant to separate")
    if problems:
        for p in problems:
            print(f"  {p}")
        print("inputkey_screen: RED — discrimination controls failed")
        return 1

    (HERE / "inputkey_screen.json").write_text(json.dumps(
        {"flagged": flagged, "clean": [c["pi"] for c in clean]}, indent=1))
    print(f"inputkey_screen: controls OK (fires on the 2 corpus-attributed rows, declines on "
          f"signature_grade/2)")
    print(f"  {len(flagged)} of {len(flagged) + len(clean)} latent-B rows are INPUT-KEYED "
          f"(cut as first body goal) — the template is invalid for these:")
    for r in flagged:
        mark = "  <- corpus-attributed" if (r["file"], r["pi"]) in MUST_FIRE else \
               "  <- LATENT: same defect, did not move testsets"
        print(f"    {r['file']:30} {r['pi']:32} clauses {r['cut_first_clauses']}{mark}")
    print(f"  {len(clean)} rows carry no cut-first clause")
    return 0


if __name__ == "__main__":
    sys.exit(main())
