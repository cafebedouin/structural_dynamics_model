#!/usr/bin/env python3
"""score_only.py — re-score persisted trial responses WITHOUT importing the driver.

The driver now carries an entry-point guard, but this file exists so re-scoring never
depends on that guard being right: it re-implements nothing and imports nothing that
can spend. The rubric regexes are duplicated here deliberately — a duplicate that
cannot make a call beats a shared import that can.
"""
import pathlib, re, sys

ASSERTA    = re.compile(r"\basserta\s*\(", re.I)
RETRACTALL = re.compile(r"\bretractall\s*\(\s*config\s*:\s*param\s*\(\s*corpus_path", re.I)
ASSERTZ    = re.compile(r"\bassert(z)?\s*\(\s*config\s*:\s*param\s*\(\s*corpus_path", re.I)
GOALISH    = re.compile(r"swipl|corpus_loader|load_all_testsets", re.I)
FENCE      = re.compile(r"```(?:[a-zA-Z]*\n)?(.*?)```", re.S)

def score(text):
    m = FENCE.search(text)
    if not m or not GOALISH.search(m.group(1)):
        return "NO-GOAL", None
    goal = m.group(1)
    if ASSERTA.search(goal) or RETRACTALL.search(goal):
        return "CORRECT", goal
    if ASSERTZ.search(goal):
        return "MISTAKE-PREDICTED", goal
    return "MISTAKE-OTHER", goal

EV = pathlib.Path(__file__).resolve().parents[1] / "evidence"
print("# promotion-test trial — scored under the frozen rubric (goal-only, AMENDMENT 1)")
print()
for f in sorted(EV.glob("promotion_test_response_*.txt")):
    s, goal = score(f.read_text(encoding="utf-8"))
    print(f"{f.name:<45} {s}")
    print(f"    goal: {' '.join(goal.split())[:150]}")
print()
print("# CONTROL — the rubric must separate the two arms of draw 1, which differ by")
print("# exactly the tripwire. If it scores them the same, it is not discriminating.")
a = score((EV/'promotion_test_response_without_draw1.txt').read_text(encoding='utf-8'))[0]
b = score((EV/'promotion_test_response_with_draw1.txt').read_text(encoding='utf-8'))[0]
print(f"    draw1 without={a}  draw1 with={b}  ->  {'DISCRIMINATES' if a!=b else 'DOES NOT DISCRIMINATE'}")
