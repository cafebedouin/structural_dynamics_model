#!/usr/bin/env python3
"""Extract the ARGUMENT-POSITION template/fact lists from every probe_harness call.

with_retracted(Templates, Goal)      -> arg1 = retract side
with_asserted(Facts, Goal)           -> arg1 = assert side
with_overlay(Templates, Facts, Goal) -> arg1 = retract side, arg2 = assert side

Only arg1 (and arg2 for overlay) matter for the rule-clause hazard; the Goal is
observation, not overlay. Balanced-paren scan, so nested terms survive.
"""
import re, subprocess, sys

files = subprocess.run(
    ["git","grep","-l","-E","probe_harness:with_(overlay|retracted|asserted)","--",
     "*.pl",":!prolog/archives",":!.claude"],
    capture_output=True, text=True).stdout.split()

CALL = re.compile(r"probe_harness:with_(overlay|retracted|asserted)\s*\(")

def split_args(s):
    """s starts just after the opening paren. Return list of top-level arg strings."""
    args, depth, cur, i, inq = [], 0, [], 0, None
    while i < len(s):
        c = s[i]
        if inq:
            cur.append(c)
            if c == inq and s[i-1] != "\\": inq = None
        elif c in "'\"":
            inq = c; cur.append(c)
        elif c in "([{":
            depth += 1; cur.append(c)
        elif c in ")]}":
            if depth == 0:
                args.append("".join(cur)); return args
            depth -= 1; cur.append(c)
        elif c == "," and depth == 0:
            args.append("".join(cur)); cur = []
        else:
            cur.append(c)
        i += 1
    return args

for f in sorted(files):
    if f.endswith("probe_harness.pl"): continue
    src = open(f).read()
    for m in CALL.finditer(src):
        kind = m.group(1)
        line = src[:m.start()].count("\n") + 1
        args = split_args(src[m.end():])
        retract = args[0].strip() if kind in ("overlay","retracted") else "[]"
        assertl = (args[1].strip() if kind == "overlay" and len(args) > 1
                   else args[0].strip() if kind == "asserted" else "[]")
        one = lambda t: " ".join(t.split())
        print(f"{f}:{line}\tkind={kind}\n\tRETRACT-SIDE: {one(retract)[:200]}\n\tASSERT-SIDE:  {one(assertl)[:200]}")
