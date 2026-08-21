#!/usr/bin/env python3
"""Phase 2 static arm: re-parse call sites, and SEPARATE the three shapes the
Phase-1 extractor silently merges — real calls, comment-position false positives,
and runtime-variable templates.

Reuses extract_overlay_templates.py's balanced-paren arg scan verbatim (imported,
not reimplemented) and adds only the comment mask + shape tagging.
"""
import re, subprocess, sys, os
sys.path.insert(0, "audits/2026-08-19_oq302_bound_false_repair")

CALL = re.compile(r"probe_harness:with_(overlay|retracted|asserted)\s*\(")

def split_args(s):
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

def comment_mask(src):
    """True at every offset that lies inside a % line comment or /* */ block."""
    mask = bytearray(len(src))
    i, n = 0, len(src)
    inq = None
    while i < n:
        c = src[i]
        if inq:
            if c == inq and src[i-1] != "\\": inq = None
            i += 1; continue
        if c in "'\"":
            inq = c; i += 1; continue
        if c == "%":
            j = src.find("\n", i)
            j = n if j < 0 else j
            for k in range(i, j): mask[k] = 1
            i = j; continue
        if src.startswith("/*", i):
            j = src.find("*/", i+2)
            j = n if j < 0 else j+2
            for k in range(i, j): mask[k] = 1
            i = j; continue
        i += 1
    return mask

files = subprocess.run(
    ["git","grep","-l","-E","probe_harness:with_(overlay|retracted|asserted)","--",
     "*.pl",":!prolog/archives",":!.claude"],
    capture_output=True, text=True).stdout.split()

VAR = re.compile(r"^\[?\s*[A-Z_][A-Za-z0-9_]*\s*\]?$")
rows = []
for f in sorted(files):
    if f.endswith("probe_harness.pl"): continue
    src = open(f).read()
    mask = comment_mask(src)
    for m in CALL.finditer(src):
        kind = m.group(1)
        line = src[:m.start()].count("\n") + 1
        in_comment = bool(mask[m.start()])
        args = split_args(src[m.end():])
        retract = args[0].strip() if kind in ("overlay","retracted") else "[]"
        assertl = (args[1].strip() if kind == "overlay" and len(args) > 1
                   else args[0].strip() if kind == "asserted" else "[]")
        one = lambda t: " ".join(t.split())
        r, a = one(retract), one(assertl)
        if in_comment:                       shape = "COMMENT_FALSE_POSITIVE"
        elif VAR.match(r) or VAR.match(a):   shape = "RUNTIME_VARIABLE"
        elif kind == "asserted":             shape = "BARE_ASSERT"
        elif r == "[]":                      shape = "DECLARED_ZERO"
        else:                                shape = "STATIC_TEMPLATE"
        rows.append((f, line, kind, shape, r, a))

print("file\tline\tkind\tshape\tretract_side\tassert_side")
for r in rows: print("\t".join(str(x) for x in r))
sys.stderr.write("TOTAL matches: %d\n" % len(rows))
from collections import Counter
for k, v in sorted(Counter(r[3] for r in rows).items()):
    sys.stderr.write("  %-24s %d\n" % (k, v))
real = [r for r in rows if r[3] != "COMMENT_FALSE_POSITIVE"]
sys.stderr.write("REAL call sites: %d over %d files\n"
                 % (len(real), len(set(r[0] for r in real))))
