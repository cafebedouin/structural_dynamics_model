"""Class sweep for OQ-356: every CALL SITE that reads effective_purity/purity_score
and then does arithmetic on the result. Reports guarded vs UNGUARDED.

Positive control at the bottom: the sweep must FIND the known-unguarded site and
must DECLINE the two known-guarded siblings. A sweep that reports 'all clean' is
worthless unless it is shown able to flag the one defect we already have in hand.
"""
import re, sys
from pathlib import Path

PROLOG = Path("prolog")
CALL = re.compile(r"(effective_purity|purity_score)\s*\(")
# arithmetic/comparison that THROWS on an atom
ARITH = re.compile(r"(>=|=<|>|<|=:=|=\\=|\bis\b|\bmax\b|\bmin\b|sumlist|sum_list|msort|sort\()")
GUARD = re.compile(r"\bnumber\s*\(")

rows = []
for f in sorted(PROLOG.rglob("*.pl")):
    if "/tests/" in str(f) or "/testsets" in str(f) or "/archives/" in str(f):
        continue
    lines = f.read_text(encoding="utf-8", errors="replace").splitlines()
    for i, ln in enumerate(lines):
        code = ln.split("%")[0] if not ln.lstrip().startswith("%") else ""
        if not CALL.search(code):
            continue
        # look at the call line + the next 4 lines (the conjunction that consumes it)
        window = "\n".join(l.split("%")[0] for l in lines[i:i + 5])
        if not ARITH.search(window.split("\n", 1)[1] if "\n" in window else ""):
            continue
        guarded = bool(GUARD.search(window))
        rows.append((str(f), i + 1, guarded, lines[i].strip()[:90]))

ung = [r for r in rows if not r[2]]
g = [r for r in rows if r[2]]
print(f"call sites reading purity then doing arithmetic: {len(rows)}"
      f"  ({len(g)} guarded by number/1, {len(ung)} UNGUARDED)\n")
print("--- UNGUARDED ---")
for f, n, _, t in ung:
    print(f"  {f}:{n}  {t}")
print("\n--- guarded ---")
for f, n, _, t in g:
    print(f"  {f}:{n}  {t}")

# ---- POSITIVE CONTROL ----------------------------------------------------
print("\n--- positive control: does this sweep discriminate? ---")
known_bad = ("prolog/giant_component_analysis.pl", 1278)
known_good = [("prolog/drl_purity_network.pl", 352), ("prolog/giant_component_analysis.pl", 365)]
found_bad = any(f == known_bad[0] and abs(n - known_bad[1]) <= 3 for f, n, gd, _ in rows if not gd)
found_good = [any(f == kf and abs(n - kn) <= 4 and gd for f, n, gd, _ in rows) for kf, kn in known_good]
print(f"  FIRES on the known-unguarded count_by_action_band site : {found_bad}")
print(f"  DECLINES (marks guarded) drl_purity_network:353        : {found_good[0]}")
print(f"  DECLINES (marks guarded) giant_comp precompute :366    : {found_good[1]}")
ok = found_bad and all(found_good)
print(f"  => sweep {'DISCRIMINATES' if ok else 'IS UNTESTED — do not cite its zeros'}")
