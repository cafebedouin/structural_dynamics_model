#!/usr/bin/env python3
"""caller_sweep.py — bound-last-argument CALL-SITE sweep for every checker hit.

Layered on top of the definition-site census (census_checker_run1_*.txt): for each
flagged predicate, find call sites whose LAST argument is a literal lowercase atom
(the bound form). Produces the A/B split input:
  - 0 bound call sites  -> class B candidate (latent; shape present, nobody bound)
  - >0 bound call sites -> adjudicate input-key vs output by reading the predicate

Positive control: must find the six is_X/3 delegation calls inside drl_core.pl
(classify_from_metrics called with Type bound) and the run_pipeline.py bound goals.

Point-in-time audit probe (lives in audits/, excluded from bound_selector_check scope
by that checker's declared path exclusion). Census HEAD: 9a5d8526.
"""
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
CENSUS = Path(__file__).parent / "census_checker_run1_HEAD_9a5d8526.txt"

SCAN_DIRS = ["prolog", "python", "agent", "scripts"]
SKIP_PARTS = {"testsets", "testsets_haiku", "testsets_flash", "testsets_kimi",
              "testsets_sonnet", "archives", "outputs", "node_modules", ".git",
              "kernels", "beta_seeds"}
SUFFIXES = {".pl", ".py", ".sh"}

hits = []
for line in CENSUS.read_text().splitlines():
    m = re.match(r"DHC_HIT: (\S+) (\S+)/(\d+) ", line)
    if m:
        hits.append((m.group(1), m.group(2), int(m.group(3))))

if not hits:
    sys.exit("caller_sweep: RED — census file yielded 0 predicates (broken parse, not clean)")

ATOM = r"[a-z][A-Za-z0-9_]*"

def call_pattern(name, arity):
    # name( a1, ..., aN ) with last arg a bare lowercase atom. Args may not contain
    # commas/parens (conservative: misses nested-term args, which cannot be a bare atom
    # in last position anyway unless simple).
    inner = r"\s*,\s*".join([r"[^(),]+?"] * (arity - 1) + [rf"({ATOM})"]) if arity > 1 else rf"({ATOM})"
    return re.compile(rf"\b{name}\s*\(\s*{inner}\s*\)")

def is_comment(line, suffix):
    s = line.lstrip()
    return s.startswith("%") or s.startswith("*") if suffix == ".pl" else s.startswith("#")

def is_clause_head(line, m):
    if line[:m.start()].strip():
        return False
    return re.match(r"\s*\)?\s*(:-|\.|-->)", line[m.end():]) is not None

files = []
for d in SCAN_DIRS:
    for p in sorted((REPO / d).rglob("*")):
        if p.suffix in SUFFIXES and p.is_file() and not any(x in p.parts for x in SKIP_PARTS):
            files.append(p)
if len(files) < 100:
    sys.exit(f"caller_sweep: RED — only {len(files)} files in scope (expected engine-scale)")

results = {}
for deffile, name, arity in hits:
    pat = call_pattern(name, arity)
    sites = []
    for p in files:
        text = p.read_text(encoding="utf-8", errors="replace")
        if name not in text:
            continue
        for i, line in enumerate(text.splitlines(), 1):
            m = pat.search(line)
            if not m or is_comment(line, p.suffix):
                continue
            if p.suffix == ".pl" and is_clause_head(line, m):
                continue
            sites.append((str(p.relative_to(REPO)), i, m.group(1), line.strip()[:120]))
    results[(deffile, name, arity)] = sites

n_zero = sum(1 for v in results.values() if not v)
print(f"CALLER_SWEEP: {len(hits)} predicates, {len(files)} files scanned, "
      f"{n_zero} with 0 bound-last-atom call sites")
for (deffile, name, arity), sites in sorted(results.items(), key=lambda kv: -len(kv[1])):
    print(f"\n== {name}/{arity} ({deffile}): {len(sites)} bound call site(s)")
    for rel, ln, atom, line in sites[:40]:
        print(f"   {rel}:{ln}  atom={atom}  | {line}")
    if len(sites) > 40:
        print(f"   ... and {len(sites) - 40} more")
