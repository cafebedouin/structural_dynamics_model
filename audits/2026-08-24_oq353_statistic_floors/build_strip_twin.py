#!/usr/bin/env python3
"""Build arm (a') — the kernel-stripped twin of testsets_haiku2.

Removes ONLY narrative_ontology:cs_kernel_id/2 FACT lines. Declaration lines
(`:- discontiguous narrative_ontology:cs_kernel_id/2,`) are PRESERVED — F5.

Two-sided reconciliation (plan §4.1 C3): anchor-matched lines vs ALL lines
mentioning cs_kernel_id; the difference must be fully accounted for as
declaration lines, ENUMERATED INDIVIDUALLY, never summed. Mismatch => STOP.
"""
import re, os, sys, glob, json, hashlib, collections

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SRC  = os.path.join(ROOT, "prolog", "testsets_haiku2")
DST  = os.path.join(ROOT, "prolog", "oq353_arm_astrip_haiku2")
OUT  = os.path.join(ROOT, "audits", "2026-08-24_oq353_statistic_floors")

ANCHOR  = re.compile(r"^narrative_ontology:cs_kernel_id\(")
MENTION = re.compile(r"cs_kernel_id")
DECL    = re.compile(r"^\s*narrative_ontology:cs_kernel_id/2\s*,\s*$")
FACT    = re.compile(r"^narrative_ontology:cs_kernel_id\(\s*([^,]+?)\s*,\s*([^)]+?)\s*\)\.\s*$")

if os.path.isdir(DST):
    import shutil; shutil.rmtree(DST)
os.makedirs(DST)

files = sorted(glob.glob(os.path.join(SRC, "*.pl")))
n_anchor = n_mention = n_decl = 0
unaccounted = []          # mention-lines that are neither anchor nor declaration
decl_lines  = []          # every declaration line, individually
removed     = []          # (file, lineno, story, kernel)
kernel_members = collections.defaultdict(list)

for f in files:
    base = os.path.basename(f)
    src_lines = open(f, encoding="utf-8").read().split("\n")
    keep = []
    for i, ln in enumerate(src_lines, 1):
        if MENTION.search(ln):
            n_mention += 1
            if ANCHOR.match(ln):
                n_anchor += 1
                m = FACT.match(ln)
                if not m:
                    print(f"STOP: anchor line does not parse as a fact: {base}:{i}: {ln!r}")
                    sys.exit(2)
                story, kernel = m.group(1), m.group(2)
                removed.append(dict(file=base, line=i, story=story, kernel=kernel))
                kernel_members[kernel].append(story)
                continue                      # <-- the strip
            elif DECL.match(ln):
                n_decl += 1
                decl_lines.append(dict(file=base, line=i, text=ln))
            else:
                unaccounted.append(dict(file=base, line=i, text=ln))
        keep.append(ln)
    open(os.path.join(DST, base), "w", encoding="utf-8").write("\n".join(keep))

print("=== C3 TWO-SIDED RECONCILIATION ===")
print(f"  lines mentioning cs_kernel_id anywhere : {n_mention}")
print(f"  anchor-matched FACT lines (removed)    : {n_anchor}")
print(f"  declaration lines (preserved)          : {n_decl}")
print(f"  UNACCOUNTED mention lines              : {len(unaccounted)}")
print(f"  reconcile: {n_anchor} + {n_decl} + {len(unaccounted)} == {n_mention} ? "
      f"{n_anchor + n_decl + len(unaccounted) == n_mention}")
if unaccounted:
    print("  STOP — unaccounted mention lines:")
    for u in unaccounted[:20]: print("   ", u)
    sys.exit(3)
if n_anchor + n_decl != n_mention:
    print("  STOP — reconciliation failed"); sys.exit(4)

# declaration lines enumerated INDIVIDUALLY, never summed
with open(os.path.join(OUT, "c3_declaration_lines.txt"), "w", encoding="utf-8") as fh:
    for d in decl_lines:
        fh.write(f"{d['file']}:{d['line']}: {d['text']}\n")
print(f"  every declaration line written individually -> c3_declaration_lines.txt "
      f"({len(decl_lines)} lines)")

# --- strip manifest: removed facts + the same-kernel EXPLICIT pairs they induce
pairs_directed = 0
for k, members in kernel_members.items():
    m = len(set(members))
    pairs_directed += m * (m - 1)          # ordered pairs, self excluded
manifest = dict(
    arm="a-prime", source_leg="testsets_haiku2", twin_dir="prolog/oq353_arm_astrip_haiku2",
    n_files=len(files), n_fact_lines_removed=n_anchor,
    n_declaration_lines_preserved=n_decl, n_unaccounted=0,
    n_kernels=len(kernel_members),
    n_kernels_ge2=sum(1 for v in kernel_members.values() if len(set(v)) >= 2),
    same_kernel_ordered_pairs=pairs_directed,
    same_kernel_unordered_pairs=pairs_directed // 2,
    removed_facts=removed,
    kernel_members={k: sorted(set(v)) for k, v in kernel_members.items()},
)
json.dump(manifest, open(os.path.join(OUT, "c3_strip_manifest.json"), "w"), indent=1)
print(f"\n=== STRIP MANIFEST ===")
print(f"  files copied                     : {len(files)}")
print(f"  cs_kernel_id facts removed       : {n_anchor}")
print(f"  kernels                          : {len(kernel_members)}")
print(f"  same-kernel ORDERED pairs induced: {pairs_directed}")
print(f"  same-kernel UNORDERED pairs      : {pairs_directed//2}")
print(f"  -> c3_strip_manifest.json")
