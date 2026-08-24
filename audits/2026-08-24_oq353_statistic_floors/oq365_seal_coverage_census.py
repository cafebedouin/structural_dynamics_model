#!/usr/bin/env python3
"""OQ-365 evidence — cs_story_uid coverage per corpus == the DP-001/OQ-25 seal's coverage.

The chimera clause (config_validation.pl, ~:199-210) enumerates readings through
cs_story_uid. Where a corpus authors none, the guard fails and the clause contributes
no violation — output IDENTICAL to a clean pass. Where a corpus authors SOME, the guard
passes and checks only that subset — also identical to a clean pass. The seal reports
nothing about its own coverage; this script is what it would report if it did.

Re-derive before citing: corpora move.
"""
import glob, os, subprocess, sys, json

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
rows = []
for d in sorted(glob.glob(f"{ROOT}/prolog/testsets") + glob.glob(f"{ROOT}/prolog/testsets_*")
                + glob.glob(f"{ROOT}/prolog/archives/datasets/*")):
    if not os.path.isdir(d):
        continue
    files = glob.glob(d + "/*.pl")
    if not files:
        continue
    u = int(subprocess.run(f"/usr/bin/grep -l cs_story_uid {d}/*.pl 2>/dev/null | wc -l",
                           shell=True, capture_output=True, text=True).stdout)
    rows.append(dict(corpus=d.replace(ROOT + "/prolog/", ""), files=len(files), uid=u,
                     share=round(100.0 * u / len(files), 1),
                     seal=("VACUOUS" if u == 0 else "PARTIAL" if u < len(files) else "full")))

vac = [r for r in rows if r["seal"] == "VACUOUS"]
part = [r for r in rows if r["seal"] == "PARTIAL"]
full = [r for r in rows if r["seal"] == "full"]

print(f"{'corpus':<48}{'files':>7}{'uid':>7}{'share':>8}  seal")
print("-" * 78)
for r in sorted(rows, key=lambda r: (r["share"], -r["files"])):
    print(f"{r['corpus']:<48}{r['files']:>7}{r['uid']:>7}{r['share']:>7}%  {r['seal']}")

unchecked = sum(r["files"] for r in vac) + sum(r["files"] - r["uid"] for r in part)
print(f"\ncorpora: {len(rows)}  ({len(vac)} VACUOUS, {len(part)} PARTIAL, {len(full)} full)")
print(f"stories under a seal that never runs : {sum(r['files'] for r in vac)}")
print(f"unchecked readings inside PARTIAL    : {sum(r['files'] - r['uid'] for r in part)}")
print(f"TOTAL readings the seal never checks : {unchecked}")
print("\nPARTIAL is the worse case: the guard PASSES and checks a subset, which is")
print("indistinguishable at the read site from full coverage.")
json.dump(rows, open(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "oq365_seal_coverage_census.json"), "w"), indent=1)
