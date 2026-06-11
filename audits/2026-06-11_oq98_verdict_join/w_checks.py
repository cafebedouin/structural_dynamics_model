#!/usr/bin/env python3
"""W3 A/B banner diff, W4 sidecar-vs-banner, falsifier-2 corpus scan (OQ-98).
Run from repo root after a pipeline run + report regeneration.
Old banner code is taken from commit e8ab707b (pre-Commit-2)."""
import json, re, glob, subprocess, importlib.util

def load_mod(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m

src = subprocess.run(["git", "show", "e8ab707b:python/enhanced_report.py"],
                     capture_output=True, text=True, check=True).stdout
open("/tmp/old_er_oq98.py", "w").write(src)
old = load_mod("old_er", "/tmp/old_er_oq98.py")
new = load_mod("new_er", "python/enhanced_report.py")
data = json.load(open("outputs/enriched_pipeline.json"))

print("=== W3 A/B: thermal_dissipation_constraint (clean green) ===")
print("OLD:", old.build_verdict_banner("thermal_dissipation_constraint", data))
print("NEW:", new.build_verdict_banner("thermal_dissipation_constraint", data))

print("=== W4: sidecar verdict vs banner VERDICT line ===")
ok = True
for sj in sorted(glob.glob("outputs/constraint_reports/*_report.json")):
    side = json.load(open(sj))
    md = sj.replace("_report.json", "_report.md")
    m = re.search(r"VERDICT: (\w+)", open(md).read())
    banner_v = m.group(1) if m else None
    match = side.get("verdict") == banner_v
    ok &= match
    print(f"{side['constraint_id']}: sidecar={side.get('verdict')} banner={banner_v} "
          f"match={match} | verdict_join passthrough: {'verdict_join' in side}")
print("W4:", "PASS" if ok else "FAIL")

print("=== Falsifier 2: zero correction-grade without join alert ===")
pc = json.load(open("outputs/pipeline_output.json"))["per_constraint"]
viol = [e.get("id") for e in pc
        if (e.get("verdict_join") or {}).get("signature_grade") == "correction"
        and not any(a["type"] == "signature_correction"
                    for a in e["verdict_join"].get("alerts", []))]
ncorr = sum(1 for e in pc
            if (e.get("verdict_join") or {}).get("signature_grade") == "correction")
print(f"correction-grade entries: {ncorr}; without alert: {len(viol)} {viol}")
print("Falsifier 2:", "PASS (non-vacuous)" if ncorr > 0 and not viol else "FAIL")
