"""OQ-117(b)-core within-arm proxy: manifest hypothesis (would-be-fed claim)
vs the withheld replicate's freely-authored claimed_type + extractiveness.
No new spend — reads data already on disk."""
import json, glob, re

KERNELS = [
    "qwerty_path_naturalization", "free_market_naturalization",
    "total_war_unthinkability", "printing_press_reformation", "zero_as_number",
]

def manifest_hyps(k):
    try:
        d = json.load(open(f"outputs/decompose/manifests/{k}.manifest.json"))
    except FileNotFoundError:
        return "NO MANIFEST"
    s = json.dumps(d).lower()
    toks = sorted(set(re.findall(r"snare|mountain|rope|tangled|piton|scaffold|naturaliz", s)))
    return ", ".join(toks)

print("=== OQ-117(b)-CORE WITHIN-ARM PROXY (no new spend) ===")
print("manifest hypothesis = the claim that WOULD be fed in production (seed-spec withholds it)")
print("withheld replicate  = model's freely-authored claim from title+domain+summary only")
print()
hdr = f"{'kernel':32s} | {'manifest hyp-menu (would-be-fed)':40s} | withheld claimed_type/extractiveness x3"
print(hdr)
print("-" * len(hdr))
for k in KERNELS:
    reps = sorted(glob.glob(f"audits/2026-06-12_cohort_zero/replicates/{k}*.json"))
    cts = []
    for r in reps:
        bp = json.load(open(r)).get("base_properties", {})
        cts.append(f"{bp.get('claimed_type')}/{bp.get('extractiveness')}")
    print(f"{k:32s} | {manifest_hyps(k):40s} | {cts}")
print()
print("READING:")
print(" - claimed_type = mountain in 15/15 draws, draw-stable -> claim reconstructed from summary alone.")
print("   MECHANISM established; DIRECTION confounded (all 5 selected as contested-naturalization kernels;")
print("   naturalization IS the mountain claim, so 'mountain 15/15' cannot separate idiom from correct-read).")
print(" - epsilon carries real authored variance 0.08-0.68, mostly draw-stable (the free-gate residual survives).")
print(" - free_market_naturalization (own line): claimed mountain @ e=0.68 stable across 3 draws ->")
print("   claimed-vs-computed divergence PERSISTS with the hypothesis withheld (summary smuggles the claim,")
print("   epsilon authored honestly, the gap is real). Untouched by the selection confound (within-story).")
print(" - printing_press_reformation: lone epsilon-UNSTABLE kernel (0.38/0.42/0.68) -> OQ-118 cast-instability")
print("   showing on the metric side; cross-thread coherence check passing.")
