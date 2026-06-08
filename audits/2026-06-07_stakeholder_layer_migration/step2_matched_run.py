# Matched run: SAME manifests, SAME backend (generate_from_manifests), SAME neutral prompt;
# vary ONLY GEN model (Haiku vs Sonnet). Cleanly isolates the GEN-model effect on drift authoring
# (no re-SCOPE reading-set confound, no legacy-vs-unified path confound). Confirms whether bumping
# GEN_MODEL Haiku->Sonnet de-saturates the drift on MATCHED readings.
import os, sys, glob, json, re, collections, pathlib
sys.path.insert(0, "/home/scott/bin/structural_dynamics_model")
os.environ["DR_GEN_PROMPT"] = "prompts/constraint_story_generation_prompt_DRIFTNEUTRAL.md"  # both arms neutral
import agent.generate_kernel_corpus as g

manifests = [json.load(open(f)) for f in sorted(glob.glob(
    "outputs/kernel_manifests/pilot_s1_neutral/*.manifest.json"))]
print(f"loaded {len(manifests)} matched manifests")
SYSTEM = [{"type":"text","text":g._SYSTEM_INSTRUCTION,"cache_control":{"type":"ephemeral"}}]

def gen(model, tag):
    jd, td, md, pl = g.run_dirs(tag)
    print(f"--- generating {tag} (model={model}) ...", flush=True)
    g.generate_from_manifests(manifests, jd, td, pl, model=model, max_tokens=8192,
                              system=SYSTEM, temperature=0.2,
                              progress=lambda *a, **k: print("   ", *a, flush=True))
    return td

def drift_dist(tag):
    mag=collections.Counter(); ack=collections.Counter(); dirc=collections.Counter(); n=0
    for f in pathlib.Path(f"prolog/testsets/{tag}").glob("*.pl"):
        for m in re.finditer(r"cs_drift_state\([^,]+,\s*[^,]+,\s*gap\((\w+),\s*(\w+),\s*(\w+)\)\)", f.read_text()):
            di,ma,ac=m.groups(); n+=1; mag[ma]+=1; ack[ac]+=1; dirc[di]+=1
    return n,mag,ack,dirc

gen("claude-haiku-4-5-20251001", "pilot_matched_haiku")
gen("claude-sonnet-4-5-20250929", "pilot_matched_sonnet")

print("\n=== MATCHED drift distribution (same manifests, same backend+prompt, vary only GEN model) ===")
for tag in ["pilot_matched_haiku","pilot_matched_sonnet"]:
    n,mag,ack,dirc=drift_dist(tag)
    if n==0:
        print(f"{tag}: EMPTY — driver/probe failure, NOT a result"); continue
    print(f"\n{tag}: N={n}")
    print(f"  magnitude {dict(mag)} | substantial-share={mag.get('substantial',0)/n:.0%}")
    print(f"  acknowledged {dict(ack)} | false-share={ack.get('false',0)/n:.0%}")
    print(f"  direction {dict(dirc)}")
nh,mh,ah,_=drift_dist("pilot_matched_haiku"); ns,ms,as_,_=drift_dist("pilot_matched_sonnet")
if nh and ns:
    hs=mh.get('substantial',0)/nh; ss=ms.get('substantial',0)/ns
    haf=ah.get('false',0)/nh; saf=as_.get('false',0)/ns
    print(f"\n=== MATCHED GEN-model effect on substantial-share: Haiku {hs:.0%} -> Sonnet {ss:.0%} (Δ {(hs-ss)*100:.0f}pts)")
    print(f"=== MATCHED ack-false-share: Haiku {haf:.0%} -> Sonnet {saf:.0%} (Δ {(haf-saf)*100:.0f}pts)")
    print("VERDICT: " + ("Sonnet DE-SATURATES on matched readings -> husk-saturation is a Haiku GEN prior; bump GEN_MODEL->Sonnet confirmed"
                         if (hs-ss)>0.15 else
                         "no clean GEN-model de-saturation on matched readings -> saturation NOT primarily a Haiku prior"))
print("MATCHED RUN DONE")
