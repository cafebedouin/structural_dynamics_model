#!/usr/bin/env python3
"""Step 0 — 2x2 model Omega: does the 4c observer claim-drift reproduce under Sonnet?

4c (Gemini): stakeholder arm claims `rope`, four-tuple arm claims `tangled_rope`, 3/3 contention
topics (engine corrected both to snare). Q: model-stable framing effect, or Gemini artifact?
Generate both arms under Sonnet (pinned) on the same 3 contention topics; read claimed_type.
Reproduces = stakeholder->rope / four-tuple->tangled_rope (or four-tuple more-extractive than
stakeholder) on >=2/3. Audit-dir only; no live writes.
"""
import json, os, pathlib, sys
REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(REPO/"agent")); sys.path.insert(0, str(REPO/"python"))
from story_generator_base import build_prompt  # four-tuple prompt
PILOT = (REPO/"prompts/constraint_story_generation_prompt_stakeholder_pilot.md").read_text()
SCHEMA = (REPO/"schemas/constraint_story_schema.json").read_text()
SONNET = "claude-sonnet-4-5-20250929"  # pinned
TOPICS = [t for t in json.loads((AUD/"step4_topics_pinned.json").read_text())["topics"]
          if t["label"]=="contention"]

def _client():
    import anthropic
    return anthropic.Anthropic()

def _ask(prompt):
    r = _client().messages.create(model=SONNET, max_tokens=8000,
                                  messages=[{"role":"user","content":prompt}])
    t = r.content[0].text.strip()
    if t.startswith("```"):
        t=t.split("```",2)[1]
        if t.startswith("json"): t=t[4:]
        t=t.strip()
    return json.loads(t)

def stakeholder_arm(topic, cid):
    p = (PILOT + "\n\n=== JSON SCHEMA ===\n" + SCHEMA + "\n\n=== YOUR TASK ===\n"
         + f"Generate a complete constraint story JSON for: TOPIC: {topic}\nUse constraint_id: {cid}\n"
         + "Author stakeholders[] + six_questions + base_properties + header + interval. "
         + "Do NOT author perspectives. Output ONLY valid JSON.\n")
    d=_ask(p); d.setdefault("header",{})["constraint_id"]=cid; return d

def fourtuple_arm(topic, cid):
    d=_ask(build_prompt(f"TOPIC: {topic}") + f"\n\nUse constraint_id: {cid}\n")
    d.setdefault("header",{})["constraint_id"]=cid; return d

print(f"Step 0 — model Omega, Sonnet pinned: {SONNET}\n4c Gemini baseline: stakeholder->rope, four-tuple->tangled_rope (3/3)\n")
rows=[]
for t in TOPICS:
    cid=t["id"]
    print(f"--- {cid} generating both arms (Sonnet) ...", flush=True)
    s=stakeholder_arm(t["topic"], cid+"_sonnet_stake")
    f=fourtuple_arm(t["topic"], cid+"_sonnet_four")
    (AUD/f"{cid}.sonnet_stake.json").write_text(json.dumps(s,indent=2))
    (AUD/f"{cid}.sonnet_four.json").write_text(json.dumps(f,indent=2))
    sc=s.get("base_properties",{}).get("claimed_type"); fc=f.get("base_properties",{}).get("claimed_type")
    se=s.get("base_properties",{}).get("extractiveness"); fe=f.get("base_properties",{}).get("extractiveness")
    rows.append((cid,sc,fc,se,fe))
    print(f"    stakeholder claimed={sc} (eps={se}) | four-tuple claimed={fc} (eps={fe})")

EXTRACT={"rope":0,"scaffold":1,"piton":2,"tangled_rope":3,"snare":4,"mountain":0}
print("\n=== Step 0 result (Sonnet) ===")
repro=0
for cid,sc,fc,se,fe in rows:
    drift = "stake-more-coordination" if EXTRACT.get(sc,9)<EXTRACT.get(fc,9) else ("same" if sc==fc else "inverted")
    if drift=="stake-more-coordination": repro+=1
    print(f"  {cid}: stake={sc} four={fc} -> {drift}")
print(f"\nclaim-drift reproduces (stake more-coordination than four-tuple) on {repro}/3")
print("VERDICT: " + ("REPRODUCES (>=2/3) — claim-layer framing effect is model-stable, not a Gemini artifact"
                     if repro>=2 else
                     "DOES NOT reproduce — 4c claim-drift was Gemini-conditional"))
