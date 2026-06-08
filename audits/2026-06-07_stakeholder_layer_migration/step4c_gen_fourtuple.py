#!/usr/bin/env python3
"""4c — generate the FOUR-TUPLE arm (control arm) on the 6 pinned pilot topics.

Existing four-tuple prompt (untouched) via story_generator_base.build_prompt; Gemini
gemini-2.5-pro (pinned, same as the stakeholder arm). Writes {cid}.fourtuple.json to the
audit dir only — never json/ or live testsets/.
"""
import json
import os
import pathlib
import sys

REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(REPO / "agent"))
sys.path.insert(0, str(REPO / "python"))
from story_generator_base import build_prompt  # noqa: E402  (existing four-tuple prompt)

PINNED = "gemini-2.5-pro"
TOPICS = json.loads((AUD / "step4_topics_pinned.json").read_text())["topics"]


def gen(topic, cid):
    from google import genai
    client = genai.Client(api_key=os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY"))
    prompt = build_prompt(f"TOPIC: {topic}") + f"\n\nUse constraint_id: {cid}\n"
    resp = client.models.generate_content(model=PINNED, contents=prompt)
    t = resp.text.strip()
    if t.startswith("```"):
        t = t.split("```", 2)[1]
        if t.startswith("json"):
            t = t[4:]
        t = t.strip()
    story = json.loads(t)
    story["header"]["constraint_id"] = cid
    return story


print(f"four-tuple arm, model pinned: {PINNED}")
for t in TOPICS:
    cid = t["id"]
    print(f"--- {cid} [{t['label']}] generating ...", flush=True)
    story = gen(t["topic"], cid)
    (AUD / f"{cid}.fourtuple.json").write_text(json.dumps(story, indent=2))
    np = len(story.get("perspectives", []))
    bp = story.get("base_properties", {})
    print(f"    perspectives={np} eps={bp.get('extractiveness')} "
          f"benef={bool(bp.get('beneficiaries'))} vic={bool(bp.get('victims'))} "
          f"hr={bp.get('human_readable','')[:60]}")
print("done")
