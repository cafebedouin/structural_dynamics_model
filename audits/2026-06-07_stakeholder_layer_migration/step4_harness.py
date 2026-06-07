#!/usr/bin/env python3
"""Step-4 pilot harness (OQ-83): stakeholder-arm generation + constant-scaffold adapter.

Model PINNED: gemini-2.5-pro (both arms; same-version-means-same-model, operator pin).
Writes only to the audit dir + temp testsets; never json/ or live testsets/.
"""
import json
import os
import pathlib
import sys

REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(REPO / "python"))
sys.path.insert(0, str(REPO / "agent"))

from generate_constraint_pl import generate_pl  # noqa: E402

PINNED_GEMINI_MODEL = "gemini-2.5-pro"
PILOT_PROMPT = REPO / "prompts/constraint_story_generation_prompt_stakeholder_pilot.md"
SCHEMA = REPO / "schemas/constraint_story_schema.json"

# CONSTANT scaffold — the SAME two inert perspectives injected into EVERY
# stakeholder-arm story purely to satisfy the schema's required `perspectives`
# (min 2) and generate_pl's unconditional read. NOT projected from the story's
# stakeholders => carries zero per-story information => cannot be a per-story
# confound. classification_type is a placeholder; A1 proved the computed path
# ignores authored perspectives, and the step4 leak witness re-proves it on
# BOTH axes (type and tuple) with this scaffold present.
CONSTANT_SCAFFOLD = [
    {"classification_type": "rope", "agent_power": "analytical",
     "time_horizon": "civilizational", "exit_options": "analytical",
     "spatial_scope": "global",
     "comment": "SCAFFOLD (OQ-83 step4): inert, constant, schema-satisfying only — NOT a measurement."},
    {"classification_type": "rope", "agent_power": "powerless",
     "time_horizon": "biographical", "exit_options": "trapped",
     "spatial_scope": "local",
     "comment": "SCAFFOLD (OQ-83 step4): inert, constant, schema-satisfying only — NOT a measurement."},
]


def apply_scaffold(story: dict, scaffold=None) -> dict:
    """Return a copy of *story* with constant scaffold perspectives, if absent.

    Stakeholder-arm stories author no perspectives; this injects the constant
    pair so validate_json/generate_pl run unchanged.
    """
    import copy
    s = copy.deepcopy(story)
    if not s.get("perspectives"):
        s["perspectives"] = copy.deepcopy(scaffold if scaffold is not None else CONSTANT_SCAFFOLD)
    return s


def compile_to_temp(story: dict, tag: str) -> pathlib.Path:
    """Compile a (scaffolded) story to a temp testset .pl; return its path."""
    cid = story["header"]["constraint_id"]
    pl = generate_pl(story)
    path = REPO / "prolog" / "testsets" / f".tmp_{tag}_{cid}.pl"
    path.write_text(pl)
    return path


def generate_stakeholder_arm(topic: str, constraint_id: str, model=PINNED_GEMINI_MODEL) -> dict:
    """One stakeholder-arm story from a topic, via Gemini + the pilot prompt."""
    from google import genai
    api_key = os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY")
    client = genai.Client(api_key=api_key)
    prompt = (
        PILOT_PROMPT.read_text(encoding="utf-8")
        + "\n\n=== JSON SCHEMA ===\n" + SCHEMA.read_text(encoding="utf-8")
        + "\n\n=== YOUR TASK ===\n"
        + f"Generate a complete constraint story JSON for: TOPIC: {topic}\n"
        + f"Use constraint_id: {constraint_id}\n"
        + "Author stakeholders[] + six_questions + base_properties + header + interval. "
        + "Do NOT author perspectives. Output ONLY valid JSON.\n"
    )
    resp = client.models.generate_content(model=model, contents=prompt)
    text = resp.text.strip()
    if text.startswith("```"):
        text = text.split("```", 2)[1]
        if text.startswith("json"):
            text = text[4:]
        text = text.strip()
    story = json.loads(text)
    story["header"]["constraint_id"] = constraint_id
    return story


if __name__ == "__main__":
    print(f"pinned model: {PINNED_GEMINI_MODEL}")
    print(f"constant scaffold: {len(CONSTANT_SCAFFOLD)} perspectives")
