"""One-off diagnostic: regenerate the 5 repeat-offender rejections and dump raw JSON.

Does NOT validate or save — output goes to outputs/kernel_manifests/run_01/rejected_raw.json
for manual inspection of whether interpretation_layer_present is correctly set.

Run: python3 -m agent.dump_rejected_stories
"""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import anthropic

from agent.story_generator_base import (
    REPO_ROOT,
    _SYSTEM_INSTRUCTION,
    build_prompt,
    strip_json_fences,
)

SEEDS = [
    {
        "constraint_id": "conditional_fitness_reading",
        "kernel_id": "personhood_boundary",
        "reading_id": "conditional_fitness_reading",
        "human_readable": "Conditional Fitness Reading (State Eugenic Authority): Moral standing contingent on state-assessed fitness; exposure of unfit infants is legitimate state action. Spartan context — Gerousia inspection council determines fitness; infants judged unfit excluded from victim set.",
    },
    {
        "constraint_id": "viability_threshold_reading",
        "kernel_id": "personhood_boundary",
        "reading_id": "viability_threshold_reading",
        "human_readable": "Viability Threshold Reading (Medical Capacity Grounding): Moral standing begins at viability (capacity for independent survival); pre-viable organisms lack full standing. Medical determination replaces state eugenic or inherent dignity claims.",
    },
    {
        "constraint_id": "viability_reading",
        "kernel_id": "personhood_boundary",
        "reading_id": "viability_reading",
        "human_readable": "Viability Reading (Medical-Professional Compromise, abortion context): Moral status begins at viability (capacity for independent life outside womb). Authority: Medical profession / Roe v. Wade trimester framework. State balances competing interests; fetus enters victim set at ~24 weeks.",
    },
    {
        "constraint_id": "mitigation_priority",
        "kernel_id": "climate_response_obligation",
        "reading_id": "mitigation_priority",
        "human_readable": "Mitigation-Priority Reading (Prevent Future Harm via Decarbonization): Rapid emissions reduction to minimize warming; intergenerational justice via prevention. Authority: IPCC carbon budgets, Paris 1.5°C target, precautionary principle. Future generations primary beneficiaries; current generation bears transition costs.",
    },
    {
        "constraint_id": "degrowth_reading",
        "kernel_id": "climate_response_obligation",
        "reading_id": "degrowth_reading",
        "human_readable": "Degrowth Reading (Reduce Throughput, Planetary Boundaries): Material throughput reduction as primary response; sufficiency over technological efficiency. Authority: Ecological economics, planetary boundaries framework, critique of green growth feasibility. Planetary systems primary beneficiary; Global North consumption enters victim set.",
    },
]

MODEL = "claude-haiku-4-5-20251001"
OUT_PATH = REPO_ROOT / "outputs" / "kernel_manifests" / "run_01" / "rejected_raw.json"


def main():
    client = anthropic.Anthropic()
    results = []

    for i, seed in enumerate(SEEDS, 1):
        cid = seed["constraint_id"]
        print(f"  [{i}/5] {cid} ...", flush=True)
        prompt = build_prompt(seed["human_readable"])
        resp = client.messages.create(
            model=MODEL,
            max_tokens=8192,
            system=_SYSTEM_INSTRUCTION,
            messages=[{"role": "user", "content": prompt}],
        )
        raw = "".join(b.text for b in resp.content if hasattr(b, "text"))
        json_text = strip_json_fences(raw)
        try:
            story = json.loads(json_text)
        except json.JSONDecodeError as e:
            story = {"_parse_error": str(e), "_raw_head": raw[:500]}

        results.append({
            "seed": seed,
            "story": story,
        })
        print(f"      kernel_codification={story.get('cs_structure', {}).get('kernel_codification', '?')}"
              f"  authority_grounding={story.get('cs_structure', {}).get('authority_grounding', '?')}"
              f"  interpretation_layer_present={story.get('cs_structure', {}).get('interpretation_layer_present', '?')}")

    OUT_PATH.write_text(json.dumps(results, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"\nDumped {len(results)} stories → {OUT_PATH}")


if __name__ == "__main__":
    main()
