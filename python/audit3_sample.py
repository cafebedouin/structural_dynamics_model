"""
audit3_sample.py — Build stratified sample for Audit 3 (profile accumulation impact).

Sampling strategy (per audit plan):
  - All T1 (signature_override_artifact): expected ~56
  - All T9 (maxent_shadow_divergence): expected ~26
  - All T4 (confirmed_liminal): expected ~1
  - 200 random from T10 (convergent_structural_stress)
  - 200 random from all other constraints

Random seed: 42 for reproducibility.

Output: outputs/audit3_sample.json
"""

import json
import random
import os
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ABD_PATH = os.path.join(REPO_ROOT, "outputs", "abductive_data.json")
PIPELINE_PATH = os.path.join(REPO_ROOT, "outputs", "pipeline_output.json")
OUT_PATH = os.path.join(REPO_ROOT, "outputs", "audit3_sample.json")

SEED = 42
T10_SAMPLE_SIZE = 200
OTHER_SAMPLE_SIZE = 200


def load_json(path):
    with open(path) as f:
        return json.load(f)


def main():
    print(f"[audit3_sample] Loading abductive_data.json...", file=sys.stderr)
    abd = load_json(ABD_PATH)
    per_constraint = abd["per_constraint"]  # dict: constraint_id -> list of hypotheses

    print(f"[audit3_sample] Loading pipeline_output.json...", file=sys.stderr)
    pipeline = load_json(PIPELINE_PATH)
    all_pipeline_ids = {c["id"] for c in pipeline["per_constraint"]}

    # Classify each constraint by its trigger classes
    by_trigger = {}
    for cid, hyps in per_constraint.items():
        for h in hyps:
            tc = h.get("trigger_class", "")
            by_trigger.setdefault(tc, set()).add(cid)

    t1_ids = sorted(by_trigger.get("signature_override_artifact", set()))
    t9_ids = sorted(by_trigger.get("maxent_shadow_divergence", set()))
    t4_ids = sorted(by_trigger.get("confirmed_liminal", set()))
    t10_all = sorted(by_trigger.get("convergent_structural_stress", set()))

    # All constrained constraints (those with any abductive hypothesis)
    flagged_ids = set()
    for ids in by_trigger.values():
        flagged_ids |= ids

    # "Other" = all pipeline constraints minus the four special categories
    special_ids = set(t1_ids) | set(t9_ids) | set(t4_ids) | set(t10_all)
    other_all = sorted(all_pipeline_ids - special_ids)

    # Sample
    rng = random.Random(SEED)
    t10_sample = rng.sample(t10_all, min(T10_SAMPLE_SIZE, len(t10_all)))
    other_sample = rng.sample(other_all, min(OTHER_SAMPLE_SIZE, len(other_all)))

    # Union of all sample IDs
    sample_ids = sorted(set(t1_ids) | set(t9_ids) | set(t4_ids) |
                        set(t10_sample) | set(other_sample))

    print(f"[audit3_sample] T1: {len(t1_ids)}, T9: {len(t9_ids)}, T4: {len(t4_ids)}", file=sys.stderr)
    print(f"[audit3_sample] T10 available: {len(t10_all)}, sampled: {len(t10_sample)}", file=sys.stderr)
    print(f"[audit3_sample] Other available: {len(other_all)}, sampled: {len(other_sample)}", file=sys.stderr)
    print(f"[audit3_sample] Total sample: {len(sample_ids)}", file=sys.stderr)

    out = {
        "sample_ids": sample_ids,
        "by_category": {
            "T1_signature_override_artifact": t1_ids,
            "T9_maxent_shadow_divergence": t9_ids,
            "T4_confirmed_liminal": t4_ids,
            "T10_convergent_structural_stress_sample": t10_sample,
            "other_sample": other_sample,
        },
        "counts": {
            "T1": len(t1_ids),
            "T9": len(t9_ids),
            "T4": len(t4_ids),
            "T10_total": len(t10_all),
            "T10_sample": len(t10_sample),
            "other_total": len(other_all),
            "other_sample": len(other_sample),
            "total_sample": len(sample_ids),
            "total_corpus": len(all_pipeline_ids),
        },
        "seed": SEED,
    }

    with open(OUT_PATH, "w") as f:
        json.dump(out, f, indent=2)
    print(f"[audit3_sample] Written: {OUT_PATH}", file=sys.stderr)


if __name__ == "__main__":
    main()
