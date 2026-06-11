#!/usr/bin/env python3
"""OQ-81 A/B execution — see AB_PLAN.md (pre-registered) for design.

Generates infrastructure_trust_paradox under 3 context arms x 3 reps with the exact
_step_generate params, via the direct messages API. Writes everything under ab_runs/
in this audit dir; touches neither prolog/testsets/ nor json/.
"""
import json, sys, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(REPO))

import agent.story_generator_base as sgb
import anthropic

MANIFEST = Path("/home/scott/bin/structural_dynamics_model/outputs/kernel_manifests/kernel_run_01/dutch_flood_control_culture.manifest.json")
CLAIM_ID = "infrastructure_trust_paradox"
UPSTREAM_ID = "husk_reading"
INJECTED_TYPE = "tangled_rope"  # archived preparedness_retention__husk_reading claimed_type
MODEL = "claude-sonnet-4-5-20250929"
MAX_TOKENS = 16384
TEMPERATURE = 0.2
REPS = 3

m = json.loads(MANIFEST.read_text())
axis = next(a for a in m["axes"] if a["claim_id"] == CLAIM_ID)
csr = m.get("commitment_system_recognition") or {}
readings = {r["reading_id"]: r for r in csr.get("readings", [])}

source_desc = sgb.axis_source_desc(m, CLAIM_ID, axis)

# Arm R: byte-what upstream_context() emits given the archived reading verdict.
ctx_r = sgb.upstream_context(
    axis, {UPSTREAM_ID: {"base_properties": {"claimed_type": INJECTED_TYPE}}}, CLAIM_ID)
assert "tangled_rope" in ctx_r and "husk_reading" in ctx_r, "Arm R context did not assemble"

# Arm K: kernel substrate, both readings presented as contested, no verdict (AB_PLAN format).
ctx_k = (
    f"\nUPSTREAM KERNEL: {csr.get('kernel_id')} (contested — readings disagree; no settled verdict)\n"
    f"  summary: {m.get('topic_summary', '').strip()}\n"
    + "".join(f"  reading {rid}: {r.get('commitment', '').strip()}\n"
              for rid, r in readings.items())
    + f"  affects_constraint: {csr.get('kernel_id')} → {CLAIM_ID}\n"
)

ARMS = {"N": "", "R": ctx_r, "K": ctx_k}

outdir = HERE / "ab_runs"
outdir.mkdir(exist_ok=True)
(outdir / "contexts.json").write_text(json.dumps(
    {"source_desc": source_desc, "arms": ARMS}, indent=1, ensure_ascii=False))

client = anthropic.Anthropic()
results = []
for arm, ctx in ARMS.items():
    static_prefix, dynamic_tail = sgb.build_prompt_parts(source_desc, ctx)
    for rep in range(1, REPS + 1):
        tag = f"{arm}{rep}"
        t0 = time.time()
        resp = client.messages.create(
            model=MODEL, max_tokens=MAX_TOKENS, temperature=TEMPERATURE,
            system=sgb._SYSTEM_INSTRUCTION,
            messages=[{"role": "user", "content": [
                {"type": "text", "text": static_prefix,
                 "cache_control": {"type": "ephemeral"}},
                {"type": "text", "text": dynamic_tail},
            ]}],
        )
        raw = "".join(b.text for b in resp.content if b.type == "text")
        (outdir / f"{tag}.raw.txt").write_text(raw)
        story, errors = sgb.process_response(raw)
        if story is not None:
            (outdir / f"{tag}.story.json").write_text(
                json.dumps(story, indent=1, ensure_ascii=False))
        bp = (story or {}).get("base_properties", {})
        row = {
            "tag": tag, "arm": arm, "rep": rep,
            "parse_ok": story is not None,
            "schema_errors": errors,
            "claimed_type": bp.get("claimed_type"),
            "base_extractiveness": bp.get("base_extractiveness"),
            "suppression_requirement": bp.get("suppression_requirement"),
            "theater_ratio": bp.get("theater_ratio"),
            "stop_reason": resp.stop_reason,
            "input_tokens": resp.usage.input_tokens,
            "output_tokens": resp.usage.output_tokens,
            "seconds": round(time.time() - t0, 1),
        }
        results.append(row)
        print(json.dumps(row))

(outdir / "summary.json").write_text(json.dumps(results, indent=1))
print(f"\nWrote {len(results)} runs to {outdir}")
