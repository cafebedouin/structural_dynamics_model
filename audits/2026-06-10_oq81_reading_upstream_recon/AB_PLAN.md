# OQ-81 A/B plan — does an injected reading claimed_type distort supplementary-axis authoring?

**Pre-registered 2026-06-10, BEFORE the run** (per the plan-review bar: arms symmetrized, metrics
and decision rules pinned pre-spend so the outcome cannot be narrated into agreement).
Operator authorized the spend ("Run the cheap A/B experiment, option (c)").

## Subject

The OQ-cited reading-edge axis: **`infrastructure_trust_paradox`** from
`outputs/kernel_manifests/kernel_run_01/dutch_flood_control_culture.manifest.json`
(kernel `preparedness_retention`; axis hypothesis **snare**, ε-bin mod,
beneficiary institutional_legitimacy, victim public_preparedness;
`downstream_of: [husk_reading]`). The upstream verdict the live pipeline would inject:
the archived husk_reading story's `base_properties.claimed_type` = **tangled_rope**
(`prolog/archives/datasets/kernel_v1_json/preparedness_retention__husk_reading.json`).

Deliberate property of this subject: the axis hypothesis (snare) and the injected upstream
verdict (tangled_rope) DIFFER, so verdict-import is distinguishable from hypothesis-following.

## Arms (identical prompt assembly except the context block)

All arms: `build_prompt_parts(axis_source_desc(manifest, claim_id, axis), CTX)` from
`agent/story_generator_base.py`, system = `_SYSTEM_INSTRUCTION`, model
`claude-sonnet-4-5-20250929`, max_tokens 16384, temperature 0.2 — the exact
`_step_generate` params. Transport is the direct messages API rather than the batch API
(same params; transport fidelity caveat carried to the writeup).

- **Arm N (no-context):** CTX = "" — the historical behavior (gkc wave-free; also what every
  corpus story actually got, per recon F1). Baseline.
- **Arm R (reading-injected):** CTX = byte-what `upstream_context()` emits:
  `\nUPSTREAM CONSTRAINT: husk_reading\n  claimed_type: tangled_rope\n  affects_constraint: husk_reading → infrastructure_trust_paradox\n`
  — the current unified-backend behavior on this manifest.
- **Arm K (kernel-substrate):** CTX names the kernel and presents BOTH readings as contested,
  no verdict:
  ```
  UPSTREAM KERNEL: preparedness_retention (contested — readings disagree; no settled verdict)
    summary: <manifest topic_summary>
    reading husk_reading: <commitment text>
    reading competence_reading: <commitment text>
    affects_constraint: preparedness_retention → infrastructure_trust_paradox
  ```
  — the OQ's candidate fix (b), made concrete here for the first time.

**Reps:** 3 per arm (9 calls). Temperature stays at the pipeline's 0.2 (fidelity over spread).

## Pre-registered metrics

Primary (decision-bearing):
1. **Authored `base_properties.claimed_type` per rep.** Verdict-import = R authors
   tangled_rope (the injected token) where N authors something else. Hypothesis-following =
   authoring tracks snare regardless of arm.
2. **Within-arm agreement** on (1): an arm's reps must agree ≥2/3 for its value to count.

Secondary (descriptive, reported but not alone decision-bearing):
3. Scalars: `base_extractiveness`, `suppression_requirement`, `theater_ratio` — between-arm
   shift vs within-arm spread.
4. Upstream-frame leakage: does R's narrative text import the husk reading's vocabulary
   (memorial/husk/retention-without-competence framing) where N's does not? (Read, quote.)

## Decision rules (what each outcome means — written before the run)

- **DISTORTION:** R differs from N on metric 1 in the direction of the injected verdict
  (R→tangled_rope, N→not), within-arm agreement holding, and K ≈ N → a reading's claimed_type
  steers authoring; supports suppressing reading-typed upstreams / fix (b).
- **INERT:** all arms agree on metric 1 and secondary shifts ≤ within-arm spread → injection is
  low-risk at n=3; OQ-81 leans closeable as "no distortion witnessed" (power caveat carried).
- **CONTEXT-SENSITIVE-EVERYWHERE:** R AND K both diverge from N → any upstream block shifts
  authoring; which shift is appropriate becomes the operator ruling, with these diffs as
  evidence.
- **UNDERPOWERED:** within-arm disagreement (<2/3) on metric 1 in any decision-relevant arm →
  no verdict from this run; report as underpowered, do not narrate a lean.

A claimed_type equal to the axis hypothesis (snare) in ALL arms counts toward INERT on metric 1
even if scalars drift — hypothesis-following is the intended behavior.

## Hygiene

- Output goes to this audit dir ONLY (`ab_runs/`); nothing is written to `prolog/testsets/` or
  `json/` (live corpus untouched; no run_pipeline involved).
- Raw API responses, assembled prompts, and parsed stories are all saved (paste-or-untag).
- Stories are schema-validated with `process_response` for parseability; lint/compile is NOT
  part of this experiment.
