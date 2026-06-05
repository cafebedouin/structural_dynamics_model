# Generation-Pipeline De-Leak — Execution Writeup

**Date:** 2026-06-05. **Work item:** audit brief F1–F9 ("a key must never leak upstream in the
output direction"). **Theory:** `docs/the_perturbation_principle.md` — the authored-claim vs
computed-type diff is the research signal; the pipeline was handing the authoring LLM the engine's
decision boundaries, collapsing it. **Plan + user rulings:** four AskUserQuestion rulings
2026-06-05 (schema bands AND ε-triggers stripped; maximal scrub; F5/F6 deferred as OQ-72/OQ-73;
linter demote-and-filter). Session commits: `29cd45d4`, `9f2d050a`, `b6c4e113`, `7ad86c5a`,
`07f7b1c0`, `d179423d` (+ docs/tracking).

## Headline finding (extends the brief)

**The binding leak was the JSON schema, not the prompt.** `python/constraint_story_schema.json`
`allOf` conditionals tied `claimed_type` to numeric bands (mountain ε≤0.25/supp≤0.05; piton
theater≥0.7; ε>0.46→measurements; ε>0.70→mandatrophy), the schema text ships verbatim inside the
generation prompt (`agent/story_generator_base.py:28` + `build_prompt`), and schema validation is
a retry-until-valid gate (`story_generator_base.py:428-433`). Consequence: a claimed-mountain with
high ε — the false summit the engine exists to catch — was **unauthorable**; the ε-axis diff for
mountains was structurally zero. A prompt-only scrub would have been cosmetic.

## What changed (each with same-turn witnesses; see git messages)

1. **Linter coordination_type 4→6** (`29cd45d4`): 286 false INVALID_COORDINATION_TYPE cleared
   (corpus lint 1821→1535, delta exactly 286); positive control (bogus value) still flagged.
   Canonical 6-value table + OQ-30 offset-active/floor-inactive asymmetry → `docs/logic_extensions.md`.
2. **Schema de-leak** (`9f2d050a`): bands + ε-trigger conditionals deleted (allOf 9→6); structural
   conditionals kept (mountain→emerges_naturally; FSM→omegas; tangled→enforcement+benef+victims;
   snare→victims; scaffold→sunset); the "0.70" string leak in `mandatrophy_resolved.description`
   cleaned; measurements/omegas unconditionally encouraged. Witness: synthetic claimed-mountain
   ε=0.6 REJECTED before → AUTHORABLE after; original mountain still passes; tangled-without-victims
   still rejected (structural gates intact); `generate_pl` compiles the false summit
   (`evidence/probe_false_summit.pl`).
3. **Prompt de-leak, maximal** (`b6c4e113`): qualitative type criteria; χ/sigmoid/f(d)/σ numeric
   tables → prose (d∈[0,1] semantics kept for `directionality_overrides`); NL-profile gate values →
   presence-with-honest-values; TYPE↔METRIC HARD GATE → claim/metric-independence directive;
   worked-example ε anchors removed; `epsilon_bin` hand-off dropped (3 orchestrators).
4. **Axes cap → optional ceiling** (`7ad86c5a`): `--axes` default None; SCOPE "THREE IS THE
   BUDGET" → distinctness-is-the-budget; §4 = ranking/ordering. F4: "2–4 readings" → emit what the
   kernel sustains.
5. **Retry-path scrub** (`07f7b1c0`): `THRESHOLD_COUPLED_LINT` filtered at the `build_user_prompt`
   choke point (covers both channels: `known_errors` + `retry_errors`). Witness: tripping story's
   lint contains MOUNTAIN_METRIC_CONFLICT, built prompt doesn't, MISSING_NL_PROFILE passes.
6. **Lens diversity** (`d179423d`) — **separate change variable** for Stage-2 attribution.

## The closing witness — at the interface where the leak exists

Source-file greps cannot prove closure (a band in a schema description string passes both an
allOf diff and a prompt-file grep). The real author-facing payload was assembled to the LLM-call
boundary (`build_prompt`, no API call) before and after:

| Grep on assembled payload | pre (`evidence/payload_pre.txt`) | post (`evidence/payload_post.txt`) |
|---|---|---|
| band value within 80 chars of a type name | **19** | **0** |
| metric-name comparison operators (ε/supp/theater ≷ 0.x) | **28** | **0** |

Both patterns fire on the pre-change payload — the positive controls for the probes themselves.

## Stage-1 single-example gate — PASS

Probe controls first: clean corpus mountain (`axiom_of_choice_consequence`) → claim=computed=
mountain at all 4 standard contexts, **no** `dr_claim_mismatch` (diff ≈ 0); synthetic
claimed-mountain/ε=0.6 → fires `type_1_false_summit-severe` (the probe discriminates).

End-to-end generated example (post-change pipeline, single seed
`evidence/stage1_seed.json` → `generate_kernel_corpus --scope --run-tag stage1_probe`):
`price_formation_kernel` produced **4 readings + 1 ordinary axis** (not the old triad;
`institutional_reading` failed validation and was dropped — visible in the run log).
The naturalist reading (`evidence/stage1_naturalist_reading.json`):

- **Authored:** claim=mountain, ε=0.0, `emerges_naturally: true`, 2 beneficiaries (the
  false-summit candidate shape; schema's kept FSM conditional forced omegas).
- **Computed:** tangled_rope (powerless), scaffold (moderate), scaffold (institutional),
  tangled_rope (analytical) — mountain at NO seat.
- **Diff detectors:** `dr_claim_mismatch = type_1_false_summit-severe`; `false_summit_mountain`
  fired with `fsm_evidence(2,0)`.
- **ε across the family:** 0.0 / 0.62 / 0.62 / 0.68 — off the 0.58 anchor; the 8/2 last-digit
  idiom persists (small sample — re-read at Stage-2 scale).

**Honesty note:** this particular story (ε=0.0) would have validated under the OLD schema too;
the new-regime-specific authorability (high-ε mountain) is witnessed by the synthetic control,
not by this story. Together they cover both halves: the pipeline produces legible non-zero diffs
end-to-end, and the previously-unauthorable object is now authorable.

## No-cap SCOPE witness (F3 contingency gate: did not fire, with a caveat)

3 topics, `--dry-run --skip-search`, no ceiling (`evidence/scope_t{1,2,3}.txt`): **uniform 7
axes, 0 deferred, on all three.** Novelty judgment: axes 4+ are NOT near-duplicates — distinct
structural deltas and disjoint observables (closest pairs: cbam compliance-barrier /
domestic-protection; UBI pilot-methodology / fiscal-opacity — borderline, not duplicate). The
contingency gate (near-duplicates ⇒ §3 not discriminating) does not fire. **Caveat for OQ-75:**
the 7-7-7/0-deferred uniformity suggests a new implicit count target (within the 4–10
decomposition bound) and §3 never dropped anything; watch the axis-count distribution at scale.
Note: these runs used the post-`d179423d` prompt (lens-diversity is kernel-scoped; flat-axis
decomposition minimally confounded — recorded for attribution anyway).

## Residuals (deliberate, recorded)

- Coordination offset/floor table stays in the prompt (engine cost params, not classification
  bands; the Boltzmann coupling threshold itself is NOT disclosed).
- Qualitative f(d)/χ direction-of-effect mentions stay (no numeric content).
- Schema-validation retry messages outside c-orchestrator are unsanitized — harmless now (the
  schema no longer carries band values for the type conditionals to echo).
- `agent/data/constraint_story_schema.json` is a stale orphan (158-line diff, no loader reads
  it; only `commitment_corpus/apply_schema_patch.py` docstring mentions it) — deletion proposed,
  awaiting operator ruling.
- c-orchestrator `_step_generate` resolves only flat `manifest["axes"]`; kernel-reading entries
  skip (pre-existing; witnessed twice this session). Kernel topics go through
  `generate_kernel_corpus.py`.
- `prolog/testsets/stage1_probe/` (glob-isolated run-tag) holds the generated probe stories;
  promotion to main corpus is the operator's call (eyeball file:
  `outputs/kernel_manifests/stage1_probe/coherence_eyeball.md`).

## Open questions filed

OQ-72 (mechanical axiom alignment key), OQ-73 (cross-frame probe; sequencing: after OQ-75
baseline), OQ-74 (coordination_type kernel-vs-reading ruling; 55% = 158/286 re-witnessed),
OQ-75 (Stage-2 rebuild: diff distribution + cross-axis invariance correlation; staked prediction
weak/positive-but-far-from-1; gated on operator go).
