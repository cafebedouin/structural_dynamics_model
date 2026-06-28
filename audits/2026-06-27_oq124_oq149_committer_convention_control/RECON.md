# Phase 0 — Recon + commit hygiene

Audit: OQ-124 + OQ-149 committer-axis cross-model divergence — convention or signal?
Date: 2026-06-27. Substrate: `prolog/testsets_haiku/` + `prolog/testsets_flash/` (960 each,
filename-matched). Evidence in this dir; all numbers from tool output, not docs.

## Step 1 — Re-classify both twins at ONE commit

The two on-disk twin outputs were classified at **different** commits (haiku `20fab78` Jun 24,
flash `8126231` Jun 13). Between them, the OQ-138 commits **`02b880cb`** (constructed-3) and
**`9bce5afc`** (FCR-9) converted **`false_ci_rope` and `constructed_high_extraction`** —
*exactly* Field A's two signatures — from RECLASSIFY→ROUTE. So the existing outputs are **not
comparable for Field A** (the operator's mtime wrinkle is material, not cosmetic).

Re-ran both via `classify_corpus` (deterministic single-clause `corpus_path` overlay +
provenance-fingerprint gate that throws on a haiku↔flash swap) at current HEAD `bbf5c92`.
Driver: `reclassify_twins.py`; log: `reclassify.log`. Baselines preserved as
`baseline_pipeline_output.{haiku.20fab78,flash.8126231}.json`.

Both outputs now stamp `code_commit_short=bbf5c92`, `n_constraints=960`, `pipeline_run_at=2026-06-27T00:00:00Z`. **`same_commit=true`.**

## Step 2 — Positive controls reproduce (read is not broken)

| Control | Target | Observed | Verdict |
|---|---|---|---|
| `claimed_type` per-id agreement | 0.721 | **0.7208** (692/960) | PASS |
| `cs_kernel_id` membership | 1.000 | **1.000** (960/960), sets identical | PASS |

Both reproduce the `2026-06-13_twin_comparison` / `2026-06-20_kernel_reading_orbits` controls.
The read is sound.

## Step 3 — Raw divergences reproduce at one commit

- **Field A (signature fork).** Bidirectional CHE↔FCR fork = **169** total, but **strongly
  asymmetric**: `haiku=CHE / flash=FCR` = **157**; `haiku=FCR / flash=CHE` = **12** (~13:1).
  Overall `signature` agreement 0.7219. Marginals: haiku 659 CHE / 234 FCR; flash 504 CHE / 357
  FCR. **Finding that refines OQ-124:** the lean is not symmetric "both directions across many
  constraints" — it is overwhelmingly *haiku-foregrounds-extraction / flash-foregrounds-false-rope*.
  The minority (12) direction exists but is small; the bidirectional retraction control must still
  name a positive-control pair in *each* direction (plan), and the 12-case arm is the binding
  scarcity.
- **Field B (`cs_reading_relation`).** Per-slot multiset agreement **0.3917** (target 0.392) over
  960 both-authored slots. Marginals: haiku `coexists_with 1167 / influences 424 / forecloses 413`
  (matches OQ-149 exactly); flash `coexists_with 1207 / forecloses 472 / influences 329` — flash
  leans **slightly more foreclosing** (472 vs 413) and less `influences`.
- **Field C (`cs_axiom_status overridden`).** **51 (haiku) vs 4 (flash)** — reproduced from the
  authored `.pl` directly.

Evidence: `recon_reproduce.py` → `recon_reproduce.json`.

## Hash-pinning (B/C are authored, commit-independent)

Per the stale-witness rule, Fields B/C parse authored facts and are pinned by source-content hash
of the corpus dirs, **not** git HEAD:

- `testsets_haiku`: `020a4dd49633691feab1e339be210ee56f2901053853e084992296da0eeb8182`
- `testsets_flash`: `8e810cda410bd0a54580c07a3f9260735fe519c2c7eacce7db04219a9a55dc70`

(`cat testsets_<twin>/*.pl | sha256sum`.)

## Step 4 — Field-C coercion-witness recoverability GATE → **NEGATIVE** (C pulled out of scope)

The Field-C SIGNAL/CONVENTION call hinges on splitting flash's `holdable` into *authored-holdable*
vs *coerced-holdable*. The committed `.pl` facts are **post-coercion**, so the two are
byte-identical there; distinguishing them needs a **per-slot coercion witness** surviving from the
original flash generation run. **It does not survive:**

1. **`run_no_scope_gemini.py` saves no raw batch responses.** Its only artifact write is
   `failures.json` = the *remaining* (un-processed) seed list (l.297-298). The raw Gemini batch
   output is consumed in-memory (`process_batch_results`) and discarded after repair.
2. **`repair_stats` is aggregate, not per-slot.** `_normalize_axiom_status` records only a global
   `axiom_status_fallback` count + a flat `axiom_status_fallback_values` list (story_repair.py:92-93)
   — no `cid` key. And no committed file anywhere contains `axiom_status_fallback`
   (`grep -rl` over `*.json/*.log/*.txt` = empty).
3. **The dominant remap is silent.** `AXIOM_STATUS_REMAP = {contested→holdable, foreclosed→holdable}`
   (story_repair.py:75, 89-90) returns with **no logging at all** — the most likely coercion path
   leaves zero trace.
4. **`json_flash/` is post-repair, not raw.** The mirrored JSON is the same `story` dict written
   after `repair_story` ran (generate_kernel_corpus.py:725,734,838); its status counts
   (holdable 1899 / overridden 4, **0 missing, 0 out-of-enum**) match the `.pl` exactly, so it
   cannot reveal pre-coercion values for the repaired subset.

Recovering the witness ⇒ re-generating flash with instrumentation = the generation spend the
operator ruled out. **Per the plan's Phase-0 step 4: Field C is OPEN-pending-instrumentation, not
runnable this pass.** It is escalated as a gated follow-up, **not** substituted with a weaker proxy.

### Enrichment for the follow-up (a code-level finding the gate surfaced)

`overridden` is **coercion-invariant**. No coercion path ever produces or removes `overridden`:
valid enums `{holdable, overridden}` pass through untouched (story_repair.py:87-88); `contested`/
`foreclosed`/out-of-enum all map to **`holdable`**, never `overridden`; and a *missing* status is
**not** silently defaulted — `generate_pl` does `ax["status"]` (generate_constraint_pl.py:672),
KeyError-caught → the story **FAILS generation** and never enters the corpus. So:

- The **`overridden` 51-vs-4 asymmetry is a real model-authoring difference**, not a pipeline
  artifact — the "optional-field-drop → defaults to holdable" mechanism the plan analogized to the
  stakeholder fix (`becd0f87`) **does not exist** for `cs_axiom_status` (drop → fail, not → holdable).
- The *only* undecidable piece is the **fine interpretation of flash's `holdable`** (authored vs
  coerced-from-contested/foreclosed). That does not change the headline asymmetry; it only blocks
  the plan's pre-registered "(flash authored holdable explicitly, not coerced)" SIGNAL clause.

This is recorded so the follow-up OQ knows the overridden side is settled and only the holdable
fine-structure needs raw-output capture.

## Scope decision carried into Phase 1

- **Field A** — runnable. Re-classified twins at `bbf5c92`.
- **Field B** — runnable; per the verdict sequencing, with C pulled out **B falls back to its
  settled-provenance covariate alone** (it may not use `cs_axiom_status`/drift, which is C and
  under convention-suspicion this same pass).
- **Field C** — OPEN-pending-instrumentation (gate negative); overridden-is-real enrichment recorded.
