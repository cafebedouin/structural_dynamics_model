# PRE-REGISTRATION — OQ-124 + OQ-149 committer-axis convention control

**Committed BEFORE execution.** Substrate pinned: twins re-classified at commit `bbf5c92`
(`pipeline_output.{haiku,flash}.json`, n=960 each); authored fields hash-pinned
(`testsets_haiku` `020a4dd4…`, `testsets_flash` `8e810cda…`). Positive controls reproduced
(claimed_type 0.7208, cs_kernel_id 1.000) — see `RECON.md`. A wrongly-specified criterion below
is **halt-and-escalate**, never inline-amended.

**Verdict sequencing:** C-gate → C → B; A independent. The Phase-0 Field-C gate returned
**NEGATIVE** (per-slot coercion witness unrecoverable — `RECON.md` step 4), so **Field C is pulled
to OPEN-pending-instrumentation** and **Field B falls back to its settled covariate alone** (it may
NOT use `cs_axiom_status`/drift).

A verdict ships only with its control firing; a verdict whose control does not fire is **OPEN**,
not ruled. Every probe is read-only/cache-clearing; a control that itself fails is halt-and-surface.

---

## Field A — signature fork (CHE ↔ FCR), bidirectional

**Unit:** the 169 matched fork slots (157 `haiku=CHE / flash=FCR` + 12 `haiku=FCR / flash=CHE`).
For each slot, classify the FCR-side limb by which `appears_as_rope` source is active
(`signature_detection.pl:1391` source-1 `explicit_rope_claim` = authored `constraint_claim(C,rope)`;
`:1402` source-2 `low_extraction_profile` = min ε ≤ `rope_epsilon_ceiling`=0.45):

- **SOURCE1_ONLY** — `claim_rope` present ∧ min ε > 0.45 (retraction MUST flip FCR off).
- **BOTH** — `claim_rope` present ∧ min ε ≤ 0.45 (retraction must NOT flip; source-2 rescues).
- **SOURCE2_ONLY** — no `claim_rope` ∧ min ε ≤ 0.45.

**Pinned decision rule (per direction, then size-weighted overall):**
- **CONVENTION** if `SOURCE1_ONLY share ≥ 0.50` of that direction's forks — the FCR limb is driven
  by the authored template slot (OQ-70-analog) the other model omits.
- **SIGNAL** if `SOURCE1_ONLY share < 0.50` **AND** the continuous-magnitude substrate is present:
  the CHE-side authored `base_extractiveness` exceeds the FCR-side on ≥ 0.80 of that direction's
  forks (the fork is a threshold-crossing of a real authored extraction-magnitude difference).
- Otherwise **MIXED → OPEN** (route to a tighter OQ).

**Pre-registered falsifier — bidirectional retraction (the discriminating control).** Via
`probe_harness:with_retracted/2` (snapshot-first, verified restore, `cache_registry:clear_all_caches/0`):

| Control cid | Direction | Class | Predicted outcome on retracting `constraint_claim(C,rope)` |
|---|---|---|---|
| `human_dignity_ai_governance__techno_optimist_reading` | minority (haiku FCR) | SOURCE1_ONLY (ε=0.78) | `appears_as_rope`/`false_ci_rope` **MUST drop** (no source-2 rescue) |
| `acceptable_risk_for_energy__expected_value_dominant` | dominant (flash FCR) | BOTH (ε=0.30) | `appears_as_rope` **MUST survive** via `low_extraction_profile` |

These are a **two-sided** control (one must flip, one must NOT) covering **each** direction — a
stronger probe than two same-sign flips. If either misbehaves, the probe is broken → **halt**.

**Consistency note (keep, do not suppress):** if `appears_as_rope` survives source-1 retraction it
is riding source-2 = exactly the continuous-magnitude substrate SIGNAL names, so the retraction
result and the decomposition agree by construction; a disagreement is itself informative — flag it.

**Recon expectation (NOT the verdict — the run confirms it):** dominant SOURCE1_ONLY share ≈ 0/157,
minority ≈ 5/12; cross-twin `base_extractiveness` Spearman 0.86 with flash systematically lower
(0.508 vs 0.565). Pre-registered so the execution either confirms or contradicts it.

---

## Field C — `cs_axiom_status overridden` 51-vs-4 — **PULLED to OPEN-pending-instrumentation**

Phase-0 gate NEGATIVE: the per-slot coercion witness (split flash `holdable` into authored vs
coerced-from-contested/foreclosed/out-of-enum) does **not** survive the original flash generation
run (`run_no_scope_gemini` saves no raw responses; `repair_stats` is aggregate; the
`contested/foreclosed→holdable` remap is silent; `json_flash/` is post-repair). Recovering it ⇒
re-generation with instrumentation = the spend the operator ruled out. **Not ruled this pass;**
escalated as a gated follow-up. **No weaker decidable-but-wrong proxy is substituted.**

**Enrichment carried to the follow-up (decidable now, recorded — not a verdict on the plan's C
question):** `overridden` is coercion-invariant (story_repair.py:87-90; missing status → generation
FAIL, generate_constraint_pl.py:672, not a silent holdable default). So the 51-vs-4 asymmetry is a
**real model-authoring difference**, not the optional-field-drop artifact the plan hypothesized; the
only undecidable piece is the fine interpretation of flash's `holdable`.

---

## Field B — `cs_reading_relation` 0.392 — settled covariate only (C pulled)

**Pinned settled covariate:** authored `base_extractiveness` (observer-axis closure-degree). Witnessed
settled: directly authored Surface-1 (json `base_properties.extractiveness`), **not** a derived
prevalence and **not** the `constraint_claim`/`claimed_natural` template slot OQ-70 implicates, and
cross-twin Spearman **0.8577** (RECON.md) — far more model-stable than the 0.392 relation call it
covaries against. It is NOT `cs_axiom_status`/drift (= Field C, under convention-suspicion this pass).

**Unit:** per slot (cid), `forecloses_fraction` = (# `forecloses` edges)/(# reading_relation edges),
per model; and that slot's authored `base_extractiveness`.

**Pinned decision rule:**
- **(a) Directionality:** paired (flash − haiku) `forecloses_fraction` over the 960 slots; sign test.
  "Directional lean" if two-sided p < 0.05.
- **(b) Covariation:** Spearman ρ(`forecloses_fraction`, `base_extractiveness`) per model, computed
  on the **disagreeing** slots and (positive control) the **agreeing** slots.
- **SIGNAL** if ρ ≥ +0.20 in **both** models on the disagreeing slots **AND** the agreeing-slot
  control reproduces ρ ≥ +0.20 (the relation call tracks the settled closure-degree substrate).
- **CONVENTION** if a directional lean exists (a) **AND** covariation fails (ρ < 0.20 in ≥1 model) —
  the split is a model default (e.g. one model defaults `coexists_with`) with no substrate covariation.
- **MIXED → OPEN** if both lean and covariation fire; **INDETERMINATE → OPEN** if neither.

**Positive control:** the agreeing ~39% must show the same covariation if SIGNAL (built into rule b).
**Threshold rationale:** ρ=0.20 and α=0.05 pinned here, pre-run.

**Phase-1 checkpoint (witnessed, not deferred):** `base_extractiveness` is confirmed
provenance-settled above (cross-twin ρ 0.86, authored Surface-1, outside the OQ-70 slot set). B does
not rest on directionality alone.

---

## Routing (per field, pinned)

- *signal* ⇒ the committer/observer-coded axis carries a model index; flag scope/index consequence
  for v8 §3 / OQ-72 invariant claims (no schema artifact built this pass).
- *convention* ⇒ the field needs a provenance bucket before it can be read as structure
  (precedent shape: stakeholder fix `becd0f87`); do not build it this pass.
- *OPEN* (Field C, or any field whose control does not fire) ⇒ record the graduation step; escalate
  the gated third-model spend only if any field returns *signal* (spend-go stays the operator's).
