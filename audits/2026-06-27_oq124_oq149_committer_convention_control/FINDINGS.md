# FINDINGS — OQ-124 + OQ-149 committer-axis convention control

**Date:** 2026-06-27. **Substrate:** twins re-classified at commit `bbf5c92`
(`pipeline_output.{haiku,flash}.json`, n=960); authored fields hash-pinned
(`testsets_haiku` `020a4dd4…`, `testsets_flash` `8e810cda…`). Every finding below cites tool
output (`recon_reproduce.json`, `recon_pin.json`, `execute_AB.json`, `retraction_control.log`),
not documentation. Decision rules + controls were committed before execution (`1beebc67`).

**Positive controls held** (the read is sound): `claimed_type` 0.7208, `cs_kernel_id` 1.000.

Per the operator ruling, each field is ruled separately — they did **not** return the same verdict.

---

## Field A (signature fork CHE↔FCR) — **SIGNAL** (dominant lean); minority counter-direction MIXED→OPEN

**The OQ-124 lean is real signal, not template convention.** The fork is strongly asymmetric
(`recon_reproduce.json`): `haiku=CHE / flash=FCR` = **157**, `haiku=FCR / flash=CHE` = **12**
(~13:1) — OQ-124's "both directions across many constraints" overstated the symmetry.

Decomposition (`execute_AB.json`, pinned rule: SOURCE1_ONLY share ≥ 0.5 ⇒ CONVENTION):

| direction | n | SOURCE1_ONLY | BOTH | SOURCE2_ONLY | source1-only share | CHE-ext > FCR-ext | verdict |
|---|---|---|---|---|---|---|---|
| haiku CHE / flash FCR (dominant) | 157 | **0** | 65 | 92 | **0.00** | **157/157 = 1.00** | **SIGNAL** |
| haiku FCR / flash CHE (minority) | 12 | 5 | 2 | 5 | 0.42 | 8/12 = 0.67 | MIXED→OPEN |

In the dominant direction **zero** forks ride the authored template slot alone: every flash-FCR
limb has min ε ≤ 0.45 (the rope ceiling), so `appears_as_rope` fires via `low_extraction_profile`
(source-2), and on **all 157** the haiku side authored higher `base_extractiveness` than flash.
The fork is a **threshold-crossing of a continuous, model-characteristic extraction-magnitude
difference**: flash authors systematically lower extraction (corpus-wide mean 0.508 vs haiku 0.565,
cross-twin Spearman 0.86 — `recon_pin.json`); on near-boundary slots flash drops below the rope
ceiling (→ FCR) while haiku stays above the snare floor 0.46 (→ CHE). The `constraint_claim(rope)`
template slot is present on 41% of these flash-FCR slots but is **never the driver** (source-2 is
already active), so retracting it changes nothing there.

**Bidirectional retraction control — discharged two-sided** (`retraction_control.log`,
`probe_harness:with_retracted/2`, verified restore):

- `human_dignity_ai_governance__techno_optimist_reading` (minority, SOURCE1_ONLY, ε 0.78):
  `appears_as_rope [explicit_rope_claim] → []`, `false_ci_rope yes → no` — **dropped** (predicted).
  Proves the probe works.
- `acceptable_risk_for_energy__expected_value_dominant` (dominant, BOTH, ε 0.30):
  `appears_as_rope [explicit_rope_claim] → [low_extraction_profile]`, `false_ci_rope yes → yes` —
  **survived** via source-2 (predicted). Confirms the dominant-direction FCR rides continuous
  low-extraction, not the template.

The retraction result and the decomposition agree by construction (no flagged disagreement).

The minority direction (12 slots) carries a genuine template component (5/12 SOURCE1_ONLY would
flip; magnitude share 0.67 < 0.80) and meets neither pinned bar — MIXED→OPEN, not ruled.

**Route:** *signal*. The signature lean carries a **model index** — the place a model's structural
coding of the same substrate is least situation-fixed is the extraction-magnitude calibration
(haiku reads more extraction, flash reads more false-rope). Flag the scope/index consequence for
committer/observer invariant claims (v8 §3, OQ-72). No schema artifact built this pass.

---

## Field B (`cs_reading_relation` 0.392) — **CONVENTION** (directional default, no substrate covariation)

Per-slot multiset agreement reproduced at **0.3917** (`recon_reproduce.json`). With Field C pulled
(below), B rested on its settled covariate alone: authored `base_extractiveness` (cross-twin
Spearman 0.86 — witnessed provenance-settled, outside the OQ-70 template-slot set).

Results (`execute_AB.json`, pinned ρ ≥ 0.20 in both models on both subsets ⇒ SIGNAL):

- **(a) Directionality — present.** flash `forecloses_fraction` 0.239 > haiku 0.206; paired
  (flash−haiku) sign test **p = 0.020** (185 pos / 142 neg of 327 nonzero). Flash systematically
  leans more foreclosing.
- **(b) Covariation — fails on disagreement.** Spearman(`forecloses_fraction`,
  `base_extractiveness`): **disagreeing** slots (n=584) ρ_haiku 0.156 / ρ_flash 0.162 (**both below
  0.20**); **agreeing** control slots (n=376) ρ_haiku 0.256 / ρ_flash 0.258 (above 0.20).

**Reading:** where the two models *agree* on the relation, the call tracks the closure-degree
substrate (ρ≈0.26); where they *disagree* (the ~60%), the split is **not** substrate-driven
(ρ≈0.16) — it is a model authoring default (flash defaults toward `forecloses`). Directional lean
∧ failed covariation ⇒ **CONVENTION** by the pinned rule.

**Route:** *convention*. The `forecloses`/`coexists_with` state structure (v8 §5) needs a
**provenance bucket** before per-slot relation values can be read as detected structure — the
forward-only stakeholder fix (`becd0f87`) is the precedent shape. Not built this pass.

---

## Field C (`cs_axiom_status overridden` 51-vs-4) — **OPEN-pending-instrumentation**

Phase-0 gate **NEGATIVE**: the per-slot coercion witness (split flash `holdable` into authored vs
coerced-from-contested/foreclosed/out-of-enum) does not survive the original flash generation run
(`run_no_scope_gemini` saves no raw responses; `repair_stats` aggregate-only;
`contested/foreclosed→holdable` remap silent; `json_flash/` post-repair — `RECON.md` step 4).
Recovering it ⇒ re-generation with instrumentation = the spend the operator ruled out. **Not ruled;
no weaker proxy substituted.**

**Decidable enrichment recorded for the follow-up:** `overridden` is **coercion-invariant** (valid
enums pass untouched, story_repair.py:87-90; missing status → generation FAIL, not a silent
`holdable` default, generate_constraint_pl.py:672). So the **51-vs-4 asymmetry is a real
model-authoring difference**, not the optional-field-drop artifact the plan hypothesized (that
mechanism does not exist for `cs_axiom_status`). Only the fine interpretation of flash's `holdable`
is undecidable; the headline asymmetry is settled and only the holdable fine-structure needs
raw-output capture.

**Route:** *OPEN-pending-instrumentation*. Graduation step: capture per-slot raw pre-repair model
status in the next flash(-class) generation run (instrument `story_repair._normalize_axiom_status`
to log `cid`), then re-run the C decision rule. Gated follow-up; spend stays the operator's.

---

## Cross-field consequence + third-model spend

- **OQ-124** (Field A): resolves toward *signal* on the dominant lean → the signature fork is
  seat/model-expressive on extraction calibration. Minority direction OPEN.
- **OQ-149** (Field B): *convention* on `reading_relation`; (Field C) OPEN-pending-instrumentation.
- **Third-model spend (OQ-124's named follow-up):** Field A returned *signal*, so the gated
  third-model / fresh-draw test is now **warranted** (the convention branch that would have
  collapsed "stable fingerprint" into "which template the model defaults to" did NOT fire for A).
  Escalate as a gated follow-up OQ; the spend-go and model choice stay the operator's.
