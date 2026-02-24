# Rope-Dominant Follow-Up: Reclassification, Diagnostics, and Config Cleanup

**Date:** 2026-02-24
**Scope:** Executes findings from the rope-dominant spot check (28 constraints).
**Commits:** Spec reclassifications (Part 1), config.pl subtype thresholds (Part 4b).

---

## Part 1: Reclassification Log

### Edits Made

| Constraint | File | Line | Old | New | Verification |
|---|---|---|---|---|---|
| `portuguese_presidential_term_limits` | `prolog/testsets/portuguese_presidential_term_limits.pl` | 75 | `tangled_rope` | `rope` | swipl consult: 0 errors |
| `thai_article_112_mountain` | `prolog/testsets/thai_article_112_mountain.pl` | 73 | `tangled_rope` | `rope` | swipl consult: 0 errors |
| `sts86_ascent_checklist` | `prolog/testsets/sts86_ascent_checklist.pl` | 56 | `tangled_rope` | `rope` | swipl consult: 0 errors |

**Predicate changed:** `narrative_ontology:constraint_claim(ID, tangled_rope)` to `narrative_ontology:constraint_claim(ID, rope)`.

### Skipped

**`decentralized_infrastructure_rope`** — No file exists in `prolog/testsets/`. The pipeline data for this constraint originates from `prolog/datasets/v5/new_civilizational_rope.pl` (human_readable: "The Auditable Bridge"). See Part 3 for the full missing-specs diagnostic.

### Scope Note

Each reclassified spec also contains `constraint_classification/3` entries that reference `tangled_rope` for some perspectives (e.g., portuguese lines 95, 113, 122; thai lines 86, 94; sts86 lines 124-133, 149-158). These were NOT changed per task scope ("Do NOT change anything else in the file"). These are authored perspective labels; the engine overrides them with computed types via `drl_core:dr_type/3` anyway. See Part 2a for why this distinction matters.

---

## Part 2: Quine Self-Replication Diagnostic

### Constraint Profile

| Property | Value |
|---|---|
| Spec file | `prolog/testsets/quine_self_replication.pl` |
| Claim | `tangled_rope` (line 109) |
| Base extractiveness (epsilon) | 0.20 |
| Suppression | 0.05 |
| Theater ratio | 0.01 |
| emerges_naturally | true |
| requires_active_enforcement | true |
| Beneficiaries | autonomous_agent_developers, computer_science_educators |
| Victims | cybersecurity_defenders, static_code_analysis_tools |

### Engine Classification Trace

**Classification cascade:** `drl_core.pl` `classify_from_metrics/6` (lines 300-385)

The tangled_rope gate (lines 352-364) requires ALL of:

```prolog
classify_from_metrics(C, BaseEps, Chi, Supp, _Context, tangled_rope) :-
    \+ natural_law_without_beneficiary(C),
    Chi >= tangled_rope_chi_floor (0.40),       % FAIL: max chi = 0.274
    Chi =< tangled_rope_chi_ceil (0.90),
    BaseEps >= tangled_rope_epsilon_floor (0.30), % FAIL: epsilon = 0.20
    Supp >= tangled_rope_suppression_floor (0.40), % FAIL: supp = 0.05
    requires_active_enforcement(C),              % PASS
    has_coordination_function(C),                % PASS (has beneficiaries)
    has_asymmetric_extraction(C), !.             % PASS (has victims)
```

**All three numeric gates fail.** Binary gates pass but are insufficient.

### Per-Perspective Engine Output

| Perspective | Chi | Engine Type | Gate |
|---|---|---|---|
| powerless | 0.217 | **mountain** | Supp <= 0.05, BaseEps <= 0.25, emerges_naturally, immutability(immediate, trapped) = mountain |
| moderate | 0.221 | **rope** | Chi <= 0.35, BaseEps <= 0.45, emerges_naturally bypasses immutability |
| institutional | -0.009 | **rope** | Chi <= 0 → skip epsilon check, emerges_naturally |
| analytical | 0.274 | **mountain** | Same as powerless (immutability from civilizational+analytical context) |

### Spec vs Engine Comparison

| Perspective | Spec `constraint_classification/3` | Engine `dr_type/3` |
|---|---|---|
| powerless | mountain (line 138) | mountain |
| institutional | rope (line 148) | rope |
| moderate | tangled_rope (line 160) | rope |
| analytical | tangled_rope (line 170) | mountain |

The spec's authored moderate and analytical perspectives claim tangled_rope, but the engine overrides to rope and mountain respectively. The `constraint_claim` of tangled_rope matches neither any engine perspective nor the modal engine type.

### Diagnosis

**Root cause: Stale claim.** Same as Part 2a — the spec author manually declared `constraint_claim(..., tangled_rope)` with the rationale (spec lines 205-211) that "the analytical view must account for all its potential functions, both benign and extractive." But the engine's numeric gates reject tangled_rope for this constraint because epsilon (0.20), chi (max 0.274), and suppression (0.05) are all below the respective floors.

This is NOT a gate bug — the gate correctly identifies that quine's extraction is too low for tangled_rope. The constraint is a mountain/rope hybrid: a mathematical necessity (mountain for the code/analyst) that serves as a coordination tool (rope for educators/institutions).

### Recommendation

**Reclassify `constraint_claim` from `tangled_rope` to `rope`.**

The mountain/rope perspectival variance is correctly captured by the engine's indexed classifications. The tangled_rope claim is a spec authoring artifact that predates the current gate thresholds. However, as an Investigate-tier constraint (epsilon 0.20, above the 0.10 reclassify threshold), this requires human review before implementation.

---

## Part 2a: Epsilon Floor Gate Diagnostic

### The Question

`config.pl` defines `param(tangled_rope_epsilon_floor, 0.30)`, referenced in 15+ files. Yet 11 of the 28 rope-dominant constraints have epsilon below 0.30 (the 4 Reclassify at 0.02-0.08 and 7 Investigate at 0.10-0.20). Why didn't the existing gate prevent their tangled_rope classification?

### The Answer

**The gate IS working correctly.** The distinction is between two independent classification paths:

| Pipeline field | Source predicate | Source file | Governed by gate? |
|---|---|---|---|
| `claimed_type` | `narrative_ontology:constraint_claim/2` | Spec file (manual) | **No** — human-authored |
| `perspectives.*` | `drl_core:dr_type/3` | Engine (computed) | **Yes** — gate fires |

The `tangled_rope_epsilon_floor` gate governs `drl_core:classify_from_metrics/6` (line 358-359), which produces the engine-computed perspective classifications. These appear in `enriched_pipeline.json` as the `perspectives` object (`json_report.pl` line 472: `drl_core:dr_type(C, Ctx, Type)`).

The `claimed_type` field comes from `narrative_ontology:constraint_claim/2` (`json_report.pl` line 190) — a manually-authored label in each spec file. The gate has no jurisdiction over it.

**Root cause:** The spec files' `constraint_claim` values predate the gate calibration. The gate was calibrated to 0.30, but nobody retroactively updated the specs whose manual claims contradicted the engine output.

### Scope of Claim/Engine Mismatch

**418 of 1,151 constraints (36.3%) have `claimed_type` != modal perspective type.**

| claimed_type | modal type | Count |
|---|---|---|
| tangled_rope | snare | **295** |
| snare | tangled_rope | 28 |
| rope | tangled_rope | 26 |
| scaffold | tangled_rope | 13 |
| tangled_rope | rope | 13 |
| piton | snare | 11 |
| scaffold | rope | 6 |
| rope | mountain | 6 |
| piton | rope | 5 |
| tangled_rope | naturalized | 5 |
| piton | tangled_rope | 4 |
| mountain | rope | 2 |
| tangled_rope | mountain | 2 |
| rope | snare | 1 |
| naturalized | rope | 1 |

**Key finding:** The dominant mismatch is 295 constraints claimed as `tangled_rope` whose 4 perspectives modal to `snare` (typically 3 perspectives see snare, institutional sees rope). This is a systematic undercount of snare and overcount of tangled_rope in the corpus.

**Distribution shift (claimed vs modal):**

| Type | Claimed | Modal | Delta |
|---|---|---|---|
| snare | 78 | 357 | **+279** (massively underclaimed) |
| tangled_rope | 752 | 509 | **-243** (massively overclaimed) |
| scaffold | 21 | 3 | -18 (overclaimed) |
| piton | 95 | 76 | -19 (overclaimed) |
| rope | 129 | 132 | +3 (approximately balanced) |
| mountain | 76 | 74 | -2 (approximately balanced) |

### Recommendation

This is a **separate project** — 418 claim updates is not a patch to the current task. Recommended next steps:

1. **Batch claim reconciliation:** For each constraint where claimed_type != modal(perspectives), the `constraint_claim/2` should be updated to match the engine's modal output. This can be scripted.
2. **Priority:** The 295 tangled_rope-claimed-but-snare-modal constraints are the highest-impact batch. Updating these would correct the corpus type distribution significantly.
3. **The 3 reclassifications in Part 1 are correct** — they're the low-epsilon subset of the 13 tangled_rope→rope mismatches. The Part 1 fixes address the symptom for these 3 constraints; the batch reconciliation would address the systemic cause.

---

## Part 3: Missing Specs Diagnostic

### Findings

| Constraint ID (pipeline) | File Found | Location | Classification |
|---|---|---|---|
| `large_cardinal_foundations` | `large_cardinals_foundations.pl` | `prolog/testsets/` | **MISNAMED** — plural "cardinals" vs singular "cardinal" in constraint ID |
| `migration_decision_threshold` | `rotmigration_decision_threshold.pl` | `prolog/testsets/` | **MISNAMED** — "rot" prefix in filename not in constraint ID |
| `dexy_gold_protocol` | `ergo_dexy_gold_protocol.pl` | `prolog/testsets/` | **MISNAMED** — "ergo_" prefix in filename not in constraint ID |
| `decentralized_infrastructure_rope` | `new_civilizational_rope.pl` | `prolog/datasets/v5/` **only** | **WRONG DIRECTORY + MISNAMED** |

### Explanation

All 4 constraints exist in the pipeline via their **module-internal constraint IDs**, which are defined inside the Prolog files and differ from the filenames. The pipeline consults modules (`:- module(constraint_NAME, []).`), not filenames. The "missing" label in the spot check came from searching by constraint ID as filename.

### Details

**`large_cardinal_foundations`**: File `large_cardinals_foundations.pl` (note plural). Also exists in `prolog/datasets/v5/`. Epsilon 0.30, suppression 0.40 — Keep tier per spot check.

**`migration_decision_threshold`**: File `rotmigration_decision_threshold.pl` (has "rot" prefix — likely a generation artifact). Also exists in `prolog/datasets/v5/`. Epsilon 0.30, suppression 0.40 — Keep tier per spot check.

**`dexy_gold_protocol`**: File `ergo_dexy_gold_protocol.pl` (has "ergo_" prefix from Ergo blockchain context). Also exists in `prolog/datasets/v5/`. Epsilon 0.20, suppression 0.20 — Investigate tier per spot check.

**`decentralized_infrastructure_rope`**: File `new_civilizational_rope.pl` in `prolog/datasets/v5/` only. Human-readable: "The Auditable Bridge". Epsilon 0.08, suppression 0.25 — **Reclassify tier** per spot check. This is the one constraint from the reclassification list (Part 1) that couldn't be edited because it has no `prolog/testsets/` copy.

### Recommendation

- The 3 testsets-present constraints (large_cardinal, migration, dexy) need no action — they work correctly despite filename mismatches.
- `decentralized_infrastructure_rope` needs either: (a) a copy into `prolog/testsets/` with the correct filename, or (b) reclassification of the claim in its current location (`prolog/datasets/v5/new_civilizational_rope.pl`).

---

## Part 4: Config.pl Changes

### 4a: Epsilon Floor Parameter — SKIPPED

No new epsilon floor parameter added. The existing `param(tangled_rope_epsilon_floor, 0.30)` is the correct classification engine gate. The 28 rope-dominant constraints' tangled_rope status came from stale `constraint_claim/2` labels, not from a missing gate. See Part 2a for the full diagnostic.

### 4b: Subtype Thresholds — ADDED

Added to `prolog/config.pl` (after line 192, before Scaffold Boundaries):

```prolog
% --- Gradient Subtype Thresholds ---
% Classification thresholds for tangled_rope subtype analysis.
% Used by tangled_gradient.py and chi_variance_decomposition.py.
% Constraints with max(g_chi) < subtype_rope_threshold are rope-dominant.
% Constraints with min(g_chi) > subtype_snare_threshold are snare-dominant.
param(subtype_rope_threshold, 0.30).
param(subtype_snare_threshold, 0.70).
```

Added to `prolog/config_schema.pl` (after tangled_rope_epsilon_floor spec):

```prolog
param_spec(subtype_rope_threshold,     number, range(0.0, 1.0), "Max g_chi for rope-dominant tangled_rope subtype").
param_spec(subtype_snare_threshold,    number, range(0.0, 1.0), "Min g_chi for snare-dominant tangled_rope subtype").
```

Verification: `swipl -g "consult('config.pl'), halt."` — 0 errors.

### 4c: Dead Parameter Audit

**27 parameters** defined in config.pl have no references in executable source code (prolog/*.pl or python/*.py) outside of config.pl and config_schema.pl.

#### Truly Dead (no refs anywhere outside config files)

| Parameter | Section | Value |
|---|---|---|
| `exit_metric_name` | 1 (Metric Naming) | `exit_options` |
| `power_metric_name` | 1 (Metric Naming) | `agent_power` |
| `scope_metric_name` | 1 (Metric Naming) | `spatial_scope` |
| `temporal_metric_name` | 1 (Metric Naming) | `time_horizon` |

#### Docs-Only (referenced in docs/ but not in source code)

| Parameter | Section | Value | Doc refs |
|---|---|---|---|
| `boltzmann_factorization_tolerance` | 7 (Boltzmann) | 0.10 | 4 docs |
| `boltzmann_floor_drift_threshold` | 7 (Boltzmann) | 0.05 | 4 docs |
| `constructed_beneficiary_min` | 6 (Structural Sig) | 2 | 2 docs |
| `constructed_resistance_min` | 6 (Structural Sig) | 0.20 | 2 docs |
| `constructed_suppression_min` | 6 (Structural Sig) | 0.20 | 2 docs |
| `contamination_strength_mountain` | 9 (Purity Network) | 0.0 | 3 docs |
| `contamination_strength_naturalized` | 9 (Purity Network) | 0.3 | 1 doc |
| `contamination_strength_piton` | 9 (Purity Network) | 0.8 | 3 docs |
| `contamination_strength_rope` | 9 (Purity Network) | 0.1 | 3 docs |
| `contamination_strength_scaffold` | 9 (Purity Network) | 0.2 | 3 docs |
| `contamination_strength_snare` | 9 (Purity Network) | 1.0 | 3 docs |
| `contamination_strength_tangled_rope` | 9 (Purity Network) | 0.5 | 3 docs |
| `data_medium_threshold` | 5 (Intent) | 0.75 | 3 docs |
| `loser_loss_max_gain` | 5 (Intent) | 0.10 | 1 doc |
| `mountain_extractiveness_min` | 5 (DR) | 0.0 | 3 docs |
| `network_cluster_degraded_floor` | 9 (Purity Network) | 0.40 | 3 docs |
| `network_contamination_risk_threshold` | 9 (Purity Network) | 2 | 3 docs |
| `network_shared_agent_min` | 9 (Purity Network) | 1 | 3 docs |
| `reformability_high_threshold` | 7 (Boltzmann) | 0.70 | 3 docs |
| `reformability_low_threshold` | 7 (Boltzmann) | 0.30 | 3 docs |
| `rope_extractiveness_min` | 5 (DR) | 0.0 | 3 docs |
| `snare_extraction_ceil` | 5 (DR) | 1.00 | 4 docs |
| `tangled_rope_suppression_ceil` | 5 (DR) | 1.00 | 2 docs |

**Notable pattern:** The `contamination_strength_*` params (7 params) are dead in config.pl because `drl_purity_network.pl` hardcodes the same values in `type_contamination_strength/2` (lines 138-145) instead of reading from config. The config params were likely intended for configurability but the implementation bypassed them.

### 4d: Hardcoded Duplicate Flagging

#### Subtype Thresholds (should read from new config params)

| File | Line | Hardcoded | Config param (new) | Match? |
|---|---|---|---|---|
| `python/tangled_gradient.py` | 181, 183, 185 | 0.30, 0.70 | `subtype_rope_threshold`, `subtype_snare_threshold` | Match |
| `python/chi_variance_decomposition.py` | 61-62 | 0.30, 0.70 | `subtype_rope_threshold`, `subtype_snare_threshold` | Match |
| `python/rope_dominant_spot_check.py` | 62 | 0.30 | `subtype_rope_threshold` | Match |

#### Existing Config Params — Hardcoded Instead of Read

| File | Line | Hardcoded | Config param | Config value | Match? |
|---|---|---|---|---|---|
| `python/scaffold_piton_gate_audit.py` | 60 | `TANGLED_ROPE_EPSILON_FLOOR = 0.30` | `tangled_rope_epsilon_floor` | 0.30 | **Match** |
| `python/coordination_vitality_diagnostic.py` | 62 | `TANGLED_ROPE_EPSILON_FLOOR = 0.30` | `tangled_rope_epsilon_floor` | 0.30 | **Match** |
| `python/coordination_vitality_diagnostic.py` | 60 | `PITON_THEATER_FLOOR = 0.70` | `piton_theater_floor` | 0.70 | **Match** |
| `python/scaffold_piton_gate_audit.py` | 54 | `PITON_THEATER_FLOOR = 0.70` | `piton_theater_floor` | 0.70 | **Match** |
| `python/linter.py` | 68 | default 0.70 | `piton_theater_floor` | 0.70 | **Match** |

#### VALUE MISMATCHES (config divergence)

| File | Line | Hardcoded | Config param | Config value | Issue |
|---|---|---|---|---|---|
| `python/coordination_vitality_diagnostic.py` | 59 | `SCAFFOLD_EXTRACTION_CEIL = 0.30` | `scaffold_extraction_ceil` | **0.45** | **MISMATCH** — config raised from 0.30 to 0.45 in v7.0 |
| `python/scaffold_piton_gate_audit.py` | 51 | `SCAFFOLD_EXTRACTION_CEIL = 0.30` | `scaffold_extraction_ceil` | **0.45** | **MISMATCH** — same stale value |
| `python/coordination_vitality_diagnostic.py` | 58 | `PITON_EXTRACTION_CEILING = 0.25` | `piton_extraction_ceiling` | **0.45** | **MISMATCH** — config raised from 0.25 to 0.45 in v7.0 |
| `python/scaffold_piton_gate_audit.py` | 52 | `PITON_EXTRACTION_CEILING = 0.25` | `piton_extraction_ceiling` | **0.45** | **MISMATCH** — same stale value |

#### Contamination Strength Hardcoding (Prolog-internal)

| File | Lines | Hardcoded values | Config params | Match? |
|---|---|---|---|---|
| `prolog/drl_purity_network.pl` | 138-145 | snare=1.0, piton=0.8, tangled_rope=0.5, scaffold=0.2, rope=0.1, mountain=0.0, naturalized=0.3 | `contamination_strength_*` | **Values match** but code doesn't read config |

---

## Part 5: Re-export Pipeline Documentation

### Pipeline Generation Chain

```
Prolog spec files (prolog/testsets/*.pl)
  ├── pipeline_output.json    (via json_report.pl — reads constraint_claim/2)
  ├── orbit_data.json         (Prolog orbit/coalition analysis)
  └── abductive_data.json     (Prolog abductive engine)
           │
    enrich_pipeline_json.py   (Python — merges all three + adds confidence)
           │
    enriched_pipeline.json    (final 6.9MB output, 1,151 constraints)
```

### Stages Affected by Part 1 Reclassifications

| Stage | File | Impact | Re-run needed? |
|---|---|---|---|
| Prolog export | `pipeline_output.json` | 3 constraints change claimed_type from tangled_rope to rope | **Yes** |
| Enrichment | `enriched_pipeline.json` | claimed_type field updates propagate | **Yes** (downstream) |
| Tangled gradient | `tangled_gradient_data.json` | Candidate pool drops from 28 to 25 rope-dominant | **Yes** |
| Chi variance | `chi_variance_decomposition_data.json` | Same subset change | **Yes** |
| Spot check | `rope_dominant_spot_check_data.json` | 3 reclassified constraints exit candidate pool | **Yes** |
| Orbit analysis | `orbit_data.json` | Minimal — orbit families don't depend on claimed_type | Likely no |
| Abductive | `abductive_data.json` | Minimal — triggers based on structural metrics | Likely no |

### Recommended Re-run Sequence

Do NOT re-run yet. Wait until:
1. Quine reclassification decision is made (adds 1 more if approved)
2. `decentralized_infrastructure_rope` spec location is resolved
3. (Optional) Batch claim reconciliation scope is decided

Then:

```bash
# 1. Regenerate Prolog pipeline output (includes all spec changes)
make pipeline_output

# 2. Regenerate enriched pipeline
make enriched_pipeline

# 3. Re-run tangled gradient analysis (new constraint count)
python python/tangled_gradient.py

# 4. Re-run chi variance decomposition
python python/chi_variance_decomposition.py

# 5. Re-run rope-dominant spot check (validate reclassifications)
python python/rope_dominant_spot_check.py
```

The tangled_rope count in the corpus drops from 752 (claimed) to 749 with these 3 reclassifications. If the batch reconciliation in Part 2a is approved, the corpus type distribution would shift significantly (752 tangled_rope → ~509, 78 snare → ~357).

---

## Summary of Actions

| Item | Status | Files changed |
|---|---|---|
| Part 1: Reclassify 3 specs | **Done** | 3 spec files (1 line each) |
| Part 1: decentralized_infrastructure_rope | **Skipped** | No testsets file exists |
| Part 2: Quine diagnostic | **Documented** | No file changes (Investigate tier) |
| Part 2a: Gate diagnostic | **Documented** | No file changes (systemic finding) |
| Part 3: Missing specs | **Documented** | No file changes (files found under different names) |
| Part 4a: Epsilon floor param | **Skipped** | Existing gate sufficient |
| Part 4b: Subtype thresholds | **Done** | config.pl, config_schema.pl |
| Part 4c: Dead param audit | **Documented** | No file changes (27 dead/docs-only params flagged) |
| Part 4d: Hardcoded duplicates | **Documented** | No file changes (4 value mismatches flagged) |
| Part 5: Re-run sequence | **Documented** | No file changes |
