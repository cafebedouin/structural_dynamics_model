# Generation Pipeline Compatibility Audit

**Date:** February 15, 2026
**Scope:** Generation prompt (v6.0), template, structural linter, vs. full classification + diagnostic engine
**Method:** Systematic cross-reference of every fact consumed by every Prolog module against template slots, prompt instructions, and linter checks
**Corpus:** 1,025 constraint story files in `prolog/testsets/`

---

## Summary

| Metric | Count |
|--------|-------|
| **Facts required by engine** | 21 distinct predicates |
| **Facts in template** | 18 (coverage: 86%) |
| **Facts explained in prompt** | 18 (coverage: 86%) |
| **Facts checked by linter** | 14 (coverage: 67%) |
| **TEMPLATE_MISSING** | 3 |
| **PROMPT_MISSING** | 0 |
| **TEMPLATE_ONLY** | 0 |
| **SILENT_DEGRADATION defaults** | 9 locations across 7 modules |
| **Stale threshold references in prompt** | 0 |
| **Stale threshold references in linter** | 0 (hardcoded values match config.pl) |
| **omega_variable arity mismatch** | 1 (template /5 vs engine /3) |

---

## Critical Gaps

### CG-1. `omega_variable` arity mismatch — template produces /5, engine reads /3

**Template (line 258-265):** declares `omega_variable/5`:
```prolog
omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
```

**Engine (`narrative_ontology.pl` line 14, 56, 68):** exports and validates `omega_variable/3`:
```prolog
omega_variable(ID, Type, Desc).
```

The template generates 5-argument facts. The reporting engine (`report_generator.pl` lines 260, 418, 567, 739, 751, 756, 760, 769, 781) reads 3-argument facts from `narrative_ontology:omega_variable/3`. These are structurally different predicates with different semantics:

- `/5`: (ID, Question, Resolution, Impact, Confidence) — narrative detail
- `/3`: (ID, TypeClass, Description) — typed classification (empirical, conceptual, preference)

**Impact:** `omega_variable/5` facts in constraint files are module-local (not in `narrative_ontology:` namespace). The report generator reads `narrative_ontology:omega_variable/3`, which these don't populate. The bridge between `/5` and `/3` appears to happen via `data_repair.pl` assertion, but generated files produce `/5` facts that are invisible to the reporting engine unless bridged.

**Current corpus status:** 1,022/1,025 files declare `omega_variable` — in `/5` form. The report generator works because it asserts bridged `/3` forms during processing. But this is fragile: the template/prompt produce one format, the engine consumes another, and an assertion bridge papers over the gap.

**Classification:** SILENT_DEGRADATION — engine runs without error but omega analysis may be incomplete for files not processed through the bridge.

### CG-2. `get_metric_average` defaults to 0.5 for missing metrics

**Location:** `structural_signatures.pl:181`
```prolog
get_metric_average(C, MetricType, Average) :-
    findall(Val, narrative_ontology:constraint_metric(C, MetricType, Val), Vals),
    (   Vals \= []
    ->  ...
    ;   Average = 0.5  % Default if no data
    ).
```

**Impact on Mountains:** The `natural_law_signature` certification chain (structural_signatures.pl:157-159) calls:
- `get_metric_average(C, accessibility_collapse, AccessCollapse)` — needs ≥ 0.85
- `get_metric_average(C, resistance, Resistance)` — needs ≤ 0.15

When these metrics are missing (default → 0.5), accessibility_collapse fails (0.5 < 0.85) and resistance fails (0.5 > 0.15). The constraint gets classified as mountain by the metric gate (if `emerges_naturally` is set) but fails natural_law_signature certification.

**Current corpus:** Only 112/1,025 files have `accessibility_collapse` and 113 have `resistance`. Of the 201 files with `emerges_naturally`, ~89 lack NL profile metrics and silently fail signature certification.

**Classification:** SILENT_DEGRADATION — mountains are classified correctly by the metric gate but get weaker/incorrect structural signatures.

### CG-3. `classify_snapshot` defaults to 0.5 for missing metrics

**Location:** `drl_lifecycle.pl:891-901`
```prolog
classify_snapshot(C, Time, Type) :-
    (   metric_at(C, base_extractiveness, Time, E)
    ->  true
    ;   safe_metric(C, extractiveness, E)
    ->  true
    ;   E = 0.5
    ),
    (   metric_at(C, suppression_requirement, Time, S)
    ->  true
    ;   safe_metric(C, suppression_requirement, S)
    ->  true
    ;   S = 0.5
    ).
```

**Impact:** Historical type reconstruction for drift detection uses extractiveness default 0.5 (5x the config `default_extractiveness` of 0.10). A constraint with no extractiveness data would be snapshot-classified with `E=0.5`, `Chi=0.575` (at analytical), potentially producing false drift events when compared against its actual classification.

**Note:** The divergence audit (M5) flagged this as an implementation inconsistency. Unresolved.

**Classification:** SILENT_DEGRADATION — drift detection may produce phantom events for constraints lacking temporal metric data.

---

## Diagnostic Gaps

### DG-1. Boltzmann factorizability — no file-level gaps

The Boltzmann compliance engine (`structural_signatures.pl`) reads:
- `constraint_metric/3` (extractiveness, suppression) — **in template** ✓
- `coordination_type/2` — **in template** ✓ (optional, commented)
- `boltzmann_floor_override/2` — **in template** ✓ (optional, commented)
- `constraint_claim/2` — **in template** ✓

Cross-index coupling is computed internally from `dr_type/3` evaluations across Power × Scope grid. No additional file-level facts required.

**Corpus coverage:** 702/1,025 files declare `coordination_type`; 95 declare `boltzmann_floor_override`. Files without `coordination_type` use `complexity_offset_default` (0.00) and `boltzmann_floor_default` (0.05).

### DG-2. Dirac orbit classification — purely computational, no file-level gaps

Orbit computation calls `dr_type/3` across standard contexts. It reads `constraint_metric(C, theater_metric, TR)` for gauge freedom descriptions and `derived_from/3` (a multifile hook for constraint derivation chains, rarely used).

No file-level facts beyond the core metrics are required. **No gap.**

### DG-3. MaxEnt shadow classifier — theater_ratio default

**Reads from files:**
- `base_extractiveness/2` via `drl_core` — defaults to 0.0 if missing
- `suppression_score/2` via `drl_core` — defaults to 0.0 if missing
- Theater metric via `constraint_metric/3` — **defaults to 0.0 if missing**
- Boolean features: `emerges_naturally`, `requires_active_enforcement`, `has_coordination_function`, `has_asymmetric_extraction`, `natural_law_without_beneficiary` — all derived from file-level facts

**Gap:** 27 files lack `theater_ratio` metric. MaxEnt defaults to 0.0, which biases the Gaussian log-likelihood toward mountain/rope types (low theater). This is correct behavior for most missing cases but could misclassify a piton without theater data.

**Classification:** Minor SILENT_DEGRADATION — affects MaxEnt probability distributions for 27 files.

### DG-4. Abductive synthesis — no additional file-level requirements

The 8 trigger classes read outputs from other subsystems:
1. Signature override mismatches — from `structural_signatures`
2. MaxEnt disagreement — from `maxent_classifier`
3. FPN contamination divergence — from `drl_modal_logic`
4. Drift signal anomalies — from `drl_lifecycle`
5. Dirac orbit anomalies — from `dirac_classification`
6. Logical fingerprint voids — from `logical_fingerprint`
7. Cross-functor consistency — from multiple modules
8. Boltzmann anomalies — from `structural_signatures`

All trigger classes read computed outputs, not raw file facts. **No direct file-level gap.** But abductive quality degrades proportionally to the quality of upstream diagnostics (which are affected by CG-2, CG-3, DG-3 above).

### DG-5. Trajectory mining — no additional file-level requirements

Trajectory computation builds 7-component vectors from:
- `dr_type/3` evaluations (type shift pattern)
- Shannon entropy from MaxEnt
- Coupling score from Boltzmann
- Drift events from lifecycle
- Purity from signatures
- Fingerprint voids from logical_fingerprint

All inputs are computed, not file-read. **No direct file-level gap.**

### DG-6. FPN contamination network — edge discovery from `affects_constraint/2`

The contamination network requires:
- `affects_constraint/2` for explicit edges — **in template** ✓ (optional, commented)
- Inferred edges from shared agents (coupling strength) — computed from `constraint_beneficiary/2` and `constraint_victim/2`
- Purity scores — computed from signatures

**Corpus coverage:** 460/1,025 files declare `affects_constraint`. Files without network edges have no contamination propagation (FPN treats them as isolated nodes). This is correct behavior but limits network analysis.

**Gap:** The prompt explains `affects_constraint` well but marks it as optional. For constraints in known regulatory clusters or decomposed families, omitting network edges is a quality gap. No way to lint for this — it requires domain knowledge.

### DG-7. Grothendieck cohomology — purely computational

Reads from orbit data (`dirac_classification`). No file-level facts. Currently disabled (`cohomology_enabled = 0`). **No gap.**

---

## Stale References

### No stale threshold references found in the generation prompt.

The prompt (v6.0) uses the following threshold values, all of which match current `config.pl`:

| Prompt Reference | Prompt Value | config.pl Value | Status |
|-----------------|-------------|-----------------|--------|
| Mountain ε ceiling | ≤ 0.25 | `mountain_extractiveness_max = 0.25` | MATCH |
| Mountain suppression | ≤ 0.05 | `mountain_suppression_ceiling = 0.05` | MATCH |
| Rope χ ceiling | ≤ 0.35 | `rope_chi_ceiling = 0.35` | MATCH |
| Rope ε ceiling | ≤ 0.45 | `rope_epsilon_ceiling = 0.45` | MATCH |
| Tangled Rope χ range | 0.40–0.90 | `tangled_rope_chi_floor/ceil = 0.40/0.90` | MATCH |
| Tangled Rope ε floor | ≥ 0.30 | `tangled_rope_epsilon_floor = 0.30` | MATCH |
| Tangled Rope suppression | ≥ 0.40 | `tangled_rope_suppression_floor = 0.40` | MATCH |
| Snare χ floor | ≥ 0.66 | `snare_chi_floor = 0.66` | MATCH |
| Snare ε floor | ≥ 0.46 | `snare_epsilon_floor = 0.46` | MATCH |
| Snare suppression | ≥ 0.60 | `snare_suppression_floor = 0.60` | MATCH |
| Scaffold χ ceiling | ≤ 0.30 | `scaffold_extraction_ceil = 0.30` | MATCH |
| Piton χ ceiling | ≤ 0.25 | `piton_extraction_ceiling = 0.25` | MATCH |
| Piton ε floor | > 0.10 | `piton_epsilon_floor = 0.10` | MATCH |
| Piton theater floor | ≥ 0.70 | `piton_theater_floor = 0.70` | MATCH |
| NL accessibility_collapse | ≥ 0.85 | `natural_law_collapse_min = 0.85` | MATCH |
| NL resistance | ≤ 0.15 | `natural_law_resistance_max = 0.15` | MATCH |

The 6 threshold divergences found in the logic divergence audit (C1-C6) affected the docs (`logic_thresholds.md`, `logic.md`) but **not** the generation prompt. The prompt was already aligned with the code values.

### No stale threshold references found in the linter.

The linter hardcodes threshold values rather than reading `config.pl` (except for metric names). All hardcoded values currently match:

| Linter Check | Hardcoded Value | config.pl Value | Status |
|-------------|----------------|-----------------|--------|
| Mandatrophy trigger | ext > 0.7 | (no config param) | N/A — linter-only |
| Omega required | ext > 0.46 | `snare_epsilon_floor = 0.46` | MATCH |
| Temporal data required | ext > 0.46 | `snare_epsilon_floor = 0.46` | MATCH |
| Scaffold danger zone | ext ≤ 0.30 | `scaffold_extraction_ceil = 0.30` | MATCH |
| Piton theater floor | theater < 0.70 | `piton_theater_floor = 0.70` | MATCH |
| Mountain ext conflict | ext > 0.25 | `mountain_extractiveness_max = 0.25` | MATCH |
| Mountain supp conflict | supp > 0.05 | `mountain_suppression_ceiling = 0.05` | MATCH |

**Risk:** If config.pl thresholds change, the linter will silently fall out of sync because it doesn't read them. These are not stale *now* but they're structurally fragile.

---

## Linter Gap Analysis

### Checks the linter performs (25 rules):

| # | Rule | Checks For | Category |
|---|------|-----------|----------|
| 1 | MISSING_MODULE | `:- module(` declaration | Generation |
| 2 | MISSING_HOOK | `interval/3` | Generation |
| 3 | OUTDATED_HOOK | `constraint_classification/3` | Generation |
| 4 | MISSING_PERSPECTIVE (powerless) | `agent_power(powerless)` | Generation |
| 5 | MISSING_PERSPECTIVE (institutional) | `agent_power(institutional)` | Generation |
| 6 | INVALID_SCOPE | spatial_scope atoms in valid set | Semantic |
| 7 | INSUFFICIENT_VARIANCE | ≥ 2 types across perspectives | Semantic |
| 8 | DEPRECATED_TERM | `noose` → `snare` | Generation |
| 9 | UNRESOLVED_MANDATROPHY | ext > 0.7 needs resolution | Semantic |
| 10 | MISSING_OMEGA | ext > 0.46 needs omega_variable | Template |
| 11 | MISSING_TEMPORAL_DATA | ext > 0.46 needs measurement/5 | Template |
| 12 | INSUFFICIENT_TEMPORAL_DATA | < 6 measurements | Template |
| 13 | MISSING_SUNSET_CLAUSE | scaffold + enforcement needs sunset | Template |
| 14 | SCAFFOLD_DANGER_ZONE | low-ext + beneficiary + no enforcement | Semantic |
| 15 | MISSING_THEATER_RATIO | piton needs theater_ratio | Template |
| 16 | LOW_THEATER_RATIO | piton theater < 0.70 | Semantic |
| 17 | MISSING_BENEFICIARY (tangled_rope) | tangled_rope needs beneficiary | Template |
| 18 | MISSING_VICTIM (tangled_rope) | tangled_rope needs victim | Template |
| 19 | MISSING_ENFORCEMENT (tangled_rope) | tangled_rope needs enforcement | Template |
| 20 | MISSING_BENEFICIARY (scaffold) | scaffold needs beneficiary | Template |
| 21 | MISSING_BENEFICIARY (non-mountain) | all non-mountain need beneficiary | Template |
| 22 | MISSING_VICTIM (snare) | snare needs victim | Template |
| 23 | MISSING_CLAIM | constraint_claim/2 required | Template |
| 24 | INVALID_CLAIM_TYPE | claim type in valid set | Semantic |
| 25 | INVALID_COORDINATION_TYPE | coordination_type in valid set | Semantic |
| 26 | SELF_REFERENCE | affects_constraint self-loop | Semantic |
| 27 | VARIABLE_IN_AFFECTS | non-ground target | Generation |
| 28 | INVALID_FLOOR_OVERRIDE | boltzmann_floor out of range | Semantic |
| 29 | FLOOR_EXCEEDS_EXTRACTION | floor > extractiveness | Semantic |
| 30 | INVALID_POWER_ATOM | directionality_override power | Semantic |
| 31 | INVALID_D_VALUE | directionality d out of range | Semantic |
| 32 | GENERIC_GROUP | placeholder group names | Generation |
| 33 | MISSING_MULTIFILE | directionality_override not in multifile | Generation |
| 34 | VACUOUS_TEST | unbound variable in test comparisons | Generation |
| 35 | DUPLICATE_MEASUREMENT | same (C, metric, time) different values | Semantic |
| 36 | REDUNDANT_MEASUREMENT | exact duplicates | Generation |
| 37 | MOUNTAIN_METRIC_CONFLICT | mountain claim vs metrics | Semantic |
| 38 | CONTEXT_ARITY | context() must have 4 args | Semantic |
| 39 | MISSING_NL_PROFILE | mountain missing NL metrics | Template |

### Gaps the linter does NOT check:

| Gap | Description | Risk |
|-----|-------------|------|
| **Missing `constraint_metric/3` entirely** | A file could have `domain_priors:base_extractiveness/2` but no `constraint_metric/3` facts. The engine reads via `constraint_metric/3` (through config metric name mapping). If `constraint_metric` is missing but `base_extractiveness` is present, the linter sees values via its regex but the engine cannot. | HIGH — classification silently fails to `unknown` |
| **`constraint_metric` vs `domain_priors` value mismatch** | Template has both `domain_priors:base_extractiveness` AND `constraint_metric(extractiveness)`. Linter checks either source but doesn't verify they match. | MEDIUM — engine uses `constraint_metric`, linter may validate against `domain_priors` |
| **Missing analytical perspective** | Linter checks for `powerless` and `institutional` but not `analytical`. The analytical perspective is required for `constraint_claim` derivation and is the default context. | MEDIUM — engine works but claim-vs-reality checks may not fire |
| **omega_variable arity** | Linter checks for presence of `omega_variable(` but doesn't verify arity (/5 expected by template, /3 by engine) | LOW — see CG-1 |
| **Stale `domain_priors:theater_ratio/2`** | Template includes standalone `domain_priors:theater_ratio/2`, but the engine reads theater via `constraint_metric(C, theater_ratio, V)`. If only domain_priors is declared, the engine won't find it. Linter accepts either. | LOW — 998/1025 files have constraint_metric(theater_ratio) |
| **`emerges_naturally` without mountain claim** | Linter only checks NL profile when `constraint_claim = mountain`. A file could declare `emerges_naturally` for a non-mountain, wasting the flag and confusing signature detection. | LOW — no classification error |
| **Network edge reciprocity** | No check that `affects_constraint(A, B)` has a corresponding entry or documentation in constraint B's file. | LOW — quality only |
| **Multifile block completeness** | Linter only checks for `directionality_override/3` in multifile. Does not verify `coordination_type/2`, `boltzmann_floor_override/2`, `affects_constraint/2`, `emerges_naturally/1`, `has_sunset_clause/1`, or `measurement/5` are in the multifile block when declared. | LOW — Prolog still loads but with warnings |

---

## Default Value Map

Every silent default in the engine, what triggers it, and whether the generation pipeline should be providing the missing data:

| Location | Missing Fact | Default | Affects | Should Template Provide? |
|----------|-------------|---------|---------|------------------------|
| `drl_core.pl:99` | `constraint_metric(C, suppression_requirement, V)` | `V = 0` | Classification: suppression treated as 0, blocking snare/tangled_rope gates | YES — template provides this |
| `structural_signatures.pl:181` | `constraint_metric(C, accessibility_collapse, V)` | `V = 0.5` | NL signature: fails certification (needs ≥ 0.85) | YES — template provides for mountains |
| `structural_signatures.pl:181` | `constraint_metric(C, resistance, V)` | `V = 0.5` | NL signature: fails certification (needs ≤ 0.15) | YES — template provides for mountains |
| `structural_signatures.pl:983` | `base_extractiveness(C, V)` | `V = 0.5` | Signature profile: mid-range default | YES — template provides this |
| `structural_signatures.pl:989` | `suppression_score(C, V)` | `V = 0` | Signature profile: zero suppression | YES — template provides this |
| `structural_signatures.pl:1430` | `excess_extraction(C, V)` | `V = 0.0` | CI_Rope certification: no excess = passes | NO — computed internally |
| `maxent_classifier.pl:235` | `base_extractiveness(C, V)` | `V = 0.0` | MaxEnt log-likelihood: biased toward mountain | YES — template provides this |
| `maxent_classifier.pl:236` | `get_raw_suppression(C, V)` | `V = 0.0` | MaxEnt log-likelihood: biased toward mountain | YES — template provides this |
| `maxent_classifier.pl:238` | `constraint_metric(C, theater_ratio, V)` | `V = 0.0` | MaxEnt log-likelihood: biased toward non-piton | YES — template provides this |
| `maxent_classifier.pl:550` | `constraint_metric(C, theater_ratio, V)` | `V = 0.0` | MaxEnt threshold proximity: no theater | YES — template provides this |
| `covering_analysis.pl:510` | `base_extractiveness(C, V)` | `V = 0.5` | Coverage analysis: mid-range | YES — template provides this |
| `covering_analysis.pl:519` | `suppression_score(C, V)` | `V = 0` | Coverage analysis: zero suppression | YES — template provides this |
| `gap_diagnostic.pl:121` | `base_extractiveness(C, V)` | `V = 0.5` | Diagnostic: mid-range | YES — template provides this |
| `gap_diagnostic.pl:128` | `suppression_score(C, V)` | `V = 0` | Diagnostic: zero suppression | YES — template provides this |
| `invertibility_analysis.pl:111-115` | All 3 core metrics | `V = 0.0` | Invertibility: zero base | YES — template provides this |
| `drl_lifecycle.pl:895-901` | `base_extractiveness` + `suppression_requirement` at time T | `0.5` each | Drift detection: false drift events possible | PARTIALLY — template provides measurements for ext > 0.46 |
| `drl_modal_logic.pl:272` | `measurement(_, C, suppression_requirement, Time, Supp)` | `Supp = 0.5` | Modal logic temporal eval: mid-range default | NO — only affects constraints with temporal analysis |
| `domain_priors.pl:64` | Any metric | `V = 0.5` (neutral prior) | Profile fallback: treated as novel domain | YES — template provides core metrics |

### Defaults that affect classification (SILENT_DEGRADATION):

1. **`drl_core.pl:99`** — suppression defaults to 0, which can prevent snare/tangled_rope classification. Triggered when `constraint_metric(C, suppression_requirement, V)` is missing. **1/1,025 files affected** (only `fnl_trace_diagnostic.pl` lacks `constraint_claim`; all others have suppression metrics).

2. **`structural_signatures.pl:181`** — NL metrics default to 0.5, failing mountain signature certification. **~89 files affected** (201 with `emerges_naturally` minus 112 with `accessibility_collapse`).

3. **`drl_lifecycle.pl:895-901`** — snapshot defaults to 0.5 extractiveness (5x config default). **Affects ~210 files** without temporal measurement data that are still subject to drift analysis.

---

## Fact Coverage Matrix

Complete list of facts the engine expects, with coverage status:

### Critical: Required for basic classification (`dr_type/3`)

| Fact | Arity | Template | Prompt | Linter | Corpus Coverage |
|------|-------|----------|--------|--------|----------------|
| `constraint_metric(C, extractiveness, V)` | /3 | ✓ | ✓ | ✓ (extracts value) | 1024/1025 (99.9%) |
| `constraint_metric(C, suppression_requirement, V)` | /3 | ✓ | ✓ | ✓ (extracts value) | 1024/1025 (99.9%) |
| `constraint_metric(C, theater_ratio, V)` | /3 | ✓ | ✓ | ✓ (piton check) | 998/1025 (97.4%) |
| `constraint_classification(C, Type, Context)` | /3 | ✓ | ✓ | ✓ (presence + type) | 1024/1025 (99.9%) |
| `constraint_claim(C, Type)` | /2 | ✓ | ✓ | ✓ (rule 23) | 1024/1025 (99.9%) |
| `emerges_naturally(C)` | /1 | ✓ | ✓ | ✓ (NL profile) | 201/1025 (19.6%) |
| `requires_active_enforcement(C)` | /1 | ✓ | ✓ | ✓ (tangled_rope) | 885/1025 (86.3%) |
| `constraint_beneficiary(C, Group)` | /2 | ✓ | ✓ | ✓ (rule 21) | 978/1025 (95.4%) |
| `constraint_victim(C, Group)` | /2 | ✓ | ✓ | ✓ (rule 22) | 971/1025 (94.7%) |
| `has_sunset_clause(C)` | /1 | ✓ | ✓ | ✓ (scaffold) | 146/1025 (14.2%) |

### Diagnostic: Required for full diagnostic stack

| Fact | Arity | Template | Prompt | Linter | Corpus Coverage |
|------|-------|----------|--------|--------|----------------|
| `constraint_metric(C, accessibility_collapse, V)` | /3 | ✓ | ✓ | ✓ (NL profile) | 112/1025 (10.9%) |
| `constraint_metric(C, resistance, V)` | /3 | ✓ | ✓ | ✓ (NL profile) | 113/1025 (11.0%) |
| `measurement(ID, C, Metric, Time, V)` | /5 | ✓ | ✓ | ✓ (rule 11-12) | 815/1025 (79.5%) |
| `coordination_type(C, Type)` | /2 | ✓ | ✓ | ✓ (type validation) | 702/1025 (68.5%) |
| `boltzmann_floor_override(C, V)` | /2 | ✓ | ✓ | ✓ (range check) | 95/1025 (9.3%) |
| `directionality_override(C, Power, D)` | /3 | ✓ | ✓ | ✓ (rules 30-31) | 74/1025 (7.2%) |

### Network: Required for FPN and coupling analysis

| Fact | Arity | Template | Prompt | Linter | Corpus Coverage |
|------|-------|----------|--------|--------|----------------|
| `affects_constraint(Source, Target)` | /2 | ✓ | ✓ | ✓ (self-ref check) | 460/1025 (44.9%) |

### Metadata: Used for reporting, integration, or validation

| Fact | Arity | Template | Prompt | Linter | Corpus Coverage |
|------|-------|----------|--------|--------|----------------|
| `interval(C, Start, End)` | /3 | ✓ | ✓ | ✓ (rule 2) | ~1024/1025 |
| `omega_variable(...)` | /5 in template, /3 in engine | ✓ (as /5) | ✓ (as /5) | ✓ (presence) | 1022/1025 (99.7%) |
| `base_extractiveness(C, V)` | /2 | ✓ | ✓ | ✗ (uses metric/3) | ~1024/1025 |
| `suppression_score(C, V)` | /2 | ✓ | ✓ | ✗ (uses metric/3) | ~1024/1025 |
| `theater_ratio(C, V)` | /2 | ✓ | ✓ | ✗ (uses metric/3) | ~998/1025 |

### Not in template but consumed by engine

| Fact | Arity | Template | Prompt | Linter | Notes |
|------|-------|----------|--------|--------|-------|
| `entity(ID, Type)` | /2 | ✗ | ✗ | ✗ | narrative_ontology internal; rarely used by testsets |
| `event(ID, Time, Actor, Type)` | /4 | ✗ | ✗ | ✗ | narrative_ontology internal; rarely used by testsets |
| `coupling_profile(C, Profile)` | /2 | ✗ | ✗ | ✗ | Declared multifile but not actively consumed |
| `input_vector(C, Vector)` | /2 | ✗ | ✗ | ✗ | Declared multifile but not actively consumed |
| `intent_viable_alternative/3` | /3 | ✗ | ✗ | ✗ | Optional intent layer; few testsets use |
| `intent_alternative_rejected/3` | /3 | ✗ | ✗ | ✗ | Optional intent layer |
| `intent_beneficiary_class/2` | /2 | ✗ | ✗ | ✗ | Optional intent layer |
| `intent_power_change/3` | /3 | ✗ | ✗ | ✗ | Optional intent layer |
| `intent_suppression_level/4` | /4 | ✗ | ✗ | ✗ | Optional intent layer |
| `intent_resistance_level/4` | /4 | ✗ | ✗ | ✗ | Optional intent layer |
| `intent_norm_strength/3` | /3 | ✗ | ✗ | ✗ | Optional intent layer |

**Note:** The `intent_*` predicates are an older layer from the v1.0 intent engine. They are consumed by `pattern_analysis.pl`, `report_generator.pl`, and `structural_signatures.pl` (for profile enrichment via `count_power_beneficiaries` and `has_viable_alternatives`), but are not required for any classification or diagnostic result. They are correctly omitted from the template and prompt.

---

## Recommended Changes

### To `constraint_story_generation_prompt.md`

1. **Clarify `omega_variable` arity.** The prompt describes omega_variable/5 but the engine's reporting layer reads `narrative_ontology:omega_variable/3` with structure `(ID, TypeClass, Description)` where TypeClass is one of `empirical`, `conceptual`, `preference`. Either:
   - (a) Add a note that omega_variable/5 is the generation format and the bridge handles conversion, OR
   - (b) Add instruction to also declare `narrative_ontology:omega_variable/3` in the multifile block and provide a `/3` fact alongside `/5`.

2. **Strengthen NL profile guidance.** The prompt correctly documents `accessibility_collapse` and `resistance` requirements for mountains, but 89/201 mountain candidates in the corpus still lack them. Add a **bold warning** in the Mountain section (not just the pre-submission checklist) that omitting these metrics causes silent signature degradation:
   > **WITHOUT `accessibility_collapse` (≥ 0.85), `resistance` (≤ 0.15), AND `emerges_naturally`, the mountain metric gate fires but the natural_law_signature certification FAILS SILENTLY. The constraint classifies as mountain but gets an incorrect structural signature.**

3. **Add `constraint_metric/3` primacy note.** The prompt instructs generating both `domain_priors:base_extractiveness/2` and `narrative_ontology:constraint_metric/3`. The engine reads ONLY `constraint_metric/3` (via config metric names). Add a note that `constraint_metric/3` is the primary key — if only one can be provided, it must be `constraint_metric/3`.

4. **Clarify theater_ratio for non-piton constraints.** 27 files lack `theater_ratio`. The prompt says it's for "piton detection" but the MaxEnt classifier and several diagnostic modules use it for all constraint types. Note that theater_ratio should always be provided even for non-piton constraints.

### To `constraint_story_template.pl`

1. **Uncomment `constraint_metric(theater_ratio)` in the NL Profile section.** Currently the NL profile section only has accessibility_collapse and resistance commented. Add theater_ratio to the "always provide" section rather than leaving it implied.

2. **Add `narrative_ontology:omega_variable/3` to multifile block.** If the bridge between /5 (template) and /3 (engine) is not reliable, add explicit /3 declarations.

3. **No structural changes needed.** The template covers all 18 predicates the engine requires. The 3 TEMPLATE_MISSING predicates (`entity/2`, `event/4`, `coupling_profile/2`) are vestigial — the engine declares them multifile but no current analysis depends on testset-provided values.

### To `structural_linter.py`

1. **Add `constraint_metric/3` consistency check.** Verify that if `domain_priors:base_extractiveness(C, V)` exists, a matching `constraint_metric(C, extractiveness, V)` also exists (and values match). The engine reads `constraint_metric/3`, so a file with only `domain_priors` facts will silently fail classification. Priority: HIGH.

2. **Read thresholds from config.pl.** The `get_metric_names_from_config` function already reads config.pl for metric names. Extend it to also read threshold values (`mountain_extractiveness_max`, `snare_epsilon_floor`, `piton_theater_floor`, etc.). Replace all hardcoded threshold comparisons with config-sourced values. This prevents silent drift if config.pl is recalibrated. Priority: MEDIUM.

3. **Check for analytical perspective.** The linter checks for `powerless` and `institutional` perspectives but not `analytical`. The analytical perspective is required for constraint_claim derivation and is the default context for `dr_type/2`. Add a check (with uniform-type exemption). Priority: MEDIUM.

4. **Check `constraint_metric/3` completeness.** Beyond extracting values for downstream checks, verify that all three core metrics (`extractiveness`, `suppression_requirement`, `theater_ratio`) are declared as `constraint_metric/3` facts. Currently the linter falls back to `domain_priors` regex if `constraint_metric` is missing — this masks files where the engine would fail. Priority: MEDIUM.

5. **Validate multifile block completeness.** Check that every predicate used in the file appears in the `:- multifile` block. Currently only checks for `directionality_override/3`. Extend to `coordination_type/2`, `boltzmann_floor_override/2`, `affects_constraint/2`, `emerges_naturally/1`, `has_sunset_clause/1`, and `measurement/5`. Priority: LOW.

### To Prolog modules

1. **`drl_lifecycle.pl:895`** — Change `classify_snapshot` default from 0.5 to read `config:param(default_extractiveness, E)` and `config:param(default_suppression, S)`. This aligns snapshot behavior with system-wide defaults and resolves divergence audit item M5. Priority: HIGH.

2. **`drl_core.pl:99`** — Change `get_raw_suppression` default from 0 to read `config:param(default_suppression, V)`. Currently defaults to 0 (the config default is 0.10). Difference is small but inconsistent with other default patterns. Priority: LOW.

3. **No predicates consume facts that shouldn't exist.** The `coupling_profile/2`, `input_vector/2`, and `intent_*` predicates are declared multifile but not actively consumed by any current analysis path. They are harmless vestigial declarations. Removing them would be a cleanup task, not a correctness fix.

---

## Appendix A: Corpus Coverage Heat Map

| Predicate | Count | % | Assessment |
|-----------|-------|---|-----------|
| constraint_claim/2 | 1024 | 99.9% | Excellent |
| constraint_metric(extractiveness) | 1024 | 99.9% | Excellent |
| constraint_metric(suppression_requirement) | 1024 | 99.9% | Excellent |
| omega_variable | 1022 | 99.7% | Excellent |
| constraint_metric(theater_ratio) | 998 | 97.4% | Good — 27 files missing |
| constraint_beneficiary/2 | 978 | 95.4% | Good |
| constraint_victim/2 | 971 | 94.7% | Good |
| requires_active_enforcement/1 | 885 | 86.3% | Good |
| measurement/5 | 815 | 79.5% | Adequate — 210 files without temporal data |
| coordination_type/2 | 702 | 68.5% | Moderate — limits Boltzmann precision |
| affects_constraint/2 | 460 | 44.9% | Low — limits network analysis |
| emerges_naturally/1 | 201 | 19.6% | Appropriate (mountains only) |
| has_sunset_clause/1 | 146 | 14.2% | Appropriate (scaffolds only) |
| accessibility_collapse metric | 112 | 10.9% | LOW — 89 mountain candidates missing |
| resistance metric | 113 | 11.0% | LOW — 89 mountain candidates missing |
| boltzmann_floor_override/2 | 95 | 9.3% | Appropriate (rare override) |
| directionality_override/3 | 74 | 7.2% | Appropriate (inter-institutional) |

## Appendix B: End-to-End Trace (3 Representative Files)

### File 1: `yt_ai_slop_incentive.pl` (recent, comprehensive)

**All facts provided:**
- constraint_claim(yt_ai_slop_incentive, tangled_rope)
- constraint_metric × 3 (extractiveness=0.75, suppression=0.80, theater=0.20)
- domain_priors × 3 (matching constraint_metric)
- requires_active_enforcement(yt_ai_slop_incentive)
- constraint_beneficiary × 2 (youtube_platform, slop_content_farms)
- constraint_victim × 2 (platform_viewers, human_content_creators)
- constraint_classification × 5 (powerless/snare, institutional/rope, analytical/tangled_rope, organized/rope, moderate/snare)
- measurement × 6 (3 time points × 2 metrics)
- coordination_type(yt_ai_slop_incentive, resource_allocation)
- affects_constraint × 2 (2 network edges)
- omega_variable/5 × 1
- interval(yt_ai_slop_incentive, 0, 10)

**Facts consumed by engine:** All required facts present. Full diagnostic stack can run.
**Dead data:** `domain_priors:base_extractiveness/2`, `domain_priors:suppression_score/2`, `domain_priors:theater_ratio/2` — these are shadowed by `constraint_metric/3` (the engine reads constraint_metric, not domain_priors directly). They serve as human-readable documentation.

### File 2: `open_source_commons.pl` (enriched, mixed)

**All facts provided:**
- constraint_claim(open_source_commons, rope)
- constraint_metric × 3
- domain_priors × 3
- constraint_beneficiary(open_source_commons, open_source_contributors)
- constraint_victim(open_source_commons, none)
- constraint_classification × 3
- measurement × 4 (2 time points × 2 metrics)
- has_sunset_clause(open_source_commons)
- omega_variable/5 × 1
- interval × 1

**Missing for diagnostics:**
- coordination_type — Boltzmann uses default complexity offset (0.00)
- affects_constraint — no network edges, FPN treats as isolated
- emerges_naturally — not a mountain, so appropriate
- accessibility_collapse, resistance — not a mountain, so appropriate

**Issues:**
- `constraint_victim(open_source_commons, none)` — the atom `none` is a domain-specific group name, not the absence of a victim. Engine derives `has_asymmetric_extraction` from this (victim exists = true). This is semantically wrong but structurally "works."
- `has_sunset_clause` declared for a rope — unusual; may trigger scaffold gate at some perspectives.
- Only 4 measurements (2 time points), below the 6-measurement threshold for full drift detection.

### File 3: `med_diet_consensus_2026.pl` (well-formed, comprehensive)

**All facts provided:**
- constraint_claim(med_diet_consensus_2026, snare)
- constraint_metric × 3 (extractiveness=0.2, suppression=0.4, theater=0.17)
- domain_priors × 2 (extractiveness, suppression)
- emerges_naturally(med_diet_consensus_2026)
- constraint_beneficiary × 2
- constraint_victim × 2
- constraint_classification × 3

**Missing for diagnostics:**
- measurement/5 — no temporal data, drift analysis unavailable
- coordination_type — Boltzmann uses default
- affects_constraint — no network edges
- theater_ratio as domain_priors — only as constraint_metric (correct)

**Issues:**
- Claims `snare` but `emerges_naturally` is set — the mountain gate will try to fire first (extractiveness=0.2 ≤ 0.25, but suppression=0.4 > 0.05 so mountain fails). The `emerges_naturally` flag is misleading but doesn't cause classification error because suppression blocks the mountain gate.
- No measurement/5 data despite potentially interesting drift story (dietary science evolution).
