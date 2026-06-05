# AGENTS.md — Bot Onboarding Reference

Canonical operational reference for AI agents working in this repository.
Start here before touching any file. See also `docs/project_orientation.md`
for the full research context and `ISSUES.md` for unresolved
engine-level issues.

---

## Contents

1. [Tech Stack](#1-tech-stack)
2. [Repo Layout](#2-repo-layout)
3. [Naming Conventions](#3-naming-conventions)
4. [Architectural Rules and Invariants](#4-architectural-rules-and-invariants)
5. [Testing Requirements](#5-testing-requirements)
6. [Generation Pipeline](#6-generation-pipeline)
7. [What Not to Do](#7-what-not-to-do)

---

## 1. Tech Stack

### Prolog engine

- **Runtime:** SWI-Prolog, invoked as `swipl`. No version pinned; use whatever
  `swipl --version` returns on the host. The engine uses standard ISO predicates
  plus SWI-Prolog extensions (nb_getval/nb_setval, setup_call_cleanup, assertion
  dynamic predicates).
- **Entry point:** `prolog/stack.pl`. Loading it pulls in all 47+ engine modules
  in dependency order. Never load individual modules directly in production — always
  load through stack.
- **Module system:** each file declares `:- module(name, [exports]).`. The engine
  uses qualified calls (`module:predicate`) everywhere — do not rely on implicit
  imports.

**Stack layers (from `prolog/stack.pl`):**

| Layer | Key modules |
|---|---|
| Schema | `narrative_ontology`, `config` |
| Data | `corpus_loader`, `domain_priors`, `constraint_instances` |
| Classification | `constraint_indexing`, `structural_signatures`, `drl_core` |
| Composition | `drl_composition`, `drl_counterfactual`, `drl_fpn` |
| Diagnostics | `drl_boltzmann_analysis`, `drl_purity_network`, `sheaf_analysis`, `arakelov_height` |
| Commitment Systems | `cs_pattern_detection`, `cs_drift_engine`, `cs_axiom_engine`, `cs_kernel_registry`, `cs_drift_mismatch` |
| Management | `scenario_manager`, `data_repair`, `report_generator` |

### Python tooling

- **Python:** ≥ 3.10 (from `pyproject.toml`).
- **Core dependencies:** `pandas`, `jinja2>=3.1.6`
- **Optional stats:** `scipy`, `scikit-learn`, `statsmodels` (`pip install -e ".[stats]"`)
- **Optional AI:** `anthropic`, `google-genai` (`pip install -e ".[ai]"`)
- **Additional:** `streamlit`, `google-api-core` (from `requirements.txt`)

### API keys (required for generation steps)

| Variable | Used by |
|---|---|
| `ANTHROPIC_API_KEY` | All Claude calls (orchestrator, story generation) |
| `GOOGLE_API_KEY` | Haiku batch generation (`agent.generate_json_haiku`) |

Neither key is needed for read-only analysis of the existing corpus.

---

## 2. Repo Layout

```
structural_dynamics_model/
├── prolog/
│   ├── stack.pl                  # Module loader — always load through this
│   ├── config.pl                 # SINGLE SOURCE OF TRUTH for all param/2 facts
│   ├── config_schema.pl          # Schema validation for config params
│   ├── drl_core.pl               # Primary classifier — classify_from_metrics/6
│   ├── constraint_indexing.pl    # χ = ε × f(d) × σ(S) computation
│   ├── structural_signatures.pl  # NL / Coordination / Constructed detection
│   ├── validation_suite.pl       # Test runner
│   ├── testsets/                 # LIVE corpus — post-de-leak rebuild (reset 2026-06-05;
│   │                             # cite the pipeline manifest for size, never a memorized count)
│   ├── archives/datasets/        # ALL previous corpora: kernel_v1/ (1,106 pre-reset),
│   │                             # original_v6/ (3,380 chimera-era), sotu/ (189), older sets
│   └── tests/                    # Engine unit tests (plunit); run from prolog/ with stack loaded
├── python/
│   ├── run_pipeline.py               # LOAD-BEARING pipeline orchestrator
│   ├── enhanced_report.py            # LOAD-BEARING per-constraint report generator
│   ├── linter.py                     # LOAD-BEARING library — from linter import lint_file
│   ├── config_sensitivity_sweep.py   # documented CLI sweep
│   ├── directionality_sensitivity_sweep.py  # documented CLI sweep
│   ├── [~23 pipeline modules]        # imported by run_pipeline.py
│   ├── [~30 exploratory scripts]     # game theory, SOTU, harvest — phase 2 pending
│   ├── shared/                       # utility package (loader, constants, maxent)
│   ├── reports/                      # report query subpackage
│   ├── tests/                        # standalone test scripts (8 files)
│   ├── sweeps/                       # parameter variation scripts (12 files)
│   └── audits/                       # audit, diagnostic, probe scripts (19 files)
├── agent/
│   ├── c-orchestrator.py         # Primary authoring entry point (6-step chain)
│   └── generate_json_haiku.py    # Batch corpus expansion via Haiku
├── json/                         # LLM-generated constraint specs (INPUTS, not outputs)
├── audits/                       # Completed audits — one audits/<YYYY-MM-DD>_<slug>/ per
│                                 # audit, writeup + evidence together (MANDATE: new audits
│                                 # go here, not docs/ or outputs/; see audits/README.md)
├── outputs/                      # All pipeline output (classifications, reports, essays)
│   ├── pipeline_output.json      # Pre-computed H¹, heights, classifications
│   ├── constraint_reports/       # Per-constraint enhanced reports
│   └── essays/                   # Synthesized essay drafts
├── docs/
│   ├── project_orientation.md    # Canonical operational reference
│   ├── deferential_realism_paper_v6.13.md  # Canonical framework paper
│   ├── logic.md                  # Formal classification rules (must match config.pl)
│   ├── ISSUES.md                 # Unresolved engine/schema issues (OQ-01 – OQ-13)
│   └── [100+ specialized docs]
├── CLAUDE.md                     # Development instructions (read before editing code)
├── ISSUES.md                     # Single tracking surface (OQs; status grammar + checker in footer)
└── README.md                     # Framework overview and documentation index
```

**Key distinction:**
- `json/` → LLM-generated constraint specifications. The orchestrator writes here;
  `run_pipeline.py` reads from here. Do not write analysis results here.
- `outputs/` → all pipeline output. Read pre-computed values from
  `outputs/pipeline_output.json`; do not recompute H¹, Arakelov heights, or
  MaxEnt distributions from scratch.

---

## 3. Naming Conventions

### Prolog testset files

**File name:** `snake_case.pl` — one constraint per file, matching the constraint ID.
Example: `abolition_reading.pl` contains constraint `abolition_reading`.

**Module declaration (line 1 of body):**
```prolog
:- module(constraint_<basename>, []).
```
Example: file `abolition_reading.pl` → `:- module(constraint_abolition_reading, []).`

**Required multifile block** — every testset must declare all of these, even if
the file does not use all of them:
```prolog
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.
```

**Required section headers** (use `/* === N. SECTION NAME === */` style):
1. Constraint Identity Rule (DP-001 comment block)
2. Namespace Hooks
3. Narrative Context
4. Base Properties (Domain Priors)
5. Indexed Classifications (P, T, E, S)
6. Validation Tests
7. Omega Variables
8. Integration Hooks
9. Temporal Measurements

**Context tuple** — exactly arity 4, always in this order:
```prolog
context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))
```
Do not add extra arguments. Linter rule `CONTEXT_ARITY` rejects any other arity.

**Power atoms** (P): `powerless | moderate | powerful | organized | institutional | analytical`

**Time horizon atoms** (T): `biographical | generational | civilizational`

**Exit option atoms** (E): `trapped | limited | analytical`

**Spatial scope atoms** (S): `local | regional | national | continental | global | universal`

**Classification type atoms**: `mountain | rope | tangled_rope | snare | scaffold | piton | naturalized`

### JSON constraint specs

**File name:** same `snake_case.json` as the Prolog counterpart.

**Required top-level keys:**
`header`, `base_properties`, `perspectives`, `omegas`, `measurements`,
`interval`, `commentary`, `boltzmann`, `network`, `directionality_overrides`

Manifest key (`manifest`) is added automatically by the pipeline; do not hand-write it.

### Python scripts

No enforced naming convention beyond the existing `snake_case.py` pattern.
The linter must be imported as a library — it cannot be run directly as
`python3 python/linter.py`; it exposes `lint_file(filepath)` and
`lint_dir(directory)`.

---

## 4. Architectural Rules and Invariants

### The classification invariants

**Rule 1 — Single entry point.** All classification routes through
`classify_from_metrics/6` in `prolog/drl_core.pl`. This is the canonical threshold
predicate. If you find a code path that classifies without calling this predicate,
it is a bug.

```prolog
% Signature:
classify_from_metrics(+Constraint, +BaseEps, +Chi, +Supp, +Context, -Type)
```

**Rule 2 — Dual threshold.** Both χ (chi, power-scaled extractiveness) AND
ε (epsilon, base extractiveness) must be checked. A predicate that checks only
one is wrong. The rope, snare, and tangled_rope clauses in `drl_core.pl` all
check both.

**Rule 3 — Config is the single source of truth.** All numeric parameters live
in `prolog/config.pl` as `param/2` facts. Do not hardcode threshold values
anywhere else. If `logic.md` specifies a threshold, it must match `config.pl` —
`logic.md` is the spec; `config.pl` is the implementation.

**Rule 3b — Thresholds never reach the authoring LLM (de-leak, 2026-06-05).**
The engine's classification bands must NOT appear in any author-facing surface:
the generator prompt (`prompts/constraint_story_generation_prompt_json.md`), the
JSON schema (`schemas/constraint_story_schema.json` — its text ships verbatim in
the generation prompt via `story_generator_base.build_prompt`), or LLM retry/
feedback text (`regenerate_stories.py` filters `THRESHOLD_COUPLED_LINT` codes;
`c-orchestrator._sanitize_feedback_error` scrubs bound values). The authored
claim vs. computed type diff is the research signal; disclosing a band lets the
author back-compute the claim and collapses it (KNOWN_STATE.md 2026-06-05).
Before adding ANY numeric threshold, metric band, or "required when ε > X" rule
to those surfaces, run the assembled-payload check: dump
`story_generator_base.build_prompt(...)` and grep it for band values near type
names — it must stay clean.

**Rule 4 — Priority cascade.** Classification priority (highest to lowest):
`mountain > piton(dead-coordination) > snare > scaffold > rope > tangled_rope > piton(fallback) > naturalized > unknown`

The cascade is implemented as ordered clauses in `classify_from_metrics/6`. The
order of clauses in `drl_core.pl` IS the priority.

### The chi formula

```
χ = ε × f(d(P, E)) × σ(S)
```

- **ε** (epsilon): base extractiveness — structural property of the constraint,
  observer-independent. Read from `narrative_ontology:constraint_metric/3` via
  `constraint_data:base_extractiveness/2`.
- **f(d)**: sigmoid over directionality d — computed by `sigmoid_f/2` in
  `constraint_indexing.pl`. Default params: L=−0.20, U=1.50, d₀=0.50, k=6.0.
- **d**: directionality — derived from observer's power position (P) and
  beneficiary/victim structure. Continuous [0, 1].
- **σ(S)**: scope modifier — from `config.pl`. Values: local=0.8, regional=0.9,
  national=1.0, continental=1.1, global=1.2, universal=1.0.

**Power modifiers (π)** — used to derive d, not directly in χ:

| Atom | π value |
|---|---|
| powerless | 1.5 |
| moderate | 1.0 |
| powerful | 0.6 |
| organized | 0.4 |
| institutional | −0.2 (net beneficiary) |
| analytical | 1.15 |

### Classification thresholds (from `config.pl`)

| Gate | Parameter | Value |
|---|---|---|
| mountain ε ceiling | `mountain_extractiveness_max` | 0.25 |
| rope χ ceiling | `rope_chi_ceiling` | 0.35 |
| rope ε ceiling | `rope_epsilon_ceiling` | 0.45 |
| tangled_rope χ floor | `tangled_rope_chi_floor` | 0.40 |
| tangled_rope χ ceiling | `tangled_rope_chi_ceil` | 0.90 |
| tangled_rope ε floor | `tangled_rope_epsilon_floor` | 0.30 |
| snare χ floor | `snare_chi_floor` | 0.66 |
| snare ε floor | `snare_epsilon_floor` | 0.46 |

### The Two-Hub Architecture

Classification variation across contexts comes from exactly two independent sources:

- **Hub 1 (Power-Scaling Sigmoid):** `constraint_indexing.pl` — `derive_directionality/3`
  → `sigmoid_f/2` → χ = ε × f(d) × σ(S). Drives the chi-threshold gates.
- **Hub 2 (Effective Immutability):** `effective_immutability/3` in
  `constraint_indexing.pl` — discrete (time_horizon, exit_options) → mountain|rope.
  Drives the mountain gate and the snare immutability check.

When χ and type disagree across contexts, the disagreement originates in one or
both hubs. Identify which hub before proposing a fix.

### Structural signatures

`structural_signatures.pl` and `signature_detection.pl` provide overrides that
fire after metric-based classification via
`integrate_signature_with_modal(C, MetricType, FinalType)` in `dr_type/3`.

Override hierarchy:
1. **NL (Natural Law) + `emerges_naturally`:** forces mountain regardless of metrics.
2. **Constructed sub-signatures:** three variants keyed by ε level.
3. Metric type passes through unchanged if no signature fires.

Signatures override; they do not replace the metric path. `dr_type/3` always
runs `metric_based_type_indexed/3` first.

### Deprecated predicates — do not call

| Deprecated | Replaced by | File |
|---|---|---|
| `dr_type_at/4` | `classify_at_time/4` | `drl_composition.pl` |
| `classify_snapshot/3` | `snapshot_type/3` | `transition_paths.pl` |

The legacy predicates used the old `power_modifier` χ path (χ = ε × π, omitting σ).
The replacements use the canonical sigmoid pipeline.

### Product site scope exclusion

`constraint_indexing.pl:954–955` excludes `regional`, `continental`, `universal`
scopes from the product site (156-context curated site). Their scope_modifier values
are not calibrated against corpus classifications. Do not add them back without
validation.

### Pre-computed values

**Read from `outputs/pipeline_output.json`, do not recompute:**
H¹, Arakelov heights, MaxEnt distributions, and per-constraint classifications
are pre-computed. The corpus is continuous-growing; the manifest key records
the timestamp and commit at which values were computed. Always cite the manifest
when reporting findings.

---

## 5. Testing Requirements

### Run the Prolog test suite

```bash
cd prolog && swipl -g "[stack], [validation_suite], run_dynamic_suite, halt" -t "halt(1)"
```

Expect: all tests pass, 0 failures. Any failure blocks merging.

### Run the full analysis pipeline

```bash
python3 python/run_pipeline.py
```

This re-classifies the full corpus and updates `outputs/pipeline_output.json`.
Single-writer convention (2026-06-04): the swipl export writes
`outputs/pipeline_output.raw.json`; `run_pipeline.py` alone writes the canonical
manifest-bearing `pipeline_output.json`. A direct swipl re-export cannot clobber it.

### Stack consistency check (wrong-qualifier / undefined-predicate detection)

```bash
cd prolog && swipl -l check_stack.pl -g "run_check_stack, halt" -t "halt(1)"
```

Compare against the recorded baseline (KNOWN_STATE.md 2026-06-04); new findings are
regressions introduced by your change.

### In-session overlay probes

Use `probe_harness:with_retracted/2` / `with_overlay/3` (snapshot-first, verified restore,
automatic cache clearing via `cache_registry:clear_all_caches/0`) instead of hand-rolled
retract/assert. Corpus membership/denominator: enumerate `corpus_loader:corpus_constraint/1`.
Tests: `cd prolog && swipl -g "[stack], [tests/test_probe_harness], run_tests, halt" -t "halt(1)"`.

### Run the linter on a testset

The linter must be called as a library, not a script:

```python
from linter import lint_file
errors = lint_file('prolog/testsets/my_constraint.pl')
```

To lint a directory: `lint_dir('prolog/testsets/')`. Outputs go to
`outputs/lint_errors.txt` when run via the pipeline.

### Linter error codes (41 codes across 31 checks)

Any linted file that produces an ERROR-level code blocks acceptance into the corpus.
WARNING-level codes are data completeness issues logged but not blocking.

| Code | What triggers it |
|---|---|
| `BARE_CONTEXT` | Context declaration missing required wrapper |
| `CONTEXT_ARITY` | `context/N` where N ≠ 4 |
| `DEPRECATED_TERM` | `noose` used instead of `snare` (renamed v3.4) |
| `DUPLICATE_MEASUREMENT` | Same measurement point declared twice |
| `FLOOR_EXCEEDS_EXTRACTION` | `boltzmann_floor_override > base_extractiveness` |
| `GENERIC_GROUP` | Beneficiary/victim uses vague names (stakeholders, general_public, etc.) |
| `IDENTICAL_EXIT_OPTIONS` | Same exit option atom repeated across perspectives |
| `INSUFFICIENT_TEMPORAL_DATA` | High-extraction constraint (ε > 0.46) has < 6 measurement facts |
| `INSUFFICIENT_VARIANCE` | Fewer than 2 distinct types across indexed perspectives |
| `INVALID_CLAIM_TYPE` | `constraint_claim` type not in the 6 valid atoms |
| `INVALID_COORDINATION_TYPE` | `coordination_type` not in `{information_standard, resource_allocation, enforcement_mechanism, global_infrastructure}` |
| `INVALID_D_VALUE` | `directionality_override` d value outside [0.0, 1.0] |
| `INVALID_FLOOR_OVERRIDE` | `boltzmann_floor_override` outside [0.0, 1.0] |
| `INVALID_POWER_ATOM` | `directionality_override` power atom not in the 6 valid atoms |
| `INVALID_SCOPE` | `spatial_scope` not in the 6 valid atoms |
| `LOW_THEATER_RATIO` | Piton requires `theater_ratio ≥ 0.70` |
| `METRIC_SOURCE_INCONSISTENCY` | Same metric declared via both `domain_priors` and `constraint_metric` |
| `MISSING_BENEFICIARY` | Tangled rope, scaffold, or non-mountain constraint lacks `constraint_beneficiary/2` |
| `MISSING_CLAIM` | `constraint_classification/3` present but no matching `constraint_claim/2` |
| `MISSING_ENFORCEMENT` | Tangled rope requires `requires_active_enforcement/1` |
| `MISSING_HOOK` | Missing `narrative_ontology:interval/3` |
| `MISSING_METRICS` | No `extractiveness` or `suppression` metric found |
| `MISSING_MODULE` | File does not begin with `:- module(id, [])` |
| `MISSING_MULTIFILE` | Uses `directionality_override/3` without declaring it multifile |
| `MISSING_NL_PROFILE` | Natural law (mountain) constraint missing NL profile metrics |
| `MISSING_OMEGA` | High-extraction constraint (ε > 0.46) has no `omega_variable/5` |
| `MISSING_PERSPECTIVE` | Non-uniform type distribution missing powerless or institutional context |
| `MISSING_SUNSET_CLAUSE` | Scaffold with `requires_active_enforcement` lacks `has_sunset_clause/1` |
| `MISSING_TEMPORAL_DATA` | ε > 0.46 but no `narrative_ontology:measurement/5` facts |
| `MISSING_THEATER_RATIO` | Piton lacks `domain_priors:theater_ratio/2` |
| `MISSING_VICTIM` | Tangled rope or snare lacks `constraint_victim/2` |
| `MOUNTAIN_METRIC_CONFLICT` | Mountain classification conflicts with ε or suppression metrics |
| `MULTI_ID` | Multiple constraint IDs in one file (one constraint per file required) |
| `OUTDATED_HOOK` | Missing `constraint_indexing:constraint_classification/3` (v4.0 hook) |
| `REDUNDANT_MEASUREMENT` | Measurement point already exists |
| `SCAFFOLD_DANGER_ZONE` | ε ≤ 0.30, beneficiary data present, but no enforcement/sunset/theater |
| `SELF_REFERENCE` | `affects_constraint(X, X)` — constraint affects itself |
| `STUB_MISMATCH` | Claimed type inconsistent with actual metric profile |
| `UNRESOLVED_MANDATROPHY` | ε > 0.70 requires resolution hook or `[RESOLVED MANDATROPHY]` annotation |
| `VACUOUS_TEST` | `test/1` clause with unbound comparison variable |
| `VARIABLE_IN_AFFECTS` | `affects_constraint` uses a Prolog variable as target |

### Config sensitivity sweep

To verify that a parameter change does not alter any classification:

```bash
python3 python/config_sensitivity_sweep.py      # 154 numeric params at ±25%
python3 python/directionality_sensitivity_sweep.py  # 17 directionality constants at ±25%
```

Results to `python/config_sensitivity_results.json` and
`python/directionality_sensitivity_results.json`. All 154 params are inert at ±25%
(established 2026-02-28 audit). If you change a param, re-run and confirm it is
still inert.

### No pytest setup

There is no `pytest.ini`, `conftest.py`, or `setup.cfg`. Testing is Prolog-native
(validation_suite) plus Python linter. Do not add a pytest layer without discussing
with the maintainer.

---

## 6. Generation Pipeline

The primary authoring command:

```bash
python3 agent/c-orchestrator.py "some topic"
```

This chains six steps automatically:

| Step | What it does | Writes to |
|---|---|---|
| 1 Research | Web search grounding via Haiku | (memory, no file) |
| 2 Decompose | UKE_SCOPE protocol selects every §3-distinct axis (no fixed count; `--axes N` is an optional ceiling, default none — changed 2026-06-05) | (manifest in memory) |
| 3 Generate | Sonnet generates one constraint story per axis. NOTE: resolves only flat `manifest["axes"]`; kernel-reading entries are skipped — kernel topics go through `agent/generate_kernel_corpus.py`, which since 2026-06-05 ALSO auto-generates a forced-flat control per kernel (`<kernel_id>_flat_control`, alignment key `narrative_ontology:flat_control_of/2`; OQ-76 — do not remove, and do not generalize to kernel-on-every-flat) | `json/`, `prolog/testsets/` |
| 4 Corpus update | Runs `python/run_pipeline.py` to re-classify full corpus | `outputs/pipeline_output.json` |
| 5 Reports | `python/enhanced_report.py` writes per-constraint reports | `outputs/constraint_reports/<id>_report.md` |
| 6 Essay | Sonnet synthesizes draft essay from constraint reports | `outputs/essays/`, `agent/analysis/essays/` |

**To expand the corpus without a full topic run** (faster, uses Haiku batch API):

```bash
python3 -m agent.generate_json_haiku
```

Reads `prolog/beta_seeds.json`, generates via Haiku with prompt caching. This is
how the chimera-era corpus grew to 3,337 constraints (archived: `prolog/archives/datasets/original_v6/`)
archive; live `testsets/` is now 223 after the chimera-collision rebuild (see CLAUDE.md
Critical Distinctions / OQ-25). Cite the pipeline manifest, not a fixed count.

### Pipeline output manifest convention

Every pipeline output JSON carries a `manifest` top-level key:

```json
{
  "manifest": {
    "pipeline_run_at": "<ISO timestamp>",
    "n_constraints": "<live count — read from the manifest, do not memorize>",
    "n_sotu_constraints": "<0 since the 2026-06-05 reset; sotu archived at prolog/archives/datasets/sotu/>",
    "code_commit": "<full SHA>",
    "code_commit_short": "<short SHA>",
    "code_dirty": false,
    "schema_version": "<version>"
  },
  ...
}
```

**When writing findings:** always cite `manifest.pipeline_run_at` and
`manifest.code_commit_short`. "The corpus" is only meaningful relative to a
timestamp; the manifest makes the timestamp citable.

---

## 7. What Not to Do

These rules are absolute. Violating them silently changes the system's semantics.

**Never:**
- Add classification logic outside `classify_from_metrics/6`. Any new type or
  threshold goes into `drl_core.pl` as a new clause in the correct priority position,
  and into `config.pl` as a new `param/2` fact, and into `logic.md` as a formal rule.
- Add numeric thresholds as hardcoded values. They go in `config.pl` only.
- Modify `config.pl` default values without re-running the sensitivity sweep.
- Call `dr_type_at/4` or `classify_snapshot/3` — both are deprecated with stale
  chi paths.
- Recompute H¹, Arakelov heights, or MaxEnt distributions from scratch. Read them
  from `outputs/pipeline_output.json`.
- Run the linter directly with `python3 python/linter.py`. It has no `__main__`
  block useful for direct invocation; use `from linter import lint_file`.
- Import from `python/tests/`, `python/sweeps/`, or `python/audits/` — those
  directories contain standalone scripts, not library modules. If you need to import
  something from a script in those directories, verify that import with a full-tree
  grep (Python imports AND subprocess path invocations) before adding it.
- Add new power, time, exit, or scope atoms without updating `config_schema.pl`,
  `config.pl`, and the linter's `INVALID_*` check lists.
- Load individual Prolog modules without going through `[stack]`.

**Canonical papers:**
- Framework paper: `docs/deferential_realism_paper_v6.13.md`. Files
  `deferential_realism_paper.md` through `v6.12.md` are superseded. Do not cite
  them as current.
- Formal classification rules: `docs/logic.md`. This is the spec; `config.pl`
  must match it.

**Archive testsets** (`prolog/archives/`): document build provenance, not active
code. Do not modify them and do not treat them as the current corpus.

**The `.tsx` artifacts** in `outputs/` are generated output. Do not edit them
directly — regenerate via the pipeline.

**UTF-8 note:** `logic.md` had double-encoded UTF-8 (mojibake) repaired February
2026. If you encounter Edit-tool failures on any file with multi-byte characters,
use `sed` or Python to make the change instead.

---

*Last updated: 2026-05-28. If architectural rules change, update this file in the
same commit.*
