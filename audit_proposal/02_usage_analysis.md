# 02 — Usage Analysis

All classifications below are derived from reference traces, not filename assumptions.
Where a classification is uncertain, it is marked AMBIGUOUS with a specific question.

Classification labels:
- **CORE** — removing it breaks a documented, working pipeline step
- **SUPPORT** — infrastructure that CORE depends on (tests, validation, repair)
- **DIAGNOSTIC** — standalone tools; produce outputs but not pipeline-critical
- **GENERATED** — output artifacts regenerable from source
- **LEGACY** — no inbound references, no recent activity, no clear current purpose
- **AMBIGUOUS** — owner decision required; specific question stated

---

## Prolog Modules (`prolog/*.pl`)

### Load Graph

`stack.pl` directly loads 37 modules. Several of those 37 load additional modules
transitively:
- `config.pl` → loads `config_schema`, `config_validation`
- `drl_core.pl` → loads `constraint_data`
- `test_harness.pl` → loads `logical_fingerprint`, `measurement_layer`
- `validation_suite.pl` → loads `data_validation` (when validation_suite is run)

**Stack-loaded modules (37 direct):**
narrative_ontology, config, corpus_loader, domain_priors, constraint_instances,
constraint_indexing, boltzmann_compliance, signature_detection, purity_scoring,
structural_signatures, drl_core, drl_composition, drl_counterfactual,
drl_boltzmann_analysis, drl_purity_network, drl_fpn, drl_modal_logic, drl_audit_core,
scenario_manager, coercion_projection, data_repair, data_verification, pattern_analysis,
intent_engine, drift_events, transition_paths, network_dynamics, drift_report, drl_lifecycle,
bifurcation_export, persistence_export, arakelov_height, sheaf_analysis, constraint_bridge,
uke_dr_bridge, report_generator, test_harness

**Transitively loaded, not in stack.pl directly (6):**
config_schema, config_validation, constraint_data, logical_fingerprint, measurement_layer, data_validation

### Module Classifications

**CORE — 43 modules**
The 37 stack-loaded modules plus 6 transitively loaded:
narrative_ontology, config, corpus_loader, domain_priors, constraint_instances,
constraint_indexing, boltzmann_compliance, signature_detection, purity_scoring,
structural_signatures, drl_core, drl_composition, drl_counterfactual,
drl_boltzmann_analysis, drl_purity_network, drl_fpn, drl_modal_logic, drl_audit_core,
scenario_manager, coercion_projection, data_repair, data_verification, pattern_analysis,
intent_engine, drift_events, transition_paths, network_dynamics, drift_report, drl_lifecycle,
bifurcation_export, persistence_export, arakelov_height, sheaf_analysis, constraint_bridge,
uke_dr_bridge, report_generator, test_harness, config_schema, config_validation,
constraint_data, logical_fingerprint, measurement_layer, data_validation

**SUPPORT — 1 module**
- `validation_suite.pl` — test runner; loads scenario_manager and data_validation;
  called in the documented Prolog test invocation. Not part of the runtime engine,
  but the documented validation workflow depends on it.

**GENERATED — 9 modules**
- `scs_*.pl` (scs_0anczbfz, scs_16dnmltr, scs_1h8m6rbw, scs_6tykj356, scs_cr7l487o,
  scs_escjkxbh, scs_g0k74r3d, scs_pn7h27wn, scs_zkg0ubdl) — auto-generated config
  sensitivity overlays produced by `python/structural_config_sensitivity.py`. Each file
  does `:- [stack]` then retracts/asserts one config param. **Not loaded on demand by
  scenario_manager.pl** — scenario_manager's `load_and_run/2` takes an arbitrary file path
  as argument and uses `user:consult(File)`. The scs_* files are not referenced by path in
  any .pl or .py file found (confirmed by grep). They belong in a generated-outputs location,
  not at `prolog/` root. Referenced once in `phase1/independent_module_descriptions.md`.

**DIAGNOSTIC — ~30 modules**
Not loaded by stack.pl or transitively, no transitive callers found in the 43 CORE modules.
These appear to be standalone report generators or analysis tools invoked directly:

| Module | Evidence of purpose |
|--------|---------------------|
| abductive_engine, abductive_helpers, abductive_triggers | Abductive inference subsystem |
| abductive_report | Report generator for abductive analysis |
| covering_analysis | Covering/descent analysis |
| diagnostic_summary | Summary report |
| dirac_classification | Dirac-style classification analysis |
| domain_priors_expanded | Extended domain priors |
| domain_registry | Domain registration |
| fingerprint_report | Logical fingerprint report output |
| fpn_report | FPN report output |
| gap_diagnostic | Gap analysis diagnostics |
| genuine_findings_query | Query module for genuine findings |
| giant_component_analysis | Network giant component analysis |
| global_delta_report | Delta report across corpus |
| grothendieck_cohomology | Grothendieck cohomology computations |
| inferred_coupling_protocol | Coupling inference protocol |
| invertibility_analysis | Invertibility analysis |
| isomorphism_engine, isomorphism_report | Isomorphism detection |
| json_report | JSON output generator |
| maxent_classifier, maxent_diagnostic, maxent_report | MaxEnt subsystem modules |
| omega1_audit | Omega-1 cohomology audit |
| orbit_report | Orbit structure report |
| post_synthesis | Post-synthesis analysis |
| product_site_export | Product site export |
| psych_bridge | Psychometric bridge |
| quantum_verification_report | Quantum verification report |
| signature_config | Signature configuration |
| signature_mapper | Signature mapping |
| tangled_rope_examples | Example tangled rope constraints |
| trajectory_mining, trajectory_report | Trajectory analysis |
| type_metadata | Type metadata |
| utils | General utilities |

*Caveat:* Some of these may be loaded by Python's subprocess calls to Prolog (e.g., via
`swipl -g "[some_module]"` invocations in Python scripts). A full Python→Prolog subprocess
trace was not performed. Owner should verify before reclassifying any of these as LEGACY.

**AMBIGUOUS — 1 module**
- `stack.pl` itself — classified here as infrastructure/loader, not a "module" in the
  functional sense. Included in the 91 count but is the entry point, not an engine module.

### Prolog Corpus Subdirectories

| Path | Classification | Basis |
|------|---------------|-------|
| `testsets/` | CORE | Pipeline reads from here; removing breaks `run_pipeline.py` |
| `testsets_sotu/` | CORE | `run_pipeline.py` hardcodes `TESTSETS_SOTU_DIR = PROLOG_DIR / "testsets_sotu"` |
| `archives/` | LEGACY | Undocumented; versions v1/v3/v4; no active code references found |
| `probsets/` | AMBIGUOUS | No references found in .py or .pl files; owner should confirm purpose |
| `gaptests/` | AMBIGUOUS | No references found; may be used in subprocess calls |
| `belief_battery/` | AMBIGUOUS | No references found in source |
| `recon_2/` | AMBIGUOUS | Created in most recent commit (2026-05-15); unclear if active work or archive |

---

## Python Files (`python/`)

### Core Pipeline Import Graph

`run_pipeline.py` imports: stdlib only (`subprocess`, `json`, `threading`, `concurrent.futures`,
`pathlib`, `datetime`) — it orchestrates via subprocess calls to Prolog and other Python scripts,
not via Python imports. It does not import sibling python/ modules in its standard invocation path.

`enhanced_report.py` reads from multiple `outputs/` JSON files; uses subprocess to call Prolog;
imports from `shared/` subpackage.

**CORE — 3 files**
- `run_pipeline.py` — documented pipeline entry point
- `enhanced_report.py` — documented report generator
- `enrich_pipeline_json.py` — enrichment step called by pipeline

**SUPPORT — 7 files**
- `shared/__init__.py`, `shared/constants.py`, `shared/loader.py`, `shared/maxent.py`,
  `shared/schemas.py` — shared utilities imported by DIAGNOSTIC scripts
- `reports/__init__.py`, `reports/__main__.py` — query module entry point

**DIAGNOSTIC — the majority (~105 files)**
Files with `__main__` blocks but not imported by run_pipeline.py. 102 of 129 Python files are
standalone-runnable. These are the "100+ diagnostic scripts" mentioned in CLAUDE.md.

Key documented diagnostics:
- `cognitive_displacement_sweep.py` — documented in CLAUDE.md
- `config_sensitivity_sweep.py` — documented in CLAUDE.md and AUDIT.md
- `directionality_sensitivity_sweep.py` — documented in AUDIT.md
- `linter.py` — documented in CLAUDE.md; has a `__main__` block (resolved issue)
- `game_theory_*.py` (6 files) — game theory analysis suite
- `cc_diagnostic.py` — Cultural Cognition profiles
- `sotu_*.py` (5 files) — SOTU sub-project scripts; write to `sotu/` directory

SOTU scripts and their write targets:
- `sotu_fetch.py` writes to `sotu/raw/`
- `sotu_generate_batch.py` and `sotu_scope_batch.py` likely write to `sotu/json/` and `sotu/pl/`
- `tangled_rope_sign_flip.py` writes to `docs/results/tangled_rope_sign_flip.md` — a Python
  diagnostic that writes into the docs/ tree (see §docs/ below)

AMBIGUOUS — files to verify:
- `repair_constraint_metrics.py` — if it exists, generates bridge facts; AUDIT.md mentions it
  but it was not found in `find python/ -name "*.py"` output. May have been deleted or renamed.
- `prolog_cleaner.py`, `testset_rebuild.py` — data modification scripts; unclear if active
- `regenerate_stories.py` — unclear if active or one-off

**GENERATED — 0 files in python/**
Python source files are not generated.

**LEGACY — likely ~5–10 files**
Files with no `__main__` block and not imported by any other module. Specific candidates
require a full import-graph walk to confirm. Owner should run:
`grep -rl "import" python/ --include="*.py" | xargs grep -l "from.*import\|^import" | sort`
and cross-reference against the standalone list.

---

## Documentation (`docs/`)

### Reference Graph (partial — key paths verified)

**Referenced from CLAUDE.md:**
- `docs/deferential_realism_paper_v6.11.md` — canonical framework paper
- `docs/logic.md` — formal classification spec
- `docs/project_orientation.md` — canonical operational reference

**Referenced from project_orientation.md** (functions as second CLAUDE.md for path purposes):
- `deferential_realism_paper_v6.11.md`, `when_apparatus_sharpens_taxonomy.md`,
  `logic.md`, `two_hub_architecture.md`, `coupling_structure_evidence.md`,
  `observers_not_humans_v5.md`, `contextuality_paper_v1.md`, `asymmetry_of_failure_types.md`,
  `when_splitting_isnt_solving.md`, `when_consensus_isnt_coherence.md`,
  `when_frame_isnt_foreground.md`, `when_nodes_arent_the_unit.md`, `when_metrics_arent_measurement.md`,
  `debugging_philosophy.md`, `docs/results/fragility_cross_tab.md`
- External: `outputs/metric_audit_writeup.md`, `outputs/metric_audit_results.*`,
  `outputs/audit3_te_robustness.*`, `python/metric_audit.py`

**Referenced from quick_start.md:**
- `prompts/constraint_story_generation_prompt.md`
- `protocols/uke_write_v2.1.md`

### Classifications

**CORE — current canonical docs:**
`deferential_realism_paper_v6.11.md`, `logic.md`, `project_orientation.md`,
`two_hub_architecture.md`, `when_apparatus_sharpens_taxonomy.md`, `coupling_structure_evidence.md`,
`asymmetry_of_failure_types.md`, `when_splitting_isnt_solving.md`, `when_consensus_isnt_coherence.md`,
`when_frame_isnt_foreground.md`, `when_nodes_arent_the_unit.md`, `when_metrics_arent_measurement.md`,
`observers_not_humans_v5.md`, `contextuality_paper_v1.md`, `debugging_philosophy.md`

**LEGACY — superseded paper versions:**
`deferential_realism_paper.md` (v1), `deferential_realism_paper_v2.md` through
`deferential_realism_paper_v6.10.md` (16 files total, all superseded by v6.11).
These are historical record; removing them loses version history that git already preserves.

**AMBIGUOUS — working notes vs. canonical:**

| File | Question |
|------|----------|
| `observers_not_humans_v2.md`, `v3.md`, `v4.md` | Are v2–v4 working history or still referenced? |
| `asymmetry_of_failure_types_first_draft.md` | Superseded by canonical? |
| `diagnostic_integration_architecture_old.md` | Superseded by non-_old version? |
| `core_v4.2.md`, `core_v4.3.md` | Superseded by current config/logic? |
| `v4_gap_analysis.md`, `v4_outline.md`, `v5_patch_document.md` | Version transition docs; historical? |
| `recon_2_scope.md`, `recon_2_scope_v2.md` | Related to prolog/recon_2/? Active or concluded? |

**GENERATED — 3 files in docs/results/:**
`docs/results/fragility_cross_tab.md`, `docs/results/h1_distribution_test.md`,
`docs/results/tangled_rope_sign_flip.md` — these files are written to by Python scripts
(confirmed for tangled_rope_sign_flip.md). They are generated outputs that happen to live
inside the docs/ tree.

---

## Audit Directories

### Relationship Between `audit/`, `audit_data/`, and `phase1/`

These three directories represent different phases or artifacts of the audit lifecycle:

| Directory | Role | Content Type |
|-----------|------|--------------|
| `audit_data/` | Evidence archive | Raw text dumps from tools (pylint, grep, git log); 27 files; referenced by AUDIT.md |
| `audit/` | Audit sub-project | Python scripts that *run* audits + their own output subdirectories (original_outputs/, outputs/, outputs_haiku/) |
| `phase1/` | Analysis documents | Intermediate analysis from a named "phase 1" audit pass; most recent commit 2026-05-07 |

**Key finding:** audit_data/ was last touched 2026-03-01 (post-Feb 2026 audit). audit/ was
last touched 2026-02-27. phase1/ was last touched 2026-05-07. These are not the same effort
at the same time.

Classifications:
- `audit_data/` — DIAGNOSTIC (evidence archive for a completed audit; still referenced in AUDIT.md)
- `audit/` — DIAGNOSTIC (self-contained audit sub-project with scripts and outputs)
- `phase1/` — DIAGNOSTIC (intermediate analysis documents from a recent audit pass; AMBIGUOUS on completeness)

---

## Other Directories

| Directory | Classification | Basis |
|-----------|---------------|-------|
| `sotu/raw/` | CORE | Source data for SOTU sub-project |
| `sotu/json/` | GENERATED | LLM-generated constraint JSON (parallel to main json/) |
| `sotu/pl/` | GENERATED / AMBIGUOUS | Identical to prolog/testsets_sotu/; duplication relationship unclear |
| `essays/` | DIAGNOSTIC | Published essays; no pipeline dependency |
| `examples/` | DIAGNOSTIC | Examples for documentation/demonstration; no pipeline dependency |
| `results/` | DIAGNOSTIC | 4 output files from one past experiment; no inbound references in active docs |
| `scripts/fix_*.sh` | DIAGNOSTIC | One-shot data repair scripts |
| `scripts/pipeline_dashboard.sh` | DIAGNOSTIC | Monitoring script |
| `scripts/run_full_pipeline.sh.legacy` | LEGACY | Filename signals legacy status |
| `prompts/archives/` | LEGACY | 13 historical prompt versions superseded by current prompts |
| `phase1/` | DIAGNOSTIC | See audit section above |
| `prolog/archives/` | LEGACY | Historical engine versions v1/v3/v4; 405 MB; no active references |

---

## Summary Table

| Path | Classification | Confidence |
|------|---------------|------------|
| prolog/stack.pl + 42 CORE modules | CORE | High |
| prolog/validation_suite.pl | SUPPORT | High |
| prolog/scs_*.pl (9) | GENERATED | High |
| ~30 prolog diagnostic modules | DIAGNOSTIC | Medium (subprocess calls unverified) |
| python/run_pipeline.py, enhanced_report.py, enrich_pipeline_json.py | CORE | High |
| python/shared/* | SUPPORT | High |
| python/reports/* | SUPPORT | High |
| ~105 python standalone scripts | DIAGNOSTIC | High |
| docs/deferential_realism_paper_v6.11.md + 14 canonical docs | CORE | High |
| docs/deferential_realism_paper.md through v6.10.md (16 files) | LEGACY | High |
| docs/results/* (3 files) | GENERATED | High |
| prolog/testsets/, testsets_sotu/ | CORE | High |
| prolog/archives/ | LEGACY | High |
| prolog/probsets/, gaptests/, belief_battery/, recon_2/ | AMBIGUOUS | — |
| json/ | GENERATED | High |
| outputs/ | GENERATED | High |
| audit/, audit_data/, phase1/ | DIAGNOSTIC | Medium |
| essays/ | DIAGNOSTIC | Medium |
| examples/ | DIAGNOSTIC | Medium |
| results/ | DIAGNOSTIC | Medium |
| sotu/ | CORE (raw) + GENERATED (json, pl) | Medium |
| scripts/run_full_pipeline.sh.legacy | LEGACY | High |
| prompts/archives/ | LEGACY | High |
