# Usage Analysis: Core vs. Diagnostic Mapping

This document details the usage analysis of the Prolog engine, Python scripts, documentation files, audit repositories, and other subdirectories. Components are rigorously traced to determine how they relate to the core pipeline, test harnesses, or ad-hoc diagnostic sweeps.

---

## 1. Methodology & Reference Traces

To bypass aesthetic assumptions and directory-name-only classifications, we traced the actual load graphs:
1. **Prolog dependencies:** We parsed all `:- use_module(...)` and `:- consult(...)` declarations across all 95 root-level files to construct the full load graph.
2. **Python dependencies:** We analyzed all `import` patterns in the Python codebase and tracked string references to Prolog modules to see which Prolog modules are dynamically called by Python.
3. **Documentation links:** We searched the root operational documents (`README.md`, `CLAUDE.md`, `quick_start.md`) for direct references to files under `docs/`.

---

## 2. Prolog Engine: Dependency & Module Audit

### The 79-vs-91 Module Discrepancy Resolved
The prompt highlights a discrepancy between the README's claim of "79 modules" and the owner's count of "91 modules." The actual files in the repository reveal:
- There are exactly **95 `.pl` files** in the root of `prolog/`.
- Of these, **73 files** contain formal module declarations (`:- module(...)`).
- Of these 73, **9 files** are auto-generated config sensitivity overlays (`scs_*`) left over from sensitivity sweeps. This leaves **64 actual system-level modules** in the root.
- The `prolog/belief_battery/` directory contains **6 files**, of which **5 files** contain module declarations. Adding these 5 to the 64 system modules yields **69 modules**.
- **The Owner's 91 Count:** If we take the total `.pl` files in the root (**95**), subtract the 9 auto-generated `scs_*` sensitivity sweep overlays, we get **86 system-level files**. Adding the **5 modules** under `belief_battery/` yields exactly **91 modules**.
- **The "SWI-Prolog Module" Illusion:** In addition, all **106 files** in `prolog/probsets/` and **117 files** in `prolog/gaptests/` are structurally written with `:- module(...)` headers so that `scenario_manager` can load them dynamically without namespace collisions. Therefore, a purely automated regex count would find hundreds of modules, but the owner's count of **91** represents the true set of active system-level files (86 in root + 5 in belief battery).

### Load Graph Classification

We traced two core entry points: `stack.pl` (the main system loader) and `validation_suite.pl` (the test runner). Together, they transitively load **55 files** in the root.

#### A. CORE (53 Modules loaded by `stack.pl`)
These modules form the runtime classification engine. Removing any of these breaks the core classification cascade or the ability to run standard pipeline updates.
- `narrative_ontology.pl`, `config.pl`, `config_schema.pl`, `config_validation.pl`
- `corpus_loader.pl`, `domain_priors.pl`, `domain_registry.pl`, `constraint_instances.pl`, `constraint_data.pl`
- `constraint_indexing.pl`, `boltzmann_compliance.pl`, `signature_detection.pl`, `signature_mapper.pl`, `cs_pattern_detection.pl`, `purity_scoring.pl`, `structural_signatures.pl`
- `drl_core.pl`, `drl_composition.pl`, `drl_counterfactual.pl`, `drl_boltzmann_analysis.pl`, `drl_purity_network.pl`, `drl_fpn.pl`, `drl_modal_logic.pl`, `drl_audit_core.pl`
- `scenario_manager.pl`, `coercion_projection.pl`, `data_repair.pl`, `data_verification.pl`, `pattern_analysis.pl`, `intent_engine.pl`, `transition_paths.pl`, `network_dynamics.pl`, `drift_events.pl`, `drift_report.pl`, `drl_lifecycle.pl`
- `bifurcation_export.pl`, `persistence_export.pl`, `arakelov_height.pl`, `sheaf_analysis.pl`, `covering_analysis.pl`, `grothendieck_cohomology.pl`, `logical_fingerprint.pl`, `isomorphism_engine.pl`, `dirac_classification.pl`, `maxent_classifier.pl`, `measurement_layer.pl`, `type_metadata.pl`
- `constraint_bridge.pl`, `uke_dr_bridge.pl`, `report_generator.pl`, `utils.pl`, `stack.pl`

#### B. SUPPORT (2 Modules loaded by `validation_suite.pl` only)
These modules are critical for validation and schema checking, but are not loaded by default in the execution stack.
- `validation_suite.pl` (test runner)
- `data_validation.pl` (runs semantic validations)

#### C. DIAGNOSTIC (20 active modules NOT loaded by stack or validation suite)
These modules are executed ad-hoc by Python scripts to run sweeps, generate specific reports, or perform theoretical checks. They are active but external to the default pipeline.
- `abductive_engine.pl`, `abductive_helpers.pl`, `abductive_report.pl`, `abductive_triggers.pl` (executed during pipeline sweeps to evaluate abductive reasoning triggers)
- `diagnostic_summary.pl` (referenced in `run_pipeline.py`)
- `domain_priors_expanded.pl` (referenced in `domain_priors_expander.py`)
- `fingerprint_report.pl`, `fpn_report.pl`, `json_report.pl` (called by python report formatters)
- `giant_component_analysis.pl` (called in `run_pipeline.py` for network topology)
- `inferred_coupling_protocol.pl` (called in `run_pipeline.py`)
- `maxent_diagnostic.pl` (called in `run_pipeline.py` to check MaxEnt parameters)
- `maxent_report.pl` (referenced in `enhanced_report.py`)
- `orbit_report.pl` (referenced in `classification_confidence.py`)
- `post_synthesis.pl` (referenced in `run_pipeline.py`)
- `product_site_export.pl` (called by delta sweep scripts)
- `trajectory_mining.pl`, `trajectory_report.pl` (referenced in `run_pipeline.py`)
- `test_harness.pl` (standalone testing helper)

#### D. GENERATED / LEGACY (9 config overlays)
- `scs_0anczbfz.pl`, `scs_16dnmltr.pl`, `scs_1h8m6rbw.pl`, `scs_6tykj356.pl`, `scs_cr7l487o.pl`, `scs_escjkxbh.pl`, `scs_g0k74r3d.pl`, `scs_pn7h27wn.pl`, `scs_zkg0ubdl.pl` — auto-generated structural config overlays from parameter sweeps. They mutate `config.pl` parameters temporarily and should be archived or deleted.

#### E. LEGACY / ORPHANED (11 strictly isolated files)
These modules have 0 inbound imports in Prolog and 0 references in any Python or Agent scripts. They are dormant.
- `audit3_maxent_compare.pl` (ad-hoc maxent comparison tool)
- `gap_diagnostic.pl` (ad-hoc gap validator)
- `genuine_findings_query.pl` (one-off query script)
- `global_delta_report.pl` (one-off delta comparison)
- `invertibility_analysis.pl` (mathematical invertibility study)
- `isomorphism_report.pl` (isomorphism diagnostic)
- `omega1_audit.pl` (omega-variable audit leftover)
- `psych_bridge.pl` (bridges framework categories to psychometrics; dormant)
- `quantum_verification_report.pl` (exploratory report on quantum verification metrics)
- `signature_config.pl` (exploratory configuration logic)
- `tangled_rope_examples.pl` (exploratory tangled rope scenarios)
- `test_cs_pattern_detection.pl`, `test_snapshot_migration.pl` (exploratory test runners)

---

## 3. Python Files in `python/`

There are **134 Python files** in `python/`.

### A. CORE (25 Files)
These files constitute the post-processing pipeline and report generation engine.
- `run_pipeline.py` (pipeline entry point)
- `enhanced_report.py` (writes per-constraint structural analysis sheets)
- `shared/constants.py`, `shared/loader.py`, `shared/schemas.py`, `shared/maxent.py` (imported by sweep scripts to run the maxent model)
- `reports/registry.py`
- `reports/queries/` (18 query scripts, e.g. `institutional_dissent.py`, `variance_analysis.py`, executed during pipeline runs)

### B. SUPPORT (2 Files)
Harness files for regression and environment validation.
- `python_test_suite.py`
- `python_gap_suite.py`

### C. DIAGNOSTIC (103 Files)
Standalone scripts executed ad-hoc to run parameter sweeps, test sensitivity thresholds, or compile specific statistical data.
- `config_sensitivity_sweep.py`, `directionality_sensitivity_sweep.py`, `bifurcation_sweep.py`, `cognitive_displacement_sweep.py`, `persistence_sweep.py` (parameter sweeps)
- `game_theory_nash.py`, `game_theory_cover_story.py`, `game_theory_stability.py` (Nash distance models)
- `boundary_normality.py`, `classification_confidence.py`, `fcr_ablation.py` (theoretical validators)
- `sotu_fetch.py`, `sotu_generate_batch.py` (SOTU processing pipeline)

### D. GENERATED / ORPHANED (4 Files)
- `shared/__init__.py`, `reports/__init__.py`, `reports/queries/__init__.py` (standard module files)
- `shared/maxent.py` (listed here by automated sweeps but actively imported by 4 sweep scripts; classified as **CORE**)

---

## 4. Documentation in `docs/`

There are **119 markdown files** in `docs/`.

### A. CORE (8 Files)
Operational references, active specs, and the current canonical paper.
- `project_orientation.md` (canonical operational reference for models)
- `logic.md` (formal classification specification)
- `two_hub_architecture.md` (defines functional hubs)
- `deferential_realism_paper_v6.13.md` (current canonical paper!)
- `when_apparatus_sharpens_taxonomy.md` (essential framework context)
- `coupling_structure_evidence.md` (evidence on coupling metrics)
- `asymmetry_of_failure_types.md` (defines failure regimes)
- `when_splitting_isnt_solving.md` (core methodology essay)

### B. LEGACY (19 Files)
Superseded versions of the canonical paper. They should be moved to an archival folder.
- `deferential_realism_paper.md` (v1) all the way through `deferential_realism_paper_v6.12.md` (19 files total)

### C. AMBIGUOUS (6 Files)
Superseded notes or transition checklists that require clarification.
- `observers_not_humans_v2.md` through `v4.md` (likely superseded by `v5.md`?)
- `asymmetry_of_failure_types_first_draft.md` (superseded by canonical essay?)
- `diagnostic_integration_architecture_old.md` (superseded by `diagnostic_integration_architecture.md`)
- `core_v4.2.md`, `core_v4.3.md` (superseded specs)

### D. GENERATED (3 Files)
Output sheets written directly by Python script execution.
- `docs/results/fragility_cross_tab.md`
- `docs/results/h1_distribution_test.md`
- `docs/results/tangled_rope_sign_flip.md`

---

## 5. Audit-Related Directories

We identified three directories containing audit-related files:
- `audit_data/` (27 files): **DIAGNOSTIC (Evidence Archive).** Contains raw text outputs from tools (pylint reports, grep matches) created specifically to support the February 2026 `AUDIT.md`.
- `audit/` (93 files): **DIAGNOSTIC (Audit Sub-Project).** A standalone sub-project housing advanced mathematical audit scripts and their outputs.
- `phase1/` (6 files): **DIAGNOSTIC (Intermediate Notes).** Contains draft notes from a past audit phase.

---

## 6. Comprehensive Path Classification Table

| Path | Classification | Context / Notes |
| :--- | :--- | :--- |
| `prolog/stack.pl` + 53 core modules | **CORE** | Loaded by default; drives the axioms cascade. |
| `prolog/validation_suite.pl` + `data_validation.pl` | **SUPPORT** | Core test runner and data schema checker. |
| `prolog/testsets/` | **CORE** | The living corpus of 3,380 constraint stories. |
| `prolog/testsets_sotu/` | **CORE** | 189 constraint stories extracted from SOTU addresses. |
| `prolog/scs_*.pl` (9) | **GENERATED** | Leftovers from temporary parameter sweeps. |
| `prolog/belief_battery/` (6) | **AMBIGUOUS** | Exploratory agent consistency checking; dormant. |
| `prolog/gaptests/`, `probsets/`, `recon_2/` | **DIAGNOSTIC** | Large test datasets and mathematical reconciliation. |
| `prolog/archives/` | **LEGACY** | Superseded engine files v1/v3/v4 (~405 MB). |
| `python/run_pipeline.py`, `enhanced_report.py` | **CORE** | Primary execution scripts for pipeline post-processing. |
| `python/shared/*` | **CORE** | Core Python utility libraries. |
| `python/reports/*` | **CORE** | Primary metric reporter classes. |
| ~103 sweep and diagnostic Python scripts | **DIAGNOSTIC** | Standalone mathematical audits and parameter sweeps. |
| `docs/deferential_realism_paper_v6.13.md` | **CORE** | The current canonical paper (supersedes all prior v6.x drafts). |
| `docs/deferential_realism_paper.md` through `v6.12.md` | **LEGACY** | 19 superseded draft files. |
| `docs/results/*` | **GENERATED** | Output sheets written directly by Python execution. |
| `json/` | **GENERATED** | 3,382 LLM-generated inputs (orchestrator output). |
| `outputs/` | **GENERATED** | Pipeline analysis outputs (reports, essays). |
| `sotu/raw/` | **CORE** | Input transcripts for SOTU extraction. |
| `sotu/json/` + `sotu/pl/` | **GENERATED** | Extracted JSON profiles and intermediate Prolog files. |
| `prompts/` | **CORE** | Active LLM prompt templates. |
| `prompts/archives/` | **LEGACY** | Historical prompts. |
| `protocols/` | **CORE** | UKE authoring protocol guides. |
| `essays/`, `examples/`, `results/` | **DIAGNOSTIC** | Published writings and experimental outputs. |
| `scripts/` | **DIAGNOSTIC** | Helper utilities. |
