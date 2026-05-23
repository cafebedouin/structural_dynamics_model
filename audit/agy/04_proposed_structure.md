# Proposed Structure: Premium Framework Layout

This document proposes a clean, logical, and highly structured top-level repository layout for `structural_dynamics_model`. It separates active source code from diagnostic sweeps, generated data inputs from pipeline outputs, and current documentation from historical archives.

---

## 1. Directory Tree Overview

```
structural_dynamics_model/
├── src/                       # Active execution source code
│   ├── prolog/                # Core SWI-Prolog engine modules (loaded by stack)
│   └── python/                # Core Python pipeline modules (loader, registry)
├── tests/                     # Test runners & schema validation
│   ├── prolog/                # validation_suite.pl, data_validation.pl
│   └── python/                # python_test_suite.py, python_gap_suite.py
├── diagnostics/               # Standalone audits & ad-hoc sweep scripts
│   ├── sweeps/                # Sensitivity, directionality, & persistence sweeps
│   └── models/                # Nash, cover story, & game theory models
├── corpus/                    # Active data inputs & constraint stories
│   ├── main/                  # The 3,337 constraint scenarios (Prolog facts)
│   ├── SOTU/                  # 189 SOTU-derived constraints & raw SOTU texts
│   └── inputs/                # 3,382 LLM-generated JSON specifications
├── agent/                     # Authoring orchestrator loop & sub-agents
├── outputs/                   # Post-pipeline generated artifacts (regeneratable)
│   ├── constraint_reports/    # Enhanced constraint analysis sheets
│   └── essay_drafts/          # Sonnet-generated essay drafts
├── docs/                      # Theoretical documentation & paper
│   ├── specs/                 # logic.md, two_hub_architecture.md, orientation
│   └── essays/                # Active theoretical papers & essays
├── history/                   # Historical record and research provenance
│   ├── audits/                # AUDIT.md, audit_data/, phase1/ (completed audits)
│   ├── paper_archive/         # Superseded papers (v1 through v6.12)
│   ├── engine_archive/        # Superseded Prolog engines (v1/v3/v4)
│   └── sweep_cache/           # Previous audit/ custom Python scripts & data
├── README.md                  # Main entry point (philosophical core)
├── CLAUDE.md                  # Development guide for human & AI models
├── quick_start.md             # CLI command cheat sheet
├── TODO.md                    # Short-term checklists
├── AGENDA.md                  # Future work & research roadmap
├── pyproject.toml             # Python build metadata
└── requirements.txt           # Python dependency lists
```

---

## 2. Directory Specifications

### `src/`
- **Purpose:** House the active execution source code of the framework (Prolog core cascade and Python orchestration modules).
- **Contents:** `src/prolog/` containing system modules loaded by `stack.pl`; `src/python/` containing core pipeline runners and shared constants.
- **What does NOT belong here:** Standalone audit sweeps, one-off diagnostics, test runners, or generated data.
- **Mapping:**
  - `prolog/` core system files → `src/prolog/`
  - `python/shared/`, `python/reports/` core files → `src/python/`
  - `python/run_pipeline.py`, `python/enhanced_report.py` → `src/python/`

### `tests/`
- **Purpose:** Group all environment validators, schema checks, and test runner configurations.
- **Contents:** Prolog `validation_suite.pl`, `data_validation.pl`, and Python regression test files.
- **What does NOT belong here:** Production logic programming or ad-hoc sweep models.
- **Mapping:**
  - `prolog/validation_suite.pl`, `prolog/data_validation.pl` → `tests/prolog/`
  - `python/python_test_suite.py`, `python/python_gap_suite.py` → `tests/python/`

### `diagnostics/`
- **Purpose:** Cleanly isolate the 100+ ad-hoc analytical sweeps, parameter sweeps, and stability models from active source code to eliminate clutter.
- **Contents:** Standalone Python sweep scripts, sensitivity tools, and game-theoretic modules.
- **What does NOT belong here:** Core classification cascades or test harnesses.
- **Mapping:**
  - `python/*_sweep.py`, `python/*_sensitivity.py` → `diagnostics/sweeps/`
  - `python/game_theory_*.py`, `python/boundary_normality.py` → `diagnostics/models/`

### `corpus/`
- **Purpose:** Act as the single source of truth for all constraint stories, generation specifications, and raw datasets.
- **Contents:** 3,337 main Prolog constraint files, 189 SOTU-derived constraint files, and 3,382 LLM-generated JSON input files.
- **What does NOT belong here:** Compiled output reports, analysis dashboards, or engine modules.
- **Mapping:**
  - `prolog/testsets/` → `corpus/main/`
  - `prolog/testsets_sotu/` + `/sotu/` → `corpus/SOTU/`
  - `json/` → `corpus/inputs/`

### `outputs/`
- **Purpose:** Gather all regeneratable outputs from post-pipeline execution.
- **Contents:** Formatted constraint reports, statistical sweep sheets, and drafted essays.
- **What does NOT belong here:** Source code, configuration parameters, or raw constraint JSONs.
- **Mapping:**
  - `outputs/constraint_reports/` → `outputs/constraint_reports/`
  - `outputs/essays/` + `essays/` → `outputs/essay_drafts/`

### `docs/`
- **Purpose:** Centralize framework publications, specifications, and active analytical essays.
- **Contents:** Current paper `deferential_realism_paper_v6.13.md`, `logic.md`, `two_hub_architecture.md`, `project_orientation.md`, and theoretical essays.
- **What does NOT belong here:** Superseded paper drafts, raw data sheets, or temporary checklists.
- **Mapping:**
  - `docs/deferential_realism_paper_v6.13.md` → `docs/essays/`
  - `docs/logic.md`, `docs/project_orientation.md` → `docs/specs/`
  - Active essays under `docs/` (e.g. `when_splitting_isnt_solving.md`) → `docs/essays/`

### `history/`
- **Purpose:** Maintain historical record and scientific research provenance.
- **Contents:** Superseded paper versions v1–v6.12, legacy engine folders, completed audit databases, and legacy sweeps.
- **What does NOT belong here:** Active specs, live pipelines, or active datasets.
- **Mapping:**
  - `AUDIT.md`, `audit_data/`, `phase1/` → `history/audits/`
  - `docs/deferential_realism_paper.md` through `v6.12.md` → `history/paper_archive/`
  - `prolog/archives/` → `history/engine_archive/`
  - `audit/` script tools → `history/sweep_cache/`

---

## 3. Navigational Maps (Per-Directory READMEs)

To facilitate navigation, we propose adding short, localized `README.md` files in crucial directories to orient developers and LLM systems:
1. **`src/README.md`:** Outlines the core Axioms classifier cascade (`drl_core.pl`), single config parameters file, and the post-processing post-pipeline.
2. **`diagnostics/README.md`:** Identifies the categories of sweep scripts (sensitivity, game-theory, persistence) and how to run them standalone.
3. **`corpus/README.md`:** Documents the constraint story schema, SOTU dataset layout, and the JSON input specs.
4. **`history/README.md`:** Outlines historical provenance, previous audit dates, and archived drafts.
