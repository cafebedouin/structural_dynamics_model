# Migration Plan & Risk Assessment

This document maps out the specific steps required to transition the `structural_dynamics_model` repository to the proposed layout. It identifies inbound references that will break, provides a risk classification for each move, and surfaces open decisions requiring owner input.

---

## 1. Move-by-Move Impact Analysis

### Move 1: Core Prolog Modules to `src/prolog/`
- **Path Shift:** `prolog/[system_modules].pl` → `src/prolog/`
- **Broken References:**
  - **CLAUDE.md & quick_start.md:** Shell commands reference `cd prolog && swipl ...`. These must be updated to `cd src/prolog && swipl ...`.
  - **Python Scripts:** Pipeline execution scripts (e.g., `python/run_pipeline.py`, `agent/c-orchestrator.py`) invoke SWI-Prolog by executing processes in the `prolog/` working directory. The working directory paths in these scripts must be updated to `src/prolog/`.
- **Risk Classification:** **REQUIRES UPDATE (Medium)**
  - *Mitigation:* The system modules use relative search paths internally: `prolog_load_context(directory, Dir), asserta(user:file_search_path(library, Dir))`. Since all system modules are moved together, internal imports within the Prolog engine will remain intact.

### Move 2: validation_suite.pl to `tests/prolog/`
- **Path Shift:** `prolog/validation_suite.pl` + `prolog/data_validation.pl` → `tests/prolog/`
- **Broken References:**
  - **CLAUDE.md & quick_start.md:** Testing commands reference `cd prolog && swipl -g "[validation_suite]..."`.
  - **Prolog Internal Loads:** `validation_suite.pl` imports `scenario_manager`, `data_validation`, and `report_generator`. Once moved to `tests/prolog/`, it will fail to load these files because they now reside in `src/prolog/`.
- **Risk Classification:** **REQUIRES UPDATE (High)**
  - *Mitigation:* We must update `tests/prolog/validation_suite.pl` to register `../../src/prolog/` in its SWI-Prolog file search path at boot time:
    ```prolog
    :- prolog_load_context(directory, TestDir),
       atom_concat(TestDir, '/../../src/prolog', SourceDir),
       asserta(user:file_search_path(library, SourceDir)).
    ```

### Move 3: Main Constraint Corpus to `corpus/main/`
- **Path Shift:** `prolog/testsets/` → `corpus/main/`
- **Broken References:**
  - **python/run_pipeline.py:** Hardcodes `prolog/testsets` to scan for active scenarios.
  - **python/linter.py:** Scans `prolog/testsets` for semantic lint errors.
  - **agent/c-orchestrator.py:** Automatically writes generated Prolog facts to `prolog/testsets/`.
  - **README.md & CLAUDE.md:** Refer to the corpus path.
- **Risk Classification:** **REQUIRES UPDATE (High)**
  - *Mitigation:* We must update the data paths in `run_pipeline.py`, `linter.py`, and the agent orchestrator `c-orchestrator.py` in lockstep with the file move.

### Move 4: SOTU Corpus to `corpus/SOTU/`
- **Path Shift:** `prolog/testsets_sotu/` + `/sotu/` → `corpus/SOTU/`
- **Broken References:**
  - **python/run_pipeline.py:** Scans `prolog/testsets_sotu/` to process SOTU constraints.
  - **sotu_fetch.py & sotu_generate_batch.py:** Write generated SOTU Prolog scenarios to `prolog/testsets_sotu/`.
- **Risk Classification:** **REQUIRES UPDATE (High)**
  - *Mitigation:* Update SOTU processing path variables in Python scripts.

### Move 5: JSON Generation Inputs to `corpus/inputs/`
- **Path Shift:** `json/` → `corpus/inputs/`
- **Broken References:**
  - **agent/c-orchestrator.py:** Writes LLM-generated JSON constraint specifications to `/json/` before converting them to Prolog.
  - **python/run_pipeline.py:** Reads files under `/json/` to cross-validate metrics.
- **Risk Classification:** **REQUIRES UPDATE (High)**
  - *Mitigation:* Update the active authoring path in `c-orchestrator.py` and the lookup path in `run_pipeline.py`.

### Move 6: Analytical Essays to `outputs/essay_drafts/`
- **Path Shift:** `essays/` → `outputs/essay_drafts/`
- **Broken References:**
  - **agent/c-orchestrator.py:** Automatically dumps Sonnet-drafted essays to `/essays/`.
- **Risk Classification:** **REQUIRES UPDATE (Medium)**
  - *Mitigation:* Update the draft essay output path in the orchestrator script.

### Move 7: Historical Audits to `history/audits/`
- **Path Shift:** `AUDIT.md` + `audit_data/` + `phase1/` → `history/audits/`
- **Broken References:**
  - **AUDIT.md:** References raw text logs inside `audit_data/` (e.g. `audit_data/pylint_results.txt`).
- **Risk Classification:** **REQUIRES UPDATE (Low)**
  - *Mitigation:* We must update file links inside `AUDIT.md` (or the new `history/audits/AUDIT.md`) to point to their new relative location under `history/audits/audit_data/`.

### Move 8: Superseded Paper Drafts to `history/paper_archive/`
- **Path Shift:** `docs/deferential_realism_paper.md` through `v6.12.md` → `history/paper_archive/`
- **Broken References:**
  - **README.md:** References `docs/deferential_realism_paper_v6.11.md`.
- **Risk Classification:** **REQUIRES UPDATE (Low)**
  - *Mitigation:* Update `README.md` to reference the current canonical paper version: `docs/essays/deferential_realism_paper_v6.13.md` (which stays in the active tree).

---

## 2. Risk & Impact Matrix

| Proposed Move | Source Path | Destination Path | Risk Level | Active References |
| :--- | :--- | :--- | :--- | :--- |
| Move 1 | `prolog/[core]` | `src/prolog/` | **Medium** | Python runner subprocess calls, `CLAUDE.md`, `quick_start.md`. |
| Move 2 | `prolog/validation_suite.pl` | `tests/prolog/` | **High** | Prolog load paths, `CLAUDE.md`, `quick_start.md` commands. |
| Move 3 | `prolog/testsets/` | `corpus/main/` | **High** | `run_pipeline.py`, `linter.py`, `c-orchestrator.py`, docs. |
| Move 4 | `prolog/testsets_sotu/` | `corpus/SOTU/` | **High** | `run_pipeline.py`, `sotu_generate_batch.py`, SOTU scripts. |
| Move 5 | `json/` | `corpus/inputs/` | **High** | `c-orchestrator.py` output paths, `run_pipeline.py`. |
| Move 6 | `essays/` | `outputs/essay_drafts/` | **Medium** | `c-orchestrator.py` essay generation dump path. |
| Move 7 | `AUDIT.md`, `audit_data/` | `history/audits/` | **Low** | Documentation links inside `AUDIT.md`. |
| Move 8 | `docs/paper_v1` to `v6.12` | `history/paper_archive/`| **Low** | `README.md` references to superseded `v6.11`. |

---

## 3. Critical Open Decisions (Requires Owner Input)

### 1. Retention of `scs_*.pl` Config Overlays
- **Ambiguity:** We identified 9 auto-generated `scs_*` (structural config sensitivity) Prolog files in the root of `/prolog/`. They temporarily mutate configuration parameters for mathematical sweeps and are currently checked into Git.
- **Decision Needed:** Should these generated overlays be completely deleted, or should we isolate them under `history/sweep_cache/`?
- **Our Recommendation:** We recommend moving them to `history/sweep_cache/` for scientific provenance if the specific runs are valuable, otherwise delete them since they can be easily regenerated by the sensitivity sweep script.

### 2. Status of the `prolog/belief_battery/` Subdirectory
- **Ambiguity:** The `belief_battery/` subdirectory contains 5 dormant Prolog modules used to validate agent consistency, but it has not been modified since February 2026.
- **Decision Needed:** Is the belief battery an active extension of the engine (should it go to `src/prolog/belief_battery/`) or an orphaned research essay (should it go to `history/engine_archive/`)?
- **Our Recommendation:** Surface this to the owner. We recommend treating it as **AMBIGUOUS** and placing it in `history/engine_archive/` unless active development is planned.

### 3. Status of `docs/observers_not_humans_v2.md` through `v4.md`
- **Ambiguity:** Version 5 of the "Observers Not Humans" essay is active, but older draft versions (v2, v3, v4) remain in the documentation folder.
- **Decision Needed:** Are these older drafts required for active comparison, or can they be archived in `history/paper_archive/` along with the older paper drafts?
- **Our Recommendation:** Archive them in `history/paper_archive/` to keep the active `docs/` folder clean and focused on current canonical texts.
