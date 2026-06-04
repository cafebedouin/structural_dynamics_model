# Repository Audit & Reorganization Proposal

> [!NOTE]
> This document is the one-page summary of our initial repository audit and reorganization proposal for `structural_dynamics_model`. It links to all five supporting phase documents located in `/home/scott/bin/structural_dynamics_model/audit/agy/`.

---

## 1. Executive Summary

`structural_dynamics_model` is a rich, organic research codebase implementing the formal Deferential Realism framework. Over time, active system modules, ad-hoc sweeps, statistical experiments, and a corpus of over 3,337 constraint scenarios have been placed alongside each other, creating significant navigational noise for both human researchers and AI agent models.

This proposal establishes a clean, modern repository structure that:
1. **Isolates core execution modules** from ad-hoc diagnostic sweeps and one-off audits.
2. **Separates raw inputs** (JSON specifications) from generated analytical outputs (reports, essays).
3. **Consolidates active theoretical specifications and publications** while cleanly archiving superseded paper versions and legacy engines.
4. **Preserves the mathematical integrity** and execution environment without breaking core workflows.

---

## 2. Reorganization Blueprint

We propose organizing the repository around five primary hubs:
- **`src/` (The Code Hub):** Active Prolog core cascade modules and core Python post-pipeline scripts.
- **`diagnostics/` (The Diagnostic Hub):** 100+ standalone mathematical audit and parameter sweep scripts cleanly separated from production source files.
- **`corpus/` (The Empirical Anchor):** The single source of truth for the 3,337 constraint stories, SOTU data, and LLM JSON specs.
- **`outputs/` (The Generated Output Hub):** Post-pipeline formatted constraint reports and drafted essays.
- **`history/` (The Scientific Archive):** Completed audits, superseded papers (v1–v6.12), and historical engine drafts preserved for scientific provenance.

---

## 3. Reorganization Audit Documents

Explore the full audit results and migration roadmap below:

- **[01_inventory.md](file:///home/scott/bin/structural_dynamics_model/audit/agy/01_inventory.md):** A detailed, verified directory and file-level scan mapping modification times, file counts, and sizes.
- **[02_usage_analysis.md](file:///home/scott/bin/structural_dynamics_model/audit/agy/02_usage_analysis.md):** Deep tracing of the Prolog load graph and Python imports to classify all components as CORE, SUPPORT, DIAGNOSTIC, GENERATED, or LEGACY.
- **[03_standards.md](file:///home/scott/bin/structural_dynamics_model/audit/agy/03_standards.md):** Demarcates industry-established standards (e.g. `src/` layout, pyproject.toml) from emerging AI preambles (`CLAUDE.md`) and custom design judgments.
- **[04_proposed_structure.md](file:///home/scott/bin/structural_dynamics_model/audit/agy/04_proposed_structure.md):** Defines the exact folder hierarchy, per-directory responsibilities, and directory-level navigational README maps.
- **[05_migration_plan.md](file:///home/scott/bin/structural_dynamics_model/audit/agy/05_migration_plan.md):** Traces every path shift, lists broken process and CLI references, maps migration risks, and highlights open decisions requiring owner input.

---

## 4. Key Decisions for the Owner

> [!IMPORTANT]
> The reorganization is fully planned but *no changes will be made* until you review and approve these three critical decisions:
>
> 1. **`scs_*.pl` Config Overlays:** Should the 9 auto-generated sensitivity sweep files in `/prolog/` root be deleted or archived under `/history/sweep_cache/`? We recommend archiving them.
> 2. **`prolog/belief_battery/` Status:** Is the belief battery an active engine extension or a concluded study? We recommend archiving it under `/history/engine_archive/` unless active development is planned.
> 3. **Draft Documentation Archiving:** Can older drafts (e.g. paper v1-v6.12, "Observers Not Humans" v2-v4) be moved to `/history/paper_archive/`? We recommend archiving them to keep `/docs/` focused on active specifications.
