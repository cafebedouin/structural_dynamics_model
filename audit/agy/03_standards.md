# Standards Survey: Established vs. Judgment Conventions

This document outlines the architectural standards and conventions guiding the repository reorganization proposal. In alignment with professional software engineering principles, we clearly demarcate widely established standards, emerging conventions, and custom design judgments.

---

## 1. Established Conventions (Industry Standards)

These conventions represent widely documented, standard practices within the software engineering community.
- **Root-Level Configuration (PEP 518 / PEP 621):** Declaring dependency metadata and tool configurations in `pyproject.toml` at the project root.
- **Root-Level Meta-Files:** Placing files like `LICENSE`, `README.md`, and `requirements.txt` at the root for standard packaging and discovery.
- **Python Source Layout (`src/` structure):** Isolating execution source code under a dedicated `src/` directory. This prevents packaging tool issues and avoids namespace contamination during local development.
- **Standard Testing Directory (`tests/`):** Grouping automated test harness scripts in a separate root-level `/tests/` directory rather than distributing them inside the production code folders.
- **Standard Documentation Directory (`docs/`):** Using a dedicated `/docs/` tree to house framework papers, specification sheets, and guides.
- **Standard Examples Directory (`examples/`):** Placing usage dossiers and demonstrations in a separate `/examples/` folder.

---

## 2. Emerging Conventions

These represent common, active practices that are gaining traction, especially in the context of LLM integration and developer onboarding, but are not yet formal standards.
- **`CLAUDE.md` / `AGENTS.md` at Root:** Providing explicit, dense development guides (typical workflows, compilation commands, and project invariants) specifically optimized for AI coding assistants.
- **Per-Directory `README.md` Files:** Including localized, concise README files in subdirectories to act as navigational maps for both human developers and LLM context loaders.

---

## 3. Design Judgment (Custom Decisions)

These are unique architectural choices made to fit the specific needs of this research codebase. They are not industry standards, but are proposed as logical solutions for this framework.
- **Preservation of the 3,337-Constraint Corpus in `prolog/testsets/`:** We explicitly preserve the internal structure of the constraint corpus. It is highly specific and custom, and moving its parent should not trigger unnecessary file shuffles within the dataset itself.
- **Isolation of Generative Inputs (`json/`):** The `json/` directory contains LLM-generated constraint *specifications* (inputs) rather than outputs. Proposing to keep them adjacent to but separated from source and generated output is a pragmatic project-specific choice.
- **Demarcation of Audit History (`archives/` and `audit_history/`):** Segmenting completed ad-hoc sweeps and the February 2026 audit logs from active code to prevent clutter, while fully retaining them for scientific provenance.
- **Grouping Python Scripts by Purpose:** Proposing to organize the 100+ diagnostic files in `python/` into logical subfolders (e.g. `sweeps/`, `models/`, `utils/`) is a custom usability judgment aimed at reducing root-level noise within the Python tree.
