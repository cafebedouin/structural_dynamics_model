# 05 — Migration Plan and Risks

For each proposed move, this document lists: source → destination, inbound references that
would break, and risk classification.

Reference check sources for every move:
- README.md, CLAUDE.md, quick_start.md, AUDIT.md
- docs/project_orientation.md (functions as second CLAUDE.md for path purposes)
- All .py files in python/ and agent/
- All .pl files in prolog/ (excluding testsets/)
- All .md files in docs/
- pyproject.toml, requirements.txt, scripts/*.sh

Risk levels:
- **SAFE** — no inbound references found
- **REQUIRES UPDATE** — references found and enumerated; changes are mechanical
- **DECISION NEEDED** — ambiguous; surface to owner before executing

---

## Proposed Moves

### M1 — Add `.env` to `.gitignore`

**Action:** Append `.env` to `.gitignore`  
**Inbound references:** None  
**Risk:** SAFE  
**Note:** This is an edit, not a move. Flagged in Feb 2026 audit; still unresolved.

---

### M2 — Add per-directory `README.md` files

**Action:** Create new README.md in: `prolog/`, `python/`, `docs/`, `sotu/`, `audit/`,
`audit_data/`, `phase1/`  
**Inbound references:** None (new files)  
**Risk:** SAFE  
**Note:** Pure additions; no existing paths change.

---

### M3 — Move `scs_*.pl` to `prolog/generated/`

**Action:** Move 9 files: `prolog/scs_*.pl` → `prolog/generated/scs_*.pl`

**Inbound references searched:** All .pl files, all .py files, all .md files.

| Source | Reference found? |
|--------|-----------------|
| Any `use_module(scs_*)` in .pl files | No |
| Any path reference to scs_* in .py files | No |
| `phase1/independent_module_descriptions.md` | Yes — text reference only, no load path |
| Any shell scripts | No |

**References that would break:** `phase1/independent_module_descriptions.md` mentions
`scs_*.pl` by name but does not reference their path. The text would need a one-line update
to say "now in prolog/generated/".

**Risk:** REQUIRES UPDATE — one text reference in phase1/.  
**Prerequisite:** Owner decision from 04_proposed_structure.md §3.

---

### M4 — Move `results/perspective_experiment/` to `outputs/perspective_experiment/`

**Action:** Move `results/perspective_experiment/*` → `outputs/perspective_experiment/`  
Then `results/` directory becomes empty and can be removed (owner decision).

**Inbound references searched:** all key files, all .py files.

| Source | Reference found? |
|--------|-----------------|
| CLAUDE.md | No |
| quick_start.md | No |
| README.md | No |
| AUDIT.md | No |
| docs/project_orientation.md | No (project_orientation references docs/results/, not top-level results/) |
| Any .py file in python/ | No writes to results/ found |
| Any .py file in agent/ | Not checked — recommend owner verify |

**Risk:** SAFE based on checks performed.  
**Prerequisite:** Owner decision from 04_proposed_structure.md §"results/ Disposition".

---

### M5 — Move superseded paper versions to `docs/archive/` (Option A)

**Action:** Move 16 files:
- `docs/deferential_realism_paper.md` → `docs/archive/deferential_realism_paper.md`
- `docs/deferential_realism_paper_v2.md` through `docs/deferential_realism_paper_v6.10.md`
  (15 more files) → `docs/archive/`

Files moved:
`deferential_realism_paper.md`, `deferential_realism_paper_v2.md`,
`deferential_realism_paper_v3.md`, `deferential_realism_paper_v4.md`,
`deferential_realism_paper_v4_notes.md`, `deferential_realism_paper_v5.md`,
`deferential_realism_paper_v6.md`, `deferential_realism_paper_v6.2.md`,
`deferential_realism_paper_v6.3.md`, `deferential_realism_paper_v6.3_notes.md`,
`deferential_realism_paper_v6.4.md`, `deferential_realism_paper_v6.5.md`,
`deferential_realism_paper_v6.6.md`, `deferential_realism_paper_v6.7.md`,
`deferential_realism_paper_v6.8.md`, `deferential_realism_paper_v6.9.md`

**Inbound references searched:**

| Source | Reference to superseded versions? |
|--------|----------------------------------|
| CLAUDE.md | No — only references v6.11 |
| quick_start.md | No — does not mention any paper version |
| README.md | References v6.11 only ("current paper is v6.11") |
| docs/project_orientation.md | References v6.11 only |
| Any Python scripts | No — paper versions are not read by pipeline |
| Any Prolog files | No |
| docs/*.md cross-references | Searched; no doc references superseded versions by path |

**Risk:** SAFE — no inbound references to the 16 superseded versions found.

---

### M6 — Consolidate `audit/` + `audit_data/` + `phase1/` (Option A only — Decision Required)

**Action:** If owner chooses Option A from 04_proposed_structure.md:
- `audit/*.py` → `audit_history/scripts/`
- `audit/outputs/` → `audit_history/reports/`
- `audit/original_outputs/` → `audit_history/reports/original/`
- `audit/outputs_haiku/` → `audit_history/reports/haiku/`
- `audit_data/*.txt` → `audit_history/evidence/`
- `phase1/*` → `audit_history/phase1/`

**Inbound references that would break:**

| Source | Reference | Breakage |
|--------|-----------|---------|
| AUDIT.md:6 | "`audit_data/` outputs" (general reference) | Text would be inaccurate |
| AUDIT.md:59 | `audit_data/verify_delegation.txt` (specific file) | Path reference breaks |
| AUDIT.md:87 | `audit_data/config_params_unused.txt` (specific file) | Path reference breaks |
| AUDIT.md:271 | "traceable to `audit_data/` outputs" (general) | Text would be inaccurate |
| CLAUDE.md | No references to audit/, audit_data/, or phase1/ | No breakage |
| quick_start.md | No references | No breakage |
| README.md | No references | No breakage |
| docs/project_orientation.md | No references to audit_data/ | No breakage |

**Risk:** REQUIRES UPDATE — AUDIT.md has 4 references to audit_data/, 2 with specific filenames.

**Additional concern:** AUDIT.md is a point-in-time evidence document. Updating path references
in it changes the document that certifies the audit chain. The owner should decide whether the
audit_data/ path references in AUDIT.md should be updated or whether a forwarding note should be
added instead (e.g., "audit_data/ has been moved to audit_history/evidence/").

**Risk (phase1/):** SAFE — no file references phase1/ by path in the checked sources.

**DECISION NEEDED:** Owner must choose between Options A, B, C from 04_proposed_structure.md
before this migration can proceed.

---

### M7 — Docs full restructure (Option B — Decision Required)

**Action:** If owner chooses Option B from 04_proposed_structure.md, the following paths change:

| Current path | New path |
|-------------|---------|
| `docs/project_orientation.md` | `docs/guides/project_orientation.md` |
| `docs/logic.md` | `docs/framework/logic.md` |
| `docs/deferential_realism_paper_v6.11.md` | `docs/paper/deferential_realism_paper_v6.11.md` |
| `docs/two_hub_architecture.md` | `docs/framework/two_hub_architecture.md` |
| `docs/when_apparatus_sharpens_taxonomy.md` | `docs/analyses/when_apparatus_sharpens_taxonomy.md` |
| [~90 more files] | various |

**Inbound references that would break:**

| Source | References canonical doc paths? | Breakage |
|--------|--------------------------------|---------|
| CLAUDE.md | Yes — `docs/deferential_realism_paper_v6.11.md`, `docs/logic.md`, `docs/project_orientation.md` | All 3 break |
| README.md | Yes — `docs/deferential_realism_paper_v6.11.md` | Breaks |
| quick_start.md | No doc/ paths | No breakage |
| docs/project_orientation.md | Self-references + ~15 other docs/ paths | ~15 break |
| docs/observers_not_humans_v5.md | References docs/results/fragility_cross_tab.md | Breaks |
| Many docs/*.md files | Cross-reference each other extensively | Extensive breakage |

**Risk:** REQUIRES UPDATE — extensive. CLAUDE.md, README.md, project_orientation.md, and
~15 other docs require simultaneous path updates. The `docs/` cross-reference graph is dense
(project_orientation.md is referenced 56 times within docs/; logic.md is referenced 69 times).

**Recommendation:** Do NOT execute Option B until Option A has been applied and settled.
Option B requires a comprehensive reference audit and lockstep multi-file update.

---

## Open Issues Flagged for Owner

### I1 — 79 vs. 91 Module Count

The prompt for this audit referenced "79 modules" as claimed by the README. The current README
says "91-module Prolog engine" in its introduction. The structure diagram says "[75 additional
modules]" alongside 7 named modules, giving ~83 — an undercount. The actual file count is 91
.pl files (verified by `find prolog/ -maxdepth 1 -name "*.pl" | wc -l`). No source currently
claims 79. If 79 was an accurate count at some prior point, the discrepancy has been resolved
by subsequent module additions. No action needed; documenting for completeness.

### I2 — `prolog/archives/` Not in README

405 MB, 6,922 files, 4 historical engine versions — invisible from the README. This is the
repo's largest undocumented mass. **Recommended action:** Add a single paragraph to README.md
under "Repository Structure" and/or to `prolog/README.md` (if created) explaining that
archives/ contains historical engine and dataset versions for provenance and are not loaded
by the active stack.

### I3 — `sotu/pl/` Duplication with `prolog/testsets_sotu/`

189 files with identical content in two locations. The synchronization mechanism is undocumented.
**Recommended action:** Owner documents whether this is: (a) intentional staging/deploy pattern,
(b) accidental duplication, or (c) a symlink that should be created.

### I4 — `docs/results/` Receives Python Output

`python/tangled_rope_sign_flip.py` writes to `docs/results/tangled_rope_sign_flip.md`.
This means the docs/ tree has an active write target inside it. **Recommended action:**
Owner decides whether docs/results/ should be moved to outputs/ and the Python script
updated to write there instead. The current arrangement works but obscures that docs/ is
not read-only.

### I5 — `prolog/probsets/`, `gaptests/`, `belief_battery/` Unclassified

214 + 117 + 7 files. No references found in Python, shell scripts, or key docs. These may
be used in subprocess calls from Python (not traced), or they may be dormant.
**Owner should verify** before any action. If unused, candidates for prolog/archives/.

### I6 — `prolog/recon_2/` Status

Created in the most recent commit (2026-05-15). Contains 4 markdown files. Whether this is
in-progress work, a completed audit pass, or historical material is unclear. **Owner should
document** its status in the new prolog/README.md.

### I7 — CLAUDE.md Path Integrity (Any Move Involving Canonical Docs)

CLAUDE.md says: "Start here: `docs/project_orientation.md`". If project_orientation.md ever
moves (Option B), CLAUDE.md must be updated simultaneously. This is a HARD dependency.
CLAUDE.md is the first file any AI model reads; a broken path in it immediately degrades
the model's ability to work in the repo. Any move touching docs/project_orientation.md,
docs/logic.md, or docs/deferential_realism_paper_v6.11.md must update CLAUDE.md in the
same commit.

---

## Migration Execution Order (if owner approves)

Recommended order minimizes dependency failures:

1. M1 (`.gitignore` update) — no risk, immediate
2. M2 (add per-directory READMEs) — pure additions, no risk
3. M5 (superseded paper versions → docs/archive/) — SAFE, no reference updates
4. M4 (results/ → outputs/) — SAFE pending agent/ verification
5. M3 (scs_*.pl → prolog/generated/) — one text update in phase1/
6. M6 (audit consolidation) — only if owner chooses Option A; update AUDIT.md simultaneously
7. M7 (docs full restructure) — only after M6 is settled; extensive lockstep updates required

Moves 6 and 7 should not be combined in a single commit. Each should be a separate,
reviewable git commit with the path-reference updates included.
