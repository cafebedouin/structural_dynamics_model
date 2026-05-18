# 04 — Proposed Structure

This document proposes a top-level layout. Where there are genuine alternatives, both
are presented with trade-offs and an explicit owner-decision header. Where a proposal
is relatively uncontroversial, it is stated as a recommendation.

**Constraint:** Pipeline entry points must be reachable in two directory hops.
**Constraint:** `prolog/testsets/` internal layout must not be reshuffled.
**Constraint:** All proposals are read-only until owner approval.

---

## Uncontroversial Proposals (Low Owner Burden)

These are straightforward based on usage analysis and have no significant trade-offs.

### 1. Keep root-level files as-is

README.md, CLAUDE.md, AUDIT.md, quick_start.md, validation_report.md, pyproject.toml,
requirements.txt, packages.txt, LICENSE, TODO.md, .gitignore — all stay at root.

**One addition:** Add `.env` to `.gitignore` (flagged in Feb 2026 audit; unfixed).

### 2. Add per-directory README.md files

**I propose** adding a one-page README.md to these directories:
- `prolog/` — explain the 91-module structure, stack.pl, testsets/ organization, and note
  archives/ is historical
- `python/` — distinguish CORE (run_pipeline.py, enhanced_report.py), SUPPORT (shared/),
  and DIAGNOSTIC (everything else)
- `docs/` — map the document families; note superseded paper versions
- `sotu/` — explain the parallel structure and relationship to prolog/testsets_sotu/

These are pure additions with no migration risk.

### 3. Mark scs_*.pl as generated artifacts

The 9 `scs_*.pl` files at `prolog/` root are generated config sensitivity overlays (produced
by `python/structural_config_sensitivity.py`). They should not live alongside named engine
modules. Two options:

**Option A (I propose):** Create `prolog/generated/` and move them there.
- Inbound references: one mention in `phase1/independent_module_descriptions.md`
- Risk: SAFE (no .pl or .py files reference them by path)

**Option B:** Leave them at prolog/ root but add a comment in stack.pl and/or the prolog/
README identifying them as generated.
- Risk: SAFE (no-op)

### 4. Document sotu/ ↔ testsets_sotu/ relationship

`sotu/pl/` and `prolog/testsets_sotu/` are confirmed identical (189 files, same content).
The pipeline reads from `prolog/testsets_sotu/`. The sotu/ Python scripts write to `sotu/pl/`,
and it appears that `sotu/pl/` is then copied to `prolog/testsets_sotu/` manually or by a
script (the mechanism is not documented).

**I propose:** Add `sotu/README.md` explaining:
- `raw/` = source SOTU texts
- `json/` = generated constraint JSON (authoring workspace)
- `pl/` = generated Prolog testsets; deployed to `prolog/testsets_sotu/` as the engine source

**Decision needed:** Does `sotu/pl/` serve any purpose beyond being a staging ground for
`prolog/testsets_sotu/`? If they are always identical, could `sotu/pl/` be replaced by a
symlink? (Owner decision — the current duplication is not harmful, just undocumented.)

### 5. Label `scripts/run_full_pipeline.sh.legacy`

The filename already signals legacy status. **I propose** no action beyond documenting it
in `scripts/` README if one is created.

### 6. Remove or archive `prompts/archives/`

`prompts/archives/` contains 13 historical prompt versions with no inbound references.
Git already preserves this history. **I propose** flagging for owner decision on deletion
(cannot delete in proposal mode).

---

## Decision Required: `docs/` Structure

**Context:** `docs/` contains 107+ .md files. 17 are paper versions of the framework paper,
of which 16 are superseded by v6.11. The directory also contains canonical operational docs,
working notes, diagnostic reports, and generated output (`docs/results/`).

### Option A — Light Touch (I lean toward this for first pass)

Move only the 16 superseded paper versions to `docs/archive/`. Everything else stays in place.

```
docs/
├── archive/                    ← NEW: 16 superseded paper versions
│   ├── deferential_realism_paper.md  (v1)
│   ├── deferential_realism_paper_v2.md
│   ├── ... (v3 through v6.10)
│   └── deferential_realism_paper_v6.10.md
├── deferential_realism_paper_v6.11.md  ← canonical
├── logic.md, project_orientation.md, two_hub_architecture.md  ← canonical
├── when_apparatus_sharpens_taxonomy.md  ← canonical
├── [all other current docs stay in place]
└── results/                    ← stays; Python scripts write here
```

**Trade-offs:**
- Pro: Minimal disruption. Only 16 files move. CLAUDE.md, project_orientation.md, and all
  cross-references remain valid. No reference updates needed.
- Pro: Reversible with `git mv docs/archive/* docs/`.
- Con: Does not address the other 10–15 potentially superseded working-notes files
  (observers_not_humans v2/v3/v4, _first_draft versions, _old versions).
- Con: Leaves docs/ at ~90 files with mixed purposes.

**References that would break:** None identified. The superseded versions are not referenced
by CLAUDE.md, quick_start.md, README.md, or project_orientation.md.

### Option B — Full Restructure

Organize docs/ by purpose:

```
docs/
├── paper/
│   └── deferential_realism_paper_v6.11.md
├── framework/
│   ├── logic.md, core_v4.3.md, two_hub_architecture.md
│   ├── categorical_architecture.md
│   ├── [formal spec docs]
├── analyses/
│   ├── when_apparatus_sharpens_taxonomy.md
│   ├── when_consensus_isnt_coherence.md
│   ├── coupling_structure_evidence.md
│   ├── [all "when_*.md" papers, contextuality_paper, observers_not_humans_v5]
├── guides/
│   ├── project_orientation.md
│   ├── debugging_philosophy.md
│   ├── [operational guides]
├── archive/
│   ├── [16 superseded paper versions]
│   ├── [working notes: *_first_draft.md, *_old.md, v2/v3/v4 of multi-version docs]
└── results/
    └── [3 generated files]
```

**Trade-offs:**
- Pro: Clear navigation for any model entering the repo; one-glance orientation.
- Con: CLAUDE.md references `docs/project_orientation.md`, `docs/deferential_realism_paper_v6.11.md`,
  `docs/logic.md` — all three paths change under Option B. CLAUDE.md, README.md, and
  project_orientation.md itself must be updated simultaneously.
- Con: project_orientation.md references ~15 other docs/ paths — all require updates.
- Con: Higher risk of breaking cross-references between docs that weren't fully traced.
- Con: Requires owner to classify ~15 ambiguous working-notes files before execution.

**Recommendation:** Start with Option A. It captures 90% of the navigational benefit
(removing 16 superseded paper versions) with near-zero risk. Option B can follow after
the owner resolves the AMBIGUOUS working-notes files (see 02_usage_analysis.md).

---

## Decision Required: Audit Directory Consolidation

**Context:** Three directories hold audit artifacts: `audit/`, `audit_data/`, `phase1/`.

### Option A — Merge into `audit_history/`

```
audit_history/
├── scripts/        ← from audit/*.py
├── evidence/       ← from audit_data/*.txt
├── reports/        ← from audit/outputs*, audit/original_outputs*
└── phase1/         ← from phase1/
```

**Trade-offs:**
- Pro: One place for all audit material; clear that this is historical not active.
- Pro: CLAUDE.md and quick_start.md have no references to any of these directories.
- Con: AUDIT.md references `audit_data/` by name 4 times with specific filenames (e.g.,
  `audit_data/config_params_unused.txt`, `audit_data/verify_delegation.txt`). Moving
  audit_data/ requires updating AUDIT.md — and AUDIT.md is a point-in-time document, so
  updating it risks confusion about provenance.
- Con: The audit/ Python scripts (run_audit.py etc.) write relative paths to `audit/outputs/`.
  Moving them changes their working assumption about where outputs land.

### Option B — Rename only

Keep `audit/`, `audit_data/`, `phase1/` in place but add a README to each explaining the
relationship.

**Trade-offs:**
- Pro: Zero reference breakage. AUDIT.md remains accurate.
- Pro: The three directories reflect different things (scripts+outputs vs. evidence vs. analysis
  documents); keeping them separate preserves that distinction.
- Con: A model entering the repo sees three audit-related directories with no explanation.

### Option C — Partial: merge audit_data/ + phase1/ only, leave audit/ standalone

`audit/` is a self-contained sub-project (scripts + their outputs). `audit_data/` and `phase1/`
are evidence/analysis that belong together. Merge those two into `audit_evidence/`.

**Trade-offs:**
- Pro: Less disruptive than full merge. audit/ retains its self-contained structure.
- Con: Still requires updating AUDIT.md's 4 `audit_data/` references.

**Recommendation:** Option B for now (add READMEs, no moves). The AUDIT.md reference
integrity concern is non-trivial: AUDIT.md is a completed evidence document whose value
depends partly on its path references being accurate. If the owner wants to merge, do it
after confirming which of the three directories (if any) will receive new content.

---

## Decision Required: `results/` Disposition

**Context:** Top-level `results/` has 4 files from one past perspective experiment:
`results/perspective_experiment/{seeding_analysis.md, u2_exemplars.json, analysis.md, experiment_log.json}`.

No Python script writes to `results/`. No reference to `results/` in CLAUDE.md, quick_start.md,
or README.md.

### Option A — Move to `outputs/`

```
outputs/perspective_experiment/   ← from results/perspective_experiment/
```

**Trade-offs:**
- Pro: Consolidates all generated output in one place.
- Pro: No Python scripts reference `results/` as a write target (verified).
- Con: `docs/project_orientation.md:500` references `docs/results/fragility_cross_tab.md`
  (a different `results/` — inside docs/). Confirm no confusion with the top-level `results/`.
- Risk: SAFE (no inbound refs to top-level results/ in active source).

### Option B — Leave in place, add CLAUDE.md note

Add a one-line note to CLAUDE.md explaining what `results/` contains.

**Trade-offs:**
- Pro: Zero risk.
- Con: `results/` remains as a small mystery directory at the root.

**Recommendation:** Option A. The top-level `results/` has no inbound references, no write
targets, and is clearly generated output from one past experiment. It belongs in `outputs/`.

---

## Proposed Final Layout (with owner decisions highlighted)

```
structural_dynamics_model/
├── README.md, CLAUDE.md, AUDIT.md, quick_start.md, validation_report.md
├── pyproject.toml, requirements.txt, packages.txt, LICENSE, TODO.md, .gitignore
│
├── prolog/                        [keep structure; add README.md]
│   ├── stack.pl, [81 named modules]
│   ├── generated/                 [proposed: move scs_*.pl here]
│   ├── testsets/                  [keep exactly as-is]
│   ├── testsets_sotu/             [keep exactly as-is]
│   ├── archives/                  [keep; add README.md]
│   ├── probsets/, gaptests/, belief_battery/, recon_2/  [keep; document in README]
│
├── python/                        [keep; add README.md]
├── agent/                         [keep; add README.md optional]
├── json/                          [keep]
├── sotu/                          [keep; add README.md documenting relationship]
├── outputs/                       [keep; add results/perspective_experiment/ if Option A]
│
├── docs/                          [add archive/ subdir for superseded papers — Option A]
│   ├── [all current canonical docs stay in place]
│   └── archive/                   [16 superseded paper versions]
│
├── prompts/                       [keep]
├── protocols/                     [keep]
├── essays/                        [keep]
├── examples/                      [keep]
├── scripts/                       [keep]
│
├── audit/                         [keep; add README.md — Option B for audit consolidation]
├── audit_data/                    [keep; add README.md]
├── phase1/                        [keep; add README.md]
│
└── audit_proposal/                [this deliverable — keep or archive post-review]
```
