# Repository Audit and Reorganization Proposal

**Repo:** `structural_dynamics_model`  
**Audit date:** 2026-05-15  
**Scope:** Read-only audit pass. No files outside `audit_proposal/` were modified.  
**Purpose:** Produce a proposal for owner review before any reorganization begins.

---

## Documents in This Proposal

| Document | What It Contains |
|----------|-----------------|
| [01_inventory.md](01_inventory.md) | Directory-by-directory inventory with sizes, dates, entry points, and what each directory actually contains |
| [02_usage_analysis.md](02_usage_analysis.md) | Classification of every file group (CORE/SUPPORT/DIAGNOSTIC/GENERATED/LEGACY/AMBIGUOUS) based on reference traces |
| [03_standards.md](03_standards.md) | Which conventions are established standards vs. my judgment calls |
| [04_proposed_structure.md](04_proposed_structure.md) | Proposed layout, with options and trade-offs for the three contested decisions |
| [05_migration_plan.md](05_migration_plan.md) | Per-move risk analysis with specific reference breakage enumerated |

---

## What I Found That May Surprise You

**`prolog/archives/` is 405 MB and not in the README.** It contains 6,922 files across 4
historical engine versions (v1, v3, v4) plus versioned datasets. It is the largest undocumented
mass in the repo and accounts for 37% of total repository size.

**`sotu/pl/` and `prolog/testsets_sotu/` are byte-for-byte identical.** 189 files in both
locations. The synchronization mechanism is not documented. The pipeline reads from
`prolog/testsets_sotu/`; the sotu/ authoring scripts write to `sotu/pl/`. One of these
appears to be a staging copy of the other.

**`docs/results/` is a write target, not a read-only docs location.** `python/tangled_rope_sign_flip.py`
writes directly to `docs/results/tangled_rope_sign_flip.md`. This makes docs/ not a pure
source tree.

**The 9 `scs_*.pl` files at `prolog/` root are generated artifacts, not engine modules.**
They are config sensitivity overlays produced by `python/structural_config_sensitivity.py`
and were not moved to a generated-outputs location after creation.

**102 of 129 Python files are standalone-runnable scripts.** The python/ directory functions
more as a collection of analysis tools than as a library. The "100+ diagnostic scripts" in
CLAUDE.md is accurate.

**No source currently says "79 modules."** The README says 91 (correct). The README structure
diagram undercounts (says "[75 additional]"), but this is a diagram error, not a real
discrepancy. Actual count: 91 .pl files at prolog/ root; 81 named engine modules.

---

## Owner Decisions Required

These five questions must be resolved before any migration executes. The trade-offs are
documented in [04_proposed_structure.md](04_proposed_structure.md) and
[05_migration_plan.md](05_migration_plan.md).

### D1 — `docs/` restructure depth
**Option A (light touch):** Move only the 16 superseded paper versions to `docs/archive/`.
Zero reference breakage. Leaves ~90 remaining files in flat docs/ structure.

**Option B (full restructure):** Organize by purpose (paper/, framework/, analyses/, guides/).
Requires updating CLAUDE.md, README.md, and ~15 docs cross-references simultaneously.

### D2 — Audit directory consolidation
**Option A:** Merge `audit/` + `audit_data/` + `phase1/` into `audit_history/`.
Requires updating 4 specific path references in AUDIT.md.

**Option B:** Leave in place; add per-directory READMEs explaining relationships.
Zero risk.

**Option C:** Merge `audit_data/` + `phase1/` only (into `audit_evidence/`); leave
`audit/` standalone. Requires same AUDIT.md updates as Option A.

### D3 — `results/` top-level directory
**Option A:** Move 4 files to `outputs/perspective_experiment/`. No inbound references break.

**Option B:** Leave in place; add CLAUDE.md note identifying contents.

### D4 — `sotu/pl/` duplication
Is `sotu/pl/` a deliberate staging/deploy pattern, accidental duplication, or something
that should be a symlink? The answer determines whether any action is needed.

### D5 — `prolog/probsets/`, `gaptests/`, `belief_battery/` status
No active references found in Python scripts or key docs. Are these:
(a) used in subprocess calls to Prolog that weren't traced?
(b) dormant but intentional history?
(c) candidates for prolog/archives/?

---

## Recommended Execution Sequence

If the owner approves the low-risk proposals unconditionally, I recommend this order:

1. **Immediate, no decision needed:**
   - Add `.env` to `.gitignore`
   - Add per-directory README.md files (prolog/, python/, docs/, sotu/, audit dirs)
   - Move 16 superseded paper versions to `docs/archive/` (Option A for D1)

2. **After D3 resolved:**
   - Move `results/` → `outputs/` if Option A chosen

3. **After D4 resolved:**
   - Document or fix `sotu/pl/` duplication

4. **After D2 resolved:**
   - Audit directory consolidation (if Option A or C)

5. **Not recommended until D1 Option A has been in place for a few weeks:**
   - docs/ full restructure (Option B) — requires extensive lockstep updates

---

## What I Did Not Do

- **Subprocess trace:** Python scripts that call Prolog via `subprocess` may load Prolog
  modules not captured by the static import analysis. Some DIAGNOSTIC modules in prolog/ may
  actually be CORE via this pathway. Owner should verify before classifying any prolog/
  DIAGNOSTIC modules as deletable.
- **Agent/ interior:** The agent/ directory was inventoried at top level but not fully
  traced. The Streamlit app (agent/app.py) and UKE narrative orchestrator
  (agent/uke_narrative_orchestrator.py) were not import-traced.
- **`prolog/probsets/`, `gaptests/`, `belief_battery/`:** No inbound references found,
  but these were not confirmed as unused — they may be loaded by Python subprocess calls.
- **Full docs cross-reference map:** Only the canonical docs referenced by CLAUDE.md and
  project_orientation.md were traced. A full cross-reference map of all 107 docs/ files
  would be needed before executing Option B.
