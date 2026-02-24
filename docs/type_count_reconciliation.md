# Type Count Reconciliation Report

**Generated:** 2026-02-24 01:20

**Corpus size:** 1151 constraints

---

## 1. Raw Field Census

Cross-tabulation of constraint types across 6 counting columns.

| Type | claimed_type | maxent_top_type | p.powerless | p.moderate | p.institutional | p.analytical |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| [social_governance] | 1 | 0 | 0 | 0 | 0 | 0 |
| mountain | 140 | 147 | 147 | 127 | 127 | 147 |
| naturalized | 0 | 0 | 20 | 0 | 3 | 0 |
| piton | 95 | 107 | 80 | 76 | 74 | 74 |
| rope | 62 | 52 | 30 | 61 | 793 | 52 |
| scaffold | 21 | 0 | 3 | 22 | 56 | 0 |
| snare | 78 | 357 | 319 | 358 | 0 | 533 |
| tangled_rope | 752 | 488 | 551 | 506 | 97 | 344 |
| **Total** | **1149** | **1151** | **1150** | **1150** | **1150** | **1150** |

**Note:** `claimed_type` sums to corpus size (one per constraint). Perspective columns may differ because constraints can have `unknown` or `null` perspectives.

## 2. Filter Methods and Report Counts

Each type uses a specific filter method in `TYPE_CONFIGS` (from `type_reporter.py`).

| Type | Filter Method | Family | Raw Filtered | Normalized+Deduped |
| :--- | :--- | :--- | ---: | ---: |
| mountain | `unanimity` | validation | 126 | 126 |
| rope | `unanimity` | validation | 16 | 16 |
| tangled_rope | `any_perspective` | diagnostic | 568 | 606 |
| snare | `standard` | diagnostic | 78 | 82 |
| scaffold | `standard` | diagnostic | 21 | 22 |
| piton | `any_perspective` | diagnostic | 80 | 82 |

**Filter method definitions:**

- **`standard`**: `claimed_type == type_name`
- **`unanimity`**: `claimed_type == type_name` AND all non-unknown perspectives match
- **`any_perspective`**: ANY perspective value == type_name (ignores claimed_type)

For diagnostic family reports, normalization expands one-per-omega, then dedups on `(constraint_id, omega_question)`.

## 3. Historical Count Reconciliation

Four tangled_rope counts from different sources:

| Count | Method | Pipeline State | Reproducible? |
| ---: | :--- | :--- | :---: |
| 773 | `any_perspective` | Pre-piton-gate-changes | No |
| 677 | `any_perspective` | Post-gate-changes, pre-FCR-fix | No |
| 606 | `any_perspective + diagnostic normalization + dedup` | Current pipeline | Yes |
| 752 | `claimed_type == 'tangled_rope'` | Current enriched_pipeline.json | Yes |

**773:** Historical count before coordination_vitality gate additions. Not reproducible from current data — represents earlier pipeline state.

**677:** After piton gate changes but before the false_ci_rope override fix. Not reproducible from current data — represents intermediate pipeline state.

**606:** Current tangled_rope report entry count. Uses any_perspective filter, then diagnostic normalization (expand one-per-omega), then dedup on (constraint_id, omega_question).

**752:** Structural count: constraints where the LLM's claimed_type label is 'tangled_rope'. One type per constraint, sums to 1151.

### Verification of reproducible counts

- `any_perspective` raw filtered: **568** constraints
- `any_perspective` + normalization + dedup: **606** report entries
- `claimed_type == 'tangled_rope'`: **752** constraints

606 count **confirmed** — matches current tangled_rope report entry count.

752 count **confirmed** — matches claimed_type count.

## 4. Double-Counting Analysis

With `any_perspective` filter, a constraint can appear in multiple type reports if different perspectives compute different types.

### piton ∩ tangled_rope

**2 constraints** appear in both reports:

- `ritual_transition_scaffold` — analytical: tangled_rope, institutional: rope, moderate: piton, powerless: piton
- `ship_of_theseus` — analytical: tangled_rope, institutional: rope, moderate: piton, powerless: piton

### Multi-type constraints

**2 constraints** appear in multiple any_perspective type reports:

- `ritual_transition_scaffold`: tangled_rope, piton
- `ship_of_theseus`: tangled_rope, piton

## 5. Canonical Count Regimes

### Structural regime (`claimed_type`)

One type per constraint. Sums to corpus size.

| Type | Count |
| :--- | ---: |
| mountain | 140 |
| rope | 62 |
| tangled_rope | 752 |
| snare | 78 |
| scaffold | 21 |
| piton | 95 |
| *(none/null)* | 2 |
| **Total** | **1150** |

### Report regime (per TYPE_CONFIGS filter)

Perspectival; may not sum to corpus size due to filter method differences and double-counting.

| Type | Filter | Raw Filtered | Report Entries |
| :--- | :--- | ---: | ---: |
| mountain | `unanimity` | 126 | 126 |
| rope | `unanimity` | 16 | 16 |
| tangled_rope | `any_perspective` | 568 | 606 |
| snare | `standard` | 78 | 82 |
| scaffold | `standard` | 21 | 22 |
| piton | `any_perspective` | 80 | 82 |

**The gradient analysis (Part 3) will use `claimed_type` as its population definition.**

## 6. Summary

The four tangled_rope counts reflect four different operations on the pipeline data:

1. **773** and **677** are historical snapshots from earlier pipeline states (pre/post piton gate changes). They are not reproducible from current data and exist only in documentation.

2. **606** is the current tangled_rope *report entry count*, computed via `any_perspective` filter + diagnostic normalization + dedup. This is what the tangled_rope diagnostic report shows.

3. **752** is the *structural count* — constraints where `claimed_type == 'tangled_rope'`. This is the observer-invariant population definition.

Both reproducible counts (606 and the structural count) are correct for their respective methodologies. They measure different things: report entries (perspectival, normalized) vs. structural population (one label per constraint).
