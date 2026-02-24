# Batch Claim Reconciliation Report

**Generated:** 2026-02-24

## 1. Executive Summary

Reconciled **429** of 1151 constraint claims to match engine-computed modal types. Pre-reconciliation mismatch rate: 37.6% (433 constraints).

**Rationale:** `constraint_claim/2` predicates should reflect the current engine baseline so that future LLM-generated constraint stories produce meaningful diffs against an accurate reference. The mismatched claims were stale artifacts from earlier calibrations.

## 2. Pre-Reconciliation Snapshot

Full mismatch data preserved in `outputs/claim_engine_mismatch_snapshot.json`.

- Total constraints: 1151
- Matched (claim == modal): 716
- Mismatched: 433
- Skipped (null/unknown): 2
- Unmapped (no file found): 1
  - IDs: catholic_church_1200

## 3. Reconciliation Log

### Mismatch Transitions

| Claimed (before) | Modal (after) | Count |
|---|---|---:|
| tangled_rope | snare | 301 |
| snare | tangled_rope | 28 |
| rope | tangled_rope | 26 |
| mountain | scaffold | 14 |
| tangled_rope | rope | 14 |
| scaffold | tangled_rope | 13 |
| piton | snare | 11 |
| scaffold | rope | 6 |
| rope | scaffold | 6 |
| piton | rope | 5 |
| piton | tangled_rope | 4 |
| scaffold | snare | 1 |
| tangled_rope | scaffold | 1 |
| [social_governance] | tangled_rope | 1 |
| tangled_rope | piton | 1 |
| rope | mountain | 1 |

**Total:** 433

### Batch Update Results

- Files modified: 429
- Skipped (no file): 0
- Skipped (already correct on disk): 4
- Skipped (disk mismatch): 0
- Skipped (no regex match): 0
- Failed swipl: 0

## 4. Distribution Shift

| Type | Claimed (before) | Modal (after) | Delta |
|---|---:|---:|---:|
| [social_governance] | 1 | 0 | -1 |
| mountain | 140 | 127 | -13 |
| piton | 95 | 76 | -19 |
| rope | 62 | 54 | -8 |
| scaffold | 21 | 22 | +1 |
| snare | 78 | 363 | +285 |
| tangled_rope | 752 | 508 | -244 |

## 5. Tie-Breaking Decisions

**Precedence:** snare > tangled_rope > piton > scaffold > rope > mountain > naturalized

**Total ties resolved:** 33

| Tied Types | Count | Resolved To |
|---|---:|---|
| mountain vs scaffold | 19 | scaffold |
| rope vs tangled_rope | 7 | tangled_rope |
| naturalized vs rope vs snare vs tangled_rope | 5 | snare |
| snare vs tangled_rope | 1 | snare |
| mountain vs rope | 1 | rope |

## 6. Python Value Fixes

| File | Old | New |
|---|---|---|
| `python/coordination_vitality_diagnostic.py` | `PITON_EXTRACTION_CEILING = 0.25` | `PITON_EXTRACTION_CEILING = 0.45  # Must match config.pl param: piton_extraction_ceiling -- updated 2026-02-24` |
| `python/coordination_vitality_diagnostic.py` | `SCAFFOLD_EXTRACTION_CEIL = 0.30` | `SCAFFOLD_EXTRACTION_CEIL = 0.45  # Must match config.pl param: scaffold_extraction_ceil -- updated 2026-02-24` |
| `python/scaffold_piton_gate_audit.py` | `SCAFFOLD_EXTRACTION_CEIL = 0.30` | `SCAFFOLD_EXTRACTION_CEIL = 0.45  # Must match config.pl param: scaffold_extraction_ceil -- updated 2026-02-24` |
| `python/scaffold_piton_gate_audit.py` | `PITON_EXTRACTION_CEILING = 0.25` | `PITON_EXTRACTION_CEILING = 0.45  # Must match config.pl param: piton_extraction_ceiling -- updated 2026-02-24` |

## 7. Quine Self-Replication Resolution

- **Status:** reconciled
- **Transition:** tangled_rope → rope
- **Perspectives:** powerless=mountain, moderate=rope, institutional=rope, analytical=mountain
- **Tie resolution:** mountain vs rope → rope (rope > mountain in precedence)

## 8. Pipeline Re-run Sequence

After reconciliation, regenerate downstream artifacts:

```bash
python3 python/run_pipeline.py
python3 python/tangled_gradient.py
python3 python/chi_variance_decomposition.py
python3 python/rope_dominant_spot_check.py
```

**Expected changes:**
- `tangled_rope` pool shrinks significantly (claimed distribution shifts)
- `snare` pool grows correspondingly
- Tangled gradient subtypes may redistribute
- The 88% genuinely-perspectival finding should be re-verified against the reconciled population
- Scaffold/piton gate audit results will change due to threshold corrections (0.25/0.30 → 0.45)

## 9. Remaining Deferred Items

This reconciliation does **not** address:

1. **Dynamic config reading** — Python files still hardcode thresholds (now correct values). Refactoring to use `shared.loader.read_config()` is a separate task.
2. **27 dead/docs-only config parameters** — flagged in followup Part 4c, not cleaned up.
3. **`contamination_strength_*` config/code bypass** — `drl_purity_network.pl` ignores these config values.
4. **`decentralized_infrastructure_rope` missing testsets copy** — constraint exists in `prolog/testsets/new_civilizational_rope.pl` but has no dedicated testset file.
5. **`constraint_classification/3` reconciliation** — indexed classifications may also diverge from engine output; out of scope for this claim-only reconciliation.

