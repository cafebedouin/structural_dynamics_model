# Phase 1 — linter MISSING_NL_PROFILE advisory made honest (OQ-296)

Executed 2026-08-18. Site located by content (`grep -n MISSING_NL_PROFILE python/linter.py`),
not by the OQ's cited line numbers — those had drifted.

## Claims verified against code before editing

1. **"defaults to 0.5" is stale.** `signature_detection.pl` `get_metric_average/3` binds
   `Average = unknown` on an empty metric list ("Honest abstain on missing data (was 0.5 ...)",
   commit `966d53c8`, OQ-44). The advisory's stated mechanism had been wrong since that commit.
2. **The binding conjunct was never mentioned.** `natural_law_signature/1` carries
   `HasAlternatives == false` under an in-file DEAD-BY-RANGE annotation (OQ-113); the advisory
   named only the three metric thresholds, so an author following it to completion still gets
   no certification. The instruction could not work.

## Discrimination record (naturally-arising pair — top of the ladder, no authored decoy)

Swept all five live legs + the kernel_v1 archive for mountain candidates
(`constraint_claim(_, mountain)` or `emerges_naturally(...)`):

```
testsets:                        21 candidates / 279 files, 0 trip
testsets_haiku:                  72 candidates / 960 files, 0 trip
testsets_flash:                 106 candidates / 960 files, 0 trip
testsets_kimi:                   39 candidates / 1005 files, 0 trip
testsets_sonnet:                 34 candidates / 1001 files, 0 trip
archives/datasets/kernel_v1:     42 candidates / 1106 files, 1 trip
TOTAL: 314 mountain candidates over 5311 files; 1 trip
```

POSITIVE: `prolog/archives/datasets/kernel_v1/correct_latin__living_drift_reading.pl`
NEGATIVES: the other 313 candidates — neither the positive nor the negatives authored to be found.

**Note for the roster:** check 25 fires on **0/4205 across all five LIVE legs**. It is not
dead — it discriminates (1/314 candidates, and the candidate population is large) — but its
only live instance is in the archive. This is a coverage fact about the current corpus, not a
defect in the check; recorded here rather than spawned.

## Before / after on the natural positive

BEFORE:
```
MISSING_NL_PROFILE: Mountain candidate is missing required NL profile data:
accessibility_collapse, resistance. Without these, the mountain metric gate will not fire
(emerges_naturally) and/or the natural_law_signature certification will fail
(accessibility_collapse and resistance default to 0.5). Add the missing declarations or
reclassify the constraint.
```

AFTER:
```
MISSING_NL_PROFILE: Mountain candidate is missing required NL profile data:
accessibility_collapse, resistance. Without these, the mountain metric gate will not fire
(emerges_naturally), and the profile metrics read as the `unknown` absence sentinel, so the
constraint abstains instead of receiving a signature. Add the missing declarations or
reclassify the constraint. NOTE: this restores metric COVERAGE only — it cannot yield a
natural_law certification. natural_law_signature/1 is unsatisfiable at HEAD for every
constraint (its HasAlternatives == false conjunct is dead-by-range; OQ-113/OQ-296,
unblocking is GAP-08 §7), so no authoring action reaches it.
```

Differences, each justified:
- `default to 0.5` → `unknown` absence sentinel + abstain. Corrects the stale mechanism (claim 1).
- Dropped `and/or the natural_law_signature certification will fail`. That clause implied
  certification was reachable by fixing coverage; it is not.
- Added the scope NOTE naming the dead conjunct and routing to GAP-08 §7. Discharges claim 2 —
  the author is told the NL path is closed rather than sent down it.
- `emerges_naturally` coverage half kept verbatim (live and useful, per the ruling).

## Behavior preservation

Detection unchanged: **1 fire / 314 candidates before AND after**. Only the message string
and the site comment changed; the trigger and the `missing` computation were not touched.
