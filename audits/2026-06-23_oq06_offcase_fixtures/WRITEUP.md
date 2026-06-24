# OQ-06 — Off-case fixtures for `cs_drift_unacknowledged` / `cs_axiom_foreclosed`

**Date:** 2026-06-23
**Verdict:** **RESOLVED.** All four off-case conjuncts are now witnessed in both
directions (fires-when-it-should AND stays-silent-when-it-should), three on genuine
corpus UIDs and one (a structural absence) via a transient single-conjunct probe.

## Question

Four conjuncts of two CS-drift predicates had only ever been exercised in the on-case
(firing) direction. "Stays silent when the off-condition holds" was untested, so silence
could mean *the guard correctly blocked* OR *the predicate never dispatched* — Build
Discipline Pattern 5 (absence satisfies the gate) at the conjunct level.

Predicate bodies (witnessed 2026-06-23, byte-exact — the `Files:` line in ISSUES.md was
**stale**, pointing at `cs_drift_engine.pl`, which only *mentions* the predicate in a
comment at lines 34–35; the real definitions are):

- `cs_pattern_detection.pl:412–416` — `cs_drift_unacknowledged(UID, Gap)` — C3 `Dir \= stable`, C4 `Mag \= minor`.
- `cs_axiom_engine.pl:137–141` — `cs_axiom_foreclosed(UID, Atom)` — C2 `grounding = empirically_contingent`, C4 `Magnitude \= minor`.

## Method (search first, then branch per conjunct)

The OQ's load-bearing claim ("no live off-case UID," ×4) is an **empty-result claim** —
a fact about a search, not the corpus, until the search is shown to find. So:

**Phase A (`search.pl`, `search_output.txt`).** For each of the four real corpuses
(`testsets`, `testsets_haiku`, `testsets_flash`, `archives/datasets/kernel_v1`), overlay
`corpus_path` with `asserta` in its **own fresh swipl process** (`load_all_testsets/0` is
`corpus_loaded`-guarded and `consult` accumulates facts, so a sequential one-process scan
would load only the first corpus and pollute counts), then bucket authored facts per
conjunct using shared filter predicates.

Each off-bucket carries a **two-sided planted control** (sensitivity + specificity) run on
live `testsets`, using the *exact* bucket-filter predicate the enumeration uses — a too-loose
filter passes sensitivity yet inflates the off-count, recreating the OQ-06 defect one level
up. Plus a **per-corpus overlay fingerprint** (`corpus_path` + `cs_drift_state/3` /
`cs_axiom/3` counts) so a no-op `asserta` that silently reads the default corpus is caught.

All four controls PASS:
```
bucket_c3_off       base=0 sens=1[PASS] spec=0[PASS]
bucket_c4_off       base=3 sens=4[PASS] spec=3[PASS]
bucket_axiom_c2_off base=9 sens=10[PASS] spec=9[PASS]
bucket_axiom_c4_off base=0 sens=1[PASS] spec=0[PASS]
```
Fingerprints differ per corpus (`cs_drift_state/3` = 70 / 955 / 960 / 875), confirming each
overlay took. (`cs_axiom/3` is multifile-but-**static**, unlike the dynamic `cs_drift_state/3`
and `cs_axiom_grounding/3`; the axiom planted controls declare `dynamic(narrative_ontology:cs_axiom/3)`
for the control process only — this does not change how the filter *reads* `cs_axiom`.)

## Phase A result — off-bucket counts (live off-cases vs witnessed absence)

| Conjunct | testsets | haiku | flash | kernel_v1 | Branch |
|----------|---------:|------:|------:|----------:|--------|
| drift C3 (Dir=stable) | 0 | 0 | 0 | 0 | **absence** → transient probe |
| drift C4 (Mag=minor) | 3 | 10 | 41 | 8 | **real UID** `0b5146c6…` (testsets) |
| axiom C2 (grounding≠ec) | 9 | 253 | 28 | 212 | **real UID** `e0fb873f…` (testsets) |
| axiom C4 (Mag=minor+ec) | 0 | 1 | 3 | 0 | **real UID** `b65e1d35…` (haiku) |

**drift-C3 is a structural absence, not a coverage gap.** Stable-direction drifts: testsets
7 (all minor), flash 280 (all minor), haiku 37 (35 minor + 2 non-minor). The 2 haiku
stable+non-minor drifts are **both `acknowledged=true`**. So the off-case (stable +
non-minor + *unacknowledged*) cannot occur in the authored corpus: unacknowledged stable
drifts are always minor; non-minor stable drifts are always acknowledged. The transient
probe is the appropriate **permanent** witness for this conjunct — no synthetic fixture is
written to `testsets/` (THREE-LIVE-LEGS: singleton sparsity is intended).

The other three conjuncts have genuine live off-cases, so they are closed on real data
(gold standard). `cs_axiom_foreclosed`'s C4 off-case is absent on `testsets`/`kernel_v1` but
live on `haiku`/`flash`; the same filter finding members there is the cross-corpus sensitivity
control that licenses the two zeros as genuine absence.

## Phase C — witness matrix (`probe.pl`, `probe_output.txt`)

1. **Real-corpus fire control (named legs).** `cs_drift_unacknowledged` fires on 46 real
   testsets UIDs; `cs_axiom_foreclosed` on 12 (testsets) and 182 (haiku) — end-to-end
   dispatch on real data, on legs overlapping the absence-witnessed legs.
2. **Transient matched-pair matrix** (synthetic fresh UIDs, single-conjunct isolation): for
   each conjunct, a SILENT row (off-fixture, all siblings satisfied → no fire) and a paired
   FIRED row (the one off-field flipped on → fire). **All 8 rows PASS** → each off-conjunct
   is what blocks, and the sibling-satisfied firing path works.
3. **Real off-case silence** on genuine UIDs: drift-C4 `0b5146c6…` SILENT, axiom-C2
   `e0fb873f…/state_racial_classification_…` SILENT (testsets), axiom-C4 `b65e1d35…/
   verifiable_significance_…` SILENT (haiku). The `with_asserted` restore-verify never threw.

## Files

- New: `search.pl`, `probe.pl`, `search_output.txt`, `probe_output.txt`, this writeup.
- No change to engine code or `testsets/`. All fixtures transient.
- Doc edits: `ISSUES.md` (OQ-06 resolved + `Files:` corrected), `KNOWN_STATE.md`,
  regenerated `issues/INDEX.{md,json}`.

## Reproduce (from `prolog/`)

```
swipl -g "[stack], consult('../audits/2026-06-23_oq06_offcase_fixtures/search.pl'), controls, halt" -t "halt(1)"
swipl -g "[stack], consult('../audits/2026-06-23_oq06_offcase_fixtures/search.pl'), scan(testsets), halt" -t "halt(1)"   # + haiku/flash/kernel_v1
swipl -g "[stack], consult('../audits/2026-06-23_oq06_offcase_fixtures/probe.pl'), probe_testsets, halt" -t "halt(1)"
swipl -g "[stack], consult('../audits/2026-06-23_oq06_offcase_fixtures/probe.pl'), probe_haiku, halt" -t "halt(1)"
```
