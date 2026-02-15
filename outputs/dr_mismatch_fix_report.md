# dr_mismatch/4 Coverage Gap Fix Report

**Date:** 2026-02-14
**Module:** `drl_core.pl` (ERROR DETECTION section)
**Triggered by:** Abductive engine finding — 88 `coverage_gap` hypotheses

---

## Executive Summary

The abductive reasoning engine identified 88 constraints with multi-type Dirac
orbits (genuine classification variance across observer positions via
`gauge_orbit/2`) but for which `dr_mismatch/4` never fired
`perspectival_incoherence`. This report documents the root cause analysis, fix
implementation, and before/after verification.

---

## Root Cause Analysis

The abductive engine's initial diagnosis identified two causes (analytical
context exclusion and cut-after-first-match). Investigation revealed a **third
and primary cause**: the perspectival_incoherence clause queried pre-stored
`constraint_classification/3` facts rather than computing types on-the-fly.

### Cause 1: Stored-fact dependency (PRIMARY — blocked all 88)

The perspectival_incoherence clause used:
```prolog
constraint_indexing:constraint_classification(C, Type1, Ctx1),
constraint_indexing:constraint_classification(C, Type2, Ctx2),
Type1 \= Type2,
```

This required **two stored classification facts** with different types for the
same constraint. However, testset files typically store only 1-3 perspectives
in `constraint_classification/3`, while `gauge_orbit/2` computes types
on-the-fly via `dr_type/3` across all 4 standard contexts. Result: most
constraints had only 1 stored fact, making it impossible to find a differing
pair.

**Example:** `ai_driven_surveillance_sensor_layer` had exactly 1 stored
`constraint_classification/3` fact (snare from powerless context), but
`dr_type/3` shows variance across all 4 contexts:
- powerless: snare, moderate: snare, institutional: rope, analytical: snare

### Cause 2: Cut isolation (SECONDARY — blocked unbound-C enumeration)

The claim-vs-reality clauses (false_summit, snare_as_rope, piton_as_snare)
used cuts (`!`) directly in `dr_mismatch/4` clauses. These cuts prevented
perspectival_incoherence from firing when the ErrorType argument was unbound
(e.g., in `findall` with unbound ErrorType). With bound ErrorType
(`perspectival_incoherence`), head unification skipped the claim clauses, so
cuts did not apply.

### Cause 3: Analytical context exclusion (TERTIARY — blocked 1 of 88)

Lines 469-470 excluded any context pair involving the analytical observer:
```prolog
P1 \= analytical,
P2 \= analytical,
```

Of the 88 coverage-gap constraints:
- **87** had variance between non-analytical contexts (powerless, moderate,
  institutional), so the analytical exclusion was not the blocker for them.
- **1** had variance only when the analytical context was included.

The exclusion was over-conservative: `gauge_orbit/2` includes all 4 standard
contexts (including analytical), and the analytical modifier (`pi=1.15`) is a
deliberately calibrated observer position, not noise.

---

## Fix Implementation

Three changes to `drl_core.pl`, all within the ERROR DETECTION section:

### 1. Compute types on-the-fly (fixes Cause 1)

Rewrote the perspectival_incoherence clause to use `dr_type/3` with
`standard_context/1`, matching `gauge_orbit/2` methodology:

```prolog
dr_mismatch(C, perspectival_gap(Type1, Ctx1, Type2, Ctx2),
            perspectival_incoherence, informational) :-
    standard_context(Ctx1),
    standard_context(Ctx2),
    Ctx1 = context(agent_power(P1), _, _, _),
    Ctx2 = context(agent_power(P2), _, _, _),
    P1 @< P2,
    dr_type(C, Ctx1, Type1),
    dr_type(C, Ctx2, Type2),
    Type1 \= Type2.
```

### 2. Delegate claim mismatches to helper (fixes Cause 2)

Moved false_summit, snare_as_rope, and piton_as_snare to a helper predicate
`dr_claim_mismatch/4`. Cuts are now local to the helper, so they cannot
prevent perspectival_incoherence from firing for the same constraint:

```prolog
dr_mismatch(C, Context, ErrorType, Severity) :-
    dr_claim_mismatch(C, Context, ErrorType, Severity).
```

### 3. Include all standard contexts (fixes Cause 3)

Removed `P1 \= analytical, P2 \= analytical` restrictions. All 4 standard
contexts are now included, matching `gauge_orbit/2`.

### Backward compatibility

Added a second clause to `dr_mismatch/3` to expose perspectival_incoherence
through the legacy interface (which binds Context, preventing unification with
`perspectival_gap/4`):

```prolog
dr_mismatch(C, perspectival_incoherence, informational) :-
    dr_mismatch(C, _, perspectival_incoherence, informational).
```

---

## Before/After Comparison

### Mismatch Detection Counts

| Metric | Before | After | Delta |
|---|---|---|---|
| `perspectival_incoherence` (unique constraints) | 1 | 809 | +808 |
| `type_1_false_summit` | 76 | 76 | 0 |
| `type_3_snare_as_rope` | 1 | 1 | 0 |
| `type_5_piton_as_snare` | 0 | 0 | 0 |
| Constraints with BOTH claim mismatch AND PI | 0 | 50 | +50 |

**Note on false_summit:** The per-constraint behavior is unchanged (same 76
constraints are flagged). The baseline measurement of "1" in the original
analysis used `findall` with unbound C, which was blocked by the cut artifact.
With pre-bound C enumeration, both before and after return 76.

### Abductive Engine Results

| Metric | Before | After | Delta |
|---|---|---|---|
| `coverage_gap` hypotheses | 88 | 0 | -88 |

All 88 coverage-gap constraints are now detected by `perspectival_incoherence`.

### Full Pipeline Results

| Metric | Before | After |
|---|---|---|
| Test suite | 1025/1025 PASS | 1025/1025 PASS |
| Reports generated | All | All |
| Abductive total hypotheses | 88 (no MaxEnt) | 0 (no MaxEnt) / 56 (with MaxEnt) |

### Coverage Gap Cause Breakdown (of the 88)

| Cause | Count | Percentage |
|---|---|---|
| Stored-fact dependency (all 88 affected) | 88 | 100% |
| Also affected by analytical exclusion | 1 | 1.1% |
| Also affected by cut (unbound-C only) | varies | n/a |

### Remaining Coverage Gaps

| Count | Description |
|---|---|
| 0 | No remaining `coverage_gap` hypotheses after fix |

---

## Caller Impact Assessment

| Caller | Location | Pattern | Impact |
|---|---|---|---|
| report_generator | line 112 | `setof/3` via `dr_mismatch/3` | Now sees PI results (additive) |
| report_generator | line 416 | `setof/3` via `dr_mismatch/3` | Now sees PI results (additive) |
| report_generator | line 472 | `setof/3` for `type_1_false_mountain` | Unaffected (specific type) |
| abductive_engine | line 380 | `\+` with bound ErrorType | Now finds PI (fixes coverage_gap) |

No callers break. All use `setof`/`findall` or negation-as-failure, which
handle multiple results correctly.

---

## Files Changed

- `prolog/drl_core.pl` — ERROR DETECTION section rewritten (lines 414-496)
  - No changes to classification logic, action routing, or any other section
  - `dr_claim_mismatch/4` helper added (same logic, isolated cuts)
  - `perspectival_incoherence` clause rewritten (on-the-fly computation)
  - `dr_mismatch/3` backward compat extended for PI

No changes to `dirac_classification.pl`, `abductive_engine.pl`, `config.pl`,
or any other module.
