# Audit: Authoring-Closure + Fabricated-Default Census — Findings
*Execution date: 2026-05-30. Plan: `.claude/plans/audit-task-authoring-closure-fabricated-default-census.md`*

---

## Methodology

Plan-directed audit: plan specified claims, witnesses, and graduation steps.
Execution ran every claim command, ran every graduation tripwire, and records
verdicts from tool output. Where tool output contradicts plan claims, the tool
output governs — see §Contradictions with Plan.

**Phase structure followed:** recon (plan) → witness execution → tripwire graduation.

---

## Track A — Authoring Closure

### A1 — Set P: Prolog input predicates (grep-witnessed)

Command run:
```bash
grep -n "narrative_ontology:" prolog/drl_core.pl prolog/drl_composition.pl \
    prolog/constraint_indexing.pl prolog/boltzmann_compliance.pl
```

Key confirmed call sites (subset; full output available):
- `drl_composition.pl:179` — `measurement(_, C, suppression_requirement, Time, Supp)`
- `drl_composition.pl:180` — `measurement(_, C, base_extractiveness, Time, BaseX)`
- `drl_composition.pl:187` — `measurement(_, C, theater_ratio, Time, TR_t)`
- `drl_core.pl:96` — `constraint_metric(Constraint, ActualMetricName, Value)`
- `boltzmann_compliance.pl:243/249` — `constraint_metric(C, ExtMetricName/SuppMetricName, ...)`

All A1 predicates in plan confirmed present. TIER: grep-witnessed.

### A2 — Set A: MeasurementMetric schema enum (grep-witnessed)

```bash
grep -n -B5 -A10 "MeasurementMetric" python/constraint_story_schema.json
```

Output confirmed enum contains only `["theater_ratio", "base_extractiveness"]`.
`suppression_requirement` absent from enum, absent from prompt, absent from compiler
measurement emission (generate_constraint_pl.py:654-669 only processes `theater_ratio`
and `base_extractiveness`). TIER: grep-witnessed.

### A3 — Schema enforcement (execution-witnessed)

Live jsonschema validation run (per Gap-Closer 2 plan):
```python
from generate_constraint_pl import validate_json
story = {..., "measurements": [{"metric": "suppression_requirement", ...}]}
errors = validate_json(story)
```

Output confirmed:
```
'suppression_requirement' is not one of ['theater_ratio', 'base_extractiveness']
```
Error fires once per measurement entry with that metric. Schema enforcement is
**structural prohibition** at validation time before .pl write.

This audit ran the same pattern:
- `D1A_OLD` patch text confirmed present in `drl_composition.pl` ✓
- Schema enum confirmed live: only two values ✓

TIER: execution-witnessed.

### A4 — P\A gap: suppression_requirement

```bash
grep -rn "narrative_ontology:measurement.*suppression_requirement" prolog/testsets/*.pl | wc -l
```
OUTPUT: `0`

No suppression_requirement measurement/5 facts exist in any of the 223 testsets.
The schema forbids authoring them. The engine reads them unconditionally on the
temporal path (drl_composition.pl:179) and fabricates Supp=0.5 on every call.

**UNDECLARED exclusion confirmed.** No comment, rationale, or scope document in
schema / prompt / compiler explains why `suppression_requirement` is excluded from
`MeasurementMetric` while the engine reads it. The declared-exclusion pattern
(comment with rationale before enforcement point) exists for scope contexts
(constraint_indexing.pl:950-965) but has no equivalent for MeasurementMetric.

### A4 addendum — OPEN-7: requires_active_enforcement (new finding)

Plan A4 left as OPEN: does `requires_active_enforcement/1` feed the main
classification path?

Graduation run:
```bash
grep -rn "requires_active_enforcement" prolog/drl_core.pl | grep -v "^.*:[0-9]*:%"
```

OUTPUT:
- `drl_core.pl:277` — `\+ requires_active_enforcement(C)` (in `scaffold_temporality_check`)
- `drl_core.pl:286` — `\+ requires_active_enforcement(C)` (in `natural_law_without_beneficiary`)
- `drl_core.pl:371` — `requires_active_enforcement(C)` (in `tangled_rope` gate)

`requires_active_enforcement` IS on the main classification path — `tangled_rope`
requires it (line 371), and `scaffold_temporality_check` negates it (line 277).

**A\P gap CLOSED.** `domain_priors:requires_active_enforcement/1` is in both Set P
(drl_core.pl:277/286/371) and Set A (emitted by compiler at generate_constraint_pl.py:457-458).
This is not a gap.

---

## Track B — Fabricated-Default Census

### B0 — Denominator verification

```bash
ls prolog/testsets/*.pl | wc -l
```
OUTPUT: `223` ✓

```bash
grep -rL "constraint_metric.*suppression_requirement" prolog/testsets/*.pl | wc -l
```
OUTPUT: `32` ✓

The 32 testsets lacking `constraint_metric.*suppression_requirement` are the
`_contradictions.pl` files — axiom contradiction stubs that carry only
`cs_axiom_contradiction`, `cs_story_uid`, and `cs_contradiction_of` facts.
They are NOT returned by `covering_analysis:all_corpus_constraints/1` (which
requires `constraint_metric(C, extractiveness, _)`). TIER: grep-witnessed.

### B2 — Per-site tripwire results

All five tripwire graduation scripts run from `python/sweeps/tripwire_fabricated_defaults.py`.
Results: `audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json` (moved here from gitignored `outputs/` 2026-06-11, OQ-33 close — location mandate).

---

#### Site D1a — drl_composition.pl:179 (temporal Supp=0.5)

fires-now: 647/647 temporal rows (all time points — suppression_requirement measurement
never exists). TIER: grep-witnessed (0 facts in corpus).

Tripwire run: patch `Supp = 0.5` → `Supp = 999.9`, run `constraint_history` over
full corpus via default analytical context.

```
Baseline rows: 647  Patched rows: 647  Total changed: 279
```

Change distribution:
| transition | count |
|-----------|-------|
| tangled_rope → snare | 219 |
| unknown → snare | 60 |
| **→ unknown** | **0** |

**279/553 non-unknown baseline rows affected (50.4%).**

**Mechanism confirmed:** `snare_suppression_floor = 0.60`. Fabricated `Supp=0.5`
falls below this floor, blocking constraints from reaching `snare` even when all
other snare gates (Chi, BaseEps, immutability) pass. They land in `tangled_rope`
(if tangled_rope_suppression_floor=0.40 ≤ 0.5) or `unknown` (if other gates also
fail). The real suppression requirement is absent from all 223 testsets, so every
temporal classification is affected.

**Verdict: LOAD-BEARING-WRONG (confirmed, execution-witnessed)**

The fabricated Supp=0.5 systematically misclassifies snare-eligible constraints as
tangled_rope/unknown on the temporal path. Not a minor perturbation; 50% of temporal
classifications are wrong in a directional way (too-low not too-high).

TIER: **execution-witnessed** (tripwire script run, output pasted above)

---

#### Site D1b — drl_composition.pl:180 (temporal BaseX=0.5)

fires-now: fires only at time points WITHOUT base_extractiveness measurement, but
`constraint_history` generates timeline entries only at time points where SOME
measurement exists (from `findall(T, measurement(_, C, _, T, _), Ts)`). All 647
measurement rows have base_extractiveness at the queried time point:

```bash
grep -rh "narrative_ontology:measurement(" prolog/testsets/*.pl | \
  grep -oP ",\s*\K(theater_ratio|base_extractiveness|suppression_requirement)" | sort | uniq -c
```
OUTPUT:
```
647 base_extractiveness
644 theater_ratio
```

Every time point with any measurement has a base_extractiveness measurement.
The fallback fires only on contradiction files (no measurements at all), which
generate no timeline entries via `constraint_history`.

Tripwire run: patch `BaseX = 0.5` → `BaseX = 999.9`:
```
Baseline rows: 647  Patched rows: 647  Total changed: 0
```

**Verdict: LATENT-TRAP (confirmed) — fallback unreachable via constraint_history.**
Fires only on direct `classify_at_time` calls for contradiction stubs at arbitrary T.
Not on the main temporal analysis path.

TIER: execution-witnessed

---

#### Site D2 — drl_core.pl:96 (static Supp=0)

fires-now: 32 contradiction files (confirmed grep). TIER: grep-witnessed.

Tripwire run: patch `Value = 0` → `Value = 999.9`, run `dr_type` over all_corpus_constraints:
```
Baseline rows: 191  Patched rows: 191  Total changed: 0
```

**Mechanism:** The 32 contradiction files are not enumerated by `all_corpus_constraints`
(requires `constraint_metric(C, extractiveness, _)`, which contradiction files lack).
For all 191 classified constraints, `constraint_metric.*suppression_requirement` IS present.
The static Supp=0 fallback only fires on non-classified stubs.

**Verdict: DORMANT — fires only on non-classified contradiction stubs.**
Not LOAD-BEARING-WRONG. The static path fabrication has no effect on corpus classification.

TIER: execution-witnessed (plan claimed LOAD-BEARING-WRONG; corrected by tripwire)

---

#### Site D20 — boltzmann_compliance.pl:245 (Boltzmann BaseEps=0.5)

fires-now: 32 contradiction files. TIER: grep-witnessed.

Tripwire run: patch multi-line `BaseEps = 0.5` → `BaseEps = 999.9`,
run `boltzmann_compliant` over all_corpus_constraints:
```
Baseline rows: 191  Patched rows: 191  Total changed: 0
```

Same mechanism as D2: contradiction files not enumerated. For classified constraints,
extractiveness metric is present. DORMANT on live corpus.

**Verdict: DORMANT (plan said UNSURE; corrected by tripwire)**

TIER: execution-witnessed

---

#### Site D21 — boltzmann_compliance.pl:251 (Boltzmann Supp=0)

fires-now: 32 contradiction files. TIER: grep-witnessed.

Tripwire run: patch multi-line `Supp = 0` → `Supp = 999.9`:
```
Baseline rows: 191  Patched rows: 191  Total changed: 0
```

DORMANT for same reason as D20.

**Verdict: DORMANT (plan said UNSURE; corrected by tripwire)**

TIER: execution-witnessed

---

### B-extra — Off-path sites (confirmation of plan verdicts)

**covering_analysis.pl:486/493 (OPEN-4):**

```bash
grep -n "classify_at_interpolated" prolog/product_site_export.pl prolog/json_report.pl
```
OUTPUT: (empty — 0 matches)

`product_site_export.pl` uses `covering_analysis:all_corpus_constraints` for
enumeration only. `json_report.pl` does not call `classify_at_interpolated`.
`classify_at_interpolated` is called only from `gap_diagnostic.pl` (analysis tool).

**Verdict: LATENT-TRAP (analysis path only) — confirmed, off main DR classification.**
TIER: grep-witnessed

**constraint_indexing.pl:840 (OPEN-5):**

```bash
grep -rL "constraint_data:base_extractiveness\|domain_priors:base_extractiveness" \
    prolog/testsets/*.pl | wc -l
```
OUTPUT: `32` — same 32 contradiction files. DORMANT on classified corpus.

**gap_diagnostic.pl:120/127, omega1_audit.pl:102/115, invertibility_analysis.pl:111/113/115:**
All confirmed analysis-only, not on dr_type path. Plan verdicts retained.

---

## Complete B2 Summary (Corrected)

| # | site | const | fires-now | tripwire-flips | verdict | tier |
|---|------|-------|-----------|----------------|---------|------|
| D1a | drl_composition.pl:179 | 0.5 | 647/647 temporal rows | **279 (219 TR→snare, 60 unk→snare)** | **LOAD-BEARING-WRONG** | execution-witnessed |
| D1b | drl_composition.pl:180 | 0.5 | 0 via constraint_history | **0 via constraint_history** | **LATENT-TRAP** | execution-witnessed |
| D2 | drl_core.pl:96 | 0 | 32 contradiction stubs only | **0** | **DORMANT** | execution-witnessed |
| D20 | boltzmann_compliance.pl:245 | 0.5 | 32 contradiction stubs only | **0** | **DORMANT** | execution-witnessed |
| D21 | boltzmann_compliance.pl:251 | 0 | 32 contradiction stubs only | **0** | **DORMANT** | execution-witnessed |

All off-path sites retain plan verdicts (LATENT-TRAP or PATH-ASSERTED).

---

## Contradictions with Plan

Three plan verdicts corrected by tripwire execution:

**1. D1a flip count and direction.**
Plan (instance-reported): 443/519 non-unknown temporal classifications flipped to `unknown`.
Actual (execution-witnessed): 279/647 rows changed; 219 tangled_rope→snare + 60 unknown→snare; 0 →unknown.
The direction is reversed: Supp=0.5 SUPPRESSES snare (blocks snare gate at 0.60 floor),
NOT pushes to unknown. The instance-reported claim was based on incorrect reasoning about
the classification cascade.

**2. D2 verdict.**
Plan (path-asserted): LOAD-BEARING-WRONG (blocks tangled_rope + snare on 32 testsets).
Actual (execution-witnessed): DORMANT — the 32 affected testsets are non-classified
contradiction stubs, not classified constraints. `all_corpus_constraints` excludes them.

**3. D20/D21 verdicts.**
Plan: UNSURE (path-asserted only, tripwire pending).
Actual (execution-witnessed): DORMANT — same 32 contradiction stubs as D2.

---

## Closed OPEN Goals

| OPEN | graduation step | status | result |
|------|----------------|--------|--------|
| OPEN-1 | D1b tripwire flip count | **CLOSED** | 0 flips via constraint_history |
| OPEN-2 | D2 tripwire flip count | **CLOSED** | 0 flips; fires on non-classified stubs |
| OPEN-3 | D20/D21 tripwire | **CLOSED** | 0 flips; fires on non-classified stubs |
| OPEN-4 | covering_analysis caller confirmation | **CLOSED** | gap_diagnostic only, not json_report or product_site_export |
| OPEN-5 | constraint_indexing.pl:840 fires-now | **CLOSED** | 32 contradiction stubs (same set as D2/D20/D21) |
| OPEN-6 | D1a flip count (instance-reported) | **CLOSED** | 279 type changes; 0 unknown flips; direction reversed from plan claim |
| OPEN-7 | requires_active_enforcement main path | **CLOSED** | IS on main path (tangled_rope gate line 371, scaffold_temporality_check lines 277/286) |

All 7 OPEN goals graduated to execution-witnessed.

---

## Key Structural Finding

The `suppression_requirement` fabricated-default (D1a) is the only load-bearing fabricated-default
site on the main DR classification path. Its effect is not random noise — it is a systematic
directional bias:

- `snare_suppression_floor = 0.60`
- Fabricated `Supp = 0.5` fails this floor
- Every temporal classification that would otherwise be snare becomes tangled_rope or unknown
- 279/553 non-unknown temporal rows (50.4%) are misclassified on every corpus run

The other four main-path sites (D1b, D2, D20, D21) are dormant or latent-only:
they fire on contradiction stubs (D2/D20/D21) or on time points unreachable via
`constraint_history` (D1b).

OQ-33 resolution options (a/b/c) remain design decisions for the repo owner.
This audit surfaces the scope: one active defect (D1a), four dormant or latent sites.

---

## Scripts

- Tripwire script: `python/sweeps/tripwire_fabricated_defaults.py`
- Tripwire results JSON: `audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json` (moved here from gitignored `outputs/` 2026-06-11, OQ-33 close — location mandate)
