# Schema Drift Audit — 2026-05-30

Audit of producer→consumer field contracts for the five main JSON interchange files.
Findings are empirical (run output, not doc re-statement). Two drift directions:

- **DRIFT-A (extra)**: producer writes fields the consumer's schema contract doesn't know
  about → "unexpected field" warnings → risk of silent drop.
- **DRIFT-B (missing)**: consumer reads a field the producer doesn't always write → silent
  default substitution → fabricated value wearing a measurement's costume.

---

## F0 — Linter Findings (documented; not applied this pass)

Running `linter.py` against the three new populism-axis testsets produced three distinct
categories of warning. They are NOT all the same bug.

### F0a: Stale enum — INVALID_COORDINATION_TYPE

Affected: `cultural_backlash_mobilization`, `elite_legitimacy_collapse`

`linter.py:288-290` has 4 valid coordination types:
```
information_standard, resource_allocation, enforcement_mechanism, global_infrastructure
```

The engine (`boltzmann_compliance.pl:461-466`) and JSON schema
(`constraint_story_schema.json:83-90`) both have 6:
```
+ attachment_coordination, identity_coordination
```

**Trace**: `boltzmann_floor_for(cultural_backlash_mobilization, F)` → `F = 0.08` via
`coordination_type_to_floor_param(identity_coordination, boltzmann_floor_identity_coordination)`
and `config.pl:371 param(boltzmann_floor_identity_coordination, 0.08)`. Does NOT fall through
to `boltzmann_floor_default`. Classification is unaffected. Linter emits a false positive.

**Fork** (repo-owner decision; not applied here): option (a) update `linter.py:288-290` to
match the engine's 6-value enum; option (b) remove `identity_coordination` and
`attachment_coordination` from `constraint_story_schema.json`. Note: option (b) would delete
a working engine feature with tuned config params — it is not a symmetric alternative.

### F0b: Generation defect — LOW_THEATER_RATIO (BUG, not a fork)

Affected: `cultural_backlash_mobilization:159`, `elite_legitimacy_collapse` (analogous line)

```
Source line 159: constraint_indexing:constraint_classification(cultural_backlash_mobilization, piton, ...
theater_ratio = 0.58; piton_theater_floor = 0.70
```

**Engine trace** (from pipeline_output.json):
```
cultural_backlash_mobilization:
  claimed_type: tangled_rope
  theater_ratio: 0.58
  perspectives: {powerless: tangled_rope, moderate: tangled_rope, institutional: rope, analytical: tangled_rope}

elite_legitimacy_collapse:
  claimed_type: tangled_rope
  theater_ratio: 0.58
  perspectives: {powerless: tangled_rope, moderate: tangled_rope, institutional: rope, analytical: tangled_rope}
```

Neither constraint is classified piton from ANY perspective. The testset declares a piton
classification at one context (line 159 for cultural_backlash) that the engine cannot produce:
piton requires theater_ratio >= 0.70; both testsets have 0.58. The declared type is unreachable
under the engine's piton gate. This is a **generation defect** in both testsets — the generator
wrote a piton classification that no engine path can reach at the authored theater_ratio.

This is not a fork. The fix is unambiguous regardless of how the linter enum is resolved:
the piton classification at that context must be corrected (remove or raise theater_ratio to >= 0.70).

**Fix** (not applied here): regenerate both testsets, or manually correct the piton
classification at the analytical/institutional context.

### F0c: Linter regex bug — MISSING_VICTIM / MISSING_ENFORCEMENT

Affected: `generational_economic_decline`

```python
# linter.py:86-88
found_types = set(re.findall(
    r'constraint_classification\(.*?,[\s\n\r]*(mountain|rope|snare|tangled_rope|scaffold|piton)',
    content, re.DOTALL))
```

With `re.DOTALL`, the non-greedy `.*?` extends past test predicates that use Prolog variables
(e.g., `constraint_classification(C, TypeTarget, ...)`) and spans into comments. For
`generational_economic_decline`, the match opens at line 130 (a mountain classification) and
spans 147 lines to the comment at line 277 which mentions `tangled_rope` in a decomposition
note. `tangled_rope` enters `found_types` despite no tangled_rope classification existing in
the testset.

**Witnessed** (run output):
```
found_types: ['mountain', 'tangled_rope']
match opens at: 'constraint_classification(generational_economic_decline, mountain,\n    context(a'
match closes at: 'ple stories: housing_unaffordability (ε ≈ 0.35, tangled_rope'
```

The constraint correctly classifies as mountain in the engine. The MISSING_VICTIM and
MISSING_ENFORCEMENT warnings are false positives.

**Scope of F0c infection** (run against full 223-testset corpus):
```
Testsets where DOTALL adds phantom types: 1
  generational_economic_decline.pl: phantom {'tangled_rope'}
```
Only 1 testset affected. F1–F4 schema drift findings are derived from JSON inspection and code
reading, not from linter output — F0c does not infect them. Within F0, only F0c's own finding
(MISSING_VICTIM/MISSING_ENFORCEMENT on generational_economic_decline) is a linter false positive;
F0a and F0b are independently verified.

**Fix** (linter fix, not applied here): strip Prolog comments from `content` before
`found_types` scan, or replace `re.DOTALL` with a pattern that cannot span past closing `)`.

---

## F1 — Schema Registration (applied 2026-05-30)

**File**: `python/shared/schemas.py`

Five fields were written by `prolog/json_report.pl` into every per-constraint entry but absent
from `PIPELINE_FIELDS`, generating ~2164 "unexpected field" warnings per pipeline run:

| Field | Write site | Notes |
|-------|-----------|-------|
| `cs_instance_count` | json_report.pl:503, 508 | Always 0 or N UIDs |
| `cs_drift_terminal` | json_report.pl:504, 522-524 | null when no trajectory |
| `cs_axiom_foreclosed` | json_report.pl:505, 527-529 | null when no foreclosure |
| `cs_drift_unacknowledged` | json_report.pl:506, 532-534 | Always written (true/false) |
| `drift_trajectory` | json_report.pl:451-456 | **Absent** (key not in entry) for constraints with no `measurement/5` facts |

Fields are read by `enhanced_report.py` (`build_cs_extended_section` lines 2148-2172,
`build_drift_trajectory_section` lines 582-604). DRIFT-A-cosmetic confirmed by drop test:

```
# Drop test on cultural_backlash_mobilization (has measurements):
['cs_axiom_foreclosed', 'cs_drift_terminal', 'cs_drift_unacknowledged', 'cs_instance_count',
 'cs_pattern', 'cs_pattern_signals', 'cs_verdicts', 'drift_events', 'drift_trajectory']
```

**Secondary fix**: `_check_field` was updated to treat absent-but-nullable as valid (`drift_trajectory`
is absent for `catholic_church_1200` and `study_as_exercise`, which have no measurements). Previously
the validator raised "missing required field" even with nullable=True.

**Post-fix witnesses**:
- `validate_pipeline_output` → 0 errors, 0 unexpected-field warnings
- SC1: 5 fields still present on `cultural_backlash_mobilization` after fix
- SC3: sentinel `_AUDIT_SENTINEL_xyz` still fires as unexpected field

---

## F2 — Fabricated-Default: abductive_triggers (applied 2026-05-30)

**File**: `python/enrich_pipeline_json.py`

When `abductive_data.json` fails to load, the original code set `abd_data = {}`, causing
`abductive_triggers = []` for all constraints. `[]` is indistinguishable from "genuinely no
triggers." Fixed: set `abd_data = None` on load failure; `enrich_entry` now writes
`abductive_triggers = None` when `abd_data is None`.

**Severity**: LATENT on current corpus (`abductive_data.json` loads for all constraints).
Defensive hardening, not an active leak patch.

**Witness** (run against test entry):
```
abductive_triggers when abd_data=None:     None    ← file absent
abductive_triggers when abd_data={} (cid absent):  []  ← file loaded, no triggers
```

Schema: `("abductive_triggers", list, True)` — nullable to accommodate None.

---

## F3 — Fabricated-Default: coalition_type (applied 2026-05-30)

**File**: `python/enrich_pipeline_json.py`

When `orbit_data.json` fails to load, the original code set `orbit_data = {}`, causing
`coalition_type = classify_coalition({}) = "other"` for all constraints. Fixed: set
`orbit_data = None` on load failure; `enrich_entry` now writes `coalition_type = None`
when `orbit_data is None`.

**Severity**: LATENT on current corpus (orbit_data.json covers 194/194 constraints).
Defensive hardening.

**Witness** (run against test entry):
```
coalition_type when orbit_data=None:  None    ← file absent
```

Schema: `("coalition_type", str, True)` — nullable to accommodate None.

---

## F4 — Sidecar Channel Opened (applied 2026-05-30)

**File**: `python/shared/schemas.py`

`validate_report_sidecar` had no call to `_warn_unexpected_fields`. Sidecar DRIFT-A was
completely silent. Two undeclared fields were written by `enhanced_report.py`:

- `stability_band` (enhanced_report.py:2782) — always written
- `husk_metrics` (enhanced_report.py:457) — always written

Both added to `SIDECAR_FIELDS`. `_warn_unexpected_fields` logic added to
`validate_report_sidecar`. Future undeclared sidecar fields will now surface.

**Witness** (SC4):
```
Schema drift warnings (sidecar test):
  [WARN] [test] sidecar unexpected field: _FAKE_xyz   ← channel works
  (stability_band and husk_metrics: no warning — correctly registered)
```

---

## OPEN Items (not applied this pass)

| Item | Description | Decision needed |
|------|-------------|-----------------|
| F0a-fork | linter.py valid_coord_types has 4 values; engine has 6 | Add identity/attachment_coordination to linter, or remove from schema? Repo-owner decision. |
| F0b-bug | Piton declared at theater_ratio=0.58 in cultural_backlash_mobilization and elite_legitimacy_collapse; engine produces tangled_rope/rope at all perspectives — piton gate unreachable | Regenerate or correct the two testsets. Not a fork — fix direction is clear. |
| F0c-regex | linter.py found_types regex (DOTALL) spans into comments; 1/223 testsets affected (generational_economic_decline only) | Strip comments or fix pattern; does not infect F1–F4 findings |
| E2/E3 | validation section has no schema contract | Add VALIDATION_FIELDS analog if section grows? |
| E6-consumers | stability_band sidecar consumer unconfirmed | Determine DRIFT-A-cosmetic vs DRIFT-A-dataloss. (husk_metrics half RESOLVED 2026-06-02: sole consumer was enhanced_report.py `build_husk_signature` — cosmetic/report-only — and the field was removed with the observer husk in ef92a61d; see design_gaps.md GAP-02.) |
