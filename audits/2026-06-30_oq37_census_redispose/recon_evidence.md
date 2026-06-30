# OQ-37 census re-disposition — Phase 1 recon evidence (read-only)

Date: 2026-06-30. Method: collect raw evidence, no analysis (analysis in `writeup.md`).
All claims below cite tool output or code inspection, verified against live source (not the
drifted OQ-37 census text).

## 0. The confound's root — the compiler emit set (`python/generate_constraint_pl.py:608-635`)

`constraint_metric` facts emitted, verbatim from source:
```
narrative_ontology:constraint_metric({cid}, extractiveness, ...)        always
narrative_ontology:constraint_metric({cid}, suppression_requirement, ...) always
narrative_ontology:constraint_metric({cid}, theater_ratio, ...)         always
narrative_ontology:constraint_metric({cid}, accessibility_collapse, ...) mountain only (gated)
narrative_ontology:constraint_metric({cid}, resistance, ...)            mountain only (gated)
narrative_ontology:has_sunset_clause({cid}).                            when bp flag true (:634)
```
NEVER emitted: sunset_time · alternatives_available · accumulation_speed ·
internalization_depth · resistance_to_change · inevitability.
⇒ every "read-but-never-authored" name traces to this fixed emit set. "Liven" = add to compiler
(+ schema + validator + prompt); "strip" = remove a consumer of a never-emitted name.

## 1. Authoritative cross-corpus fact census

`grep -rhoE "constraint_metric\([^,]+,[[:space:]]*<Name>,[^)]*\)"` (the FACT pattern — NOT the
bare name, which also appears as omega-variable IDs and narrative text: this session's
false-presence trap).

| name (target)          | testsets(116) | haiku(960) | flash(960) | kernel_v1(1106) |
|------------------------|---------------|------------|------------|-----------------|
| inevitability          | 0 | 0 | 0 | 0 |
| internalization_depth  | 0 | 0 | 0 | 0 |
| resistance_to_change   | 0 | 0 | 0 | 0 |
| accumulation_speed     | 0 | 0 | 0 | 0 |
| sunset_time            | 0 | 0 | 0 | 0 |
| alternatives_available | 0 | 0 | 0 | 0 |
| **resistance** (control) | **130** | **1032** | **1064** | **101** |
| **extractiveness** (control) | **113** | **960** | **960** | **1550** |

Positive controls fire on every leg ⇒ the probe is live; the six 0-counts are genuine absence
(authored-zero across 3,142 stories), not a dead grep.

## 2. Per-name read sites (engine source; testsets/archives/probsets excluded)

- **inevitability** — NO engine read. Sole consumer (`constraint_status/3` `binding_limit` clause)
  ALREADY REMOVED (D2 strip; documented `constraint_bridge.pl:20-25`). Remaining greps =
  omega-variable names + narrative text in testset stories. ✔ plan disposition #1.
- **resistance_to_change** — 8 read sites, 6 modules: `data_validation.pl:320` (member list),
  `report_generator.pl:650` (safe_get_metric dflt 0.0 → MISSING), `:818` (label text),
  `json_report.pl:265` (serializes null when absent), `utils.pl:205` & `:213` (in helpers w/ 0
  callers), `:346-348` (debug print), `metric_drift_events.pl:174` & `:247`
  (`function_obsolescence` detector + `drift_event` clause). ✔ plan disposition #2.
- **accumulation_speed** — sole read `utils.pl:211`, inside `safe_get_profile_components/2`
  (def `utils.pl:210`), which has ZERO callers across prolog/python/agent. Dead orphan helper. ✔ #5.
- **sunset_time** — reads `metric_drift_events.pl:185` & `:252` only; never emitted by compiler. ✔ #3.
- **internalization_depth** — sole read `psych_bridge.pl:19` (`with_psych_metric/2`). `psych_bridge`
  NOT in `stack.pl` and no `use_module(psych_bridge)` anywhere → module never loaded. Two breaks:
  module unloaded + input never emitted. ✔ #4.

## 3. has_sunset_clause is LIVE (settles Phase-1(c))

Emitted as a fact (`generate_constraint_pl.py:634`, gated on bp flag). Live reads:
`drl_core.pl:294` (scaffold_temporality_check cl.1), `signature_detection.pl:1260` (pure_scaffold),
`metric_drift_events.pl:184,251`, `logical_fingerprint.pl:173,219,243,253`, `omega1_audit.pl:301`,
`invertibility_analysis.pl:122`, `cs_pattern_detection.pl:218`. NOT a wiring bug — authored-sparse.
✔ plan disposition #3 (has_sunset_clause).

## 4. `safe_metric/3` fails silently (metric_drift_events.pl:66)

```
safe_metric(C, Metric, Value) :- narrative_ontology:constraint_metric(C, Metric, Value).
```
No default. ⇒ `detect_function_obsolescence` dies at its FIRST goal
(`safe_metric(C, alternatives_available, Alt)`, never authored) — never reaches the
`resistance_to_change` read at :174. The detector is dead at the head, not at the resistance read.

## 5. resistance ≠ resistance_to_change (OQ-64 morphology check)

- `resistance` = NL/coercion-GRID metric: one of `GRID_METRICS = [accessibility_collapse,
  stakes_inflation, suppression, resistance]` (`python/grid_first_contact_gate.py:48`); NL-profile
  feature #3 in `get_constraint_profile` (`signature_detection.pl:182`); validator bounds it for
  mountains (`Mountain resistance > 0.15` flagged, `validate_constraint_story.py:240-246`). Feeds
  the false_natural_law signature path.
- `resistance_to_change` = drift-domain concept (institutional inertia / resistance to abolition),
  used ONLY in `function_obsolescence` ("low resistance → obsolete"). Not a grid metric.
- Distinct referents sharing a name-stem ⇒ repointing the dead `function_obsolescence` read to
  `resistance` would be the OQ-64 trap (classify by referent, not value string). See writeup.

## 6. Probe (a0) — inevitability supersession by false_natural_law

`false_natural_law` (FNL) documents capturing "Ideological inevitability claims: 'there is no
alternative' when alternatives exist but are suppressed" (`signature_detection.pl:1018`) — but
STRUCTURALLY: gate is `claimed_natural(C), boltzmann_compliant(C, non_compliant)`
(detector `:1040`; override `:912` FNL→tangled_rope). The scalar `inevitability` metric is NOT on
the FNL path. ⇒ capability superseded by FNL structural detection; scalar metric unneeded.
(Code-read inference; FNL fires on claimed_natural + Boltzmann fail regardless of any scalar cue.)

## 7. Probe (b) — sunset_violation vs scaffold_suppression_escalating non-redundancy

`scaffold_suppression_escalating` (`cs_pattern_detection.pl:207`) fires on a RISING
suppression_requirement SERIES (`metric_trend(C, suppression_requirement, increasing)`) — a
metric-TREND verdict, COMMENTARY/annotate-only (OQ-39 row 14). `sunset_violation`
(`metric_drift_events.pl:182`) fires on `has_sunset_clause(C) + sunset_time < current_year` — a
declared-EXPIRY violation. Orthogonal axes ⇒ the sunset tell is non-redundant; it would survive.

## 8. Probe (d) — Part D masked unknowns are MOOT post-reset

The 3 uncharacterized pre-reset masked-unknown readings:
- `constitutional_supremacy_reading`: ABSENT from testsets/, haiku, flash.
- `hybrid_atrophy_reading`: ABSENT from testsets/; haiku=1, flash=1 (independent redraws, OQ-26).
- `relational_autonomy`: ABSENT from testsets/; haiku=1, flash=1 (independent redraws).
The 2026-06-01 diagnosis was on the pre-reset corpus (reset 2026-06-05). The exact instances no
longer exist in the live leg; twin occurrences are NEW DRAWS, not the same stories. ⇒ cannot
re-witness; record moot. A current-corpus masked-unknown characterization would be a fresh sweep.
