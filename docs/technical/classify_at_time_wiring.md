# classify_at_time/4 — Wiring Gotchas for Surface-3 and OQ-33 Work

Generated 2026-05-30 during authoring-closure + fabricated-default census audit.
Scope: three non-obvious facts about `classify_at_time/4` that produce wrong results or
wrong conclusions when missed. Relevant to anyone approaching Surface 3 (temporal perturbation
primitive), OQ-33 (suppression fabrication), or the Surface-2 Boltzmann floor primitive.

---

## 1. The `suppression_requirement` temporal fallback — corrected by row-23 (was `Supp=0.5`)

> **RECONCILED 2026-05-31 (Commit A / row-23 + a non-arc schema-enum change).** This section's
> original (2026-05-30) claims are **now false** and were left as a cautionary record of
> distrust-the-aggregate: (a) `suppression_requirement` is **not** schema-forbidden — the
> `MeasurementMetric` enum was later expanded to include it; (b) the count is **not** 0 — live
> ground truth is **6** `measurement/5` suppression facts; (c) the `Supp=0.5` fabricated fallback
> was **replaced**. Corrected below; the tripwire history is retained because it is the witness for
> row-23.

**The predicate today** (`drl_composition.pl`, `classify_at_time/4` — restructured by row-23):

```prolog
( narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp)   % temporal series
-> classify_at_time_with_supp(C, Time, Context, Supp, Type)
;  narrative_ontology:constraint_metric(C, suppression_requirement, Supp)       % authored SCALAR
-> classify_at_time_with_supp(C, Time, Context, Supp, Type)                      % STOPGAP (OQ-46)
;  Type = unknown ).
```

**What is true now (verify against the live schema/corpus):**
- **The schema ALLOWS temporal suppression.** `$defs/MeasurementMetric` is
  `[theater_ratio, base_extractiveness, suppression_requirement]` — three metrics. A story may
  author a `suppression_requirement` measurement series; `generate_constraint_pl.py` emits it (the
  `sr_measurements` branch — see `generator_emission_map.md`'s measurement whitelist). **Live: 6
  suppression `measurement/5` facts exist**, not 0.
- **The `Supp=0.5` fabrication is gone (row-23, OQ-41).** Absent temporal suppression falls back to
  the authored *scalar* `constraint_metric(C, suppression_requirement, _)` — real per-constraint
  data — and returns `unknown` only if no suppression is authored anywhere (0 rows on the live corpus).

**Why it still matters:** most constraints author only the scalar (650/656 timeline rows have **no**
temporal suppression series), so the scalar fallback is the live path for nearly all temporal
classification. It is no longer a *fabrication* (it is authored data), but it is still not a measured
*trajectory* — it is a STOPGAP until the generation template authors temporal series (OQ-46). A
Surface-3 temporal-suppression primitive should still wait on OQ-46, but for the corrected reason
(scalar-as-constant, not "schema-forbidden").

**Historical tripwire (2026-05-30, the pre-row-23 `Supp=0.5` fabrication, kept as the row-23 witness):**
source-patch `Supp=0.5` → `999.9`, `constraint_history` over the corpus: 279/647 rows changed (219
tangled_rope→snare, 60 unknown→snare, 0→unknown). The fabricated 0.5 sat below
`snare_suppression_floor=0.60`, demoting snare-eligible rows low. Row-23 fixed it: substituting the
authored scalar moved **268** rows (the same low-mis-sort, now resolved upward). See KNOWN_STATE.md
(2026-05-31) and ISSUES.md OQ-41.

**The `base_extractiveness` fallback** (`drl_composition.pl`, same clause) keeps `BaseX = 0.5` on
absence — `base_extractiveness` is schema-authorable and almost always present, so this fallback is
**latent** (tripwire: 0 changes). It was *not* changed by row-23 (row 24 of the census; left as latent).

---

## 2. `classify_at_time` bypasses the signature override layer

**The call chain** (`drl_composition.pl:193`):

```prolog
classify_at_time(C, Time, Context, Type) :-
    ...
    drl_core:classify_from_metrics(C, BaseX, Chi, Supp, Context, Type).
```

**The call chain in `dr_type/3`** (the static path):

```prolog
dr_type(C, Context, FinalType) :-
    classify_from_metrics(C, BaseEps, Chi, Supp, Context, MetricType),
    integrate_signature_with_modal(C, MetricType, FinalType).   % ← NOT called by classify_at_time
```

**The mistake:** expecting `classify_at_time` and `dr_type` to agree on signature-overridden
constraints.

**Concrete divergence:**
- A constraint with `false_natural_law` signature gets `tangled_rope` from `dr_type` at every
  context (the signature override fires unconditionally inside `integrate_signature_with_modal`).
- `classify_at_time` on the same constraint returns whatever `classify_from_metrics` returns
  at each time-indexed (BaseX, Chi, Supp) triple — which may be `naturalized`, `rope`, or
  `unknown` depending on the time-series values.
- The temporal and static surfaces diverge by construction on any signature-locked constraint.
  This divergence is NOT an observational signal — it is a structural artifact of
  `classify_at_time` not going through the signature layer.

**Implication for cross-surface analysis:** do not interpret a static/temporal type mismatch
as evidence of temporal drift on constraints with non-trivial canonical signatures
(`false_natural_law`, `false_ci_rope`, `false_summit_mountain`, `coupling_invariant_rope`).
The mismatch is in the call chain, not in the constraint's structural history.

**Check signature before running temporal comparisons:**

```prolog
signature_detection:constraint_signature(C, Sig),
memberchk(Sig, [false_natural_law, false_ci_rope, false_summit_mountain,
                coupling_invariant_rope])
```

If this succeeds, any static/temporal divergence for C is call-chain artifact.
Only `constructed_*` signatures preserve the metric type (relevant for `vulnerability_protection_reading`
and a handful of others).

---

## 3. `boltzmann_floor_for/2` is NOT a fabricated-default — its fallback reads from config

**The predicate** (`boltzmann_compliance.pl:452–459`):

```prolog
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:boltzmann_floor_override(C, Floor), !.
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_floor_param(Type, ParamName),
    config:param(ParamName, Floor), !.
boltzmann_floor_for(_, Floor) :-
    config:param(boltzmann_floor_default, Floor).
```

**The mistake:** expecting to tripwire `boltzmann_floor_for` by poisoning a hardcoded
constant the way `Supp=0.5` can be tripwired. There is no hardcoded constant.

**Why this matters for Surface-2 primitive work:** the correct perturbation target is the
`boltzmann_floor_*` family of config params (e.g. `boltzmann_floor_identity_coordination`)
via retract/asserta overlay — the same pattern as all Surface-1 config-param perturbations.
The proof-of-life (`python/sweeps/proof_of_life_surface2.py`) overlaid
`boltzmann_floor_identity_coordination` 0.08→0.60 for `civic_eugenic_reading` and
confirmed a −0.52 delta in `excess_extraction/2`. That is the correct instrument.

**Fallback path** (when `boltzmann_floor_override` and `coordination_type` are both absent):
`config:param(boltzmann_floor_default, Floor)` — a parameterised value, auditable and
perturbable via the standard config overlay. It is not a fabricated default; it is a
documented fallback with a named config param.

**Priority chain for a constraint with no authored Boltzmann data:**
1. `boltzmann_floor_override/2` fact → exact per-constraint override (authored in testset)
2. `coordination_type/2` fact → type-specific floor param from config
3. Catch-all → `boltzmann_floor_default` from config

`coordination_type_offset/2` has the same three-clause structure with `complexity_offset_default`
as the catch-all.
