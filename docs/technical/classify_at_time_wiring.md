# classify_at_time/4 — Wiring Gotchas for Surface-3 and OQ-33 Work

Generated 2026-05-30 during authoring-closure + fabricated-default census audit.
Scope: three non-obvious facts about `classify_at_time/4` that produce wrong results or
wrong conclusions when missed. Relevant to anyone approaching Surface 3 (temporal perturbation
primitive), OQ-33 (suppression fabrication), or the Surface-2 Boltzmann floor primitive.

---

## 1. `suppression_requirement` in `measurement/5` is schema-forbidden — the Supp=0.5 fallback is structural

**The predicate** (`drl_composition.pl:179`):

```prolog
classify_at_time(C, Time, Context, Type) :-
    (narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 0.5),
    ...
```

**The mistake:** assuming the `Supp=0.5` fallback fires only on testsets that are missing
the field, and that authoring `suppression_requirement` into the `measurements` section
would fix it.

**Why it's wrong:** the schema's `MeasurementMetric` enum (python/constraint_story_schema.json:108–113)
only permits `["theater_ratio", "base_extractiveness"]`. The compiler (`python/generate_constraint_pl.py:656–669`)
iterates those two keys and emits nothing for any other metric name. The schema is
enforced by jsonschema before any `.pl` file is written — a story with
`measurements[].metric == "suppression_requirement"` produces the error
`'suppression_requirement' is not one of ['theater_ratio', 'base_extractiveness']`
and is rejected at generation time.

**Concrete count:** 0/223 live testsets have a `narrative_ontology:measurement/5` fact
for `suppression_requirement`. The fallback fires on 100% of temporal classifications.

**Tripwire result (execution-witnessed, 2026-05-30):** Source-patch `Supp=0.5` → `Supp=999.9`,
run `constraint_history` over full corpus: 279/647 temporal rows changed.
- 219 tangled_rope → snare
- 60 unknown → snare
- **0 → unknown**

The fabricated Supp=0.5 is below `snare_suppression_floor=0.60`. It blocks the snare gate,
demoting snare-eligible constraints to tangled_rope or unknown. 50.4% of non-unknown temporal
rows (279/553) are systematically mis-classified too low. See OQ-33 for resolution options.

**Note on static fallback (D2, drl_core.pl:96):** The static path uses `Supp=0` (not 0.5).
BUT the 32 testsets lacking `constraint_metric.*suppression_requirement` are `_contradictions.pl`
stubs, excluded by `all_corpus_constraints/1`. Tripwire shows 0 changes on the 191 classified
constraints. D2 is DORMANT on the live classified corpus.

**Consequence for Surface-3 primitive:** there is no clean authored baseline for temporal
suppression. Any perturbation of the temporal surface (`constraint_history/3`) runs against
a fabricated baseline, not a measured one. The primitive should not be built until OQ-33
is resolved (options a/b/c in ISSUES.md).

**There is also a `base_extractiveness` fallback** (`drl_composition.pl:180`):

```prolog
    (narrative_ontology:measurement(_, C, base_extractiveness, Time, BaseX) -> true ; BaseX = 0.5),
```

`base_extractiveness` IS authorable (in the schema enum), so this fallback only fires
on the 32 testsets that have no `measurements` section at all, or at queried time-points
not in a testset's authored series. It is latent, not structural.

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
