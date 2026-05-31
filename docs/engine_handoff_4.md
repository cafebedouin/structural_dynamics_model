# Engine Handoff No. 4 — Denominator Closed, Backlog Live

*2026-05-29. A snapshot for the next session. Read Handoffs 1–3 first for the thesis (radio
telescope for constraints; the engine points, does not adjudicate; nothing self-certifies, so
trust = external perturbation that survives, claim by claim; coverage makes a green readable
rather than blind; demotion status is per-instrument, not absolute). This doc is current state,
not a narrative. Verify against the live repo — re-cut cold; do not trust this doc over a run.*

---

## The discipline this session is built on (one note, load-bearing)

Nothing built this session was *wrong*. What needed correcting, repeatedly, was claims sitting one
tier above their evidence — characterized-not-pasted, asserted-path-not-grepped, errored-counted-as-
swept. The over-stating report is not a competence failure; it is the direction a summary drifts
when nothing perturbs it, and **it drifts most invisibly when the surrounding work is strong enough
to vouch for it.** The fix that held: paste-or-untag, per claim, no exceptions for claims riding in
on good work. The witness-tier ledger below exists so the next session inherits *which claims are on
the floor and which are one paste from it* — do not re-flatten it.

---

## Current state — one number per question

**Denominator: 191 engine params (+ 6 authored fields = 197 type-moving predicates).** Witnessed by
bidirectional dataflow trace, residual zero both directions. The 191 = 168 `config:param/2` + 23
supplementary (11 `power_role_heuristic/4`, 6 `exit_modulation/2`, 6 `positional_displacement/2`).
`perturb.py` reaches all 191 (extended this session from the 168 config-only).

**Three observable surfaces — named so they don't collapse into one:**
- **Surface 1 — static type** (`product_site_export` → `dr_type/3`): 191 engine params + 6 authored
  fields (extractiveness, suppression, theater_ratio, accessibility_collapse, resistance,
  DirectionalityOverride.d_value). This is the surface `perturb.py` sweeps.
- **Surface 2 — excess-extraction / PoA** (`boltzmann_compliance`): `boltzmann_floor_override` + the
  boltzmann config params. **Unexported.** Moves extractive overhead, not static type.
- **Surface 3 — temporal / drift** (`classify_at_time/4`): Measurement.value/time_point,
  interval.start/end. **Unexported** — the static export calls `dr_type/3` directly and bypasses it.

Surfaces 2 and 3 are named future fronts, not omissions. The 5 authored fields that are "dead for
static export" are live on these surfaces.

**Demotion sort — every one of the 191 has a witnessed status:**

```
  6  shadowed              positional_displacement — inert-at-current-config (cognitive_displacement_
                           profile=uniform). coverage=0 is CONDITIONAL, labeled; live if profile→positional.
  0  errored-untested      (empty — see below)
 20  unperturbable          witnessed coverage=0 (4 original + 16 integer-inert)
  0  reachable-but-locked   empty: the category is real, no PARAM occupies it; the lock lives at
                           READING granularity (welfare_reading / false_natural_law)
 24  perturbed-and-survived final-type flips, 18 kernels, wired into _WITNESSED_PARAMS + _WITNESSED
141  perturbable-but-unperturbed   the remaining backlog, epsilon-first
───
191  ✓   every term witnessed; none counted-as-handled-that-wasn't
```

**OQ-32: RESOLVED.** The reorg path bug (`parent.parent → parents[2]`) is fixed in all 6 affected
sweep scripts. The "Front B gated behind OQ-32" framing from earlier handoffs no longer applies.

**Reading backlog: 126 no-kernel readings** on the current corpus (`c70e6a2b1aad`), outside the
kernel instrument by design. The 7 full-CS-no-kernel readings are GENUINE kernel-less constraints
(repo-owner confirmed) — not an authoring gap, no relinking. Class-perturbability is witnessed on
`testsets_3000` (14 flips, bifurcation_sweep) but the corpus boundary holds: current-corpus entries
are UNRUN. With OQ-32 resolved, the single-constraint diagnostics that probe these are live again.

---

## How the denominator was derived (provenance — one paragraph)

The number went directory-glob → name-pattern-glob → named-forms → grep-the-whole-namespace (choked
on "huge") → schema authoring-closure → **bidirectional dataflow trace**, and only the last is
complete-by-construction. Backward from `classify_from_metrics/6`: every numeric predicate on a path
*to* the classifier (the 191; exclusions — `type_contamination_strength`, `type_immunity`,
`category_profile*`, the Boltzmann static-type path — confirmed off-path by pasted EXIT-1 greps).
Forward from the 11 schema-authored fields (authoring-closure: nothing enters unauthored, so the
schema is the complete input set): which reach final type (6 live, 5 dead-for-export, partitioned by
surface). Residual zero both directions. The lesson worth carrying: completeness is a property of
*following edges*, not of *searching nodes* — a trace cannot commit the "narrow substrate labeled as
the whole" defect that every prior method did, because its substrate IS the dataflow.

---

## Witness-tier ledger (do not re-flatten)

- **191 engine params — grep-witnessed** (backward trace + pasted EXIT-1 exclusions).
- **6 authored live fields — grep-witnessed + perturb-confirmed** (2026-05-30). Per-field ordered
  edge greps pasted below; liveness verified by per-field retractall/assertz overlay with per-context
  type flips pasted. Authored-live count: **6**. Denominator: **197** (191 engine params + 6 authored
  fields). Controls (2 dead-for-static-export fields) confirm chain breaks.

  **CONTROLS (dead-for-static-export — chains break):**

  Control A — boltzmann_floor_override:
  - `grep -rn 'boltzmann_floor_override' prolog/ --include='*.pl' | grep -v 'testsets|archive'`
    → matches only in `boltzmann_compliance.pl:453`, `narrative_ontology.pl:69,93`, `config.pl:367` — zero
    matches in `drl_core.pl` or `product_site_export.pl`.
  - `grep -n 'boltzmann_compliance|use_module' prolog/product_site_export.pl`
    → `use_module(covering_analysis)`, `use_module(constraint_indexing)`, `use_module(drl_core)`,
    `use_module(domain_priors)`, `use_module(data_repair)` — no `use_module(boltzmann_compliance)`.
  - `grep -n 'dr_type|boltzmann' prolog/product_site_export.pl`
    → `drl_core:dr_type(C, Ctx, T)` only. Chain terminates at boltzmann_compliance.pl (Surface 2).

  Control B — Measurement.value (and time_point, interval.start/end):
  - `grep -rn 'narrative_ontology:measurement\b' prolog/ --include='*.pl' | grep -v 'testsets|archive'`
    → matches only in `drl_composition.pl` (classify_at_time, constraint_history, drift predicates)
    and `coercion_projection.pl`, `drl_counterfactual.pl` — zero in `drl_core.pl` or
    `product_site_export.pl`.
  - `grep -n 'classify_at_time' prolog/product_site_export.pl prolog/drl_core.pl` → exit 1 (zero matches).
  - `grep -n 'dr_type|classify_at_time' prolog/product_site_export.pl`
    → `drl_core:dr_type(C, Ctx, T)` only. Chain terminates at drl_composition.pl (Surface 3).

  **FIELD 1: extractiveness — live via classify_from_metrics/6 argument (BaseEps)**

  Edge chain:
  - schema: `"extractiveness"` in base_properties (schema.json:288,296)
  - generator: `generate_constraint_pl.py:435` emits
    `narrative_ontology:constraint_metric({cid}, extractiveness, {v})`
  - config: `config.pl:24` `param(extractiveness_metric_name, extractiveness)`
  - constraint_indexing.pl:524–525: `config:param(extractiveness_metric_name, ExtMetricName)`,
    `narrative_ontology:constraint_metric(Constraint, ExtMetricName, BaseScore)` in
    `extractiveness_for_agent/3`
  - drl_core.pl:84: `base_extractiveness(C, V) :- constraint_data:base_extractiveness(C, V)`
  - drl_core.pl:431: `base_extractiveness(C, BaseEps)` in `metric_based_type_indexed`
  - drl_core.pl:309–392: `classify_from_metrics` gates on BaseEps (mountain:312–313, piton:326,
    snare:337, rope:356, tangled_rope:367–368, naturalized:389–391)

  Liveness (testset: `bodily_autonomy_primary`, ε=0.68, signature=false_ci_rope with variance):
  - Baseline: powerless→snare, analytical→snare
  - ε=0.20: powerless→**rope** (flip), analytical→**rope** (flip),
    institutional/immediate naturalized→**tangled_rope** (flip)
  - Overlay confirmed: `constraint_metric(bodily_autonomy_primary, extractiveness, V)` = 0.2

  **FIELD 2: suppression — live via classify_from_metrics/6 argument (Supp)**

  Edge chain:
  - schema: `"suppression"` in base_properties (schema.json:289,301)
  - generator: `generate_constraint_pl.py:436` emits
    `narrative_ontology:constraint_metric({cid}, suppression_requirement, {v})`
  - config: `config.pl:23` `param(suppression_metric_name, suppression_requirement)`
  - drl_core.pl:94–96: `get_raw_suppression` reads via `config:param(suppression_metric_name,
    ActualMetricName)` → `narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value)`
  - drl_core.pl:433: `get_raw_suppression(C, Supp)` in `metric_based_type_indexed`
  - drl_core.pl:309–394: classify_from_metrics gates on Supp (mountain:310–311, snare:338–339,
    tangled_rope:369)

  Liveness (testset: `bodily_autonomy_primary`, suppression=0.75):
  - Baseline: powerless→snare, analytical→snare
  - supp=0.20: powerless→**tangled_rope** (flip), analytical→**tangled_rope** (flip)
    [snare gate fails at Supp=0.20 < snare_suppression_floor=0.60; metric→unknown; false_ci_rope
    with unknown → tangled_rope]
  - Overlay confirmed: `constraint_metric(…, suppression_requirement, V)` = 0.2

  **FIELD 3: theater_ratio — live via classify_from_metrics/6 piton clause (TR lookup on C arg)**

  Edge chain:
  - schema: `"theater_ratio"` in base_properties (schema.json:111,290,306)
  - generator: `generate_constraint_pl.py:437` emits
    `narrative_ontology:constraint_metric({cid}, theater_ratio, {v})`
  - config: `config.pl:25` `param(theater_metric_name, theater_ratio)`
  - drl_core.pl:292–296: `effective_theater_ratio(C, MetricName, TR)` falls back to
    `narrative_ontology:constraint_metric(C, MetricName, TR)`
  - drl_core.pl:327–330, 380–383: piton-dead-coord and piton-fallback clauses inside
    `classify_from_metrics` call `effective_theater_ratio(C, TheaterMetricName, TR)` and gate
    on `TR >= piton_theater_floor` (config.pl:265, value 0.70)

  Note: TR is read inside classify_from_metrics via a lookup on the constraint arg C, not passed
  as a top-level argument to classify_from_metrics/6.

  Liveness (testset: `cyclopean_point_as_manufactured_center`, TR=0.78, signature=false_ci_rope
  with variance):
  - Baseline: institutional/civ/trapped→**piton**, institutional/immediate/trapped→**piton**
  - TR=0.30: institutional/civ→**naturalized** (flip), institutional/immediate→**naturalized** (flip)
    [piton gates fail at TR=0.30 < piton_theater_floor=0.70]
  - Overlay confirmed: `constraint_metric(…, theater_ratio, V)` = 0.3

  **FIELD 4: accessibility_collapse — live via integrate_signature_with_modal/3 (signature path)**

  Edge chain:
  - schema: `"accessibility_collapse"` in base_properties (schema.json:334)
  - generator: `generate_constraint_pl.py:444` emits
    `narrative_ontology:constraint_metric({cid}, accessibility_collapse, {v})`
  - signature_detection.pl:136: `get_metric_average(C, accessibility_collapse, AccessCollapse)`
    in `get_constraint_profile/7`
  - signature_detection.pl:155: `get_metric_average` reads via
    `findall(Val, narrative_ontology:constraint_metric(C, MetricType, Val), Vals)`
  - signature_detection.pl:280–296: `natural_law_signature` gates on
    `AccessCollapse >= CollapseMin` (natural_law_collapse_min)
  - signature_detection.pl:97–100: `constraint_signature(C, natural_law)` calls
    `get_constraint_profile` and `natural_law_signature`
  - drl_core.pl:419: `signature_detection:integrate_signature_with_modal(C, MetricType, FinalType)`
    in `dr_type/3` — calls `constraint_signature(C, Signature)` and
    `resolve_modal_signature_conflict(ModalType, natural_law, Result) :- !, Result = mountain.`

  **PATH ANNOTATION**: accessibility_collapse reaches `dr_type/3` via `integrate_signature_with_modal/3`
  (signature override), NOT via `classify_from_metrics/6` argument slots. Both paths are inside
  `dr_type/3`; the field IS live on Surface 1.

  Liveness (testset: `explanatory_closure_mechanism`, AC=0.92, signature=natural_law baseline):
  - Baseline: powerless→**mountain**, analytical→**mountain**
  - AC=0.50: powerless→**rope** (flip), analytical→mountain [immutability still gives mountain
    at analytical via metric path]
  - New signature at AC=0.50: **ambiguous** (natural_law broken because AC < natural_law_collapse_min=0.85)
  - Overlay confirmed: `constraint_metric(…, accessibility_collapse, V)` = 0.5

  **FIELD 5: resistance — live via integrate_signature_with_modal/3 (signature path)**

  Edge chain:
  - schema: `"resistance"` in base_properties (schema.json:340)
  - generator: `generate_constraint_pl.py:446` emits
    `narrative_ontology:constraint_metric({cid}, resistance, {v})`
  - signature_detection.pl:138: `get_metric_average(C, resistance, Resistance)` in
    `get_constraint_profile/7` (same get_metric_average path as accessibility_collapse)
  - signature_detection.pl:291–292: `natural_law_signature` gates on
    `Resistance =< ResMax` (natural_law_resistance_max=0.15)
  - Same integrate_signature_with_modal → dr_type chain as Field 4.

  **PATH ANNOTATION**: same as accessibility_collapse — via signature override, not
  classify_from_metrics/6 argument slots.

  Liveness (testset: `explanatory_closure_mechanism`, resistance=0.08, isolated test —
  only resistance changed, AC and emerges_naturally held at baseline):
  - Baseline: powerless→**mountain**, analytical→**mountain**
  - resistance=0.40: powerless→**rope** (flip), analytical→**rope** (flip)
  - New signature at resistance=0.40: **constructed_low_extraction** (natural_law broken because
    Resistance=0.40 > natural_law_resistance_max=0.15; Extraction low → constructed_low;
    constructed_low_extraction + mountain → rope)
  - Overlay confirmed: `constraint_metric(…, resistance, V)` = 0.4

  **FIELD 6: DirectionalityOverride.d_value — live via classify_from_metrics/6 argument (Chi)**

  Edge chain:
  - schema: `"d_value"` in DirectionalityOverride (schema.json:221,227)
  - generator: `generate_constraint_pl.py:709` emits
    `constraint_indexing:directionality_override({cid}, {power_atom}, {d_value})`
  - constraint_indexing.pl:61–62: `directionality_override/3` declared multifile+dynamic
  - constraint_indexing.pl:405–412: `derive_directionality` first clause reads
    `directionality_override(Constraint, Power, D)` as first priority
  - constraint_indexing.pl:526–531: `extractiveness_for_agent` calls `derive_directionality`,
    then `sigmoid_f(D_eff, PowerMod)`, then
    `Score is BaseScore * PowerMod * ScopeMod` (= Chi)
  - drl_core.pl:432: `constraint_indexing:extractiveness_for_agent(C, Context, Chi)` in
    `metric_based_type_indexed`
  - drl_core.pl:332–392: `classify_from_metrics` gates on Chi (snare:334–335, scaffold:344,
    rope:351–358, tangled_rope:363–366, piton-fallback:376–377, naturalized:391)

  Liveness (testset: `regulatory_recognition_reading`, institutional d=0.08, signature=false_ci_rope
  with variance):
  - Baseline: institutional/civ/trapped→**rope**, institutional/immediate/trapped→**naturalized**
  - d_value institutional=0.80: institutional/civ→**snare** (flip), institutional/immediate→**snare**
    (flip) [d=0.80 → higher sigmoid → higher Chi → snare gate succeeds]
  - Overlay confirmed: `constraint_indexing:directionality_override(…, institutional, V)` = 0.8

  Corpus validation post-test: `run_dynamic_suite` → Errors: 0, Warnings: 0. ✓
- **24 survivors — witnessed** (per-pair coverage/fold_survival + sampled before/after final types:
  e.g. `snare_chi_floor`×sovereign_legitimacy `tangled_rope→snare`; integer survivors
  `boltzmann_min_classifications` `rope→scaffold`, `critical_mass_threshold`
  `tangled_rope/snare↔naturalized`, `fcr_override_enabled` `tangled_rope→scaffold`). These survived
  the signature layer — type flips, not metric movement a signature eats.
- **223-pass suite (post `:- dynamic` declarations), overlay smoke tests, Fisher-fires-on-non-
  witnessed-path — witnessed** (pasted runs).
- **OQ-32 fix, priority-sort fix, Task 4/Task 5 commit — instance-reported, not PM-seen.** Cheap to
  confirm: re-run one OQ-32 script cold; re-run the demotion sort; re-run the Task-4 guard tests.

The errored-untested bucket is empty *because it was emptied honestly*: 19 integer-typed params
errored under the ±10% float-multiply sweep (silently — the summary's clean sum hid them as "swept").
Re-run at integer ±1 → 3 survivors + 16 witnessed-inert. The lesson: **errored ≠ inert ≠ swept; an
errored param is UNTESTED and must never be counted as handled.** A sum can reconcile while one term
is doing dishonest work — perturb the terms, not just the total.

---

## Orphaned outputs — closed

- `alt_power_transform_results.json` — **deleted** (testsets_3000 corpus, no re-run path).
- `outputs/epsilon_sensitivity_results.json` — **wired** into the E5 Fisher-probe sub-section (fires
  on all paths, witnessed on the non-witnessed `abolition_reading`: Fisher 17.417).
- The 6 pre-fix OQ-32 sweep outputs — **the hold is discharged** (OQ-32 resolved). Next instance:
  re-run each fixed script, diff against the pre-fix output, then delete the stale pre-fix file. Until
  diffed, keep — they are the only baseline for the post-fix comparison.
- `config_sensitivity_results.json`, `directionality_sensitivity_results.json` — **superseded** by the
  extended `perturb.py` (now covers their 168 + 23 params). Delete after one confirming diff that
  perturb.py's coverage matches; if either captures a signal perturb.py doesn't, wire instead. One
  decision, then close.

---

## The next move (fronts, by readiness)

1. **Front A — param backlog, 141, epsilon-first. LIVE, ungated, the named one thing.**
   `python3 python/sweeps/witness_backlog.py --resume`. The 4 epsilon params (`rope_epsilon_ceiling`,
   `tangled_rope_epsilon_floor`, `fpn_epsilon`, `piton_epsilon_floor`) now sort first (priority bug
   fixed) and haven't been tested at adequate values. Per-kernel-per-param, perturb-and-observe each
   — a param governing one kernel may be signature-locked on another (OQ-30, `tangled_rope_chi_floor`).
   Survivor = coverage>0 AND fold_survival<1.0 AND a final-type flip.
2. **Confirm OQ-32** by re-running one of the six fixed scripts cold (it's instance-reported).
3. **Front C — reading backlog, 126.** Now unblocked. Owed: a per-reading witness on the *current*
   corpus (the testsets_3000 class witness does not transfer — corpus boundary).
4. **Front B — Surfaces 2 & 3.** Scope a per-surface primitive (different from `perturb.py`, which
   overlays Surface-1 predicates). Do NOT force them into the Surface-1 primitive (false unification).
   Lowest urgency, highest novelty.

---

## Verify-or-correct for the next session (paste real output; a code-read does not close these)

1. **The 6 authored-field forward paths** — trace-asserted, not grep-witnessed. Paste the per-field
   edge-greps to graduate them; if any path doesn't hold, the 6 (and the 197) shift.
2. **OQ-32 fix** — re-run one of the six scripts cold, paste the clean run (no `config.pl not found`).
3. **Task 4 guard tests** (`check_orbits_corpus_hash` 3-case, `_manifest_step` hard-error) — were
   instance-reported in their session, not PM-seen. Re-run, paste PASS.
4. **The demotion sort** — re-run `--json-out`, confirm the 6/0/20/0/24/141=191 block reproduces.

---

## Substrate facts (land these where noted)

- **Guard 2 verifies testset identity, not export coverage.** A testset can fail to produce an orbit
  entry (`catholic_church_1200`: 192 pipeline / 191 orbits) and Guard 2 passes silently. The 191/223
  gap (32 no-orbit testsets, OQ-29) is the same blindness at larger scale. → `build_discipline.md`,
  adjacent to stale-corpus.
- **OQ-30 confirmed by witness:** `tangled_rope_chi_floor` flips on `false_ci_rope` readings, locks on
  `false_natural_law` (`welfare_reading`). Per-kernel-per-param is empirical, not rule-derivable.
- **Errored ≠ inert ≠ swept** (the integer-19 lesson). → `build_discipline.md`, defect families.
- **Completeness is edge-closure, not node-search** (the denominator arc). → the discipline note.

---

## Pointers

`python/sweeps/perturb.py` (extended to 191; PRH/EM abolish+reassert overlays, PD retract/asserta) ·
`python/sweeps/demotion_pass.py` (the sort; 6/0/20/0/24/141; `_WITNESSED` holds 24) ·
`python/sweeps/witness_backlog.py` (`--resume` for Front A) · `python/sweeps/regenerate_orbits.py`
(atomic regen, Handoff 3) · `python/enhanced_report.py` (`_WITNESSED_PARAMS`, 18 kernels; E5 band +
Fisher probe) · `prolog/constraint_indexing.pl` (`:- dynamic` PRH/EM/PD declarations) ·
`prolog/drl_core.pl` (`classify_from_metrics/6` — the backward-trace root) ·
`agent/data/constraint_story_schema.json` (the 11 authored fields — the forward-trace root) ·
`ISSUES.md` (OQ-29 partial; OQ-30 confirmed; OQ-31 done; OQ-32 resolved) ·
`docs/technical/build_discipline.md` (defect families) · Handoffs 1–3.
