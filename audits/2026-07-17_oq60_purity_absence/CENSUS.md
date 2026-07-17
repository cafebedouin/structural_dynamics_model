# OQ-60 Phase-0 census — findings (2026-07-17)

Read-only census of the five absence mechanisms that terminate in `purity_score`.
Code state: clean HEAD `f3e89f43` (`purity_scoring.pl` + `boltzmann_compliance.pl`
unmodified). Harness: `census_oq60.pl`. Snapshot pin: `pin.txt` (0f).

## Method (0a — instrument the real predicates, do not reconstruct)

Per constraint, the five absence branches are isolated by driving the real predicates'
**own** building blocks (`coupling_test_powers/scopes/context`, `classify_at_context`,
`scope_invariance_test`, `excess_extraction`, `boltzmann_floor_for`) and **cross-validating**
each derived tag against the real predicate's observable output:
- m2 (`GridSize<2`): if tagged, real `cross_index_coupling` must return exactly `0.0`.
- m5 (floor clause 3): if tagged, real `boltzmann_floor_for` must equal `boltzmann_floor_default`.

**Anti-reconstruction control: 0 cross-check mismatches on all four legs** — the instrument
agrees with the real predicates everywhere it fired.

## Mechanism → disposition mapping

A constraint's post-fix disposition:
- **gate-fail** (`epistemic_access_check` fails) → `-1.0` sentinel (UNCHANGED by this fix;
  short-circuits before subscores).
- else **any mechanism m1–m5 fires** → `unknown` (the fix).
- else → `scored` (unchanged).

## Cross-leg result (the headline)

| Leg | rows | sentinel (−1.0) | scored | **→ unknown** | live victims by mech | crosscheck |
|---|---|---|---|---|---|---|
| `testsets/`       | 145  | 28  | 108  | **9**  | all m5 | 0 mismatch |
| `testsets_haiku/` | 960  | 466 | 492  | **2**  | all m5 | 0 mismatch |
| `testsets_flash/` | 960  | 212 | 668  | **80** | all m5 | 0 mismatch |
| `kernel_v1/`      | 1106 | 2   | 1102 | **2**  | all m5 | 0 mismatch |

**Every live (gate-pass) victim on every leg is mechanism 5 (fabricated floor-default).**
Mechanisms 1–4 (the three originally-witnessed SI-empty / coupling-grid mechanisms of the
2026-06-03 audit, plus grid-empty) have **zero gate-pass victims anywhere**: on `testsets/`
the 11 constraints that trip m1–m4 are *all* gate-fail (already `-1.0`); on the other three
legs m1–m4 never fire at all. This confirms and localizes the origin audit's "latent, no
current victim" finding (`0/1106 variant_0`), and adds the empirical result that the
**only live-firing mechanism is m5 — the one the rev-4 plan added.**

Implication for Phase 1: **C-SI / C-COUPLING / C-CC / C-EX are latent** (expect
`per_constraint` byte-identical on the live-firing witness); **only C-FLOOR moves
constraints** (scored→unknown). The m5 victims have a *full* grid (G=12) and real ε data —
their purity currently rests entirely on `boltzmann_floor_default=0.05` because they carry no
`coordination_type`. Two of the `testsets/` nine are high-scoring:
`conceptual_framework_reading`=0.972, `vocabulary_collision_reading`=0.948 — "pristine by
fabricated floor," the exact OQ-60 pathology.

C-EX-before-C-FLOOR ordering holds: m4 (ex-fail) has 0 gate-pass victims, so C-EX moves
nothing; C-FLOOR then makes `boltzmann_floor_for` fail on absent `coordination_type`, which
makes `excess_extraction` fail → the EX subscore no-data → `unknown`. The m5 effect surfaces
at the EX subscore line (`purity_scoring.pl:88`), which C-EX must have already wired.

## 0b — raw subscore two-sided check (model check)

- **Ceiling violations (raw SI > 1.0 at N≥1): 0 on all legs.** The out-of-range SI=1.25 arises
  *only* at N=0 (`variant([])`), and every such constraint is gate-fail — so in real scoring the
  1.25 is never reached. This validates the formula account: **no subscore ceiling clamp is
  needed** (`purity_scoring.pl:70` stays as-is); a raw SI>1 at N≥1 would falsify the mechanism-1
  analysis, and none exists.
- **Floor violations (raw SI < 0, N≥6): 0 on all legs.** The `max(0.0, …)` at line 70 is not
  load-bearing on any current corpus (no constraint reaches N≥6). Lines 57/79/87 `max(0.0,…)`
  are structurally unreachable-negative (their arguments are already in [0,1]). The only
  load-bearing clamp is the final `min(1.0, max(0.0, …))` at line 50 — which is exactly what
  concealed the 1.25, and is retained.

## 0e — cache audit

- `cached_coupling/2` and `cached_classification/3` are **in-session `:- dynamic` only**,
  **per-constraint keyed** (`cached_coupling(C, Score)`, `cached_classification(C, Ctx, Type)`).
  No coarser-than-per-constraint key → **no memoized-absence residue** between the cache-off
  census and the cache-on pipeline.
- **No on-disk persistence** of coupling/purity across runs (grep: only the in-session `assertz`
  + `cache_registry` hooks; `giant_component:gc_node_purity` is an in-session dynamic too).
- Census ran with caches cleared at leg start; cross-checks (0 mismatch) confirm no stale serve.

## 0f — snapshot pin

`pin.txt`: HEAD `f3e89f43`, TSV sha256 × 4, per-leg file counts. Asserted equal at the
Phase-2 join.

## 0g — report/JSON strict-parser check

- **No strict parser reads `enhanced_report.py` output** (no scraper/glob/parse of
  `constraint_reports/`). The unconditional coverage line (R4) is safe to add.
- **JSON side is permissive**: `python/shared/schemas.py` already declares
  `purity_score: float | None` (nullable, "26/1034 null when purity not computed") with
  no `additionalProperties` enforcement — additive coverage-sibling fields are safe.
- `json_report.pl:322` already maps the `-1.0` sentinel → JSON `null`; Commit 0 extends the
  same guard to `unknown` → `null` (inert until a producer emits `unknown`).

## 0h — golden capture

The census TSVs were produced from **clean HEAD** (unmodified engine), so they ARE the golden.
Test (b)'s two-sided control: `alignment_constraint_narrowing` on `testsets/` — a fully-authored
constraint (override=0.12, coordination_type=enforcement_mechanism, G=12, variant with 2 types) —
scores exactly `purity_score = 0.3541666666666667`. This value must be byte-identical pre/post
every commit. (Captured in `census_testsets.tsv`.)

## 0d — coverage / decision statistic (partial; −1.0-in-aggregate pending 0c)

- Every leg carries ≥1 unknown post-fix. Under R3's default **coverage-1.0** gate, the
  corpus-wide *clean* purity aggregates (`giant_component_analysis.pl:898-904` intrinsic/effective
  min/median/mean) therefore gate to `unknown` on **all four legs**. This is precisely the R3
  falsifier surface: coverage-1.0 blanks the whole aggregate on a single unknown member
  (P(≥1 unknown)=1 for every leg here), so a future relaxation ruling — not this pass — decides
  whether a threshold < 1.0 is warranted. 96/145 `testsets/` constraints carry `coexists_with`
  edges, so per-component refinement is available if that ruling is ever taken.
- **−1.0-in-aggregate census** (OQ-62, from 0c): the explicit purity averages/counts
  (`maxent_report:375`, `maxent_diagnostic:606/616/625`, `grothendieck_cohomology:791`,
  `json_report:2013`, `giant_component:893`) **already filter −1.0** (`>= 0.0` / `\= -1.0`), so
  they are not OQ-62 victims. The real −1.0 exposure is **propagation into shared aggregate
  state, unfiltered at the store**: `drl_purity_network:224` (`effective_purity` returns
  `-1.0` when `Intrinsic = -1.0`), `drl_fpn:107` (asserts `fpn_intrinsic(C, -1.0)`),
  `giant_component:353` (asserts `gc_node_purity(C, -1.0, -1.0)`). Downstream means over network/
  FPN state can therefore average −1.0. **Filed to OQ-62 (not fixed here).** So the promoted
  "never average −1.0" invariant *describes a latent risk*, it does not contradict a clean code
  path — safe to promote as a writing rule.

## 0c — consumer polarity (throw-site map for Commit 0)

Full per-site table: `consumer_polarity_0c.md`. Summary: **~30 reader sites THROW on `unknown`**
(every unsafe path is a hard arithmetic/`purity_zone` throw — there is **no silent-misorder
category**, because no consumer feeds raw purity to `@<`/`sort`/`min`/`max`). Critically,
**`unknown \= -1.0` succeeds**, so every existing `\= -1.0` guard is now *insufficient* — guards
must become `number(P)`, not merely extend the `\=`. Two **state-poisoning** asserts
(`drl_fpn:107` → `fpn_intrinsic`, `giant_component:353` → `gc_node_purity`) are safe at the store
but make later aggregate arithmetic throw — they need a guard at the store. Genuinely safe
(no change): the abductive_triggers evidence-term sites (87/467/531/824/957),
`context_profile_mining:191`, `genuine_findings_query:101`, `json_report:1338`
(`write_json_number` maps `unknown→null`). **Producer-side (point D):** many producer call sites
read a producer *failure* as clean/absent silently (FNL clause fails → not flagged; purity tests
→ `pass`; drift/severity fall through to `watch`) — these are the C-COUPLING/C-CC/C-EX/C-FLOOR
per-site handling targets, addressed in the producer commits, not Commit 0.

## Positive control (per-process, every leg)

A bare constraint (`oq60_control_bare`: 3 authored classifications so the epistemic gate passes,
no grid/coupling/extraction/coordination data) fired **all five branches** with
`purity_score = 1.0` in the same process that reported each leg's zeros — proving the probe
would have flagged the pathology it reports as latent for m1–m4.
