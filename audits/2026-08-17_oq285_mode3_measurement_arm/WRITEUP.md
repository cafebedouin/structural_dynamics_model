# OQ-285 — recommendation (D): no Mode-3 measurement arm exists, AND the axis the OQ names is empty by design, so the honest gap is not "we lack a reason token"

**Executed:** 2026-08-17
**OQ:** OQ-285 (step 1 of its code gate — a *recommendation*, not an implementation)
**Verdict:** On `prolog/testsets/`, the FAILS-vs-`unknown` axis OQ-285 proposes to instrument is
**empirically empty on one side (0 of 1333 agent seats FAIL) and registered-by-design on the
other**, while the 152 live `unknown` seats all sit in a single group the OQ's binary does not
name; no instrument in this repository separates *expressive incapacity* from *coordinate
position* — the maximal population for such a design is **8 seats** — so the recommendation is
**(D) the question as posed is mis-addressed**, with a `design_gaps.md` entry (next free
**GAP-36**) asserting the instrument absence rather than the token absence. Scope: measured on the
default leg for the group split and the coordinate test; the group (i) result and the twin-leg
population are static across all five live legs.
**Substrate:** `prolog/testsets/` (273 files, 230 stakeholder-bearing) for every engine
measurement; static parse of `testsets_haiku|flash|kimi|sonnet` for §4.
`outputs/pipeline_output.json` manifest: `n_constraints` **273**, `code_commit` **c06bcb2**
(`c06bcb26ad6fc2159294c936ef5f90b65953ed68`), `code_dirty` **true**, `pipeline_run_at`
**2026-08-15T00:03:33Z**, `schema_version` 2. No pipeline run was performed by this audit — every
probe here is read-only and writes only into this directory.
**Fired:** live — three flips, none of them predicted by the design being checked. (1) The
authorising plan's *"the signature layer never converts an `unknown` modal type to a real type"*
was **falsified by measurement: 29 live seats do exactly that** (§2), and the plan's supporting
citation was to a clause unreachable from the live call path. (2) A parser positive control caught
a **false absence in this audit's own scan** — a regex missing the `narrative_ontology:` qualifier
reported 0 stakeholder-bearing files in all four twin legs; the true counts are 494/748/987/1000
(§4). (3) `descent_status/2` was verified emitting **`descends(unknown)` on 12 live constraints**,
a consumer-visible global-section claim naming the absence token as the type (incidental item 1).
**Evidence map:**
| artifact | what it is | claim it witnesses |
|---|---|---|
| `RECON.md` | Phase-0 route-back on OQ-285's brief; 6 items | the brief's binary, its retired instrument, its missing option |
| `PREREGISTRATION.md` | verbatim 1B prediction + freeze witness (md5 + mtime ordering) | §3's non-uniform result was predicted, not read into |
| `seat_dump.pl` | read-only per-seat probe (17 columns incl. pre-signature `metric_type`) | producer of `seats_testsets.tsv` |
| `seats_testsets.tsv` | 1545 seat rows (1333 agent seats) on `testsets/` | §§1–3 |
| `phase0_group_split.txt` | the (i)/(ii)/(iii)/(iv) counts + group-(iv) reachability | §1 |
| `group_i_cross_leg.txt` | static atom-vocabulary scan, five legs + kernel_v1, with its control | §1's "group (i) is empty everywhere" |
| `item2_immutability_hole_cross_leg.txt` | `effective_immutability/3` table-hole census, five legs, with a transcription control | §3's attribution split; incidental item 2 |
| `phase1a_signature_layer.txt` | per-seat rescue/demote transitions + the 2×2 | §2 |
| `phase1b_within_constraint.txt` | within-constraint distribution + coordinate-uniformity test | §3 |
| `phase1b_alwaysunknown_support.txt` | support counts for the 19 always-unknown fine coordinates | §3, the small-n rebuttal |
| `control_demotion.pl` | ablation-lever discrimination control for §2's zero | §2's `DEMOTE 0` is informative |
| `witness_demotion_control.txt` | its baseline / planted / restored output | §2 |
| `phase1c_twin_intersection.txt` | twin-leg id, seat-name and coordinate intersections + parser control | §4 |
| `witness_totality_suite.txt` | OQ-137 reading-totality suite, corpus-loaded | `RECON.md` item 2 |
| `witness_item9_monitor_domain.txt` | residual-monitor domain vs seat-level exposure, with positive control | §2.1's guarded-zero claim; incidental item 9 |
| `arithmetic_selfcheck.py` / `.txt` | machine re-check of every total, percentage and dedup claim in this directory | that the tables in this document add up |
| `FINDINGS_INCIDENTAL.md` | 9 findings outside OQ-285's scope, as a minting proposal | items 1–2 go to the operator ahead of the disposition |

---

## 0. What this document is, said plainly

**The verdict was reached during planning; the runs here are confirmation of a written-down
prediction, not live inquiry.** Three candidate arms were proposed and all three are dead:

- the **signature join** — killed on two independent measurements (§2);
- the **within-constraint split** — killed by its own preregistered mechanism (§3);
- the **twin legs** — killed on population, at seat level rather than the constraint level the
  plan guessed at (§4).

The one thing planning did *not* have right is §2's mechanism, and correcting it made the
conclusion stronger rather than weaker. That correction is the only genuinely new result here.

## 1. The absence is one group, not the OQ's binary

`phase0_group_split.txt`, 1333 non-excluded agent seats over 230 constraints on `testsets/`:

```
  (i)   derivation FAILED                            0
  (ii)  classification never started                 0
  (iii) metrics exist, no band fit, not rescued    152
  (iv)  real type computed then declined             0
  ---- total unknown tokens                        152   of 1333 agent seats
```

Group (iv) is not merely 0 — it is **unreachable on this corpus**: only five signatures occur
anywhere (`constructed_high_extraction`, `coupling_invariant_rope`, `false_ci_rope`,
`false_natural_law`, `false_summit_mountain`), and **0 seats** match any of the seven
`residual_route/2` `(metric_type, signature)` pairs. The `residual_signature_override_enabled = 0`
lever is presently a lever over an empty set.

**Group (i) is empty across the entire live corpus, not just this leg.** A static parse of every
authored `constraint_stakeholder/7` fact in all five legs, each atom checked against the code
vocabularies (`exit_modulation` `constraint_indexing.pl:490-495`; `canonical_d_for_power` `:392-397`;
the five roles `stakeholder_seats.pl:83-87`) — `group_i_cross_leg.txt`:

```
leg                              seats   agent  badRole  badPow  badExit  FAILS
testsets                          1545    1355        0       0        0      0
testsets_haiku                    3774    3186        0       0        0      0
testsets_flash                    4144    3802        0       0        0      0
testsets_kimi                     5252    4549        0       0        0      0
testsets_sonnet                   7331    6522        0       0        0      0
archives/datasets/kernel_v1          0       0        0       0        0      0
  total agent seats scanned across the five live legs: 19414
```

**0 malformed atoms in 19,414 agent seats.** The FAILS cell OQ-285 asks to distinguish is not a
`testsets/` artefact — it is empty everywhere. (The authorising plan reported this total as 21,414;
the per-leg figures it lists sum to 19,414. The leg counts, not the total, are what the argument
uses.)

**Control, at its actual strength.** The scanner is exercised on four synthetic cases and is
two-sided in the shape that matters: it flags F2 (good role + bad exit), flags F3 (bad role + bad
power), and **declines** both on clean input and on bad-role-only — which per the asymmetry in
`derive_directionality_for_stakeholder/3` is correctly *not* a FAILS, since a malformed role routes
to the canonical-power fallback at `stakeholder_seats.pl:79`. These are **authored decoys, so a
floor**: they show authored malformation gets caught, not that every malformation shape does. No
natural negative is available — the population contains zero malformed atoms, which is the result
itself.

**Independent corroboration of the seat count.** This audit's engine probe reports 1545 total
`constraint_stakeholder/7` rows and 1333 agent seats on `testsets/`; planning's static parser
reported 1545 seats and 1355 non-excluded agent seats. The 22-seat gap is exactly the
`stakeholder_non_agent` registrations the static scan does not filter. Two independent
implementations, same substrate.

## 2. The signature is a co-occurring classification — and it is a ONE-WAY VALVE

### 2.1 What the layer actually does

Measured per seat, splitting `dr_type_with_d/4` at `integrate_signature_with_modal/3`
(`phase1a_signature_layer.txt`):

```
  metric unknown -> final unknown  (signature left it)        152
  metric unknown -> final REAL     (signature RESCUED it)      29
  metric REAL    -> final unknown  (signature DEMOTED it)       0
  metric REAL    -> final REAL                               1152

  RESCUE breakdown:
      signature=false_ci_rope              unknown -> piton   25
      signature=coupling_invariant_rope    unknown -> rope     4
```

**On this corpus at this config the signature layer only *reduces* abstention, never creates it.**
181 seats arrive at it with an `unknown` modal type; 29 leave with a real one; 0 real types are
demoted. So the abstention is determined **entirely** by `classify_from_metrics/6` falling through
its cascade at `drl_core.pl:459`.

**That one-way behaviour is a corpus-and-config fact, NOT an architectural invariant — stated
carefully because the shorter version is wrong.** Demotion is designed in, ruled, and deliberately
guarded:

- `residual_route/2` under the committed lever `residual_signature_override_enabled = 0`
  (`config.pl:508`) maps a **real** modal type to `unknown` — e.g.
  `resolve_modal_signature_conflict(mountain, coordination_scaffold, Result)`
  (`signature_detection.pl:955`). The *guard* is what creates the demotion path; flipping the lever
  to 1 restores the legacy manufacture instead.
- Those seven clauses are the OQ-138 "residual seven", **ruled ABSTAIN-guard by the operator
  2026-07-14** after the whole `resolve_modal_signature_conflict` table was converted clause by
  clause from overwrite to route (FSM, FCR-9, constructed-3, FNL convert; CI-rope keep). The
  direction is the accumulated residue of seven independent rulings plus the OQ-37 honest-`unknown`
  convention — not a stated valve contract.
- The zero is *monitored*: `residual_signature_firing/1` (`signature_detection.pl:1028-1040`) feeds
  `_prolog_residual_signature_gate()` (`python/run_pipeline.py:840-861`, called at `:1043`), which
  fails the pipeline loud and reopens **OQ-225** on any future fire.

Measured here: **0 seats match any of the seven residual `(metric_type, signature)` pairs**
(`phase0_group_split.txt`), and the constraint-level monitor also reads 0. So "one-way valve" is a
correct description of today's behaviour and an **incorrect** description of the design. Anyone
citing §2.1 downstream must carry the config condition with it.

This **corrects the plan that authorised the audit**, which asserted that every reachable clause
with an `unknown` first argument returns `unknown`. Two reachable paths do not:
`resolve_with_perspectival_check(C, _, false_ci_rope, piton)` via `piton_candidate/1`
(`signature_detection.pl:843-845`) and `resolve_modal_signature_conflict(_,
coupling_invariant_rope, rope)` (`:939`) through the fallthrough at `:862-863`. The plan's citation
— `resolve_modal_signature_conflict/3`'s `false_ci_rope` clause at `:946` — is **unreachable from
`integrate_signature_with_modal/3`**, which calls `resolve_with_perspectival_check/4` (`:812-814`)
whose own `false_ci_rope` clause cuts first at `:846`.

### 2.2 The zero is a measured zero, not an unlooked-at one

`DEMOTE 0` is the load-bearing half of §2.1, so it owes a discrimination control.
`false_summit_override_target` (`config.pl:506`) is a committed ablation lever whose default,
`mountain`, means "no overwrite"; setting it to `unknown` makes
`resolve_modal_signature_conflict(mountain, false_summit_mountain, Target)`
(`signature_detection.pl:967-974`) turn a real metric type into `unknown` — exactly the cell
reported empty. Same counting code, `probe_harness:with_overlay/3` (`witness_demotion_control.txt`):

```
=== BASELINE (committed config: false_summit_override_target=mountain) ===
    metric REAL    -> final unknown  (DEMOTE)  0
=== PLANTED (false_summit_override_target=unknown) ===
    metric REAL    -> final unknown  (DEMOTE)  4
=== RESTORED (must equal baseline) ===
    metric REAL    -> final unknown  (DEMOTE)  0
```

**Strength, labelled:** this is an **ablation-lever control on real corpus data through the
engine's own demoting clause** — stronger than an authored decoy, weaker than a naturally-arising
positive. Its negative side *is* naturally arising: the baseline decline is the committed config on
the real corpus. It licenses "*the counter sees a demotion when one occurs on this leg through this
clause*", not "*all demotion paths are covered*".

### 2.3 The signature does not even correlate with abstention

2×2 over **all 230** stakeholder-bearing constraints — not the 60 unknown-bearing ones, which would
condition on the outcome (`phase1a_signature_layer.txt`):

```
  signature                        has_unk  no_unk   tot    rate  unkSeats
  constructed_high_extraction           16     111   127   12.6%        40
  false_ci_rope                         41      40    81   50.6%       103
  coupling_invariant_rope                0      15    15    0.0%         0
  false_summit_mountain                  0       4     4    0.0%         0
  false_natural_law                      3       0     3  100.0%         9
  TOTAL                                 60     170   230               152
```

A reason token joined on signature would fire on **111 `constructed_high_extraction` constraints
with no abstention to explain**, and `false_ci_rope` is a coin flip. The `unknown`-signature row is
**absent**: 0 stakeholder-bearing constraints carry it, so the "genuinely reasonless
sub-population" is not a sub-population of anything.

### 2.3.1 This table is what kills the signature arm, and §2.1's falsification does not touch it

**Stated explicitly, because a reader who notices Check 1 collapsed may conclude the arm is back.**
It is not. The two are independent:

- §2.1's original claim ("no reachable clause maps `unknown` to a real type") was a **static read of
  clause coverage**. It was falsified. It was also **never load-bearing** — it offered a *mechanism*
  story on top of a result that had already been measured.
- This 2×2 was **measured**, not read: per-constraint outcomes joined to per-constraint signatures
  over all 230 stakeholder-bearing constraints. Nothing in it depends on which clauses are reachable,
  what the cuts do, or whether `resolve_with_perspectival_check/4` intercepts.

**The signature-join arm dies on non-correlation** — 12.6% vs 50.6% base rates, and the
`unknown`-signature row absent because 0 of those 26 constraints are stakeholder-bearing — **not on
the clause enumeration.** Had Check 1 been right, the arm would still be dead, and by this table.

**Cite §1's group-(iii) breakdown and this table as ONE observation, not two.** The 103 / 40 / 9
sub-path counts in §1 are byte-identical to the `unkSeats` column here because they *are*
`constraint_signature/2` under other names — signature is uniform within a constraint on every one
of the 230, so the two views are the same join. Treating them as independent evidence would
manufacture agreement.

### 2.4 What survives

The determining fact — *why `classify_from_metrics/6` fell through for this seat's (metrics, `D`)
pair* — is **not serialized anywhere**. A co-occurring classification ships; the determining one is
discarded. The mechanism, stated precisely: `classify_from_metrics(C, BaseEps, Chi, Supp, Context,
MetricType)` (`drl_core.pl:505`) takes **constraint-level** `BaseEps` and `Supp` together with
**seat-level** `Chi` and `Context`, where `Chi` comes from `extractiveness_for_agent_d/4` and `D`
from `derive_directionality_for_stakeholder/3`'s `(role, exit)`. **Abstention is a function of
constraint metrics × seat coordinate.** That is what lets seats on one constraint differ, and it is
what §3 tests.

## 3. The within-constraint arm — preregistered, and dead as predicted

`phase1b_within_constraint.txt`. Per-constraint distribution over the 230:

```
  none (0 unknown)        170 / 230    73.9%
  some-but-not-all         58 / 230    25.2%
  all seats unknown         2 / 230     0.9%
  affected constraints: 60; unknown seats 152; mean unknown/affected 2.53;
  corpus mean seats/constraint 5.80
```

So the phenomenon is overwhelmingly *within*-constraint: on 58 of the 60 affected constraints, some
seats type and some do not, on identical authored metrics. That is the shape a positional-blindness
story wants. **The preregistered mechanical test dissolves it.**

Excluding the 193 immutability-hole seats (incidental item 2 — a known coordinate artefact that
would otherwise drive the result), leaving 1140 seats / 108 unknown:

```
  coordinate = (power, exit): 30 distinct values | MIXED 15 | always-unknown 0 | always-typed 15
      of the 108 unknown seats, 108 (100.0%) sit on a MIXED coordinate
  coordinate = derived d alone: 19 values | MIXED 9 | always-unknown 0 | always-typed 10
      of the 108 unknown seats, 108 (100.0%) sit on a MIXED coordinate
```

**NON-UNIFORM at the strongest available margin: not one `(power, exit)` pair, and not one value of
`D`, is always-unknown.** Every unknown seat sits on a coordinate that types perfectly well under
other constraints. Per the freeze in `PREREGISTRATION.md`, this is **not** evidence for an arm — it
means the (metric-band × `D`) table has no row for that *combination*, structurally the same defect
as incidental item 2. Without the pre-commitment, "non-uniform" is the more interesting-sounding
branch and quietly becomes support for positional blindness; it is not.

**The one apparent counterexample is small-n, checked rather than waved off.** At the full authored
coordinate `(role, power, time, exit, scope)` there are 19 always-unknown cells — but
`phase1b_alwaysunknown_support.txt` shows **18 of them have support n = 1** (a single seat) and the
19th has n = 2 across 2 constraints. No coordinate is blind at any usable support.

**The instrument discriminates, and the record is in the run itself.** The uniformity test emits all
three outcome classes on real data without prompting — always-typed cells (15 at `(power, exit)`),
mixed cells (15), and always-unknown cells (19 at the finer cut). A test that could only report
"mixed" would be uninformative; this one declines.

**Sensitivity.** Without the immutability-hole exclusion the result is identical in kind
(31 coordinates, 18 mixed, **0 always-unknown**, 152/152 unknown seats on a mixed coordinate). The
exclusion changes nothing; it is reported because the plan required it, not because it mattered.

**Attribution split, as required.** Immutability-hole seats abstain at 44/193 = **22.8%** against
108/1140 = **9.5%** elsewhere — a real 2.4× enrichment. 44 of the 152 live unknowns (**28.9%**)
trace to that table hole. Any count of "Mode-3 seats" that omits this split measures a defect's
footprint and will be read as a phenomenon's shape.

## 4. The twin-leg arm dies on population — at seat level, which is not where the plan looked

The plan expected this to be settled by the *constraint*-level intersection (its guess: "≤494 and
plausibly under 200"). **That guess is wrong in the safe direction and for the wrong reason.**
`phase1c_twin_intersection.txt`:

```
  testsets_haiku     files   960   stakeholder-bearing   494  (51.5%)
  testsets_flash     files   960   stakeholder-bearing   748  (77.9%)
  testsets_kimi      files  1005   stakeholder-bearing   987  (98.2%)
  testsets_sonnet    files  1001   stakeholder-bearing  1000  (99.9%)

  four-way matched ids (GAP-35 population):             957
  four-way matched AND stakeholder-bearing in ALL four:  392
```

**392 is a perfectly usable constraint population.** The arm dies one level down:

```
  identical SEAT-NAME set in all four legs:            0   of 392
  identical (name,power,exit) set in all four legs:    0   of 392
  ≥1 seat NAME shared by all four legs:               60   of 392
  four-way name-matched SEATS:                        73
  ...of those, with an IDENTICAL (role,power,time,exit,scope): 8
```

**Eight seats.** GAP-35's matched-seed structure is a `constraint_id` join; the *seat roster* is
re-authored per leg. So a cross-leg comparison varies the author **and** the position at once — the
confound is structural, not statistical, and no sample size fixes it. The entire population on
which position could be held fixed while the authoring model varies is 8 seats across 4 legs.

**A parser control was owed here and it fired.** The first run of this scan reported **0**
stakeholder-bearing files in all four legs — a regex anchored on `constraint_stakeholder(` while
the authored facts are module-qualified `narrative_ontology:constraint_stakeholder(`. A
known-positive file (`ai_risk_prioritization__existential_risk_reading.pl`, 8 facts) caught it. The
control is one-sided: no file in `testsets_haiku/` carries the `/7` discontiguous declaration
*without* also carrying a fact, so the confusable-negative case does not exist in the population
and the control is reported at that altitude. Had this scan shipped uncorrected it would have
retired the twin-leg arm on a **false absence** while reaching the right verdict — a right answer
from a broken instrument.

## 5. Admissibility (`design_discipline.md` §5)

The Ω_C half of OQ-285 asks whether the gauge axis should carry *expressible-from-here* as a
predicate distinct from *types-to-X-from-here*. §5's rule: a category whose blind cells are by
construction undetectable from inside the system may not be addable at all on an in-system control.

**The cells here are not undetectable in principle — they are undetectable with the instruments
this repository has.** §3 shows every live abstention is coordinate-explicable and none is
coordinate-intrinsic; §4 shows the only foreign-vocabulary instrument (the twin legs) confounds
author with position at n = 8. So the category is not refused on principle; it is **unsupported on
evidence**. That is a weaker and more honest statement than "inadmissible", and it is the one the
data licenses.

## 6. What would change this recommendation — a real answer, not a disclaimer

**An instrument that varies expressive vocabulary while holding BOTH the constraint metrics AND the
seat coordinate `D` fixed.** Nothing in this corpus does that: the twin legs vary the author
(§4), the seats vary `D` (§3), and the metrics vary by constraint. That is a *generation* spend, not
a re-read, and it is a matched-seed decision that GAP-35 records as unreversible after the fact.

Only one *observation* would revive (A): a seat coordinate that types under **no** metrics
whatsoever. §3 measured that directly and found **zero at any usable support** — and even had one
turned up, it would be a missing rule-table row, not blindness. So realistically: nothing in the
current corpus revives (A).

### 6.1 THE MOST IMPORTANT PARAGRAPH IN THIS DOCUMENT — three instruments were wrong and the verdict did not move, and that is a WARNING, not a strength

OQ-285's brief says: *"Say what would change your recommendation. If nothing would, say that, and
treat it as a warning about the recommendation rather than a strength."* This audit produced the
**empirical** version of that test without meaning to. Three instruments were falsified in the
course of writing it — a static clause-read (§2.1), a regex that reported a **false absence** and
would have retired the twin-leg arm on a broken scan (§4), and an arithmetic total (§1) — and
recommendation (D) held through all three.

**The honest reading is the second one, and it should be said in the writeup rather than left for
the reader.** A verdict insensitive to its own instruments being wrong is a verdict that was not
resting on them. Concretely: **this document has a thin load-bearing core and a large corroborating
periphery.** Exactly two measurements carry (D):

1. **§2.3's 2×2** — the signature does not correlate with abstention (12.6% / 50.6%).
2. **§4's seat-level intersection** — 8 seats share a full authored coordinate across four legs.

Everything else — §1's group split, §2.1's mechanism, §2.2's control, §3's uniformity test, §5, §7 —
is corroboration. Corroboration is by definition not load-bearing, so its being wrong moves nothing.
**That is why the verdict survived, and it is not evidence the verdict is right.**

**The inference that actually follows.** Three of roughly six instruments in the periphery were
wrong on first construction — a ~50% first-pass error rate. The two core measurements were built by
the same process, in the same session, by the same instance, and **neither has been independently
re-derived.** The correction record does not validate them; it is the best available *estimate of
their error rate*, and that estimate is bad.

**So the concrete ask for the Claude-web check (step 2 of the OQ-285 gate) is narrow, not general.**
Do not audit this document. **Re-derive §2.3 and §4 from the substrate, independently.** If both
survive an outside re-derivation, (D) is earned. If either falls, (D) falls with it — and no amount
of the surviving periphery will hold it up.

## 7. The false-absence sub-rule (c), instantiated against OQ-285 itself

`seat_perceived_vs_real/4` (`stakeholder_seats.pl:160-169`, registered at
`reading_registry.pl:104`) **already carries the blind-vs-absent distinction per seat, by name** —
`untyped` (derivation failed) versus a derived type. That is exactly the group (i) vs (ii)/(iii)/(iv)
boundary OQ-285 asks for.

**And group (i) is empirically 0** (§1), so `seat_perceived_vs_real/4` emits `untyped` in 0 cases
and has no consumer outside tests. `consensus_provenance/2`'s `seats_untyped` verdict
(`stakeholder_seats.pl:250-254`) is the only place the distinction reaches an output token, and it
requires *every* seat on a constraint to fail — also 0.

**The repository's one per-seat blind-vs-absent surface separates the empty cell from everything
else, and does not touch the live split.** This is OQ-285's own cited false-absence sub-rule (c) —
*a perfect control ladder on the WRONG predicate still yields a false absence* — instantiated
against the OQ that cites it. **A well-built surface on the wrong predicate.**

## 8. Recommendation

**(D) — the question as posed is mis-addressed.** The FAILS/`unknown` axis is empty on one side and
registered-by-design on the other (`RECON.md` item 2, `reading_registry.pl:142`); the live variance
sits on an axis OQ-285 does not name (metric-band × `D` table coverage). Filing this as (B) would
report "no arm on the Mode-3 question" and conceal that the question was aimed at an empty cell.

**What the `design_gaps.md` entry should assert (next free GAP-36 — proposal only; not written).**
Not *"we lack a reason token beside `NSeats`/`NReal`"* — a token is buildable in an afternoon and
would report a distinction nothing here can measure, which is Pattern 1 with a philosophical
justification attached. Instead:

> **GAP-36 — no instrument separates expressive capacity from coordinate position.** Every live
> seat-level abstention is explicable by the seat's `(metrics, D)` coordinate, and the corpus
> contains no population that varies expressive vocabulary while holding both halves fixed (maximal
> such population: 8 four-way-matched seats with identical authored coordinates). Any
> *expressible-from-here* predicate is therefore unmeasurable as built, and remains so until a
> matched-seat generation spend exists. The absent thing is the **instrument**, not the token.

**No code was written. `ISSUES.md` and `KNOWN_STATE.md` are untouched** — the operator lands the
disposition after the Claude-web check (OQ-285 gate step 2).

## 9. Residue

- **Not run, deliberately:** Phase 2's six-corpus census. Its gate condition ("if Phase 1 resolves
  the arm question, the census shrinks or is dropped") is met — §§2–4 resolve it on one leg, and the
  static scan already settles group (i) across all five. What a census would still add is the
  (ii)/(iii)/(iv) split and per-leg signature distribution on the kimi/sonnet legs, where groups (ii)
  and (iv) are likeliest to be nonzero. **That is a real open, not a dropped one** — it is a
  descriptive question about other legs, and it does not bear on the arm verdict.
- **Owed if that census ever runs:** instrument `classify_from_metrics/6`'s exit point directly.
  Copying §1's sub-path labels across would re-emit `constraint_signature/2` under a new column name
  and manufacture the correlation §2.3 refutes.
- **`FINDINGS_INCIDENTAL.md`** holds 9 findings outside this OQ's scope, as a minting proposal.
  **Items 1 and 2 are live wrong output shipping today** and go to the operator ahead of the OQ-285
  disposition; they do not depend on anything OQ-285 resolves. Next free OQ label is **OQ-296**
  (OQ-295 was taken by the surfacing entry, commit `2caaf77b`) — the plan's "next free OQ-295" is
  stale.
- **Pin drift found and filed:** `commentary_census.pl:163` cites `stakeholder_seats.pl:182` for
  text now at `:252`.
