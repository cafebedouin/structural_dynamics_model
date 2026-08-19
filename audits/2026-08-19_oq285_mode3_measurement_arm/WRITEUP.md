# OQ-285 — RECOMMENDATION (B): no Mode-3 measurement arm exists, and the route the OQ proposes to disambiguate has **zero population** on every corpus in the repository

**Executed:** 2026-08-19
**OQ:** OQ-285 (gated by its own operator ruling: recommendation before code)
**Verdict:** **(B) — no arm exists; the honest outcome is a declared absence in `design_gaps.md`.**
Across 19,141 non-excluded agent seats on five live legs, `dr_type_for_stakeholder/3` **never fails**
(0) and `dr_type_with_d/4`'s catch-all clause **never fires** (0); every one of the 1,934 `unknown`
tokens at the H¹ read site comes from a *completed* reading whose six-type cascade matched nothing.
The collapse OQ-285 describes is real in the code and **empty in the corpus** — and the concept it
wants formalized (*positions systematically unable to see*) already has a live, computed carrier on
the **other** side of `is_real_type/1`: `extraction_blindness`, firing on 134/279 live stories.
Caveat the body carries: the positive control for the empty routes is an **authored decoy** (floor
grade) — no naturally-arising positive exists anywhere in the population, so this corpus can
establish *"no population"* but cannot establish *"the instrument discriminates on real cases."*
**Substrate:** live leg `prolog/testsets/` + `testsets_haiku|flash|kimi|sonnet` +
`archives/datasets/kernel_v1`. Pipeline manifest (`outputs/pipeline_output.json`):
`n_constraints` **279**, `code_commit` **6523046**, `code_dirty` **true**,
`pipeline_run_at` 2026-08-18T20:34:53Z. Probes ran at HEAD `6d0d5f19` (clean, unchanged
open→close), so the JSON is one commit-state behind the probes; every claim sourced from the
JSON is flagged as such and none of them turns on that gap.
**Fired:** latent — the OQ's premise is corrected (three routes, not two; the named one is
empty) and a live dead token is surfaced (`untyped`, 0/22,080 authored seats across five legs), but
no consumer-visible verdict changed, because the hazard is conditional on an input the corpus does
not produce. The operator's gate is what fired: building the reason token first would have shipped a
surface reporting a distinction with no population.
**Evidence map:**
- `audit_log.md` — sequence, HEAD open/close pair, prior-art grep result.
- `absence_route_census.pl` — the route classifier (a/b/c/real) over `stakeholder_agent_seats/2`.
- `route_census_all_legs.txt` — **raw** counts, 6 corpora. Witnesses §2.
- `route_probe_control.pl` / `control_two_sided.txt` — two-sided discrimination control with
  verified restore. Witnesses that §2's zeros are declines, not silence.
- `sibling_surface_sweep.pl`, `route_c_subdiag.pl`, `gap_status_sweep.pl`, `sibling_surfaces.txt`
  — §3. Carries its own bare-`[stack]` caveat for the two sheaf rows.
- `twin_seat_join.py` / `twin_seat_join.txt` — §4, the GAP-35 / GAP-31 id-and-seat intersection.

---

## 0. Refusals and specification notes (routed back, at the volume of a completion)

I did not need to refuse the brief: every step is executable and every predicate is where it is
claimed to be, including the pin `stakeholder_seats.pl:337-341` quoted in the CS sketch's v6.1
correction — verified line-exact. Three notes route back anyway, in descending order of consequence.

**(i) The brief's step-2 binary is under-specified against the code, and the under-specification is
load-bearing.** It asks for *"seats where `dr_type_for_stakeholder/3` FAILS versus seats deriving a
literal `unknown`."* The second half is not one thing. `unknown` reaches the vector by **three**
distinct routes with three different epistemic meanings (§1), and the brief's phrasing merges the
two that differ most: a seat whose *inputs were unavailable* (clause-2 catch-all) and a seat whose
*classification ran to completion and returned "no type applies."* Had I counted the brief's binary
as written, I would have reported `a=0, b+c=1,934` and concluded "the distinction is live, one side
is empty" — which is true but conceals that the **only** populated route is a produced reading, not
an absence at all. I counted three and report three.

**(ii) The OQ's own mechanism paragraph inherits the same merge.** It says the failed-derivation
token *"lands on the same token as a seat that derives literal `unknown` (`dr_type_with_d` fallback)"*
— naming route (b) as the second member. Route (b) is also empty. The populated third route is not
mentioned in the entry.

**(iii) A zero-count is the weakest evidence shape this project recognizes, and it is what carries
this recommendation.** I mitigated with breadth (six corpora) and a two-sided control, and I state
the control's grade honestly in §2.4 rather than letting the breadth imply more than it earns.

---

## 1. What is and is not distinguishable at the H¹ read site (read off the code, not the entry)

**Three routes to `unknown`, not two.** `stakeholder_type_vector/2` maps each seat through
`seat_type_token/3`; `stakeholder_obstruction/5` and `obstruction_from_vector/3` then filter with
`is_real_type(T) :- T \== unknown`. A seat arrives at `unknown` by exactly one of:

| Route | Mechanism | Site | Epistemic meaning |
|---|---|---|---|
| **(a)** | `dr_type_for_stakeholder/3` **fails** → `seat_type_token/3`'s if-then-else else-branch | `stakeholder_seats.pl:337-341` | the engine **could not derive** a type for this position |
| **(b)** | `dr_type_with_d/4` clause 1 body fails → clause 2 catch-all binds `unknown` | `drl_core.pl:516-525` | an **input was missing** (invalid context, no `base_extractiveness`, or the OQ-44 non-numeric-suppression fail-close at `drl_core.pl:364-365`) |
| **(c)** | `classify_from_metrics/6`'s terminal clause | `drl_core.pl:475-476` | inputs complete, cascade ran, **no type matched** — a negative determination |

Route (a) needs `derive_directionality_for_stakeholder/3` to fail, which needs an out-of-vocabulary
`exit_options` atom (with a valid role) or an out-of-vocabulary role **and** power.
`dr_type_with_d/4` itself can never fail — clause 2 is unconditional — so (a) is strictly narrower
than the entry implies.

**What survives the read site.** The *count*: `stakeholder_obstruction/5` carries `NSeats` and
`NReal` in-band, and both are serialized (`h1_stakeholder_n_seats` / `h1_stakeholder_n_real`). At the
story level that is already a three-way discrimination — 43 stories with `NSeats=0` (no position
existed), 2 with `NSeats>0, NReal=0` (positions existed, none typed), 4 with `NReal=1` (one typed),
230 computed. So *"nobody was in the room"* and *"people were in the room and none produced a
reading"* **are** distinguishable today, in shipped JSON.

**What does not survive.** Which of (a)/(b)/(c) produced any individual `unknown`. The entry
characterizes this loss correctly. My correction is to its *content*: the collapse is not
blind-vs-absent, it is **"could not derive" vs "input missing" vs "derived, and the answer was
none-of-the-six."** And neither of the first two has ever occurred.

**Neither the entry nor the corrected sketch is wrong about the code.** They are wrong about which
distinction the collapse actually erases, and silent about the collapse having no live instances.

---

## 2. The counts (step 2)

### 2.1 Route census — all six corpora

Raw: `route_census_all_legs.txt`. Domain = `stakeholder_agent_seats/2` (non-excluded, non-`stakeholder_non_agent`),
i.e. exactly the H¹ vector's own domain.

| corpus leg | stories | agent seats | **(a) derivation FAILS** | **(b) clause-2 catch-all** | **(c) cascade fell through** | real |
|---|---|---|---|---|---|---|
| `testsets/` (live) | 279 | 1,362 | **0** | **0** | 152 | 1,210 |
| `testsets_haiku/` | 960 | 3,091 | **0** | **0** | 323 | 2,768 |
| `testsets_flash/` | 960 | 3,738 | **0** | **0** | 406 | 3,332 |
| `testsets_kimi/` | 1,005 | 4,539 | **0** | **0** | 241 | 4,298 |
| `testsets_sonnet/` | 1,001 | 6,411 | **0** | **0** | 812 | 5,599 |
| **five live legs** | **4,205** | **19,141** | **0** | **0** | **1,934** | **17,207** |
| `archives/datasets/kernel_v1/` | 1,106 | **0** | — | — | — | — |

**`kernel_v1` has zero agent seats.** The stakeholder layer postdates it (OQ-83 Phase A), so the
breadth archive cannot exercise this frame at all. The stakeholder-H¹ population is the five live
legs and nothing else — worth knowing before anyone proposes an archive sweep as the missing
witness.

### 2.2 Route (c) is 100% a completed reading

`route_c_subdiag.pl` splits route (c) by whether the signature layer rewrote a real metric type to
`unknown` or the cascade itself fell through:

```
=== route c sub-diagnosis ===
  cascade_fell_through                   152
```

152/152 — no signature rewrite. So on the live leg, **`unknown` in the stakeholder H¹ vector means
exactly one thing, without exception:** ε, χ and suppression were all present, the six-type cascade
ran in authored order, and none of mountain/snare/scaffold/rope/tangled_rope/piton/naturalized
matched. That is a *reading*, tokenized as an absence and filtered as N/A.

### 2.3 The zero-seat stratum is largely not stories

```
=== zero-agent-seat stories ===
  total                                  43
  of which *_contradictions              26
```

26 of the 43 `NSeats=0` stories are the OQ-306 `*_contradictions.pl` axiom meta-files — not stories,
carrying no story blocks. Only 17 are genuine stories with no agent seat. Anyone reading
`h1_stakeholder_n_seats == 0` as "a story with no positions" is off by 26/43 on the live leg. Not
this OQ's business to fix; flagged because it sits inside the denominator this OQ would report over.

### 2.4 The control — two-sided, restore-verified, and honestly graded

`control_two_sided.txt`. Plants are enumerated by `census/1` itself (same path), not by a side call:

```
--- BASELINE (unmodified live leg) ---
  route_c_cascade_literal_unknown             152
  route_r_real_type                           1210
--- PLANT 1: route a (valid role, BOGUS exit atom) ---
  route_a_derivation_FAILS                    1
--- PLANT 2: route a (BOGUS role AND BOGUS power) ---
  route_a_derivation_FAILS                    2
--- PLANT 3: route b (well-formed seat, constraint with NO base_extractiveness) ---
  route_a_derivation_FAILS                    2
  route_b_clause2_catchall_unknown            1
--- RESTORE ---
  route_c_cascade_literal_unknown             152
  route_r_real_type                           1210
  RESTORE VERIFIED: baseline == post-restore
```

The instrument fires on both empty routes and declines on the unmodified corpus, and the decline is
witnessed against a **naturally-arising negative population of 19,141 seats** — the strong half.
The positive half is an **authored decoy**, the floor of the discrimination ladder, and it is the
floor *because no naturally-arising positive exists anywhere in this population*. Per the standing
rule, that has a name: **for the route-(a)/route-(b) question this corpus supplies no decline, so
the question "would the engine's failure modes be distinguishable on real cases?" is UNANSWERABLE
from this corpus.** What *is* answerable, and is answered: those routes have no instances.

---

## 3. Sibling surfaces — the repository already carries this distinction, four times, under other names

The brief's step 3 asks the right question ("under another name?"), and the answer is yes — including
one carrier the brief does not list, which is the one that matters.

| Surface | Kind of absence it splits | Live population |
|---|---|---|
| `report_generator:gap_status/3` (OQ-197) | `gap(...)` / `no_gap` / `undetermined(no_seats \| single_seat \| single_power_position)` | 141 / 86 / 52 — **all three populated** |
| `diagnostic.purity_n_*` (OQ-60) | `purity_n_gate_fail` vs `purity_n_no_data` vs `purity_n_scored`/`_n_total` | 43 / 13 / 223 of 279 — **both absence kinds populated** |
| `stakeholder_obstruction/5` `NSeats`/`NReal` | zero-seats vs seats-none-real vs one-real | 43 / 2 / 4 (49 nulls of 279) |
| `sheaf_undetermined_reason/2` | `insufficient_seats` vs `uncomputable_height` | 53 / **0** in shipped JSON |
| `seat_perceived_vs_real/4`'s `untyped` | derivation-failed, per seat | **0 of 22,080** authored seats, five legs |

**`gap_status/3` is the closest match and its header says so in as many words:** *"Closes the
Build-Discipline Pattern-6 collapse where measured-no-gap and didn't-look both emitted a
success-shaped empty."* It is the blind-vs-absent distinction, with named reasons, over the same seat
substrate — built already, at the constraint level.

**And `extraction_blindness` is the concept's actual live carrier.** `report_generator:label_gap/4`
fires it when *an extractive-typed seat sits at LOWER power than a functional-typed seat* — verbatim
in `omega_from_gap/5`: *"computes as extractive at lower-power seats but functional at higher-power
seats — extraction masked by perspective."* That is the CS sketch §2.5 claim — *positions
systematically unable to see drift they participate in producing* — as a **computed, populated,
serialized** surface:

```
=== gap_status/3 source=stakeholder ===        === gap_status/3 source=canonical ===
  gap(extraction_blindness)              134     gap(extraction_blindness)              148
  gap(general_type_mismatch)               7     gap(general_type_mismatch)               7
  no_gap                                  86     no_gap                                  71
  undetermined(no_seats)                  45     undetermined(no_seats)                  27
  undetermined(single_power_position)      4     undetermined(single_seat)               26
  undetermined(single_seat)                3
```

**134 of 279 live stories (48%).** This is the false-absence sub-rule (c) result the OQ's own gate
anticipated: *"a perfect control ladder on the WRONG predicate still yields a false absence."*
OQ-285 laddered `is_real_type/1` — the filter that discards seats — while the concept's carrier sits
on the surviving side, measuring blindness as **what a position asserts relative to its power**,
never as **whether it can assert**. Prior art check: `build_discipline.md:2955` cites
`extraction_blindness` only as an instance of the present-but-wrong-proxy rule, not as a Mode-3
formalization, so this reading of it is new and should be treated as this pass's claim, not as a
retrieved fact.

**Two negative results worth recording.** (i) `omega_variable/3` is **not** a carrier: 1,949
authored facts on the live leg, but it is `(ID, Type, Description)` at the *constraint* level with
Ω_E/Ω_C/Ω_P typing the question's undecidability — never seat-indexed, so it cannot say which
position can express what. (ii) The `untyped` token — introduced deliberately as *"an EXPLICIT
absence token, not a silent failure that a census/aggregate read site would conflate with 'no such
seat'"* — has **never fired**, on any leg, in 22,080 authored seats. It is correct, guarded, and
dead, by exactly the mechanism that makes route (a) empty.

**One caveat on my own sweep.** Under bare `[stack]` + corpus, `sheaf_status/2` read
manifest 155 / undetermined 124 and `sheaf_undetermined_reason/2` read
`insufficient_seats` 53 / `uncomputable_height` 71 — while the shipped `pipeline_output.json` reads
manifest 155 / genuine 65 / fragile 6 / undetermined 53 and `uncomputable_height` **0**.
`uncomputable_height` fires when *"H¹ = 0 but `arakelov_height/2` fails"*, and Arakelov height is
unfitted under bare `[stack]`. **The JSON is authoritative; my in-engine sheaf rows are a
load-state artifact and I do not build on them.** Flagged rather than dropped because it is the
documented bare-`[stack]` hazard reproducing on a new surface.

---

## 4. The arm question, answered directly (step 4)

**Is there any instrument here that could distinguish *a position that cannot express the question*
from *a position that produced no reading*? No.** Three independent closures, each checked rather
than assumed.

### 4.1 The twin legs — id intersection checked FIRST, and it dies at the seat level

`twin_seat_join.txt`:

```
four-way twin id intersection: 957   (GAP-35 declares 957)   ✓ confirmed
SEAT-level join over the 957 four-way matched ids:
  stories with stakeholder seats in ALL FOUR legs:   392
  ... with IDENTICAL seat-name sets across 4 legs:     0
  ... with a NON-EMPTY 4-way seat-name intersection:  60
```

GAP-35's story-level count is exact. But **Mode 3 is a predicate of a position, and positions do not
join.** Of the 957 matched ids, only 392 carry seats in all four legs; **zero** have the same seat
set; 60 share even one seat name — and GAP-31 rules that a recurring seat name across two readings
*is two seats, not one*, so those 60 are a naming coincidence, not an identity. The matched-seed
structure is real at the resolution of a story and absent at the resolution the question needs.
A paired design over the twins is dead on arrival — not for want of population, but for want of the
join. (Also relevant if anyone proposes the live leg as a fifth arm: it shares 5 ids with
haiku/flash and 49 with kimi/sonnet, and is the moving leg besides.)

### 4.2 OQ-277 cross-coding — retired, and retired for this exact failure

Closed 2026-08-12 by operator ruling: *"RETIRED, not deferred-pending-resources."* Its close states
the reason precisely — a 55%-self-agreeing reference means a confusion matrix *"cannot separate 'our
taxonomy disagrees with Wu's' from 'Wu's disagrees with itself'"* — and adds that **no sample size
repairs it; the reference is the problem.** A Mode-3 foreign-vocabulary probe inherits that
structure exactly: it needs an outside vocabulary stable enough that "this position cannot express
X" is a fact about the position and not about the vocabulary. OQ-277 is the repository's one attempt
at such a probe and it failed on the reference, not the run. It is also, on inspection, not a
candidate on its own terms: it cross-coded *incident taxonomies*, never positional expressibility.

### 4.3 The authored substrate cannot record the measurement, so it is not merely unrun

The complete seat-level vocabulary is `constraint_stakeholder/7` (role, agent_power, time_horizon,
exit_options, spatial_scope) plus `stakeholder_secondary_role/3` (232), `stakeholder_non_agent/2`
(28), `stakeholder_gain_flow/2` (131). **No field records what a position can or cannot articulate,
and none is derivable from the five that exist.** A Mode-3 datum has nowhere to land.

This is what makes the closure structural rather than budgetary, and it is where `design_discipline.md`
§5 bites: *a category whose blind cells are by construction undetectable from inside the system may
not be addable at all on an in-system control.* Worse than undetectable — **self-certifying**. Any
in-corpus field saying "this position cannot see X" would be authored by the same model that authors
that position's reading, so the generator would be certifying its own blindness. That is the OQ-70
class verbatim (*no signature may read a single authored perspective as a story-level claim*), and
the Ω_P trap from the synthesis-side rules (an orientation gloss typed Ω_P lets the actor
self-certify by fiat).

---

## 5. Recommendation: **(B)** — declare the absence

**(A) is false** (§4). **(C) — "the arm could be built" — is not honestly available**, and this is
the part of the recommendation I would most want an outside reader to attack. A generation spend
*could* mint a seat-level expressive-capacity field and a matched-seed leg keyed on seat identity;
that is a real, costed, non-re-read spend. But it would not produce a *measurement*, because the new
field's value would come from the same authoring pass as the reading it is supposed to be
independent of (§4.3). The arm needs an **independent vocabulary**, and this project has already
declined the two routes to one: GAP-30 (no externally-grounded ε — the "real corpus, external
measure" pipeline is declined by ruling) and OQ-277 (the one foreign-vocabulary instrument, retired
because the foreign vocabulary was unstable). Recommending (C) would name a spend that buys a
surface, not an arm — which is the move the operator's gate exists to catch.

**What the `design_gaps.md` entry should assert** (proposed; not written — the operator lands the
disposition):

> **GAP-40 — No expressive-capacity gauge: the engine can measure what a position *says*, never
> whether a position *could say it*.**
>
> 1. **The absent capability.** A per-seat predicate distinguishing *this position cannot express
>    the question* from *this position produced no reading*. `is_real_type/1` collapses both to
>    `unknown` at the H¹ read site; `stakeholder_obstruction/5` preserves the *count* of non-real
>    seats (`NSeats`/`NReal`) and never the *kind*.
> 2. **Why it is declared, not fixed.** The collapse is real in the code and **empty in the
>    corpus**: 0 failing derivations and 0 catch-all firings across 19,141 agent seats on five legs
>    (2026-08-19). All 1,934 `unknown` tokens are completed readings whose cascade matched nothing.
>    A reason token beside `NSeats`/`NReal` would report a distinction with no population — Pattern 1
>    with a philosophical justification.
> 3. **Why the gap is structural, not budgetary.** The seat schema has no slot for expressive
>    capacity, and any in-corpus slot would be authored by the model that authors the seat's
>    reading — self-certifying blindness (OQ-70 class). The two routes to an independent vocabulary
>    are already declined: GAP-30 and the OQ-277 retirement. Seat-level pairing is additionally
>    foreclosed by GAP-31 + GAP-35: of 957 four-way matched twin ids, **0** have identical seat sets.
> 4. **What the engine *does* carry, so this is not read as a hole where the concept lives.**
>    `extraction_blindness` (`report_generator:label_gap/4`) computes *"extractive at lower-power
>    seats, functional at higher-power seats — extraction masked by perspective"* on 134/279 live
>    stories, and `gap_status/3` splits *cannot-pose* from *posed-and-no* with named reasons. The CS
>    sketch §2.5 claim has a live carrier; what it lacks is a formalization of blindness as
>    **inexpressibility**, as distinct from blindness as **power-correlated divergence in what is
>    asserted**.
> 5. **Revival condition.** A single naturally-arising route-(a) or route-(b) seat, or an authored
>    seat-level capacity field whose value is sourced independently of the seat's own reading.
>
> Related: GAP-29, GAP-30, GAP-31, GAP-35; OQ-197, OQ-203, OQ-277, OQ-285.

**One consequence for OQ-285 itself, offered not ruled.** The Ω_C question — *should the gauge axis
carry an expressive-capacity predicate?* — survives this recommendation intact, but its evidence
base changes: it can no longer cite the H¹ collapse as motivation, because that collapse has no
instances. Its live motivation, if it has one, is the residue between `extraction_blindness` and
inexpressibility, which is a question about §2.5's prose, not about `is_real_type/1`.

**A standing probe, since a repair that encodes the tested claim into the instrument is the failure
mode here.** `absence_route_census.pl` is the falsifier for point 2 and should be re-fired on any
new leg, any change to `derive_directionality_for_stakeholder/3`, and any widening of the
`exit_modulation/2` or role vocabulary. A nonzero (a) or (b) count reopens this gap. Failure
semantics pre-committed: a nonzero count is a **revival**, not a caveat.

---

## 6. What would change this recommendation

Four things would, and I can name what each costs — which is the test of whether the recommendation
is falsifiable rather than merely confident.

1. **One naturally-arising route-(a) or route-(b) seat, anywhere.** Cost: re-run
   `absence_route_census.pl` (~2 min/leg). This flips the OQ from "no population" to "live
   collapse" and makes the reason token a real repair. **This is the cheapest falsifier and it is
   already written.**
2. **A demonstration that `extraction_blindness` and Mode 3 come apart on a real case** — a position
   that classifies a constraint the same way its high-power peers do while being structurally unable
   to articulate the drift. Cost: a hand-read of, say, 20 stories from the 86 `no_gap` bucket
   (positions compared, no divergence found), asking whether any carries in-file prose asserting a
   blindness the type divergence did not catch. This would show the concept has a residue the live
   surface misses, moving the recommendation toward (C) with a *named* target.
3. **An independent source for a seat-level capacity judgment** — a human coder, or a second model
   given only the position's authored attributes and never the story's readings. That breaks the
   self-certification argument in §4.3, which is the load-bearing beam under my rejection of (C).
   Cost: a real spend, and a preregistration with an effect-size floor against the coder's own
   self-agreement — the control OQ-277 did not have and died for lacking.
4. **A generation regime producing seats with out-of-vocabulary `exit_options` or role/power atoms.**
   Cost: none — it would arrive on its own. Note this is a *degradation* falsifier: it would make the
   collapse live by making the corpus worse, which is an argument for the reason token and against
   the generator, in that order.

**And the warning, stated as a warning.** Items 1 and 4 would change the *count*, not the
*argument* — the argument in §4.3 (no authored slot; any in-corpus slot self-certifies) is
insensitive to all four items except 3. So the recommendation has one real hinge, and it is item 3.
If an outside reader wants to attack this document, §4.3 is where to push, and the fact that
three of my four falsifiers cannot reach it is a weakness of the recommendation, not a strength of it.
