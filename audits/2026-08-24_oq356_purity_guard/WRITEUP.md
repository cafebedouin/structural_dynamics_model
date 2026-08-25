# OQ-356 — `count_by_action_band/8` purity guard: the fix, its coverage line, and its witnesses

**Executed:** 2026-08-24
**OQ:** OQ-356 (with a correction landing on OQ-352's headline; `in_float_range/3` routed to its own follow-up)
**Fired:** live
**Pre-fix tree:** git HEAD `b2782c7f9`
**Manifest cite:** no `pipeline_output.json` is consumed by this work. `giant_comp`
reads the corpus and the engine directly; every leg figure below is dated and
names the leg and the code state it was measured at, per the rule that "HEAD
yields N" is ambiguous between engine-regime and corpus.

---

## Verdict, at its scoped altitude

`giant_component_analysis.pl:1278` read `drl_purity_network:effective_purity/4`
and compared the result with `EP >= 0.0` and **no `number/1`**. Under OQ-60 path
0a `unknown` is a RETURN VALUE, not an exception, so the `catch(..., _, fail)`
one line above intercepted nothing and the comparison threw
`type_error(evaluable, unknown/0)`, killing the whole Phase-3 contamination
block. **Measured: 17 of 20 corpora died at the same point; the 3 that passed are
non-witnesses** (one by unreachability, two by degeneracy).

The guard now lands ahead of the comparison, as a **single-pass partition** that
also returns the two counts the read site needs, and the report prints a
coverage line so a shrinking scorable domain can no longer read as falling
contamination.

**The fix is witnessed at four independent altitudes.** One pre-registered
criterion (monotonicity) needed correcting in BOTH directions: it is **vacuous**
on the three haiku legs, where the ten cap rows are identical and no value of the
guard could make it fail, and it is a **live, passing discriminator** on
`original_v6`, where the table genuinely moves. Reported at that altitude rather
than as a flat pass — see *Criterion 3*.

---

## What changed

| file | change |
|---|---|
| `prolog/giant_component_analysis.pl` | `count_by_action_band/8` → **`/10`**, restructured as a single-pass partition (`partition_scorable_purity/4`) applying `number(EP), EP >= 0.0`; `NKept`/`NExcluded` accumulated independently; coverage line emitted above the collapse table in `report_contamination_collapse_analysis/2`; the one caller widened |
| `prolog/tests/test_purity_absence.pl` | 6 new tests next to `gc_ingest_collapses_unknown_to_sentinel`, plus a three-way EX-subscore dispatch harness (`unknown` / throws / fails) |
| `audits/2026-08-23_oq352_report_driver/purity_guard_sweep_v3.py` | new: the repaired, reachability-keyed, interprocedural find-criterion (v2 kept for the record) |

### Why this is ONE behavioural change, not a one-token insertion

The coverage line's excluded count must be produced by **the same conjunction
the guard rejects** — one pass over `Members` partitioning into kept and
excluded. Implementing that IS the guard: there is no version of the coverage
line that does not test `number(EP)`. So `:1276-1281` became a restructured
partition loop, not `number(EP),` dropped into an existing conjunction. OQ-356's
own "the fix (one word)" was true of the guard considered alone and stopped
being true when the coverage line was ruled in.

### Why `/10` and not `/9`

`NKept` is **accumulated**, never derived as `|Members| - NExcluded`. Derived, the
conservation identity `NKept + NExcluded == |Members|` would be true by
construction — a check that cannot fail, which is this OQ's own defect class.
Accumulated, it genuinely tests that the partition is total: it fires if a member
falls through both branches or is counted twice. A later reader who collapses
`/10` to `/9` silently deletes the test while leaving the assertion in place.

### Two identities, not one

```
(1)  NS + NB + NW + ND  ==  NKept        band coverage of the filtered domain
(2)  NKept + NExcluded  ==  |Members|    the guard's partition
```
Same total strength as the single conflated form, but a band-coverage bug (a
numeric value at or above the `1.01` upper bound lands in no band) now fails (1)
instead of reading as a guard bug in (2).

### Behaviour preservation

The pre-fix code collected with `findall/3` (ALL solutions per member); the
partition commits to the first. Those differ iff `effective_purity/4` is nondet.
**Measured semidet on 2,241 members across three legs**, with a live positive
control that the probe can count multiplicity (`determinism_witness.txt`).

---

## The acceptance criteria, one by one

### Criterion 1 — the hand-computed row: **PASS**

Literal, drift-proof constants, two-sided at both dangerous edges. `count_in_zone/4`
→ `in_float_range/3` is half-open `[Lo, Hi)`, so a member at exactly the sound
floor must land in Sound **and not** in Borderline; at exactly the degraded floor,
in Warning **and not** in Degraded. Floors are read from `config:param/2` at test
time, never hardcoded. The full row over `[0.70, 1.00, 0.55, 0.30, 0.05, 0.00]`
is asserted as `NS=2, NB=1, NW=1, ND=2` with the four bands total over the list.

> **Deviation D1, stated rather than absorbed.** The plan sited the hand-computed
> row inside a `count_by_action_band` call over members with *injected exact
> purities*. Pinning exact effective purities through the real chain requires
> abolishing and re-copying `purity_score/2`'s (18-line) or `effective_purity/4`'s
> (25-line, including a CONVERGENCE-CRITICAL comment) body into the test file —
> a canonical fork (Build Discipline Pattern 2) with a larger blast radius than
> the thing it would witness. The row is therefore computed over a literal EP
> list through the same `count_in_zone/4` the predicate uses, and the
> guard/partition half is carried by the tests on the real predicate. Nothing the
> row was asked to establish is dropped; only the injection point differs.

### Criterion 2 — conservation, both identities, non-zero subtrahend: **PASS, twice**

Synthetically, where the members are hand-enumerable, and on a real leg where the
population is one nobody constructed. Neither subsumes the other.

- **Unit:** 7 members = 4 real scorable + 3 synthetic (one per exclusion cause).
  `NKept=4`, `NExcluded=3`, both identities hold.
- **Leg (`testsets_haiku`, 960 files, post-fix):** `**Purity coverage**: 579 of 589
  giant-component members have a numeric effective purity; 10 excluded`.
  Identity (1) holds on all ten rows; identity (2) holds; subtrahend 10 ≠ 0.
- **Leg (`archives/datasets/original_v6`, n=3380, post-fix)** — the size arm
  OQ-353 needs: `NKept=2989`, `|Members|=3014`, `NExcluded=25`. Both identities
  hold on all ten rows; subtrahend 25 ≠ 0. Completed rc=0 in **1247 s**, where at
  pre-fix HEAD it threw at 221 s.

**Independently cross-checked.** `exclusion_cause_census.pl` reproduces
`run_phase3`'s setup exactly and gets GC=589, kept=579, excluded=10 — matching
the emitted coverage line.

### The equivalence requirement — and the naming ruling vindicated by data

The guard drops three silently different populations, and a fourth was already
being dropped: (a) `effective_purity` succeeds with a NON-NUMBER, (b) it THROWS,
(c) it FAILS, (d) it returns a NUMBER below 0.0 (the `-1.0` gate-fail sentinel,
excluded pre-fix by the existing filter). Each is asserted separately in the unit
control, so a regression names which one broke.

On `testsets_haiku`'s giant component the 10 excluded split **4 (a) + 6 (d)**,
0 (b), 0 (c). The 4 are exactly the "4 unknown-purity GC members" OQ-356 names —
so the OQ's figure and `NExcluded=10` are two different populations, not a
discrepancy. **An excluded count written as "count the unknowns" would have missed
6 members, and the conservation identity would then have broken as a FALSE ALARM
attributed to the guard.** The ruling to name the argument `NExcluded` rather than
`NUnknown` is therefore settled by measurement, not by argument.

### Criterion 3 — monotonicity: **PASSES NON-VACUOUSLY ON `original_v6`; VACUOUS on the three haiku legs**

This criterion did not survive contact with the data in the form the plan gave
it, and the correction runs in both directions. Both halves are stated, because
the first half alone would have been an overstatement — and I made it before the
v6 run landed.

**The pre-registered stop condition was *monotonicity FAILS at HEAD*. It did not
fail**, so no halt on that ground. But on the two legs that already completed the
sweep at HEAD it holds because **the sequence is CONSTANT** — all ten cap rows
identical:

```
testsets_haiku2 PRE-FIX   63 / 136 / 455 / 7    x10 rows, identical
testsets_haiku3 PRE-FIX   61 / 135 / 425 / 6    x10 rows, identical
testsets_haiku  POST-FIX  51 / 124 / 394 / 10   x10 rows, identical
```

A constant sequence is trivially non-decreasing and non-increasing, so on those
legs **no value of the guard could make criterion 3 fail.** The plan's inference
for this branch — *"the invariant is a property of the existing code, so a
post-fix violation is attributable to the guard; criterion 3 becomes a real
discriminator"* — does not follow **there**, and downgrading that into a caveat
would be exactly the absorption the plan warned about.

**But on `original_v6` the table MOVES, and criterion 3 is a genuine passing
discriminator:**

```
Cap    Sound  Borderline  Warning  Degraded
0.10     225         118     2215       431
0.20     215          96     2222       456
0.30     214          92     2224       459
0.40..1.00  (identical to the 0.30 row)
```

NS is strictly non-increasing (225 → 215 → 214), ND strictly non-decreasing
(431 → 456 → 459), and 11 members genuinely change band. On the largest leg — the
one OQ-353's size arm needs — the criterion is doing real work and it PASSES.

**Why the haiku legs are flat — measured, not guessed.** `Contam is min(Cap,
RawContam)` binds only when `RawContam > Cap`, and there is no memoization in
`drl_purity_network` (so this is not a stale cache). Two-sided probe
(`cap_inertness_probe.pl`):

| leg | max RawContam observed | members whose EP moves, cap 0.10 → 1.00 | max abs delta |
|---|---|---|---|
| `testsets_haiku2` | 0.134250 | **1 of 995** | 0.017125 |
| `testsets_haiku3` | 0.136200 | **3 of 993** | 0.032580 |

The cap CAN bind — both maxima exceed the sweep's lowest cap — so the flatness is
not an empty network. It is that the cap binds on a handful of edges with an
effect far below one band width.

**The finding that survives, stated at the altitude the evidence supports.** Not
"the collapse sweep carries no information" — v6 falsifies that. The accurate
claim is about the sweep's **RANGE**: on every leg measured, the table saturates
at or below the default cap of 0.30, so **the upper two-thirds of the sweep
(caps 0.40–1.00) reproduce the 0.30 row exactly and carry no information at
all** — 8 of 10 rows on v6, 10 of 10 on the haiku legs. That is a property of the
Phase-3 instrument, it outlives OQ-356, and it is routed to its own OQ rather
than absorbed here.

> **Recorded because it is the honest version of a mistake I made mid-run:** on
> the strength of the three flat haiku tables I wrote in the investigations
> ledger that the sweep "may carry no information on any leg — ten copies of one
> row". The v6 run, which came later, falsified that. The ledger line was
> corrected in place rather than left standing, and the claim narrowed to the
> range statement above.

### Criterion 4 — the free negative control, *fires at N, declines at N−1*: **PASS**

Preserved deliberately: nothing was cleaned up before the detector existed.

| altitude | at pre-fix HEAD `b2782c7f9` | at the fixed tree |
|---|---|---|
| **leg** (`testsets_haiku`, identical invocation) | `rc=2 THROW` in 7.5 s, last section `### Contamination Collapse Analysis`, `>=/2: Arithmetic: 'unknown/0' is not a function` | `rc=0 OK` in 21.5 s, last section `### Key Finding` |
| **leg** (`archives/datasets/original_v6`, identical invocation) | `rc=2 THROW` in 221 s, same error, same section | `rc=0 OK` in 1247 s, last section `### Key Finding` |
| **unit** (`run_tests(purity_absence)`) | exit 1, `7 tests passed`, 1 failed with `received error: >=/2: Arithmetic: 'unknown/0' is not a function` | exit 0, **all 13 passed** |

> **The arity delta, acknowledged rather than papered over.** The unit pair is
> *fires at N, declines at N′*, NOT *N−1*: V4a's throw-witness called
> `count_by_action_band/8` and V4c widened it to `/10`, so the two halves cannot
> be signature-identical. Binding conditions, all three met: (1) the pasted RED
> shows `Arithmetic: 'unknown/0' is not a function`, **not** an
> `existence_error` — checked as the error TERM, not the colour, because an
> `existence_error` would mean the test was written against a predicate that did
> not exist yet and the reading would be worthless; (2) the two halves differ by
> the arity change and **nothing else** in that test's body — the other five
> tests are additions, not edits to it; (3) **the load-bearing discrimination
> record is therefore criterion 4 at the LEG level**, where the invocation is
> byte-identical across both commits (`run_giant_comp_leg.py`, unchanged between
> halves). The unit pair is supporting evidence.

Also recorded: the RED reads as a **scoped per-test error** — `run_tests/1` did
NOT abort, and the suite's other 7 tests ran and passed. That was a
pre-registered stop-and-ask if it had gone the other way; it did not.

**The full 20-leg census, same script, same invocation, both commits:**

| | pre-fix HEAD `b2782c7f9` | fixed tree |
|---|---|---|
| `rc=0 OK` | **3** (`testsets`, `haiku2`, `haiku3` — all three non-witnesses) | **20** |
| `rc=2 THROW` | **17**, every one at `### Contamination Collapse Analysis` with `>=/2: Arithmetic: 'unknown/0' is not a function` | **0** |

The pre-fix half also re-verified the partition rather than assuming it: **no leg changed side**,
and the one `n` that moved materially (`testsets_nemotron_think`, 732 → 1003) is a leg that
finished filling, not a leg that changed behaviour.

### V6b — a pre-registered prediction that actually resolved: **HELD**

OQ-356 predicted, against the partial n=732 draw: *"When it completes it is a fresh draw and
**will throw unless its unknown-purity count reaches zero**."* `testsets_nemotron_think`
completed at n=1003 on 2026-08-23. Re-run at n=1003 **on the same pre-fix tree**, it still
throws — same error, same section (`census_prefix_HEAD.txt`). Post-fix it completes in 45.3 s.

**The committed n=732 census row was NOT overwritten.** A superseded-at note was appended to
`audits/2026-08-23_oq352_report_driver/giant_comp_leg_census.txt` instead. Overwriting the row
would destroy the record of the prediction having been made against a *specific draw*, which is
the only thing that makes its resolution meaningful.

### Criterion 5 — the non-witnesses, named, and criterion-relative

`testsets`, `haiku2`, `haiku3` are **three passes and ZERO witnesses** for the
guard. Each has THREE roles, and flattening them to "non-witness" would license
deleting two live controls:

| leg | why it passed pre-fix | role 1: criterion 2 | role 2: criterion 3 | role 3: invariance (V6c) |
|---|---|---|---|---|
| `testsets` | **unreachability** — GC 4.7%, under `run_phase3`'s `GCFrac > 0.10` gate at `:855`. It has an unknown-purity member and *would* throw if it got there | non-witness | n/a (never enters the block) | n/a |
| `testsets_haiku2` | **degeneracy** — 0 unknown-purity GC members, so the conservation subtrahend is 0 and identity (2) holds trivially | non-witness (degenerate) | **informative** — monotonicity needs no subtrahend, so its HEAD reading calibrates the invariant (and is what exposed the vacuity above) | **load-bearing** — the guard must change nothing, making its table an exact before/after oracle |
| `testsets_haiku3` | same | non-witness (degenerate) | **informative** | **load-bearing** |

**Witnessed positive control for `testsets`'s unreachability**, so the k=0 claim is
a fact about the run rather than about a search: the live artifact prints both
declines in sequence — *"No significant component found at threshold 0.500"* then
*"No giant component (>25% of nodes) found at any threshold from 0.10 to 0.50."*
(preserved as `S5c_testsets_giant_component_analysis_PREFIX.md`).

### V6c — the exact invariance oracle: **PASS on both legs**

Both degenerate legs have **zero** cause-(a) members, so the guard must exclude
nothing new. Machine-checked, scoped to the count rows:

```
haiku2: all 10 count rows IDENTICAL pre/post
haiku3: all 10 count rows IDENTICAL pre/post
```

> **The plan's stated expectation here was mis-derived, and the correction is
> itself an instance of this OQ's defect class.** The plan required `NExcluded`
> to be **exactly 0** on both legs, reasoning from "zero unknown-purity GC
> members". Measured: `NExcluded` is **1** on haiku2 and **2** on haiku3 — and
> every one of them is cause **(d)**, a numeric `-1.0` gate-fail sentinel that
> the PRE-EXISTING `EP >= 0.0` filter already dropped. Cause (a) is 0 on both, as
> the plan said. So the expectation conflated the defect-class population with
> the whole excluded set — **the same `NExcluded`-vs-`NUnknown` equivalence trap
> the plan itself warns about, reappearing inside the plan's own expected value.**
> The stop-and-ask trigger was *"the guard changes any COUNT on a leg that
> already passed"*; **no count moved**, so the trigger did not fire and the
> invariance argument is intact.
>
> The diff is scoped to the ten count rows on purpose: the post-fix artifact
> carries the coverage line this change deliberately adds, so a whole-table diff
> is GUARANTEED to differ and would false-alarm on a difference we introduced.

---

## The repaired find-criterion (the class half)

v2's criterion was **idiom-keyed, not reachability-keyed**, and failed in two
independent ways. The regex bug is the smaller one: a bare `>` alternative
matched the `->` of every if-then-else, so "does arithmetic within 5 lines"
actually meant "has an if-then-else nearby". The serious one is a **structural
blind spot** — v2 only ever inspected lines *containing* a purity call, so a
hazard one predicate downstream was invisible at ANY window width. Widening the
window, the improvement OQ-356 itself proposed, **would never have found it.**

The repaired criterion models whether the value REACHES arithmetic: bound-variable
tracking to the operation, guards (including clause-order fail-closed guards),
forward flow, and value-following ACROSS predicate boundaries and out of a
collected list into element-level arithmetic.

**Held-out acceptance test, pre-committed: PASSED.** `giant_component_analysis.pl:596`
(`in_float_range/3`) was found by hand and deliberately withheld from every
expected-findings list. The repaired sweep surfaced it unaided, with the chain:

```
giant_component_analysis.pl:596  [comparison on V]
    in_float_range(Lo, Hi, V) :- V >= Lo, V < Hi.
    via count_in_zone/4 arg1 from <frozen pre-fix fixture>:11
    via in_float_range/3 applied by include/3 at giant_component_analysis.pl:593
```

**The instrument's own control is a ratchet unless its fixture is frozen.** v2's
control asserted "fires on `:1278`" — but Commit 1 FIXES `:1278`, so a live-tree
scan afterwards passes that half **vacuously** while still printing
`=> sweep DISCRIMINATES`: this OQ's defect class, reproduced inside the instrument
built to detect it. The pre-fix predicate text was therefore snapshotted BEFORE
the edit (`fixtures/count_by_action_band_prefix.pl`, captured at `V3c` with its
line-offset map and md5) and the "fires" half points at the fixture. The
"declines" half stays on the live tree. This is criterion 4's rule — *availability
is not automatic; when a detector will follow, preserve the defective state
deliberately* — applied to the INSTRUMENT rather than to the corpus.

Adjudication of every candidate, with the three-verdict vocabulary and the
instrument's declared bounds: **`adjudication_table.md`**. Summary: 2 rows
emitted (both REAL, both explained), `fpn_report.pl:94` **LATENT — unreachable
today by caller discipline** (witnessed, not argued), 7 false positives each
verified first-hand this session.

---

## Corrections landed on OQ-356's own headline

1. **"Phase 3's contamination block has never run on any corpus, ever" is too
   strong.** The narrow form is fully sufficient for everything the OQ argues:
   **no persisted numbers, and the block has reached completion exactly twice —
   both inside this audit's own census, both on legs the OQ itself classifies as
   degenerate.** Trading an inferred positive control for a witnessed one (the two
   printed declines in the live `testsets` artifact) is a straight upgrade.
2. **There is a SECOND call site for four of the five reports.**
   `find_first_giant/4` (`:882-898`) calls `report_gc_composition`,
   `report_contamination_sources`, `report_multihop_contamination` and
   `report_sound_constraint_exposure` — **but not**
   `report_contamination_collapse_analysis` — behind `LFrac > 0.25` at a lower
   coupling threshold. So the collapse sweep's k=0 rests on ONE gate
   (`GCFrac > 0.10`, `:855`→`:862`); the other four sit behind two gates in
   series. Reading "the fragmentation gate" as singular would miss this.
3. **On the 17 throwing legs the four earlier reports DO execute** — every census
   throw's last printed section is `### Contamination Collapse Analysis`, i.e. the
   four prior sections printed — but nothing persists, because the stage dies
   before the artifact write. So this fix newly **persists** four already-executing
   reports and newly **produces** one.

---

## The first-ever Phase-3 numbers are UNVALIDATED

Landing this makes 17 legs emit contamination statistics nobody has ever seen.
**They are reported, not citable.** Establishing their floor is OQ-353/OQ-354's
job, not this OQ's. The criterion-3 finding above is the first thing anyone
citing them needs to know: on every leg measured the collapse table **saturates
at or below the default cap of 0.30**, so caps 0.40–1.00 reproduce the 0.30 row
exactly — 8 of 10 rows on v6, 10 of 10 on the haiku legs.

---

## Evidence map

| artifact | what it shows |
|---|---|
| `census_prefix_HEAD.txt` | **V3** negative control at pre-fix HEAD: the 17 THROW / 3 OK partition reproduces; no leg changed side |
| `census_fixed.txt` | **V6** the same census at the fixed tree — criterion 4 per leg |
| `V3b_haiku2_PREFIX.md`, `V3b_haiku3_PREFIX.md` | **V3b** pre-fix collapse tables on the two degenerate legs; unobtainable after the fix |
| `V3b_cap_inertness_prefix.txt`, `cap_inertness_probe.pl` | why criterion 3 is vacuous: the cap sweep is near-inert by data, two-sided |
| `../2026-08-23_oq352_report_driver/oq356_plunit_RED_prefix.txt` | **V4a/V4b** the control's negative half: exit 1, scoped per-test error, correct error TERM. **Sited in the OQ-352 dir by the operator's ruling** — see the cross-directory note below |
| `../2026-08-23_oq352_report_driver/oq356_plunit_GREEN_fixed.txt` | **V4c** the positive half: all 13 tests pass, exit 0. Same ruled siting |
| `V5_haiku_FIXED.md` | **V5** primary leg witness: coverage line + collapse table, criterion 2 with a non-zero subtrahend |
| `V5_original_v6_FIXED.md` | **V5** the n=3380 leg OQ-353's size arm needs |
| `V6c_haiku2_FIXED.md`, `V6c_haiku3_FIXED.md` | **V6c** post-fix tables for the exact invariance diff |
| `parse_collapse_table.py` | the read-site checker (criteria 2, 3, V6c), with a 7-case planted-fixture selftest: 1 pass, 5 declines, 1 unparseable |
| `run_giant_comp_leg.py` | the per-leg runner — the census invocation VERBATIM, with stdout persisted; unchanged between the pre-fix and post-fix halves, which is what makes criterion 4 signature-stable |
| `exclusion_cause_census.pl`, `exclusion_cause_census_haiku.txt`, `exclusion_cause_census_degenerate_legs.txt` | the (a)/(b)/(c)/(d) split that settles the `NExcluded` naming and explains V6c's non-zero counts |
| `determinism_probe.pl`, `determinism_witness.txt` | `effective_purity/4` semidet on 2,241 members, with a live multiplicity control — the behaviour-preservation premise for the single-pass partition |
| `3b-i_fpn_completion_witness.txt` | the `fpn_report.pl:94` LATENT verdict's witness (3328/3380 rows reached the arithmetic; 3380−3328 = 52) |
| `fixtures/count_by_action_band_prefix.pl` + `fixtures/PROVENANCE.txt` | **V3c** the frozen pre-fix predicate text, line-offset map and md5 — what keeps the sweep's positive control from going vacuous |
| `adjudication_table.md` | Step 3b: every candidate, three verdicts, in-file witnesses, and the instrument's DECLARED BOUNDS |
| `sweep_v3_output.txt` | the repaired sweep's run, its positive control, and the held-out acceptance test |
| `S5c_testsets_giant_component_analysis_PREFIX.md` | the witnessed positive control for `testsets`'s unreachability (both declines printed in sequence) |
| **the plunit output committed to `audits/2026-08-23_oq352_report_driver/`** | per the operator's ruling siting it there; see the note below |

> **Cross-directory note (deliberate exception, ruled).** The operator ruled by
> name that the suite be run once and its output committed to
> `audits/2026-08-23_oq352_report_driver/`, the OQ-352 evidence dir where the
> census and sweep already live. That splits OQ-356's evidence across two dated
> dirs, against the standing location mandate. It is honoured, and discharged
> mechanically rather than silently: this evidence map names the plunit output at
> its oq352 path, and a dated line was appended to that dir's `WRITEUP.md`
> pointing back here — so the pointer is two-way and neither dir reads as
> complete on its own.
