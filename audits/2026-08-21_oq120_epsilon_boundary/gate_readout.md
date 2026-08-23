# gate_readout — OQ-120 Phase 0, Step C

**Executed:** 2026-08-23 · **OPEN HEAD** `f88c8c3c` · **HEAD at readout** `f88c8c3c` (unmoved)
**Prereg md5:** `b181e1a2a9cd42b86d190be09f61d400`, frozen before any sweep code was written.
**Substrate:** 18 live legs (17,104 stories) + `archives/datasets/kernel_v1` (1,106), swept at
0.01-rail + 1e-4 bracket resolution with adaptive bisection. 117 grid points × 4 canonical seats.
**No leg moved during its sweep** (before/after `.pl` counts identical on all 19; no VOID), 0
took-effect guard failures, 0 restore failures. The only `NO_EPS` is `testsets`'s 27
`axiom_contradiction` non-story members.

---

## THE BRANCH: **G1**, and its subtype depends on a reading the prereg does not disambiguate

| prereg test | required | observed | verdict |
|---|---|---|---|
| **G2** | `N_rail ≥ 10` | **9191** | ✓ |
| | ≥2 distinct MODELS | **7** | ✓ |
| | ≥1 `snare_epsilon_floor` transition whose **FT pair is exactly {rope, snare}** | **0** (of 122,031 live transitions) | ✗ |
| | | | **G2 FAILS** |
| **G0** | `N_rail ≥ 10` | 9191 | ✓ |
| | **none** attribute to `snare_epsilon_floor` | 4621 qualifying do (*moved*) / 1 does (*decisive*) | ✗ |
| | | | **G0 FAILS on both readings** |
| **G1** | anything else | — | **FIRES** |
| ↳ G1a | `N_rail < 10` | 9191 | not this |
| ↳ G1b | a `snare_epsilon_floor` transition exists **but on a single leg** | *decisive:* 1 transition, 1 leg, 1 model · *moved:* 18 legs, 9 models | **G1b** (ruled: decisive) |

> ### ⚖ OPERATOR RULING, 2026-08-23 — **score on DECISIVE**. POST-HOC SPECIFICATION, labelled.
>
> The prereg did **not** define decisiveness; it froze a test phrased in terms of gates
> "attributing". The executor scored **both** readings and presented both **before** asking, and
> the operator settled it **having seen both scorings**. That is post-hoc specification and is
> labelled as such here rather than presented as if it had been frozen.
>
> **Ruling:** *moved* is not a measurement — crossing 0.46 flips the bit by construction, so under
> that reading G0's test is unsatisfiable wherever any transition exists at 0.46, and a criterion
> that cannot come out false is not a criterion. The plan half-saw this at Step B ("a scalar
> attributor would have to pick one — which is a narrative, not a measurement") and then failed to
> define decisiveness. The adopted definition — *the gate's own output type is an MT endpoint* —
> answers what OQ-120 actually asks: not *does the bit change* but *does the gate decide the type*.
>
> **Both tables are kept in place below so a cold reader can re-derive under either reading and
> see that the branch changes if they do.** The mitigation for the post-hoc-ness is the
> score-both-then-ask ordering, not the ruling's content.
>
> **Consequence: G1b fires, and G1b is not G0.** One decisive transition in 122,031,
> non-replicating across its own model's redraw triple, on the noisiest leg in OQ-347's table. The
> operator reads that substantively as a draw artifact and so does the executor. **It does not
> matter: the gate was frozen precisely so that a result which *looks* like G0 to the people who
> wanted G0 cannot be scored as G0, and 1 ≠ 0.** [G1-D] governs — Step D writes
> `threshold_boundary_relabel.md` and lands **no commit**; OQ-120 stays **open** and is
> re-specified.

## The three frozen numbers

```
N_eps   = 9351      (story × seat) cells with a qualifying ε-gate transition
N_reach = 9344      ... within their own model stratum's authored-ε [min,max]
N_rail  = 9191      ... visible at 0.01 rail resolution
```

Supporting counts:

```
qualifying transitions (clean)              9853
qualifying with an `unknown` endpoint        178   counted, reported SEPARATELY per prereg
all located transitions, live legs        122031
MT-invariant / FT-only among qualifying     4602   = 46.7% — see below, this is large
```

`N_eps → N_reach → N_rail` barely attenuates (9351 → 9344 → 9191, 98.3% survival). The stratum
range filter removes almost nothing because the authored-ε ranges over each stratum's
claimed-rope-or-snare population are wide; the filter is doing very little work here and should
not be cited as evidence of anything.

## The one thing the prereg did not pin: **a gate BIT that MOVED vs a gate that DECIDED**

Crossing 0.46 flips the `snare_epsilon_floor` bit f→t **by construction, at every transition
located there**, whether or not the snare clause has anything to do with the outcome. So G0's
operational test — *"**none** attribute to `snare_epsilon_floor`"* — is **unsatisfiable as
written** unless there are literally zero type transitions anywhere at ε = 0.46. G0's *prose*
("dominated by χ and by the coalition step at 0.46, which moves d rather than the gate") describes
the observed data almost exactly. **G0's prose and G0's test disagree, and the measurement
satisfies the prose while failing the test.**

To make that distinction measurable rather than rhetorical, the analyzer scores each transition
two ways. **MOVED** = the bit changed across the bracket. **DECISIVE** = the bit changed *and* the
type that gate's own clause produces is actually an endpoint (read on MT, since the gate lives in
`classify_from_metrics/6`, which produces MT).

```
                                   MOVED      DECISIVE
snare_epsilon_floor  (all live)     4717             1
rope_epsilon_ceiling (all live)     6571          1936
```

**The DECISIVE predicate is itself an introduced instrument and carries its own two-sided
control**: it fires on C1's planted shape (`tangled_rope → snare` carrying `snare_epsilon_floor`)
and declines on the naturally-arising `tangled_rope → naturalized` shape — asserted in-code, and
it now also has a **naturally-arising positive** (the single case below), which lifts it off
authored-decoy grade.

## The single naturally-arising `snare_epsilon_floor`-DECISIVE transition

```
leg      testsets_haiku3        stratum  claude-haiku-4-5-20251001 | thinking off
story    equal_protection_kernel__antisubordination_reading
seat     analytical             authored ε 0.28   claimed_type tangled_rope
ε        0.4599 → 0.4600        MT/FT  tangled_rope → snare  (both)
χ        0.680434 → 0.680582    (stays well above the 0.66 snare floor; χ is NOT what moved)
gates that changed:  [snare_epsilon_floor]   ← the ONLY one. coalition_fired did not fire.
```

This is a clean, real instance: the snare ε floor, alone, deciding a type at 0.46. It is also
**one transition in 122,031**, and it **does not replicate across its own model's redraw triple**:

| leg | authored ε | supp | transitions at ≈0.46 |
|---|---|---|---|
| `testsets_haiku` | 0.31 | 0.68 | **0** |
| `testsets_haiku2` | 0.31 | 0.44 | **0** |
| `testsets_haiku3` | 0.28 | 0.62 | 2 (one decisive) |

1 of 3 same-model redraws of the same seed. Per OQ-347's operative line it must be read against
**the noisier of the two models' floors** — and haiku is the noisiest in the table (**65%**
seat-vector churn with ε pinned). A draw artifact is fully consistent with this observation.
`archives/datasets/kernel_v1` (reported separately, never corroboration) has **0** decisive cases
in 10,215 transitions.

## What the pair evidence says, for the Step D labels

**`:591` `threshold_boundary(snare_epsilon_floor, extractiveness, T, rope, snare)`**

FT pairs among all 4717 `snare_epsilon_floor`-MOVED transitions:

```
tangled_rope -> naturalized   4373      rope -> naturalized     38
rope -> tangled_rope           122      rope -> rope            31
unknown -> naturalized          46      unknown -> scaffold     30
tangled_rope -> piton           43      piton -> piton          12
```

**`{rope, snare}` is observed 0 times.** The one decisive case is `tangled_rope → snare`. So the
`rope` half of the label is falsified on every reading; the `snare` half is reachable exactly once
in the whole live corpus, and not from `rope`.

**`:593` `threshold_boundary(rope_epsilon_ceiling, extractiveness, T, rope, tangled_rope)`**

```
all attributed (n=6571)              DECISIVE (n=1936)          DECISIVE, MT pairs
piton -> tangled_rope   2792         piton -> rope      1188     rope -> rope        1163
piton -> rope           1363         rope -> naturalized 617     rope -> naturalized  748
piton -> unknown        1069         rope -> rope         79     rope -> piton         25
rope -> naturalized      617         rope -> piton        19
tangled_rope -> rope     335         piton -> piton       18
```

**The plan's Step D prediction for `:593` was `rope, naturalized`. The observation does not confirm
it.** The modal decisive FT pair is `piton → rope` (1188), with `rope → naturalized` second (617);
the modal decisive **MT** pair is `rope → rope` (1163), i.e. MT does not move at all and the change
is signature-layer. The plan's own instruction — *"if the observed modal pair differs, the
observation wins and the divergence from this sentence is itself a finding"* — applies. Note also
that the labelled pair `rope → tangled_rope` appears **0 times** among decisive transitions (and 36
times in N_rail from any cause), so the existing label is falsified too — just not toward the
predicted replacement.

**Under [G1-D] no relabel commits.** These numbers are recorded for whenever the label question is
decided; they license nothing today.

## MT-invariant / FT-only: 4602 of 9853 qualifying (46.7%)

Nearly half of the qualifying ε-gate transitions leave MT unchanged and move only the
signature-resolved type — the same shape as the already-witnessed
`authority_vacuum_incommensurability` flip (recon finding 3). The prereg required this be kept as
its own category rather than absorbed, and it is much larger than that single prior witness
suggested. It is also why the `:593` decisive MT modal pair is `rope → rope`.

## Deciding-gate census

Over the 9853 qualifying transitions:

```
naturalized_epsilon_floor   5232      coalition_fired            4488
rope_epsilon_ceiling        5232      naturalized_chi_ceiling    4481
snare_epsilon_floor         4621      rope_chi_ceiling           4481
                                      tangled_rope_chi_floor     4481
                                      piton_extraction_ceiling   4468
                                      scaffold_extraction_ceil   4468
                                      rope_chi_nonpositive         84
```

`rope_epsilon_ceiling` and `naturalized_epsilon_floor` are the same 0.45 param read in opposite
senses and always co-move (5232/5232), as do `scaffold_extraction_ceil` and
`piton_extraction_ceiling` (both 0.45 on χ). **`snare_chi_floor` appears in 0 qualifying
transitions** — though it appears 21,740 times across all 122,031. G0's phrasing names it as one of
the three gates transitions should attribute to; among the transitions G0's own filter admits, it
never does.

## Controls

| | result |
|---|---|
| **C1** | **PASS on all 19 corpora.** Carrier selected by the frozen procedure (supp ≥ 0.60 ∧ victims < critical_mass, ranked by max seat χ). On `testsets`: `state_killing_legitimacy__abolition_reading`, transition located ε 0.4599→0.4600, `tangled_rope → snare`, deciding gate set = `{snare_epsilon_floor}` alone. **The probe can see an ε-floor crossing.** Naturally-arising carrier, not synthetic — the synthetic fallback was never needed. |
| **C2** | **PASS on all 19.** With `snare_epsilon_floor` overlaid to 0.90, C1's 0.46 transition **vanishes** on every corpus; `restore-verified 0.46` printed each time. **The probe reads the threshold, not the rail.** |
| **C3** | **PASS on all 19.** The took-effect guard *fails* under the `carbon_tax_2026` shadow plant (direct fact 0.55 wins over perturbed 0.85). The guard is called and can decline. |
| **C4** | **PASS on 10 corpora** (carrier suppression 0.28–0.50, all below the 0.60 floor): declined **55/55** rail points at all 4 seats with `snare_epsilon_floor` SATISFIED and `snare_suppression_floor` BLOCKING, and 0 `snare_epsilon_floor`-attributed transitions. **SKIPPED-precondition on 8** (the carrier is a per-leg redraw and its suppression is 0.60/0.65/0.68 there — at or above the floor, so the control's premise is absent), **SKIPPED-absent on 1** (`testsets`, carrier not present). **0 FAIL.** |

C1 and C2 both discriminated on every corpus, so the prereg's precondition for interpreting the
branches is met.

## Two defects in this audit's own instruments, found and fixed before any number was published

1. **Double emission.** `emit_transition`'s seat filter was a bare disjunction
   `( MT0 \== MT1 ; FT0 \== FT1 )` inside `forall/2`, which yields once per succeeding branch — a
   seat whose MT *and* FT both changed emitted twice, one whose MT was invariant emitted once.
   **Differential, not uniform**: `testsets` went 3228 → 1863 rows, not 3228 → 1614. Every
   transition-level count in the first full sweep was wrong, and the MT-invariant share was
   understated relative to everything else. Fixed to `MT0-FT0 \== MT1-FT1`; all 19 corpora re-swept.
   *The cell-level numbers `N_eps`/`N_reach`/`N_rail` are duplication-invariant (they count distinct
   `(leg, story, seat)` cells) and are identical across the two runs — which is a consistency
   check, not a discrimination check, and is not offered as evidence the fix was unnecessary.*
2. **C4 reported an absent precondition as a FAIL.** The carrier's suppression is a per-leg redraw
   property; on 8 legs it sits at or above the snare floor, so the control cannot run there. The
   first run printed `FAIL declined 0/55` for those. An absent precondition is a SKIP; reporting it
   as failure misattributes the instrument. Fixed to print
   `SKIPPED precondition-carrier-suppression-…-NOT-below-snare-floor-…`.

The pre-fix output is preserved at `raw_PREFIX_double_emission/` with its own README — per
*build_discipline* → *when a defect is found, its before-commit is a free negative control*. No
number in this readout comes from it.

## Findings about the plan itself (reported, not routed around)

1. **S8 is false as written** — the gate baseline. Pristine `f88c8c3c` is GREEN; this audit's own
   Step A turns two rows red (`audit writeup`, `apparatus`), both because the new dir has no
   `WRITEUP.md`/`Fired:` line yet. Settled with a two-sided control, not assumed. Details in
   `audit_log.md`. The concurrent leg generation is **not** implicated.
2. **G0's test and G0's prose disagree** (above). The gate as frozen cannot fire on a corpus where
   any type transition exists at ε = 0.46, which no amount of evidence about domination can change.
3. **G1's subtypes are not total over G1's outcome space.** Under the *moved* reading the result is
   G1 but neither G1a (`N_rail < 10`) nor G1b (single leg). G1b fits exactly under the *decisive*
   reading. The subtype is therefore downstream of ruling #2.
4. **The plan's `:593` prediction (`rope, naturalized`) is not confirmed** by the observed modal
   pair, on either the attributed or the decisive reading.
5. **The recon manifest is stale** (`14:30:09Z`/`d7b4d4f` cited; on disk `16:36:35Z`/`885151b`).
   `n_constraints 285` matches and S11 re-derived identically, so nothing downstream moves.
6. Minor: S4's T2 pin drifted `:459-462` → `:424-427`; S6 has an unnamed near-miss
   (`gap_diagnostic.pl`'s unrelated `threshold_boundary/3`); OQ-347 step 4's situation-fixed core
   does **not** exist yet, so the prereg's preferred population was declared absent, not substituted.
   Full table in `substrate_check.md`.

## Write-set discipline

`git diff --stat -- outputs/ python/ prolog/` is **empty**. Working tree carries only
`audits/INVESTIGATIONS.md` (modified) and this directory (untracked). HEAD has not moved from
`f88c8c3c` during the run.


---

## Four points the operator required stated before Step E

### 1. Legs vs models in G2's second criterion — the plan's own revision, not an executor substitution

The plan carries **both** phrasings, and the later one explicitly supersedes:

- `plan:331` — *"`N_rail ≥ 10` **and** ≥2 distinct legs contribute"*
- `plan:342` — *"**Corroboration is counted in DISTINCT MODELS, not legs — this was rewritten
  2026-08-23 against the OQ-347 floor table and the criterion changed.**"*, with the reason given
  in full (same-model redraw pairs *are* the floor, so two same-model legs agreeing is inside the
  noise the floor describes).

`PREREGISTRATION.md:61` froze **MODELS**, carrying the 2026-08-23 revision. So this was not a
denominator swapped in at scoring time. **Both denominators are reported anyway**, since it costs
nothing and the criterion failed on the pair test regardless:

```
legs contributing to N_rail    18   (kernel_v1 excluded, as G2 requires)
models contributing to N_rail   7
```

≥2 on either denominator; G2 fails on the `{rope, snare}` pair test, not on corroboration.

### 2. Nineteen corpora — mandated at freeze, not a post-hoc expansion

S5 is titled *"ENUMERATE THE LEGS AT EXECUTION TIME — do not take a list from this plan"* and its
own observed list names all eighteen **including `haiku3 993`**, plus `kernel_v1 1106`. Step B
(`plan:270`) reads *"Run across **every leg enumerated at execution time (S5)** — 18 non-empty as
of 2026-08-23."* The five/six-leg figure belongs to the plan's pre-revision draft, which the plan
itself flags as dead: *"The plan-review loop ran against a five-leg substrate that no longer
exists."* So: 18 live legs (= the model legs plus their same-model redraw pairs/triples and the
thinking-on regime legs) + `kernel_v1` swept separately and never counted toward corroboration.
The redraws **were** in scope at freeze time.

### 3. The pair filter — `{piton, rope}` is INSIDE, and N_eps does not understate

The frozen rule is **at least one endpoint** in `{rope, snare, tangled_rope, naturalized}`, not
both (`PREREGISTRATION.md`, N_eps; implemented as `pair_ok = bool(pair & PAIR_TYPES)` at
`analyze.py:87`). `piton → rope` is admitted via its `rope` endpoint. Witness:

```
ε-gate-attributed transitions (live)         11288
  ... admitted by the one-endpoint filter    10031
  ... EXCLUDED                                1257
{piton, rope} transitions  1382   admitted 1382   excluded 0

excluded pairs (neither endpoint admitted):
  piton -> unknown 1069 · piton -> scaffold 118 · piton -> piton 33
  unknown -> scaffold 30 · unknown -> piton 7
```

The 178 `unknown`-endpoint transitions are `unknown → naturalized` (151) and `unknown → rope`
(27) — they passed the one-endpoint filter on their *admitted* endpoint and were then split out
for separate reporting, exactly as frozen. So the answer is *inside*, the set was not widened at
scoring time, and the only population N_eps omits is the 1257 with **neither** endpoint admitted,
of which 1069 are `piton → unknown`.

### 4. S8 re-observed at CLOSE

Recorded against both stamps in `audit_log.md`. OPEN: pristine `f88c8c3c` GREEN, then two red rows
caused by this dir lacking `WRITEUP.md`. CLOSE: re-run after `WRITEUP.md` lands — see the CLOSE
block in `audit_log.md`.
