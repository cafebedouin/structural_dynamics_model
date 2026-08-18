# OQ-190 — The cast blast radius is the classification layer at presence grade, and the VERDICT bucket closes YES: a live resolved claim needs verdict draw-stability real

**Executed:** 2026-08-17
**OQ:** OQ-190 (splits_from OQ-118, gates OQ-75); routes to OQ-118 Limb 3
**Verdict:** 447 SUSPECT rows across 86 files depend on cast/verdict draw-stability — 17 at
name-identity grade (`-and-fails`), 221 at presence/cardinality grade that OQ-118 **never scored**
(`-grade-unmeasured`), 209 `-unwitnessed`; **0** survive. The verdict bucket closes **YES** on
first-hit: OQ-136's resolution rests on `founding_problem_status` *absence* being a generation-path
property, so the reserved OQ-118 Limb-3 temperature sweep is now load-bearing and is escalated as a
spend-go, unspent. Scope caveat the body carries: `-grade-unmeasured` is the dominant disposition
because OQ-118 scored **name identity only**; nothing here measures presence- or cardinality-grade
stability, and the routed follow-on is what would.
**Substrate:** `prolog/testsets/` n=279 for the pruner; prevalence over all five live legs
(279/960/960/1005/1001). Pipeline manifest: `pipeline_run_at=2026-08-18T03:06:44Z`,
`n_constraints=279`, `code_commit=5ce17390`, `code_dirty=true`. Code HEAD at OPEN and CLOSE below.
**Fired:** live — the conclusion-invariance instrument was tested before adoption and would have
manufactured a false survival on its only candidate claim (§4); the 3d resolved-entry extractor
returned 0 and was defective (§3); the documentary control passed on a self-reference while missing
the one claim the prereg requires it to flag (§3); and the verdict bucket produced a live dependent
that flips OQ-118 Limb 3 from reserved to spend-go (§5); and the free precursor the ruling
required found that **26 of 43 absences are not stories at all** — 60% of the phenomenon the
reserved design was about to be spent on is a denominator artifact (§8), which closed the reserve
without spending it and spawned OQ-306.

**Prior art** (`docs/technical/build_discipline.md`, grepped this pass): no hit for
`constraint_captured`, `shared_agent_link`, `agent_beneficiary`, `has_mandatrophy_declaration`,
`stakeholder_gain_flow`, `founding_problem_corroboration_class`. Not a re-discovery.

**Evidence map**

| artifact | what it is | claim it witnesses |
|---|---|---|
| `audit_log.md` | OPEN/CLOSE HEAD stamps, dirty-tree adjudication, frozen prereg md5 above the first result line | ordering: recon → freeze → results |
| `RECON.md` | field vocabulary, rename seam, prevalence, derivation-graph seeds | §1–§2 below; the three premise corrections |
| `PREREGISTRATION.md` | the frozen rule (md5 `875605570f7413d1bf88a56e664f88f3`) | every disposition in §3–§6 |
| `stability_positive_grade.tsv` | OQ-118 stability re-read, positive vs absence-agreement split | §1: `victims` is 0/6 positive, not 4/6 |
| `corpus_prevalence.tsv` | per-leg emission counts, manifest-stamped | §2: the radius is not empty by absence; `founding_problem_corroboration_class` = 0/4205 |
| `edge_admission_raw.tsv` | raw pruner output: treatment diff, control diff, presence, verdict per edge | §2: 10 admitted / 4 rejected, zero `broken` |
| `derivation_graph.tsv` | the admitted+rejected edge set with grades and deciding diffs | §2; the T2b closure's input |
| `consumer_census.tsv` | 465 rows, two passes (name-keyed + closure) | §3 headline arithmetic |
| `doc_claims.tsv` | 23 active-surface documentary dependents | §3; the in-arc annotation queue |
| `promoted_audit_edges.tsv` | 850 forward-citations from the active surface into `audits/` | standing artifact: which historical records are promoted to live |
| `resolved_closure_premises.tsv` | 20 resolved OQs whose closure quotes a cast-presence count as settled | §3; the routed reanalysis queue |
| `conclusion_invariance_reach.out` | the §(b) instrument run on the frozen draw-slices | §4: the fallback branch, witnessed |
| `dispositions.md` | the frozen rule applied to every row | §3–§6 |
| `absence_agreement_exposure.tsv` | per-field split of stable cells into positive vs absence-agreement, all 38 scored fields | §7: the conflation's full reach; the apparatus controls are 18/18 genuine `PRESENT` |
| `limb3_incidence_recon.out` | live-corpus `founding_problem_status` absence by model × prompt_commit | §8: 26/43 absences are non-story artifacts → OQ-306 |
| `limb3_temperature_aliasing.out` | **SUPERSEDED** — comma-split parse truncated `sampling_params` at its first comma | kept as the defective read the correction is against |
| `limb3_temperature_aliasing_CORRECTED.out` | quote-aware re-read: sampling_params by stratum + the 28-file haiku emission split | §8a: temperature CONSTANT in the deciding cell (clean attribution); block-emission mechanism |
| `python/audits/oq190_blast_radius.py` | the re-runnable sweep; `--selftest` = 12 controls | all census claims |
| `prolog/probe_oq190_edge_admission.pl` | the edge-admission pruner | `derivation_graph.tsv` |

---

## 1. Three premises the plan carried were wrong, and RECON corrected them before the freeze

**(a) "Cast fields are ~0/6 draw-stable" is a population claim, not a per-row fact — and one row is
worse than stated.** `stability_table.json`'s `status` conflates *positive agreement* with *shared
absence*. Split out (`stability_positive_grade.tsv`):

| field | stable/6 | **positive**-stable/6 | absence-stable/6 |
|---|---|---|---|
| `base_properties.victims` | 4 | **0** | **4** |
| `base_properties.vindicated_propositions` | 1 | **0** | 1 |
| `stakeholders.roster_card` | 2 | **2** | 0 |
| `stakeholders.time_horizon_multiset` | 1 | **1** | 0 |
| (the other 5 cast fields) | 0 | 0 | 0 |

`victims` at "4/6 stable" is Pattern 5 — every stable cell is `agreement_kind: absence`, i.e. the
field was absent in all three draws and the comparison had nothing to compare. A reader taking
`status` at face value reports `victims` as the *most* stable cast field; it is tied for the least.
Conversely `roster_card` and `time_horizon_multiset` carry genuine positive stability, so "fails by
construction" is true of the population (**3 positive-stable cells out of 54**) and not of every row.
The prereg records this as §(0a) rather than inheriting the rounder claim.

**(b) The verdict bucket is in a different stability regime from the cast bucket** — 7/12
positive-stable vs 3/54. Treating "cast/verdict" as one radius gets the arithmetic wrong, which is
part of why §5 separates cleanly.

**(c) One hop in the plan's exhaustion set does not exist.** `check_indexical_relativity/1` appears
nowhere in the repo (two prose comments). `has_mandatrophy_declaration/1` has **zero consumers** —
a T5b terminal, not a closure edge (§(0b)).

## 2. The derivation graph: the [EDGE] prediction confirmed, at a harder grade than predicted

The plan predicted presence/cardinality reach into classification. It reaches at **name-identity**
grade too — the grade OQ-118 actually scored.

`constraint_captured/1` (`narrative_ontology.pl:332–335`) joins `stakeholder_gain_flow`'s receiver
against the `constraint_stakeholder` roster **on the name**, and gates `classify_from_metrics/6`'s
scaffold clause (`drl_core.pl:420`), three signature gates (`signature_detection.pl:1220/1378/1477`)
and the maxent mirror. `roster_card` is 2/6 positive-stable and the receiver name must survive *and*
match a surviving roster entry.

The pruner (`probe_oq190_edge_admission.pl`) discriminated rather than admitting everything:
**10 edges admitted, 4 rejected, zero `broken`.** The rejections are the informative half, and each
was decided against a **live** positive control (treatment diff 0 while the control diff was 91–472):

| rejected edge | why it matters |
|---|---|
| `constraint_beneficiary → constraint_captured` | the capture join reads the **stakeholder roster**, not beneficiaries — a plausible edge that does not exist |
| `disappearance_verdict → q6_cell` | `q6_cell` reads `founding_problem_status` **only**; the sibling verdict atom does not reach it |
| `founding_problem_status → dr_type`, `disappearance_verdict → dr_type` | no verdict atom is on the classification path |

Largest admitted diff in the graph: e11 `founding_problem_status → q6_cell`, **472** tuples.

## 3. The census, and three instrument defects the controls caught

**Headline (§(c) arithmetic, declared before the run):**

    SUSPECT = confirmed-and-fails (17) + confirmed-grade-unmeasured (221) + unwitnessed (209) = 447

across **86 distinct files**. `SUSPECT-confirmed-and-survives` = **0**, reported beside the headline
and merged into neither total. `cleared` = **0** — §(a) permits T4 only for a consumer shown *absent*
from the closure, and none was. Cast 429 / verdict 18. Body figure: 18 ground-fact authorship rows.

`-grade-unmeasured` dominates because OQ-118 scored name identity only. Those 221 rows are
**confirmed in the radius and unscoreable in this arc** — a distinct state from unexamined, and the
whole content of the routed follow-on in §7.

**Three defects, each found by a control rather than by review:**

1. **The 3d resolved-ISSUES extractor returned 0 and was broken.** It required a resolved entry to
   *announce* a stability premise. Hunting a positive control found OQ-52, which closes on
   *"289/293 both-authored, **4 victim-only**"* — a cast-presence count quoted as a settled
   population fact, resting on stability without ever saying the word. Second admitting pattern
   added: **0 → 20 rows**. Of those 20, **zero** announce; all 20 quote. A 0 on the highest-yield
   surface is indistinguishable from a small radius, which is exactly the shape 3d exists to catch.
2. **The documentary control passed on a self-reference while missing its target.** It asked only
   *"does some row say draw-stable"* and matched **OQ-190's own body**, while OQ-75's
   *"compare only draw-stable fields"* — the one claim the prereg says must be flagged before any
   doc-radius claim — was missed entirely, because that sentence names the **stability table**, not
   a cast field. Second admitting pattern (`stability-instrument-as-gating-premise`) + a self-pin on
   OQ-190's span + a control that asserts the **target line**: 9 → 23 rows, now including
   `CLAUDE.md:1039–1041`.
3. **The sweep censused its own probe** once the probe was committed — `git ls-files` does not
   distinguish a consumer from the tool measuring consumers. Self-pin + control added.

Controls at close: **12 green — 5 two-sided pairs + 2 self-pins.** Cast and verdict pairs are
naturally-arising population members; the 3d pair is naturally arising and its decline (OQ-53, a
resolved entry carrying a draw-stability claim about a **non-cast** field) is a near-miss on the same
surface. The closure pair is **synthetic and declared as such**: a natural T2b candidate's status is
decided by the graph under test, so using one would be circular — reported at the authored-fixture
rung. No free before-commit negative pair exists (OQ-190 responds to no defect commit), stated in the
prereg rather than left unaddressed.

## 4. `-and-survives` was tested before adoption, and it would have manufactured a survival

§(b) pre-authorized deleting the category if the machinery did not earn its keep. **It was run
first** (`conclusion_invariance_reach.out`) on its only candidate — `shared_agent_link/4` network
topology, the claim §(b) itself names as the genuine-survival shape.

Across the frozen draw-slices: k=1 → 5 stories / **0** shared-agent edges; k=2 and k=3 → 6 stories /
**0** edges. The six cohort-zero stories are on six unrelated topics and share no beneficiary or
victim name in **any** draw, so the component count equals the story count in every slice and **no
draw could have made it differ.** Invariant by construction, not by measurement — a consistency
check wearing a discrimination check's clothes. Admitting `-and-survives` on it would have written a
manufactured survival into the one confirmed state the headline excludes.

The declared slice-size confound fired exactly as §(b) predicted, and both available readings are
artifacts: raw, the conclusion is 5, 6, 6 — count-valued, differing from slice size alone, reading as
FAILURE; restricted to the 5 common stories it reads as trivially invariant.

**Branch taken: category deleted.** All 17 name-identity rows land `-and-fails`; the excluded line
is 0.

## 5. Verdict bucket: **YES**, and the Limb-3 spend is now load-bearing

`ISSUES.md:5784`, OQ-136, **resolved** 2026-07-02:

> **q6_unmeasured (26) + no_agent_seats (26) = authoring artifact** (clustered on model AND
> prompt_commit, p_holm=8e-4; 25/26 overlap = ONE generation-path gap)

`q6_unmeasured` fires precisely when `\+ founding_problem_status(C, _)` (`stakeholder_seats.pl`
`q6_cell`, first branch). The resolution therefore asserts that 26 stories' **absence of a verdict
field is a property of the generation path**, not of the draw — a backward crossing of the
determinism frontier. OQ-202/OQ-203/OQ-204 were minted from it.

**OQ-118 cannot settle this in either direction.** It scored `founding_problem_status` at
name-identity grade — 3/6 positive-stable, three value-shifts, **zero presence-flips** — but all six
frozen stories *have* the field authored, so the cohort contains **no story in the `q6_unmeasured`
condition at all.** The presence premise is unmeasured, not confirmed.

Corroboration from the pruner: e11 `founding_problem_status → q6_cell` is admitted with the graph's
largest treatment diff (**472**). And the census's own guard does not catch this: `Σ buckets ==
n_corpus` (`run_pipeline.py`) is a **totality** check — a redraw redistributes stories between
`q6_unmeasured` and the named cells without changing the sum, so the guard stays green while the
finding moves.

**§(f) applied: ESCALATE AS A SPEND-GO, DO NOT SPEND.** The reserved ~$1.5–2 temperature sweep is
now load-bearing on a live resolved claim with three OQs downstream. The operator's ruling is owed.
Exhaustion is neither claimed nor needed — Amendment B runs to first-hit **or** exhaustion, both
closure arms passed, so the `reserved-pending` escape does not apply.

## 6. Inert, and never rendered as cleared

`founding_problem_corroboration_class/2` — **T5 inert-unexercised**: emitted, read by two consumers,
**0/4205 authored** across all five live legs. `constraint_vindicates/2` and
`has_mandatrophy_declaration/1` — **T5b inert-unconsumed**: authored (150/279 and 236/279) and
emitted, with no reader anywhere. Opposite conditions, distinct labels; neither is `cleared`. The
mandatrophy clause is admitted at name-identity grade (e10/e10b), so it becomes a T1 dependent the
day anyone writes its reader.

## 7. Operator-required defences (added at the close ruling, 2026-08-17)

### `cleared = 0` is a FLOOR, not a measurement — and the population is near-tautological

The honest answer is the second one. **Clearing was never attempted in this arc.**

- **71 rows** reached the T4 candidate branch (per-story render, story is the unit — all 71 are
  test-file rows). **0** were carried to a clearing test.
- **394 rows** were never candidates: they entered the census by matching a cast/verdict name or by
  the closure, and §(a) permits T4 only for a consumer shown **absent** from the closure.

So the census population *is* "rows the sweep hit," and a row in it by name-match is by construction
not absent from the name surface. **`cleared = 0` therefore reports that no row was tested for
clearing, not that no row would clear.**

**447 is a floor for a SECOND and INDEPENDENT reason — vocabulary under-coverage — and a reader must
not assume the clearing gap covers it.** RECON step 1 took `cohort_stability.FIELDS` as the declared
single source of truth for the authored field vocabulary, on the strength of
`cohort_sigma_seat_eval.py:32` importing it. That list turned out to **under-cover its own subject**:
`gain_flow` and `fixing_cost` are the authored sources of `constraint_captured/1` and
`piton_candidate/1` — the name-identity edges that put the cast radius on the classification path —
yet they sit outside the frozen cast-9 (in `FIELDS` they are top-level entries bucketed `sigma`).
The census therefore may under-enumerate **wherever else the authored surface exceeds `FIELDS`**,
and nothing in this arc bounds that. The two gaps are independent: clearing was never *attempted*,
and the vocabulary was never *verified complete against the schema*. Closing either leaves the other
open. The decline arms show the instrument *can* decline on
constructed and on natural negatives; they do not show it declined on this population, because it
was never asked to. Anyone citing the 447 should read it as *"447 not shown safe,"* never as
*"447 shown unsafe."*

The 71 test-file rows are the cheapest place to attack that floor, and they are named in
`consumer_census.tsv` (`tier == T4?`).

### The self-census exclusion is a DECLARED PATH LIST, not a filter

Excluding paths from a census is the move Amendment A exists to constrain, so it is auditable.
`SELF_EXCLUSIONS` in `python/audits/oq190_blast_radius.py` names three paths, each with its reason:

| excluded path | reason |
|---|---|
| `audits/2026-08-17_oq190_blast_radius/` | this audit's own directory — its artifacts ARE the census, not inputs to it |
| `python/audits/oq190_blast_radius.py` | the sweep itself: every name in its vocabulary tables would census as a hit |
| `prolog/probe_oq190_edge_admission.pl` | the pruner: its `edge/4` facts name every cast/verdict predicate by design |

Three selftest checks hold the declaration to the code **in both directions**: no declared exclusion
leaks into the surface; **every declared exclusion still names a real tracked path** (a stale entry
is an exclusion doing nothing while reading as diligence); and **no undeclared path is excluded**
(recomputed from scratch and diffed against the declaration). Corpus-data exclusion
(`DATA_PREFIXES`) is a separate rule, declared separately, so a data rule cannot quietly absorb an
instrument rule.

### The absence-conflation does NOT reach the instrument — checked, not assumed

The concern is exact: the apparatus-presence rows are the control-eligible negatives and their 6/6
was read off the same table whose `status` conflates agreement with absence. If any were 6/6 *by
shared absence*, the cast decline arm and the closure's must-not-reach arm would stand on a vacuous
positive.

**They are not.** All three are 18/18 literal `PRESENT` tokens, zero absence cells
(`absence_agreement_exposure.tsv`, values pasted per story in the raw read). Reading
`cohort_stability.classify_field`: `all_absent` and `all_empty` return `("stable", "absence")`
*before* the `len(uniq) == 1` branch that yields `positive`, so `positive` cannot be reached by a
uniformly-absent field. The controls are sound.

**Full exposure map, since the question is about reach rather than about three rows.** Eight of the
38 scored fields carry absence-inflated stables: `has_sunset_clause` (6/6 absence — pure),
`coercion_grid.presence` (6/6 — pure), `victims` (4), `cs_structure.presence` (4), `gain_flow` (3),
`fixing_cost` (3), `vindicated_propositions` (1), `requires_active_enforcement` (1). The other 30 are
clean.

**Both machine consumers already filter it.** `cohort_sigma_seat_eval.py:208` (`if
r["agreement_kind"] == "absence": continue`, comment citing Pattern-5) and
`oq118_reprobe.py:117` both exclude absence before tabulating. **The defect's reach is to human
readers taking `status` at face value** — which is what the OQ-190 plan did, and what the three
in-arc annotations now guard.

**One genuinely unrouted item, now routed to OQ-304:** `gain_flow` and `fixing_cost` are
**1/6 positive-stable each** (3 absence, 2 unstable) — and they are the authored sources of edges
e6/e9, i.e. of `constraint_captured/1` and `piton_candidate/1`, the sharpest name-identity edges in
the graph. They sit **outside** the prereg's frozen cast-9 (in `FIELDS` they are top-level entries
bucketed `sigma`), so the frozen bucket definition did not cover the two fields feeding its own
headline finding. This *strengthens* the finding — the base is weaker than `roster_card`'s 2/6 — but
it is a scope gap and is now named in OQ-304.

### `shared_agent_link/4` has ZERO edges on the frozen cohort — the corollary of the §4 kill

Recorded so a future reader learns it here rather than by running it. Across all three frozen
draw-slices the six cohort-zero stories produce **0** shared-beneficiary and **0** shared-victim
edges — they are on six unrelated topics and share no actor name in any draw
(`conclusion_invariance_reach.out`). **`shared_agent_link` is therefore not a viable re-witness
target on cohort zero at all**, in either direction: it cannot show a survival (nothing to be
invariant) and it cannot show a failure (nothing to break). A re-witness of the contamination-network
claims needs a corpus where stories share actors — the live leg or a twin, not the frozen cohort.

### Correction to the plan's exhaustion set, restated

`check_indexical_relativity/1` was named in the endorsed §(e) exhaustion set and **does not exist**
(two prose comments, `narrative_ontology.pl:155` and `:576`). Caught by RECON §2b before the freeze
and carried as prereg §(0b). Recorded here because a sender-side pin that was never checked is the
same shape this thread's discipline is about, and the record should say so plainly rather than let
the correction sit only in a §(0) note.

## 8. Limb-3 free precursor: the incidence recon the spend-go ruling required

The ruling approved the un-reserve in principle and **withheld the spend** on the ground that the
reserved design (verdict *value* stability across temperature) does not target the question that
un-reserved it (verdict *presence/absence attribution*). It required the free Ω_E version first.
Run: `limb3_incidence_recon.out`, zero spend, artifacts on disk.

**Live corpus `prolog/testsets/`, n=279: `founding_problem_status` absent in 43 (15.4%).**

| stratum | absent | total | rate |
|---|---|---|---|
| **no `story_provenance` at all** (`*_contradictions`) | **26** | 26 | **100%** |
| `claude-haiku-4-5-20251001` @ `22843cdf` | **16** | 28 | **57%** |
| `claude-sonnet-4-5-20250929` | 1 | 75 | 1.3% |
| `claude-sonnet-5` | 0 | 127 | 0% |
| `gemini-2.5-flash` / `claude-sonnet-4` / `kimi-k3` | 0 | 23 | 0% |

**The headline result: 26 of the 43 absences — 60% — are not stories.** Every one of the 26
`*_contradictions` files carries exactly one predicate, `narrative_ontology:cs_axiom_contradiction`,
and nothing else: no `six_questions`, no `base_properties`, no stakeholders. They are axiom
meta-files (documented: `ISSUES.md:4038` *"axiom meta-files"*, `:5345` *"pl-only (NO_JSON)"*), and
they sit in the `n_constraints = 279` denominator — confirmed present in `per_constraint`. Prior art
is honest here: **the artifact kind is known and OQ-136 itself named 9 of them as a stratum.** What
is new is the share — the stratum has grown **9 → 26** and now dominates the absence count.

**Consequences for the spend design, which is what the ruling asked for:**

1. **A temperature sweep cannot reach 60% of the phenomenon.** An axiom meta-file has no
   `six_questions` block to populate; no sampling temperature makes it grow one. These absences are
   structural, not stochastic, and they are also **un-targetable** — the files carry no
   `story_provenance`, so there is no model or prompt_commit to sweep at.
2. **The genuine story-level phenomenon is much smaller than the raw count suggests:**
   43 − 26 = **17 absences over 253 story files = 6.7%**, and **16 of the 17 sit in one stratum**
   (haiku @ `22843cdf`, 57%). The remaining 1 is a lone sonnet-4-5 file.
3. **Within-stratum variance is where a probe must aim, and it exists in exactly one place.** Five
   of seven strata are at 0% absence across 225 stories — nothing there for temperature to move. The
   haiku/`22843cdf` stratum at 16/28 is the only cell showing both outcomes, and it is a
   well-defined 28-story target.
4. **It corroborates OQ-136's clustering while narrowing what the clustering means.** The haiku
   numbers reproduce exactly (16/28 haiku; 17/74 at `22843cdf`) — but that is the *same* stratum
   OQ-136 measured, not independent replication. The `*_contradictions` limb, meanwhile, is now
   explainable **with no stability claim at all**: those files could never have carried the field.

### 8a. The gating aliasing check — CORRECTED: temperature IS constant in the deciding cell

**The first run of this check was defective, and the correction is recorded here rather than
substituted silently.** `limb3_temperature_aliasing.out` split `story_provenance/8` arguments on
`,` — but `sampling_params` values *contain* commas
(`'max_tokens=16384,temperature=api_default'`). Arg 8 was therefore truncated at its first comma,
every compound value read as its `max_tokens` prefix, and **every temperature term in a compound
value was silently dropped.** A truncated-but-plausible field value that parses fine is Pattern 4 in
the probe rather than in the engine. Corrected, quote-aware re-read:
`limb3_temperature_aliasing_CORRECTED.out` (the defective output is kept beside it, superseded).

| | defective first read | corrected |
|---|---|---|
| stories carrying a temperature term | 31 / 279 (11%) | **80 / 279 (29%)** |
| — of which numeric | 31 | 42 |
| — of which symbolic (`api_default`/`default`) | 0 | 38 |
| the deciding cell's `sampling_params` | `max_tokens=16384` (temperature absent) | **`max_tokens=16384,temperature=api_default`** |

**This lands the ruling on its FIRST branch, not the third state I reported.** The haiku @
`22843cdf` cell is **28/28 at a single `sampling_params` value**, temperature term included. So
temperature is **constant within the one cell that exhibits both outcomes**, and therefore cannot
explain the 16-vs-12 split inside it. The clustering attribution is clean and unaliased, and the
reserve closes permanently — which is exactly the condition the sweep ruling named.

Corpus-wide, every stratum is a *single* `sampling_params` value, so temperature is perfectly
confounded with (model, prompt_commit) **between** strata. That does not matter here: the question
is within-cell variance, and the only cell with variance holds temperature fixed.

**(ii) Where temperature varies at all, it separates nothing.** Across the 80 temperature-bearing
stories the only absences are the 16 in the haiku cell — where temperature is constant. The other
64 carry 0 absences across four distinct temperature terms (0.1, 0.2, 1.0, `default`).

**(ii) A positive mechanism, measured over the full stratum, that discriminates against
temperature.** Splitting all 28 haiku files:

| | n | median bytes | median facts | carry `constraint_stakeholder` |
|---|---|---|---|---|
| **has** `founding_problem_status` | 12 | 39,072 | 96 | **12/12** |
| **absent** | 16 | 27,564 | 58 | **0/16** |

The split is **categorical and non-overlapping**: absent max 32,788 B < present min 34,470 B; absent
max 64 facts < present min 84. Two distinct fact families — stakeholders and the verdict atoms —
go missing **together**, all-or-nothing. That is a **whole-block emission failure**, consistent with
truncation under `max_tokens` + adaptive thinking, or with a schema-branch miss. A temperature
mechanism would produce a graded continuum with partial emission; it produces neither.

*Honest caveat on what carries the weight:* file size is downstream of the missing block and is
**not** independent evidence. The discriminating facts are the **categorical** 0/16-vs-12/12 split
and the **co-missing** families — a per-field sampling outcome does not remove two whole fact
families from 16 files while leaving 12 complete with no overlap.

This upgrades OQ-136's few-file hand-read to a full-stratum measurement, and the mechanism **is
already owned by OQ-202** ("Generation authoring gap: haiku + contradictions paths under-emit the
fact layer"), open since 2026-07-02. Nothing new needs minting for it.

**Ruling recorded: RE-RESERVE — Limb 3 closes resolved-by-corpus, not resolved-by-spend.** The
~$1.5–2 reserve is released and does not carry forward. On the corrected read the close is
*stronger* than filed: temperature is constant in the deciding cell (clean attribution), **and** a
positive mechanism accounts for the split. A within-cell temp sweep would hold constant the one
thing already held constant — there is nothing for it to vary.

**But "released" must not be read as "the temperature question was answered corpus-wide."** It was
answered on one cell where temperature is fixed, plus a mechanism attribution. For the other
199 stories that carry no temperature term, no sampling-parameter attribution is possible from
substrate at all — the reserve is partly **unspendable-as-designed**, not merely unnecessary. That
capability absence is declared as **GAP-37**, so the next person reaching for a temperature
explanation learns it before scoping work rather than after.

### 8b. The denominator finding is routed OUT — it is larger than the verdict question

26 non-stories inside `n_constraints = 279` means **every corpus rate computed against that
denominator is off by ~10%**, not merely `q6_unmeasured`. And the stratum was **9** when OQ-136
measured it and is **26** now: an artifact stratum that grows silently inside a denominator **gets
worse while looking stable**, so historical corpus rates are not comparable to current ones even
when both were computed correctly at their own time. Filed as **OQ-306** with its own census, its own
denominator ruling, a consumer sweep, and a growth guard — the growth, not the presence, is what
makes it recur. It is the same shape OQ-190 was filed to enumerate: a population that silently
includes non-members.

## 9. Residue — what changed in substrate, and what is handed off

**HEAD stamps.** OPEN `f80bc3eb`, CLOSE `8c34157f`. They differ **only by this audit's own five
commits** (`cd79b5e1`, `643d1cba`, `badf88dd`, `d0199458`, `8c34157f`); `git diff --stat OPEN CLOSE`
over the audit's read-set (the nine engine files, the two generators, the three trackers, and
`audits/2026-06-12_cohort_zero/`) is **empty**. No concurrent writer. `git status --short
audits/2026-06-12_cohort_zero/` is clean — no archived artifact was mutated.

**Landed in substrate:**
- `ISSUES.md` — OQ-190 closed scoped; OQ-118 Limb 3 flipped to spend-go-pending with §5's
  enumeration; OQ-75's stability-table premise dispositioned; new OQ minted for the presence/
  cardinality re-score; new OQ minted for the 20 routed resolved-entry reanalyses.
- `KNOWN_STATE.md` — the `victims` 0/6-vs-4/6 correction and the `constraint_captured` name-identity
  channel.
- `repair-by-annotation` edits to the 23 `doc_claims.tsv` rows.

**Handed off, with why-this-one and what it is gated behind:**
1. **OQ-118 Limb 3 — spend-go escalation (operator ruling owed).** §5 is the enumeration that
   prices it. Gated behind nothing; it is the ruling itself.
2. **Re-score the frozen draws at presence and cardinality grade.** This converts the 221
   `-grade-unmeasured` rows — the bulk of the radius — into dispositioned ones. It re-reads the
   **same 17 cohort-zero replicates already on disk**: Ω_E, **zero generation spend**. **Not**
   Limb 4: Limb 4 is the τ-calibrated graded *distance* metric for name identity (renamed-vs-fresh
   cast) with its own control burden; this is a different, much cheaper question at two coarser
   grades. Given the headline, higher-value than most of the repair queue.
3. **The 20 routed resolved-entry reanalyses**, in `resolved_closure_premises.tsv`. OQ-52 and OQ-136
   are the two with named downstream mints and go first.
4. **Side product, recorded in OQ-118:** the `reading_diff` re-point's cohort-one gate is now
   satisfiable — `constraint_stakeholder` is dense on the live corpus (236/279) and `reading_diff.pl`
   was un-stranded onto the live stakeholder-seat schema (`01cff6a7`).

**Not built here, deliberately:** the OQ-118 Limb-4 graded distance metric (exact set-match conflates
"fresh cast" with "renamed cast") — Limb 4's own item, with its own control burden.
