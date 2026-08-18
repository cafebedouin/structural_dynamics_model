# OQ-190 dispositions — the frozen rule applied to every census row

Adjudication is a **separate read-only deciding pass**: it applies
`PREREGISTRATION.md` §(a)/§(b)/§(c) to the artifacts already written. It generates nothing.

All 12 controls (5 two-sided pairs + 2 self-pins) are GREEN, which is the precondition
§(d) sets on making any radius claim at all.

## Code surface (`consumer_census.tsv`, 465 rows)

| disposition | rows | grade mix |
|---|---|---|
| `SUSPECT-confirmed-grade-unmeasured` | 221 | presence:204, cardinality:17 |
| `SUSPECT-unwitnessed` | 209 | —:209 |
| `n/a-authorship` | 18 | —:18 |
| `SUSPECT-confirmed-and-fails` | 17 | name-identity:17 |

**Headline arithmetic (§(c)):** SUSPECT = 17 + 221 + 209 = **447**
across **86 distinct files**.

`SUSPECT-confirmed-and-survives` = **0** — reported beside the headline, never merged in.
The conclusion-invariance instrument was NOT carried (§(b) fallback branch taken); see §4 below.

Body figures, never the headline: `n/a-authorship` (ground-fact producers) = 18;
`cleared` = **0** — §(a) permits T4 only for a consumer shown ABSENT from the closure, and
no row was so shown. `inert-unexercised` and `inert-unconsumed` are §3 below (field-level, not row-level).

### Bucket split

- **cast**: 429
- **verdict**: 18

### The 17 name-identity rows — the only grade OQ-118 actually scored

| site | via | reaches |
|---|---|---|
| `prolog/commentary_census.pl:95` | `founding_problem_status` atom identity | `outputs/commentary_census.json` q6 cells |
| `prolog/drl_core.pl:420` | `stakeholder_gain_flow` ⋈ `constraint_stakeholder` on receiver NAME | `classify_from_metrics/6` scaffold clause; 3 signature gates; maxent spec |
| `prolog/drl_purity_network.pl:114` | beneficiary/victim NAME-set intersection across constraints | contamination network edges + purity cascade |
| `prolog/drl_purity_network.pl:164` | beneficiary/victim NAME-set intersection across constraints | contamination network edges + purity cascade |
| `prolog/inferred_coupling_protocol.pl:228` | beneficiary/victim NAME-set intersection across constraints | contamination network edges + purity cascade |
| `prolog/json_report.pl:1363` | beneficiary/victim NAME-set intersection across constraints | contamination network edges + purity cascade |
| `prolog/maxent_classifier.pl:204` | `stakeholder_gain_flow` ⋈ `constraint_stakeholder` on receiver NAME | `classify_from_metrics/6` scaffold clause; 3 signature gates; maxent spec |
| `prolog/narrative_ontology.pl:169` | verdict atom identity | `has_mandatrophy_declaration/1` (no consumer — T5b terminal) |
| `prolog/narrative_ontology.pl:170` | verdict atom identity | `has_mandatrophy_declaration/1` (no consumer — T5b terminal) |
| `prolog/narrative_ontology.pl:333` | receiver name | `constraint_captured/1` |
| `prolog/narrative_ontology.pl:335` | roster membership of the receiver name | `constraint_captured/1` |
| `prolog/signature_detection.pl:867` | `uncaptured` + `fixing_cost_class` (gain_flow receiver name) | FCR-branch signature refinement |
| `prolog/signature_detection.pl:1220` | `stakeholder_gain_flow` ⋈ `constraint_stakeholder` on receiver NAME | `classify_from_metrics/6` scaffold clause; 3 signature gates; maxent spec |
| `prolog/signature_detection.pl:1378` | `stakeholder_gain_flow` ⋈ `constraint_stakeholder` on receiver NAME | `classify_from_metrics/6` scaffold clause; 3 signature gates; maxent spec |
| `prolog/signature_detection.pl:1477` | `stakeholder_gain_flow` ⋈ `constraint_stakeholder` on receiver NAME | `classify_from_metrics/6` scaffold clause; 3 signature gates; maxent spec |
| `prolog/signature_detection.pl:1479` | `uncaptured` + `fixing_cost_class` (gain_flow receiver name) | FCR-branch signature refinement |
| `prolog/signature_detection.pl:1794` | `uncaptured` + `fixing_cost_class` (gain_flow receiver name) | FCR-branch signature refinement |

## Documentary surface (`doc_claims.tsv`, 23 rows)

- `stability-instrument-as-gating-premise`: 21
- `named-field-stability-claim`: 2

All 23 are `repair-by-annotation` (in-arc): each names the stability table or a cast field
as a gating premise and needs the OQ-118 finding attached, not a re-analysis.
Files: `KNOWN_STATE.md`(12), `ISSUES.md`(8), `CLAUDE.md`(3)

## Resolved-ISSUES closure premises (`resolved_closure_premises.tsv`, 20 rows)

Reconciliation PASSED before the read (extractor 151 = `issues_status.py` 151), per §(g).

- `cast-presence-count-as-settled-fact`: 20

**Zero resolved entries ANNOUNCE a stability premise; all 20 quote a cast-presence count as
a settled population fact.** That is the surface's whole point — a closed issue is a standing
claim that does not announce itself as live — and it is why the first version of this
extractor returned 0 and would have been read as a small radius.

OQs: OQ-45, OQ-52, OQ-63, OQ-64, OQ-83, OQ-86, OQ-108, OQ-114, OQ-117, OQ-134, OQ-136, OQ-137, OQ-151, OQ-153, OQ-186, OQ-187, OQ-188, OQ-195, OQ-207, OQ-255

All 20 are `repair-by-reanalysis` (routed, not in-arc): re-opening a closed OQ's arithmetic
is a ruling, not an annotation.

---

## §4 — `SUSPECT-confirmed-and-survives`: the §(b) FALLBACK BRANCH was taken, with a witness

§(b) pre-authorized two branches and required that one be recorded. **The category is DELETED.**
It is not vacant-by-accident; it was tested and its door opens onto a false positive.

Witness: `conclusion_invariance_reach.out`. The instrument's only candidate claim in this radius
is the one §(b) itself names — `shared_agent_link/4` network topology, where names churn but
topology might not. Run over the frozen cohort-zero draw-slices:

- k=1: 5 stories, **0** shared-agent edges → 5 components
- k=2: 6 stories, **0** shared-agent edges → 6 components
- k=3: 6 stories, **0** shared-agent edges → 6 components

The six cohort-zero stories are on six unrelated topics and share **no beneficiary or victim name
in any draw**. So the component count equals the story count in every slice, and **no draw could
have made it differ.** That is a consistency check wearing a discrimination check's clothes — the
`build_discipline` shape *"a check that CANNOT fail witnesses nothing while looking exactly like
one that passed."* Admitting `-and-survives` on it would have manufactured a survival into the one
confirmed state §(c) excludes from the headline.

**The declared slice-size confound fired exactly as §(b) predicted it would**, and is worth
recording because it shows both available readings are artifacts: the raw conclusion is 5, 6, 6 —
count-valued, so it differs from slice size alone and reads as FAILURE; restricted to the 5 stories
common to all slices it reads as trivially invariant. Neither reading measures anything.

Consequence for the headline, per §(c): all 17 name-identity rows land
`SUSPECT-confirmed-and-fails`, and the excluded-line figure is **0**.

---

## §5 — VERDICT BUCKET (Amendment B / OQ-118 Limb 3): **YES — a live claim needs verdict stability real**

Run outside the cheapness filter, to first-hit. **First hit found; the bucket closes YES.**

**The dependent: OQ-136, resolved 2026-07-02, and the three OQs minted from it.**

`ISSUES.md:5784` closes OQ-136 on:

> **q6_unmeasured (26) + no_agent_seats (26) = authoring artifact** (clustered on model AND
> prompt_commit, p_holm=8e-4; 25/26 overlap = ONE generation-path gap)

`q6_unmeasured` is the q6 cell that fires precisely when `\+ founding_problem_status(C, _)` —
the authored verdict side is **absent** (`stakeholder_seats.pl` `q6_cell`, first branch). So the
resolution asserts that 26 specific stories' **absence of `founding_problem_status` is a property
of the generation path** (model × prompt_commit), not of the draw. That is a backward crossing of
the determinism frontier on a verdict field, and OQ-202/OQ-203/OQ-204 were minted from it.

**Why OQ-118 cannot settle it, which is what makes this a spend-go rather than an answer.** OQ-118
scored `founding_problem_status` at name-identity grade: 3/6 positive-stable, three stories
value-shifting, **zero presence-flips**. But every one of the frozen six *has* the field authored —
the cohort contains no story in the `q6_unmeasured` condition at all. The presence claim OQ-136
rests on is therefore **unmeasured**, not confirmed; OQ-118's population cannot speak to it in
either direction.

**Corroborating edge from the pruner** (`derivation_graph.tsv`): e11 `founding_problem_status →
q6_cell` is ADMITTED with a treatment diff of **472** — the largest in the graph. The verdict atom
is not a decoration on the census; removing it moves 472 observation tuples.

**And the census's own guard does not catch this.** `run_pipeline.py`'s `Σ buckets == n_corpus`
invariant is a **totality** check. A redraw redistributes stories between `q6_unmeasured` and the
named cells without changing the sum, so the guard stays green while the finding moves — exactly
the success-shaped absorption the prereg flagged in advance.

**§(f) outcome map applied — YES ⇒ ESCALATE AS A SPEND-GO, DO NOT SPEND.** The reserved ~$1.5–2
OQ-118 Limb-3 temperature sweep is now **load-bearing**: a live resolved claim, with three OQs
downstream of it, needs the verdict class actually draw-stable. The operator's ruling is owed;
this audit does not spend.

**Exhaustion is not claimed and is not needed.** Amendment B runs to first-hit **or** exhaustion;
a first hit closes the bucket YES. Both closure arms passed, so the `reserved-pending` escape does
not apply. For the record the verdict consumer set is 18 census rows across 8 files, plus the two
field-level dispositions in §6.

---

## §6 — Field-level dispositions (T5 / T5b): inert, and **never** rendered as cleared

| field | tier | evidence | why it is not `cleared` |
|---|---|---|---|
| `founding_problem_corroboration_class/2` | **T5 inert-unexercised** | emitted by the generator; read by `enhanced_report.py` + `stakeholder_seats.pl:605`; **0/4205 authored** across all five live legs (`corpus_prevalence.tsv`) | a consumer starved of input. The moment one story authors it, its readers become live verdict dependents. |
| `constraint_vindicates/2` | **T5b inert-unconsumed** | emitted; declared at `narrative_ontology.pl:84,126`; **no reader anywhere**; authored 150/279 on the live leg and 735/1001 on sonnet | an input with no consumer. Safe *now*; a dependent the moment anyone writes a reader — and `vindicated_propositions` is 0/6 positive-stable with presence-flips in 3 stories. |
| `has_mandatrophy_declaration/1` | **T5b inert-unconsumed** | `narrative_ontology.pl:158–170`, exported at `:111`; **no consumer** in `prolog/`, `python/`, `agent/`. The plan's named hop `check_indexical_relativity/1` **does not exist** (two prose comments only) | same. Its verdict-atom clause is admitted at name-identity grade (edges e10/e10b, treat_diff 15 each) — it *would* be a T1 dependent the day it gets a reader. |

T5 and T5b are opposite conditions and are recorded under distinct labels, per §(a). Neither is
`cleared`; both stay in the census with the doc sweep applied.
