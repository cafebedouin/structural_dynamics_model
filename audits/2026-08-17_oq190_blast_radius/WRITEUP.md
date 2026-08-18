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
that flips OQ-118 Limb 3 from reserved to spend-go (§5).

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

## 7. Residue — what changed in substrate, and what is handed off

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
