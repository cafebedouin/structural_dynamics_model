# OQ-190 PREREGISTRATION — frozen before any adjudication

**Frozen:** 2026-08-17, after RECON.md, before any census row is dispositioned.
**Scope:** Ω_E. **Zero generation spend is authorized.** Any step that would require a new draw
returns `reserved-pending` and escalates instead.

This file is a **handoff**. Per *Write the receiver's prompt*, every section below enumerates
concrete actions a receiver who read only this file can take. Where it names a design instead of an
action, that is a defect in this file, not a licence to improvise.

**The receiver's licence to refuse is stated here so it can be exercised.** If any instruction below
is correct in prose and wrong when executed, report the refusal at the volume of a completion and
route it back — do not silently repair it. Scope is narrow: *executed as written, this produces what
the design forbids.* "Hard" or "I'd do it differently" is a one-sentence flag that proceeds.

---

## §(0) Two RECON corrections this prereg carries, so the frozen text is not stale on arrival

**(0a) "Cast is 0/6 draw-stable" is a POPULATION claim, not a per-row theorem.**
`stability_table.json` `status` conflates positive agreement with shared **absence**. At positive
grade the cast bucket is **3 stable cells out of 54** — but `stakeholders.roster_card` is 2/6 and
`stakeholders.time_horizon_multiset` is 1/6, both genuinely positive, and
`base_properties.victims` is **0/6** positive (its four "stable" cells are `agreement_kind:
absence`, i.e. the field was absent in every draw). Wherever §(b) below says a name-identity
re-witness "fails by construction," read: *fails for the 7 cast fields at 0/6 positive, and is a
2/6-or-1/6 coin for `roster_card` and `time_horizon_multiset`.* A row keyed on those two is
`SUSPECT-confirmed-and-fails` on the population read and MUST record the per-field number beside it.

**(0b) The §(e) exhaustion set loses one named hop and gains a row.** `check_indexical_relativity/1`
does not exist in the repo (two prose comments only). `has_mandatrophy_declaration/1` has **zero
consumers** in `prolog/`, `python/`, `agent/` and is therefore a **T5b terminal**, not a closure
edge. Exhaustion is unaffected in kind — the closure still runs — but the receiver must not go
looking for that predicate and must not read its absence as an unfinished sweep.

---

## §(a) The dependency rule

A consumer or claim depends on draw-stability iff it **crosses the determinism frontier backward** —
asserts something about the kernel/topic/material expected to hold on a redraw. Applied as ordered
strata; first matching tier wins.

| Tier | Test | Disposition | Grade |
|---|---|---|---|
| T1 | joins stories on cast **name identity** | DEPENDENT (strongest) | name-identity |
| T2 | keys a threshold or topology on cast **cardinality** | DEPENDENT | cardinality |
| T2b | consumes a value whose own derivation is T1–T3 | DEPENDENT-transitive | inherited from the **weakest** edge on the path |
| T3 | corpus aggregate over cast **presence**, reported as a population property | DEPENDENT-weak | presence |
| T4 | per-story descriptive render; story is the unit; no cross-draw expectation | CLEARED | — |
| T5 | mechanism exists, input **never authored** on any live leg | INERT-unexercised (*not* cleared) | — |
| T5b | field is authored and emitted but has **no consumer** | INERT-unconsumed (*not* cleared) | — |
| T6 | anything unclear | SUSPECT-unwitnessed (fail-closed) | — |

**T5 and T5b are opposite conditions and must not share a label.** T5 is a consumer starved of input
(`founding_problem_corroboration_class/2`: emitted, read by `enhanced_report.py` and
`stakeholder_seats.pl`, **0/4205 authored across all five legs**). T5b is an input with no consumer
(`constraint_vindicates/2`, `has_mandatrophy_declaration/1`) — safe *now*, dependent the moment
anyone writes a reader, so T5b rows still get the doc sweep. In the writeup **"inert" never renders
as "cleared"** for either.

**T2b is computed as a closure over `derivation_graph.tsv`, never by grep.** A consumer reachable
from a cast field through any chain of derived values is T2b even though its source text contains no
cast name. **T4 may only be assigned to a consumer shown ABSENT from the closure**, never to one the
grep merely did not hit.

---

## §(b) Disposition vocabulary — each disposition names its admitting instrument

`{cast | verdict | both}` × `{SUSPECT-confirmed-and-fails | SUSPECT-confirmed-grade-unmeasured |
SUSPECT-confirmed-and-survives | SUSPECT-unwitnessed | cleared | inert-unexercised |
inert-unconsumed}`, every row also carrying its **grade**. Documentary hits additionally carry
`repair-by-annotation` (in-arc) or `repair-by-reanalysis` (routed OQ).

**`SUSPECT-confirmed-grade-unmeasured`** — mechanism keyed on presence or cardinality, grades OQ-118
produced no number for. Confirmed in the radius, unmeasurable *in this arc* rather than unexamined.
Per RECON §4 this is the disposition of edges E1–E5, E7, E8, i.e. the classification layer.

### `SUSPECT-confirmed-and-survives` — admitting instrument: CONCLUSION-INVARIANCE across the frozen replicate draws. Nothing else opens this slot.

A grade restriction alone would leave the category vacant *and* reachable by the wrong route. At
name-identity grade the population result is 3/54 positive — a mechanism re-witnessed against the
field it keys on fails on the population read, so no sanctioned path produces a survival, while the
unsanctioned one (a 0-diff sensitivity result misread as survival) stays open.

The category survives because it points at something real: **claim-level invariance is not
field-level stability.** `shared_agent_link/4` joins on beneficiary/victim name sets and those names
do not reproduce — but if the claim resting on it concerns network *topology* (is the contamination
graph connected, does the component count hold), that conclusion can be invariant across draws whose
casts share no names. That is a genuine survival, measurable on artifacts already on disk.

**The instrument, as concrete actions:**

1. **Construct** a draw-slice corpus per draw index k over the frozen cohort-zero replicates in
   `audits/2026-06-12_cohort_zero/replicates/`: for each k, the set of story JSONs whose draw index
   is k. Do not regenerate; read what is on disk.
2. **Re-run the claim's own conclusion** — never the field — on each slice, and show the conclusion
   holds across slices.
3. **Bar.** Admits `-and-survives` **only** on conclusion-invariance. **Never** on field stability.
   **Never** on a sensitivity diff (`oq35` grades an edge; it clears nothing).
4. **Slice-size confound — declare and control it.** The slices are uneven: `organization_floor`
   has only d2/d3, so k=1 has 5 stories and k=2/k=3 have 6. A count-valued conclusion would differ
   from slice size alone and read as failure. **Either** restrict to the 5 stories present in all
   three slices, **or** use a conclusion invariant to n — state which, per claim, in the row.
5. **Two-sided control, mandatory.** The instrument must **hold** for a conclusion known to be
   draw-invariant **and fail** for one known not to be (a conclusion keyed directly on cast names is
   the natural negative). One-sided, it manufactures survivals.
6. **Reach limit.** A claim whose conclusion is not computable over the cohort-zero six lands
   `SUSPECT-unwitnessed` — which **does** count in the headline. The instrument's reach may not
   become a quiet route out of the radius.

**Fallback, pre-authorized:** if at execution the machinery is not worth carrying, **delete the
category** and let name-identity dependents land `-and-fails` or `-unwitnessed`. Either is
defensible; a slot with no door is not. Record which branch was taken.

---

## §(c) Headline rule (Amendment A) — arithmetic declared, since this number gets quoted downstream

    SUSPECT = confirmed-and-fails + confirmed-grade-unmeasured + unwitnessed

**`SUSPECT-confirmed-and-survives` is EXCLUDED — and the reason is the new instrument's, not the old
comparison's.** It is *not* excluded because the field held up against an OQ-118 comparison: §(b)
rules that route out by construction (at name-identity grade the population is 3/54 positive, so
nothing reaches this slot by holding up against OQ-118). It is excluded because **the claim's own
conclusion was shown invariant across the frozen draw-slices** — the claim does not rest on cast
stability at all. The exclusion is therefore a **dependency disproven at the claim level**, not a
dependency survived at the field level. Counting such a row would inflate the radius with rows the
instrument positively removed from it, exactly as Amendment A forbids deflating it.

It is reported as its own line beside the headline, never merged into either total. `cleared`,
`inert-unexercised`, `inert-unconsumed` are body figures, never the headline.

**Amendment A restated as the governing constraint:** the cheapness filter may not shrink the radius.
Headline the census on **SUSPECT count, never cleared count.** Anything skipped for cost is
`SUSPECT-unwitnessed`, a *distinct* disposition from `SUSPECT-confirmed-and-survives`.

---

## §(d) Five control pairs, pre-declared, each two-sided

Cast, verdict, closure, documentary, and conclusion-invariance (the last only if §(b)'s
`-and-survives` category is carried). **All must pass before any radius claim is made** — "the radius
is small" is otherwise a fact about the search.

**Discrimination altitude, declared:** the cast and verdict pairs are *naturally-arising population
members* (middle rung of the ladder), not authored decoys. **Check-don't-assume, executed:** OQ-190
responds to no defect commit, so **no free before-commit negative pair is available** — there is no
commit N at which the radius was defective and no parent N−1 at which it was clean. Stated here
rather than left unaddressed.

| Bucket | Must FIRE | Must DECLINE |
|---|---|---|
| Cast | `drl_purity_network.pl` `shared_agent_link/4` — contamination edges are literally the intersection of beneficiary/victim **name sets** across constraints | a consumer of an **apparatus-presence** row only (`boltzmann`/`network`/`interval` presence), lineage provably outside authored cast |
| Verdict | `commentary_census.pl` `q6` source → `outputs/commentary_census.json` | the **other** `commentary_census` sources (`extraction_reading`, `consensus`, `empty_chair`) — same file, same machinery, not verdict-keyed; a same-path decline |

**The cast decline control may NOT be drawn from the computed layer** (`dr_type`, `h1_band`, purity,
fingerprint). RECON §4 now shows this is load-bearing, not precautionary: the computed layer *is*
transitively cast-derived (E1–E8), so a sweep that declines on it would pass the control **by
exhibiting exactly the blindness the control exists to rule out** — certifying the instrument's worst
failure mode as a success. Apparatus-presence rows are the only control-eligible negatives, and they
are eligible because their lineage is established independently (and, per RECON §1a, they measure
18/18 positive-stable), not because they are presumed clean.

**Third control — the closure, tested in BOTH directions.** Transitive closures go everywhere by
default. A fire-only control passes perfectly while the closure tags half the repo T2b, and that
artifact is *indistinguishable from the RECON §4 headline this audit predicts* — there would be no
way to tell a true finding from an over-broad instrument.

| Closure arm | Target |
|---|---|
| Must REACH | a synthetic consumer that reads a derived value and contains no cast name |
| Must NOT REACH | a synthetic apparatus-presence-only consumer, lineage established outside authored cast |

**Both closure arms are SYNTHETIC, and the reason is declared rather than left to contradict §(d)'s
preference for natural members:** a natural T2b candidate's status is *decided by the graph under
test*, so using one is circular. Independent ground truth is available only by construction here.
Report the closure's discrimination at that lower rung (authored fixture).

**This chains into Limb 3.** Amendment B's exhaustion criterion includes the T2b closure, so an
**under-broad** closure yields a false `NO-and-exhausted`, the temp sweep stays reserved, and the
dependent claim hardens — the precise failure Limb 3 was priced to avoid. **Exhaustion may not be
asserted on an instrument tested in one direction only: if either closure arm fails, the verdict
bucket returns `reserved-pending` regardless of what the sweep found.**

**Fourth and fifth pairs — the documentary instruments are NOT covered by the two mechanical
controls.** 3c and 3d are different parsers over a different surface, and 3d is the half that answers
Limb 3.

- **Documentary FIRE (free, naturally arising, on the active surface):** OQ-75's own text says
  *"compare only draw-stable fields."* That is a known documentary dependent. **No doc-radius claim
  may be made until the doc sweep flags it.**
- **Documentary DECLINE:** an active-surface passage mentioning a cast field descriptively without
  resting a claim on its stability — the sweep must not flag it.

**Edge-admission pruner (applies at graph-build time, not adjudication).** An edge is admitted only
where the cast value can **change** the derived value, not merely where it is read. Unbounded
transitivity plus weakest-edge grade inheritance degrades everything to presence grade and lands the
whole repo in `SUSPECT-confirmed-grade-unmeasured` — true, unactionable, unclosable. The pruner is
the `python/audits/oq35_field_counterfactual.py` shape used at build time: perturb the source, diff
the derived value; **0-diff with a live positive control ⇒ edge NOT admitted.** Record admitted and
rejected edges both, with the diff that decided each. This decides what the graph *contains*, not
what a dependent's disposition *is*, so it does not cross the decide/generate split.

---

## §(e) Verdict-bucket exhaustion criterion (Amendment B)

The verdict bucket is **exempt from the cheapness filter**. One genuine dependent closes it
(un-reserve the spend); a NO requires **exhaustion**. Run to first-hit or to exhaustion; an
unexhausted verdict sweep returns `reserved-pending`, never `cleared`.

Exhaustion = the complete emitted-name consumer set for `disappearance_verdict/2`,
`founding_problem_status/2`, `founding_problem_corroboration_class/2` and
`has_mandatrophy_declaration/1` across `prolog/`, `python/`, `agent/`, **plus their T2b closure**,
plus the active-document surface and its forward-cited audit edges. Per §(0b) the previously named
hop `has_mandatrophy_declaration/1 → check_indexical_relativity/1` **does not exist**; that predicate
is a T5b terminal and its closure is empty. **A name-keyed sweep alone does not exhaust.** Anything
short of the above returns `reserved-pending`.

Cheap first-hit target, checked early: any active document or OQ quoting a `q6` `commentary_census`
number as a corpus fact. Note the census's `Σ buckets == n_corpus` invariant (`run_pipeline.py`) is a
**totality** check, not a stability check — a redraw redistributes buckets without tripping any
guard, exactly the shape that would otherwise read as safe.

---

## §(f) The Limb-3 outcome map, written before the run

| Outcome | Action |
|---|---|
| **YES** — a live claim needs verdict stability real | the reserved ~$1.5–2 temp sweep becomes load-bearing → **escalate to the operator as a spend-go; do not spend** |
| **NO-and-exhausted** | stays reserved; record the enumeration that priced it, in OQ-118 |
| **NO-but-unexhausted** | `reserved-pending`; OQ-190 closes scoped with the residue named |

---

## §(g) Claim scope

Code + report surfaces, plus **active** documents (`CLAUDE.md`, `AGENTS.md`, non-rolled-off
`KNOWN_STATE.md`, `docs/`, open ISSUES entries **and the resolution notes of resolved ones**).
`audits/` is **not swept**; instead sweep the active surface for **forward-citations into `audits/`**
and follow those edges — bounded by the active surface, not the archive. Record the edge list as its
own artifact (`promoted_audit_edges.tsv`).

**3d count reconciliation is mandatory before any 3d finding.** Reconcile the resolved-entry count
the extractor parsed against `python3 python/issues_status.py`'s own resolved count; a mismatch
**aborts** the 3d read. A `**Status:** resolved` variant that does not match the extractor's pattern
drops **silently**, and silent drop-out on the highest-yield surface is indistinguishable from a
small radius.

---

## §(h) What may NOT be concluded from the available instruments

- `oq35_field_counterfactual.py` tests **sensitivity** (cosmetic-or-sensitive). It does **not** yield
  survives-or-not. Correct use: separate T4/T5 from genuine dependents, and grade an edge. **A 0-diff
  from `oq35` may never, at any grade, move a row into `-and-survives`.**
- The OQ-118 Limb-4 graded distance metric **does not exist** (exact set-match conflates "fresh cast"
  with "renamed cast") and is **not** built here — Limb 4's own item, with its own control burden.
- Presence/cardinality-grade dependents **cannot be scored at all** in this arc, however cheap the
  counterfactual was. They land `SUSPECT-confirmed-grade-unmeasured`.
- Everything skipped for cost lands `SUSPECT-unwitnessed`.
