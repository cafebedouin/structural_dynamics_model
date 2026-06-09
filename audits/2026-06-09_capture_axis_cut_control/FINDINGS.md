# Step 1 capturer-cut discriminating control — FINDINGS (HALT)

**Plan:** `~/.claude/plans/jaunty-juggling-wozniak.md` (build the capture axis snare vs piton).
**Date:** 2026-06-09. **Pre-registration:** `PREREGISTRATION.md` (this dir, written before the run).
**Witness:** `step1_capturer_cut_control.out` (this dir). **Probe:** `capturer_cut_control.pl` (this dir;
must be copied to `prolog/` to re-run — it `[stack]`s and so resolves relative to `prolog/`).

## Verdict: Outcome 2 → HALT

The pre-registered three-outcome verdict (fixed by the plan):
1. cut TRUE on (a), FALSE on (b)&(c) → proceed to Step 2.
2. **cut TRUE on (a) AND (b)** → HALT; capture-as-receipt not computed-representable; needs an
   authored gain-flow surface. ← **this is what the run produced.**
3. cut FALSE on (a) → HALT, redesign.

**Steps 2–4 of the plan did NOT run** (gated on Outcome 1). The deliverable is the halt + escalation.

## The run (verbatim, from `step1_capturer_cut_control.out`)

```
CASE cap_a
    seat=capturer_a role=beneficiary | candidate | raw_metric=scaffold final_type=scaffold (favorable)
    seat=payer_a role=payer | not-candidate | raw_metric=snare final_type=snare (unfavorable)
  RESULT cap_a | candidate_set=[capturer_a] | cut=true

CASE mild_b
    seat=bystander_b role=beneficiary | candidate | raw_metric=naturalized final_type=naturalized (favorable)
    seat=payer_b role=payer | not-candidate | raw_metric=snare final_type=snare (unfavorable)
  RESULT mild_b | candidate_set=[bystander_b] | cut=true

CASE dmv_c
    seat=excluded_c role=excluded | not-candidate | raw_metric=snare final_type=snare (unfavorable)
    seat=payer_c role=payer | not-candidate | raw_metric=snare final_type=snare (unfavorable)
  RESULT dmv_c | candidate_set=[] | cut=false

CASE dmv_designed
    seat=admin_d role=agenda_setter | candidate | raw_metric=rope final_type=rope (favorable)
    seat=payer_d role=payer | not-candidate | raw_metric=snare final_type=snare (unfavorable)
  RESULT dmv_designed | candidate_set=[admin_d] | cut=true
```

## Reading the cases

- **(a) cap_a → cut TRUE** (predicted). Genuine capturer: the beneficiary seat reads favorable.
- **(b) mild_b → cut TRUE — the false-positive (predicted).** The load-bearing two-part witness
  (the sharpening's required check) is satisfied:
  1. **candidate membership TRUE** — `candidate_set=[bystander_b]`: the (b) seat *is* beneficiary-side
     and *entered* the cut's candidate set; and
  2. **cut TRUE** — fires *because favorable-dr_type succeeded* on a seat that is **not** a capturer
     (no `constraint_beneficiary`, merely unharmed).
  So the false-positive is genuine — the cut fired on a non-capturer that was a real candidate, **not**
  a no-candidate artifact misread as "false on (b)." This is what rules out the circularity the
  pre-registration flagged.
- **(c) dmv_c → cut FALSE, `candidate_set=[]`** (predicted). False *because the candidate set is
  empty* — the intended easy-case reason, witnessed (not a favorable-test failure).
- **(d) dmv_designed → cut TRUE** (predicted, supplementary). A realistic DMV (designed,
  agenda_setter present, **no** capturer) fires the cut TRUE on its agenda_setter — the cut mislabels
  an uncaptured designed constraint as captured. Reinforces (b).

## Why the cut cannot work (the structural reading the run witnesses)

`role_base_d` (config.pl:156–160) hands every beneficiary-side role a **low directionality**
(agenda_setter 0.12, beneficiary 0.25). χ = ε·sigmoid_f(d)·σ(S)
(`constraint_indexing:extractiveness_for_agent_d/4`) is **extraction *from* the seat's directional
view, not gain *to* the seat.** A low-d seat therefore computes low/negative χ → a favorable type
(rope/scaffold/naturalized) **whether or not the payers' extraction actually accrues to it.** The cut
degenerates into "does C have a beneficiary-side-*role* seat at all" — which is why (a), (b), and (d)
are indistinguishable to it, and (c) is the only false (no such seat). The false-positive is therefore
**insensitive to the `favorable/1` set**: the problem is upstream of the type label, in the
d-derivation. (Witnessed on these cases — under-claim: this says the cut false-positives on *these*
constructed mild-favorable seats, not "capture is unrepresentable everywhere.")

## Bonus finding — the one authored signal launders capture the *wrong* way

cap_a's beneficiary seat read **scaffold** while mild_b's *identically-constructed* seat read
**naturalized**. The only difference between the two constraints is the `constraint_beneficiary(cap_a,
capturer_a)` fact. The cause: `has_coordination_function(C) :- constraint_beneficiary(C, _)`
(narrative_ontology.pl:303). So the *only* authored fact that could mark capture
(`constraint_beneficiary`) feeds **coordination detection**, pushing the capturer's seat toward
**scaffold** — i.e. authoring a beneficiary makes the constraint look *more* like benign coordination
infrastructure, not more like capture. Both results are still favorable (cut TRUE on both), so the
verdict is unchanged — but it underscores that there is no authored *gain-flow* signal: the nearest
authored fact actively points the wrong direction for capture.

## Escalation (the deliverable)

Capture-as-receipt is **not computed-representable from current signals.** The snare/piton split needs
an **authored gain-flow surface** ("who the extraction accrues to") — the same gap-class as
`fixing_cost`/benefit-of-fixing (OQ-90). Landed as:
- **GAP-10** in `docs/design/design_gaps.md` (declared absence of a gain-flow / authored-receipt surface);
- **OQ-92** in `ISSUES.md` (the missing surface; proposes — for operator ruling — unifying the capture
  split and the `fixing_cost` question under one authored-receipt design; the unification is flagged as
  a design proposal, not asserted, since the two are distinct scalars sharing the receipt/accrual family);
- **OQ-90** updated: the `has_computed_capturer` proxy is the loose proxy the review flagged; Step 1
  HALTED it; the in-branch piton refinement (Steps 2–4) is gated on OQ-92.

**Operator ruling needed:** whether to add an authored gain-flow surface (and whether one surface
covers both capture and fixing_cost). Out of plan scope and out of this control's authority.
