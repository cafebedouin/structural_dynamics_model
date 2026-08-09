# PREREGISTRATION — OQ-262 severance/intrinsicness audit

**Status: DRAFT — AWAITING R2 SIGN-OFF. Frozen at the operator's signature (§K);
after sign-off this file is never amended, only quoted.**

Authored: 2026-08-09, after Phase A recon (see §G provenance declaration).
Plan of record: `~/.claude/plans/review-oq-262-from-issues-md-ancient-stonebraker.md`
(operator-approved, amendments A1–A5 + riders 1–3 binding).

---

## A. Mutation definition (two-axiom readings)

A **mutation** of a reading is any of:

- **DROP** — an authored axiom (or, where a file authors no axioms, the commitment
  carried in the file's own prose) must be dropped or demoted from assertable to
  non-assertable for the pair to be jointly tenable.
- **SUBORDINATE** — an authored commitment must be re-ranked as subordinate to the
  other reading's commitment in a domain where both claim to govern, and the file
  itself does not author that subordination.
- **RESTRICT** — an authored reference frame or commitment must be confined to a
  narrower domain than the file authors.
- **OVERRIDE** — an authored grounding type must be re-read as a different kind
  (e.g., a deontological commitment treated as merely instrumental).
- **ADD** — joint tenability requires a bridging commitment (meta-principle, domain
  partition, reconciliation clause) that **neither** file authors.

**Not a mutation (mere emphasis):** a joint statement that foregrounds one reading's
vocabulary while every authored commitment of both readings remains fully assertable
as written.

**Thin-base rule:** each fiat reading authors exactly 2 axioms + 1 reference frame.
When a call's load-bearing step depends on content not present in the authored facts
or the file's own prose (e.g., cross-sibling analogy, background knowledge of the
debate literature), the call is a *reading of the text*, not a measurement — it is
marked **INFERRED** (§C) and survives only at that footing.

## B. Verdict classes (per pair; the annotation is the output, not the pass/fail)

- `genuine(zero_mutation)` — both readings' full authored commitments are jointly
  assertable as written.
- `severed(<what>)` — tenability requires DROP / SUBORDINATE / RESTRICT / OVERRIDE of
  a named commitment; the annotation names the commitment and which reading loses it.
- `intrinsic(<what>)` — tenability requires ADD; the annotation names the added content.
- `severed_and_intrinsic(<dropped>, <added>)` — the minimal route requires both.
- `undetermined(<why>)` — the authored base is too thin to decide; the annotation
  names what is missing. Carried as its own class, never folded into the others.

**Minimality:** where multiple repair routes exist, annotate the route touching the
fewest authored commitments; ties annotated as alternatives in the mutation text.

**Structural class `miscoded_asymmetry`** — a pair whose relation profile is
coexists_with in one direction and forecloses in the reverse. **Pinned (operator
rider 2): this flag does NOT terminate the row.** The pair still carries a
coexists_with claim in one direction, so it is ALSO judged for severance/intrinsicness
on that claim; the structural flag rides alongside the judged verdict in the same row.

## C. Footing: RULED vs INFERRED

**RULED** requires an in-file witness quote for every load-bearing step: an axiom
atom, contradiction-rationale prose, or the reading's own gain/directionality text.
Cross-sibling analogy alone = **INFERRED** (standing cross-sibling footing rule,
`build_discipline.md`). The footing is carried on every row and never aggregated away.

## D. Mechanical tier (Prolog probe, Phase C step 2)

All edge access through `cs_kernel_registry:cs_edge_target_member/4` /
`kernel_pair_edge/5` — never raw `cs_reading_relation` target match.

- **M1 contradiction-join:** pair {C1,C2} of kernel K flags `severance_candidate` iff
  the pair carries a coexists_with edge in ≥1 direction AND ∃ axioms a of C1, b of C2
  with `cs_axiom_contradiction(a, b)`.
  - Must fire (fiat): empirical_precedent_reading | empathy_simulation_reading.
  - Must NOT fire (fiat): truth_procedure_reading | utopian_fiction_reading
    (contradiction declared but no coexists edge — negative control).
  - On kernel_test (arm B only): must fire categorical_abolition | deterrence_instrument;
    must NOT fire retributive_desert | categorical_abolition (contradiction declared
    but the pair is mutual forecloses, not coexists — second negative control).
- **M2 relation-asymmetry:** pair flags `miscoded_asymmetry` iff coexists_with in one
  direction AND forecloses in the reverse.
  - Must fire (fiat): exactly empirical_precedent→utopian_fiction and
    predictive_synthesis→utopian_fiction; must NOT fire on symmetric or
    influences-asymmetric pairs. Must fire nowhere on kernel_test (recon: all CP pair
    profiles symmetric).

Mechanical flags are row annotations feeding the judged tier — never verdicts.

## E. The CP control (blind, pre-registered)

**Gate (HALT-bearing):** the 3 pairs of kernel `state_execution_authority`
(abolition | retributive, abolition | deterrence, retributive | deterrence) are each
expected **NONZERO mutation** (severed / intrinsic / severed_and_intrinsic). This is
a pre-specified k=3 unanimity criterion (OQ-264 standard-#3 shape) — the sole,
pre-registered exception to the no-rate rule (§I; amendment A4).

**Outcome semantics (written before the run):**
- 3/3 nonzero → control passes → fiat judging proceeds.
- Any `genuine(zero_mutation)` → check miscalibrated; fiat verdicts do not stand;
  HALT recorded; writeup as HALT (valid closure input — grammar returns for
  redefinition under a fresh prereg).
- Any `undetermined` → control not discharged; HALT-pending; cause classified
  (thin-base vs grammar under-specification) per the kill-condition
  cause-classification rule. Fail-closed either way: fiat verdicts are not produced.

**Blind mechanism:** a clean-context read-only subagent receives ONLY the three
kernel_test `.pl` files + the instrument text (§J) — no divergence numbers, no fiat
verdicts, no OQ-261/OQ-262 context, no expected directions. Its verdicts + mutation
text + quotes are recorded verbatim in `READOUT_cp_blind.md` BEFORE any comparison
with expectations.

**A5 leak check (pre-launch, literal):** read §J exactly as the subagent receives it
and confirm no CP pair name, kernel name, or expected direction survives in it.
Performed and its result pasted before launch.

**Rider 3 asymmetry (recon-confirmed):** the triplet authors ZERO axioms
(`recon_kernel_test.log`: `axioms: NONE AUTHORED` ×3), so the blind judge works from
a strictly thinner evidence base (frames + story prose) than the main instance has
for fiat — the control is calibrated on a different (easier-or-harder, unclear which)
problem. The writeup carries this; CP 3/3 is NOT directly transferable footing for
the fiat verdicts.

**Arm decision (R2, §K):**
- **Arm A** — triplet only (the plan's pinned control).
- **Arm B (recommended)** — triplet PLUS the axiom-bearing kernel_test family
  `state_killing_authority` (found in recon §4), judged by a second blind subagent
  under the same instrument (its three files only):
  - categorical_abolition | deterrence_instrument (mutual coexists + declared
    contradiction): expected **NONZERO**, joining the gate → k=4 unanimity.
  - deterrence_instrument | retributive_desert (mutual coexists, no declared
    contradiction, two compatible pro-execution justifications with no rival
    exclusive-location form): expected **`genuine(zero_mutation)`** — the two-sided
    zero-mutation control the fiat family cannot supply (§F). A nonzero verdict on it
    is reported against this pre-registration (a strike against the grammar's
    negative power); NOT HALT-bearing (the HALT rides only on the expected-nonzero
    gate — both-direction semantics declared). An `undetermined` leaves the `genuine`
    class uncalibrated (§F posture), also not HALT-bearing.
  Provenance (A2 discipline): this arm was composed AFTER recon, BEFORE any verdict,
  and enters the freeze only by the operator selecting it at R2.

## F. The zero-mutation control (amendment A1) — branch selected from the plan's
pre-drafted templates, blanks filled from recon only

**Branch 2 (selected):** "No credible zero-mutation candidate exists in the fiat
family (recon basis: five of six foundational axioms are rival exclusive
location/requirement claims on the same quantity —
`efficacy_located_in_knowledge_production` / `_participant_psychology_not_enactment` /
`_subjective_ontological_break` / `_process_rigor_not_outcome_realism` /
`efficacy_requires_documented_causal_chain` — and the sixth reading authors
`efficacy_question_bracketed_not_answered`, in authored-atom tension with any reading
that answers the question; the family already reads `real_closure`, H1r=2
(`recon_live.log`)). The `genuine` class is UNCALIBRATED this audit; any `genuine`
verdict is reported as an uncontrolled-class outcome and the writeup carries the
scoping."

Under arm B, the §E expected-genuine CP row partially lifts this: if it passes, the
writeup language is pre-committed as "`genuine` calibrated on the CP substrate,
uncalibrated on fiat"; if it fails or is undetermined, `genuine` stays uncalibrated
everywhere.

## G. Provenance declaration (amendment A2 — declare, don't launder)

This grammar was authored AFTER the Phase-A per-pair commitment inventory was read —
including the flagged contradiction pair (empirical_precedent | empathy_simulation)
and the two asymmetric utopian pairs. The R2 gate fixes the criterion before
*verdicts*, not before *substrate knowledge*. In particular, M1/M2's named
fire/no-fire targets (§D) were specified against pairs already known to satisfy them:
those controls test IMPLEMENTATION, not discrimination.

## H. Pre-committed altitude branch (amendment A3 — written before any verdict table
exists)

If RULED rows are a minority of the fiat table, the writeup's verdict altitude drops
to "a reading of the authored text under a frozen grammar," not an audit result, and
the annotations are cited as interpretive.

## I. No-rate rule (amendment A4)

No share/percentage observable exists over the deliverable per-pair table — verdicts
are per-pair witnessed calls, never "n of 13 genuine." (This discharges OQ-264
clause 2 by construction: no denominator is minted.) The CP unanimity gate (§E) is
the sole, pre-registered exception: a k=3 (arm A) / k=4 (arm B) unanimity criterion
over pre-named rows, which is a control gate, not a corpus statistic. The writeup
states this distinction so it does not read as violating its own rule.

## J. Blind-judge instrument (verbatim; the ONLY task text the CP subagent receives
besides the file contents)

> You are given several Prolog story files. Each file authors one "reading" — a
> commitment structure with (possibly) axioms (`cs_axiom`, `cs_axiom_status`,
> `cs_axiom_grounding`), a reference frame (`cs_reference_frame`), typed relation
> edges to sibling readings (`cs_reading_relation(UID, Target, Type)`), and
> narrative/metric content.
>
> For every unordered pair {A, B} of the given readings connected by a
> `coexists_with` edge in at least one direction, answer: what is the MINIMAL
> mutation of A and/or B required for the two readings to be jointly tenable — held
> simultaneously by one coherent framework?
>
> A mutation is any of: DROP (an authored axiom — or, where a file authors no
> axioms, a commitment carried in the file's own prose — must be dropped or demoted
> from assertable to non-assertable); SUBORDINATE (an authored commitment must be
> re-ranked below the other reading's in a domain where both claim to govern, where
> the file does not author that subordination); RESTRICT (an authored reference
> frame or commitment must be confined to a narrower domain than the file authors);
> OVERRIDE (an authored grounding type must be re-read as a different kind); ADD
> (joint tenability requires a bridging commitment — meta-principle, domain
> partition, reconciliation clause — that neither file authors).
>
> NOT a mutation (mere emphasis): a joint statement that foregrounds one reading's
> vocabulary while every authored commitment of both readings remains fully
> assertable as written.
>
> Verdict classes: `genuine(zero_mutation)` | `severed(<what>)` | `intrinsic(<what>)`
> | `severed_and_intrinsic(<dropped>, <added>)` | `undetermined(<why>)`. Where
> multiple repair routes exist, annotate the one touching the fewest authored
> commitments.
>
> For each pair output one row: {pair, verdict class, mutation text (one to three
> sentences stating the actual minimal mutation — write it so a different judge
> could re-evaluate it), witness quotes (verbatim text from the files grounding the
> call), footing: RULED if every load-bearing step cites in-file text, INFERRED if
> any step relies on analogy or content the files do not carry}.
>
> The annotation is the deliverable, not the pass/fail.

## K. R2 sign-off (the freeze)

The operator signs BEFORE any verdict is produced. The freeze is the seat — the
criterion is fixed before the instance defining it sees which verdicts it yields.

- [ ] Arm A (CP triplet only) or [ ] **Arm B (triplet + state_killing_authority,
  recommended)** — select one.
- [ ] Signed as written (any edit before signing is fine; after signing, none):

**R2 sign-off:** ____________________ (operator, date)
