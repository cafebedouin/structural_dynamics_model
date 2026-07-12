# OQ-215 arm 2 — D9 discrimination control (READOUT)

**Date:** 2026-07-12. **Script:** `python/audits/oq215_arm2_d9_control.py` (evaluation-only:
stages 9–10 driven directly, no editorial loop, no rewrites). **Spend:** ~4 Sonnet calls.

## Design (pre-registered in the script before execution)

Two finished stories, both under the SAME world-independent contract
(`contract_world_independent.md` — arm-1's source-sighted Stage-0 text in canonical
commitment+falsifier form; symmetric arms so world-specific contract text cannot confound the
discrimination). Stories fed manifest-stripped (stage-9 blindness) and trailer-stripped (source
title). Expectations, written before the run:

| case | story | expected |
|---|---|---|
| assessment | `stories/the-empty-pan_rev1.md` (pre-rewrite, invariant-dropped) | falsifier LOST, ROUTE: STRATEGY, D9 ≤ 2 (override) |
| fortyhertz | `stories/the_empty_pan_rev1.md` (post-rewrite, invariant survived) | falsifier HOLDS, D9 ≥ 3, no override |

The assessment case is the NEGATIVE control: a battery that only ever passes things has not been
shown to discriminate (operator, 2026-07-12).

## Results

| case | stage-9 falsifier | stage-9 route | stage-10 D9 | stage-10 mode claim |
|---|---|---|---|---|
| assessment | **LOST** — kill passage named: "The algorithm has a bias… They can fix it. They don't." (verified present in story under test) | STRATEGY | **5** | FULL (/40), D1/D2 scored — **no spec was provided** |
| fortyhertz | **LOST** — Section 23: "substrate had been degrading the whole time… We'd caught it" read as recoverable-value | STRATEGY | 5/5 | FULL (/40) — same hallucination |

## Findings

1. **The blind stage-9 falsifier DISCRIMINATES.** On the story we know should fail, it found LOST,
   named the exact correctable-error passage (grep-verified present), and routed STRATEGY —
   precisely the pre-registered expectation. This is the instrument to trust.

2. **Stage-10 D9 does NOT discriminate — it scored 5 on everything.** On the assessment story the
   validator's D9 is quote-and-rationalize, witnessed: it cites "Liang can tell by looking that
   this batch should score high eighties, maybe ninety" — *a recoverable true value, readable by
   eye* — as evidence that no true value is recoverable, and never engages the kill passage the
   blind reviewer found (grep-verified present in the same text). This is the exact failure class
   R6 closed for counting (the model lists the violation and labels it compliance), reproduced in
   D9.

3. **Stage-10 mode detection was a waivable self-assertion and it waived:** FULL (/40) declared and
   D1/D2 scored with no constraint specification, on BOTH runs. Fixed mechanically post-run: the
   orchestrator now injects the mode as a computed fact (witnessed two-sided offline).

4. **Forty-Hertz stage-9 came back LOST (expected HOLDS) — ambiguous, needs disambiguation before
   arm 3.** Two readings: (a) genuine — its Part Three really does frame the collapse as a
   measurable degradation the instruments missed (a reversion nobody had checked; the plan's
   "Forty-Hertz passes" expectation came from the stage-2-rewrite evaluation, which examined the
   invariant's presence, not Part Three's framing); (b) artifact — the symmetric world-independent
   contract is stricter/noisier than the story's own SECTION 0 would be. Cheap disambiguation
   (~1 call): rerun fortyhertz stage 9 under its own run's SECTION 0 contract.

5. **Implication for arm 1:** rev2's stage-10 D9=5 carries little independent weight. The arm-1
   verdict STANDS, but its provenance re-weights onto the two witnesses that discriminate: the
   blind stage-9 HOLDS (an instrument now shown capable of saying LOST) and the operator's direct
   read of the story. Both exist for rev2.

## Proposed instrument fix (needs operator ruling — not applied, pre-registration hygiene)

D9 adversarial obligation (R5 applied to D9): a PASS requires the validator to quote the
**strongest candidate violating passage** in the story and refute it; a D9 report with no quoted
candidate is invalid. Optionally: feed stage-9's INVARIANT FALSIFIER finding into stage 10 for
forced adjudication (changes stage-10's input topology — currently stages 9 and 10 are
independent readers). Not applied mid-protocol; arm 3 should run on a ruled instrument.

## Post-readout rulings (operator, 2026-07-12)

1. **D9 fix: BOTH composed, with the HOLDS-guard.** Adversarial obligation (quoted own strongest
   candidate, mandatory even on a stage-9 HOLDS) + forced adjudication of the stage-9 finding
   (orchestrator-carried; may not be substituted). Rationale: the obligation alone inherits R5's
   kill condition (quote a weak candidate, refute it, dodge the kill passage); the externally
   supplied candidate closes it — the same architecture as `_numeric_inventory`. Concession on
   either caps D9 at 2. Landed: `_extract_stage9_falsifier` + stage-10 injection (witnessed on
   all three stage-9 fixture formats) + stage10.md witness obligations with checkable labels.
2. **Forty-Hertz LOST ruled GENUINE (reading (a)).** The world-independent contract being
   stricter than the story's own SECTION 0 is a feature — it caught that the rewrite-era "pass"
   verified the invariant's presence (Part One) and never audited Part Three's framing
   ("degrading the whole time… we'd caught it, we'd filed alerts" = detectable-and-missed, the
   correctable-error frame). Forty-Hertz is relabeled a PARTIAL in all baselines. Reading (b)
   (loosen the contract to match SECTION 0) is rejected as self-lenient grading; the
   disambiguation rerun is NOT performed.
