# OQ-218 Stage 1 — Adjudication (operator ruling, 2026-07-12)

**RULING: Stage 1 = repair confirmed, both legs. The gate opens; the fresh-source batch is
released.** (Operator, endorsing the Claude-web adjudication in full: "Agree with Claude web.
This is the ruling.")

## Assignment verification (key-independent)

Before adjudicating, the operator verified A/B assignment from quote-provenance alone, without
opening AB_KEY.md: Sonnet's B-candidate ("The frustration isn't despair...") is the exact line
the manifest CUT, so it can only exist in the seed; every A-quote (the boots line; "Self-report;
not a measurement; not entered."; the notebook grant) is a witnessed addition in the improved
file. A = improved, B = seed, confirmed independently. The blind wall held; the labels are right.

## The pre-registered kill condition did not fire — strongest form

Kill condition was "arms name nothing" (violation legible only to vocabulary-owners,
F-PROCESS-VOUCH's subtlest form). Instead: Sonnet named the target_prior blind, in nearly the
pre-registration's own words ("a second, deeper expectation: that a *fixed* or unbiased version
of the same instrument would be adequate at all") and cited both carriers (sensor scene,
notebook grant) as the mechanism. A cold reader cannot recover the pre-registered sentence from
a story that doesn't execute it. Same signal class as arm-1's stage-9 reviewer spontaneously
articulating the true-grain distinction.

## Finding: the legibility gradient (with its caveat, priced not dodged)

The arms return a gradient — **named** (Sonnet: full articulation), **felt-and-quoted** (Gemini:
never articulated the falsifier layer, then picked the falsifier line itself as its Q2
inimitable sentence), **absent** (neither). The break layer's own theory predicts this: breaks
are reader-seat-indexed, so legibility MUST vary with which priors a reader holds live; a break
that landed identically for every reader would be a shallow one.

**Caveat (the unflattering reading, carried):** possibly the second layer is simply too quiet
and Sonnet is just the stronger reader. Cannot be fully excluded — but the quietness was
purchased deliberately by the transparency gate (aphorism cut; grant in smaller letters; one
register felt rather than three explained), and partial legibility is that gate's known cost,
paid where the design said it would be. Making the layer louder means diagramming it, which
fails the other gate. Logged as consistent-with-theory, caveat attached; Stage 2's arms
accumulate the distribution.

## Two data points stronger than the headline

1. **Cut-ratification.** Sonnet reached for a Q2 candidate in the seed, landed on the exact
   line the improvement cut as a stage-10-named flinch, and rejected it for the same reason the
   manifest gave ("a common explain-the-emotion template" — competent, not singular). The
   subtraction and a blind reader's taste converged on one sentence from opposite sides of the
   wall: the protocol's cut-discipline validated by an instrument that never saw the manifest.
2. **Contested-keep frontier.** The improver flagged exactly two keeps as closest calls (the
   boots line; the grant); the two arms each independently selected a different one of those two
   as the single sentence no other model writes. The substitution control's decision boundary
   sits precisely on the inimitability frontier — its hardest calls are the highest-value lines.
   The operator_estimate field earned its keep: both beats rated "low probability the median
   model produces this" are the ones cold readers found inimitable.

## Legs

- **Contaminated §1a (operator):** PASS — grain re-founded by category; falsifier granted and
  shrugged at; R3 (second-fail record, weakest block) overturned to KEEP.
- **Blind arms:** PASS at the pre-registered bar — one full naming, one felt-detection, zero
  seed-preference, Q4 discrimination from both arms.

## Instrument note for Stage 2 (binding on the arm design)

Q1 as phrased evaluates stories independently, so layer-separation only surfaced via Q4's
comparative frame. Keep Q1 as-is (leading the witness would be worse); treat **Q4 as the
discrimination instrument** it turned out to be.

## Stage 2 model pinning (operator instruction, 2026-07-12 — dissolves the R12 confound)

The Sonnet 4.5 → Sonnet 5 migration (commit `c23319d1`) landed between Stage 1 and Stage 2,
which would have bundled a model change into the batch (the R12 class: model A/B never bundled).
**Operator instruction: run Stage 2 with the respective model — the batch pins all ten Anthropic
stages to `claude-sonnet-4-5-20250929` via the per-stage `--stage-N-model` overrides.** The
generator matches the Stage-1 seed era exactly; the retrospective question stays cleanly
askable; the 4.5-anchored density baselines (37.6/47.6 defect band, threshold 10.0, ~0.5
improved ceiling) remain valid for this batch. The Sonnet-5 re-baseline happens on ordinary
pipeline use, separately.
