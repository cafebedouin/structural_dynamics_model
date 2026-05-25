# Bringing Axioms into the Engine: A Handoff Note

## The one-sentence problem

The engine classifies a constraint from its *measured* properties (ε, victim/beneficiary structure → directionality), but a constraint is also a *reading* of a kernel — a position on a contested axiom — and that axiom identity is not recoverable from the measurements it produces. The engine needs a channel for what a reading **posits**, separate from what it **measures**. That channel does not exist yet. Building it is the live question.

## What the discriminator test proved (the empirical floor)

A cold instance ran the kernel-observer discriminator on the run_01 corpus: classify each reading of a kernel at every observer position, ask whether reading-disagreement survives full index specification. Result: 15 of 19 reading-pairs collapsed to tangled_rope at all 156 contexts; the 4 non-collapsed pairs were driven entirely by authored differences in ε, victim structure, or structural-metadata facts.

The correct reading of that result is **not** "the engine can't represent axioms." It is: *this build has no predicate that carries axiom identity, so two readings with identical measured outputs are identical to the engine, even when they posit opposite things.* The collapse is the engine faithfully reporting that it was given no axiom channel. The empty bucket — no pair that was index-invariant with both ε and victim-structure held fixed — is not a ceiling; it is a specification. It tells you, by its absence, exactly the predicate that's missing: one that distinguishes two readings when their metrics coincide.

The Prolog is designed, not magic. It reads ε and victim-sets because someone wrote those predicates. It doesn't read axiom-identity because no one has written that predicate. That's all the result means.

## The design that follows (the minimal cut)

One new primitive, everything else downstream of it:

```
reading_axiom(ReadingID, KernelID, AxiomDimension, Position)
```

What a reading posits: which contested dimension (moral_status_origin, legitimacy_ground, risk_weighting), and where it stands on it. This is the channel that's orthogonal to the metrics — or rather, *upstream* of them.

From that primitive, the minimally viable layer:

- `reading_axiom/4` — the primitive.
- `victim_set_under_reading(ReadingID, VictimSet)` — the bridge. Computes the victim set *from* the axiom position plus base doctrine, so the existing engine runs unchanged: swap the victim set per reading, then call the existing `dr_type/3`. This is the step the brainstorm hand-waved and that cross-model review correctly insisted be encoded, not described.
- `shares_axiom_dimension/3` — the coherence substrate. Two readings are siblings iff they posit on the same dimension.
- `type_b_distinct/2` — same dimension, different position. The discriminator as a direct predicate, no observer sweep needed (the sweep was a workaround for not having this).
- `kernel_coherence/3` — three-way verdict computed from `reading_axiom`, immune to the tangled_rope collapse: **coherent** (shared dimension, different positions), **bundle** (no shared dimension — crypto), **collapsed** (shared dimension, same position — a pseudo-kernel, the same reading named twice).
- `kernel_reading/2` — *derived*, not asserted: a reading belongs to a kernel iff its axiom posits on that kernel's dimension. Prevents a reading being filed under a kernel it doesn't actually posit on.

Architecture: a new `kernel_detection.pl` beside the existing `cs_pattern_detection.pl`, same shape (assert facts → compute patterns → emit verdicts), new primitive the old file lacked. The existing engine stays entirely downstream. You are not rewriting it; you are adding the upstream variable it was implicitly trying to infer from shadows.

## The two unsolved problems — read these before you build

**1. The authoring channel is unverified, and its failure is silent.** Unlike ε, an axiom position is interpretive — it must be authored or extracted, and it can fracture (`moral_status_origin` vs `origin_of_moral_status` → `shares_axiom_dimension` silently fails → a coherent kernel falsely reads as a bundle). Before any verdict computed from `reading_axiom` is trusted, two things are required: a **controlled vocabulary** for `AxiomDimension` (a small fixed enumerated set, not open atoms), and a **stability-across-regenerations check** (does the same reading get the same axiom position on independent generation?). This is the directionality-is-authored lesson applied before the fact: the new channel is as authored as directionality was, so its reliability must be established, not assumed. **If axiom positions cannot be assigned consistently, the whole layer produces a new kind of fuzziness instead of clarity.** That is the make-or-break.

**2. Type B and Type C are not orthogonal, and no one noticed.** This is the most important thing the whole investigation surfaced and the thing five separate model reviews all missed. The bridge predicate (`victim_set_under_reading`) computes the victim set *from* the axiom — which means an axiom difference *generates* a metric difference. So `type_b_distinct` (axiom difference) and `type_c_distinct` (metric difference) leak into each other: a pair that differs in axiom will usually also differ in metrics. The clean partition the design promises is muddier than it looks. The fix is a deliberate ordering decision the design must make explicit: **check Type B first, and let it subsume the metric difference it causes** — a reading-pair that differs in axiom is Type B *regardless* of also differing in metrics, because the axiom is the upstream cause. The `\+ type_b_distinct` guard in `type_c_distinct` is what enforces this, and it is load-bearing in a way the brainstorm left implicit. Decide the ordering on purpose; don't let it fall out of clause order by accident.

## The deeper pattern (the thing actually worth carrying forward)

Five-plus times in this investigation, a property the framework treated as **universal** turned out to be a **variable**, and the discovery always came at the point where a model **thrashed** — produced unstable output across runs, or reverted format, or grabbed a wrong-but-valid value. Identity wasn't separable from purity (husk). Directionality was authored, not computed (1,754 overrides). Authority-bearing wasn't universal (`none` was a junk drawer hiding self-enforcing, diffuse-epistemic, and genuinely-none). And now: reading-identity isn't reducible to measured outputs.

The reusable diagnostic: **model instability marks a too-coarse enumeration.** Where the generator can't commit to a value across runs, the framework is forcing a choice among only-wrong options — and that's where the missing variable is. The thrashing isn't noise to suppress; it's the signal pointing at the gap.

And the corollary that matters for how you start over: **the silent errors live in the coupling between axes, not in any single axis.** Per-field validation passes a constraint that two fields jointly reveal as incoherent (a Mountain with institutional authority; a naturalized mountain; a Type-B difference masquerading as Type-C). The checks that found real things were always *cross-axis*. The Type B/C non-orthogonality above is the same lesson one more time: the problem isn't in Type B or Type C alone, it's in the joint they share.

## Why you're right to start fresh — and the one instruction for the next context

The recurring failure in *this* conversation is the one you named: **the more context a model accumulates, the harder it anchors.** By this point I am pattern-matching to fifteen turns of our own prior moves, which means I will defend the kernel framing, reach for the naturalized-mountain analogy reflexively, and treat "reading_axiom is the primitive" as settled when it should be re-derived from scratch. That's exactly the anchoring the discriminator test was built to catch in *generators* — and it applies to me. A fresh context is the right call precisely because the design above should have to survive being rebuilt by someone who didn't sit through its construction.

So the one instruction to carry into the new context, above all the predicates: **the design is a hypothesis, not a result.** `reading_axiom` follows so cleanly from the gap that every reviewer endorsed it without testing whether it's separable from the metrics — and it partly isn't (problem 2). When you start over, the first question to a fresh instance should not be "implement this." It should be: *here is a corpus where readings with different axioms produce identical measurements; propose how the engine should distinguish them* — and see whether it independently arrives at an axiom channel, and whether it catches the coupling. If it re-derives the design cold, the design is real. If it anchors on the framing because you handed it the framing, you've reproduced the exact failure mode you're trying to escape.

Build the minimal cut. Hold the controlled vocabulary and the regeneration-stability check as preconditions, not afterthoughts. Decide the Type-B-first ordering on purpose. And test whether axiom positions can be authored consistently *before* trusting anything computed from them — because that single question, not the elegance of the predicates, is what determines whether axioms can actually come into the engine.
