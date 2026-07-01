# Two-Axis Architecture: Observer and Committer

**Purpose:** Architectural note recording a decision that had, until now, been made by omission: that the commitment-systems (CS) layer and the observer-axis classification machinery are *separate axes*, deliberately decoupled. As first written this document claimed they were joined at exactly one bridge; that claim was superseded by the cross-axis inventory and the mediator-layer decision — see the dated amendment below. This document fixes that reference frame so the rest of the system can be reasoned about as drift from it. It explains what the two axes are, why they must not be unified, the one empirical result that turns "should be separate" from a preference into a constraint, and the gaps that remain open by design rather than by accident.

**Audience:** Whoever reads this codebase next — most likely a later version of the person who wrote it. It assumes familiarity with the Deferential Realism (DR) framework (the observer axis) but explains the committer-axis vocabulary as it goes.

**Status:** This is the companion to the [two-hub note](#relation-to-the-two-hub-note), one level up. The two-hub note explains why the *observer* axis has two independent internal mechanisms that should not be merged. This note explains why the *whole system* now has two independent axes that should not be merged. It is the same argument recurring at a higher scale, which is itself the main finding.

---

## Why this document exists at all

The CS layer was not designed alongside the observer axis. It grew beside it. Kernels, readings, the axiom engine, the drift engine, the typed reading-relations — each was added to answer a specific question, and at no point was there a recorded decision about how this new machinery relates to the existing purity network, cohomology, and classification pipeline. The relationship was left implicit.

That is, in the vocabulary of the system itself, **unmarked drift**: an architectural mutation that accumulated without the structure ever naming it. Three independent surveys of the codebase — one of the observer-diagnostics, one of the network properties, one architectural — converged on the same observation: the CS layer sits *beside* the observer-axis machinery, the two are mutually blind in several places, and where they ought to agree they sometimes contradict each other with nothing surfacing the contradiction.

The honest response to discovering unmarked drift is not to write a document that pretends the architecture was always coherent. It is to **mark the drift** — to declare the reference frame explicitly, so that from here forward the relationship between the axes is a decision on the record rather than an accident in the code. That is what this document does. Everything below is the marking.

### Amendment (2026-06-09): the bridge is unblessed; the mediator layer is the decided join

This document's original central topological claim — that the axes are "joined only by the
`influences`-entailment bridge through `drl_composition`" — is false of the live code and was
false when written. The Tranche-2 cross-axis inventory (ISSUES.md OQ-15) found **16 distinct
cross-axis surfaces threaded through 7 modules in both directions**; the blessed bridge was one
of them, not the only one. The decided architecture — recorded here per this document's own
mark-the-drift rule; until this amendment the decision existed in conversation only (OQ-14):

- A third layer, neither CS nor DR (the **comparison/mediator layer**), becomes the sole
  sanctioned reader of both axes. Both axes become read-only sources; the mediator writes only
  to JSON output.
- Three grep-enforceable invariants: no axis reads the other; no axis reads the mediator; only
  the mediator may read both.
- `influences` → `detect_necessity_inheritance` is thereby **unblessed**: it stops being the
  privileged junction and enters the mediator like any other cross-axis read.
- The 16 inventoried surfaces triage into three buckets: genuine comparisons (→ mediator);
  substrate-level story-field readers in `cs_pattern_detection.pl` (→ a named substrate layer,
  since they read the shared authored input, not the other axis's outputs); and the
  `constraint_neighbors/3` exclusion-filter case (decision per item).

The mediator is **decided but not built** (OQ-15, open). Until it exists, every cross-axis read
is a nominal boundary violation that happens to be behaviorally clean — the inventory found zero
back-channel violations (no module asserts facts the other axis reads at runtime). The sections
below that stated the single-bridge topology have been edited to match this amendment; the
document's core argument — that the axes must not be *unified* — is untouched, since it never
depended on the join being singular, only on the join being narrow, named, and sanctioned.

A note on ordering, because it matters and it is the same shape as the thing being documented. The detection-independence result (below) is, logically, *prior* to the decision to keep the axes separate — the math implies the architecture. But it cannot be *written* in that order, because at the moment of writing, the decision has not yet been made; there is no fixed frame for the math to be "prior to." You have to declare the reference frame before you can state what is downstream of it. This document fixes the frame. A later theory treatise can state the math as prior, because by then the frame will exist. Logical priority and authoring priority run in opposite directions here, and conflating them would produce a clean theorem resting on an undocumented choice. This is the t0-fixing problem the CS layer itself describes — you have to decide which reading of the kernel you are in before you can measure anything as drift — applied to the act of writing about the system.

---

## The two axes

**Observer axis.** Given a fixed constraint, how does its classification vary with the position of the observer? This is the original DR machinery: the power-scaled extraction sigmoid, the effective-immutability table, the directionality derivation, the cohomological obstruction H¹ that measures how badly the classifications-across-positions fail to glue into a single coherent answer. The observer axis answers: *the same constraint looks different from different seats — how, and how much?* Its central diagnostic object is **perspectival incoherence** — disagreement across observer positions about what a fixed thing is.

**Committer axis.** Given a contested commitment, what are the distinct readings of it, what foundational axioms does each reading rest on, are those axioms mutually compatible, and where is each reading drifting over time? This is the CS layer: kernels (the contested commitment), readings (position-indexed interpretations of the kernel), the axiom engine (foundational commitments and their contradictions), the drift engine (t0 reference frame → t1 observed state → t2 computed terminal attractor), and the typed reading-relations (`forecloses`, `coexists_with`, `influences`). The committer axis answers: *which commitment exists to be looked at, out of what space of alternatives, resting on what premises, drifting which way?* Its central diagnostic objects are **axiom conflict** (two readings hold contradictory foundational commitments) and **drift trajectory** (where continued drift along a measured vector terminates).

These are different questions about different things. The observer axis holds the commitment fixed and varies the seat. The committer axis holds the seat fixed and varies the commitment. They are, in the most literal sense, orthogonal coordinates: one indexes by position, the other by which-kernel-and-which-reading.

---

## Representation grounding: which store is authoritative per metric (OQ-40)

The two axes are not only conceptually distinct; they read *different Prolog stores* for the same metric, and this is by design. A single metric — `extractiveness`, `base_extractiveness`, `suppression_requirement` — is authored in two representations that can legitimately carry different values:

- **`constraint_metric/3` is the authoritative scalar / observer representation.** It is the single per-constraint scalar the observer-axis machinery reads: `drl_core` consumes it (e.g. `base_extractiveness/2` at `drl_core.pl:85`, which delegates to `constraint_data:base_extractiveness/2`), and the static classification path (`classify_from_metrics/6` and everything routing through it) is grounded on it. When the observer axis asks "how does this fixed constraint classify from each seat," the fixed value it holds is the `constraint_metric` scalar.
- **`measurement/5` is the temporal / committer representation.** It is the time-indexed series `measurement(MeasId, Constraint, Metric, Time, Value)` that the temporal machinery reads: `classify_at_time` (`drl_composition`) and the metric-drift/`drift_events` path consume it to answer "where is this reading at time t, and which way is it drifting." When the committer axis asks about drift, the object it varies over is the `measurement/5` series.

**The split is intended, per metric — not a defect to reconcile.** The scalar and the temporal series answer different questions (a seat-invariant level vs. a trajectory), and forcing one representation to serve both would flatten exactly the distinction the two-axis architecture exists to preserve. This records the 2026-06-24 ruling on OQ-40 census rows 19–20 (`audits/2026-06-24_oq41_basex_t0/`): the `base_extractiveness` scalar-vs-temporal split is the two-axis design working as intended.

**The live correctness obligation that rides with the split.** Because the representations are independent, a constraint may author a metric in the temporal store *only* — with no scalar counterpart. The 2026-06-24 audit found ~15 live constraints that author `base_extractiveness` as a `measurement/5` series at real historical times with **no scalar `constraint_metric`**, so their series is their *only* authoritative ε. For that temporal-only family, an off-grid temporal query (asking `classify_at_time` at a synthetic time like `Time=0`, before the story's authored grid) is not an edge case but the main path — and answering it with a fabricated default *or* a fail-closed `unknown` both destroy authored signal (the reverted rows 24–25 fail-close erased a real `settler_colonial`/`cultural_zionist` divergence). So any temporal consumer must resolve an off-grid `Time` to an on-grid value rather than impute or fail-close. This obligation is tracked in the OQ-83 / OQ-195 temporal-grid family; a second instance — a temporal *gate* (`compute_temporal_stability`) that reads the *scalar* store instead of `measurement/5`, so it never sees the series at all — is spun out as OQ-201.

---

## Why they must not be unified

The instinct — the same instinct the two-hub note had to argue against one level down — is to fold the committer axis into the observer axis: to treat the typed reading-relations as edges in the purity-contamination network, so that the whole system is one connected graph with one notion of structural influence. This would be wrong, and the reason is the same reason the two hubs of the observer axis should not be merged: **the mechanisms have incompatible mathematical characters, and their independence is what produces the diagnostics.**

The purity-contamination network assumes scalar flow: contamination moves from low-purity neighbors to high-purity targets, monotonically, continuously, with no categorical type-overrides. The typed reading-relations do not have that character:

- `forecloses` is logical preemption — one reading makes another structurally impossible. That is a discontinuous, categorical override, not a gradient flow. It cannot be expressed as a contamination weight without lying about what it is.
- `coexists_with` is explicit non-domination — two readings are both live and neither displaces the other (this is the licensed-plurality signature). Its contamination weight is *zero by definition*; encoding it as an edge in a flow network asserts a flow that the relation specifically denies.
- `influences` is entailment-based necessity inheritance, and it is *already* consumed — by `detect_necessity_inheritance` in `drl_composition`, for mountain-axiom structure. Reusing it as a contamination edge would double-count it.

Forcing these into the contamination model would corrupt the network's semantics and, concretely, would invalidate the fixed-point convergence proof the network relies on — that proof assumes contamination only flows downward in purity and never forces a categorical type change, and `forecloses` carries exactly the kind of influence the proof excludes. So the separation is not a matter of taste. It is that the algebra of the committer-axis edges is incompatible with the algebra the network assumes. **Feeding them in would not be integration; it would be a category error.** This is precisely the displaced-beneficiary mistake — inferring a semantic claim from an edge that does not encode it — that the build already learned to avoid once, now seen at the level of whole subsystems.

The `influences` → `detect_necessity_inheritance` junction was originally the one *blessed* bridge — the single defined junction where committer-axis structure feeds observer-axis machinery. Per the 2026-06-09 amendment above it is no longer privileged: it is one of the 16 inventoried cross-axis reads, all destined to route through the mediator layer. Its narrowness — it carries entailment, which both layers understand, and nothing else — remains a design virtue; what changed is that narrowness no longer confers sole-sanctioned status.

There is also one *accidental* coupling that should be severed regardless of any other decision: `shared_agent_link` generates contamination edges between any two constraints that share victim/beneficiary classes, and all readings of a single kernel share those classes (the abolition, deterrence, and retributive readings of capital punishment all involve the same social agents). So the network was silently generating weak contamination edges between sibling readings — edges that already have explicit, typed `cs_reading_relation` coverage, now expressed twice: once intentionally and once as noise. The fix is a one-line filter excluding intra-kernel pairs from the shared-agent calculation. This is not an architectural choice; it is removing spurious structure that contradicts the separation. (Applied.)

---

## The result that makes separation a constraint, not a preference

Everything above is an argument from the *character* of the mechanisms. There is also an empirical result that makes the separation a fact about the domain rather than a design choice — and it came out of a prediction that failed, which is worth stating plainly because the failure is the finding.

The prediction was a clean parallel: the two-hub note found that observer-axis *hub-conflicts* (constraints where the continuous extraction mechanism and the discrete immutability mechanism pull classification in opposite directions) cluster at cohomological band H¹ = 4. If the committer axis were the same principle recurring, then committer-axis "false stables" — readings that are structurally foreclosed but appear stable — should likewise cluster at some characteristic H¹ band.

They do not. A diagnostic (`cs_drift_mismatch`) was built to find readings that are simultaneously metric-stable (no detectable network drift) and CS-foreclosed (the drift trajectory terminates at axiom_foreclosure, or the foreclosure-routing predicate fires). Twelve such mismatches were found. Of the three with computed H¹, two sit at H¹ = 0 and one at H¹ = 5 — no cluster, and nowhere near the observer-axis band of 4.

The negative result is *better* than the parallel would have been, and the reason is the case that anchors it. Consider `hanbali_reading` — a reading whose foundational axiom is that analogical reasoning (qiyas) is forbidden innovation. It is CS-foreclosed: its axiom rests on an empirically-contingent premise that is being overridden and the authority structure has not acknowledged it. And it sits at a **fully-measured, genuine H¹ = 0.** This is not H¹-zero by absence of data — all twelve pipeline subsystems ran, all four canonical observer positions were computed, and they *all agree*: powerless, moderate, institutional, and analytical observers every one classify it as tangled_rope. The Wasserstein distances across all power transitions are exactly zero; the contextuality fraction is zero; the orbit is constant. The observer-axis cohomology machinery looked as hard as it can look and correctly found nothing, because there is genuinely no perspectival disagreement to find.

Contrast `absolutist_reading`, which the same diagnostic flags and which sits at H¹ = 5: its perspectives fracture (tangled_rope / tangled_rope / rope / snare), its Wasserstein distance at the institutional→analytical transition is 0.618, its contextuality fraction is 0.833. That reading is genuinely fracturing across positions, and the cohomology catches it. Hanbali is not fracturing across positions at all — and is nonetheless structurally foreclosed.

This is the finding, stated as a design constraint:

> **A reading can be perfectly coherent on the observer axis (H¹ = 0, every position agrees) while being structurally foreclosed on the committer axis. Perspectival coherence does not imply axiomatic validity. The committer axis detects a failure mode that observer-axis cohomology is constitutionally blind to.**

The blindness is structural, not a measurement gap. H¹ measures *disagreement across positions*. A foreclosed axiom held by an authority structure produces no disagreement across positions — every observer sees the same reading rest on the same falsified premise. The failure is not in how different seats experience the constraint; it is in what the commitment is committed to. That is invisible to a machinery built to measure perspectival variation, by construction.

If committer-foreclosure had clustered at some H¹ band, it would have meant the observer-axis cohomology already half-captured it, and the committer axis would be partly redundant. The H¹ = 0 foreclosure proves the opposite: the two axes detect *disjoint* failure modes. This is the strongest justification for the committer axis existing at all — it sees something the observer axis cannot see in principle.

**Caveats, kept in the body because they are part of why the finding is trustworthy:**

- The finding rests on one airtight existence proof (hanbali) and one confirming contrast (absolutist). It is *not* a distribution. Three of twelve mismatches have computed H¹; the other nine are not in the Arakelov-eligible pipeline and their H¹ is uncomputed. The qualitative claim — that observer-coherent committer-foreclosure *exists*, and therefore the axes are detection-independent — holds on the strength of the single fully-measured case. The quantitative claim — how committer-foreclosure distributes over H¹ — is not made, and would require computing the nine.
- The *metric-stability* half of the mismatch verdict carries a separate caveat that does **not** touch the H¹ result. Four of hanbali's five explicit neighbors have null purity in the pipeline; the contamination network is effectively a stub for it. So "no network drift detected" reflects sparse data, not a positive measurement of stability. The claim is therefore scoped to the cohomological layer only: **CS-foreclosure detects what H¹ cannot.** It is not leaned on as "CS-foreclosure detects what the metric-stability network cannot," because for this case the network did not really measure. The H¹ measurement is clean; the metric-stability measurement is sparse. Only the clean half bears weight.

---

## The recurring principle

Step back and the same architectural move appears at three nested scales:

1. **Within the observer axis** (the two-hub note): continuous power-scaled extraction (Hub 1) and discrete effective-immutability (Hub 2) are independent mechanisms, kept separate because their mathematical characters differ, interacting at the mountain gate — and that interaction point is where the *false mountain* diagnostic lives (high extraction perceived as immutable, detectable only because the hubs are independent).

2. **Within the committer axis:** the discrete, categorical typed reading-relations (`forecloses`/`coexists_with`/`influences`) sit beside the continuous drift-and-axiom machinery, and their conjunction is the diagnostic (contradictory axioms + `coexists_with` = licensed plurality; contradictory axioms + `forecloses` = real closure — the same contradiction reads as two opposite structures depending on the edge it co-occurs with).

3. **Between the axes:** the whole observer-axis apparatus and the whole committer-axis apparatus are independent, kept separate because their algebras are incompatible, interacting at a small inventoried set of cross-axis reads (originally summarized as the single `influences`-entailment bridge; per the amendment, the decided sole join is the mediator layer) — and the cross-axis interaction point is where the *committer false-stable* diagnostic lives (observer-coherent but committer-foreclosed, detectable only because the axes are independent).

It is one principle, applied recursively: **hold mathematically-incompatible mechanisms separate, and read the diagnostics off their interaction points.** The committer/observer decoupling is not unfinished integration. It is this principle at the top level. The fact that the same shape recurs at every scale examined — and that the surveys surfaced the top-level instance as unmarked drift with exactly the structure of the false-mountain conflict the observer axis already knew how to handle — is the clearest evidence the system is tracking something real rather than generating structure to fit.

---

## What is verified

The committer axis produces gap-structured distributions — clustering with forbidden regions, not a smear — which is the signature of a real carving rather than an arbitrary one. From the promoted corpus:

- **Drift trajectories** pile into husk (57) and axiom_foreclosure (25), with repudiation rare (2) and revival absent. The terminal attractors cluster; most of the possible-terminal space is empty.
- **Axiom conflicts:** 35 licensed-plurality (contradiction + `coexists_with`) versus 15 real-closure (contradiction + `forecloses`), plus 1 with no typed edge. Both signatures present at volume — the licensed-plurality case, which the system originally could not express at all, is now the more common of the two.
- **Kernel divergence:** 79 reading-pair divergences across 34 kernels — the cross-reading divergence diagnostic fires broadly, not just on the test triplet.
- **Drift acknowledgment:** 84 instances of unacknowledged drift; 30 foreclosed-axiom routings (all from generated grounding-type data, none hand-authored; zero spurious firings on deontological axioms — the routing is selective, not a relabel).

The foreclosure routing and the drift trajectory **corroborate**: every reading the routing predicate marks foreclosed also has a drift trajectory terminating at axiom_foreclosure, computed by an independent path from the same authored substrate. Two separate computations converging on the same verdict is the cross-check that distinguishes signal from artifact — and it is only possible because foreclosure is computed from authored shape rather than asserted, which is the architecture that lets the generator supply substrate it can honestly know without making a truth claim it cannot certify.

---

## What is open, by design or by deferral

**Open by design (the separation is the answer):**

- The CS drift state is invisible to the network's drift-velocity and stability assessment. A reading the CS engine diagnoses as foreclosed registers as "stable" in the network layer. This is the cross-axis false-stable — and it is *correct* that the network does not see it, because the network measures perspectival/metric drift and CS-foreclosure is neither. The `cs_drift_mismatch` diagnostic exists precisely to surface the disagreement rather than force the layers to reconcile. The disagreement is the finding, not a bug.
- The reading-relation edges do not enter the contamination network, but the grounds differ by relation type. `forecloses` is excluded by mathematical incompatibility: it is gradient-orthogonal to scalar contamination flow, a categorical override incompatible with the monotone endofunctor convergence proof — verified empirically by `test_forecloses_fpn_injection.pl`. `coexists_with` is excluded by correct design intent (zero-flow by definition; encoding it as a network edge would assert a flow the relation specifically denies), but the exclusion currently holds by absence only: no edge constructor exists, and `constraint_neighbors/3` has no label filter to enforce it mechanically. The gate for `coexists_with` is deferred; see below.

**Open by deferral (real work, not yet done):**

- **The comparison/mediator layer itself (OQ-15).** Decided, designed at the invariant level
  (sole reader of both axes; axes read-only; writes only to JSON; three grep-enforceable
  invariants), not built. Until it lands, the 16 inventoried cross-axis surfaces remain
  nominal boundary violations that are behaviorally clean. Building it also gives the DR/CS
  Π-difference annotation (OQ-08) its natural permanent home, and is where `classify_at_time`
  would split (OQ-17).
- **`coexists_with` gate in `constraint_neighbors/3`.** The `coexists_with` exclusion from the contamination network holds by absence (no edge constructor exists), not by mechanical enforcement. `constraint_neighbors/3` is label-blind — a coexists_with edge, if constructed, would be admitted and computed identically to a scalar injection. The enforcement site is `constraint_neighbors/3`; adding a label filter there is the gate that would make the exclusion mechanical. Deferred until coexists_with edge construction is on the near horizon.

- **Abductive-trigger blindness.** The abductive engine imports no CS module, so no committer-axis finding — kernel divergence, axiom conflict, drift trajectory — can surface as a hypothesis. The system's explanatory layer is currently observer-axis only: perspectival incoherence generates hypotheses, axiom conflict does not. This is a genuine asymmetry and a known limitation, not merely future work. It is the sharpest single integration point: the junction where, eventually, both axes' findings would converge into unified hypotheses.
- **The nine uncomputed mismatches.** Computing their H¹ would upgrade the detection-independence finding from existence-proof to distribution. Until then the finding is qualitative.
- **Kernel-level analytics are partial.** Most observer-axis diagnostics (orbit shape, Arakelov height, Boltzmann compliance) operate per-constraint. The system has moved from atomic constraints to structured constraint families (kernels with multiple readings), but most analytics have not lifted to the kernel level. There is no predicate for "do the orbit shapes across a kernel's readings differ systematically" or "what is the Arakelov-height differential across readings." This is a phase transition the analytics have not fully absorbed — not a bug, a lag.
- **The naturalized-mountain hook.** The contamination table treats all mountains as immune (strength 0.0), but the CS layer can identify a specific subpopulation — low-extraction mountains with extraction authority and both victims and beneficiaries, the cover-story variant. Letting the CS diagnostic inform contamination strength is the one place where a *carefully designed* door through the separation boundary might later be warranted. Noted as an intentional future gap, not a current fix.

**Non-goals (things that would break the architecture):**

- No forced unification of the axes into one graph or one classification.
- No reduction of committer-axis diagnostics to observer-axis structures (H¹ must not be used as a proxy for foreclosure risk — the hanbali case is the proof of why it cannot be).
- No reconciliation of drift disagreement — the mismatch must remain *representable as a disagreement*, not collapsed to one side's verdict.

---

## Relation to the two-hub note

The two-hub note is this document's sibling and its structural precedent. It argued that the observer axis has two independent internal mechanisms (continuous extraction, discrete immutability) that should not be merged, because their independence is what makes the false-mountain diagnostic possible, and it verified this empirically — hub-conflicts cluster at H¹ = 4, the false mountain is detectable only because the hubs are independent.

This document is that argument one scale up. Where the two-hub note keeps two mechanisms separate within one axis, this note keeps two axes separate within one system. The two-hub note's false mountain (high extraction perceived immutable) and this note's committer false-stable (observer-coherent but axiomatically foreclosed) are the same kind of object: a failure visible only at the seam between independent mechanisms, invisible to either mechanism alone. The recurrence of the structure across scales is, as noted, the main finding — the architecture is built on one principle, and the principle reproduces itself at every level it is examined, including the level of the system catching its own unmarked drift.

---

## Summary

The system has two axes. The observer axis measures perspectival variation — how a fixed constraint classifies differently across positions, quantified by H¹. The committer axis measures commitment structure — which reading of a contested kernel, resting on what axioms, drifting toward what terminal. They are orthogonal coordinates with incompatible algebras, deliberately decoupled, joined today by 16 inventoried cross-axis reads (the formerly-blessed `influences`-entailment bridge among them) and, by decision (2026-06-09 amendment), eventually by a single comparison/mediator layer — decided, unbuilt (OQ-15).

The decoupling is a decision, now marked: the committer-axis edges must not enter the contamination network, because their categorical/zero-flow/entailment characters are incompatible with the network's scalar-flow algebra. The decision is made a constraint rather than a preference by a single fully-measured result: a reading (hanbali) that is observer-coherent at genuine H¹ = 0 while being committer-foreclosed, proving that perspectival coherence does not imply axiomatic validity and that the committer axis detects a failure mode observer cohomology cannot see in principle. The finding is scoped to the cohomological layer (the metric-stability half rests on sparse data) and is an existence proof rather than a distribution (one clean case, nine uncomputed mismatches outstanding) — both caveats kept visible because they are what make the claim trustworthy.

The same separate-mechanisms-with-diagnostics-at-the-seam principle appears within the observer axis (two hubs), within the committer axis (typed edges beside drift machinery), and between the axes. Holding the layers apart is not unfinished integration; it is the system's organizing principle at its top level. What remains open — abductive-trigger integration, full H¹ computation of the mismatch set, kernel-level analytics, the naturalized-mountain hook — is enumerated as deferral, and what must never be done — forced unification, reduction of one axis to the other, reconciliation of the drift disagreement — is enumerated as non-goal.

The reference frame is now fixed. From here, changes to the relationship between the axes are drift from a marked baseline rather than mutation of an unmarked one.

---

*Architecture note for the two-axis (observer + committer) system. Companion to the two-hub note (observer-axis internal structure) and prerequisite to the eventual DR v7 treatise (which states the detection-independence result as a theorem, downstream of the frame this document fixes). CC0 Universal.*
