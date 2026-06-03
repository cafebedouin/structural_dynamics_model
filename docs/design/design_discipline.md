# Design Discipline

**A statement of the design commitments the Deferential Realism engine rests on, the
consequences that follow from them, and the open questions that remain — separated, as the
papers separate axioms from theorems from empirical findings, so that a wrong foundation can be
found wrong without taking the whole structure down with it.**

**Version: v1.2** *(first versioned draft was v1.0, the consequences-only sketch restructured into
axioms/consequences/empirical/open. v1.1: mutual-deference framing of Axiom R and §7, routing-as-
correctability in §5, character-upstream-of-selector in §8, §3 labeled abductive. v1.2: invariant
boundary as seat-identified/fallible/interested with the three-way distinction (§2); CS drift
terminal states as the existing early-warning signal for sealing (§5); ε disciplines stated as
rules (§7). Supersedes the unversioned consequences-only sketch,
retained as design_discipline_v0_backup.md)*

**Status:** Companion to `build_discipline.md`, referenced from `CLAUDE.md`, amended as design
decisions are made and recorded. Where `build_discipline.md` governs *how we build and verify*,
this governs *what the engine is for, what follows from that, and what it must not become.*

---

## 0. The declared seat

This document does not issue from nowhere, and by its own central commitment (§2) it cannot. So
the seat is declared first, before anything is argued from it.

**The seat is philosophical skepticism held for a purpose.** We could be wrong about everything —
any foundational idea here, including the seat theorem the rest leans on, could be false. No
foundation is certified; the regress of justification does not bottom out (§2, and the seat
theorem's §8). But one must choose a seat to do anything at all, and a *good* seat is one chosen
for a declared purpose rather than mistaken for the floor. The purpose declared here is: **to
understand clearly the choices a position implies — what follows if you stand here — even when
the foundational ideas might be wrong.** The engine is an instrument for making the implications
of a seat legible, not for proving the seat correct. That a foundation could be wrong does not
defeat the work; understanding what a position commits you to is valuable independent of whether
the position is ultimately right, and is in fact the only thing available once neutrality is
gone.

This seat is itself seat-theorem-compliant, and saying so is not circular — it is the one move
the theorem permits. It does not claim to be the correct seat; it declares itself, states its
purpose, and stakes it as something that could be wrong. A document that argued "all positions
are seated" from a position it presented as unseated would commit the one inconsistency the
framework names (the no-seat pose). This one shows its seat on the first page.

A consequence for how to read the rest: the axioms below are **design commitments, declared and
contestable**, not discovered truths. The theorems are what follows *given* the axioms. The
empirical findings are corpus-conditional and can shift. The open questions are sorted by whether
they are closeable (more work resolves them) or constitutively open (undecidable from inside the
engine — a chosen cut, not a pending answer). The separation is the point: it lets a reader
reject an axiom and see exactly what falls, rather than having to reject or accept the whole.

---

## 1. Provenance: engine first, theory second

A fact about how this knowledge was produced, recorded because it bears on the document's
authority. **The engine was built first; the theory was discovered second.** The classification
machinery, the two hubs, the signature overrides, the commitment-system layer — these were
constructed to do a job. The formal results that now read as the engine's foundation (the seat
theorem, the trifurcation, the detection-independence theorem, the separation principle) were
*read off* a working system afterward, as descriptions of what the engine was already doing.

This ordering is why the theory is trustworthy in the specific way it is: it was not imposed on
the engine as a design target the engine was then built to satisfy (which would make the
engine's conformance to the theory circular and uninformative). It was extracted from behavior
that already obtained. When the theory and the engine disagree, that is not the engine failing
to implement the theory — it is the theory being an imperfect description of the substrate, and
the substrate wins (the FNL predicate-identity case in §5 is a worked instance: the canonical
paper described a behavior the engine never had). The papers are *descriptions of the engine*,
versioned and corrigible, not specifications it must obey.

The seed was an engineering reframing of philosophical paradoxes (`debugging_philosophy.md`):
the claim that most paradoxes are not metaphysical fractures but **engineering failures** — Type
A unmarked state mutation (drift), Type B axiomatic inconsistency (structure), or Type C
indexical underspecification (ambiguity). That trifurcation is the seed from which everything
here grows, and its recurrence at every scale (§3) is the strongest evidence the engine tracks
something real.

---

## 2. The foundational axioms (declared, contestable)

These are stated as the seat theorem and its companions establish them, but they are held here
as *commitments*, not certified truths. Reject any one and the consequences in §§4–8 that depend
on it fall; the dependencies are marked so the damage is legible.

### Axiom S1 — Contentful verdicts are seated (the Coupling Theorem)

A verdict is *seat-free* iff it is *contentless*. Contrapositively: **any verdict that settles
something the situation did not already settle is issued from a standpoint — a live parameter not
fixed by the situation.** (Seat theorem §4, Corollary 1.) A classification is a contentful
verdict; therefore there is no seat-free correct classification waiting to be found. The engine
does not return "the answer" because, for any question with content, a seat-free answer does not
exist.

*Contestable at:* the two premises (a property of a situation is fixed by the situation; a
parameter co-determining a verdict without being fixed by the situation is a standpoint, not a
feature of the situation). A joint-carving realist who holds the world self-partitions into a
privileged frame denies this. The axiom is staked, not proven; it binds those who grant the
premises and cannot compel one who does not (seat theorem §8).

### Axiom S2 — The only coherent residue of neutrality is declaration (Corollary 2a)

Since seat-freedom is unavailable to any contentful verdict, an *undeclared* seat is not an
absent seat — it is content asserted without a standpoint, the **no-seat pose**, the unique
inconsistency the framework permits. The discipline that follows has one law: **a contentful
verdict that does not show its seat is not neutral but concealed, and concealment is the error.**
The engine is, at root, a discipline of declaration: it surfaces *which seat* a classification
issues from, so the seat is shown rather than hidden.

*Follows from S1 alone* — no further premise. This is the most secure thing in the document.

### Axiom S3 — Which seats to declare requires a selection rule, itself a declared bet (Corollary 2b)

S1 yields infinitely many seats (every covariate a verdict is sensitive to). A workable
discipline can attend to a few. Going from "all verdicts are seated" to "declare *these* seats"
requires a **selection rule**, and that rule is a substantive, contestable claim about which
seats carry weight — not a theorem. The engine's selection rule is **interrogative** (a battery
of questions whose answers are staked but whose questions are not verdicts), and its dominant
question is **Q1: who benefits and who pays** — the distributional bet that a consequential
arrangement always has a for-whom. This is the engine's pointing at *power and its masks* (§4),
and it is a *declared selector that could lose a round*, not a derived necessity.

*Contestable at:* the selector itself. Someone who grants S1 and S2 can hold a rival selector
(e.g. that maintained arrangements are mostly civilizational goods, not installed extractions).
The framework cannot rank rival selectors neutrally — that would be a seat-free verdict among
contentful claims, which S1 forbids. The selector stays declared-and-defended, not proven.
*(This is why §4's "power and its masks" is the engine's purpose but not its certified ground.)*

### Axiom T — The trifurcation (the seed, generalized)

Apparent contradictions in a reasoning system are predominantly one of three engineering kinds,
not metaphysical fractures: **Type A drift** (a reference frame mutating unmarked across stages —
resolved by frame-fixing at t0), **Type B structure** (genuine axiomatic inconsistency — resolved
by revising axioms), **Type C ambiguity** (a single question packaging multiple indexed queries —
resolved by specifying the index). (`debugging_philosophy.md`.)

*This is the generative axiom* — the others recur from it (§3). It is held as a diagnostic
commitment: when something looks paradoxical or stuck, classify the mechanism before reaching for
a metaphysical reading.

### Axiom R — Deferential realism: mutual deference (realist about the invariant, deferential about the rest)

The name is a two-way claim, not a label. **We defer to reality where it is invariant; reality
defers to the seat where it is not.** Some structural properties are operational regardless of
seat — gravity, the mountain — and no standpoint reclassifies them; this is the realist anchor,
and it is not chosen. Two invariants bind the seat: the seat-independent operationals (mountains),
and the constraint on which seats are *reachable* (one cannot declare into a seat one cannot
occupy). For everything not invariant — most of the classification space — what is real depends on
the seat, and there the engine is deferential: a snare exists as a snare only from positions that
experience it so, with no seat-free fact of its snare-hood.

Operationally: the engine treats a constraint's structural inputs (base extractiveness ε,
suppression, coordination function, enforcement) as **observer-independent data within the
frame** — read from evidence, the same across seats — and treats *classification* as
**irreducibly seat-dependent** for the non-invariant. Indexicality is localized: it enters only
through the directionality derivation (Hub 1) and the time-horizon/exit context (Hub 2);
everything else is observer-independent data or deterministic transformation. (Two-hub note.) The
two directions of deference are the two ends of the indexicality gradient the engine measures:
mountains (zero divergence under indexing, fully recoverable from non-indexed data — defer to
reality) and snares (essential-indexed, nonexistent without observer position — reality defers to
seat). A third stance, **committer-relative**, extends the deferential half: realist about a
reading's authored axioms, deferential about which reading a community holds.

*Contestable at:* the realist half, and specifically ε — the least-grounded, most load-bearing
primitive, authored by judgment for the non-invariant though pinned at the invariant pole (§7
develops what kind of realism this is — neither strong realism nor instrumentalism, but
mutual deference anchored at the mountain). If even the invariant anchor is not genuinely
seat-independent, the realist half is weaker than stated. The deferential half follows from S1.

**The invariant boundary is itself identified from within a seat — and this is the engine's
purpose, not a defect.** A constraint *is* invariant if it actually resists seat-variation
(witnessed as zero divergence under indexing — an empirical fact about the constraint, not a
decree). But the *identification* of a given constraint as invariant is a contentful verdict,
hence seated (S1), hence fallible and revisable. Mountains are not invariant *because we called
them mountains*; our calling a thing a mountain is a seated, corrigible claim the engine can get
wrong. This is not a hole — **the false-mountain diagnostic is the engine already catching its own
mis-identification of an invariant**: a false mountain is a constraint seated-judged invariant
that is not. So the fallibility of the boundary is *operationalized*, not hoped away, which
strengthens Axiom R rather than weakening it.

And the mis-identification is not random — it is *interested*, which is exactly why the engine
focuses where it does (§4). **Society and other actors have a stake in painting variants as
invariants**: "the natural order of markets," "the inevitability of hierarchy" are variants
someone benefits from your taking as floor. A false mountain is a variant an interested party has
an interest in your treating as invariant. So the seat-relativity of the invariant boundary is not
a weakness to confess — it is the attack surface the engine is built to watch, and the reason a
power-meter (§4) is the right instrument for it. Three cases the engine must keep distinct, in
ascending order of danger:

- **An invariant you selected and declared** — owned, revisable, seated and shown ("I will never
  lie" — a chosen floor, held as a stake, able to lose a round). Legitimate; it is declared.
- **An invariant you did not select** — a genuine seat-independent operational (gravity). The real
  floor; defer to it.
- **An invariant someone else selected for you, concealed** — a variant installed as invariant by
  an interested party, presenting as the second case while being the first case run by someone
  else without your knowing. This is the laundered invariant, the concealed seat, the false
  mountain — and telling it from the genuine floor is the engine's central work, because it
  *presents as* the genuine floor by design.

---

## 3. The recurring structure (why these axioms are one principle, not five)

The single most important structural claim, and the strongest evidence the engine tracks
something real rather than generating structure to fit: **the same three-way cut, and the same
hold-incompatible-mechanisms-separate move, recur at every scale examined.** This is not one
claim restated; it is independent derivations arriving at one shape.

The trifurcation (Axiom T) is the seed. It reappears as:

- **The Omega types** (`omega_variables.md`): Ω_E empirical / Ω_C conceptual / Ω_P preference —
  the same cuts as Type A/B/C, now as resolution-routing. Drift needs measurement (Ω_E);
  structure needs definition/revision (Ω_C, framework selection); ambiguity-of-value needs the
  stakeholder (Ω_P). "Data cannot resolve conceptual ambiguity; definitions cannot reveal values;
  values cannot generate facts" is the same incompatibility the axioms keep separate.
- **The seat theorem's residue**: a question is closeable by evidence (empirical), by definition
  (conceptual), or it is *constitutively undecidable from inside* and resolves only to a declared
  cut (preference / the self-referential case). Ω_P is the seat theorem's "no neutral ranking of
  selectors" stated as a routing category.
- **Separate-mechanisms-at-the-seam**, three nested times (two-hub note, two-axis note):
  within the observer axis (continuous extraction / discrete immutability — the two hubs, kept
  apart because the *false mountain* lives at their seam); within the committer axis (categorical
  typed edges / continuous drift machinery — licensed-plurality vs real-closure lives at their
  seam); between the axes (observer / committer — the *committer false-stable* lives at their
  seam). One principle: **hold mathematically-incompatible mechanisms separate, and read the
  diagnostics off their interaction points.**

The recurrence is the finding. A structure that arrives from ethics (Hume's is/ought),
political anthropology (Scott's metis), and the logic of induction (Goodman's grue) — three
unrelated directions converging on "a tool has a constitutive outside invisible from within"
(the rationality-as-test-case capstone) — is load-bearing in a way no single elegant framing
could be. The engine's separations are instances of this convergence, not arbitrary choices.

To be exact about this section's status, since it is the most interpretive in the document: §3 is
*evidence for* the axioms, not part of their proof. The axioms (§2) stand or fall on their own
premises; the recurrence does not *prove* them — it is the abductive case that they track
something real rather than being imposed. A skeptic can grant every instance of the recurrence and
still hold that the pattern is an artifact of the framing lens rather than a discovered structure,
and the engine-first provenance (§1) is the only defense against that, which is itself a claim
about history one has to trust. So §3 is strong but not decisive, and it is labeled evidence, not
derivation, deliberately.

### 3a — The same cut, read as a method: perturbation (the one move under all the instruments)

The trifurcation has been read here two ways — as a taxonomy of *failures* (Axiom T: which kind of
paradox is this) and of *resolutions* (the Ω types: measurement / definition / stakeholder). There is
a third reading that unifies them and is the engine's actual operating procedure: **Type A/B/C are the
three dimensions you perturb.** A Type-X failure is axis X having varied *unmarked*; the method is to
vary axis X *on purpose*, hold the rest fixed, and read what stays against what moves. What moves is
variant → seated → the depth (the deferential half). What stays is invariant **under the probes
actually run** — and the procedure licenses only that, not "real" simpliciter: a flat axis and a
perturbation never run (or run along the wrong sub-axis) present the identical flat result (the
cyclopean §4 point; the "I-didn't-find-it ≠ it-isn't-there" gap the framework polices everywhere).
So invariance-under-perturbation licenses **defer provisionally** — Axiom R's realist half held
exactly as §2 already holds it: *seat-identified, fallible, revisable*, with the **false-mountain
diagnostic** as the operationalized catch for the residual gap (a constraint deferred-to as invariant
that a not-yet-run perturbation would have moved). The honest consequence, stated not smuggled:
**Axiom R's realist pole reached this way is unfalsified-so-far invariance, not certified floor** —
the framework is more search-relative than a naive reading of "defer to the mountain" admits, which
is consistent with §2 (the boundary is identified from within a seat), not a new concession. The
genuine seat-independent operationals (gravity) are the limit this search approaches, not something
the procedure ever hands you finished.

- **Axis C — perturb the observer** (the indexical (P,T,E,S)): `reading_diff`.
- **Axis B — perturb the axiom** (the kernel's commitments / which reading): `axiom_diff`; cross-kernel
  the Westphalia near-kernels, where two near-identical kernels held side by side make the invariant
  legible as their agreement and the divergence as their disparity.
- **Axis A — perturb time**: the drift/lifecycle machinery.
- and the reflexive fourth, **perturb the apparatus** (`perturb.py`, the stability band, the
  `ε`-stability rule §7): which verdicts survive the choice of calibration, which were the threshold's
  verdict and not the constraint's (§5).

This is not new machinery — it is the operational form of the commitments already stated. **The seat
of a verdict (S1, S2) *is* the perturbation that flips it:** a verdict is seated in axis X exactly when
some variation along X changes it, and contentless exactly when none does. So "declare the seat" and
"name the axis your verdict is sensitive to" are one act, and the engine surfaces the seat *by* finding
the axis along which the classification is not invariant. The recurrence of §3 is therefore also a
recurrence of *one move* across independently-built instruments (observer, axiom, temporal,
calibration), which is the §3 evidence-argument carried into method. Full statement:
`docs/the_perturbation_principle.md` (theory) and `docs/design/the_perturbation_move.md` (code).

---

## 4. What the engine is for (consequence of S3)

The engine is a **discipline of declaration** (S2) pointed, by its declared selector (S3), at
**power imbalances and how power masks itself.** The pointing is the chosen part; the declaration
is the structural part. Every capability serves surfacing the ways a constraint that extracts
presents itself as something else, and making visible *from which seat* that presentation holds
and from which it breaks.

This adjudicates ambiguous design calls. A change that makes the engine better at surfacing
masked extraction *and showing its seat* is aligned with purpose. A change that makes the engine
more "complete" or "general" at the cost of either blurring how power masks itself (the selector)
or concealing a seat (the law) is drift. The engine is not a general-purpose classifier of social
structures; it is a meter for power imbalance and its disguises, and a discipline for declaring
the seat the meter reads from.

The deepest consequence, from S1: **the engine surfaces structure for a human to take a seat
against; it does not return the answer.** The point is to force the active perception shift — to
make the user test a verdict from a kernel, a reading, an observer position, against their own
sense of what is true — not to hand them a verdict to accept. An engine that returned "the
correct classification" would be performing the no-seat pose: contentful verdict, concealed seat.

**Power is relational, not an attribute.** A party's seat is set by its position *in the
relationship the constraint governs*, not by a fixed attribute. A bank is institutional toward
its depositors, powerless toward its regulator. So apparent same-level conflict (regulator vs
bank, both "institutional") is not same-level once the constraint is fixed: the enforcing party
holds the institutional seat; the weaker party has dropped to moderate or powerless and is
modeled there. The engine represents such conflict by re-seating, not by adjudicating between two
institutional seats — and it has no sub-position adjudication, correctly.

This resolves an apparent flaw in the power-role heuristic. The institutional seat keys on
whether the constraint has any beneficiary and **ignores victim status**
(`power_role_heuristic(institutional, true, _, 0.15)`). This is not a blind spot — it is the
encoding of "the institutional seat is, by construction, the enforcing / net-benefiting party."
An institution genuinely harmed by a constraint is, in that relationship, not in the institutional
seat. Consequence for reading data: **the institutional seat is the most meaningful of the four
for the "extractive from every position" test, not the least** — because the seat is biased
toward net-benefit by construction (the −0.2 power modifier, the benefit-assuming directionality),
a constraint clearing the extraction ceiling *even from the institutional seat* has overcome that
bias, the strong signal that it extracts from every position. This is witnessed in substrate, not
merely argued: v6.13.1 §5.4's reachability analysis finds institutional *upward* dissent
unreachable under the standard heuristic, because f(d) < 0 at canonical d forces institutional χ
below the thresholds — so clearing it means defeating a built-in bias. *(Calibration-dependent,
the regime the engine runs in.)*

**Framing-difference vs enforcement-difference — handled by two different axes, and conflating
them is the error this guards against:**
- *Enforcement difference* (one party enforces, the other complies) → observer axis, via
  relational re-seating.
- *Framing difference* (two parties read one kernel through incompatible interpretations) →
  committer axis: two readings of one kernel, with `coexists_with`/licensed-plurality where both
  stand and real-closure where one forecloses the other. A constraint snare-under-one-reading and
  tangled_rope-under-another is **not** routed to a single type — collapsing it would destroy the
  disagreement the committer axis exists to preserve.

Practical rule for a high-extraction constraint "extractive from every seat": before routing to
snare, check reading-multiplicity. A *single-reading* kernel extractive from every observer
position (institutional ceiling included) is a genuine cross-seat snare. A *multi-reading* kernel
where readings diverge is a plurality, routed to committer machinery, not collapsed.

---

## 5. Classification is routing, not truth (consequence of S1)

A type assignment is a **routing decision** — what handling does this constraint need — not a
metaphysical claim about what the constraint *really is*. This is S1 applied to classification:
since there is no seat-free correct type, "what is it really" has no seat-free answer; "what
handling does it need from here" does. Consequences:

- **Where you draw a band boundary is a calibration, not a discovery.** There is no true value of
  `tangled_rope_chi_floor`; there is a value that routes constraints to handling that makes the
  output make sense from the seats that experience them. (Corollary 1: neutrality and triviality
  are one point — a "true" threshold would be a seat-free contentful verdict.)
- **Never move a calibrated threshold to rescue a specific reading.** That fits the instrument to
  the sample. If a reading classifies wrongly, ask whether the *seat*, the *reading*, or the
  *calibration* is wrong — not whether to nudge a floor. (Structural fixes — closing a gap
  between bands that should abut — are exempt: geometry, not calibration. §6.)
- **"The output must make sense" is the test, and it is seat-relative.** A verdict makes sense
  when, from the seat it is computed at, it matches a careful observer's sense of what is true.
  "Makes sense" is never global — always "from here."

**What makes routing *good* (defined by outcomes, not by static feature-preservation).** "Makes
sense from the seat" is necessary but underspecified on its own — it invites the charge that
routing reduces to whatever feels right from a perspective. It does not, and the criterion is
dynamic, not static: **good routing is the routing that keeps the model correctable.** Route
critique to defense — fold every disconfirmation back into the existing classification — and the
model drifts from reality until it breaks against the world. Good routing is the opposite move:
updating the kernel when the reading no longer fits, holding the current perspective as one choice
among many rather than the correct one, letting a verdict be taken from you by what you did not
see. The test is not "does this routing preserve the right features" (a static criterion that
still needs a seat to say which features are right); it is "does this routing keep the model able
to be corrected, or does it seal the model against correction." A sealed model eventually breaks —
that is the observable failure, and it is why the criterion is outcome-defined rather than
felt-rightness-defined. This connects §5 directly to §8: routing-critique-to-defense is the
model-level form of reabsorbing a disconfirmation (Corollary 3), and the override audit's
laundering is its classification-layer instance — relabeling a snare as a hybrid is critique
(the metric's extraction reading) routed to defense (the more comfortable type). Good routing at
every level is correctability preserved; bad routing is correctability sealed off.

**The early-warning signal already exists — it is the commitment-systems drift machinery.**
"Breaks against the world" is a *lagging* indicator: by the time the model produces nonsense, the
sealing is complete. But the engine does not have to wait for the break, because the committer
axis's drift engine already computes the *approach* to sealing as a trajectory toward terminal
attractors. A kernel-plus-interpretation that is sealing routes its drift trajectory toward
**husk** (the interpretation hollowed out, kept running past its live content) or
**axiom_foreclosure** (the founding premise overridden but unacknowledged) — and that trajectory
is measurable *before* the terminal state is reached. Routing critique to defense is drift toward
foreclosure; the CS engine measures the drift, not just the arrival. So the leading indicators of
a sealing model are not new machinery to invent — they are the drift trajectory the engine
already tracks, plus their classification-layer shadows: override density rising over time (the
correction layer doing more work to hold a reading together), disconfirmations consistently
reclassified rather than absorbed, and the admissible-reclassification space shrinking (fewer
readings the model will accept). One caveat, or this becomes a single-seat error one level up:
**what counts as a disconfirmation is itself partly selector-shaped** (Q1 influences what reads as
a problem), so these are heuristics read from a seat, not a context-free dashboard. The drift
machinery flags the approach; a human reads the flag from a declared seat.

**Fudge factor compounds — the §2 distinction applied to slack.** The engine tolerates real
calibration slack where a structural marker is genuinely present (line-drawing at the margins is
seat-relative and approximate). But slack on slack produces *wrong intuitions*, the opposite of
the engine's purpose (to sharpen parsing of power dynamics with greater granularity). The rule:
tolerate slack where structure is present; refuse it where the marker the type asserts is
*absent*. A type claiming coordination on a constraint with no coordination structure does not
make sense from any seat — the asserted thing is not there to be perceived. That is not fudge; it
is error, and it is the **liveness test** (the seat theorem's Q5 / the seed's index-specification):
a thing whose removal rearranges nothing is fixed-by-situation; a thing whose asserted property is
absent fails the test, and claiming it is concealment.

### The override layer corrects metric blindness — mostly *(audit finding; predicate identity now resolved)*

The signature-override layer is the **correction-grade** half of the verdict-layer pattern
documented in v6.13.1 §4.4 (correction-grade overrides the type and needs high precision;
commentary-grade annotates without reclassifying). A full-corpus audit (not the toy substrate) of
the override forcing metric-snare → tangled_rope found it **predominantly load-bearing, not
laundering**:

- The dominant behavior fires on ~1641 readings. The naive read — "launders snares into the
  less-damning hybrid 1641 times" — is **false**, an artifact of a single-seat read. Most carry
  both tangled_rope markers (coordination + asymmetric extraction); snare was the artifact of a
  coordination-blind snare clause gating on the extraction triple alone. For these the override
  *repairs a known blindness*; the upstream fix is making the snare clause coordination-aware.
- The genuinely-wrong behavior — relabeling a constraint extractive from *every* seat — is a
  single-digit tail, isolable by the §4 cross-seat + reading-multiplicity tests.

**Predicate identity (resolved against substrate, ruling (a)):** the audit attributed
snare→tangled_rope to `false_natural_law` at signature_detection.pl:749; the run confirms the
firing clause IS `false_natural_law` (`resolve_modal_signature_conflict(_, false_natural_law,
tangled_rope)`, wildcard input), detection is claimed_natural ∧ Boltzmann-non-compliant with no
beneficiary check, and naturalized is unreachable from FNL by construction. So the audit's name
and counts (1730/1641) stand; **v6.13.1 §4.2/§4.4 are stale** — they describe FNL as
mountain→naturalized, a behavior the engine has never had (the FNL→tangled_rope override is dated
v5.1, predating v6.13.1). This is the engine-first/theory-second pattern of §1 in miniature: the
paper, not the engine, was wrong, and the substrate ruled. Correcting v6.13.1 is a versioned doc
edit (per the v6.13.1 §4.3 errata precedent) — settled as a fact, pending only the where/how of
landing it.

**The durable methodological finding, independent of the predicate question:** a property computed
at one observer seat is **not** a property of the constraint. The analytical seat is tuned
(π = 1.15) to surface extraction other seats normalize, so it reads highest by design. A
population uniform-and-damning from that seat sorts into honest-hybrid-from-most-seats vs
extractive-from-every-seat the moment observers are disentangled. **Never rule on a population
from a single-seat read.** The engine's observer-relativity is the sorting tool; using it is the
method, not optional rigor. This is the build-discipline rule (an empty/clean result is a fact
about the probe until the probe is shown to discriminate) at the design level — and this very
document violated it twice (§6, the δ double-miss), which is why it is stated this sharply.

---

## 6. The open questions, sorted by kind

The seat theorem and the trifurcation force a distinction the first draft of this document blurred:
an open question is either **closeable** (more work — measurement, definition, construction —
resolves it: Ω_E or Ω_C) or **constitutively open** (undecidable from inside the engine; resolves
only to a declared, chosen cut: Ω_P, or the self-referential case the stream essay describes).
Conflating them is an error — it promises that constitutively-open questions will close with
effort, which they will not.

### Closeable (Ω_E / Ω_C — more work resolves)

- **The FNL doc correction** (§5): settled as fact, pending only the governance of landing it in
  v6.13.1. Ω_C (where it lands), not Ω_E.
- **Calibration debt.** The χ / ε / suppression thresholds were calibrated on a 691-constraint
  pre-rebuild Western corpus and never recalibrated against the live corpus. Any threshold argued
  from "the corpus shows" is, until recalibration, arguing from the wrong corpus. Ω_E — resolved
  by recalibration after the rebuild. **Do now whatever does not require calibration (structural
  fixes); defer calibration until the corpus is worth calibrating against.** This is the
  engineering image of the Axiom-status distinction in §2: structural fixes are corpus-independent
  (safe now); calibration fixes are claims about the world (wait for a trustworthy corpus).
- **The metric=unknown holdouts (OQ-37).** Diagnosed as honest band-gap fall-through (a taxonomy
  coverage gap), not a compute bug; the χ-partition gap was closed structurally (Move 1), the
  suppression-floor recalibration deferred (Ω_E, post-rebuild). Closeable.

### Constitutively open (Ω_P / undecidable-from-inside — a chosen cut, not a pending answer)

- **Is the coupling-score the right operationalization of natural-vs-constructed?** That the
  threshold is robust (wide-margin partition) says the engine is internally consistent; whether
  the score *measures the right thing* is undecidable from inside the engine — it is the stream
  essay's "is the witness real or is it the fold" question, resolvable only by a human taking a
  declared seat against it. Ω_P. No amount of sweeping closes it. *(The honest framing is not
  "we'll resolve this later" but "this is a seat the engine cannot occupy from within.")*
- **Is the engine's selector (Q1, who-benefits-who-pays) the right master axis?** This is Axiom S3
  put to itself, and it is the rationality-capstone's Ω_P exactly: no analytic ranking of rival
  selectors is available from above; decision authority sits with whoever bears the frame's
  consequences, not with the analyst (which is itself the beneficiary problem the selector names).
  Constitutively open; the selector stays declared-and-defended, able to lose a round.
- **The truly symmetric same-power conflict.** Two parties of equal enforcement capacity, opposing
  stakes, neither dropping seat (§4). Appears to be a **non-configuration** — it tends to resolve
  into asymmetry (→ relational re-seating) or framing difference (→ committer axis). Whether an
  irreducible symmetric same-power conflict is a real configuration or a thought experiment is
  open; flagged, not resolved.

### The δ layer — present-in-code, not load-bearing-validated (a status, not a question)

Intra-*position* cognitive disagreement (two observers at the same structural seat, different
cognitive orientations) is *represented* in the engine: the cognitive-displacement parameter δ
(v6.13.1 §5.6, §5.10), the δ-band population analysis (498 of 822 H¹ ≥ 3 constraints flagged
δ-sensitive), the claim that Theorem 1's cover-story mechanism could operate within a single
position across orientations, and Kahan's Cultural Cognition as a candidate psychometric bridge
(hierarchy-egalitarianism ↦ δ). **This machinery is present-in-code, not validated-as-load-bearing**
— the δ analogue of path-asserted: the predicate exists; that it measures something real and is
relied upon is a separate, unwitnessed claim. The δ-band statistics (498/822) are a property of
the current corpus and threshold geometry, not a witnessed finding, and must not be cited as
validated. The correct status is neither "the engine does not model intra-observer conflict"
(false) nor "the engine models and validates it" (false) but: **the representation exists, is not
yet load-bearing, and may never be.**

*(Recorded with its history because the history is the caution: this document's drafts got the δ
status wrong in both directions — first claiming the engine does not model intra-observer conflict
at all, a single-seat read of the v7 docs that missed v6.13.1's δ layer; then over-correcting to
"built and validated." The δ layer is exactly the kind of present-but-unproven structure that
invites a confident claim in either direction, and neither is earned. If δ is ever to be relied
on, it needs a positive control: does a δ-flip correspond to a disagreement a careful observer
would recognize, or is the δ-band an artifact of threshold-proximity?)*

What is genuinely deferred, narrower than "intra-observer conflict": whether an agent with
*internally contradictory stakes* (distinct from a single δ-orientation, and distinct from "two
readings" on the committer axis) is a real phenomenon needing machinery beyond what δ and the
committer axis already provide. Open research.

---

## 7. The ε caveat (carried from v7 §6, stated as a design constraint)

Base extractiveness ε is the **least-grounded, most load-bearing primitive.** The entire observer
axis is a deterministic transform of it (χ = ε × f(d) × σ(S)), yet ε is *authored by judgment* —
read by the generator from a reading's narrative — not computed from beneath. ε is
observer-invariant (constant across seats) but **not invariant across generation runs**: the same
topic, scoped differently, yields different ε, correctly (different scopings are different
readings) — so any single corpus's ε-dependent statistics are *one sample conditional on one
coherent generation*, not a point estimate.

Two standing disciplines, stated as rules:
- **Rule (ε-pairing):** Cross-axis quantities may be paired only at one corpus load, at fixed ε. Committer-axis
  routing is ε-independent by design; pairing it with an ε-dependent observer-axis quantity from a
  *different* ε state silently mismatches them.
- **Rule (ε-stability):** A cross-axis anchor must be ε-stable — its signature-resolved
  classification must not flip under small ε perturbation, or the result is an artifact of where a
  hand-authored number landed near a threshold.

This connects to §6's calibration debt: a threshold tuned against ε from a superseded generation
is tuned against the wrong sample. It also bears on Axiom R: ε is exactly where the realist half
is weakest, since ε is authored, not measured — which is declared here rather than hidden.

**What kind of realism ε actually is (the position, committed).** A reviewer pressed that the
document *sounds like* strong realism (ε tracks something mind-independent) while *operating like*
instrumentalism (ε is a parameter that works), and asked which. Neither — and the answer is the
framework's own name, read as a two-way claim rather than a label. **Deferential realism is mutual
deference**, and stating both directions is what makes it neither strong realism nor relativism:

- **We defer to reality, where reality is invariant.** Some things are operational regardless of
  seat — gravity, the mountain. No standpoint reclassifies them; reality does not bend to where
  one stands. That is the *realism*, and it is not chosen. There are two invariants the seat must
  defer to: the seat-independent operationals (mountains), and the *constraint on which seats are
  reachable at all* — one does not have free choice of seat, the available positions are bounded
  by how one is actually situated. You cannot declare into a seat you cannot occupy. This is the
  anchor that keeps the framework from being relativism: there is a class of things no seat can
  reclassify, and the reachable seats are themselves constrained.

- **Reality defers to the seat, where reality is not invariant.** For everything that is not an
  invariant — most of the classification space — what is real depends on the context, and the
  context is the seat. A snare exists *as a snare* only from positions that experience it so;
  there is no seat-free fact of its snare-hood. Here reality defers to the standpoint: the seat
  fixes what is real. This is the *deferential* half, and it is not free invention, because it
  operates only on what the invariants leave open.

So ε is neither a measurement of a mind-independent quantity (strong realism) nor a mere
convenience (instrumentalism). It is authored *within the frame the invariants pin* — fixed by
judgment for the non-invariant, but pinned at the invariant end by what holds no matter the seat
(mountains stay mountains across all authorings; ε near zero is the mountain pole and the sigmoid
produces no effect there, which is why genuine mountains show zero divergence under indexing). The
realist half of Axiom R is therefore *stronger* than "ε approximates something real": it claims ε
is fixed within a frame whose invariant pole is not authored at all, and observer-independent
across the seats inside that frame. The deferential half operates on top of that anchor. The two
directions are not a philosophical gloss — they are the two ends of the indexicality gradient the
engine already measures (two-hub note): **mountains, zero divergence under indexing, fully
recoverable from non-indexed data — the invariant pole where we defer to reality; snares,
essential-indexed, nonexistent without observer position — the pole where reality defers to the
seat.** The name is the theorem.

This is closer to *legal* realism than scientific realism for the deferential half — the Real is
what the Realist takes it to be, and that circularity is closed by fixing
observer-position/kernel/reading — but unlike a pure legal realism it has a hard floor: the
invariants the seat cannot touch. The open question this leaves (§6, long-term) is whether ε's
authoring is stable within the frame — "does the frame hold," not "does it match a seat-free
reality," because for the non-invariant there is no seat-free reality to match, and for the
invariant there is nothing to author.

---

## 8. The discipline as evidence, and the limit of any discipline (the t3 / honoring gap)

A property of the method, recorded because it is itself a design commitment: **the framework
repeatedly declines to confirm its own clean predictions and returns messier, truer results
instead.** The predicted terminal came back husk not repudiation; the predicted hub-conflict band
came back 4 not 5–6; the predicted cohomological clustering came back H¹ = 0, inverting into the
stronger detection-independence result; a drafted theorem's anchor dissolved under verification
and forced a better one; this document's δ claim was wrong in two directions and the substrate
corrected it. A framework that generated structure to fit its operator would confirm the draft;
this one falsifies it.

The design commitment producing this is the witness discipline — and it is the **operational form
of the seat theorem's Corollary 3 (temporal non-interchangeability).** The seat that stakes a
claim cannot pre-occupy the later seat that confronts it; reabsorbing a disconfirmation is
*legible as retreat* precisely because the seats are temporally non-interchangeable. "Witness
before claiming" (build_discipline) and "declare the seat" (this document) are the same law at two
levels: a claim that hides its witness is a claim that conceals its seat.

But there is a limit no discipline of this kind reaches, and it must be stated or the discipline
oversells itself. **Declaration can be mandated; honoring cannot.** Whether a seat is shown is a
checkable fact about a verdict (S2 is enforceable). Whether a declared seat, having staked a
prediction, *honors* a disconfirmation rather than reabsorbing it — folding the loss back in the
moment it arrives — is decided later, by a different occasion, and cannot be certified in advance
(seat theorem §6, Corollary 3; the rationality capstone's closing). The discipline secures the
gate; it does not secure the fall. **That gap is not a defect in the discipline but the limit on
what any discipline of this kind can reach — the same limit, one level down, that separates method
from character.** The engine can bring a verdict to the edge — surface the seat, stake the claim,
name the round it could lose. It cannot take the fall for the one who holds it. Whether the loss
is conceded or reabsorbed, on the morning it comes due, is character, not method, and does not
transfer.

This locates character precisely, which is why it is *not* a fourth Omega type or a question the
framework tracks (the temptation to coin one should be refused — it would re-absorb into the
system the one thing that sits outside it). **Character is upstream of the selector, not downstream
of the method.** S3's selector — who-benefits-who-pays — is a declared bet, but *what makes a
person declare that bet rather than another* is values: the taste that picks the seat. Someone who
cares about status, power, money, or fame picks different seats and a different selector than
someone who cares about the truth; run the primary vices and virtues up and down the list and each
selects differently. The engine cannot reach that level and does not try to. It operates *after*
the seat is picked — surfacing the implications of the declared frame (§7) — and the picking is
done by a character the method neither installs nor inspects. This is the honest terminus: the
discipline secures declaration; character chooses the seat and decides, when the round is lost,
whether to take the fall. Neither transfers, and the engine claims neither.

---

## Open items for the next pass on this document

*(What this draft has not yet done — not load-bearing claims.)*

- **Structure now mirrors the papers (axioms / consequences / empirical / open), per the ruling.**
  Remaining: the §2 axioms should be checked word-for-word against the seat theorem's numbered
  corollaries and v6.13.1 §2's Axiom 1–6 / §3 Theorem 1–4 statements; this draft paraphrases and
  may drift terminologically. The Axiom-numbering here (S1–S3, T, R) is this document's own and
  should be reconciled with — or explicitly distinguished from — the papers' numbering so a reader
  does not conflate "Axiom S1 here" with "Axiom 1 in v6.13.1."
- **Clause citations** (`power_role_heuristic`, signature_detection.pl:749, §5.4 reachability,
  §4.4 grade-distinction) are now grounded in the FNL ruling and v6.13.1, but should be re-checked
  against live code if v6.13.1 has itself drifted from current source.
- **The FNL doc correction to v6.13.1** is settled as fact (ruling (a)); the version-and-land is a
  separate action requiring the v6.13.1 edit (per the user's "version it" instruction) — draft the
  errata entry and the §4.2/§4.4 replacement text, land as v6.13.2 or a §4.3-style errata note.
- **The constitutively-open questions (§6) each want an Omega-log entry** with their Ω-type, so
  they are tracked as the framework tracks blockers rather than restated each session. The
  selector question (S3-to-itself) and the operationalization question are both Ω_P; the
  symmetric-conflict question is Ω_C-or-non-configuration.
- **Long-term empirical validation (principle, not yet a specific test).** Two probes belong on
  the long horizon, once the engine works well enough to pose them: (a) **ε-frame stability** —
  do independent generators authoring the same kernel produce stable ε, or does ε vary by author
  enough to undermine the within-frame observer-independence Axiom R and §7 claim? (b)
  **false-positive rate on high-trust cases** — run the engine on constraints a careful observer
  would call genuine coordination (the canonical hard cases for an extraction-meter), and check
  whether the selector (Q1) overfires, flagging extraction where there is coordination. This is
  the constructive form of "make the selector able to lose a round" (§2, Axiom S3): a documented
  case where Q1 misleads weakens the selector-bet first and most. Neither is a current test — the
  engine is not yet at the point of posing them cleanly — but the *principle* is recorded: the
  soft spots (ε, the selector) get pressure-tested empirically when the engine is mature enough,
  not left as permanent declarations.

- **Document scope: this is a Claude-Code-facing document.** No table of contents, glossary, or
  how-to-read preamble — the audience is the engine's own development, and the recurring
  vocabulary is meant to be read as recurrence (§3), not flattened into fixed definitions. A
  human-facing version, with the scaffolding a general reader needs, becomes reasonable once the
  framework has developed further; it is explicitly deferred, not omitted by oversight.
