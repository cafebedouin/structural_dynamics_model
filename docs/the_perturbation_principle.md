# The Perturbation Principle

*Invariance under perturbation is how the engine tells the real from the seated — one move, run on the observer, the axiom, time, and the apparatus itself. (It is the engine's instrument for that separation, not the whole of the engine: §7.1 marks what it excludes.)*

**Status:** v0.1 draft — theoretical foundation. Two companions: `docs/design/the_perturbation_move.md`
(the same claim illustrated in the substrate, code-facing) and `docs/the_perturbation_principle_accessible.md`
(*"What Stays and What Moves"* — the underlying idea for a general reader, system-specifics stripped).
The accessible version is the load-bearing modesty check on this one: where the two disagree in
emphasis, the accessible version's narrower claim is usually the honest one, and §1/§2/§7.1 below were
tightened against it. Builds on
`debugging_philosophy.md` (the trifurcation), `cyclopean-point.md` (disparity-as-signal),
`seat-theorem-v1.md` (verdicts are seated), `docs/design/design_discipline.md` (Axiom R, §3
recurrence), `omega_variables.md` (the Ω types), `two_hub_architecture.md`, and
`commitment_systems/commitment_systems_sketch_v5_2.md`. This document does not introduce new
machinery; it names the move all of that machinery already makes.

---

## 0. The declared seat

By the framework's own law (`design_discipline.md` §0, seat theorem §8) this document is seated and
says so first. **The seat: that there is a single move under the engine's many instruments, and
naming it is worth more than cataloguing them separately.** This is a unification bet — it could be
wrong by being too coarse (collapsing distinctions that earn their keep) or by being a framing lens
that finds its own shape everywhere (the §3-recurrence skeptic's charge, `design_discipline.md` §3).
It is staked, not proven. What defends it is the same thing that defends §3: independent instruments,
built at different times for different jobs, turn out to have the same signature — and that
convergence is evidence, not decoration. **But that defense is itself an invariance claim and §6
binds it, so §7.1 runs the control rather than asserting the convergence; the result narrows this
seat to a bet on *form*-unity (the move recurs) and explicitly declines the stronger bet on a single
metaphysical fixedness.** Read §0 as already bounded by §7.1.

---

## 1. The principle, in one sentence

**Hold everything fixed but one dimension; vary that dimension; read off what stays and what moves.
What stays is invariant — defer to it as real. What moves is variant — it is seated, and the
movement is the depth.**

That is the engine's read, stated once — every diagnostic that *separates the real from the seated*
is an instance of it. (Not every computation the engine runs: its single-setting machinery — MaxEnt,
the override layer, purity propagation — produces the values the move perturbs and is not itself the
move; §7.1 draws that boundary, and it is what keeps the claim from being vacuously universal.) The
rest of this document says which dimensions — three especially-useful axes (not a proof that there
are exactly three: you can perturb any dimension you can hold-the-rest-fixed and vary, and the
reflexive fourth is one such), heterogeneous in level — and why "invariant vs variant under
perturbation" is not a metaphor for the framework's commitments but their operational form.

---

## 2. The three axes are the trifurcation, read as perturbations

`debugging_philosophy.md` sorts apparent contradictions into three engineering kinds, and
`design_discipline.md` (Axiom T, §3) makes that trifurcation the generative seed. The seed has so
far been read two ways: as a taxonomy of **failures** (which kind of paradox is this?) and as a
taxonomy of **resolutions** (the Ω types: measurement, definition, stakeholder). This document reads
it a third way, which unifies the first two: **the trifurcation is a taxonomy of the dimensions you
can perturb.** A failure of Type X is what happens when axis X varied *without your holding it* — the
unmarked perturbation. The method is the same perturbation run *on purpose*.

| Axis | Trifurcation type | What you vary | What a careless variation looks like | The instrument |
|------|-------------------|---------------|--------------------------------------|----------------|
| **A — time** | Type A: drift (unmarked state mutation across stages) | the time index t | a reference frame mutating unmarked between t0 and t1 (drift read as paradox) | the lifecycle / drift machinery (`transition_paths.pl`, `drift_events.pl`): drift velocity, acceleration, terminal attractors |
| **B — axiom** | Type B: structure (axiomatic inconsistency) | the kernel's foundational commitments / which reading's axioms | two incompatible axiom sets treated as one position | `axiom_diff.pl` (cs_axiom + grounding); cross-kernel diff (the Westphalia near-kernels, OQ-58/59) |
| **C — observer** | Type C: ambiguity (indexical underspecification) | the observer position (P,T,E,S) | one question packing many indexed queries (a perspectival gap read as a single verdict) | `reading_diff.pl` (the authored (P,T,E,S) cells); the observer axis / Hub 1 + Hub 2 |

The diagnostic question debugging_philosophy asks ("which axis drifted unmarked?") and the
constructive method this document names ("perturb that axis on purpose") are the same operation run
in opposite directions: the first *recovers* the axis that moved when it shouldn't have; the second
*moves* an axis deliberately to see what depends on it. Failure-diagnosis and invariance-measurement
are one instrument pointed two ways.

**Two cautions the perturbation reading must keep that the *failure* reading does not need.** First,
**these three are useful, not exhaustive.** Axiom T (`design_discipline.md`) claims the
*failure-kinds* are predominantly three; the *perturbation* reading inherits no such closure — you can
perturb any dimension you can hold-the-rest-fixed and vary, and the reflexive fourth axis (§5) is the
standing proof that "three" is a convenience, not a count. Treat A/B/C as the three axes that map onto
the trifurcation and earn their keep most often, not as a partition of all possible perturbations.
Second, **the three are heterogeneous in level** — C is *positional* (where you stand), B is
*epistemic* (what you assume), A is *temporal* (when you ask). They are not three coordinates of one
space; they are three different kinds of "hold the rest, vary this." Naming that heterogeneity is not
housekeeping: it is *why the form can recur while the yield differs* (§7.1). A move that means the same
formal thing across levels of different kinds has no right to assume it *delivers* the same kind of
result at each — and §7.1 finds, on control, that it does not.

---

## 3. Why "invariant vs variant" *is* the framework's commitments, not a restatement of them

The principle is not a new claim laid beside the seat theorem, Axiom R, and the cyclopean point. It
is their shared operational core. Three identifications:

**(a) Invariant-under-perturbation = situation-fixed = defer to reality (Axiom R, realist half).**
`cyclopean-point.md` draws the only line that matters between objective and seated: a parameter the
*situation* fixes is discovered (perspectives converge on it — the asymptote's territory); a
parameter *you* fix to say anything is a standpoint (perspectives don't converge, they structure).
Perturbation operationalizes that line **per axis**: a property that does not move when you vary axis
X is, over X, situation-fixed — the mountain pole of `design_discipline.md`'s Axiom R, the thing no
seat reclassifies, witnessed (not decreed) as zero divergence under indexing. *Defer to it.*

**(b) Variant-under-perturbation = the open parameter = the seat = the depth (cyclopean
disparity; Axiom R, deferential half).** A property that *does* move when you vary axis X is, over X,
seated: the situation left it open and the perturbation reveals which way it was set. The size and
structure of the movement is exactly cyclopean-point's *disparity-as-signal* — the depth no single
position discloses, read off the disagreement. `reading_diff`'s disparity cells, `axiom_diff`'s
grounding mismatch, the drift trajectory: each is a measured disparity, the standpoint-set content of
the verdict.

**(c) The seat of a verdict *is* the perturbation that flips it (seat theorem S1).** Seat theorem
Corollary 1: a verdict is seat-free iff it is contentless. Read through this principle: **a verdict
is seated in axis X exactly when some perturbation along X changes it.** A contentless verdict is one
no perturbation moves (it asserted nothing the situation had not already fixed). So "declare the
seat" (`design_discipline.md` S2) and "name the perturbation your verdict is sensitive to" are the
same act. The engine surfaces the seat *by* finding the axis along which the classification is not
invariant. Declaration is not a confession added after classification; it is what the perturbation
read directly produces.

This is why the principle is foundational rather than merely useful: it is the procedure that makes
the central distinctions *measurable* instead of asserted. Without it, "this is a mountain" / "this
is seated" / "this is the depth" are claims; with it, they are read off whether a held-everything-
else-fixed variation moves the verdict.

---

## 4. Noise vs disparity = vanishes vs persists under perturbation

`cyclopean-point.md` needs, and supplies, the line between *noise* (disagreement that shrinks as you
aggregate — error around a fixed value) and *disparity* (disagreement that persists and structures as
you aggregate — different settings of an open parameter). The perturbation principle restates this as
the test for whether a variation is signal:

- **A property that converges as you add perturbations was never seated in that axis** — the
  variation was noise around a situation-fixed value (the asymptote's territory; an uncontested
  parameter). Report it as invariant.
- **A property that holds stable, position-correlated structure across perturbations is seated** —
  the variation is disparity, the depth. Report it as variant, *with its axis*, and do not average it
  away. Collapsing it is the cyclopean error: fusing two eyes destroys the depth they were for.

The danger this guards (cyclopean-point's social layer): **zero variation has two causes that look
identical — a genuinely flat axis, and a suppressed perturbation never run.** An invariance you
*found* and an invariance you *failed to probe for* present the same flat result. Which is why §6 is
not optional.

---

## 5. The reflexive fourth axis: perturb the apparatus

The three axes perturb the *object* (the constraint, its readings, its history). There is a fourth,
turned on the instrument itself: **perturb the engine's own constants and read which verdicts
survive.** This is `python/sweeps/perturb.py` — vary a config threshold across values, re-run the
classification, measure per-kernel type-stability (fold-survival) — and its consumers (the stability
band in `enhanced_report.py`, the sensitivity sweeps). It answers the same question one level up:
*which classifications are invariant under the choice of calibration, and which are artifacts of
where a hand-tuned number landed near a boundary?* (`design_discipline.md` §5: where you draw a band
boundary is a calibration, not a discovery — so a verdict that flips under a small threshold
perturbation was the threshold's verdict, not the constraint's.)

This is the principle applied to itself, and it is the honest move the framework demands: an engine
that perturbs observer, axiom, and time but treats its own constants as fixed has an undeclared
seat (the calibration). The reflexive axis declares it. The `ε`-stability rule
(`design_discipline.md` §7) is the same discipline for the one authored primitive: a cross-axis
anchor must not flip under small `ε` perturbation, or it is an artifact of authoring, not structure.

---

## 6. The discipline the principle forces (and why it is the build-discipline spine)

An invariance claim is **the null result one level up** — and `build_discipline.md`'s spine applies
exactly: *a clean read is byte-identical to a read that didn't look.* "Invariant under perturbation"
is byte-identical to "I never perturbed hard enough," "I perturbed the wrong axis," or "my probe
didn't dispatch." So:

**Every claimed invariant requires a perturbation you know in advance must move a seated verdict —
the positive control.** Self-diff is the canonical one (`reading_diff`, `axiom_diff`): a reading
diffed against itself must return all-invariant under the strict key; if it does not, the operator
is broken and its invariance claims are worthless. Only once the probe is shown to *find* variance on
a known-variant case does its "invariant" on the real case carry information. This is not an extra
rigor bolted on — it *is* `build_discipline.md`'s "every diagnostic needs a positive control,"
recognized as the same law: the perturbation is the diagnostic, invariance is its null result, and a
null result is unfalsified until the probe fires on a positive control.

Two further disciplines, both already enforced in the substrate:

- **Declare the axis with the verdict (never bake it).** The perturbation key is the seat
  (§3c). It must be an explicit, caller-supplied parameter, never a silent default — because a
  baked key is a concealed seat (S2's no-seat pose). Witnessed: `reading_diff` *throws* rather than
  default-bake a `weighted` alignment; `axiom_diff`'s concept map is empty by default and the report
  says so. A perturbation operator that picks its own axis silently is performing the no-seat pose.
- **One axis at a time; hold the rest.** Vary two axes at once and you cannot attribute the movement
  — the partition between invariant and variant blurs. The whole power of the move is that the held
  dimensions make the varied one's contribution legible. (This is why the engine localizes
  indexicality to specific derivations rather than letting it leak everywhere — `two_hub_architecture`,
  Axiom R: everything not on the varied axis is observer-independent data or deterministic transform.)
- **The method relocates disagreement; it does not dissolve it — and that is the gain.** Because the
  axis is the seat (§3c), two analysts who perturb different axes can read different invariants, and
  the method does not adjudicate between them. What it does is move the dispute from the unfalsifiable
  "who is right" to the answerable "which axis matters here" — and that relocation *is* the progress
  the move buys (it is S2 again: the seat is now shown, so the disagreement is about a declared thing).
  The failure that masquerades as using the method is asserting complexity without naming an axis:
  "it's a matter of perspective" with no perturbation actually run is the null result uncontrolled —
  a refusal to read the disparity dressed as having read it.

---

## 7. What recurs, and why that is the evidence

The principle's claim to be foundational rests on the same footing as `design_discipline.md` §3: it
is not asserted top-down but read off independently-built instruments that turn out to share a
signature.

- The observer operator (`reading_diff`) and the axiom operator (`axiom_diff`) were built days apart
  for different layers, and have **the same shape**: a declared alignment key (the axis/seat), a
  partition into agreement (invariant) / disparity (variant) / blind (uncovered), and an
  order-independent stability verdict across a declared key set. Neither was designed to match the
  other; they match because the move is one move.
- The drift machinery (axis A) predates both and computes the same partition in time: what is stable
  across the lifecycle vs what drifts, with terminal attractors (husk, axiom_foreclosure) as the
  structure the temporal disparity resolves toward.
- `perturb.py` (the reflexive axis) is, by its own docstring, *the single primitive that unifies the
  type-stability sweep family* — which is itself an instance of the principle recognizing itself: the
  bespoke sweeps were all the same move, so one primitive serves (collapsing them onto it is the
  in-progress consolidation, `build_discipline.md` Pattern 3 / faith-merge).

### 7.1 — The recurrence claim is itself an invariance claim, and §6 binds it

"The same signature recurs across four independently-built layers" is an *invariance* — invariant
under the choice of which instrument you look at. By §6's own law it is the null result one level up,
and carries no information until a control shows the framing can *fail to fit*. Without that control,
"the recurrence is evidence" is byte-identical to "my framing finds its own shape everywhere" — the
exact §3-recurrence charge (`design_discipline.md` §3) that §0 staked and this section was meant to
answer. So the control must be run here, in two parts.

**(a) The framing must correctly EXCLUDE — and it does.** Most of the engine is *not* this move. The
MaxEnt classifier computes one distribution at one ε; the signature-override layer applies a
single-pass reclassification at one setting; the purity/contamination network propagates along edges;
H¹ and Arakelov height measure obstruction of a fixed configuration. None is perturb-hold-read: none
varies an axis and reads invariant-vs-variant; each emits a verdict at a single setting. The framing
correctly reports them as not-fitting. (The tell that this is a real boundary, not a dodge: the Fisher
ε-sensitivity probe *built over* the MaxEnt classifier **is** the reflexive move — the classifier is
the single-setting computation, the sensitivity probe over it is the perturbation. The move is the
probe, not the thing probed.) Because the engine has large tracts the framing excludes, the
recurrence across the four perturbation instruments is not vacuous — the lens discriminates.

**(b) The within-scope NEGATIVE control — the axiom layer — promoted from taste to verdict.** §6
demands a place the signature *should* break, tested. Ω_P named one and this document previously
filed it as a taste question, which is exactly the untested-neither §6 forbids: **the axiom layer has
no mechanical alignment key.** At the observer layer `exact` (P,T,E,S) aligns cells mechanically
(readings reuse the canonical tuples), so the invariant/variant partition is *read off the substrate*
with zero hand-authoring. At the axiom layer `exact_name` is all-blind (0 within-kernel reading-pairs
share an axiom name — every reading authors bespoke names), so the *only* non-degenerate key is the
hand-declared `axiom_concept/2`. Run the control: **this is a real divergence, not an absorbed
surface difference.** The *form* recurs (declared key → partition → verdict), but the *epistemic
yield* does not: at axes C, A, and the reflexive axis the invariant is **discovered** (a mechanical
key separates situation-fixed from seated); at axis B the invariant is **constituted by the seat** —
with no concept map there is no invariant at all, only blind. So "same signature" is true **at the
level of form** and overstated if read as "same thing each time."

**What §7 therefore establishes — scoped to what the control licenses.** The evidence supports: the
engine has a **recurring method** (perturb-hold-read), discriminably present in four instruments and
absent from its single-setting machinery — *not* the stronger claim that the four invariances are one
underlying fixedness. The axiom-layer divergence tilts Ω_P toward its anti-unification horn: the
unification is of **form/method**, and it flattens a genuine difference in yield (mechanical-discovery
vs seat-constitution) if read as more. §0's seat is hereby narrowed to what survived its own control:
**a bet on form-unity, not on a single metaphysical fixedness.** The engine still does not *apply* the
principle — the principle is read off what the engine already does (`design_discipline.md` §1) — but
"read off" now means *off the form*, with the yield-divergence kept in view, not averaged away (which
would be the cyclopean error, §4, committed against this document's own evidence).

---

## 8. Open questions

- **Ω_C — is "perturb A, B, C, and the apparatus" exhaustive, or are there axes not yet named?**
  The committer/observer split suggests B and C are themselves coarse (B bundles axioms and which-
  reading; C bundles all of P,T,E,S). Whether the right granularity is four axes or a finer lattice
  is a conceptual question, not a measurement.
- **Ω_P — does the unification earn its keep? Partially adjudicated (§7.1), no longer pure taste.**
  The negative control was run: the axiom layer's missing mechanical key is a **real divergence in
  yield** (form recurs, discovery-vs-seat-constitution does not), so the unification is established
  *at the level of form/method* and overstated beyond it. The residual Ω_P is narrower: granting that
  the form unifies, does naming it as one move clarify (one positive control, one discipline, one
  shape for the next operator) more than it obscures (the yield-divergence it must keep flagging)?
  That remainder is preference-shaped — declared, defended, able to lose a round — but the empirical
  half is no longer open: the form-unity is witnessed, the yield-divergence is witnessed.
- **Ω_E — staked, not neutral (§0 committed the seat, so this commits the prediction).** §0 bet
  form-unity; §7.1's control found the yield diverges at the axiom layer. The honest empirical
  consequence is therefore a **bet on cross-cutting**: invariance under one axis should *not* strongly
  predict invariance under another — a constraint can be observer-invariant (a mountain) while
  axiom-variant (its readings ground it differently) and time-variant (drifting). The corpus already
  shows one such case — `absolute_sovereignty` is observer-`robustly_binocular` against its siblings
  yet its axiom *grounding inverts* (variant on B, structured on C) — which is cross-cutting in a
  single constraint. So the prediction is: measure cross-axis correlation of invariance and find it
  **weak/positive-but-far-from-1**; strong correlation would *surprise* the form-unity reading and
  upgrade it toward a single metaphysical fixedness (the horn §7.1 declined). This is the falsifiable
  consequence of §0's seat, and it points toward method-not-metaphysics. Measurable once the corpus is
  worth calibrating against.
