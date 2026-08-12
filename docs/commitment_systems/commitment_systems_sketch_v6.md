# Commitment Systems and the Architecture of Drift — v6

*Full self-contained reissue, 2026-08-06. Supersedes v4 + v5 + v5.1 + v5.2 read together (the
prior documents stay on disk as history, each carrying a superseded-by header). This is the
framework's marked-revision pattern applied to itself, at reissue scale: the v4→v5.2 chain had
grown four links deep, and since v5.2 (2026-05-25) whole subsystems were built in code with no
spec section — the same Type-A spec-vs-code drift v5.2 acknowledged for two pattern atoms, now
at subsystem scale. This version records the acknowledged-drift event, absorbs the missing
subsystems, and puts the spec's machine-checkable enumerations under a gate-integrated tripwire
so the third occurrence is caught by a checker rather than an audit.*

*Code citations use section name plus number ("v6 §6 (Drift-terminal subsystem)") because
numbers renumber and names survive. Every code-facts claim in this document was re-verified
against the code at write time (2026-08-06). Ruled content and candidate/proposed content are
kept typographically distinct: PROPOSED marks a structure no operator ruling has adopted.*

---

## 1. What changed from v5.2

### 1.1 The acknowledged-drift event

The framework's preferred acknowledgment mode, applied to itself:

**Kernel:** the formal specification of the commitment-systems framework, as encoded in the
sketch chain (v4 as last self-contained document, plus the v5/v5.1/v5.2 deltas).

**Drift:** between 2026-05-25 (v5.2) and this reissue, the following were built in code with no
spec section — the operational layer absorbed new structure while the formal layer remained
nominally unchanged, which is this framework's own definition of interpretive accretion at the
spec layer:

- **Drift-terminal subsystem** (`prolog/cs_drift_engine.pl`): six terminal attractors, the
  gap descriptor (7 directions × 3 magnitudes × acknowledgment bit), the acknowledgment
  principle, authored-ack provenance, and the surviving-referent precondition (OQ-227).
  Doc-side presence before this reissue: one line in `observer_diagnostics.md`.
- **Axiom layer** (`prolog/cs_axiom_engine.pl`): axiom statuses, contradiction pairs, the
  computed-only foreclosure route, and two finding surfaces.
- **Kernel-obstruction layer** (`prolog/cs_kernel_registry.pl`): the reading-site gluing
  obstruction and its four statuses, plus the per-context agreement trichotomy.
- **Trifurcation router** (`prolog/cs_trifurcation.pl`): within-kernel Type A/B/C routing with
  fail-closed `unknown` (OQ-55).
- **Cross-axis mismatch detector** (`prolog/cs_drift_mismatch.pl`): the false-mountain
  cross-axis diagnostic.
- **`scaffold_suppression_escalating`** verdict (`prolog/cs_pattern_detection.pl`, OQ-39,
  commit `062507fc`): in code, absent from the spec AND from its own module's header list.
- **The Type-B adjudication** (`type_b_adjudication_2026-07-23.md`): an operator ruling on what
  the framework IS — the theory of the blocked-B remedy gate — living outside the sketch chain.

**Acknowledgment:** this version. Each subsystem gets a spec section (§§5–9), the adjudication
is absorbed as §3, and the enumerations go under the tripwire below.

### 1.2 The versioning rule (stated while being broken)

The chain this reissue ends never had a written rule for when a delta suffices and when a
reissue is due. Stated now, so the next multi-section change does not re-litigate it:

- **Delta document (vN.x):** exactly one marked revision to one section. The rest of the prior
  version is carried by reference and remains authoritative.
- **Full reissue (vN+1):** triggered when a change touches more than one section, OR when the
  delta chain reaches depth ≥ 3 (reading the current spec should never require holding more
  than two documents).

By this rule the v4+v5+v5.1+v5.2 chain (depth 3, and this change touches many sections) was
past both triggers. The reissue is the rule's first application — applied late, and marked.

### 1.3 The spec-enum tripwire

Manual spec/code sync has now failed structurally twice (two pattern atoms at v5.2; six
subsystems at v6). The fix is not more write-time discipline — it is a gate check, in the same
family as the `terminal_set_pinned` tripwire. Every machine-checkable enumeration in this
document sits in a fenced sentinel block (`<!-- spec-enum: <name> -->` … `<!-- /spec-enum -->`).
`python/spec_enum_check.py` (wired into `scripts/gate.sh`) holds a manifest of required enum
names, fails loud if any sentinel block is absent — a rewritten section that drops its sentinel
goes RED, never silently green on nothing-to-diff — and diffs each block against its code pin.
Its positive controls (add-atom, remove-atom, deleted-sentinel) ride every run. If you edit an
enumeration in code, this document goes red at the gate until the matching block is updated —
that is the point.

### 1.4 Correction-lineage table

One row per marked revision in the v4→v5.2 chain, so the acknowledgment history stays legible
inside the reissue. Without this table the reissue would itself be interpretive accretion at the
spec layer — absorbed corrections with the marks sanded off.

| Claim / change | Introduced | What it superseded | Current status |
|---|---|---|---|
| Interpretive accretion redefined substrate-neutrally; two sub-species (text-anchored, principle-anchored) | v5 §2 | v4's "the text is fixed" definition (internally inconsistent with its own common-law example) | Carried — §2.3, §4 |
| Sub-species failure modes assigned (semantic detachment vs coherence-absorption strain) | v5 §2 | v4's single undifferentiated accretion failure mode | Carried — §2.4 |
| Interpretation-layer condition corrected (three-layer schema/prompt/Prolog inconsistency documented; intended condition keyed off authority) | v5 §3 | The schema's kernel-only gate | Carried — §4.3; the intended condition is documented in code (`cs_pattern_detection.pl` `cs_interp_layer/1` comment) but the flag is still consulted only at the anchored-fixity fork |
| `noncanonical_formalized` rename retired (redundant against the field enum) | v5 §4 | A proposed sharpening | Still retired — recorded so it is not re-proposed |
| Committer/ground axis recorded as a bundle null | v5 §5 | The possibility-space exploration | Superseded by the v5.1 split (below) |
| Bundle null split: beneficiary/victim bit graduated (ε-independent, bounded); everything else standing null on sharper grounds | v5.1 §§5.1–5.5 | v5's single undifferentiated null | Carried — §10 |
| `natural_law_constraint` + `epistemic_consensus` promoted from unspecced to acknowledged | v5.2 §2 | Seven-atom spec enumeration vs nine-atom code | Carried — §4 |
| Positional-blindness formalization claim corrected: the presheaf/H¹ machinery formalizes disagreement between positions that each produced a reading, **not** the inability to see — a non-reading seat maps to `unknown` and `is_real_type/1` filters it out before H¹ | v6.1 §2.5 | v4→v6's implied "the framework formalizes this," which read as covering both claims of the paragraph | Live — the unable-to-see claim is now marked UNFORMALIZED; build question is OQ-285 |

---

## 2. Core model

This section carries v4's theory, with v5's corrections folded in. Each subsection carries an
explicit marker — **[CARRIED]** means the v4 text is reproduced substantively unchanged
(fidelity checkable by diff against v4), **[CARRIED, abridged]** means the normative content is
carried but illustrative/rhetorical prose is compressed (v4 keeps the full text), **[CHANGED]**
names the fold. v4 subsection inventory: Three
primitives; The structural problem; Five patterns of response; Why systems fail; Different
positions see different things; How to use the framework; The framework as commitment system; A
demonstration; Why this matters; Appendix (Provisional Structural Refinements);
Operationalization status. All eleven appear below.

### 2.1 Three primitives — [CARRIED from v4]

A commitment system has three components.

A **kernel** is the stabilized arrangement of commitments that constitutes the system's nominal
core. This may be a text (a constitution, a foundational paper, a creed), a body of practice
(the methodology of a craft tradition), a narrative (an identity's organizing story), a
relationship (the implicit terms a marriage rests on), or some combination. The kernel is what
participants in the system point to when asked what the system is.

An **authority structure** is the mechanism that determines what counts as legitimate
interpretation of the kernel. Authority structures may be external (a priesthood, a judiciary,
an academic discipline, a board), distributed (the parties to a relationship, a community of
practice, a peer-review system), internal (an individual's self-interpretive practices), or
composite. Authority structures have incentives, resource flows, and stakes in preserving their
standing; treating them as politically neutral interpretive machinery misses what they are.

**Drift** is unmarked mutation of operational practice relative to the kernel. Drift is
intrinsic because the environment the kernel was stabilized within continues to change after
the kernel stabilizes. No commitment system preserves perfect fidelity to its kernel across
time. The question is not whether drift occurs but whether the system's authority structure can
acknowledge drift without losing legitimacy.

The kernel is frame-relative. At any moment T1, the kernel is whatever participants in the
system point to as the reference for that analysis. Drift between T1 and T2 is measured against
the T1 kernel, not against some eternal fixed reference. The kernel itself can change between
T1 and T2, and that change is itself drift to be acknowledged or denied. This treatment makes
systems with evolving kernels analyzable in the same terms as systems with fixed ones.

### 2.2 The structural problem — [CARRIED from v4]

The people with authority to officially acknowledge drift are usually the same people whose
standing depends on pretending there isn't any. A church hierarchy that admits doctrine has
changed undermines its claim to unchanging truth. A constitutional court that admits it is
making new law undermines its claim to just reading old law. A spouse who admits the
relationship has fundamentally changed has to deal with what that means. An AI lab that admits
its alignment framework has drifted has to address what the systems trained against the earlier
framework actually do.

So most systems develop elaborate ways of not admitting drift — reinterpreting old commitments
to mean new things, or quietly doing something different while insisting nothing has changed.

This is the framework's central observation. The question is not whether drift happens. The
question is whether the system can name it.

### 2.3 Five patterns of response — [CHANGED: v5's substrate-neutral interpretive-accretion fold]

Systems handle drift in five characteristic patterns. The patterns are claims about attractor
states in the configuration space of possible commitment system arrangements, not partitions of
all possible systems. Real systems often mix patterns and many of the most interesting cases
occur at boundaries.

*Marked revision.* The kernel is precisely specified. Authority is voluntary and grounded in
expertise. Drift is formalized as revision: someone proposes a change, the change is checked,
and if it works the system absorbs it. Acknowledgment is marked and legible rather than
absorbed silently. Mathematics works this way. Some healthy long-term relationships work this
way. Programming languages with formal proposal-and-deprecation procedures work this way. The
pattern is stable when acknowledgment capacity matches environmental change rate.

*Interpretive accretion* (redefined substrate-neutrally by v5). A stabilized kernel that cannot
be revised directly is paired with a lineage-grounded authority structure. The formal mechanism
for changing the kernel does not function or does not exist, so drift migrates into
interpretation: everyone insists the kernel controls while the operational meaning shifts
substantially. Authority is grounded in continuity with the kernel, whatever its encoding. The
pattern has two sub-species, distinguished only by how the kernel is held:

- **Text-anchored accretion.** The kernel is a fixed text — a constitution, a scripture, a
  founding statute — whose meaning develops through an interpretive lineage. Brahmanical
  commentary on the Vedas, Catholic doctrinal development, and constitutional originalism
  (despite its claim to fixity) work this way.
- **Principle-anchored accretion.** The kernel is a formalized principle with formal apparatus
  (tests, elements, holdings) but no canonical text, developed through a precedential lineage.
  Common-law doctrine is the canonical case. The naming is deliberately substrate-neutral:
  principle-anchored accretion also covers scientific paradigms extended through canonical
  methodological inheritance and standards bodies whose rulings accumulate into quasi-doctrine.

Both are interpretive accretion — the mechanism (lineage-grounded authority over an unrevisable
kernel, drift absorbed into interpretation) is identical; only the encoding substrate differs.
(v4's original definition opened "the text is fixed," which was false for the common-law case
v4 itself named as canonical — the v5 correction; see §1.4.)

*Diffuse reconstruction.* The kernel is under-specified or intentionally ambiguous. No
centralized authority structure exists to adjudicate. Many parties produce mutually
incompatible readings claiming the same source. Platonism in philosophy works this way. The
various interpretations of "alignment" or "safety" in AI development work this way. Terms like
"freedom" or "justice" in contemporary political discourse work this way. The pattern persists
indefinitely but lacks operational coherence. It often serves strategic purposes for parties
who benefit from operational ambiguity.

*Implicit practice.* There is no codified kernel. The kernel is whatever the system does.
Authority is grounded in practice itself. Drift is the mechanism rather than a failure of it.
The UK constitution works this way. Craft traditions transmitting tacit knowledge through
apprenticeship work this way. Long-term partnerships where the relationship is whatever the
parties do together work this way. The pattern is stable as long as practice remains coherent.
Breakdown is severe when practice loses coherence because there is no fixed referent to
reconstruct from.

*Anchored fixity.* The kernel is formalized. The authority structure grounds its legitimacy in
the kernel's unchangeability and extracts substantial benefit from preventing kernel revision.
Drift denial is the source of authority rather than a side effect. The pattern has two
sub-types that produce radically different stability profiles.

Anchored fixity *with* an interpretive-accretion layer beneath the kernel can persist
millennia. The Hindu Vedic-Brahmanical system, post-development Catholic doctrine, and the
Confucian commentary tradition operate this way. The unrevisable kernel is paired with a
substantial interpretive substructure that absorbs drift without surfacing kernel revision.
The interpretive layer does the acknowledgment work the kernel itself cannot do.

Anchored fixity *without* an interpretive-accretion layer is structurally brittle. The kernel
is supposed to govern operational practice directly. The Spartan Lycurgan system worked this
way. Certain forms of religious fundamentalism work this way. Certain trauma responses that
treat the traumatic event as unrevisable kernel without developing interpretive machinery to
process it work this way. The pattern produces accumulating gap and catastrophic breakdown when
environmental change exceeds what the kernel can govern. Spartan breakdown at Leuctra is the
canonical case at civilizational scale; identity crises in fundamentalist deconstruction are
the same pattern at personal scale.

The framework's strongest cross-domain claim is that anchored fixity without an
interpretive-accretion layer produces brittle-fixity failure identically across organizational,
identity, and relational systems. Treatment approaches that work in one domain may transfer to
others when the structural diagnosis is the same.

### 2.4 Why systems fail — [CHANGED: v5's sub-species failure-mode assignment fold]

Systems do not usually fail because participants are bad or stupid. They fail because the
mechanism for admitting change is broken or captured by people who benefit from things staying
officially the same.

The framework makes this explicit. Each pattern has its characteristic failure mode. Marked
revision fails when environmental change accelerates beyond acknowledgment capacity, or when
the authority structure develops extraction stakes in kernel preservation that did not exist
before. Interpretive accretion fails differently by sub-species (the v5 assignment):
**text-anchored accretion** fails through *semantic detachment* — interpretation visibly
detaches from the text until the gap between text and operational meaning becomes too large to
plausibly reinterpret (cover story: "we are faithful to the text"); **principle-anchored
accretion** fails through *coherence-absorption strain* — the precedent or doctrine graph grows
too large or too internally contradictory to maintain coherence (cover story: "we are applying
established doctrine"). Diffuse reconstruction fails by never resolving operational coherence;
the failure is the persistent condition rather than an event. Implicit practice fails when
practice loses coherence through generational discontinuity, environmental disruption, or
authority structure breakdown. Anchored fixity without interpretive accretion fails
catastrophically when environmental change exceeds kernel-governance capacity. Anchored fixity
with interpretive accretion fails slowly through accumulated unrecognized drift that the
interpretive layer can no longer plausibly absorb.

These are structural predictions, not contingent observations. The pattern plus the
environmental rate plus the authority structure's incentives produce the failure mode.

### 2.5 Different positions see different things — [CARRIED from v4; citation updated to the v8 paper; **v6.1 (2026-08-12): the formalization claim CORRECTED — see the marked paragraph**]

Within any commitment system, participants at different positions see different things.
Insiders, external observers, beneficiaries, victims, reformers, conservatives — each position
produces a different classification of the same kernel-practice relationship. The disagreement
has structure rather than being noise. Some positions are systematically unable to see drift
they participate in producing, because seeing it would threaten the authority structure that
grants them their position.

This is not a moral claim about participants. It is a structural claim about how authority
structures with stake in kernel preservation generate predictable blindness. The constitutional
originalist cannot easily acknowledge that their preferred interpretation is itself an
evolution of original meaning. The church hierarchy cannot easily acknowledge that doctrinal
development is doctrinal change. The institutional reformer cannot easily acknowledge that
their proposed reform serves institutional interests. The identity-anchored person cannot
easily acknowledge that their commitments have shifted operationally.

The disagreement between positions is data, not problem. Different perspectives on the same
system produce unreconcilable accounts because the structural mechanisms they pass through
differ across positions. The framework formalizes **this** through presheaf machinery developed
in companion work (the Deferential Realism engine; entry point
`docs/deferential_realism_paper_v8.md`) — observer positions form a category, classification
data forms a presheaf that often fails the sheaf gluing axiom. The technical detail is not
necessary to use the framework.

> **[CORRECTED v6.1, 2026-08-12 — the formalization covers the disagreement, NOT the blindness.]**
> The paragraph above this one makes a second claim — that some positions are *systematically
> unable to see* drift they participate in producing — and the sentence just above appeared to
> offer the presheaf machinery as its formalization. **It is not one, and the code is explicit
> about why.** H¹ measures disagreement among positions that each **produced a reading**. A
> position that cannot produce one is filtered out *before* H¹ is computed: a seat whose type
> derivation fails maps to `unknown` (`stakeholder_seats.pl:337-341`), and
> `is_real_type/1` removes it, so the obstruction — in that predicate's own words — "counts it as
> neither agreeing nor disagreeing." The same token also carries a seat that derives a literal
> `unknown`; both are collapsed as "untypeable for pair-counting purposes." **So the engine's
> disagreement measure is silent exactly where a position is blind, and cannot distinguish a blind
> position from an absent reading.** What survives is the *count* — `stakeholder_obstruction/5`
> carries `NSeats`/`NReal` in-band — never the *kind*.
>
> The unable-to-see claim therefore stands as this document's own structural assertion, presently
> **unformalized**, not as a described output of the engine. Whether the gauge axis should carry
> an "expressible-from-here" predicate distinct from "types-to-X-from-here" is **OQ-285**;
> `design_discipline.md` §5 governs whether the category is addable at all, since Mode-3 blind
> cells are by construction undetectable from inside the system that has them. Background:
> `docs/concealment/concealment_without_a_concealer_v0_4.md` §5.3 (mode 3, positional blindness). The structural insight is that disagreement between positions
can be a measurable feature of the system rather than evidence that someone is wrong.

The framework's analytical position is also a position. The analyst sees patterns participants
cannot access from their positions, but this exterior vantage is constitutive of the analytical
move rather than incidental to it. The framework does not claim the analyst sees truth while
participants see illusion. It claims the analyst's position permits structural description that
participant positions cannot produce, and that this is the same kind of positional difference
the framework analyzes everywhere else.

### 2.6 How to use the framework — [CARRIED from v4, abridged: closing worked examples compressed]

When examining any commitment system, four questions arise in order.

*What is the kernel and the authority structure?* Many disputes about commitment systems turn
out to be disputes about which kernel is operative or which authority is legitimate. Specifying
these is the precondition for further analysis.

*Is drift occurring, and is it acknowledged?* Drift is intrinsic. The question is whether the
system can name it. The acknowledgment pattern is diagnostic of the configuration.

*Which pattern is the system in, and is the pattern functional in this environment?* The five
patterns produce different failure modes. Environmental rate matters. Authority structure
incentives matter. Functionality assessments are conditional on environment and incentive
structure; they produce probability distributions over trajectories, not deterministic
predictions.

*Who has standing to acknowledge drift, and do they benefit from pretending it does not exist?*
This question is more prominent in relational and identity systems than in organizational
systems because acknowledgment in those domains is structurally distributed. It generalizes to
organizational systems through the structural distribution of who benefits from acknowledgment
and who pays for it.

The framework provides scaffolding for thinking about these problems clearly. It does not
resolve the substantive questions any particular commitment system faces.

### 2.7 The framework as commitment system — [CARRIED from v4, abridged; §1.1/§1.3 cross-references added]

The framework analyzes commitment systems. It is itself a commitment system, and treating it as
one is part of the discipline the framework requires.

Its kernel is the structural claims developed in this document. Its authority structure
consists of whoever adopts the framework and applies it. Its drift is real: the framework's own
commitments change as it gets applied, criticized, and revised — §1.1 records the latest
instance. Its acknowledgment capacity is whatever revision discipline its practitioners
maintain — now partially machine-enforced (§1.3).

The framework's own preference for marked revision over interpretive accretion is not a bias to
be apologized for. The framework's purpose is making drift visible. Marked revision is what
drift becoming visible consists of. A framework that did not prefer marked over absorbed would
be a framework that could not make the distinction it exists to make. The framework is not
neutral about visibility. The framework is neutral about which patterns are viable:
interpretive accretion is adaptive across millennia in the right environments; implicit
practice is stable under appropriate conditions; anchored fixity with interpretive accretion
can produce extreme longevity. The analytical position is committed; the pattern assessments
are not.

### 2.8 A demonstration: the environment-conditional stability of mathematics — [CARRIED from v4, abridged: case detail compressed, diagnosis intact]

The framework's value test is whether it yields analytical moves that domain-specific framings
don't naturally produce. One demonstration.

Mathematics is conventionally treated as the paragon of stable knowledge. The framework's
prediction about marked-revision configurations supports this characterization, but adds a
calibration condition: stability is environment-relative. Mathematics' acknowledgment machinery
is calibrated for a specific environment — proofs checkable by human readers within reasonable
time, a literature small enough for experts to track. That environment has been changing:
computer-assisted proofs (the four-color theorem; Hales' Kepler proof, whose referees spent
four years unable to verify the computational part), subfields proliferating faster than
experts can track them (Buzzard's argument that no single person understands the complete proof
of Fermat's Last Theorem), and proof assistants adopted as the only feasible verification for
some contemporary work (the Liquid Tensor Experiment).

The framework's diagnosis: mathematics' acknowledgment machinery is being stressed by an
environment that now produces drift faster than human-paced verification can track. The proof
assistant movement is bandwidth extension in response to environmental acceleration — not
abandonment of the architecture but recalibration of it. The conventional framing (mathematics
as paragon of stability) obscures what mathematicians are doing; the framework names it.

The same analysis applies to any commitment system facing environmental acceleration. AI
development is the contemporary case where the analysis matters most: training objectives,
evaluation criteria, and safety specifications are kernels; labs, deployment institutions, and
regulatory bodies are authority structures; distribution shift, reward hacking, goal
misgeneralization, and emergent behaviors are drift. The framework predicts AI development will
face the same bandwidth-extension challenge mathematics faces, complicated by capability
acceleration on the operational side and rapid authority-structure evolution on the governance
side simultaneously. These predictions are concrete enough to be wrong, which is what makes
them worth testing.

### 2.9 Why this matters — [CARRIED from v4, abridged]

The standard debates about cultural change collapse into moralized disputes — tradition versus
progress, fidelity versus innovation, original meaning versus living interpretation. These
debates persist because they cannot be settled in their own terms.

The commitment systems framework reframes these debates as questions about architecture rather
than values. The question is not whether drift is good or bad but whether the system's
authority structure can acknowledge the drift its domain actually produces, given the rate at
which that domain changes and the incentives the authority structure has. Systems whose
acknowledgment capacity matches their environment's drift rate adapt without crisis. Systems
whose acknowledgment capacity falls behind accumulate gap until something breaks. Naming this
mechanism is what the framework is for.

### 2.10 Provisional structural refinements (v4 appendix) — [CHANGED: compressed, with routing]

v4 closed with an appendix of provisional refinements — working notes, not commitments. They
are carried in compressed form with their current routing; v4 remains the full text.

- **Drift decomposition** (source: environmental/authority/endogenous; relationship:
  unrecognized/exploited) — still a working note; the drift-terminal subsystem (§6)
  operationalizes *direction and magnitude* of gap, not source.
- **Three structural principles** (authority extends only as far as enforcement; drift is
  intrinsic; acknowledgment that changes the system requires standing within the system) —
  the third is now load-bearing: it is the premise of the blocked-B gate (§3).
- **Lifecycle phases** (formation → stabilization → operation → atrophy →
  renewal-or-dissolution) — never operationalized; now a declared absence (§11), with v3
  holding the material.
- **Cross-cutting features** (decoupled formalization, ritualized renewal, velocity mismatch,
  distribution of acknowledgment authority, kernel encoding substrate) — working notes; the
  kernel-encoding-substrate axis was promoted into the accretion sub-species split (§2.3).
- **Default conditions** (nesting, coupling) — working notes, unchanged.
- **Observer parameters** (power, exit, time horizon, scope) — realized: these are the DR
  engine's observer-context indices; the CS layer is the second axis beside them (§9, §10).
- **Trajectory channels** (operational separation, dormant-container activation) — working
  notes; dormant-container activation has a mechanism-level cousin in the quarantined dynamics
  document (reconstruction-from-symbol), cite-not-merge (§6.6).
- **Cover story analysis** — operationalized beyond v4's proposal: the verdict layer (§5) and
  the cross-axis diagnostics (§9) fire on exactly the predicted mismatch shapes.

### 2.11 Operationalization status — [REPLACED]

v4's "Operationalization status (as of 2026-05-17)" is superseded in full by §§4–9 of this
document, which specify the operational encoding per subsystem against the code as of
2026-08-06.

---

## 3. Position in the trifurcation

*New section; absorbs the ruling content of `type_b_adjudication_2026-07-23.md` (which stays on
disk — it carries the chat-provenance detail and the withdrawn counter-positions).*

The `docs/debugging_philosophy.md` trifurcation classifies reasoning defects: Type A (drift —
fix the frame), Type B (kernel failure — revise the axioms), Type C (ambiguity — specify the
index). Where does Commitment Systems sit?

**The ruling (operator, 2026-07-23): types are individuated by their FIX, not their generating
mechanism.** Stage 2 of the trifurcation is a remedy test — does freezing the frame eliminate
the contradiction? — not a mechanism test. Applied to a commitment system, frame-fixing returns
a clean report of a plank replaced: no contradiction, a trivial and uninformative pass, because
a drifting institution is not a confused reasoner. So the presence of drift and time does not
route Commitment Systems to Type A.

**The refinement:** Debugging Philosophy's Type B silently assumed the reviser exists. Russell
got ZF; the Liar got Tarski — mathematics had standing to patch its own kernel and a community
that wanted the patch. That assumption is invisible inside mathematics and false nearly
everywhere else. Drop it and B splits into **revisable-B** (kernel revision is available and
wanted — the Debugging Philosophy cases) and **blocked-B** (kernel revision is structurally
unavailable to the only parties with standing). Commitment Systems is the theory of the second
half: the five patterns are the map of what blocked-B does with drift it cannot ratify.
Interpretive accretion is blocked-B's most successful workaround; anchored fixity without an
accretion layer is blocked-B with no workaround — the one that shatters.

**Blockage is a gate, not a type** (operator, same day). Read as a *split*, revisable/blocked
strains the ruling it is built on — it would mint six cells, not three types. The resolution:
restoring standing is not a fix; it is the enabling condition for running one. Nobody resolves
Sorites by acquiring authority. Blockage is a **gate on remedy execution** — it mints no types,
the trifurcation survives intact at three, and Commitment Systems is the theory of the gate.
This yields the diagnostic's implicit Stage 0 in precise form: not "is the object a reasoning
instrument?" but **"does an agent exist who can execute the fix and wants to?"** A remedy test
run against an institution that returns an uninformative pass is what a closed gate looks like.

**PROPOSED, not ruled (OQ-235): the gate is three-valued.** The binary gate collapses three
closures with disjoint treatments — **vacancy** (no one has standing; diffuse reconstruction;
remedy: build an authority structure), **capture** (someone has standing and benefits from not
using it; anchored fixity with extraction stakes; remedy: change incentives or route around),
**bandwidth** (someone has standing, wants to, and cannot keep up; the mathematics case; proof
assistants as bandwidth extension, not reform). The binary gate cannot tell capture from
bandwidth, and the difference is prescribing revolution vs prescribing tooling. This remains a
proposal; whether blocked-B admits any capability move at all is OQ-235.

---

## 4. Pattern taxonomy

The nine pattern atoms emitted by `cs_pattern/3` (`prolog/cs_pattern_detection.pl`). The
classification always honors the LLM-authored `cs_structure` fields (`kernel_codification`,
`authority_grounding`); verdicts (§5) are commentary, never overrides.

<!-- spec-enum: cs_patterns -->
```
anchored_fixity_brittle
anchored_fixity_with_accretion
diffuse_reconstruction
epistemic_consensus
implicit_practice
interpretive_accretion
marked_revision
natural_law_constraint
no_pattern_match
```
<!-- /spec-enum -->

### 4.1 Dispatch

Classification dispatches on the authored `(kernel_codification, authority_grounding)` pair
(`cs_classify/5`):

| kernel_codification | authority_grounding | Pattern |
|---|---|---|
| `none` | any | `no_pattern_match` `[kernel_none]` |
| any | `none` | `no_pattern_match` `[authority_none]` |
| `formalized` | `expertise` or `distributed` | `marked_revision` |
| `formalized` | `extraction`, interp layer present | `anchored_fixity_with_accretion` |
| `formalized` | `extraction`, interp layer absent | `anchored_fixity_brittle` |
| `formalized` | `lineage` | `interpretive_accretion` (principle-anchored) |
| `fixed_text` | `lineage` | `interpretive_accretion` (text-anchored) |
| `distributed` | `distributed` | `diffuse_reconstruction` |
| `implicit` | `practice` | `implicit_practice` |
| any | `self_enforcing` | `natural_law_constraint` |
| any | `diffuse_epistemic` | `epistemic_consensus` |
| anything else | | `no_pattern_match` `[anomalous_field_combination]` |

The two accretion rows are the v5 sub-species (§2.3). The `(distributed, extraction)` cell has
no row — see §11.4 for what the classifier currently does with it and the three unresolved
options.

### 4.2 The two acknowledged additions (v5.2)

**`natural_law_constraint`** (trigger: `self_enforcing` authority, any kernel). Authority is
grounded in the constraint's own self-enforcement — no external adjudicator, because the
constraint operates whether or not any institution ratifies it. This is not a sixth attractor
in the same sense as the five core patterns: it is a structural precondition that rules out the
commitment-system architecture entirely. Commitment systems presuppose that authority and
kernel are separable (drift is possible because the two can diverge); a self-enforcing
constraint has no such gap. Genuine cases: mathematical limits, physical law, genuine Nash
equilibria with no alternative-suppression mechanism. Political actors frequently claim
`self_enforcing` status for constructed extraction mechanisms — those trigger
`false_natural_law_constraint` (§5).

**`epistemic_consensus`** (trigger: `diffuse_epistemic` authority, any kernel). Authority is
grounded in distributed community consensus about what the evidence requires — no single
adjudicator, but no claim to practice-based implicit authority either. Adjacent to
`diffuse_reconstruction` but differing in grounding: reconstruction's distribution has no
epistemic basis; consensus's distribution is coordinated epistemic practice (peer review,
methodological standards). Failure modes: captured consensus (authority becomes `extraction`
while claiming `diffuse_epistemic`), and acknowledgment bandwidth falling behind the evidence
rate (the §2.8 mathematics case). No verdict predicate exists for it — a carried gap (§11.5).

### 4.3 The interpretation-layer condition (carried from v5 §3)

`interpretation_layer_present` discriminates only at the `(formalized, extraction)` fork.
The v5-documented three-layer inconsistency (schema gate keyed on kernel only; generation
prompt requiring kernel AND authority; Prolog consulting the flag only at anchored fixity)
remains structurally as documented; the intended condition — licensed when
`authority_grounding = lineage` (any kernel) OR (`formalized` AND `extraction`) — is recorded
in the code (`cs_interp_layer/1` comment, `cs_pattern_detection.pl`) but the flag still has no
classification effect for accretion constraints, whose interp layer is *implied* by the clause
(signal atom `interp_layer_implied`), not read from the authored field. A latent inconsistency
noted in code: `privilege_waiver_threshold` asserts interp-present but that fact is not read at
the accretion clauses. Documented, not fixed here.

---

## 5. Verdict layer

Verdict atoms accompany the pattern; they never override it (commentary-grade, per the
verdict-grade distinction: correction-grade verdicts like FNL/FCR/FSM override classification;
CS verdicts annotate). Each fires when the LLM-claimed structure is inconsistent with computed
structural signals; agreement is noise, only mismatch surfaces. Eight verdict atoms
(`cs_verdict/2` clause heads, `prolog/cs_pattern_detection.pl`):

<!-- spec-enum: cs_verdicts -->
```
false_anchored_fixity_accretion
false_anchored_fixity_brittle
false_diffuse_reconstruction
false_implicit_practice
false_interpretive_accretion
false_marked_revision
false_natural_law_constraint
scaffold_suppression_escalating
```
<!-- /spec-enum -->

The seven `false_*` verdicts each fire when their claimed pattern contradicts metric signals
(suppression/theater thresholds, enforcement coordination type, sunset/enforcement structure,
beneficiary presence for `false_natural_law_constraint`).

**`scaffold_suppression_escalating`** (OQ-39, commit `062507fc`) is the eighth and the one this
reissue adds to the spec: it fires when a constraint certifies `scaffold` at any standard
observer context AND its authored `suppression_requirement` series is rising — contradicting
the scaffold "suppression declines over time" expectation. It records the rule-break WITHOUT
reclassifying (rising suppression is not coercion; reclassifying would assert a coercion the
evidence does not show). Unlike the seven, it is gated on `dr_type` = scaffold — ORTHOGONAL to
`cs_pattern` — so its clause sits FIRST in the predicate and commits with `once/1` instead of a
trailing cut, leaving every sibling clause reachable; a constraint can carry this verdict PLUS
a pattern verdict. (Placement/cut gotcha documented at the clause.)

**Carried gaps:** (a) no `epistemic_consensus` verdict (§11.5). (b) Rejection-signal
differentiation is partially landed: `kernel_none` / `authority_none` exist as distinct
signals, but a field pair outside the taxonomy still collapses to the same
`no_pattern_match, [anomalous_field_combination]` as a recognized-but-unhandled pair
(`cs_pattern_detection.pl` catch-all clause) — the shakeout's proposed three-way split
(no-fields / outside-taxonomy / rule-gap) is unbuilt.

---

## 6. Drift-terminal subsystem

*The core addition of this reissue. Code: `prolog/cs_drift_engine.pl`.*

### 6.1 Architecture

Authored t1 in, theory-derived t2 out — the `cs_grounding_mismatch` pattern applied to time:

```
cs_drift_state/3 (authored t1 gap)  →  cs_terminal_attractor/4 (theory table)
                                    →  cs_drift_trajectory/3 (computed t2)
```

t2 is NEVER authored. The generator authors where the system stands relative to its declared
reference frame (`cs_reference_frame/2`, t0); the engine extends that gap vector to a terminal
attractor. The full table is Appendix A.

### 6.2 The gap descriptor

`cs_drift_state(UID, Moment, gap(Direction, Magnitude, Acknowledged))` — UID is the story_uid
surrogate (UUIDv4), never the constraint name (a name-keyed call fails silently; witnessed).

<!-- spec-enum: cs_gap_directions -->
```
authority_erosion
axiom_overriding
codification_collapse
practice_drift
repudiation_pressure
revival_pressure
stable
```
<!-- /spec-enum -->

<!-- spec-enum: cs_gap_magnitudes -->
```
minor
severe
substantial
```
<!-- /spec-enum -->

`Acknowledged` is a boolean. The descriptor space is 7 × 3 × 2 = 42 combinations, each matching
exactly one attractor row (row-disjointness, Appendix A).

### 6.3 The six terminals

<!-- spec-enum: cs_terminals -->
```
axiom_foreclosure
extinction
husk
repudiation
revival
stable_pattern
```
<!-- /spec-enum -->

- `stable_pattern` — drift absorbed; the system continues under its reference frame.
- `husk` — form persists but normative grounding hollowed (Kodashim: the community studies
  Temple sacrifice as memorial, knowing the Temple is gone).
- `extinction` — text and practice both gone; terminal (Hittite/Sumerian).
- `revival` — acknowledged departure plus active reconstruction (Modern Hebrew).
- `repudiation` — legitimacy collapses entirely (post-1945 total-war cases).
- `axiom_foreclosure` — foundational premise falsified; death-in-progress (Aristotelian
  cosmology post-Galileo; unabsorbed empirical refutation).

Named cases are examples, not proofs; the `codification_collapse + severe → extinction` row is
definitional (no text + no practice = extinction), with Hittite/Sumerian as illustrations.

### 6.4 The acknowledgment principle

Acknowledgment rescues substantial drift to `stable_pattern` where an institutional medium
survives to act on it. It cannot rescue severe drift (gap too large) or codification collapse
(the medium itself is destroyed). For codification collapse that is acknowledged and being
rebuilt, the direction should be authored `revival_pressure` instead — acknowledged-and-rebuilding
is not a valid `codification_collapse` state.

`cs_drift_trajectory/3` does NOT depend on `cs_drift_unacknowledged/2` — the latter is a
separate Type-A static diagnostic (§5-adjacent, in `cs_pattern_detection.pl`): it fires on
non-stable, non-minor, unacknowledged gaps. Acknowledged drift still gets a computed terminal.

### 6.5 Authored-ack provenance (OQ-126 Gap 1)

The Acknowledged bit is AUTHORED — the story author's own call. The computed terminal is
therefore CONDITIONAL on taking that bit at face value; it is not a settled honor-vs-reabsorb
verdict (that verdict is seated and floats — a hostile reader can read the same edit as
reabsorption-as-retreat). The JSON surface carries this as provenance:
`cs_drift_terminal_basis: "authored_ack"` plus a `cs_drift_ack_witness` object whose
`confrontation_path: "none_exists"` sentinel states that NO external confrontation path exists
(OQ-107 unbuilt) — absence of a path, not a clean probe. The v8 paper (§5.9) reads the same bit
as the chosen-vs-adaptive boundary and names the same honest residual: the positions most
thoroughly foreclosed are the least likely to author a dissenting reading, so evidence for the
choice/foreclosure distinction is thinnest exactly where formation ran to completion.

### 6.6 Two preconditions on the terminal set

The six terminals are complete only under two preconditions. Both are DOCUMENTED and
change-detected, never machine-enforced — a test cannot verify a precondition about what is out
of scope. The terminal and direction SETS are pinned by the `terminal_set_pinned` tripwire
(`prolog/tests/test_cs_drift_engine.pl`): any added or removed member fires RED, forcing
whoever edits the table to read the scope first.

**Surviving referent (OQ-227, ruled 2026-07-24).** Deep-time referent-dissolution — the
referent a commitment is *about* decays while its internal form and grounding persist perfectly
— is unhandled state-space. It surfaces `sealed_closure`: warm, complete, "checks against
nothing," and unrecognizable by any observer including the keeper. The discriminator from
`husk` is *recognizability, not referent-presence*: a husk is a **recognized** hollowing
(Kodashim knows the Temple is gone); sealed closure is unrecognizable from every standing
vantage. It is undetectable by construction (H¹ = 0, no pre-closure snapshot, no referent to
check against), so the row is declared-open, NOT added — an unreachable enum row with no
possible positive control would counterfeit a finding. Authored-only if ever added; reviver
trigger is an authoring channel for referent-dissolution, not a query. (§11.1.)

**Single-continuant identity (new this reissue).** The six terminals also presuppose that the
system whose gap is authored at t1 is the same single system whose terminal is computed at t2.
Two endpoint regimes break that presupposition rather than the referent: **schism** — the
system's identity splits into two or more successors, each inheriting kernel and authority
claims (currently forcible only into `repudiation` or into two separately-authored systems with
no relation between them); and **absorption/syncretism** — form and practice persist inside a
successor system's frame (discriminator vs `husk`: the grounding is *transferred*, not
hollowed; vs `extinction`: the form survives). Declared absence, tracked as OQ-265; resolution
is a ruling on scope-note vs authored-only terminal per candidate, including a per-candidate
detectability verdict — not a code change. (§11.2.)

### 6.7 Disambiguation: the other axis has a terminal vocabulary too

The DR (observer) axis carries `predicted_terminal_state/3`
(`prolog/transition_paths.pl:231–256`), whose vocabulary is `{piton, snare, tangled_rope,
stable}` — metric-driven predictions over drift *events* in the measurement series. That is a
different vocabulary on the other axis. `cs_drift_trajectory/3` terminals are committer-axis
attractors over the authored gap descriptor. Never conflate them; a "terminal state" claim must
name its axis.

### 6.8 Mechanism-level counterparts (analogical — cite, do not merge)

Three terminals have mechanism-level counterparts in the quarantined diachronic dynamics theory
(`commitment_dynamics_v1.md`): `husk` ↔ symbol-over-dead-commitment; `revival` ↔
reconstruction-from-symbol; `extinction` ↔ reservoir extinction. Per the diachronic quarantine
(§10.4: the dynamics live on their own evidence, outside the framework), these are analogies
that generate hypotheses — they are not a bridge, and no shared formalism is implied.

---

## 7. Axiom layer

*Code: `prolog/cs_axiom_engine.pl`. This section declares the canonical enumeration rather than
reporting the code's two comment surfaces symmetrically.*

**Canonical: the AUTHORED axiom-status vocabulary is `{holdable, overridden}`.**
`narrative_ontology.pl` (the authoring surface, `cs_axiom_status/2` declaration) carries
exactly these two; the generator refuses to author `foreclosed` directly — that is a truth
claim it cannot certify. `foreclosed` is a **computed-only** extension: `cs_axiom_foreclosed/2`
routes an axiom to foreclosed-for-classification when three separately-authored structural
facts compound — grounding `empirically_contingent` (from `cs_axiom_grounding/3`), drift
direction `axiom_overriding` at non-minor magnitude, and unacknowledged. Minor magnitude is
excluded (minor drift self-corrects and lacks the evidential weight for foreclosure routing).
The `cs_axiom_engine.pl` header comment that lists three statuses in one enum describes the
union of authored and computed values; it carries a clarifying note (this reissue) — no code
change, the mismatch is resolved as documentation by this declaration.

Facts: `cs_axiom/3` (UID, Role, Atom — a normative claim held by a reading instance;
UID-keyed), `cs_axiom_status/2` (axiom-level, not UID-keyed), `cs_axiom_grounding/3`
(GroundingType ∈ {empirically_contingent, deontological, conventional, theological,
instrumental}), `cs_axiom_contradiction/2` (symmetric mutual-exclusion pair, declared
selectively by scope — NOT derived from `forecloses` edges, which preserves the
licensed-plurality/real-closure distinction: if contradiction were derived from foreclosure,
licensed plurality would be structurally impossible).

Finding surfaces: `cs_axiom_inconsistent/2` — one reading holds both sides of a declared
contradiction (a Type B finding); `cs_kernel_axiom_conflict/4` — two readings of the same
kernel hold contradictory axioms (a FINDING, not an error: contested kernels SHOULD exhibit
axiom conflicts; that is the structural signature of a genuine reading contest — with
`coexists_with` it reads licensed plurality, with `forecloses` real closure, and a `forecloses`
edge with no declared contradiction is structural pressure only).

---

## 8. Kernel-obstruction and trifurcation layer

*Code: `prolog/cs_kernel_registry.pl` (obstruction), `prolog/cs_trifurcation.pl` (router).*

### 8.1 The reading-site obstruction

Where the observer axis computes H¹ over observer contexts, `cs_kernel_obstruction/4` computes
the gluing obstruction over the READING site: a kernel's readings are the cover, the authored
`cs_reading_relation/3` edges are the descent data. It is observer-blind by construction —
it reads only authored committer edges, never χ or classification — which keeps it
gradient-orthogonal to the observer obstruction (Theorem 7 / detection independence). H¹r =
number of foreclosing reading-pairs (readings that do NOT glue into one global section);
PluralityN = number of `coexists_with` pairs (disagree but glue: both stand).

Status is read off the counts, FAIL-CLOSED on absence: a multi-reading kernel with no typed
edge is `untyped` (gluing status undeclared), never silently glued.

<!-- spec-enum: cs_obstruction_statuses -->
```
licensed_plurality
real_closure
singleton
untyped
```
<!-- /spec-enum -->

- `singleton` — fewer than 2 readings; not contested, no verdict.
- `real_closure` — H¹r > 0: a reading forecloses another; no global section.
- `licensed_plurality` — H¹r = 0, PluralityN > 0: glued as authored plurality.
- `untyped` — ≥2 readings, no edge authored.

Unresolved edges (targets that resolve to no declared reading in the source's kernel) are
quarantined and surfaced loud (`cs_reading_relation_unresolved/4`, the OQ-58 policy: no
auto-repair tier, no plausible-form tier — never silently coerced).

Alongside the edge-driven obstruction, the registry computes the classification-driven
per-context comparison (`compare_kernel_readings/3`): at each observer context a kernel's
readings receive one of three verdicts under the OQ-51 N/A trichotomy — `agree` (every
real-typed reading classifies the same, ≥2 real), `diverge` (real-typed readings differ),
`undetermined` (<2 real readings; `unknown` is an abstention that neither agrees nor
disagrees, carried as a count on every verdict token, never demoting a real verdict).

### 8.2 The trifurcation router (OQ-55)

`cs_reading_trifurcation/3` classifies WHY a kernel's readings disagree, into the §3
trifurcation — the obstruction-status → A/B/C mapping the registry explicitly deferred:

<!-- spec-enum: cs_trifurcation_types -->
```
type_a_drift
type_b_structure
type_c_ambiguity
unknown
```
<!-- /spec-enum -->

Dispatch is on the AUTHORED obstruction edge, refined by two COMPUTED within-kernel
diagnostics:

- `real_closure` → `type_b_structure` — the authored `forecloses` edge is the signal; a member
  reading with a computed `cs_axiom_foreclosed/2` marks the diagnostic `confirmed`, otherwise
  `edge_only` (confirmation, not a gate).
- `licensed_plurality` → `type_c_ambiguity` — distinct declared seats coexist; both stand.
- `untyped` → the ONLY computed verdict: `type_a_drift` iff a member reading carries
  unacknowledged drift (`cs_drift_unacknowledged/2`); otherwise **fail-closed `unknown`** — "no
  authored edge + no drift signal" is didn't-look, not Type-X, and never yields a plausible
  default.
- `singleton` → no verdict (the predicate fails).

The verdict is commentary-grade (annotates the kernel comparison; never overrides any headline)
and carries its provenance inline — `provenance(scope(within_kernel), obstruction(Status),
Diagnostic)` — so the within-kernel scope travels with the value and cannot be misread
cross-kernel. Input-boundary discipline: every input is a per-kernel or per-member-reading
fact; cross-kernel disagreement-labeling stays OQ-56-gated.

---

## 9. Cross-axis diagnostics and deliberate boundaries

### 9.1 The false-mountain cross-axis detector

`cs_drift_mismatch/2` (`prolog/cs_drift_mismatch.pl`) fires when a reading is CS-foreclosed on
the committer axis — `cs_drift_trajectory` reaching `axiom_foreclosure`, and/or
`cs_axiom_foreclosed/2` firing (the strict subset) — while METRIC-STABLE on the observer axis
(no network contamination detected, drift velocity below threshold; infrastructure failures
read as stable, since absence of drift evidence is not evidence of drift). The source term
distinguishes `trajectory_only` from `both(...)`. This is the two-hub false-mountain shape: a
constraint that looks stable by extraction/purity metrics while carrying a foreclosed axiom
structure in the CS layer.

### 9.2 The structural mismatch diagnostics (v5.2 §4, corrected to current code)

All fire on DISAGREEMENT between LLM-asserted CS fields and the metric-computed
`constraint_signature/2` (which reads zero CS-layer fields — the two are independent
assertions, so disagreement is a structural finding):

- `cs_authority_masking/3` — computed signature indicates extraction; asserted authority
  grounding is a non-extraction label (the cover-story family).
- `cs_cover_story_active/2` — triple corroboration: a pattern verdict fires + asserted
  extraction authority + extraction-indicating computed signature.
- `cs_displaced_beneficiary/1` — a naturalized-path authority (lineage, practice,
  self_enforcing, expertise, diffuse_epistemic) that is not genuinely natural by computed
  signature, with a **`cs_reading_relation/3` `forecloses` edge** to a sibling that declares
  extraction authority. (v5.2 described this as linked "via `affects_constraint/2`" — the code
  uses the typed `forecloses` edge and intentionally excludes bare `affects_constraint/2`
  edges, since coexisting sibling readings are not domination relationships. The spec now
  matches the code.)
- `cs_grounding_mismatch/3` — the generalized superset: any asserted grounding structurally
  inconsistent with the computed signature, in both directions (naturalized label over
  extraction signature — the masking family — and extraction label over genuinely
  natural/coordination signatures). Its silent zones (undetermined pairs, an architecturally
  unreachable extraction+CI-rope cell) are documented at the predicate. `cs_authority_masking/3`
  is retained beside it: analytically distinct even though the firing sets overlap.
- `cs_naturalized_mountain/1` — low-ε constraint with extraction/diffuse_epistemic grounding
  and both victims and beneficiaries: invariability-form with extractive force.

### 9.3 Deliberate boundaries (declared scope, not omission)

Each of the following is a decision, verified still true in code at this reissue:

- **The CS layer sits outside the purity network.** `cs_reading_relation/3` typed edges
  (`forecloses`, `coexists_with`, `influences`) have no pathway into
  `constraint_neighbors/3` — deliberate: logical preemption and licensed plurality do not map
  onto "contaminates." (`network.md` items 1/3 keep the full survey.)
- **The abductive trigger engine is CS-free.** `abductive_triggers.pl` imports no CS module;
  no CS finding surfaces as an abductive hypothesis. Deferred by design.
- **No `snapshot_type` ↔ `cs_drift_trajectory` bridge.** The two temporal paths (metric
  pipeline vs authored gap descriptor) are semantically parallel with no agreement checker.
  (`observer_diagnostics.md` gap 5 keeps the detail; §6.7's axis discipline is why any future
  bridge must be built as a cross-axis comparison, not a unification.)

---

## 10. Committer-axis verdict

*Carried from v5.1 §§5.1–5.5 substantively unchanged (renumbered). The wrong-instrument
diagnosis (§10.2) is the methodologically load-bearing part and is carried in full.*

### 10.1 The original null, and what it correctly covered

v5 recorded that an extended analysis had pursued a *committer / ground axis* distinct from the
DR engine's observer positions — indexing not *where the observer stands* but *which
commitment, out of a space of possible commitments, exists to be observed* — developing a
committer sheaf over a space of possible kernels, a drifting reachable-window governed by
use-decay, and finite carrying capacity. An omega taxonomy tested it: against a classifiable
base of 319 omegas, the committer-candidate categories returned K (kernel-alternatives) 4, all
tautological self-flags from AI-alignment files; P (population-nesting) 0; C (constraint-space
contingency) 0; N (beneficiary-less path-naturalization) 0. The pre-registered kill condition
fired; the null was recorded over the whole apparatus. That null was correct for almost
everything it covered, and it is not disturbed here.

### 10.2 The wrong-instrument problem, made specific

The omega taxonomy looked for committer content in the *residue* — the unresolved analytical
material the engine could not classify and routed to omega variables. The reasoning was that
committer-axis content the engine structurally could not handle would accumulate there. This
was a reasonable place to look and it came back empty.

But the omega residue is the wrong place to look for the one committer component that is
*already wired into the engine's directionality computation.* The engine computes experienced
extractiveness as χ = ε · f(d) · σ(S), where the directionality term d is set, for constraints
carrying beneficiary/victim structure, by a heuristic that reads exactly one bit of committer
content: does this constraint have victims (and, at institutional power, beneficiaries). That
bit is not residue. It is a live input to classification. An instrument that searches the omega
catch-all for committer fingerprints is, by construction, blind to a committer signal that has
already been consumed upstream as a directionality input — the signal never reaches the residue
because the engine already used it. This is the same failure shape the original analysis itself
identified for path-naturalization: a beneficiary-indexed detector cannot see a beneficiary-less
phenomenon. Here, an omega-residue detector cannot see a committer signal the engine has
already absorbed into d. The omega null is real, but it is a null about *unabsorbed* committer
content, not about committer content as such.

### 10.3 What graduated: the beneficiary/victim bit

A predicate-level audit of the directionality channel, pre-registered with a deflationary kill
condition and an isolation requirement (a flip counts only if it survives reverting d with
every other input pinned), found: on 66 clean candidate constraints at the moderate observer,
**20 flipped, all snare → tangled_rope, all χ-gate-driven, zero confounded.** Two constraints
with identical base extractiveness can classify differently solely because one has victim
structure and the other does not. That is a committer-axis fact — a property of the commitment
itself, not of the observer's position — operationalized, live, and load-bearing.

The boundaries of what graduated, stated so the bundle does not reassemble:

- **It is one bit wide.** The engine collapses the corpus's richer beneficiary/victim content
  to presence/absence; whether the discarded richness carries weight is not established.
- **Calibration-dependent in magnitude, structural only in existence.** The flips ride a single
  hand-set constant at the one observer nearest the sigmoid's steep region. The framework
  claims the existence of the channel, not its width.
- **A symmetric channel is un-audited.** The institutional beneficiary bit is the structural
  twin; prior: dormant (institutional directionality pins χ negative). Plausible, unconfirmed.

### 10.4 What did not graduate, and now on sharper grounds

Everything else remains null, strengthened: **the framework now contains the operationalized
committer instrument the broader apparatus was supposed to ground, and the broader apparatus
was never required to build or run it.** The possibility-space structure contributed nothing to
the working channel (the heuristic is a flat lookup — a set with a grid over it, not a site
with morphisms). The reachable window, use-decay, carrying capacity, and the catastrophe
theorem are a theory of *diachronic cultural dynamics*; the graduated result is *synchronic*.
An independent adversarial review found the strong forms break against documented cases; what
survives is a mechanism-level account (decay-by-disuse, the symbol/commitment split) that needs
no geometry — quarantined in `commitment_dynamics_v1.md`, living on its own evidence. This is
the **standing null / diachronic quarantine** referenced throughout this document.
Path-naturalization (naturalization with no beneficiary) remains conceptually live but
empirically homeless: zero validated instances, retained on probation. The rich committer
dimension — *which kernel was committed to, out of what space of rejected alternatives* — has
the least support of all; the audit bought the committer *axis* one bit of credence and the
committer *space* nothing.

### 10.5 Standing follow-ups

Carried unchanged: (1) the institutional beneficiary channel (prior: dormant — a confirmed
dormancy would itself be informative, showing the graduated channel is special to the moderate
observer's sigmoid position); (2) the full SOTU omega pass (~918 omegas, the most plausible
home for non-tautological P/K content; disciplined prior: another null, worth running for the
clean null). Both recorded so the committer axis is not re-explored from first principles
without the results attached.

---

## 11. Declared absences and open rows

Capabilities the framework deliberately does not have, each recorded so an empty region is
never mistaken for a covered one. (A defect that reads as working goes to `ISSUES.md`; an
absence the design admits goes here or to `docs/design/design_gaps.md`.)

1. **`sealed_closure`** (OQ-227, ruled 2026-07-24). The referent-dissolution terminal:
   declared-open row, NOT added — undetectable by construction; adding an unreachable enum row
   with no possible positive control would counterfeit a finding. Authored-only if ever added;
   reviver trigger is an authoring channel for referent-dissolution. The near-miss search is
   *unsearchable, not concluded negative* — there is no authored referent-dissolution signal to
   search on; a reviver must not read "empty" and skip re-running. (§6.6.)

2. **Schism and absorption/syncretism** (OQ-265, minted this reissue). Identity-breaking
   endpoints: the terminals presuppose single-continuant identity (§6.6). Resolution is a
   per-candidate ruling — scope-note vs authored-only terminal — and must state, per candidate,
   whether the endpoint is DETECTABLE (detectability is what settled OQ-227). Schism looks
   detectable (two successors each authoring acknowledgments); absorption may not be. The
   candidates may not resolve the same way.

3. **The lifecycle phase model** (formation → stabilization → operation → atrophy →
   renewal-or-dissolution). v3-only material, never operationalized — there is no formal state
   machine for system-level lifecycle (the v4-era integration audit already flagged this). The
   drift-terminal subsystem is NOT a lifecycle model: it maps a single authored gap to an
   attractor, not a system through phases. v3 holds the full phase treatment.

4. **The `(distributed, extraction)` cell.** A coherent configuration (a once-coherent kernel
   operationally abandoned, distributed authority denying the drift and extracting from claimed
   continuity) with no taxonomy row. What the classifier currently does: falls through to
   `no_pattern_match, [anomalous_field_combination]` (`cs_pattern_detection.pl` catch-all).
   Three unresolved options (shakeout Claim 2, deliberately open for an operator ruling on more
   than one constraint): (a) a new named pattern (`diffuse_capture`); (b) verdict-layer overlay
   — route to claimed `diffuse_reconstruction` and let `false_diffuse_reconstruction` fire
   (live option, would catch the known case today); (c) one diffuse family with two poles keyed
   on `authority_grounding`.

5. **No `epistemic_consensus` verdict.** The pattern's two failure modes (captured consensus;
   bandwidth shortfall) have no verdict predicate; the seven `false_*` verdicts cover the other
   patterns.

6. **The three-valued remedy gate** (vacancy / capture / bandwidth). PROPOSED at §3, not ruled;
   whether blocked-B admits a capability move at all is OQ-235.

---

## Appendix A — The attractor table

The relation `cs_terminal_attractor(Direction, Magnitude, Acknowledged, Terminal)` is encoded
as **16 clauses** (with wildcards and two inequality guards) covering the **42** (7 directions
× 3 magnitudes × 2 acknowledgment values) combinations — two different counts, both stated:
16 rows in the table below, 42 points in the space they tile.

Two properties, proven separately by `prolog/tests/test_cs_drift_engine.pl`:

- **Row-disjointness AND total coverage** over the 42-combo grid: `attractor_table_row_disjoint`
  enumerates all 42 combinations and asserts each yields exactly one solution (`Ts = [_]` —
  which proves both ≥1, coverage, and ≤1, disjointness). Caveat stated in the test itself: its
  grid hardcodes the seven directions, so a Direction added later is unchecked *there*; the
  `terminal_set_pinned` tripwire is what catches a new Direction (it enumerates the table with
  Direction unbound).
- **The row semantics themselves** (which combination maps to which terminal) are asserted with
  their witness: the OQ-137 row-disjointness fix (2026-07-02) preserved the pre-fix FIRST
  solution on all 42 combinations, witnessed by the before/after enumeration diff in the fix
  commit.

The table below is generated from the `cs_drift_engine.pl` clause heads at write time and sits
under the spec-enum tripwire: the checker re-derives the rows by normalizing the clause heads
(+ guards) from the code and diffs. Notation: `*` = any value (wildcard); `*!=x` = any value
except `x` (an inequality guard); row format `direction | magnitude | acknowledged -> terminal`.
The largest enumeration in this spec must not be the one exempt from the instrument built for
enumerations.

<!-- spec-enum: cs_attractor_table -->
```
stable | * | * -> stable_pattern
*!=stable | minor | * -> stable_pattern
authority_erosion | severe | * -> repudiation
authority_erosion | substantial | false -> husk
authority_erosion | substantial | true -> stable_pattern
codification_collapse | severe | * -> extinction
codification_collapse | substantial | * -> husk
axiom_overriding | severe | * -> axiom_foreclosure
axiom_overriding | substantial | false -> axiom_foreclosure
axiom_overriding | substantial | true -> stable_pattern
practice_drift | severe | false -> extinction
practice_drift | severe | true -> revival
practice_drift | substantial | false -> husk
practice_drift | substantial | true -> stable_pattern
revival_pressure | *!=minor | * -> revival
repudiation_pressure | *!=minor | * -> repudiation
```
<!-- /spec-enum -->

Reading notes: no-drift and all minor drift are absorbed to `stable_pattern` (minor
self-corrects under inertia; a minor pressure gap is likewise absorbed — the pre-fix first
solution, now unique). Codification collapse drops the acknowledgment variable — the medium
itself is destroyed, so there is nothing to act on an acknowledgment (§6.4's reclassification
rule covers acknowledged rebuilds). The acknowledgment principle (§6.4) is visible in the
`substantial + true → stable_pattern` rows for `authority_erosion`, `axiom_overriding`, and
`practice_drift`, and in its two stated limits (severe; codification collapse).

## Appendix B — Document map

One line per satellite document, so this reissue declares what it deliberately does NOT absorb.
Status vocabulary: **absorbed** (content now lives here; file stays as history), **standalone**
(deliberately not absorbed; cite, do not merge), **historical** (superseded record).

| Document | Status |
|---|---|
| `commitment_systems_sketch.md` (v1) | historical |
| `commitment_systems_sketch_v2.md` | historical |
| `commitment_systems_sketch_v3.md` | historical — uniquely holds the lifecycle-phase material (§11.3) and predictive scaffolding v4 trimmed |
| `commitment_systems_sketch_v4.md` | absorbed (§2) — superseded-by header added |
| `commitment_systems_sketch_v5.md` | absorbed (§2, §4, lineage §1.4) — superseded-by header added |
| `commitment_systems_sketch_v5_1.md` | absorbed (§10) — superseded-by header added |
| `commitment_systems_sketch_v5_2.md` | absorbed (§4, §5) — superseded-by header added |
| `type_b_adjudication_2026-07-23.md` | absorbed (§3) as ruling content; file stays — chat provenance and withdrawn counter-positions |
| `commitment_dynamics_v1.md` | standalone by design — the quarantined diachronic dynamics theory; lives on its own evidence (§10.4) |
| `commitment_systems_and_possibility_space.md` | standalone — its superseded predecessor, kept with the null attached |
| `adversarial_kernel_case_library.md` | standalone — the dynamics theory's adjudicated test bed |
| `cs_shakeout_record.md` | standalone — settled record (the two-doc split is deliberate) |
| `cs_shakeout_proposals_open.md` | standalone — staging area; Claim 1 landed via v5; Claim 2 open (§11.4); Claims 3–4 open/parked |
| `observer_diagnostics.md` | standalone — integration gap survey; still-live items cited as declared scope (§9.3); survey keeps the detail |
| `network.md` | standalone — same |
| `construction_over_inspection.md` | standalone — methodology essay, out of spec scope |
| `anchored_fixity_russia_essay.md` | standalone — worked essay artifact |
| `commitment_systems_integration_v1.md` | historical — v4-era investigation |
| `original_plan_2026-05-26.md` | historical — partially executed; carries a status header |
