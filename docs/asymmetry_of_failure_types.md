# The Asymmetry of Failure Types

### Detection, prevention, and governance in analytical validation

#### Abstract

The trifurcation classification of failure modes — Type A (drift), Type B (structural inconsistency), Type C (indexical underspecification) — was originally diagnostic. This paper develops it architecturally. When the same taxonomy is built into a working analytical apparatus, the three types divide unevenly and unmistakably along treatment lines: Type B is detected by formal computation against structural axioms, Type C is prevented by required indexing at the schema layer, and Type A is governed by paired synchronic discipline and diachronic comparison plus external review. These are not three intensities of the same operation. They are three different relations between failure and formalism — contradiction-to-structure, contradiction-to-specification, contradiction-to-governance — and the asymmetry implies that *validation* is not a single operation but a family of operations with different points of entry, different limits, and different things they can honestly claim.

This paper is the meta-companion to *When Splitting Isn't Solving*. Where that paper takes Type C in particular and shows the apparatus refines what specification can and cannot accomplish, this paper takes the three types together and asks why their treatments differ in kind rather than only in degree. The answer is that detection is the relation a formal apparatus has to its own structural axioms, prevention is the relation it has to the schemata that constrain its inputs, and governance is the relation it has to its own continuity over time — and these are not interchangeable.

#### 1. From taxonomy to apparatus

The original trifurcation classified what kinds of failure exist in reasoning systems. Type A failures arise from unmarked drift across reasoning stages and dissolve under frame-fixing. Type B failures arise from axiomatic inconsistency and require system-level revision. Type C failures arise from indexical underspecification and dissolve when the index is specified. The framing was operational rather than metaphysical — paradoxes are engineering problems, not metaphysical revelations, and the diagnostic value of the taxonomy lay in distinguishing what kind of repair each kind of failure called for.

The Deferential Realism apparatus is a working classification system that retrospectively reads as implementing the trifurcation, but unevenly. An audit of its subsystems against the three types finds that Type B has five or more dedicated detectors at distinct layers — Boltzmann compliance, structural signatures, False CI Rope, False Summit Mountain, False Natural Law. Type C has two clean implementations, gauge orbits and cohomology, that together provide qualitative and quantitative detection of position-dependence. Type A has one primary instrument, the drift-events module that catches a handful of temporal change patterns when measurement history is available. The asymmetry is empirical and obvious to anyone reading the modules.

The interesting question is whether the asymmetry is accidental or principled. Part of it is contingent: Type A failures in static corpora require temporal evidence that most testsets do not carry, so one instrument is enough for what the data provides. But the rest of the asymmetry is not contingent. It tracks something deeper about what kinds of failure can be handled by which kinds of formalism. That deeper pattern is the subject of this paper.

The shift from taxonomy to apparatus is the shift from description to architecture. A taxonomy says what can go wrong. An architecture says what can be seen, what can be blocked, and what must be governed. The three types of the trifurcation correspond, when built into a working apparatus, to three different architectural relationships — and the relationships are not interchangeable.

#### 2. Why Type B is detectable

Type B failures are inconsistencies between a constraint's claims and the structural axioms that should hold given those claims. The contradiction is encoded in the structure itself. When a constraint is asserted as a Mountain — a natural law restricting all positions equally — but has identifiable beneficiaries, the contradiction is in the data: natural laws have no beneficiaries by definition, so the existence of beneficiaries falsifies the Mountain assertion immediately. No process is required, no observer position must be fixed; the axioms applied to the structure derive both Mountain and not-Mountain.

Because Type B inconsistencies are structural, they are available to formal detection in the apparatus's native language. That is why the apparatus elaborates Type B into multiple layers without redundancy becoming wasteful. Different layers catch different manifestations of the same underlying mismatch. *Boltzmann compliance* checks whether a constraint's classification factorizes across the Power × Scope grid, as natural laws must. *Structural signatures* check whether the constraint's origin profile — active enforcement, beneficiary asymmetry, viable alternatives — matches the type its metrics imply. The *False CI Rope*, *False Summit Mountain*, and *False Natural Law* detectors check specific patterns of mismatch between claimed and structural type. Each is doing the same kind of work — testing structural consistency against axioms — but at a different layer of the constraint's profile, and a single detector at one layer would miss what detectors at the others catch.

The multi-layer architecture is not over-engineering. It is the right response to a class of failure that can present itself in more than one formal guise. Structural inconsistency is what the apparatus already knows how to compute about, so building several distinct computations against several distinct axioms is exactly what the type calls for. Type B is the most elaborated category in the apparatus precisely because elaboration is appropriate: structure can be inspected at several depths, and each depth is its own legitimate point of entry.

The audit's most significant Type B finding deepens this further. The cover-story mechanism, where chi-level structural inconsistency is concealed at the type-surface level, shows that Type B detection has to operate at multiple measurement layers and not only multiple axioms. A constraint can satisfy surface consistency conditions while violating deep structural conditions, and only a multi-layer architecture catches the gap. Independent detectors at different levels can catch what no single detector at one level could. That is why redundancy in Type B is epistemically useful rather than wasteful: the redundancy operates across measurement layers, not within a single one.

#### 3. Why Type C is architectural

Type C failures are not best understood as contradictions waiting to be detected. They are failures of specification. The query or analysis is underspecified at the point where it needs an index — observer position, time-slice, reference domain. Once the question is properly indexed, the paradox often dissolves because what looked like one question turns out to be several.

That means Type C should not be handled by detection after the fact. It must be handled architecturally. The system requires complete indexing before validation can run. In the DR apparatus, the schema and validation pathway refuse to proceed unless the indexing fields are complete. This is stronger than flagging ambiguity — it makes underspecification structurally impossible at the stage where it would otherwise corrupt the result.

But not every Type C resolution terminates the analysis. *When Splitting Isn't Solving* develops the case in detail: index specification can produce answers that are independent — clean splits, satisfying the sheaf gluing axiom — or coupled — structured splits, violating it — and the latter carry information the former do not. The cohomological machinery that distinguishes these regimes is itself a Type C extension. It operates only after specification has been required, and it characterizes what the specified split actually does. The architectural prevention enforces specification; the cohomological measurement asks what kind of split specification produced.

The architectural treatment of Type C is therefore two-stage in the apparatus: prevention at the input layer (no analysis without indexing) and measurement at the output layer (cohomological obstruction quantifying what the specified indexing reveals). Both are architectural rather than diagnostic — they are properties of how the apparatus is built, not checks it runs after the fact. The diagnostic move that the trifurcation originally proposed for Type C — "specify the index, get consistent answers, stop" — is preserved as the input-layer requirement, but extended at the output layer with the recognition that some specified indexings produce structured rather than independent answers, and the structure is itself the object of study.

#### 4. Why Type A resists internal detection

Type A is the most difficult case because it concerns drift in the frame of analysis itself. The problem is not merely that a definition changes over time; it is that the system continues to operate as if the definition had remained stable. If the apparatus is already running under a drifted frame, its own formal operations are not positioned to detect the drift. Drift is part of the conditions under which the system inspects, not an object the system inspects.

This is why the apparatus's Type A coverage is thin. But it is also why thin coverage is not entirely the apparatus's failure. The thinness reflects a structural fact about formal validation: drift cannot be exhaustively detected from within the same frame that may itself be drifting. Some Type A patterns *can* be caught when temporal evidence is available — the apparatus's drift-events module catches metric substitution, extraction accumulation, coordination loss, sunset violations, and Boltzmann-property drift across measurement intervals. But these are diachronic comparisons against past states of the apparatus, not synchronic inspections of the present frame. They work when measurement history exists. They cannot replace the frame-level governance they presuppose.

Type A therefore requires a paired strategy rather than a single mechanism. First, *synchronic discipline*: the apparatus enforces frame stability during analysis, preventing the kind of unmarked mutation that generates the failure in the first place. Second, *diachronic monitoring*: the system compares outputs across time to see whether classifications are shifting where the underlying properties are not. Third, *external review*: a human or otherwise external audit inspects the apparatus from outside its current frame and catches forms of drift that the system's own operations cannot see. The varieties of Type A drift — semantic mutation, lifecycle-boundary violation, identity-criterion switching, temporal drift in the strict sense — are handled by these three together, not by any one of them.

The point is not that Type A is undetectable in every sense. It is that it is not exhaustively detectable from within the same frame that may itself be drifting. That distinction matters because it prevents the false promise that a formal validator can simply *check* for drift the way it checks for structural inconsistency. Drift is a governance problem before it is a validation problem, and the appropriate response is institutional discipline supplementing formal machinery, not formal machinery alone.

#### 5. The diagnostic checklist

Once the asymmetry is seen, the diagnostic move follows. The task is not to assign one of three labels mechanically. It is to ask what kind of diagnostic shape the case has, in an order that respects the architecture.

The first question is whether the case exhibits Type C features: is there an indexical gap, a missing observer position, a time-slice ambiguity, a reference-domain ambiguity? If so, the move is not to infer more carefully but to specify more carefully — and, if specification produces a structured split rather than a clean one, to study the coupling rather than to dissolve it.

The second question is whether the case exhibits Type A features: does it depend on process, iteration, frame mutation, or shifting criteria? If so, the move is not classification alone but freezing, disciplining, or comparing across time, with external review where the frame's own visibility is in doubt.

The third question is whether the case exhibits Type B features: does it survive index specification and frame-fixing while still generating structural contradiction? If so, the issue is axiomatic, and formal detection against the structural axioms is appropriate. The detection may need to operate at several layers — metric, structural-signature, origin, beneficiary, factorization — because Type B can present in several formal guises at once.

The order matters. Type C is addressed first because specification is the cheapest move and because unindexed analysis cannot reach either of the other two diagnoses cleanly. Type A is addressed second because frame-fixing must precede axiomatic check — testing the axioms against a drifting frame produces unreliable verdicts on either side. Type B is addressed last because its detection requires the input the prior moves provide: a specified, frame-fixed query against which structural consistency can be tested.

This ordering is implicit in the apparatus's actual rule cascade — gauge-orbit checks before drift-event detection before signature-driven structural overrides. The audit notes the parallel without that parallel having been designed in. It is the order the architecture demands when the three types are correctly distinguished.

#### 6. What the asymmetry means

The deeper point is that the three types correspond to three different relations between failure and formalism.

Type B is the relation of contradiction to *structure*. The apparatus can see the break because the break is in the formal vocabulary the apparatus already speaks. Detection is appropriate because the failure mode is encoded in the same language as the system's claims. Multiple detection layers are appropriate because structure can be inspected at several depths without redundancy becoming wasteful.

Type C is the relation of contradiction to *specification*. The apparatus cannot proceed unless the query is determinate enough to run. Prevention by schema is appropriate because the failure mode is in the input, not the inference, and requiring complete inputs at the schema layer eliminates the failure at the only point where it could enter. Cohomological measurement at the output layer is appropriate because what specification reveals about coupling is itself an object of study — but it is not a substitute for the prevention that comes first.

Type A is the relation of contradiction to *governance*. The apparatus cannot exhaustively manage drift through its own operations because those operations may themselves be conducted under a drifted frame. Synchronic discipline, diachronic comparison, and external review are appropriate because the failure mode is partly outside what the apparatus can inspect from within. Governance is what fills the gap between what the formalism can detect and what reliability requires.

These are not three intensities of the same operation. They are three different ways a formal apparatus can fail to line up with the object it is trying to analyze, and three different positions from which the failure can be addressed. *Validation* is therefore not a single operation. It is a family of operations with different failure modes, different points of entry, and different limits — and a system that claims to validate without distinguishing them is overclaiming about at least one of the three.

#### 7. Conclusion

The trifurcation tells us what can go wrong. The asymmetry tells us how different kinds of wrongness can be known, prevented, or governed. Type B is formally detectable, Type C is architecturally constrained, and Type A is only partially visible from within the frame that produces it.

This leads to a more precise idea of analytical validation. A formal system should not claim that it *catches errors* in general. It should say which failures it can detect, which it can exclude by design, and which it can only monitor under temporal discipline and external review. The honest version of the validation claim is structured, not flat: it has three different shapes for three different failure modes, and the shapes are not interchangeable.

This is not a universal theorem about formal systems. It is a hypothesis about the architecture of analytical apparatuses that combine static classification, observer-indexed queries, and longitudinal drift monitoring. Such systems should expect this asymmetry, and should design around it. *When Splitting Isn't Solving* developed one specialized version of the move for Type C; this paper develops the meta version for the trifurcation as a whole. Other specialized versions remain to be written — particularly for Type B, where the apparatus's coverage is richest and where cover-story phenomena reveal multi-layer measurement structure the trifurcation does not anticipate. The discipline of writing these papers as they present themselves, rather than forcing premature synthesis, is the same discipline the framework's apparatus instantiates: detection where structure permits it, prevention where specification permits it, governance where neither does.

---

#### References

The conceptual heritage drawn on in this paper is internal to a developing project on observer-dependent classification:

- *Debugging Philosophy: A Trifurcation Framework for Paradox Classification.* Establishes the original A/B/C taxonomy and the diagnostic-test structure this paper extends architecturally.
- *Trifurcation-to-Apparatus Mapping Audit* (internal, 2026-05-02). Per-subsystem analysis of the DR apparatus against the trifurcation; source for the empirical asymmetry of coverage and the cover-story mechanism.
- *Deferential Realism: A Presheaf Framework for Observer-Dependent Classification* (v6.11). The apparatus paper. Source for the specific subsystem behaviors referenced — Boltzmann compliance, structural signatures, FCR/FSM/FNL, drift-events, gauge orbits, cohomological obstruction.
- *When Splitting Isn't Solving: Sheaves, Presheaves, and the Structure of Indexical Disagreement.* The Type C extension paper this paper takes as its meta-companion. Source for the sheaf/presheaf criterion as a refinement of Type C resolution and for the product-site stability result on observer geometry.
- *Observers, Not Humans: Deferential Realism Across Observer Classes* (v5). Source for the parametric-vs-epistemic fragility distinction and the universality-class framing of structural claims.

---

*Working paper. Conceptual development in conversation with Claude (Anthropic). The asymmetry argument emerged from the audit's finding that the apparatus's coverage of the trifurcation is not symmetric, and from the recognition that the asymmetry is partly principled rather than entirely contingent on the apparatus's domain.*
