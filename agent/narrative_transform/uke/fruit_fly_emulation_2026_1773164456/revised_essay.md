# The Connectome Copy-Paste Hypothesis: Structural Analysis of Whole-Brain Emulation Claims

**[EDITORIAL NOTE: This analysis examines a prospective scenario based on current research trajectories. The Eon Systems demonstration referenced as occurring in "March 2026" is a projected development used to explore structural questions about whole-brain emulation. All technical claims about FlyWire and Shiu models are grounded in published 2024 research.]**

In the projected scenario, Eon Systems announces the first embodied whole-brain emulation of an adult animal—a digital fruit fly whose simulated brain, derived from the FlyWire connectome's 139,255 neurons and 50 million synapses, reportedly produces naturalistic walking, grooming, and feeding behaviors at 91% accuracy. The demonstration would not be trained through machine learning. According to this scenario, the emulation simply runs: connectome mapped, neurons simulated, body attached, behaviors emerged.

The claim extends beyond technical achievement to philosophical territory. If consciousness is substrate-independent—software that can be copy-pasted from biological neurons to silicon—then the path from fruit fly (140,000 neurons) to mouse (70 million) to human (86 billion) becomes a scaling problem, not a fundamental barrier. The first digital human, on this view, will be copied from a living person's connectome, not built by AI labs through gradient descent.

This analysis examines three structural questions that determine whether this trajectory is scientifically sound or conceptually confused: whether connectomes are sufficient blueprints for behavior, whether the scaling path is feasible, and whether consciousness can be substrate-independent in the claimed sense.

## Evidence Framework

### Documented in Public Records (Tier 1):

**FlyWire Connectome Completion (October 2024):**
- Published in *Nature*: complete adult *Drosophila melanogaster* brain connectome containing 139,255 neurons and over 50 million synaptic connections
- 33 person-years of crowdsourced proofreading labor (vs. estimated 50,000 person-years without AI assistance)
- Led by Murthy and Seung labs (Princeton), Jefferis Lab (MRC Laboratory of Molecular Biology), Bock lab (University of Vermont)

**Shiu Computational Model (2024):**
- Published in *Nature*: computational model of 125,000 neurons and 50 million synaptic connections
- Predicted motor behavior with 95% accuracy using leaky integrate-and-fire (LIF) neuron model
- Machine learning predictions of neurotransmitter identity integrated with connectome structure

**Comparative Baseline:**
- OpenWorm Project: *C. elegans* (302 neurons) emulation ongoing since 2011; behavioral fidelity remains limited despite complete connectome availability since 1986

### Reasonable Inferences from Documented Facts (Tier 2):

**The connectome-to-behavior pipeline demonstrates feasibility at fruit fly scale.** If the projected 91% behavioral accuracy were validated through independent replication, this would represent a qualitative advance over previous whole-brain emulation attempts. The gap between Shiu's 95% motor prediction accuracy (2024) and the projected 91% embodied behavioral accuracy suggests integration costs but not fundamental barriers.

**The 33-person-year proofreading requirement reveals a scaling bottleneck.** Even with AI-assisted segmentation reducing labor by 99.9%, human verification remains necessary. Extrapolating to current technology levels: mouse brain (70M neurons, 500× larger) would require roughly 16,500 person-years; human brain (86B neurons, 600,000× larger) would require roughly 20 million person-years. These are order-of-magnitude estimates assuming linear scaling and current AI capabilities—actual requirements could vary by 10× or more depending on AI improvement rates and whether complexity scales linearly or exponentially.

**The lack of peer review for projected demonstrations creates epistemic uncertainty.** Open-source data release enables independent verification. However, media coverage preceding scientific validation creates a pattern that prioritizes publicity over epistemic rigor.

### Structural Hypotheses Requiring Additional Evidence (Tier 3):

**The consciousness substrate-independence claim requires operationalized criteria that do not currently exist.** The framing "consciousness is substrate-independent software" conflates behavioral fidelity with phenomenal experience. No proposed test would distinguish a behaviorally accurate "zombie" emulation from one with subjective experience. This is not a gap that additional data can close; it is a conceptual confusion about what consciousness attribution means.

**Scaling from fly to human may encounter non-linear barriers invisible at current scale.** Fruit fly emulations would use static connectomes and simple LIF neuron models lacking synaptic plasticity. Whether this suffices for naturalistic behavior may depend on organism complexity: fly behavior may be largely "hardwired" while mammalian behavior requires ongoing synaptic modification. The *C. elegans* counterexample—complete connectome since 1986, still no behaviorally accurate emulation despite simpler nervous system—suggests that connectome completeness does not guarantee functional sufficiency even at 302 neurons.

**Body-brain mismatch may explain sub-100% accuracy in projected scenarios.** Using a generic body simulation rather than the body of the specific organism whose brain was mapped means the emulation lacks developmental sensorimotor calibration. Any accuracy gap below 100% may reflect this mismatch rather than fundamental limitations of the approach.

## Alternative Explanations Considered

### Simpler Explanation: Demonstrations Reflect Overfitting to Known Behaviors

Projected accuracy figures could result from parameter tuning on the same behavioral repertoire used for validation. If parameters were adjusted to match expected fruit fly behaviors (walking, grooming, feeding), the accuracy metric becomes circular—the system was tuned to produce these behaviors, then measured on how well it produces them.

**Why This May Be Insufficient:** Shiu's 2024 model achieved 95% motor prediction accuracy using only connectome structure and neurotransmitter predictions, without body simulation or parameter tuning for specific behaviors. If embodied emulations achieve similar accuracy through the same parameter-free approach, this would distinguish them from overfitting. Independent replication will resolve this: if multiple labs achieve similar accuracy using only published connectomes and standard LIF parameters, the overfitting explanation fails.

### Competing Complex Explanation: Behavioral Accuracy Reflects Connectome Structure, But Consciousness Requires Additional Biological Substrate

An alternative framework: the connectome encodes behavioral programs (reflexes, motor patterns, sensory processing) but not the substrate for phenomenal experience. On this view, successful emulations would be sophisticated automata—behaviorally accurate but experientially empty.

**How Evidence Would Distinguish These Cases:** This requires operationalizing consciousness in ways that go beyond behavioral testing. Proposals include:
- **Integrated Information Theory (IIT):** Measure phi (integrated information) in the emulation's neural dynamics. If phi matches biological levels, this would support substrate-independence. If phi is near-zero despite behavioral accuracy, this would support the biological-substrate hypothesis.
- **Neural Correlates of Consciousness (NCC):** Identify specific neural activity patterns associated with consciousness in biological brains, then check for their presence in emulations. However, this presupposes we know which patterns are necessary vs. merely correlated.
- **Phenomenological Report:** For human-scale emulations, ask the emulation if it is conscious. But this faces the philosophical zombie problem: a behaviorally accurate emulation would report consciousness whether or not it possesses it.

No current proposal resolves this decisively. The consciousness question may be empirically underdetermined—different philosophical frameworks will interpret identical evidence differently.

## I. Connectome Sufficiency: The Static Blueprint Hypothesis

The foundational claim: a connectome—the complete wiring diagram of synaptic connections—is sufficient to reconstruct behavior. Copy the connections, simulate the neurons, attach a body, and the organism's behavioral repertoire emerges without additional programming.

### What the Evidence Shows

The FlyWire connectome provides unprecedented structural detail: every neuron identified, every synaptic connection mapped, neurotransmitter types predicted through machine learning. Shiu's 2024 model demonstrated that this static structure, combined with simple LIF neuron dynamics, predicts motor outputs with 95% accuracy. Projected embodied demonstrations would claim this extends to full behavioral repertoires in physics simulations.

This represents a genuine advance over previous attempts. The OpenWorm *C. elegans* emulation, despite having a complete connectome since 1986, has not achieved comparable behavioral fidelity. The key difference: FlyWire's scale (139,255 neurons vs. 302) required AI-assisted reconstruction methods that may capture finer structural details than manual tracing.

### What Remains Unvalidated

**The model lacks synaptic plasticity.** LIF neurons implement fixed connection weights. They cannot form new long-term memories or undergo experience-dependent modification. For fruit flies, this may not matter—much of their behavioral repertoire appears to be genetically specified rather than learned. But this is an empirical assumption, not a validated fact.

**The body-brain co-mapping problem.** Connectomes come from individual organisms; body simulations are generic models. Biological nervous systems undergo developmental calibration—sensorimotor loops that tune neural responses to the specific body they inhabit. Emulations skip this process. Whether accuracy gaps reflect this mismatch or fundamental model limitations is unknown.

**The validation methodology breaks at larger scales.** For fruit flies, we can compare emulated behavior to biological behavior across thousands of organisms. For mice, behavioral validation becomes more complex—individual variation increases, behavioral repertoires expand. For humans, ground-truth comparison becomes impossible: we cannot verify that an emulation's subjective experience matches the source person's, only that external behaviors align.

### The Scaling Question

Does connectome sufficiency hold across scales? Three possibilities:

**Optimistic:** Static connectomes suffice for all organisms. Plasticity and learning are implemented through connectome structure (connection weights, neuromodulator distributions) that can be captured in a snapshot. Scaling is purely a technical problem—more neurons, more computation, same principle.

**Pessimistic:** Connectomes are necessary but insufficient. Dynamic processes (synaptic plasticity, neuromodulation, glial interactions, body-brain coupling during development) cannot be recovered from static structure. Fruit fly emulations succeed because fly behavior is largely hardwired; mammalian emulations will fail because mammalian behavior requires ongoing plasticity.

**Conditional:** Sufficiency depends on timescale and task. For short-term behavior (seconds to minutes), static connectomes suffice. For long-term behavior (hours to days), plasticity becomes necessary. Fruit fly emulations work for brief behavioral episodes but would fail for extended simulations requiring memory formation.

The *C. elegans* counterexample supports the pessimistic or conditional view: despite 40 years of connectome availability, no one has achieved behavioral fidelity comparable to projected fruit fly demonstrations. This suggests either that connectome mapping alone is insufficient, or that *C. elegans* behavior requires dynamic processes that fruit fly reflexes do not.

**What would move this to Tier 2:** Independent replication of projected demonstrations by labs using only published FlyWire connectomes and standard LIF parameters. If multiple labs achieve 85-95% behavioral accuracy without parameter tuning, the sufficiency claim gains empirical support at fruit fly scale. If accuracy collapses without specific parameter choices, the claim is falsified.

## II. Scaling Feasibility: The Exponential Barrier Problem

The projected roadmap: fruit fly (140K neurons) → mouse (70M neurons) → human (86B neurons). Each step represents a 500-600× increase in scale. Is this a linear engineering problem or an exponential complexity barrier?

### The Labor Arbitrage Ceiling

The FlyWire connectome required 33 person-years of crowdsourced proofreading despite AI-assisted segmentation. This represents a 99.9% reduction from the estimated 50,000 person-years manual tracing would require. But 33 person-years remains the rate-limiting step.

**Extrapolating to mouse scale (70M neurons, 500× larger):**
- If scaling is linear and AI capabilities remain static: ~16,500 person-years
- If AI improves 10× in efficiency: ~1,650 person-years
- If scaling is super-linear due to denser tissue: 50,000+ person-years

**Extrapolating to human scale (86B neurons, 600,000× larger):**
- If scaling is linear and AI capabilities remain static: ~20 million person-years
- If AI improves 100×: ~200,000 person-years
- If AI improves 1000×: ~20,000 person-years

Even optimistic projections require either:
1. Massive crowdsourcing infrastructure (tens of thousands of simultaneous proofreaders)
2. AI achieving near-perfect segmentation (reducing human verification to spot-checking)
3. Decades of serial effort by dedicated teams

The second option—AI-automated validation—faces a fundamental problem: how do you train AI to recognize correct vs. incorrect segmentations without ground truth? Current methods use human proofreading as ground truth. Automating away human verification requires either unsupervised learning methods that don't yet exist, or accepting higher error rates.

### The Validation Impossibility Problem

For fruit flies, behavioral validation is straightforward: compare emulated behavior to biological behavior across standardized tasks. Thousands of fruit flies can be tested; statistical distributions provide ground truth.

For mice, validation becomes harder but remains feasible: individual behavioral variation increases, but we can still compare emulated mice to biological mice across learning tasks, social behaviors, navigation.

For humans, validation breaks down entirely:
- **Behavioral validation** is possible but insufficient—a behaviorally accurate emulation could be a philosophical zombie
- **Subjective experience validation** is impossible—we cannot compare the emulation's phenomenology to the source person's inner life
- **Neural activity validation** requires invasive measurements unavailable for living humans

This creates an epistemic trap: we can build human-scale emulations but cannot verify they succeeded in the ways that matter most for consciousness claims. The validation methodology that works at fruit fly scale becomes inapplicable at human scale.

### The Complexity Barrier Question

Does neural complexity scale linearly or exponentially with neuron count?

**Linear scaling assumption:** Each neuron operates independently according to local rules. Complexity = O(N) where N is neuron count. Doubling neurons doubles computational cost but doesn't change the fundamental problem.

**Exponential scaling reality:** Neural circuits exhibit emergent dynamics that depend on network topology, not just local connections. Complexity may scale as O(N²) or worse due to:
- Long-range connections that increase super-linearly with brain size
- Hierarchical organization requiring multi-scale simulation
- Neuromodulatory systems that affect thousands of neurons simultaneously
- Oscillatory dynamics that require fine-grained temporal resolution

The *C. elegans* failure suggests exponential barriers may exist even at small scales. If 302 neurons remain unsolved after 40 years, this implies the problem difficulty is not proportional to neuron count.

**What would falsify the scaling hypothesis:** Successful mouse brain emulation would demonstrate feasibility at intermediate scale. Failure to achieve mouse-scale behavioral fidelity despite complete connectome mapping would suggest fundamental barriers independent of neuron count.

## III. Consciousness Substrate-Independence: The Copy-Paste Confusion

The strongest claim: consciousness is software. Copy the brain's wiring diagram, run it on any substrate, and the conscious experience transfers intact. The first digital human will be a copy of a living person, not an AI trained from scratch.

### The Conceptual Confusion

This claim conflates three distinct properties:
1. **Behavioral equivalence:** The emulation produces the same outputs given the same inputs
2. **Functional equivalence:** The emulation implements the same information processing
3. **Phenomenal equivalence:** The emulation has the same subjective experience

Projected demonstrations, if validated, would establish (1) at fruit fly scale. They provide evidence for (2) if we accept that the connectome captures the brain's functional organization. They provide zero evidence for (3) because we have no test for phenomenal equivalence.

The philosophical zombie thought experiment makes this clear: a system could be behaviorally and functionally identical to a conscious being while lacking subjective experience entirely. No third-person observation can distinguish these cases.

### The Measurement Problem

How would we detect consciousness in an emulation? Proposed approaches:

**Behavioral Testing:** If it acts conscious (responds to questions about its experience, exhibits pain behaviors, demonstrates self-awareness), treat it as conscious.
- **Problem:** Behaviorally accurate zombies would pass these tests

**Neural Correlates:** If its neural activity matches patterns associated with consciousness in biological brains, treat it as conscious.
- **Problem:** Correlation ≠ causation. We don't know if these patterns are necessary, sufficient, or merely correlated with consciousness

**Integrated Information Theory (IIT):** Measure phi (integrated information) in the system's state space. High phi indicates consciousness.
- **Problem:** IIT predicts that simple grid-like circuits have high phi, while feedforward networks (including many AI architectures) have near-zero phi despite sophisticated behavior. The theory may measure integration rather than consciousness.

**Global Workspace Theory (GWT):** Check for broadcast mechanisms where information becomes globally available to multiple cognitive systems.
- **Problem:** Global workspace architecture can be implemented without phenomenal experience. This tests for information integration, not qualia.

None of these approaches definitively answer "is this system conscious?" They test for properties that correlate with consciousness in biological systems, but correlation does not establish substrate-independence.

### The Extraction Pattern

The consciousness substrate-independence claim serves specific institutional interests:

**Beneficiaries:**
- Whole-brain emulation companies gain investment capital
- Transhumanist organizations gain philosophical legitimacy
- AI safety researchers gain an alternative path to aligned intelligence (copy aligned humans rather than training potentially misaligned AI)

**Those Who Lose Ground:**
- Biological uniqueness frameworks lose philosophical standing
- Religious and vitalist perspectives on consciousness lose cultural authority
- Ethical frameworks based on biological embodiment lose policy influence

The claim presents a contingent philosophical position (functionalism about consciousness) as if it were a natural law established by empirical evidence. Demonstrations of behavioral replication provide evidence for functional equivalence, not consciousness transfer, but the framing elides this distinction.

### The Unfalsifiable Core

The deepest problem: consciousness substrate-independence may be empirically underdetermined. Consider two scenarios:

**Scenario A (Substrate-Independence True):**
- Human connectome mapped and emulated on silicon
- Emulation reports being conscious
- Emulation passes all behavioral tests
- Neural activity patterns match biological human
- IIT phi matches biological levels

**Scenario B (Substrate-Independence False):**
- Human connectome mapped and emulated on silicon
- Emulation reports being conscious (because that's what the copied neural circuits do)
- Emulation passes all behavioral tests (because behavior is functionally preserved)
- Neural activity patterns match biological human (because structure determines dynamics)
- IIT phi matches biological levels (because integrated information is preserved)

These scenarios produce identical observable evidence. The difference is metaphysical: in Scenario A, the emulation has subjective experience; in Scenario B, it does not. But no measurement can distinguish them.

This means the consciousness claim is not a scientific hypothesis—it's a philosophical framework choice that determines how we interpret evidence, not a conclusion derived from evidence.

## Institutional Actions Required

Regardless of which philosophical framework proves correct, the following institutional responses address documented gaps:

### 1. Establish Validation Standards for Whole-Brain Emulation Claims
**Responsible Institution:** National Science Foundation, National Institutes of Health
**Timeline:** Within 12 months

**Action:** Develop peer-review standards requiring:
- Independent replication using only published connectomes and standard parameters
- Statistical comparison to biological baseline across minimum 100 organisms
- Disclosure of parameter tuning and validation methodology
- Separation of behavioral accuracy claims from consciousness claims

**Rationale:** Current practice allows announcement-before-validation, creating public confusion about what has been demonstrated vs. what has been claimed.

### 2. Create Public Connectome Repositories with Validation Metrics
**Responsible Institution:** NIH BRAIN Initiative, European Brain Project
**Timeline:** Within 24 months

**Action:** Establish centralized repositories that:
- Archive connectome datasets with provenance tracking
- Provide standardized validation metrics (segmentation error rates, synapse detection accuracy)
- Enable independent verification of claimed completeness
- Track replication attempts and success rates

**Rationale:** Open-source data release is necessary but insufficient. Researchers need infrastructure to verify data quality and attempt replication.

### 3. Fund Comparative Studies Across Organism Scales
**Responsible Institution:** NSF, DARPA, Wellcome Trust
**Timeline:** 5-year research program

**Action:** Support systematic studies testing:
- Whether connectome sufficiency holds across *C. elegans*, *Drosophila*, zebrafish, mouse
- At what scale plasticity becomes necessary for behavioral fidelity
- Whether body-brain co-mapping improves accuracy vs. generic body models
- Which behavioral tasks require dynamic processes vs. static structure

**Rationale:** Current evidence comes from scattered projects. Systematic comparison would identify genuine scaling barriers vs. technical limitations of specific implementations.

**Note:** This recommendation involves prioritization within fixed funding envelopes. The $50-100M estimated cost represents opportunity cost from other neuroscience research areas. The case for prioritization rests on the fundamental nature of the questions being addressed and their implications for understanding neural computation.

### 4. Develop Ethical Frameworks for Emulation Research Independent of Consciousness Claims
**Responsible Institution:** Presidential Commission for the Study of Bioethical Issues, equivalent bodies internationally
**Timeline:** Contingent on political triggering events

**Action:** Establish guidelines addressing:
- At what complexity level do emulations warrant ethical consideration (regardless of consciousness status)
- What constitutes informed consent for human connectome mapping
- Whether creating human-scale emulations is permissible before consciousness questions are resolved
- How to handle emulations that report suffering (whether or not they are "truly" conscious)

**Rationale:** Waiting for philosophical consensus on consciousness will delay needed ethical guardrails indefinitely. Precautionary principles should apply to systems that might be conscious, not only to systems proven conscious.

**Implementation Reality:** Government-level ethical frameworks face significant political barriers: deep philosophical disagreement about consciousness, religious/secular divides, international coordination challenges. Such frameworks typically require triggering events (major incidents, consciousness breakthroughs, or sustained advocacy campaigns) to create political urgency. Realistic timeline: 5-10 years absent such triggers, or 12-24 months following a major incident.

**Interim Action:** Until government frameworks emerge, individual institutions should develop provisional guidelines using precautionary principles. Professional societies should issue best-practice recommendations. Institutional review boards should extend existing protections for animal research to cover emulations above specified complexity thresholds.

## Open Questions (Ω)

### Ω: Consciousness Criteria — What observable would distinguish conscious emulation from behaviorally accurate zombie?

Current proposals (IIT phi, global workspace architecture, neural correlates) test for correlates of consciousness in biological systems, not for consciousness itself. This may reflect a fundamental limitation: consciousness might be a first-person property inaccessible to third-person measurement. If so, substrate-independence is not a scientific hypothesis but a metaphysical commitment.

**What would resolve this:** Philosophical convergence on operationalized criteria, or acceptance that the question is empirically underdetermined and requires value-based rather than fact-based resolution.

### Ω: Plasticity Threshold — At what scale does lack of plasticity invalidate behavioral fidelity?

Projected fruit fly emulations use static connectomes and simple LIF neurons. Whether this suffices may depend on organism complexity: fly behavior may be largely hardwired, while mammalian behavior requires ongoing synaptic modification. The *C. elegans* failure (no behavioral fidelity despite complete connectome since 1986) suggests even small nervous systems may require dynamic processes.

**What would resolve this:** Systematic comparison of emulation accuracy across organism scales. If mouse emulations fail despite complete connectomes, this would demonstrate a plasticity threshold. If they succeed, this would support static-connectome sufficiency at mammalian scale.

### Ω: Validation Impossibility — How to validate human emulation fidelity without ground truth?

We can compare fruit fly emulations to biological fruit flies across thousands of organisms. We cannot compare a human emulation to its source person's subjective experience—only to external behaviors. This creates an epistemic gap: the validation methodology that works at small scale becomes inapplicable at human scale.

**What would resolve this:** Either accept that human-scale validation is impossible (making human emulation unverifiable), or develop proxy metrics (neural activity patterns, information integration measures) and accept uncertainty about whether they capture what matters.

### Ω: Body-Brain Mismatch — Does the body-brain mismatch explain sub-100% accuracy?

Projected demonstrations use generic body simulations, not the bodies of organisms whose brains were mapped. Biological development involves sensorimotor calibration—neural responses tuned to the specific body they inhabit. Emulations skip this process. Whether accuracy gaps reflect this mismatch or fundamental model limitations is unknown.

**What would resolve this:** Co-map brain and body from the same organism, then compare emulation accuracy using matched vs. generic body models. If matched bodies increase accuracy to 95%+, this confirms the mismatch hypothesis. If accuracy remains at 91%, this points to other limitations.

### Ω: Investment Capital Distortion — Does funding concentration reflect scientific promise or create self-fulfilling prophecy?

Whole-brain emulation research receives substantial investment based on the substrate-independence assumption. This creates incentive structures that reward confirming the assumption rather than testing it. Alternative approaches (embodied AI, developmental robotics, hybrid biological-computational systems) receive less funding despite potentially offering complementary insights.

**What would resolve this:** Diversified funding across multiple approaches to brain-like intelligence, with explicit comparison of progress rates and capability benchmarks. If emulation-first approaches consistently outperform alternatives, this validates the investment concentration. If not, this reveals path-dependency rather than scientific superiority.

---

## METADATA

**Adversarial Review:**
- Weakest link: The consciousness substrate-independence section relies on philosophical argument rather than empirical evidence. A critic could argue this conflates "we don't know how to test for consciousness" with "consciousness is substrate-independent." Defense: The text explicitly marks this as a conceptual confusion rather than an empirical claim, and notes that the question may be underdetermined.
- Most likely criticism: "The essay is too skeptical of genuine scientific breakthroughs." Defense: The essay accepts behavioral accuracy claims as significant if validated, while separating these from consciousness claims. Skepticism targets conceptual conflation, not technical achievement.

**Brittleness Assessment:**
- Independent evidence lines: 3 (FlyWire publication, Shiu model, OpenWorm baseline)
- Critical dependencies: The analysis depends on projected demonstrations being validated through independent replication. If validation fails, the scaling discussion becomes entirely speculative. However, the consciousness analysis stands independently—it critiques the conceptual framework regardless of technical success.

**Source Quality:**
- Tier S sources: 3 (Nature publications for FlyWire and Shiu model, OpenWorm as comparative baseline)
- Tier C sources: 0 (projected demonstrations are explicitly marked as Tier 3 hypotheses requiring validation)

**Model Transparency:**
- Models used: Deferential Realism constraint analysis
- Visibility mode: B (invisible scaffolding)
- Limitations disclosed: N/A for Mode B

**DR Scaffolding (Mode B):**
- Constraint stories used: 3 (connectome_sufficiency, scaling_feasibility, consciousness_substrate_independence)
- Structural signatures detected:
  - connectome_sufficiency: false_natural_law (physics-washed—claims natural necessity but shows constructed coupling)
  - scaling_feasibility: coupling_invariant_rope (genuine coordination mechanism, pristine purity)
  - consciousness_substrate_independence: false_natural_law (physics-washed—presents philosophical framework as empirical finding)
- Purity gradient: scaling_feasibility (0.988, pristine) receives strong language; connectome_sufficiency (0.369, contaminated) and consciousness_substrate_independence (0.313, contaminated) receive cautious language with explicit epistemic hedging
- Omega-to-question mapping:
  - omega_plasticity_threshold → "At what scale does lack of plasticity invalidate behavioral fidelity?"
  - omega_consciousness_criteria → "What observable would distinguish conscious emulation from behaviorally accurate zombie?"
  - omega_validation_impossibility → "How to validate human emulation fidelity without ground truth?"
  - omega_body_brain_mismatch → "Does the body-brain mismatch explain sub-100% accuracy?"
  - investment_capital_distortion → "Whether funding concentration reflects scientific promise or creates self-fulfilling prophecy?"
- Unsupported translations: None—every DR-derived insight has independent Tier 1 evidence from published papers or clearly marked projections