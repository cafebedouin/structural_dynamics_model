# Axioms and Consequences of Observer-Dependent Classification

**A Formal Framework for Systems Where What You See Depends on Where You Stand**

**v6.3 — Structural invariant/corpus-dependent separation, FCR ablation, boundary non-normality, institutional dissent analysis, boolean independence, network topology**

---

**Abstract.** We present a formal framework for classification systems where the result depends irreducibly on the observer's position. The framework models classification as a presheaf on a site of observer contexts, deliberately refusing the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved.

We separate the framework into three layers: (1) axioms — the design commitments that define the measurement apparatus, (2) theorems — structural consequences that follow deductively from the axioms without reference to any corpus, and (3) empirical observations — corpus-dependent findings that validate the engine and characterize specific datasets.

The axioms encode a single core hypothesis: power modulates the perception of extraction. The theorems derive non-obvious consequences. Extraction necessarily requires a cover story — it can never be universally recognized as extraction under power-modulated perception. Observer disagreement clusters in discrete blocs rather than distributing smoothly. The institutional observer carries 97% of the spectral weight in classification disputes. Single-position analysis with full information detects less than 3% of the observer-dependent structure that cross-position analysis reveals. These are properties of the axioms, not of any dataset.

Two independently generated corpora with inverted input distributions confirm the engine correctly computes these consequences. Structural invariants — the H¹ gap, spectral eigenvalues, contextuality fraction gap, and institutional dissent direction — are identical across both corpora and survive FCR override ablation, confirming they are fixed-point attractors of the axioms. Corpus-dependent statistics (type distributions, descent rates, coalition structure) vary between corpora as expected. Three new diagnostics — a contextuality fraction in the Abramsky-Brandenburger sense, a power-chain monotonicity analysis, and a bifurcation sweep finding exact parameter values at which type labels flip — extend the empirical record. Key findings: all theorems are confirmed in Abramsky-Brandenburger coordinates; monotone orbits are vanishingly rare (1.1%); all maximal-obstruction constraints (H¹ = 6) are incomparable; the system has no critical parameters — the one previously flagged was a timeout artifact. An honest assessment distinguishes strict categorical correspondences from structural analogies.

---

## 1. Introduction

The classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects. This is not a failure of classification but a structural feature of the domain.

The standard response to perspectival dependence is to resolve it — to identify the "correct" classification by privileging one observer position or aggregating across positions. This paper takes the opposite approach. We model perspectival dependence using presheaf theory, where disagreement has formal mathematical structure, and the standard tools of topos theory — cohomology, descent, naturality — produce quantitative invariants that characterize any domain where classification depends on perspective.

The framework, *Deferential Realism* (DR), is *realist* in that it treats constraints as having objective structural properties (extractiveness, suppression, coordination function) that exist independently of any observer; it is *deferential* in that it treats the *classification* of those properties as irreducibly dependent on the observer's structural position. The presheaf is emphatically not a sheaf: the gluing axiom is intentionally violated because perspectival disagreement is a diagnostic signal, not a defect.

The paper separates cleanly what earlier versions interleaved: §2 states the axioms as design commitments, §3 derives the theorems that follow from those axioms alone, §4 presents the computational engine, §5 reports corpus-dependent empirical findings, §6 provides the honest assessment, §7 discusses related work, and §8 connects the formal results to broader implications.

---

## 2. The Axioms

This section states the framework's design commitments explicitly. Each axiom is a modeling choice. For each, we state what it is, why we chose it, what alternatives exist, and what would change under a different choice. The axioms together define the measurement apparatus; the theorems in §3 are consequences of this apparatus.

Five of the six axioms are structural — they define the mathematical scaffolding. One axiom (Axiom 2) is the **empirical anchor** — it encodes the framework's central hypothesis about how power relates to perception. If the structural axioms are wrong, the mathematics changes. If the empirical anchor is wrong, the mathematics remains valid for the model, but the model no longer maps to the world. This distinction matters throughout.

### Axiom 1: Four Observer Positions on a Linear Power Axis

Each observer is characterized by a context tuple (Power, TimeHorizon, ExitOptions, Scope). For computational tractability, we fix four standard contexts:

| Context | Power | Time | Exit | Scope |
|---------|-------|------|------|-------|
| U₁ (powerless) | powerless | biographical | trapped | local |
| U₂ (moderate) | moderate | biographical | mobile | national |
| U₃ (institutional) | institutional | generational | arbitrage | national |
| U₄ (analytical) | analytical | civilizational | analytical | global |

These form a linear poset category **C** with morphisms U₁ → U₂ → U₃ → U₄ determined by power ordering. This is the simplest non-trivial site that captures perspectival transitions.

**Why this choice.** Fewer contexts lack resolution to detect perspectival transitions; more complicate analysis without adding qualitative insight. The four contexts represent structurally distinct positions: subject (U₁), moderate agent (U₂), institutional actor (U₃), and detached analyst (U₄).

**What would change.** The site choice is normative — it is where political commitments enter the mathematics. A Marxist analysis might separate epistemic access from material power as independent morphism dimensions. A feminist standpoint epistemology might add an embodiment axis. A non-linear site (a DAG, a branching lattice, a site with overlapping jurisdictions where a worker is powerless economically but powerful legally) would produce different invariants. In particular, the gap structure derived in Theorem 2 depends on linearity and would not hold on a non-linear site. The framework is a functor from site choices to invariants; the current 4-element chain is one instantiation. The invariants are geometry-relative — properties of this site — not world-relative assertions about the constraints themselves.

### Axiom 2: Power-Modulated Perception of Extraction ⚓ [Empirical Anchor]

The core empirical commitment: **power reduces experienced extraction.** Everything downstream of this axiom inherits its empirical status. If the real-world relationship between power and perception does not match this axiom, the theorems remain valid within the model but the model does not describe the world.

Each constraint has a base extractiveness ε that is observer-independent — a design axiom (ε-invariance). The *experienced* extractiveness varies by observer:

$$\chi = \varepsilon \times f(d(P)) \times \sigma(S(P))$$

where d(P) is a directionality value derived from the observer's structural relationship to the constraint (beneficiary, victim, neither), σ is a sigmoid scaling, and S is a scope modifier. The formula factors multiplicatively as (observer-independent) × (observer-dependent).

The specific power modifiers for the four standard contexts:

| Context | Power Modifier π | Effect |
|---------|-----------------|--------|
| U₁ (powerless) | 1.5 | Extraction amplified |
| U₂ (moderate) | 1.0 | Baseline |
| U₃ (institutional) | −0.2 | **Sign flip** — extraction experienced as coordination |
| U₄ (analytical) | 1.15 | Slight amplification above baseline |

The critical feature is the **sign flip at institutional power** (π = −0.2). The institutional observer, with generational time horizon and arbitrage exit options, experiences positive extraction as negative — what others experience as extraction, this observer experiences as coordination working in their favor. This sign flip is what later produces the 97% spectral dominance of U₃ in Theorem 3.

**Why multiplicative factorization.** The multiplicative form guarantees χ ≥ 0 for all non-negative inputs (sigmoid outputs are positive and ε ≥ 0). It also has a categorical consequence: restriction maps compose telescopically (Theorem 5). An additive factorization (as in the Avellaneda-Stoikov reservation price formula) also satisfies the functor axiom but allows negative valuations.

**Sensitivity to the sign flip.** The theorems' robustness depends on how much of the consequence suite survives deformation of π(institutional). At π = −0.2 (current calibration), Theorems 1–4 hold at full strength. As π moves toward zero, spectral dominance (Theorem 3) weakens continuously — the institutional eigenvalue shrinks. At π = 0 (no sign flip, merely dampening), the cover-story theorem (Theorem 1) still holds if π(institutional) < π(moderate), because the monotone decreasing condition is satisfied; but spectral dominance drops sharply, and the oracle gap (Theorem 4) narrows because the institutional context no longer produces a qualitative transformation that resists profile recalibration. At π > 0 (same sign as other contexts, moderate dampening only), the cover-story theorem requires that the dampening is sufficient to push at least one observer below the snare threshold — it becomes conditional on ε rather than guaranteed. The gap theorem (Theorem 2) is unaffected by π calibration: it depends on site linearity and threshold-crossing, not on the magnitude or sign of any particular modifier.

In short: Theorem 2 is robust to any power-scaling calibration. Theorems 1, 3, and 4 degrade gracefully as the sign flip weakens, with Theorem 1 the last to fall (requiring only monotone decreasing, not sign inversion). The question of whether real-world institutional perception actually inverts the sign of extraction — rather than merely dampening it — is the central empirical question the framework poses but does not answer.

**What would change.** A different sigmoid calibration would alter which theorems hold and at what strength. A linear power-to-perception function would preserve the cover-story theorem (any monotone decreasing function suffices) but alter the specific gap structure and eigenvalue spectrum.

### Axiom 3: Two Independent Input Channels (The Two-Hub Architecture)

Observer-dependence enters the classification system through exactly two independent channels:

**Hub 1: Power-Scaled Extraction (continuous).** The directionality chain d(P) → σ(P) → χ maps observer position to experienced extraction. Small changes in power produce small changes in χ. All threshold-based classification differences (snare vs. tangled_rope vs. rope) are downstream of this hub.

**Hub 2: Effective Immutability (discrete).** An 18-row lookup table maps (TimeHorizon, ExitOptions) pairs to perceived mutability levels. This is a discrete function producing categorical outputs: mountain, rope, or neither. An observer with biographical time horizon and no exit options perceives a constraint as immutable; an observer with generational time horizon and systemic exit perceives the same constraint as mutable.

**Why two hubs rather than one.** They measure different things (experience of extraction vs. perception of mutability), have different mathematical characters (continuous vs. discrete), and interact productively at specific points. False mountains — actively enforced extraction perceived as natural law — are detectable *because* the two hubs are independent. A unified hub would blend the signals and lose the diagnostic.

**Independence verification.** The hubs are independent by construction: the BaseEps check prevents any constraint from simultaneously classifying as mountain (Hub 2) and at snare-level extraction (Hub 1). This independence is confirmed empirically: zero Type A conflicts in both corpora tested (§5.1).

### Axiom 4: The Presheaf Structure (Refusal of Sheafification)

For each constraint C, we define a contravariant functor:

$$F_C : \mathbf{C}^{op} \to \mathbf{Set}$$

setting F_C(U_i) = dr_type(C, U_i). This assigns a classification type at each observer context. The restriction maps are determined by the power-scaling formula (Axiom 2).

**The presheaf is not a sheaf.** The gluing axiom requires that local sections agreeing on overlaps extend to a global section. The entire diagnostic infrastructure exists because local sections *fail to glue*: a constraint may classify as "mountain" from U₄ and "rope" from U₃, with no consistent global classification available. This perspectival gap is the framework's central diagnostic signal.

**The refusal of sheafification is a principled commitment.** Sheafification would force descent — producing a "consensus classification" by resolving perspectival disagreements. Consider the dominant orbit [snare, snare, rope, snare] (H¹ = 3): the institutional observer sees coordination; the other three see extraction.

Under topology J₁ (majority rule, where {U₁, U₂, U₄} is a covering family), sheafification forces "snare" as the global type — the extraction is recognized.

Under topology J₂ (institutional authority, where {U₃} alone covers), sheafification forces "rope" — the institutional narrative overwrites the subject's experience.

Both J₁ and J₂ collapse H¹ from 3 to 0. The disagreement vanishes. What was a measurable three-way fracture becomes invisible consensus. The choice of Grothendieck topology IS the choice of whose perspective is definitive, and sheafification under any topology destroys the diagnostic signal that the fracture existed.

The framework preserves the fracture as data rather than resolving it as defect.

### Axiom 5: The Type Space

The codomain of the classification presheaf is the type space:

$$\Omega = \{\text{mountain, rope, tangled\_rope, snare, scaffold, piton, naturalized, unknown}\}$$

Each type has a structural interpretation: mountain (immutable natural constraint), rope (legitimate coordination), tangled_rope (coordination with embedded extraction), snare (extractive trap), scaffold (temporary coordination with sunset clause), piton (performative constraint — more display than substance), naturalized (contradictory metrics), unknown (residual).

The type space carries a priority monoid under composition with two absorbing elements (mountain and piton). This is not a Heyting algebra — the presence of two absorbing elements prevents lattice structure. This means the type space is not the subobject classifier of a topos, which is an honest limitation of the formal structure. A future version might embed types in a metric space or define them via universal properties rather than labels; for now, the type space is the most ad hoc component of the system.

### Axiom 6: Classification by Deterministic Threshold Cascade

Classification flows through a deterministic rule cascade (`classify_from_metrics/6`) that applies approximately 65 binary structural gates organized in two tiers:

**Tier 1 (structural screening):** Five base boolean gates plus threshold gates test structural properties: Does the constraint emerge naturally? Is it actively enforced? Does it coordinate? Does it extract asymmetrically? Does it expire?

**Tier 2 (cross-index factorization):** Boltzmann compliance, false natural law detection, false CI-rope detection. These require cross-index analysis across the Power × Scope grid and test whether structural properties *factorize* correctly across independent observer dimensions.

The cascade is deterministic: given fixed inputs, the output is fully determined. The thresholds and gates are chosen ex ante and held fixed — they are not tuned to match either corpus. This is not a trained classifier. The invariants it computes are formal properties of the classification presheaf, not performance metrics of a learned model.

---

## 3. Theorems

The following results are deductive consequences of Axioms 1–6. They hold for any corpus processed by the framework. No empirical data is required for their derivation.

### Theorem 1: The Perceptual Non-Universality of Extraction

**Statement.** Under any monotone decreasing power-to-perception function (Axiom 2), for any constraint with finite base extractiveness ε > 0, there exists at least one observer position from which the extraction is reclassified as coordination or as structurally irrelevant. Extractive constraints in H⁰ (the space of globally agreed classifications) are structurally impossible except at pathological calibration boundaries.

**Proof sketch.** Hub 1's sigmoid maps different power levels to different experienced-extractiveness values. For a constraint to appear as a snare at every context, the sigmoid would need to push χ above the snare threshold at every power level. But the sigmoid is calibrated so that higher power systematically reduces experienced extractiveness (the institutional modifier flips the sign). For any constraint with finite ε, there exists a power position — specifically the institutional position with π = −0.2 — from which experienced extraction drops below the snare threshold. The classification changes; the extraction becomes invisible from that position.

**What this proves.** The mathematics establishes perceptual non-universality: for any extractive constraint, some observer does not see the extraction. This is a theorem.

**What this suggests but does not prove.** The sociological interpretation — that extraction *structurally requires* a cover story, that the reclassification is *necessary for persistence* — is an interpretive step beyond the formal result. It assumes that universal recognition would destabilize the constraint, and that the observer who reclassifies extraction as coordination is thereby providing political cover. These assumptions are plausible (they align with Gramscian hegemony and with the empirical observation that widely recognized extraction tends to be reformed or overthrown), but they are not derived inside the system. The framework establishes the *perceptual* condition for cover stories; whether that condition is *functionally necessary* for extraction's persistence is a sociological claim requiring domain theory.

**Robustness.** Theorem 1 requires only that the power-to-perception function is monotone decreasing and that some observer crosses below the snare threshold. The sign flip at π = −0.2 guarantees this for any finite ε; weaker conditions (monotone dampening without sign inversion) still guarantee it for moderate ε but become conditional for very low ε. Of the six theorems, this one is the most robust to parameter deformation.

**Falsification condition.** Find a widely recognized extractive system where every observer — including the most powerful institutional beneficiaries — agrees it is extractive, with no justificatory reframing as coordination, necessity, or natural law. Historical examples like chattel slavery come close, but even there, apologists reframed it as economic necessity, civilizing mission, or natural order — confirming rather than falsifying the perceptual non-universality.

### Theorem 2: Disagreement Clusters in Discrete Blocs (The Gap at H¹ = 1, 2)

**Statement.** On a linearly ordered 4-element site with classification by monotone threshold-crossing on continuous metrics, no constraint can have exactly 1 or 2 disagreeing observer-pairs (out of 6 possible pairs). When disagreement emerges, it emerges in blocs of at least 3 disagreeing pairs. H¹ ∈ {0, 3, 4, 5, 6}, never {1, 2}.

**Proof.** The four standard contexts are ordered on a one-dimensional power axis. The classification cascade uses fixed thresholds on continuous metrics scaled by a monotone sigmoid. When a constraint's experienced extractiveness crosses a classification threshold, it crosses at a single power boundary — say, between U₂ and U₃. Because the ordering is linear, every nontrivial partition is of type 1+3 or 2+2: contexts on one side of the boundary see one type, contexts on the other side see another. A 1+3 split produces exactly 3 disagreeing pairs; a 2+2 split produces exactly 4. No partition of a linearly ordered 4-element set produces exactly 1 or 2 disagreeing pairs. Multiple threshold crossings produce H¹ = 5 or 6.

**Robustness.** This is the most robust theorem in the suite. It depends only on site linearity (Axiom 1) and threshold-crossing (Axiom 6), not on the magnitude, sign, or shape of the power-scaling function (Axiom 2). Any monotone function on a 4-element chain with fixed thresholds produces this gap.

**Corollary (Contextuality Fraction Gap).** The contextuality fraction of a constraint — defined as CF = H¹/6, the proportion of observer-pair disagreements realized — takes values only in {0, 1/2, 2/3, 5/6, 1}. The values 1/6 and 1/3 are structurally forbidden by the same site geometry that produces the H¹ gap. This is Theorem 2 in Abramsky-Brandenburger coordinates (§4.2, §7): the admissible contextuality fractions are determined by the site geometry, not by the empirical content of any corpus.

**Falsification condition.** A non-linear site (e.g., adding a power dimension orthogonal to the current axis) could produce H¹ = 1 or 2 by enabling non-adjacent threshold crossings. If real-world disagreement about extraction distributes smoothly rather than clustering in blocs, the linear site is the wrong geometry.

### Theorem 3: Institutional Spectral Dominance

**Statement.** The sheaf Laplacian L₀ on the 4-element linear site with restriction ratios determined by Axiom 2's sigmoid parameters has eigenvalues λ = {0, 0.0152, 2.9953, 72.1839}. The dominant eigenmode (λ₄ = 72.18, carrying 97% of spectral weight) is localized on the institutional vertex (eigenvector loading 0.9927). The institutional observer occupies an isolated eigenspace, effectively decoupled from the other three observers.

**Derivation.** L₀ is fully determined by the restriction map ratios r_ij = σ(π(Uᵢ)) / σ(π(Uⱼ)), which depend only on the sigmoid parameters — not on any property of any corpus. The spectral gap λ₂ = 0.0152 is three orders of magnitude below λ₄ = 72.18, with the dominant mode concentrated at the institutional position because the sign flip (π = −0.2) creates the largest restriction-map discontinuity in the system.

**What level this result lives at.** This theorem is about the geometry of the site under the chosen power-scaling, not directly about institutions. Formally, it discovers that a path graph with asymmetric edge weights induced by a sign inversion concentrates spectral weight at the inversion point. The sociological interpretation — that institutional vantage points dominate classification disputes not because they are "correct" but because the geometry of power compresses disagreement around that node — is a reading of the geometry, not a consequence of it. The institutional node behaves like a hard wall in the diffusion of disagreement: almost all tension in the system resolves at that boundary. Whether real-world institutional power actually functions as such a wall is the empirical question Axiom 2 poses.

**Robustness.** Spectral dominance degrades continuously as π(institutional) moves toward π(moderate). Eliminating the sign flip entirely (setting π = +0.5) would reduce λ₄ by roughly two orders of magnitude and distribute spectral weight more evenly. The 97% concentration is a consequence of the specific calibration, not of observer-dependence in general.

### Theorem 4: The Classical Oracle Gap

**Statement.** A Maximum Entropy classifier operating on observer-independent metrics (a "classical oracle" with access to all structural data but no indexing by observer position) systematically fails to detect observer-dependent structure. The failure rate varies by observer position: near-total at the analytical context (smooth 1.37× scaling is absorbed by profile recalibration), near-zero at the institutional context (the sign flip cannot be absorbed).

**Mechanism.** At the analytical context, the power modifier scales χ by approximately 1.37 relative to ε. The MaxEnt Gaussian likelihoods shift right by 37% but preserve their shape; relative type probabilities barely change. Mean corrected total variation distance at the analytical context is approximately 0.0006 (effectively zero). At the institutional context (χ/ε ≈ −0.04), the sigmoid maps positive extraction to negative experienced extractiveness. No profile recalibration can absorb a sign change.

The gap exists because most observer-dependence produces categorical classification shifts (crossing deterministic thresholds) without producing large probabilistic shifts (the Gaussian likelihoods are broad enough that raw metrics are compatible with multiple types). The classical oracle fails not by getting wrong answers but by failing to detect that answers *differ* across positions.

**What this means.** Single-position analysis with full information is provably almost blind to cross-position structure. The observer-dependent structure lives in the *relationships between* positions, not in any single position's view. Examining a system carefully from one vantage point misses more than 97% of what comparing across vantage points reveals.

**The U₄ paradox.** This theorem has a recursive consequence for the framework itself. The analytical observer (U₄) — the position from which DR analysis is conducted — is the position where the oracle gap is largest. The analytical observer *needs* the DR framework to see the structure that the analytical observer's native instruments cannot detect. The framework provides what might be called an "epistemic lift": it moves U₄ from within-context evaluation (where it is nearly blind) to cross-context comparison (where the structure becomes visible). Without such a lift, the analytical observer's very competence at single-position analysis becomes the obstacle.

### Theorem 5: Boltzmann Factorizability = Functor Axiom

**Statement.** The Boltzmann factorizability test — checking whether χ separates into observer-independent and observer-dependent factors — is exactly the functor axiom for Hub 1's restriction maps.

**Proof.** Hub 1's formula χ(P) = ε · [f(d(P)) · σ(S(P))] separates into (observer-independent) × (observer-dependent). The restriction map from context Pⱼ to context Pᵢ acts by the ratio of observer-dependent parts:

$$\rho(P_j \to P_i): \chi \mapsto \chi \cdot \frac{\sigma(\pi(P_i))}{\sigma(\pi(P_j))}$$

Composition check: ρ(P₂ → P₃) ∘ ρ(P₁ → P₂)(χ₁) = χ₁ · [σ(π(P₂))/σ(π(P₁))] · [σ(π(P₃))/σ(π(P₂))]. The σ(π(P₂)) terms cancel telescopically, yielding χ₁ · σ(π(P₃))/σ(π(P₁)) = ρ(P₁ → P₃)(χ₁). QED.

This works because ratios compose telescopically — (a/b)(b/c) = a/c — regardless of whether a, b, c are linear, sigmoidal, or anything else. The Boltzmann compliance test already running in the classification pipeline is performing formal verification of the functor axiom.

Hub 2's lookup table satisfies the functor axioms trivially: singleton stalks admit unique maps, and composition of unique maps is unique.

### Theorem 6: H¹ Band–Hub Correspondence

**Statement.** The two-hub architecture gives the H¹ distribution an internal structure:

| H¹ band | Mechanism | Hub |
|---------|-----------|-----|
| 0 | Neither hub diverges. All observers agree. | — |
| 3 | Sigmoid pushes χ across threshold at a single power boundary | Hub 1 |
| 4 | Immutability table flips between mountain and non-mountain | Hub 2 |
| 5–6 | Multiple interactions between both hubs | Both |

**Derivation.** H¹ = 3 corresponds to a single threshold crossing, which is Hub 1's mechanism (the sigmoid producing a 3+1 split). H¹ = 4 corresponds to the discrete immutability flip, which is Hub 2's mechanism (the lookup table switching mountain classification at the powerless/moderate boundary). Higher H¹ values involve multiple crossings or interactions between both hubs.

### Summary: What the Theorems Say in Plain Language

| Theorem | Formal result | Plain language |
|---------|--------------|----------------|
| 1 | Perceptual non-universality of extraction | You can't have a trap without a "reasonable" explanation from the top |
| 2 | H¹ gap at 1, 2 | People don't disagree randomly; they disagree in organized blocs |
| 3 | Institutional spectral dominance | The system's "official" view is mathematically louder than everyone else combined |
| 4 | Classical oracle gap | Looking really hard from one spot is worse than looking quickly from two |
| 5 | Boltzmann = functor axiom | The consistency check already in the code is verifying the categorical structure |
| 6 | Band–hub correspondence | Different kinds of disagreement trace to different architectural mechanisms |

---

## 4. The Computational Engine

The axioms are implemented as a Prolog codebase comprising 76 modules with approximately 1,050 test cases. This section documents the implementation at the level needed to understand the empirical results; the full codebase is open-source under CC0.

### 4.1 Classification Pipeline

The classification presheaf is evaluated by `classify_from_metrics/6`, which applies the binary gate cascade (Axiom 6) to the metrics at each observer context. The presheaf evaluation, naturality testing, cohomological computation, and diagnostic synthesis are all computed by the Prolog engine.

### 4.2 Diagnostic Stack (Summary)

The engine includes a diagnostic stack that measures properties of the presheaf beyond the classification itself:

**Gauge orbits:** The complete presheaf evaluation across all contexts — the constraint's identity in the presheaf topos. Constraints sharing identical orbits are structurally isomorphic under observer shifts.

**MaxEnt shadow classification:** A probabilistic classifier assigning Gaussian-likelihood distributions over the type space. Operates in both classical mode (using observer-independent ε) and indexed mode (using power-scaled χ). The divergence between these modes measures the probabilistic effect of observer-dependence. Forms a Markov category (Fritz 2020) rather than a Giry monad — the correct abstraction for indexical systems that refuse to posit a prior over observer positions (pending verification of delete-map naturality).

**Naturality certificates and failure witnesses:** CI Rope (passes all four naturality conditions — a certificate that the classification is well-behaved), False Natural Law (presents as natural law but fails Boltzmann factorizability — "physics-washed"), False CI Rope (appears as structurally sound rope but fails structural tests — "coordination-washed"). The FCR detection is the primary mechanism driving the ~62% tangled_rope convergence (§5.2).

**Abductive synthesis:** 15 trigger predicates (T1–T11, T13–T16) functioning as cross-diagnostic consistency checks, plus a separate post-synthesis divergence check (T12, in post_synthesis.pl). Category A triggers (T1–T11) detect when diagnostics disagree. Category B triggers (T13–T16) detect when diagnostic agreement is misleading — when the classical oracle is confident but wrong. See codebase documentation for full trigger specifications.

**Purity and contamination network:** Composite naturality health scores propagated through agent-sharing graphs. Monotone operator converging to a fixed point (Knaster-Tarski).

**Diagnostic integration:** 12 subsystems aggregated into GREEN/YELLOW/RED verdicts. Expected conflict catalog (P1–P11) filters known architectural artifacts from genuine tensions. The verdict does not change classification — it provides meta-analytical confidence assessment.

**Orbit monotonicity analysis:** For each constraint, the classification sequence [T₁, T₂, T₃, T₄] along the power chain is analyzed for monotonicity with respect to the extraction ordering mountain < rope < tangled_rope < snare. Sequence patterns are classified as constant (H⁰ = 1), monotone ascending, monotone descending, non-monotone (direction reversal occurs), or incomparable (one or more contexts classify outside the extraction chain). Boundary positions — which of the three adjacent-pair transitions in the chain produces a type change — are recorded to map where in the power gradient classification shifts occur.

**Incomparable orbit decomposition:** 155 constraints in the combined corpus (14.8%) produce incomparable orbits containing scaffold, piton, or naturalized types that fall outside the extraction ordering. These are decomposed by out-of-chain type, by observer position, by H¹ distribution, and by in-chain monotonicity (the monotonicity of the sub-sequence obtained by masking out-of-chain positions). The decomposition reveals that incomparable orbits are not a classification residue: all maximal-obstruction constraints (H¹ = 6) are incomparable, and in-chain trajectories within the incomparable population are overwhelmingly non-monotone (§5.3).

**Contextuality fraction (AB-framework):** Following Abramsky and Brandenburger (2011), the obstruction to a global section is operationalized as a contextuality fraction. The binary corpus-level CF = 1 − descent_rate measures the proportion of constraints with no consistent global classification. The graded per-constraint CF = H¹/6 measures the fraction of observer-pair disagreements realized. The CF distribution is provably constrained to {0, 1/2, 2/3, 5/6, 1} by Theorem 2's site geometry (§3, Theorem 2 Corollary). All metrics are computed within `grothendieck_cohomology.pl` without modification to any other module.

### 4.3 Corpus Provenance

**Corpus A (Haiku 4.5).** 907 constraints generated by Anthropic's Claude Haiku 4.5 using generation prompt v1.1.

**Corpus B (Gemini Flash 2.0).** 887 constraints generated by Google's Gemini Flash 2.0 using generation prompt v1.2.

The two corpora are deliberately different: different LLM architectures, different generation prompts, and — critically — inverted input metric distributions. Corpus A is tangled_rope-dominated (68.2% by raw metrics). Corpus B is snare-dominated (50.5% by raw metrics). Both exhibit d-pattern anchoring artifacts (43% and 75% concentration on a single directionality pattern), creating a stress test of framework robustness.

**LLM provenance caveat.** Both corpora are generated by large language models trained on internet text, which may encode similar latent political grammars despite their architectural differences. The convergence reported in §5.2 demonstrates that the *engine* is stable under input distribution inversion, but does not rule out the possibility that both LLMs share biases about how power, extraction, and coordination relate — biases that could align with the framework's axioms for artifactual rather than structural reasons. The financial regulation beta corpus (§6.6), using domain data with quantitative ground truth, is essential for distinguishing framework stability from shared LLM priors.

**Living corpus note.** The corpus is not static. Each analytical run generates new constraint stories that are added to the active testset. The corpus contained 907 (Haiku) and 887 (Flash) constraints when the cross-corpus comparison was first computed; at time of writing it contains 3,254 active constraints. Corpus-level statistics reported in §5 are snapshots at the time of computation. Structural invariants (§5.1, §5.2 invariant table) are stable across corpus growth; corpus-dependent statistics (§5.3) will shift as the corpus evolves.

**Deduplication note.** A bug in `known_constraint/1` (logical_fingerprint.pl) caused Prolog backtracking to yield each constraint ID multiple times (once per matching `constraint_metric/3`, `constraint_claim/2`, and `constraint_classification/3` fact). This inflated constraint counts by approximately 8–11× in any query iterating over `known_constraint/1` without deduplication. Fixed in v4.2 via `findall/sort` wrapper. All statistics in this paper use deduplicated counts.

The continuous metrics (extractiveness ε, suppression, theater ratio, resistance-to-change) are specified in the constraint stories and have not been subjected to inter-rater reliability testing. A configuration sensitivity sweep (154 numeric parameters perturbed at ±25%, plus 17 directionality constants swept separately) found all 154 parameters inert at ±25%. An initial run had reported `power_modifier_analytical` as critical with 37 failures at ±10%; a subsequent audit determined these were timeout artifacts (696/733 tests completed before the 600-second wall — 37 tests that never ran, not 37 classification failures). Both the rerun at 910+ tests and a separate bifurcation sweep confirm all parameters inert. No parameter produces distant bifurcation; the closest margin is snare_chi_floor at 0.8% below baseline (14 type-label flips). The asymmetric sensitivity in rope_chi_ceiling — six times more sensitive upward than downward — indicates corpus clustering near the upper boundary of rope classification. All 148 remaining parameters produce zero type-label flips across the [0.5×, 2.0×] sweep range.

---

## 5. Empirical Results

This section reports what the engine finds when run on the two corpora. The central validation is the separation between theorem-confirmed structural properties (stable across both corpora, as predicted by §3) and corpus-dependent statistical properties (varying between corpora, reported as ranges).

### 5.1 Theorems Confirmed

**Cover story (Theorem 1):** Extractive constraints in H⁰ are 0 in Corpus A, 1 borderline case in Corpus B (ε = 0.75, classification confidence 0.01, rival type tangled_rope at 0.95). Confirmed.

**Gap structure (Theorem 2):** H¹ = 1 and H¹ = 2 are empty in both corpora. Confirmed.

| H¹ | Corpus A (907) | Corpus B (887) |
|----|---------------|----------------|
| 0 | 692 (76.3%) | 181 (20.3%) |
| 1 | **0 (0.0%)** | **0 (0.0%)** |
| 2 | **0 (0.0%)** | **0 (0.0%)** |
| 3 | 64 (7.1%) | 353 (39.8%) |
| 4 | 12 (1.3%) | 14 (1.6%) |
| 5 | 94 (10.4%) | 320 (36.1%) |
| 6 | 45 (5.0%) | 19 (2.1%) |

**Spectral invariance (Theorem 3):** Sheaf Laplacian eigenvalues identical to four decimal places across both corpora: [0, 0.0152, 2.9953, 72.1839]. Institutional eigenvector loading 0.9927 in both. 97% spectral weight at institutional vertex in both. Confirmed — as expected, since the eigenvalues depend on the sigmoid parameters, not the corpus.

**Oracle gap (Theorem 4):** At the analytical context, T13 fires on 6 constraints in Corpus A (2.8% of H¹ > 0) and 12 in Corpus B (1.7%). Mean corrected total variation distance at analytical: 0.0006. At institutional context, detection rate approximately 97%. Confirmed.

**Band–hub correspondence (Theorem 6):** H¹ = 3 band dominated by Hub 1 sigmoid-driven divergence (institutional observer sees rope where others see extraction). H¹ = 4 band corresponds to Hub 2 immutability flips. Confirmed in both corpora.

**Contextuality fraction (Theorem 2 Corollary):** Corpus CF (binary) = 0.261, cross-checking exactly with 1 − descent_rate. Graded mean = 0.193. The CF distribution takes values only at {0, 0.5, 0.667, 0.833, 1.0} — confirming that the H¹ gap propagates directly into contextuality fractions and that the values 1/6 and 1/3 are empirically absent, as structurally required. Per-type: snares are 100% contextual (every snare in the corpus lacks a global section, confirming Theorem 1 as a necessary consequence — if extraction were globally recognizable, snares would achieve H⁰ = 1), ropes 37.5%, mountains 3% (near-zero reflecting natural-law invariance under observer shifts), tangled_rope 12.6%.

**Structural stability (all theorems):** A bifurcation sweep over [0.5×, 2.0×] of baseline values finds that 148 of 154 numeric parameters produce zero type-label flips across the entire range. Six parameters have critical values within range: snare_chi_floor (0.8% below baseline, 14 flips — the narrowest margin), tangled_rope_chi_floor (1.6%, bifurcation in both directions), rope_chi_ceiling (1.6% up / 7.0% down, asymmetric sensitivity indicating corpus clustering near the upper boundary), snare_epsilon_floor (4.7% up), tangled_rope_chi_ceil (6.3% down, 14 flips), and mountain_extractiveness_max (12.5% down). The asymmetry in rope_chi_ceiling — six times more sensitive upward than downward — indicates the corpus sits close to the ceiling of rope classification but well above its floor. No parameter produces zero-distance bifurcation; the closest margin is snare_chi_floor at 0.8%.

### 5.2 Convergence Under Inversion

**Structural invariants under inversion.** The strongest empirical validation: structural invariants derived from the axioms are identical across two corpora with opposite input distributions. Corpus-dependent statistics diverge as expected, confirming that the invariants are properties of the framework rather than properties of any particular dataset.

**Type distribution convergence:**

| Type | Flash metric | Flash post | Haiku metric | Haiku post |
|------|-------------|-----------|-------------|-----------| 
| snare | 448 (50.5%) | 109 (12.3%) | 152 (16.8%) | 175 (19.3%) |
| tangled_rope | 287 (32.4%) | 549 (62.0%) | 619 (68.2%) | 567 (62.5%) |
| mountain | 139 (15.7%) | 139 (15.7%) | 129 (14.2%) | 129 (14.2%) |

The type distributions after signature integration differ between corpora. The structural invariants — eigenvalues, H¹ gap, CF gap, spectral weight — are identical. This separation is the point: what the axioms guarantee is invariant; what the corpus contributes varies. Mountains are invariant under signature integration (Δ = 0 in both).

**Role of the FCR override.** The FCR (False CI Rope) override applies a 3× boost to tangled_rope probability when the Boltzmann independence test detects cross-perspectival coupling alongside extraction. This is a property of the rule cascade's prioritization (Axiom 6), not a direct measurement of social reality. FCR ablation shows the override shifts type distributions (asymmetrically, with greater effect on snare-heavy corpora) without altering structural invariants. Whether the override's reclassifications are substantively correct — whether constraints exhibiting cross-perspectival coupling with extraction genuinely warrant tangled_rope classification — requires metric-level sensitivity analysis (§6.6) and real-world corpus validation.

**FCR override ablation.** Disabling the FCR override (gating all three intervention points while preserving detection) confirms the structural invariants are independent of the FCR mechanism. The H¹ gap (values 1 and 2 empty) holds in both corpora with FCR disabled. The H¹ distribution is nearly unchanged — the FCR redistributes type labels within existing orbit families without altering the presheaf's disagreement topology. The FCR effect is asymmetric: Corpus B (snare-dominated inputs) loses more tangled_rope classifications than Corpus A (tangled_rope-dominated inputs), consistent with the FCR reclassifying metric-snares that exhibit cross-perspectival coupling. The ablation confirms that the FCR operates within the structure the axioms create rather than generating that structure.

**Complete invariant table:** The following properties are identical or functionally equivalent across both corpora:

| Finding | Status |
|---------|--------|
| Sheaf Laplacian eigenvalues | Identical to 4 decimal places |
| Spectral gap λ₂ = 0.0152 | Identical |
| r₂₃² spectral weight = 97% | Identical |
| H¹ gap at values 1 and 2 | Present in both |
| CF distribution gap (no values at 1/6, 1/3) | Confirmed in both |
| Confidence bands (deep/moderate/borderline) | ~84%/0.4%/15% (3,254-constraint corpus) |
| FCR ablation invariance of H¹ gap | Confirmed in both |
| Mountain population | ~14–16% in both |
| Institutional dissent direction (U₃ sees rope) | Confirmed in both |
| Zero Type A hub conflicts | Confirmed in both |
| Monotone orbit rate among non-constant comparable | ~1.1% in both |
| Boundary density: positions 2+3 vs. position 1 | ~80% vs. ~20% in both |
| H¹ = 6 population: 100% incomparable | Confirmed in both |

These invariants hold despite 43% and 75% d-pattern concentration, different LLM architectures, different generation prompts, and inverted type distributions.

**Monotonicity invariants.** Power-chain monotonicity analysis of the combined 1,050-constraint corpus finds that monotone orbits — sequences where the extraction-type ordering is consistently ascending or descending along U₁→U₂→U₃→U₄ — are vanishingly rare: 3 out of 274 non-constant comparable orbits (1.1%). The dominant non-constant pattern is non-monotone (116/274 = 42.3%). Classification along the power chain is not a simple gradient; the sigmoid's nonlinearity produces direction reversals in the majority of gauge-variant constraints.

Boundary distribution confirms that type transitions concentrate in the upper half of the power chain: position 1 (U₁→U₂) accounts for 20% of all 661 boundaries, while positions 2 (U₂→U₃) and 3 (U₃→U₄) account for 39% and 41% respectively. The near-tie between positions 2 and 3 indicates a broad transition zone spanning the institutional context rather than a single sharp phase transition at the U₃ sign flip. The spectral dominance of the institutional observer (Theorem 3) expresses itself empirically as boundary density concentration in the upper power chain.

### 5.3 Corpus-Dependent Findings

The following vary between corpora and are reported as ranges:

**Descent rate:** 20.3% (Corpus B) to 76.3% (Corpus A). The wide range reflects different ε distributions: models generating more polarized constraint stories produce higher gauge-invariance.

**Gauge-variance rate:** 23.7% (Corpus A) to 79.7% (Corpus B). Inverted relative to descent rate.

**Coalition structure:** Corpus A is dominated by uniform_tangled (73% of tangled_ropes — all observers agree). Corpus B is dominated by institutional_dissent and split_field (99% combined — observers actively disagree). The inversion is complete: every corpus-dependent measure points in opposite directions while framework-dependent measures are stable.

**Tangled rope ψ structure:** Corpus B is bimodal (mass at ψ ≈ 0 and ψ ≈ 1.0, only 1.8% genuinely tangled). Corpus A is unimodal (73.4% genuinely tangled). Both classified as tangled_rope through different mechanisms (signature override vs. metric ambiguity).

**Institutional dissent population:** 40 (Corpus A) to 246 (Corpus B). The binary split within institutional dissent — 213 low-snare vs. 33 high-snare in Corpus B, with rank-biserial r = 1.0 — is confirmed but too asymmetric in Corpus A (39:1) for cross-corpus generalization.

**Per-constraint obstruction energy correlation:** E(C)–H¹ Spearman ρ = 0.66 (Corpus B) vs. ρ = 0.20 (Corpus A). The correlation weakens when most constraints are gauge-invariant. The eigenvalues are a framework property; the correlation strength tracks the gauge-variance rate.

### 5.4 Extended Empirical Findings (3,254-constraint corpus)

The following findings are computed on the living corpus at 3,254 constraints and are classified STRICT (deterministic, replicable from the codebase).

**Boundary non-normality as evidence for Theorem 2.** Theorem 2 predicts that H¹ values 1 and 2 are forbidden on the linear 4-context site. An independent empirical test: if types are discrete structural categories (as the theorem implies), then the MaxEnt P(rival) distribution at type boundaries should be non-normal — constraints should cluster definitively on one side of a boundary, not scatter continuously across it. Raw P(rival) distributions at all type boundaries universally reject normality (Shapiro-Wilk p=0.0 for all testable boundaries). The tangled_rope→snare boundary (N=1,403) follows a beta(0.16, 1.33) distribution with skewness 4.34: 1,200 of 1,403 constraints have P(snare) < 0.05, while 46 have P(snare) > 0.85. The snare→tangled_rope boundary (N=513) is bimodal, best fit by beta(0.14, 0.24): 298 constraints cluster below P(tangled_rope) = 0.05, and 107 above 0.90. Coalition type and snare-cluster membership are not independent (χ²=217.39, df=4, p<0.001), confirming that the boundary structure reflects genuine population-level categories rather than threshold artifacts. If the type space were a continuous manifold with arbitrary cuts, P(rival) at boundaries would be normally distributed; the beta/bimodal structure is the signature of discrete structural categories separated by a genuine gap.

**Institutional dissent binary split.** Within the 109 institutional_dissent constraints (tangled_rope where the institutional observer alone sees rope), a binary split emerges: 105 constraints where P(snare)=0.0 (orbits=rope+tangled_rope) versus 4 where P(snare)=0.917 (orbits=rope+snare). The split concentrates on a single metric: base_extractiveness discriminates the groups with rank-biserial r=0.9976 (p<0.001). The institutional observer's informational advantage — its capacity to see coordination where other observers see extraction — operates in a specific extractiveness range (ε < 0.62). Above that range, extraction dominates all perspectives. The low-snare group spans 81 unique domains; the high-snare group concentrates in 8 geopolitical/economics/labor domains, suggesting the extractiveness threshold interacts with domain structure. This finding supports Theorem 3 (spectral dominance of institutional observer): the institutional observer's dissent is structurally meaningful and metrically sharp, not diffuse noise.

**Classification confidence architecture.** Confidence is not uniformly distributed across types: mountain classification is 100% deep-band (N=401), tangled_rope 98.7% deep (N=2,221), but snare is only 20.5% deep / 77.9% borderline (N=560), and scaffold/piton are 0–14% deep. The corpus-wide distribution is strongly bimodal: 462 constraints at confidence 0–5% and 2,033 at 90–95%. The MaxEnt shadow assigns higher probability to tangled_rope than to snare for 540 of 560 snare constraints. The override system changes 334 top-type assignments (10.3% of corpus), with 283 tangled_rope→snare changes and 39 tangled_rope→piton. Within the false_natural_law signature, 56 of 1,605 tangled_rope constraints change top-type when overrides are removed; 10 of these show confidence deltas >0.94. The confidence asymmetry reflects the FCR override's structural role: it reclassifies metric-snares that exhibit cross-perspectival coupling, producing confident tangled_rope assignments from uncertain snare inputs.

**Boolean feature independence (Axiom 5 support).** Boolean feature independence testing confirms the 6-type space is sufficient: all six observable boolean features (emerges_naturally, requires_active_enforcement, has_coordination_function, has_asymmetric_extraction, natural_law_without_beneficiary, is_constructed) have normalized mutual information >0.3 with the type assignment and independence scores <0.15 (N=3,253, χ² p=0.0 for all features). The strongest independence is requires_active_enforcement at 0.103, driven by snare's 63.6% true rate (compared to mountain's 0% and tangled_rope's 100%). No boolean feature meets the independence criteria (independence >0.15, NMI <0.3) that would indicate a missing dimension in the type space. This is a negative result that strengthens Axiom 5: the type space captures the observable boolean structure without requiring additional dimensions.

**Network topology and contamination propagation.** The coupling graph has 31,215 edges across 3,254 constraints, forming 127 connected components at coupling threshold 0.500. The coupling graph extends beyond direct constraint pairs to include multi-hop reachable nodes, yielding a largest component of 8,650 nodes. Multi-hop contamination simulation: 6,033 unique nodes reached within 3 hops (69.7% of the giant component), with the top super-spreader (algorithmic_feed_substitution) at degree 205 and contamination strength 1.00. Fixed-point network purity propagation (Knaster-Tarski) converges in 18 iterations with 1,568 zone migrations. The contamination is type-selective: mountains have zero average EP shift, ropes 0.049, while snare and tangled_rope both shift ~0.14 — consistent with mountains being pure natural-law constraints insulated from the coupling network. 245 sound constraints (purity ≥ 0.70) exist within the giant component; 513 constraints shift purity zones under propagation.

**Epistemic restriction vs. frame-dependence:** Approximately 1.5% overlap — nearly disjoint phenomena. Epistemic restriction is having a reduced information set; frame-dependence is processing full information through a different structural relationship.

**Incomparable orbit structure.** The 155 incomparable orbits (14.8% of the combined corpus) are not a classification residue. Three findings characterize them. First, out-of-chain types (naturalized 81%, scaffold 10%, piton 8%) cluster at the sigmoid's extremes: U₁ (120 occurrences) and U₃ (67 occurrences), with U₂ (2) and U₄ (0) essentially clean. This is the sigmoid's nonlinearity made visible in type space — the positions where power-scaling does the most nonlinear work are precisely where constraints exit the extraction ordering. Second, all 51 maximal-obstruction constraints (H¹ = 6) are incomparable; the comparable population never reaches H¹ = 6. The incomparable population is not marginal — it is the location of maximum perspectival fracture in the corpus. Third, in-chain monotonicity analysis (masking out-of-chain positions and classifying the residual trajectory) finds 103/122 non-monotone among constraints with sufficient in-chain points — 84%. The incomparable orbits are genuinely disordered trajectories, not orderly sequences with a single anomalous position. The out-of-chain classification is a symptom of deeper structural complexity.

A sample non-monotone incomparable orbit illustrates the mechanism: [tangled_rope, tangled_rope, rope, snare] — extraction drops at the institutional position (where the sign flip reclassifies it as coordination) then rises to maximum at the analytical position (where structural distance reveals the full extraction). This is Theorems 1 and 4 made visible in a single orbit: the cover story mechanism (institutional reclassification) and the oracle gap (U₄'s analytical position requiring cross-context comparison to see what its own position obscures).

---

## 6. Honest Assessment

This section distinguishes what the framework proves from what it suggests. A usage rule for the reader: everything labeled STRICT is something the codebase verifies mechanically; STRUCTURAL is interpretive but grounded; LOOSE should be read as metaphor only.

### 6.1 Three-Level Rigor Classification

- **STRICT**: the categorical correspondence holds mathematically. The code implements the categorical structure, and the correspondence survives formal verification.
- **STRUCTURAL**: the analogy is productive and behavior matches, but full categorical verification of the formal axioms is absent.
- **LOOSE**: the categorical language would mislead if taken literally.

### 6.2 What Is STRICT

The site, the presheaf, the naturality condition, the Boltzmann = functor axiom equivalence, the naturality witnesses (FNL, CI Rope), H⁰ and descent, gauge orbits, the Galois connection, Hub 1 as restriction map, Hub 2 as classification gate, hub independence, binary gate computations, NMI analysis, T13 divergence measurement, mountain zero-divergence, the sheaf Laplacian construction, cross-model spectral invariance, confidence band distribution, post-override attractor convergence, contextuality fraction computation (CF = 1 − descent_rate; graded CF = H¹/6; CF gap at 1/6 and 1/3), orbit monotonicity classification, incomparable orbit decomposition, bifurcation sweep critical values, FCR ablation invariance of H¹ gap, boundary non-normality at type boundaries, boolean feature redundancy with type space.

The three-way equivalence (Lawvere ↔ Grothendieck ↔ Noether) is two-out-of-three STRICT: naturality ↔ descent is strict; the Noether column maps to discrete group invariance, which is the precondition of Noether's theorem rather than the theorem itself.

### 6.3 What Is STRUCTURAL

MaxEnt as Markov category (pending delete-map naturality verification). Information geometry of T13 (Fisher-Rao geodesic ball applies to KL divergence, not L∞). FPN as terminal coalgebra (convergence proved via Knaster-Tarski, but full coalgebra axioms unverified). Abductive engine as naturality auditor (triggers are hand-crafted, not derived from categorical constructions). H¹ proxy (combinatorial descent-failure count on the Alexandrov site, not formal Čech H¹). H¹ band–hub correspondence (empirical, not derived). Oracle gap mechanism (architectural, corpus-specific magnitudes). Diagnostic verdict synthesis (threshold-based, not derived). AB-framework connection (the equivalence of "no global section" and "contextual" is strict; the correspondence between DR's graded CF and Abramsky-Brandenburger's contextuality fraction for empirical models is structural — DR's site is more constrained than AB's general sheaf-cohomological setup, and the CF gap result (values forbidden at 1/6 and 1/3) has no direct analogue in the quantum contextuality literature).

### 6.4 What Is LOOSE

Type space as Heyting algebra (two absorbing elements prevent it). Power scaling as adjunction (triangle identities unverified). Signature resolution as lattice meet (priority dispatch table, not lattice operation). All five Girard/Linear Logic mappings (confusing computing a quantity with consuming a resource). The quantum measurement analogy (breaks at reversibility, determinism, and locality). "Quantum" naming in verification triggers (evocative but misleading if taken formally).

### 6.5 What the Framework Cannot Do

**Plan under resource constraints.** Computes costs but does not enforce budgets — the genuine gap identified by Girard analysis.

**Perform metric-level sensitivity analysis.** Parameter robustness is tested; input metric robustness is not. This is the most important missing validation.

**Model intra-level dynamics.** The power chain is a static site. Power and benefit asymmetries enter as parameters of perception, not as generators of temporal flows at the same observer level. The framework captures what different observers see at an instant; it does not model U₃ broadcasting a narrative, U₁ partially internalizing it, and the type labels changing through those interactions.

**Extend to infinite or non-linear sites.** The H¹ gap and spectral structure depend on the specific site geometry.

**Establish causation.** Detects structural patterns, does not explain *why* extraction emerged or *whether* reform would succeed.

**Model lateral extraction.** The power axis is vertical; the victim must be coded as powerless/trapped, misrepresenting the actual geometry. A 2D site (Power × Relational Distance) is the natural extension.

**Distinguish framework properties from LLM priors.** The structural invariants under inversion demonstrate that axiom-derived properties are stable across different LLM-generated corpora. Both corpora inherit whatever latent political grammar their training data shares. Corpus-dependent statistics (type distributions, coalition structure, descent rates) vary between corpora and will shift as the living corpus grows. The invariants that hold are properties of the axioms; the statistics that vary are properties of the data.

### 6.6 What Would Strengthen the Framework

1. **Metric-level sensitivity analysis.** Systematically varying input metrics (ε, suppression, theater ratio, resistance-to-change) within plausible ranges to measure reclassification rates. Parameter sensitivity has been addressed; input metric sensitivity remains the most important missing validation.
2. **Vector-valued sheaf Laplacian.** Replacing scalar χ stalks with MaxEnt distribution vectors (Δ⁷ ⊂ ℝ⁸) to capture both hub contributions.
3. **Corpus diversity.** The corpus is living and grows with each analytical run. D-pattern concentration should be monitored; new constraints generated from analytical use naturally diversify the distribution away from the anchoring artifacts present in the original LLM-generated corpora.
4. **Extension to enriched sites.** Temporal morphisms creating bigraded cohomology H^{p,q}. H^{1,1} would detect constraints whose observer-dependence itself changes over time.
5. **Financial regulation beta corpus.** Testing portability to a domain with quantitative ground truth (e.g., court rulings on extractive contracts where H¹ gaps can be validated against actual legal ambiguity).
6. **Lateral extraction formalization.** A relational dimension representing same-level relationships.
7. **Sensitivity sweep on π(institutional).** Mapping which theorems survive at π = −0.01, π = 0, and π = +0.5.
8. **Intra-level dynamics layer.** An influence matrix over observer positions, with an update rule using the DR gate structure as a hard wall, to model cover story formation and collapse as a dynamical process rather than a static classification.
9. **The sheafification question remains open.** See Axiom 4 for the argument against.
10. **Per-constraint diagnostic walkthrough.** A structured walkthrough of 3–4 constraints — showing the full diagnostic stack, theorem instantiation, omega resolution scenarios, and the contrast between structurally clean constraints (GREEN verdict, gauge-invariant) and coordination-washed constraints (YELLOW verdict, false CI rope signature) — would demonstrate the engine's analytical output more concretely than corpus-level statistics.
11. **Temporal coupling inference.** The `infer_structural_coupling/3` mechanism is implemented but dormant: it produces 0 inferred edges on the current single-snapshot corpus because gradient computation requires 3+ temporal measurements per constraint. Longitudinal measurement data — tracking extractiveness over time — would activate this mechanism and enable detection of constraints whose extraction co-varies, adding a temporal dimension to the coupling graph.

---

## 7. Related Work

**Standpoint epistemology** (Harding 1986; Haraway 1988). DR formalizes the claim that knowledge is perspectival by providing presheaf-theoretic machinery where the "standpoint" is a point of the site. The formalization adds quantitative invariants: the descent rate measures *how much* is perspectival; the H¹ distribution measures the *structure*; the Galois lattice identifies *which* standpoints are decisive. What DR lacks is standpoint epistemology's rich account of how standpoints are constituted.

**Social choice theory** (Arrow 1951; Sen 1970). Arrow's impossibility theorem shows no aggregation rule satisfies a set of desirable axioms; the perceptual non-universality theorem (Theorem 1) shows no observer position universalizes extraction. The formal structures are different (preference lattices vs. presheaves on sites), but the impossibility results are spiritually related.

**Institutional analysis** (Ostrom 1990). Ostrom asks "what kind of institution is this?" from a single analytical perspective; DR asks "what kind does this look like to different observers?" The approaches are complementary: Ostrom provides institutional semantics that DR currently lacks.

**Topos-theoretic approaches in physics** (Isham and Butterfield 1998; Döring and Isham 2008). Both are presheaves on sites of contexts; both formalize context-dependent truth. Key disanalogy: quantum measurement involves irreversibility, stochasticity, and entanglement, none of which are present in DR.

**Contextuality and sheaf cohomology** (Abramsky and Brandenburger 2011). Abramsky and Brandenburger formalize quantum contextuality using sheaf cohomology: a system is contextual iff the obstruction to a global hidden-variable assignment is non-trivial in H¹. DR's refusal of sheafification is structurally parallel — perspectival disagreement is the obstruction, and the contextuality fraction (CF = H¹/6 per constraint) operationalizes this obstruction in DR's discrete setting. The CF gap result — values forbidden at 1/6 and 1/3 by the site geometry — has no direct analogue in the quantum contextuality literature, where the admissible contextuality fractions depend on the specific measurement scenario rather than on a fixed site structure. Key disanalogy: AB contextuality arises from incompatible measurement bases in quantum mechanics; DR contextuality arises from power-modulated perception across structurally distinct observer positions. The mathematics is similar; the source of incompatibility differs.

**Computational social science.** DR is not machine learning. Classification is computed from continuous metrics via a hand-designed deterministic rule cascade, and the central question is not "which label is correct?" but "how does the label depend on who is labeling?"

**Markov categories** (Fritz 2020). The correct abstraction for DR's MaxEnt layer — capturing compositionality without requiring the Giry monad's distribution-over-distributions structure.

**Sheaf Laplacians** (Hansen and Ghrist 2019). Applied to DR's path-graph site, confirms the institutional phase transition as the dominant spectral feature.

---

## 8. Conclusion

Classification of social structures depends irreducibly on who is observing. This paper shows that this dependence has formal mathematical structure and derives specific consequences from a small set of axioms.

The framework's core commitment is a single empirical hypothesis: power modulates the perception of extraction. Encoded as a presheaf on a site of observer positions, this hypothesis produces consequences that are not visible from the informal intuition alone. Extraction cannot be universally perceived as such. Disagreement clusters in discrete blocs. The institutional observer carries 97% of the spectral weight. Single-position analysis is provably almost blind to cross-position structure. These are theorems, not findings.

Two independently generated corpora with inverted input distributions confirm the engine correctly computes these consequences. Structural invariants — eigenvalues, H¹ gap, spectral weight, CF gap, institutional dissent direction — are identical across both corpora and survive FCR ablation, confirming they are fixed-point attractors of the axioms rather than artifacts of any particular dataset. Corpus-dependent statistics (type distributions, descent rates, coalition structure) diverge as expected. Three new diagnostics deepen the empirical record. The contextuality fraction confirms Theorem 2 in Abramsky-Brandenburger coordinates, with the H¹ gap propagating into a provable constraint on admissible CF values. The orbit monotonicity analysis finds that classification along the power chain is almost never a simple gradient: 98.9% of non-constant comparable orbits are non-monotone, with boundary density concentrated in the upper half of the power chain. The incomparable orbit decomposition reveals that the 14.8% of constraints classified outside the extraction ordering are not noise — they are the location of maximum perspectival fracture, with all maximal-obstruction (H¹ = 6) constraints in this population.

A clarification is warranted: the framework does not explain *why* extraction requires perspectival cover — that remains a sociological claim requiring domain theory. What the framework provides is formal machinery to establish *that* perceptual non-universality holds under power-modulated classification, to measure *how much* of a domain is perspectivally fractured, and to identify the specific coalition structure of the fracture. The categorical vocabulary organizes the diagnostic machinery; explanation requires a theory of the domain.

**Broader stakes.** The formal results have implications beyond the social-constraint domain that serves as the test case. For democratic theory: if institutional perception is spectrally decoupled from other positions (Theorem 3), then institutional actors designing reforms are working from a classification that is structurally orthogonal to the experience of those affected — not because they are ignoring the data, but because their position transforms it. For regulatory design: if single-position analysis misses more than 97% of cross-position structure (Theorem 4), then regulatory impact assessments conducted from a single vantage point are provably almost blind to the effects that matter most. For epistemic justice: if extraction structurally requires perceptual non-universality (Theorem 1), then the demand to "prove extraction exists" from the perspective of its beneficiaries is not a neutral epistemic standard but a structural impossibility — the beneficiary's position is precisely where the extraction is invisible. These connections are interpretive rather than formal, but they indicate where the framework's mathematical results make contact with questions that matter.

We close with the open question that the framework itself raises: should the presheaf be sheafified? The answer remains no. The framework's value lies precisely in measuring perspectival fracture — in quantifying the gap between local truth and global truth, and in identifying the structural patterns in that gap. The descent rate, the H¹ distribution, the near-absence of snares from H⁰, the institutional observer as dominant dissenter, the oracle gap, the contextuality fraction — these are features of the presheaf's failure to be a sheaf. Sheafification would erase them. The truth of a social system, on this account, is not the consensus but the fracture itself.

---

## References

Abramsky, S., & Brandenburger, A. (2011). The sheaf-theoretic structure of non-locality and contextuality. *New Journal of Physics*, 13(11).

Amari, S., & Nagaoka, H. (2000). *Methods of Information Geometry*. American Mathematical Society.

Arrow, K. J. (1951). *Social Choice and Individual Values*. Wiley.

Čencov, N. N. (1982). *Statistical Decision Rules and Optimal Inference*. American Mathematical Society.

Döring, A., & Isham, C. J. (2008). "What is a thing?": Topos theory in the foundations of physics. In *New Structures for Physics*, Springer.

Fritz, T. (2020). A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics. *Advances in Mathematics*, 370.

Hansen, J., & Ghrist, R. (2019). Toward a spectral theory of cellular sheaves. *Journal of Applied and Computational Topology*, 3.

Haraway, D. (1988). Situated knowledges. *Feminist Studies*, 14(3).

Harding, S. (1986). *The Science Question in Feminism*. Cornell University Press.

Isham, C. J., & Butterfield, J. (1998). A topos perspective on the Kochen–Specker theorem. *International Journal of Theoretical Physics*, 37(11).

Lawvere, F. W. (1969). Adjointness in foundations. *Dialectica*, 23(3–4).

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic*. Springer.

Noether, E. (1918). Invariante Variationsprobleme. *Nachrichten von der Gesellschaft der Wissenschaften zu Göttingen*.

Ostrom, E. (1990). *Governing the Commons*. Cambridge University Press.

Sen, A. K. (1970). *Collective Choice and Social Welfare*. Holden-Day.

Wheeler, J. A. (1989). Information, physics, quantum. In *Complexity, Entropy, and the Physics of Information*, Addison-Wesley.

Yuen, H. (2023). A quantum complexity-theoretic reduction for the unitary synthesis problem. arXiv:2306.13073.
