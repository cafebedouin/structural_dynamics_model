# Presheaf Classification on a Power-Indexed Site: A Framework for Observer-Dependent Structural Analysis (v5)

**Abstract.** We present a formal framework for analyzing classification systems where the result depends irreducibly on the observer's position. The framework models classification as a presheaf on a site of observer contexts, deliberately violating the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved. Applied to two independently generated corpora of social constraints (887 and 907 constraints, produced by different large language models with opposite metric distributions), the framework yields quantitative invariants: a descent rate of 20–76% (the fraction admitting observer-independent classification), a cohomological obstruction measure H¹ capturing the structure of observer disagreement, and 19–21 structural families of constraints with identical transformation behavior under observer shift. Observer-dependence enters the system through exactly two independent mechanisms — a continuous power-scaled extraction hub and a discrete effective immutability hub — whose independence is verified empirically (zero Type A conflicts) and whose interaction produces the framework's most diagnostic findings. The most philosophically significant finding is that extractive constraints in H⁰ are vanishingly rare (0–1 per corpus) — extraction is almost never observer-independent. A classical MaxEnt oracle detects only 1.7–2.8% of the observer-dependence that cohomological analysis detects at the analytical context, an order-of-magnitude gap that establishes cross-context coherence as irreducible to within-context evaluation. We prove that the Boltzmann factorizability test already running in the classification pipeline is equivalent to the functor axiom for the presheaf's restriction maps, closing the formal verification of the categorical structure. The MaxEnt probabilistic layer forms a Markov category (Fritz 2020) rather than an incomplete Giry monad — the correct abstraction for indexical systems that refuse to posit a prior over observer positions. The framework's strongest empirical result is cross-model structural convergence: two corpora with inverted input distributions — one snare-dominated (51%), the other tangled_rope-dominated (68%) — converge to nearly identical post-override populations (~62% tangled_rope in both), identical sheaf Laplacian eigenvalues [0, 0.0152, 2.9953, 72.1839], identical confidence band distributions (~75%/5%/20% deep/moderate/borderline), and identical superselection structure, establishing these as fixed-point properties of the framework's context geometry rather than any particular corpus. The per-constraint obstruction energy E(C) correlates with H¹ at Spearman ρ = 0.20–0.66, with the correlation strength itself tracking the gauge-variance rate of the corpus. We provide an honest assessment distinguishing strict categorical correspondences from structural analogies.

---



## 1. Introduction

The classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects. This is not a failure of classification but a structural feature of the domain: different observer positions have access to different information, different time horizons, different exit options, and different power to act on what they observe.

The standard response to perspectival dependence is to resolve it — to identify the "correct" classification by privileging one observer position or aggregating across positions. This paper takes the opposite approach. We argue that perspectival dependence has formal mathematical structure, that the structure is best captured by presheaf theory, and that the standard tools of topos theory — cohomology, descent, naturality — produce quantitative invariants that characterize any domain where classification depends on perspective.

The framework, which we call *Deferential Realism* (DR), models observer positions as objects of a small category (a *site*), classification at each position as a *presheaf* on that site, and the degree of observer disagreement as *cohomological invariants* of the presheaf. The framework is *realist* in that it treats constraints as having objective structural properties (extractiveness, suppression, coordination function) that exist independently of any observer; it is *deferential* in that it treats the *classification* of those properties as irreducibly dependent on the observer's structural position, deferring to each perspective's local truth rather than asserting a global one. The presheaf is emphatically not a sheaf: the gluing axiom is intentionally violated because perspectival disagreement is a diagnostic signal, not a defect.

Applied to two independently generated corpora of social constraints — 907 constraints produced by Anthropic's Haiku 4.5 and 887 produced by Google's Gemini Flash 2.0, each with quantitative metrics for extractiveness, suppression, and structural properties — the framework produces six headline findings:

1. **The descent split.** The fraction of constraints admitting a global section — a classification that is the same from every observer position — ranges from 20% to 76% across the two corpora. The existence of a substantial observer-independent subpopulation is robust; its precise magnitude depends on the epsilon distribution of the input corpus. Classification that is independent of who is observing is never universal, but its prevalence varies.

2. **The near-absence of snares from H⁰.** Across both corpora, extractive constraints in H⁰ are vanishingly rare: zero in one corpus, one borderline case in the other. Extraction is almost never observer-independent. There is almost always at least one position from which the extraction is invisible — reclassified as legitimate coordination or immutable constraint. The persistence of extraction requires this cover story.

3. **The superselection gap.** The distribution of observer disagreement is not continuous. No constraint in either corpus has exactly 1 or 2 disagreeing observer-pairs (out of 6 possible pairs). When perspectival dependence emerges, it emerges in blocs of at least 3 disagreeing pairs — a consequence of the site's linear power ordering and the classification cascade's threshold geometry. This gap is invariant across both corpora.

4. **The classical oracle gap.** A Maximum Entropy classifier operating on observer-independent metrics — a "classical oracle" with access to all structural data but no indexing by observer position — detects only 1.7–2.8% of the observer-dependence that cohomological analysis detects at the analytical context. The gap is observer-position-dependent, ranging from near-total detection at the institutional context (where the sigmoid sign-flips) to near-total failure at the analytical context (where smooth observer scaling is absorbed by profile recalibration). The framework measures the gap at its worst case, making the claim conservative: cohomological analysis adds the most diagnostic value precisely where MaxEnt is weakest.

5. **The independence of epistemic restriction and frame-dependence.** Two phenomena that both look like "observer-dependence" — restricted information access (what an observer can see) and structural frame (how an observer processes what they see) — are nearly disjoint, with only 1.6% overlap. The system has two independent kinds of observer-dependent classification error.

6. **Cross-model structural convergence.** The sheaf Laplacian eigenvalues [0, 0.0152, 2.9953, 72.1839] are identical to four decimal places across both corpora, despite the two corpora being generated by different LLMs with different anchoring artifacts (43% and 75% concentration on a single directionality pattern, respectively). More strikingly, the two corpora have *inverted* input distributions — one is snare-dominated (51%), the other tangled_rope-dominated (68%) — yet converge to nearly identical post-override populations (~62% tangled_rope), identical confidence band distributions (~75%/5%/20%), and identical superselection structure. The spectral gap, the institutional dominance signature (r₂₃² ≈ 70, carrying 97% of spectral weight), and the absence of non-mountain stability anomalies are all invariant. These are fixed-point properties of the framework's context geometry, not of any particular corpus.

The framework comprises a classification presheaf evaluated at four standard observer contexts (§2.1–2.3), a naturality test connecting Boltzmann factorizability, Lawvere naturality, and Grothendieck descent — where Boltzmann factorizability is proved equivalent to the functor axiom for the presheaf's restriction maps (§2.4) — a contamination network propagating structural health through agent-sharing graphs (§2.5), a two-hub architecture localizing all observer-dependence to exactly two input channels (§2.7), ~65 binary structural gates encoding pre-metric structural identity (§2.8), and a diagnostic stack comprising gauge orbits (§3.1), MaxEnt shadow classification forming a Markov category (§3.2), abductive synthesis with 15 trigger classes (§3.3), trajectory mining (§3.4), and a 12-subsystem diagnostic integration engine producing traffic-light verdicts (§3.5). Cohomological results including cross-corpus validation appear in §4, an honest assessment in §5, and related work in §6.

This paper does not argue that category theory should replace political philosophy, institutional analysis, or any existing approach to understanding social structures. It argues something more specific: that when classification depends on observer position, presheaf theory provides the correct formal framework, cohomology measures the structure of disagreement, and the resulting invariants characterize the domain in ways that informal analysis cannot. The framework is general — it applies wherever classification is perspectival — but the evidence is drawn from the specific domain of social constraints, where the phenomena are vivid and the stakes are clear.

Concretely, given any domain with perspectival classifications, the framework computes: (a) a *descent rate* measuring what fraction of the domain admits observer-independent truth, (b) a *cohomological fracture profile* (the H¹ distribution) characterizing the structure of disagreement — not just that observers disagree but how many disagree and in what pattern, (c) *structural families* of objects with identical transformation behavior under observer shift, including cross-domain twins from unrelated domains that turn out to be structurally isomorphic, and (d) a *coalition lattice* revealing which groups of observers form natural consensus blocs and which observer is the structurally decisive dissenter. These are portable invariants: the formal machinery applies wherever classification depends on who is classifying. The two-corpus design establishes which of these invariants are topological properties of the framework (stable across corpora) and which are statistical properties of the input (corpus-dependent). The most striking validation is convergence under inversion: two corpora with opposite input distributions produce nearly identical framework-level outputs, establishing the framework's structural properties as fixed-point attractors rather than artifacts of any particular input.
## 2. The Framework

This section defines the mathematical framework. A reader unfamiliar with the codebase should be able to reconstruct the formal structure from this section alone.

### 2.1 The Site: Observer Positions

The fundamental modeling choice is the parametrization of observer positions. Each observer is characterized by a 4-tuple:

$$\text{context}(\text{Power}, \text{Time}, \text{Exit}, \text{Scope})$$

where **Power** ranges over a partially ordered set of power levels, **Time** over temporal horizons (biographical, generational, civilizational), **Exit** over exit options (trapped, mobile, arbitrage, analytical), and **Scope** over scales of observation (local through global).

For computational tractability, we fix four **standard contexts** that span the relevant range of observer positions:

| Context | Power | Time | Exit | Scope |
|---------|-------|------|------|-------|
| U₁ (powerless) | powerless | biographical | trapped | local |
| U₂ (moderate) | moderate | biographical | mobile | national |
| U₃ (institutional) | institutional | generational | arbitrage | national |
| U₄ (analytical) | analytical | civilizational | analytical | global |

These four contexts are the objects of a finite poset category **C**, with morphisms determined by the power ordering: U₁ → U₂ → U₃ → U₄. This gives **C** the structure of a linear 4-element poset — a chain. It is the simplest non-trivial site that captures the phenomenon of interest [STRICT].

The choice of four contexts is deliberate. Fewer would lack the resolution to detect perspectival transitions; more would complicate the analysis without adding qualitative insight. The four chosen contexts represent structurally distinct positions: the subject of a constraint (U₁), an observer with moderate agency (U₂), an institutional actor who may benefit from or reform the constraint (U₃), and a detached analyst evaluating structural properties (U₄). The power ordering captures the intuition that increased power generally provides access to more information about a constraint's structure.

The choice of site is normative. Different political theories would induce different site factorizations: a Marxist analysis might separate epistemic access from material power as independent morphism dimensions; a liberal framework might make Scope a proper morphism generator rather than a parameter; a feminist standpoint epistemology might add an embodiment axis absent from the current parametrization. The framework is a functor from site choices to invariants — disagreements about what counts as a "standpoint" translate into disagreements about which invariants the framework computes. The current 4-element linear poset is one instantiation in a family of possible sites, chosen as the simplest non-trivial member. Several empirical results depend on its linearity: the H¹ gap at 1 and 2 (§4.2) is a consequence of linear ordering, not a universal property of perspectival dependence; the eigenvalue spectrum and superselection structure (§4.5) are determined by this site's restriction ratios. The invariants the framework computes are geometry-relative — properties of this site's measurement apparatus — not world-relative assertions about the constraints themselves. Different site choices would produce different invariants, and the framework provides *a* correct formalization of observer-dependent classification, not *the* correct abstraction. The site is where political commitments enter the mathematics, and the framework makes that entry point explicit rather than hiding it behind a claim of objectivity.

The two-hub architecture (§2.7) reveals that observer-dependence enters the system through exactly two channels, both localized in the context tuple: Hub 1 reads Power to derive a directionality value, Hub 2 reads (Time, Exit) to derive an immutability perception. Everything else in the classification pipeline is observer-independent data or deterministic transformation.

### 2.2 The Presheaf: Classification as Local Truth

The core construction is the classification presheaf. For each constraint *C* in the corpus, we define a contravariant functor:

$$F_C : \mathbf{C}^{op} \to \mathbf{Set}$$

by setting $F_C(U_i) = \text{dr\_type}(C, U_i)$, where dr_type computes the classification of constraint *C* as observed from context $U_i$. The result is an element of the eight-element type space $\Omega$ (defined in §2.3).

This is a presheaf by construction: for each object of the site, it assigns a set (here, a single element of $\Omega$); for each morphism $U_i \to U_j$ in the site, there is an implicit restriction map $F_C(U_j) \to F_C(U_i)$ determined by the classification cascade's response to the change in observer context [STRICT].

The presheaf is **not** a sheaf. The gluing axiom requires that if local sections agree on overlaps, they extend to a global section. The entire diagnostic infrastructure of the framework exists precisely because local sections *fail to glue*: a constraint may classify as "mountain" (immutable natural law) from U₄ and "rope" (legitimate coordination mechanism) from U₃, with no consistent global classification available. This **perspectival gap** is the framework's central diagnostic signal, not a defect to be resolved [STRICT].

**The restriction map.** The restriction map has two independent channels, corresponding to the two hubs of the classification architecture (§2.7). The mechanism by which classification changes across observer positions involves both a continuous power-scaling function and a discrete immutability lookup:

**Channel 1 (Hub 1 — continuous).** Base extractiveness $\varepsilon$ is a context-independent property of the constraint — this is a design axiom (ε-invariance): the same constraint has the same base extractiveness regardless of who is observing it, though the *experienced* extractiveness varies by observer. The context tuple is closed at arity 4: given fixed ε and a fixed (Power, TimeHorizon, ExitOptions, Scope) context, χ is fully determined. Informally: how much extraction you experience depends on how powerful you are. A powerless observer trapped in a constraint feels the full force of its extractiveness; an institutional observer with exit options and long time horizons experiences the same constraint as substantially less extractive — possibly reclassifying it from "snare" to "rope." The experienced extractiveness at a given context is:

$$\chi = \varepsilon \times \sigma(\pi(P)) \times \sigma(S)$$

where $\pi(P)$ is a power index associated with the observer's power level, $\sigma$ denotes a sigmoid scaling, and $S$ contributes a scope modifier. Higher power generally reduces experienced extractiveness (the sigmoid is calibrated so that institutional and analytical observers perceive less extraction), which means the classification presheaf is sensitive to the observer's structural position. The restriction map — the effect of following a morphism $U_i \to U_j$ in the site — is determined by this scaling: moving to a higher-power context generally decreases experienced extractiveness, potentially reclassifying a "snare" as a "rope." The multiplicative factorization of χ has formal significance for the presheaf's categorical structure, proved in §2.4.

**Channel 2 (Hub 2 — discrete).** Independently, the (TimeHorizon, ExitOptions) pair in the observer's context determines a perceived mutability level via a lookup table of 18 hard-coded mappings. This perception governs whether the mountain gate and rope gate are available in the classification cascade.

### 2.3 The Type Space

The codomain of the classification presheaf is the eight-element type space:

$$\Omega = \{\text{mountain}, \text{rope}, \text{tangled\_rope}, \text{snare}, \text{scaffold}, \text{piton}, \text{naturalized}, \text{unknown}\}$$

Each type has a structural interpretation:

- **Mountain**: immutable natural constraint (thermodynamic laws, impossibility theorems). Observer-independent by design.
- **Rope**: legitimate coordination mechanism. Changes the constraint landscape but serves coordination.
- **Tangled rope**: coordination mechanism with embedded extraction. The entanglement of legitimate and extractive functions is irreducible.
- **Snare**: extractive trap. The constraint exists primarily to extract from those subject to it.
- **Scaffold**: temporary coordination mechanism with an explicit sunset clause.
- **Piton**: performative constraint with high theater ratio (more display than substance).
- **Naturalized**: contradictory metrics — the constraint cannot be classified from this position.
- **Unknown**: residual category when no classification threshold fires.

The type space carries a binary composition operation defined by priority rules. Importantly, this composition has **two absorbing elements**: both "mountain" and "piton" absorb all other types under composition. A Heyting algebra — the subobject classifier of a topos — requires a unique top element. The presence of two absorbing elements means the type space is a **priority monoid**, not a Heyting algebra (see §5.3). What is present is an associative, idempotent composition with a fallback identity ("unknown") and priority-based conflict resolution — not the implication and complementation structure that a Heyting algebra would provide.

### 2.4 Naturality: The Boltzmann Factorizability Test and the Functor Axiom

The classification presheaf assigns types; the naturality test asks whether those assignments are *well-behaved*. Specifically: does the classification factorize across the independent index dimensions of Power and Scope?

The **Boltzmann factorizability test** evaluates each constraint across a Power × Scope grid and measures the degree to which:

$$\chi(P, S) \approx f(P) \times g(S)$$

If the classification factorizes — if changing Power has the same effect regardless of Scope, and vice versa — then the classification is **natural** in the categorical sense. The factorization condition is exactly the commutativity of the naturality square:

$$
\begin{array}{ccc}
(P_1, S) & \xrightarrow{P\text{-shift}} & (P_2, S) \\
\downarrow & & \downarrow \\
\text{Type}_1 & \xrightarrow{\text{should =}} & \text{Type}_2
\end{array}
$$

This is genuine naturality by construction. It is the formal spine of the framework, and it connects to two other formal conditions (STRICT):

**The three-way equivalence.** The same computational test — invariance of classification under observer shifts — has three mathematical names, each arising from a different theoretical tradition:

| Condition | Lawvere name | Grothendieck name | Noether name |
|-----------|-------------|-------------------|--------------|
| Classification constant across contexts | Naturality of the presheaf | Descent (H¹ = 0) | Symmetry conservation |
| Classification factorizes across P × S | Naturality square commutes | Product-site factorizability | Separability of the Lagrangian |
| Failure detected | Naturality failure witness | Descent obstruction | Broken symmetry |

The Lawvere ↔ Grothendieck equivalence (naturality ↔ descent on this site) is STRICT — these are genuinely the same mathematical condition. The Noether column uses "conservation" as shorthand for invariance under a discrete group action, which is the precondition of Noether's theorem rather than the theorem itself. The overall equivalence is therefore STRUCTURAL (two-out-of-three: Lawvere ↔ Grothendieck is STRICT; Noether is a productive structural parallel). Every other diagnostic layer in the framework — MaxEnt entropy, abductive analysis, trajectory mining, drift detection, cohomological computation, diagnostic integration — is a different way of measuring proximity to or deviation from this invariance condition.

**The functor axiom equivalence.** The multiplicative factorization of experienced extractiveness has a categorical consequence that was identified post-v3. Hub 1's formula χ(P) = ε · [f(d(P)) · σ(S(P))] separates into observer-independent (ε) and observer-dependent (f(d(P)) · σ(S(P))) factors. The restriction map from context Pⱼ to context Pᵢ acts by the ratio of observer-dependent parts:

$$\rho(P_j \to P_i): \chi \mapsto \chi \cdot \frac{\sigma(\pi(P_i))}{\sigma(\pi(P_j))}$$

The composition check is immediate:

$$\rho(P_2 \to P_3) \circ \rho(P_1 \to P_2)(\chi_1) = \chi_1 \cdot \frac{\sigma(\pi(P_2))}{\sigma(\pi(P_1))} \cdot \frac{\sigma(\pi(P_3))}{\sigma(\pi(P_2))} = \chi_1 \cdot \frac{\sigma(\pi(P_3))}{\sigma(\pi(P_1))} = \rho(P_1 \to P_3)(\chi_1) \quad \checkmark$$

The σ(π(P₂)) terms cancel telescopically. This works because χ factors *multiplicatively* as (observer-independent) × (observer-dependent), and restriction maps are determined by the *ratio* of observer-dependent parts. Ratios compose telescopically — (a/b)(b/c) = a/c — regardless of whether a, b, c are linear, sigmoidal, or anything else. The multiplicative version guarantees χ ≥ 0 by construction (sigmoid outputs are positive and ε ≥ 0).

This yields the central formal result: **Boltzmann factorizability — the test that χ separates into observer-independent and observer-dependent factors — is exactly the functor axiom for Hub 1's restriction maps.** The Boltzmann compliance test already running in the classification pipeline is performing formal verification of the functor axiom, without the categorical label [STRICT].

Specifically: Boltzmann-compliant constraints have multiplicatively separable χ, so their restriction maps compose, F is a well-defined functor, and the presheaf structure is strict. Boltzmann-non-compliant constraints have coupled dimensions, restriction maps may not compose, and the presheaf structure is approximate.

The scope modifier σ(S) does not introduce coupling. The chi variance decomposition confirms that σ(S) is assigned per-perspective as a structural constant (powerless → local = 0.8, analytical → global = 1.2), independent of constraint content. So σ(S) = σ(S(P)), which folds into the observer-dependent factor: χ(P) = ε · [f(d(P)) · σ(S(P))]. No coupling. The factorization holds.

The population of chi-override constraints — where χ_actual ≠ ε · f(d) · σ(S) — constitutes the documented boundary where the functor axiom fails and the presheaf structure is approximate. Across both corpora, chi overrides range from 0% (Gemini Flash, where the v1.2 generation prompt eliminated the arithmetic that produced overrides) to 12.3% of evaluations (Haiku, all at the powerless context, where the sigmoid's generic power-scaling fails to capture specific vulnerability). The restriction maps do not compose cleanly for these constraints. This is a well-characterized exception population mapping to the same category as Boltzmann-non-compliant constraints.

Hub 2's 18-row lookup table with discrete outputs satisfies the functor axioms trivially: singleton stalks admit unique maps between them, and composition of unique maps is unique. The interesting property of Hub 2 is independence from Hub 1, already verified empirically (zero Type A conflicts across both corpora).

**The comparison with additive factorization.** The Avellaneda-Stoikov reservation price formula r(q) = s − qκ provides a comparison case: there, the factorization is *additive* (ℝ acting on valuations by translation) rather than multiplicative (ℝ₊ acting on extractiveness by scaling). The shift operators compose by additive telescoping: Δ(q₁→q₂) + Δ(q₂→q₃) = Δ(q₁→q₃). Both pass the functor axiom, but the multiplicative version has the additional property that χ ≥ 0 is guaranteed by construction, whereas the additive version allows negative valuations. The DR sigmoid and the A-S linear formula are two instances of the same abstract pattern: factorizability of the stalk assignment guarantees compositionality of the restriction maps.

**Naturality certificates and failure witnesses.** Three derived signatures operationalize the naturality test:

- **Coupling-Invariant Rope (CI Rope)**: passes all four naturality conditions (Boltzmann compliance, scope invariance, absence of nonsensical coupling, no excess extraction). This is a naturality certificate — formal evidence that the classification is well-behaved.
- **False Natural Law (FNL)**: the constraint presents as a natural law (mountain-like metrics) but fails the Boltzmann factorizability test. Cross-index coupling is detected where none should exist. The constraint is "physics-washed" — constructed but disguised as natural. This is a naturality failure witness.
- **False CI Rope (FCR)**: the constraint appears to be a structurally sound rope but fails one or more structural tests. The apparent coordination is itself illusory — a doubly-broken classification. The FCR gate includes a deferral mechanism: when metric perspectival variance is detected, the override is deferred, allowing the raw metric-based type to stand. This prevents FCR from forcing a reclassification when the constraint genuinely exhibits perspectival variation that the override would suppress.

All three classifications are STRICT: they test well-defined conditions on the naturality square.

An empirical covering analysis independently confirms the scope modifier's limited effect. The institutional scope cells (in/loc, in/nat, in/glo) produce 100% identical classifications — all three are observationally redundant. The minimum discriminating subset of the Power × Scope grid requires only 8 of the 12 cells, confirming that the scope modifier σ(S) does not introduce coupling at the institutional context. This result is consistent across both corpora (Flash and Haiku) and independently validates the χ variance decomposition finding that f(d) accounts for 94.8% of classification variance, with scope contributing the remaining 5.2%.

**Open item 1 from v2/v3 — CLOSED.** The formal verification of restriction map composition, listed as an open item in previous versions, is resolved by the argument above. The chain is: Boltzmann compliance test → multiplicative separability → telescopic composition of restriction maps → functor axioms satisfied. The chi variance decomposition provides the σ(S) independence evidence. The chi-override population (0–12.3% across corpora) constitutes the documented exceptions.
### 2.5 Purity and Contamination

Constraints do not exist in isolation. They share agents — individuals or institutions that enforce, benefit from, or are subject to them — creating a network of influence. The **purity score** is a composite measure of a constraint's naturality health, combining four subscores: factorizability (Power × Scope independence), scope invariance, coupling cleanliness, and excess extraction above a thermodynamic floor. The score ranges from 0.0 (complete naturality failure) to 1.0 (perfect naturality).

The **contamination network** propagates purity through the agent-sharing graph. A high-purity constraint (structurally clean coordination) that shares an agent with a low-purity constraint (contaminated extraction) has its effective purity reduced. Crucially, contamination flows *against* the purity gradient — from lower-purity to higher-purity constraints — giving the propagation a contravariant character. Mountains are immune to contamination (immunity = 0.0); ropes are fully susceptible (STRUCTURAL).

The **Fixed-Point Network (FPN)** computes the equilibrium distribution of effective purities by iterating the contamination operator until convergence. The operator is monotone (contamination is non-decreasing) and bounded below (purity is floored at 0.0), so by the Knaster-Tarski theorem, iteration from intrinsic purities converges to a greatest fixed point. In coalgebraic terms, this equilibrium is the terminal coalgebra of the contamination endofunctor on the purity lattice — a suggestive coalgebraic reading, though the operator itself is just an order-theoretic monotone map on a finite lattice (STRUCTURAL; see §5.2). The FPN's output feeds into the diagnostic integration system (§3.5) and serves as an input to abductive triggers T6 (accelerating pathology) and T7 (contamination cascade).

### 2.6 Corpus Provenance and Two-Corpus Design

The empirical results reported in this paper are drawn from two independently generated corpora of social constraints — laws, norms, institutions, regulatory mechanisms — evaluated by the same Prolog analytical pipeline. The two-corpus design is methodological: it distinguishes framework-stable findings (topological invariants that hold regardless of input distribution) from corpus-dependent findings (statistical properties sensitive to the input's metric distribution).

**Corpus A (Haiku 4.5).** 907 constraints generated by Anthropic's Claude Haiku 4.5 using generation prompt v1.1, rebuilt February 2026 from the original seed corpus. The generation process follows a specific workflow: the LLM generates "constraint stories" — structured JSON files modeling power dynamics, extractiveness metrics, and structural relationships — which the Prolog infrastructure evaluates via independent analytical tools. An analyst receives both the story and the diagnostic report, then writes a "diff-essay" analyzing the gap between what the LLM assumed and what the structural analysis reveals. Constraint stories being wrong in characteristic LLM ways is a feature: the diagnostic infrastructure surfaces *where and why* stories fail.

**Corpus B (Gemini Flash 2.0).** 887 constraints generated by Google's Gemini Flash 2.0 using generation prompt v1.2, which stripped arithmetic worked examples from the exemplar to reduce chi-override artifacts. Gemini Flash generates variable perspective counts (mode 5, range 2–6) and uses additional agent power values beyond the standard 4-context grid. The pipeline still computes χ for the standard 4 contexts regardless, so sheaf cohomology operates on the same 4-node graph.

**D-pattern anchoring.** Both corpora exhibit a known generation artifact: concentration of directionality patterns on the exemplar's structural position template. In Corpus A, 43% of constraints share the d-pattern (0.9, 0.7, 0.12, 0.72); in Corpus B, 74.6% share the same pattern. The anchoring is more severe in Corpus B despite the v1.2 prompt fix, because the anchoring derives from the exemplar's *structural position assignments* (powerless/trapped, institutional/arbitrage, etc.), not the chi arithmetic that v1.2 removed. This artifact creates pathological input correlation — a stress test of framework robustness rather than a flaw in the empirical design. The invariance of topological findings under such severe anchoring is itself a result (§4.5).

**Calibration.** The continuous metrics assigned to each constraint (extractiveness ε, suppression σ, theater ratio θ, resistance-to-change ρ) are specified in the constraint stories and validated against the analytical pipeline. They have not been subjected to inter-rater reliability testing. A hostile reader could reasonably ask: how much of the output is a property of the formal framework, and how much is an artifact of the calibration?

The answer has two parts. First, the formal framework — presheaf structure, naturality testing, cohomological computation, Galois connection — is independent of any particular corpus and its metric assignments. A different corpus with different metrics would produce different empirical numbers (a different descent rate, a different H¹ distribution, different structural families) but the same formal machinery would apply. The framework's contribution is the diagnostic vocabulary, not the specific invariants computed for any one corpus.

Second, a configuration sensitivity sweep provides direct evidence of calibration robustness. At 1,023 constraints, all 118 numeric parameters governing classification thresholds, power modifiers, and coupling gates were perturbed at ±10% and ±25%, with each of the 472 resulting configurations running the full validation suite. Of the 118 parameters, 103 (87%) were inert at ±25% perturbation — the classification output did not change. Eight parameters were moderate: stable at ±10% but breaking at ±25%, including the Boltzmann coupling threshold (current value 0.25, floor approximately 0.19–0.22). Of the seven parameters initially flagged as critical, six were reclassified upon investigation as timeout artifacts or integer-rounding effects, leaving one genuinely critical parameter: the analytical power modifier, a structural constant that scales every experienced-extractiveness calculation. The system's stability is a consequence of design rather than luck: the critical parameter is load-bearing by construction, not by calibration fragility. The parameter count has grown to approximately 170 with the addition of abductive engine thresholds, diagnostic integration parameters, and trigger configurations; a rerun of the sensitivity sweep against the expanded parameter set is open validation work.

**Type distributions.** Type distribution depends on whether structural signatures are integrated. Table 1 reports both the metric-only classification (from the deterministic cascade before signature resolution) and the post-override classification (after signature integration), for each corpus.

**Table 1: Dual Type Distribution**

| Type | Flash metric | Flash post | Haiku metric | Haiku post |
|------|-------------|-----------|-------------|-----------|
| snare | 448 (50.5%) | 109 (12.3%) | 152 (16.8%) | 175 (19.3%) |
| tangled_rope | 287 (32.4%) | 549 (62.0%) | 619 (68.2%) | 567 (62.5%) |
| mountain | 139 (15.7%) | 139 (15.7%) | 129 (14.2%) | 129 (14.2%) |
| rope | 10 (1.1%) | 62 (7.0%) | 7 (0.8%) | 18 (2.0%) |
| scaffold | 2 (0.2%) | 10 (1.1%) | 0 (0.0%) | 9 (1.0%) |
| piton | 0 (0.0%) | 17 (1.9%) | 0 (0.0%) | 9 (1.0%) |

*Flash: Gemini Flash 2.0, N=887. Haiku: Claude Haiku 4.5, N=907.*

The two corpora have opposite metric-only distributions: Flash is snare-dominated (50.5%), Haiku is tangled_rope-dominated (68.2%). Yet after signature integration, both converge to approximately 62% tangled_rope. The signature system — principally the false_ci_rope override, which applies a 3× boost to tangled_rope probability when the Boltzmann independence test detects cross-perspectival coupling alongside extraction — acts as a fixed-point attractor: different input distributions converge to similar post-override outputs. This convergence reframes the signature system from a correction mechanism to an architectural constraint of the framework. Mountains are invariant under signature integration (Δ = 0 in both corpora), as expected: natural law constraints receive the natural_law signature, which reinforces rather than overrides their metric classification. The mountain population is stable across both corpora (~14–16%), confirming that natural law identification is robust to generator differences.

**Index sufficiency.** Testing across all index configurations on the Flash corpus, only 3 genuine classification collisions occur out of 887 constraints (0.3%), with 19 additional collisions representing expected perspectival variance. The 4-axis context tuple captures classification-relevant variation without gaps: the non-mountain anomaly rate is 0.0%.

**Test suite.** The validation suite comprises approximately 1,150 test cases covering classification logic, naturality testing, cohomological computation, abductive trigger firing, and diagnostic integration.

**Codebase scope.** The Prolog implementation comprises 77 modules covering classification (drl_core, constraint_indexing), diagnostics (maxent_classifier, boltzmann_compliance, dirac_classification, grothendieck_cohomology, purity_scoring, logical_fingerprint, drl_lifecycle), synthesis (abductive_engine, abductive_triggers, diagnostic_summary), reporting (json_report, report_generator, enhanced output), and configuration (config, config_validation, validation_suite). Additional modules support specialized analyses not detailed in this paper: psychological pattern classification (psych_bridge), counterfactual analysis (drl_counterfactual), intent-interval classification (intent_engine), network phase-transition analysis (giant_component_analysis), invertibility analysis, covering analysis, coercion projection, and scenario management for corpus loading. These peripheral modules extend the framework's analytical reach but are not part of the core presheaf-diagnostic architecture documented here.

The empirical findings reported in §4 present framework-stable invariants (those identical across both corpora) separately from corpus-dependent metrics (those presented as ranges).

### 2.7 Two-Hub Architecture

Every classification in DR flows through `classify_from_metrics/6`. But the observer-dependent inputs to that function come from two independent subsystems that share no internal logic. This section documents why they are independent, why they should not be unified, and what their independence reveals about where indexicality lives in the system.

**Hub 1: Power-Scaled Extraction (the sigmoid).** The directionality derivation chain — `derive_directionality/3` → `sigmoid_f/2` → `extractiveness_for_agent/3` — maps observer position to an experienced extraction value χ. This is a continuous transformation: small changes in power position produce small changes in χ. The mechanism is structural. An observer's relationship to the constraint (beneficiary, victim, neither) determines a directionality value *d*, and the sigmoid amplifies or suppresses base extraction accordingly. All threshold-based classification differences (snare vs. tangled_rope vs. rope) are downstream of this hub [STRICT].

The multiplicative factorization χ(P) = ε · [f(d(P)) · σ(S(P))] is exactly the condition that guarantees the restriction maps compose: the ratio σ(π(Pᵢ))/σ(π(Pⱼ)) cancels telescopically under composition, satisfying the functor axiom. Boltzmann factorizability — already tested by the Boltzmann compliance module — is therefore equivalent to the presheaf's functor axiom for Hub 1 (§2.4) [STRICT]. The chi-override population — where χ_actual ≠ ε · f(d) · σ(S) — ranges from 0% to 12.3% of evaluations across the two corpora and constitutes the exact boundary where the functor axiom fails and the presheaf structure is approximate. In Corpus A, all 473 violations occur at the powerless context, where the sigmoid's generic power-scaling fails to capture specific vulnerability. In Corpus B, the v1.2 generation prompt eliminated the arithmetic that produced overrides, yielding zero violations.

**Hub 2: Effective Immutability (the lookup table).** The `effective_immutability_for_context/2` table maps 18 (TimeHorizon, ExitOptions) pairs to a perceived mutability level. This is a discrete function: 18 hard-coded facts that produce categorical outputs (mountain, rope, or neither). No sigmoid, no continuous transformation. The mechanism is perceptual. An observer with biographical time horizon and no exit options perceives a constraint as immutable; an observer with generational time horizon and systemic exit perceives the same constraint as mutable. The mountain gate and the rope gate in `classify_from_metrics/6` both check this table independently of χ [STRICT]. Hub 2's lookup table satisfies the functor axioms trivially: singleton stalks admit unique maps, and composition of unique maps is unique.

**Why the hubs should not be unified.** Three reasons prevent collapsing Hub 2 into Hub 1:

1. *They measure different things.* Hub 1 measures how power position transforms the *experience* of extraction — a structural-economic relationship. Hub 2 measures how temporal horizon and escape options transform the *perception* of mutability — a structural-epistemic relationship. An observer can experience high extraction (Hub 1 says snare) while perceiving the constraint as mutable (Hub 2 says not-mountain). This combination is precisely the tangled rope — the most common constraint type in both corpora.

2. *They have different mathematical characters.* Hub 1 is continuous, differentiable, and admits sensitivity analysis. Hub 2 is discrete and categorical — there is no meaningful "halfway between mountain and rope" immutability perception. Forcing a continuous parameterization onto an inherently discrete judgment would introduce false precision.

3. *They interact productively at specific points.* The mountain gate requires BOTH low χ (Hub 1) AND effective_immutability = mountain (Hub 2). False mountains — actively enforced extraction that an observer perceives as natural law — are only detectable *because* the two hubs are independent. A unified hub would blend the signals and lose the diagnostic.

**Indexicality localization.** Indexicality enters the system through exactly two input channels:

- **Hub 1 channel:** `derive_directionality/3` produces *d* from the observer's structural relationship to the constraint. This is the primary indexical input.
- **Hub 2 channel:** The (TimeHorizon, ExitOptions) pair determines which row of the immutability table applies. This is the secondary indexical input.

Everything else — the sigmoid, the thresholds, the structural signatures, the MaxEnt likelihoods, the ~65 binary structural gates (§2.8) — is either observer-independent data or deterministic transformation of indexed inputs. The system's observer-dependence is fully localized to these two input channels [STRICT].

**Hub independence: empirical verification.** Zero Type A conflicts exist in either corpus — no constraint is simultaneously classified as mountain by the immutability table and at snare-level extraction by the sigmoid. The mountain gate's BaseEps check prevents this by requiring low base extraction as a precondition for mountain classification. Hub-conflict constraints — where the immutability table flips between mountain and non-mountain across contexts — cluster at exactly H¹ = 4 (§4.2), forming a distinct cohomological band that corresponds to Hub 2's discrete transition. This means the cohomological formalism has decomposed observer-dependence into bands corresponding to specific architectural mechanisms: H¹ = 3 for Hub 1-driven divergence, H¹ = 4 for Hub 2-driven divergence [STRICT for hub independence and zero Type A conflicts; STRUCTURAL for H¹ band → hub correspondence].

### 2.8 Binary Structural Gates

Binary structural gates are the hard classification boundaries that operate upstream of the continuous metrics. They determine what *kind* of thing a constraint is before `classify_from_metrics/6` applies threshold logic to the continuous values. A constraint that fails `emerges_naturally/1` will never classify as mountain regardless of how low its extraction is. A constraint that passes `has_coordination_function/1` opens the scaffold gate regardless of its suppression score.

These gates matter because they are the observable residue of intent without requiring intent as an input. The framework does not ask *why* a constraint was created; it asks whether the constraint has structural properties — beneficiaries, victims, enforcement mechanisms, expiration dates — that correlate with different constraint architectures.

**Inventory.** The framework contains approximately 65 binary gates organized across five structural dimensions:

| Dimension | Gates | What it discriminates |
|-----------|-------|-----------------------|
| Origin (7) | `emerges_naturally`, `is_constructed`, `natural_law_without_beneficiary`, `natural_law_signature`, `false_natural_law`, `effective_immutability`, `has_viable_alternatives` | Natural law vs. constructed coordination vs. designed extraction |
| Mechanism (7) | `requires_active_enforcement`, `has_coordination_function`, `has_asymmetric_extraction`, `scaffold_temporality_check`, `coercion_without_coordination`, `undocumented_coordination`, TR ≥ 0.70 | Enforcement-dependent vs. self-sustaining, coordinating vs. extracting, genuine vs. theatrical |
| Lifecycle (9) | `has_sunset_clause`, `has_temporal_data`, `drifting_without_limit`, plus six `detect_*` drift predicates | Static vs. drifting vs. expiring |
| Topology (6) | `has_beneficiaries`, `has_victims`, `fingerprint_actors`, `nonsensically_coupled`, `coupling_invariant_rope`, `false_ci_rope` | Relational structure of the constraint's actor and coupling network |
| Diagnostic (8) | `boltzmann_compliant`, `epistemic_access_check`, `snare_immutability_check`, `extractive_immutable`, `self_sustaining_extraction`, `unenforced_suppression`, `unaccountable_extraction`, `no_exit_for_victims` | Internal consistency of structural properties |

**The binary fingerprint.** Each constraint's binary gate profile yields approximately 12 independent bits of structural information, encoding structural identity prior to any continuous metric computation [STRICT].

**Two-tier architecture.** The binary gates form a two-tier system:

- **Tier 1: Binary Screening.** Five base boolean gates plus threshold gates. Fast, deterministic, sufficient for ~80% of structural questions. These gates test structural properties of the constraint itself: Does it emerge naturally? Is it enforced? Does it coordinate? Does it extract asymmetrically? Does it expire?

- **Tier 2: Cross-Index Factorization.** Boltzmann compliance, false natural law detection, false CI-rope detection, coupling-invariant rope verification. These gates require cross-index analysis — the constraint must be classified across multiple Power × Scope grid positions. They test whether structural properties *factorize* correctly across independent observer dimensions. Genuine natural law classifications are Boltzmann-invariant; constructed mimicry introduces coupling between dimensions that should be independent.

The two-tier architecture is STRUCTURAL — the boundary between structural-property tests and factorization tests is a design decision, not a derived consequence.

**Independence analysis.** NMI (Normalized Mutual Information) analysis measures how much information each binary gate carries beyond what the type classification already captures. `requires_active_enforcement` carries the most independent information (NMI = 0.663), meaning it is the gate least predictable from knowing only the type. This makes structural sense: tangled_rope *requires* active enforcement by boolean specification, but enforcement status within other types varies [STRICT for NMI computation].

**FCA gate compression.** Formal Concept Analysis on the extractable gate matrix (32–33 gates across the two corpora) yields GF(2) rank 30 — only 2–3 gates are linearly redundant. The concept lattice contains 1,865–2,202 formal concepts (far fewer than 2³³ but larger than expected for aggressive compression). The most structurally significant finding is that snare vs. tangled_rope has *zero* perfect gate separators in either corpus. No single gate or gate combination in the extracted set cleanly distinguishes snares from tangled ropes. This confirms the two-hub architecture from a new angle: the snare/tangled_rope distinction is driven by continuous χ thresholds (Hub 1), not binary structural features. The gate space captures Hub 2's contribution (mountain separation: 7 perfect separators) but is structurally blind to Hub 1's continuous gradation. Only 33 of ~65 gates are extractable from the JSON export; the missing ~32 are primarily Hub 2 features computed inside Prolog modules. Full FCA requires Prolog-side extraction.

**Lateral extraction.** The binary gates produce correct structural fingerprints for lateral extraction (peer manipulation, workplace bullying, communal narcissism): `requires_active_enforcement`, `has_asymmetric_extraction`, and `has_coordination_function` all fire correctly. However, the context representation must be distorted to produce correct classifications — the victim is coded as powerless/trapped and the extractor as institutional, misrepresenting the actual structural geometry. The gates are not broken; the power axis is inadequate. See §5.4 for the full limitation.

---

## 3. The Diagnostic Layers

The framework is not a single classifier but a stack of diagnostic layers, each providing a different view of the same underlying presheaf structure. The classification presheaf provides the foundation; the diagnostic layers measure its properties.

### 3.1 Gauge Orbits (Dirac Classification)

The **gauge orbit** of a constraint is the complete set of (type, context) pairs across all standard contexts — the full evaluation of the presheaf at every object of the site. Two constraints with identical orbits exhibit the same perspectival profile; they are, from the presheaf-theoretic viewpoint, sections of the same functor.

The orbit decomposition classifies constraints by the structure of their perspectival variation:

- **Singleton orbits** (one type across all contexts): gauge-invariant constraints. Classification is observer-independent. These are the global sections, the constraints satisfying descent.
- **Multi-type orbits**: constraints with perspectival dependence. The Dirac classification module further distinguishes first-class constraints (where the type change represents a genuine shift in structural character as power changes) from second-class constraints (where the type change reflects a classification artifact) [STRICT — the orbit computation under a finite group is standard].

The orbit is the constraint's identity in the presheaf topos. This implements Grothendieck's relative viewpoint: a constraint IS its behavior under observer shifts, not its classification at any single context.

The two corpora yield 19–21 orbit families from which structural families are derived (§3.4). The dominant orbit in both corpora is gauge-invariant tangled_rope (529 constraints in Corpus A, with similar dominance in Corpus B), reflecting the prevalence of constraints whose tangled character is visible from every observer position.

### 3.2 Probabilistic Shadow (MaxEnt Classification)

The deterministic classifier assigns a single type at each context. The Maximum Entropy (MaxEnt) shadow classifier runs alongside it and assigns a *probability distribution* over all types. Where the deterministic classifier evaluates the presheaf, the MaxEnt classifier evaluates a probabilistic extension — for each constraint-context pair, it assigns a point in the probability simplex $\Delta^5$.

The construction uses Gaussian log-likelihoods for each type based on the constraint's metrics, combined with prior weights and boolean features, normalized via log-sum-exp to a proper distribution. The Shannon entropy of this distribution, normalized by $\log(6)$, measures classification uncertainty: 0 means the probabilistic and deterministic classifiers agree completely; 1 means classification is maximally uncertain.

The diagnostic value of the MaxEnt layer lies in disagreement detection. Three levels of disagreement are tracked:

- **Hard disagreement**: the probabilistic top type differs from the deterministic classification. The two classifiers see different constraints. Across corpora, hard disagreements range from 38 (Corpus B) to 183 (Corpus A).
- **Soft disagreement**: the top types agree, but the probabilistic confidence is low (the second-ranked type has nearly as much probability mass). The deterministic classifier commits to a type that the probabilistic classifier considers marginal.
- **Entropy flag**: the normalized Shannon entropy exceeds a threshold, indicating that probability mass is spread across multiple types. The constraint sits near a multi-way classification boundary.

The overlap between MaxEnt hard disagreements and multi-type gauge orbits ranges from 55% to 91% across corpora. The stable residual — 17 single-orbit hard disagreements in both corpora — identifies near-boundary constraints where the deterministic classifier is confident but the probabilistic model detects ambiguity.

**Indexed MaxEnt variant.** An extended MaxEnt computation uses power-scaled χ instead of raw ε, producing a probability distribution that accounts for observer position. The divergence between classical MaxEnt (using ε) and indexed MaxEnt (using χ) directly measures the probabilistic effect of observer-dependence. This divergence is the foundation for trigger T13 (§3.3) and the oracle gap finding (§4.3). Constraints where the indexed and classical distributions diverge are exactly the constraints where power-scaling changes the probabilistic answer — where the numbers look innocuous from a neutral perspective but become significant when evaluated from a specific power position [STRICT — the divergence is a direct measurement].

**Categorical structure: Markov category.** The MaxEnt layer assigns a probability distribution over the type space at each context, forming stochastic morphisms in a Markov category (Fritz 2020). The deterministic classifier embeds as the deterministic subcategory: definite type assignments compose via the presheaf functor. Copy (feeding a classification to multiple diagnostic subsystems — gauge orbits, MaxEnt, abductive engine) and delete (forgetting classification at one context) are well-defined.

The Giry monad — a natural candidate abstraction for a classifier that assigns probability distributions — requires a multiplication map μ: G(G(Ω)) → G(Ω), collapsing a distribution over distributions into a single distribution. Constructing μ in DR would require a *prior over observer positions*: putting a probability measure over {powerful, powerless, analytical, neutral} would collapse the perspectival structure into a single "expected" distribution, destroying the diagnostic signal. This is precisely what indexical relativity refuses to posit. The Giry monad is too strong; the Markov category is the correct abstraction for indexical systems that maintain perspectival separation without observer-position averaging.

The Markov category status is STRICT pending verification of one condition: naturality of the delete map (whether marginalization over types at one context commutes with the restriction map to another context). If verified, the upgrade from the v3 characterization ("incomplete Giry monad, STRUCTURAL") to "complete Markov category, STRICT" is complete.

**Information-geometric interpretation.** The MaxEnt distributions at each context live on the probability simplex (Δ⁵, g_Fisher), where g_Fisher is the Fisher information metric — the unique contractive Riemannian metric on the simplex, by Čencov's theorem. The L∞ divergence used by T13 defines a polytope threshold on this simplex. The Hellinger distance H²(p_cl, p_idx) = Σᵢ (√pᵢ^cl − √pᵢ^idx)² provides a reparametrization-invariant alternative that decomposes additively by type: the ratio Hᵢ²/H² identifies which types carry the divergence under observer shift.

Corpus-level Hellinger decomposition shows that observer shift moves probability mass primarily along the snare↔tangled_rope axis (tangled_rope: ~44%, snare: ~32%), confirming that Hub 1's sigmoid is the primary mechanism. Mountain contributes less than 0.2% of Hellinger divergence, confirming mountain stability from a new angle. The decomposition provides finer-grained diagnostics than the scalar T13 divergence: rather than "this constraint has divergence 0.07," the framework can report "this constraint's divergence is 68% along the snare-rope axis and 22% along the scaffold axis" [STRUCTURAL — the decomposition is a standard construction; the corpus fractions are empirical].

### 3.3 Abductive Synthesis

The abductive engine synthesizes signals from all other subsystems into structured hypotheses. Its 15 trigger classes function as cross-functor consistency checks, organized around a central design principle: the distinction between detecting when diagnostics *disagree* and detecting when their *agreement* is misleading.

The critical distinction throughout is between **artifacts** and **genuine findings**. An artifact is an expected disagreement — the deterministic classifier intentionally overrides the metric classification via a structural signature, so the MaxEnt model (which does not know about overrides) naturally disagrees. A genuine finding is an unexpected disagreement, revealing structural anomalies that no single diagnostic layer would detect.

#### Category A — Disagreement Detection (T1–T11)

The first eleven triggers implement the original design question: *do the diagnostics disagree with each other?* Each trigger examines whether two or more independent diagnostic views of a constraint agree, and generates a hypothesis when they disagree.

**T1: Signature Override Artifact.** The most pragmatically valuable trigger. When MaxEnt shows a hard disagreement and a known signature override (FNL, FCR, natural law) explains the discrepancy, the trigger classifies the disagreement as a mechanistic artifact rather than a genuine anomaly. T1 runs first; subsequent triggers skip constraints already explained by T1. This is the artifact filter that prevents the engine from generating hundreds of false findings. Confidence: 0.85 [STRUCTURAL].

**T2: Deep Deception.** FNL signature with high MaxEnt P(mountain). The constraint claims naturality, fails Boltzmann compliance, AND the MaxEnt model independently assigns high probability to mountain type. The deception is "metrically deep" — it looks natural from every diagnostic angle except the factorizability test that detects hidden cross-index coupling [STRUCTURAL].

**T3: Metric-Structural Divergence.** High MaxEnt entropy (the metrics are ambiguous) but a stable single-type Dirac orbit (the discrete classification is consistent across all contexts). The constraint sits on a classification boundary but has not yet crossed it. The ambiguity is metric, not structural [STRUCTURAL].

**T4: Confirmed Liminal.** Three independent diagnostics — MaxEnt entropy, multi-type Dirac orbit, and drift events — all detect transition. The constraint is genuinely in structural transit. The triple confirmation makes this among the highest-confidence triggers [STRUCTURAL].

**T5: Coverage Gap.** The Dirac orbit detects perspectival variation across all four standard contexts, but the mismatch detector (which excludes analytical context and uses cut after first match) does not fire. A known diagnostic blind spot [STRUCTURAL].

**T9: MaxEnt Shadow Divergence.** MaxEnt strongly favors a type different from the constraint's signature override target. Catches FCR-gated constraints where the probabilistic model rejects the override intent — the FCR gate deferred the override because perspectival variance exists, but MaxEnt's probability mass points away from what the override *would have* reclassified to [STRUCTURAL].

**T10: Convergent Structural Stress.** A "common core + rare gate" design. The common core requires ≥4 of 5 stress indicators (false signature, low purity, drift events, high coupling, elevated entropy). The rare gate requires at least one genuinely tail-distributed anomaly signal (MaxEnt hard disagreement OR H¹ ≥ 4). The common core confirms "broadly stressed"; the rare gate establishes "anomalously so." This trigger underwent three design iterations to reduce false positive rate [STRUCTURAL].

**T11: Snare-Leaning Tangled.** Override target is tangled_rope but MaxEnt ψ (snare-lean ratio) exceeds threshold: P(snare) > 0.85 AND ψ = P(snare)/(P(rope) + P(snare) + 0.001) > 0.90. The probabilistic model overwhelmingly classifies these as snares despite the tangled_rope classification [STRUCTURAL].

**T6: Accelerating Pathology.** FPN zone migration + purity drift event. The static equilibrium analysis shows contamination AND temporal dynamics confirm it is actively worsening. Requires FPN subsystem [STRUCTURAL].

**T7: Contamination Cascade.** FPN effective purity divergence + network drift. Distinguishes theoretical vulnerability from ongoing contamination propagation. Requires FPN subsystem [STRUCTURAL].

**T8: Dormant Extraction.** Low MaxEnt entropy + clean type (rope or mountain) + extractive fingerprint voids + coupling above threshold. The constraint looks clean by every metric but has the structural fingerprint pattern of hidden extraction — a sleeper [STRUCTURAL].

**Corpus sensitivity note.** Triggers T2–T8 fire zero times on the Flash corpus; they require multi-temporal or FPN data not present in single-snapshot analysis. Triggers T9–T11 fire at meaningful rates (T9: ~207, T10: ~232, T11: ~192 on Flash; lower on Haiku due to different metric distributions). Trigger populations are corpus-dependent; the trigger *definitions* are framework properties.

#### Category B — Agreement Verification (T13–T16)

After Category A establishes which diagnostics disagree, Category B asks a different question: *is the remaining agreement misleading?* These triggers detect cases where the framework's classical tools are systematically blind to structure that the indexed and cohomological tools can see. This represents a qualitative expansion of the engine — it now audits its own confidence.

**T13: MaxEnt Divergence (corrected).** The classical MaxEnt (using raw ε) and the indexed MaxEnt (using power-scaled χ) produce divergent probability distributions. T13's diagnostic power varies dramatically by observer context. At the analytical context — where the Prolog implementation runs T13 — only 6–12 constraints fire across the two corpora (1.7–2.8% of H¹ > 0). The smooth analytical observer scaling (χ ≈ 1.37ε) is absorbed by profile recalibration: indexed profiles shift right by 37%, preserving relative type probabilities. At the institutional context (χ/ε ≈ −0.04, sign flip), T13 fires on approximately 97% of observer-dependent constraints, because no profile recalibration can absorb a sign change. The framework runs T13 at the analytical context — the worst case for MaxEnt detection — making the oracle gap a conservative bound. See §4.3 for the full per-context analysis. Confidence: 0.80 [STRICT — the divergence is a direct measurement].

**T14: Hub-Conflict.** Constraints where Hub 2's immutability table flips the classification. These sit at exactly H¹ = 4: the discrete (TimeHorizon × ExitOptions) lookup table switches between "mountain" and "not mountain" as the observer position changes. The classification change is driven by a discrete table lookup, not by the continuous extraction metrics. Example: a neuroscientist at exit_options(analytical) sees biological curiosity as immutable law (mountain); a trapped subject sees it as inescapable suffering (snare). The extraction metrics are identical; the immutability perception is different. Confidence: 0.75 [STRUCTURAL].

**T15: Epistemic Trap.** The powerless observer's restricted information set produces a different classification than the full-information classifier. The gap between `classify_from_restricted/3` and `dr_type/3` measures the epistemic cost of the observer's position. Three distortion patterns:

| Pattern | What happens |
|---|---|
| mountain → rope | Natural laws appear as coordination choices. The observer may waste effort trying to change something unchangeable. |
| tangled_rope → snare | Mixed systems appear purely extractive. The observer sees more extraction than exists. |
| tangled_rope → rope | Extraction becomes invisible. The restricted view shows only the coordination function. |

The third pattern is the dangerous one: "it's just the rules" is literally what the powerless observer perceives, because the features that reveal extraction are not in their accessible information set. This is the structural mechanism behind cover stories. Confidence: 0.70 [STRUCTURAL].

**T16: Classical Oracle Failure (largest trigger class).** MaxEnt entropy is low (oracle is confident) but H¹ > 0 (cohomological obstruction confirms observer-dependence). The classical oracle thinks it has a clear answer, but the structural analysis reveals the answer changes with observer position. This trigger catches the vast majority of observer-dependent constraints that T13 misses — constraints where observer-dependence produces categorical classification shifts without producing large probabilistic shifts. The Gaussian likelihoods are broad enough that raw metrics are compatible with multiple types, so shifting from ε to χ does not move the probability mass much — but it *does* cross the deterministic thresholds. Together, T13 and T16 cover approximately 97–99% of all H¹ > 0 constraints. Confidence: 0.55 + min(0.15, H¹ × 0.03), scaling with H¹ severity [STRUCTURAL].

**Note on T12.** T12 (post-synthesis divergence) exists as a separate module in the post-synthesis phase, not as a standard trigger in the main engine.

**Trigger Summary Table**

| ID | Name | Category | Signal Type | Confidence | Rigor |
|---|---|---|---|---|---|
| T1 | Signature Override Artifact | A (artifact filter) | MaxEnt vs. override | 0.85 | STRUCTURAL |
| T2 | Deep Deception | A | FNL + mountain metrics | 0.70 | STRUCTURAL |
| T3 | Metric-Structural Divergence | A | entropy vs. orbit | 0.65 | STRUCTURAL |
| T4 | Confirmed Liminal | A | triple confirmation | 0.75 | STRUCTURAL |
| T5 | Coverage Gap | A | orbit vs. mismatch | 0.60 | STRUCTURAL |
| T6 | Accelerating Pathology | A | FPN + drift | 0.70 | STRUCTURAL |
| T7 | Contamination Cascade | A | FPN + network drift | 0.65 | STRUCTURAL |
| T8 | Dormant Extraction | A | clean type + extractive voids | 0.50–0.70 | STRUCTURAL |
| T9 | MaxEnt Shadow Divergence | A | MaxEnt vs. override target | 0.75 | STRUCTURAL |
| T10 | Convergent Structural Stress | A | multi-signal + rare gate | 0.50–0.70 | STRUCTURAL |
| T11 | Snare-Leaning Tangled | A | MaxEnt ψ ratio | 0.65 | STRUCTURAL |
| T13 | MaxEnt Divergence | B | indexed vs. classical MaxEnt | 0.80 | STRICT |
| T14 | Hub-Conflict | B | H¹ = 4 band | 0.75 | STRUCTURAL |
| T15 | Epistemic Trap | B | restricted vs. full view | 0.70 | STRUCTURAL |
| T16 | Classical Oracle Failure | B | confident oracle + H¹ > 0 | 0.55–0.70 | STRUCTURAL |

### 3.4 Trajectory Mining and Structural Families

A constraint's **trajectory** is its complete presheaf evaluation enriched with continuous diagnostics: for each of the four standard contexts, the trajectory records the type, experienced extractiveness χ, MaxEnt entropy, and classification confidence. Where the gauge orbit records only the discrete type at each context, the trajectory captures the full quantitative profile.

The **trajectory distance** between two constraints is a weighted 4-component metric:

| Component | Weight | What it measures |
|-----------|--------|-----------------|
| Shift distance | 0.35 | Agreement of type profiles across contexts |
| Metric distance | 0.25 | Similarity of continuous metrics (χ, entropy) |
| Stability distance | 0.25 | Similarity of purity, coupling, and naturality health |
| Pathology distance | 0.15 | Similarity of drift counts and contamination status |

Hierarchical agglomerative clustering (HAC) with average linkage groups constraints into **structural families** — equivalence classes under trajectory similarity. A two-stage approach is used: first, constraints are grouped by their discrete shift pattern (the ordered type profile across contexts, yielding shift groups); then HAC is applied within each shift group using the continuous metric components. This is semantically sound because constraints with different shift patterns have high shift-distance by definition, so they would rarely merge before the clustering cut level.

The most striking product of trajectory mining is **cross-domain twins**: constraints from entirely unrelated domains that belong to the same structural family because they transform identically under observer shifts. A tax regulation and a technology governance mechanism, originating in different legal systems and serving different functions, may be structurally isomorphic — invisible from any single observer position, visible only by examining how both vary across contexts. This is Grothendieck's relative viewpoint implemented as computation: identity is determined by behavior under morphisms, not by intrinsic properties [STRUCTURAL].

### 3.5 Diagnostic Integration and Verdict Synthesis

The diagnostic integration system aggregates signals from all available subsystems into a single structured verdict per constraint. It is the synthesis layer: where individual diagnostic probes each measure one aspect of the presheaf structure, the integration system asks whether their collective testimony is consistent.

**The 12 subsystems.** Each probe examines one diagnostic dimension and returns a signal:

| Subsystem | What it measures |
|---|---|
| maxent | Probabilistic type agreement / disagreement / entropy |
| cohomology | H¹ obstruction, descent status |
| abductive | Triggered hypotheses (genuine vs. artifact) |
| signature | Override signature match |
| boltzmann | Factorizability compliance |
| purity | Structural health for clean types |
| dirac | Dirac class consistency |
| fingerprint_voids | Structural void detection |
| drift | Temporal event severity |
| context_gap | Restricted-view classification divergence |
| fcr_gate | FCR override deferral status |
| gauge_orbit | Multi-type orbit detection |

**Signal classification.** Each probe returns one of three signals: *agrees* (the subsystem's view is consistent with the deterministic classification), *disagrees(Detail)* (the subsystem's view conflicts, with structured evidence), or *inconclusive* (the subsystem lacks sufficient data to evaluate). Disagreements are then further classified as expected conflicts or genuine tensions.

**Expected conflict catalog (P1–P11).** Eleven patterns recognize known architectural disagreements that are NOT genuine tensions. Each pattern is a meta-predicate that matches specific (subsystem, disagreement detail, deterministic type) triples and explains why the conflict is expected:

| Pattern | Name | What it explains |
|---|---|---|
| P1 | signature_override_artifact | MaxEnt disagrees because signature override forces a type MaxEnt cannot replicate |
| P2 | fcr_gate_deferral | FCR signature present but perspectival variance gate deferred the override |
| P3 | cohomological_fracture_divergence | H¹ > 0 confirms genuine perspectival fracture; ambiguity is structural |
| P4 | natural_law_incidental_beneficiaries | True natural laws can have incidental beneficiaries (fingerprint voids expected) |
| P5 | constructed_non_compliance | Constructed types couple dimensions deliberately; Boltzmann non-compliance is confirmatory |
| P6 | fcr_zero_excess_coupling | FCR triggered by coupling structure, not extraction overhead |
| P7 | pre_post_override_divergence | Restricted or Dirac classifier sees pre-override metric-based type |
| P8 | tangled_rope_mixed_dirac | Tangled rope is mixed first-class/second-class by definition |
| P9 | residual_type_artifact | Residual types not in MaxEnt's 6-type model; disagreement is mechanistic |
| P10 | perspectival_orbit_variance | Multi-type orbit IS the perspectival fracture |
| P11 | fcr_deferred_signature_mismatch | FCR override target mismatch because perspectival variance gate deferred |

The catalog is validated by a selftest predicate that verifies each pattern fires on at least one corpus constraint [STRUCTURAL — hand-crafted, empirically validated by selftest].

**Convergent rejections.** When 2+ independent subsystems point to the *same* alternative type, this is a convergent rejection — the strongest signal the integration system can produce. A constraint classified as rope that shows MaxEnt hard disagreement favoring snare AND restricted-view classification as snare has a convergent rejection toward snare from two independent analytical perspectives [STRUCTURAL — counting-based].

**Verdict computation.** The verdict distribution varies across corpora, reflecting different input characteristics:

| Verdict | Corpus A (907) | Corpus B (887) |
|---------|---------------|----------------|
| GREEN | 194 (20.2%) | ~40 (4.5%) |
| YELLOW | 760 (79.2%) | ~825 (93.0%) |
| RED | 6 (0.6%) | ~22 (2.5%) |

The shift toward yellow in Corpus B reflects the higher H¹ > 0 population (80% vs 32%): more observer-dependent constraints generate more diagnostic tensions. The low green rate is not a quality concern — it means the diagnostic layers are detecting genuine perspectival structure rather than passing constraints through unexamined [STRUCTURAL — threshold-based verdict computation].

**Critical design constraint.** The verdict does NOT change classification. Red means "analyst should investigate," not "reclassify." The deterministic Tier 1 classifier remains the source of truth. The diagnostic integration system provides meta-analytical commentary on how confident the analyst should be in the classification, not a competing classification. This separation is deliberate: the verdict is an observation about the coherence of diagnostic signals, not a correction of classification.

---

## 4. Cohomological Results

This section presents the empirical core of the framework. Every number traces to corpus computation. Where findings are stable across both corpora, single values are reported. Where findings are corpus-dependent, ranges are given. The distinction between framework-stable and corpus-dependent findings is itself a result (§4.5).

### 4.1 H⁰: Global Sections

The zeroth Čech cohomology $H^0(\mathcal{U}, F_C)$ counts the global sections of the classification presheaf — constraints whose classification is the same from every observer position. A global section exists if and only if the gauge orbit is a singleton: all four standard contexts agree on the type.

**Result.** The descent rate — the fraction of constraints admitting a global section — ranges from 20.3% (Corpus B) to 76.3% (Corpus A). The existence of a substantial observer-independent subpopulation is robust across both corpora; the magnitude is sensitive to the epsilon distribution of the input. The wide range is itself informative: the descent rate is a property of the corpus-generator pair, not a framework constant. Models that generate more polarized constraint stories (where extraction and coordination metrics are clearly distinguished) produce higher gauge-invariance; models that generate more nuanced stories (with extraction and coordination balanced at similar levels) produce higher gauge-variance. The radical difference traces to a specific mechanism: Corpus B generates higher epsilon values for tangled_rope, pushing more constraints into the snare zone at the analytical context while the false_ci_rope override holds them as tangled_rope deterministically, creating per-context type disagreement (H¹ > 0).

The breakdown by type within H⁰ is dominated by tangled ropes and mountains in both corpora. The mountains in H⁰ are the genuine natural laws of the corpus: thermodynamic constraints, impossibility theorems (Arrow's, Gödel's), information-theoretic limits (Chaitin's omega, halting problem). These constraints are context-invariant by their nature — the second law of thermodynamics does not depend on who is observing.

The ropes in H⁰ are universally recognized coordination mechanisms: standard protocols (TCP/IP, metric system), well-established legal frameworks. The largest H⁰ group is the tangled ropes: constraints where the entanglement of coordination and extraction is visible from every observer position. These constraints descend — every observer sees them as tangled — but the *structure* of the tangling may vary across the Power × Scope grid. Descent tests consistency of the type; naturality tests consistency of the mechanism.

**The near-absence of snares.** Across both corpora, extractive constraints in H⁰ are vanishingly rare: zero in Corpus A, one borderline case in Corpus B. The single exception in Corpus B is a false_natural_law snare with ε = 0.75 and classification confidence of 0.01 (rival type: tangled_rope at 0.95) — a borderline case at the extreme edge of the classification boundary.

This near-absence is the most philosophically significant finding of the cohomological computation. It means that extraction is almost never observer-independent: every constraint that any observer classifies as a snare is classified differently by at least one other observer. In presheaf-theoretic terms, "this constraint is extractive" is almost never a global section — it is a local truth that fails to glue.

The mechanism is structural, not accidental. Hub 1's sigmoid amplifies extraction differently at each power level: the power-scaling function maps different observer positions to different experienced-extractiveness values through the directionality derivation chain. For a constraint to appear as a snare at every context, the sigmoid would need to push χ above the snare threshold at every power level — but the sigmoid is calibrated so that higher power systematically reduces experienced extractiveness, making this impossible for any constraint with finite base extractiveness except at pathological calibration boundaries [STRUCTURAL].

**Mountain stability.** All genuine mountains — constraints in H⁰ with ε < 0.05 — show exactly zero MaxEnt divergence between classical and indexed runs in both corpora. The sigmoid produces no effect on near-zero extraction. Mountains and snares occupy opposite poles of the indexicality spectrum: mountains are fully classical (recoverable from non-indexed data), snares are fully indexed (invisible without observer position) [STRICT — zero divergence is a measurement, not an inference].

### 4.2 H¹: Perspectival Fracture

For each constraint, the H¹ proxy counts the number of disagreeing context-pairs among the $\binom{4}{2} = 6$ unordered pairs of standard contexts. A pair $(U_i, U_j)$ disagrees when $F_C(U_i) \neq F_C(U_j)$.

**Why disagreeing-pairs is a meaningful cohomological measure.** The site is a linear poset (powerless < moderate < institutional < analytical), not a discrete set. Equipped with the Alexandrov topology (upper sets are open), $U_i \cap U_j = U_{\max(i,j)}$ is non-empty for every pair — all elements are comparable in a linear order. The disagreeing-pairs count therefore measures the number of restriction morphisms along which the presheaf's type assignment fails to be compatible. This is a combinatorial descent-failure count on the poset site: it records how many morphisms in the site category witness a failure of the presheaf to satisfy the descent condition. It is not formal Čech H¹ (which would require the quotient $\ker(\delta^1)/\operatorname{im}(\delta^0)$ and is trivially 0 on a discrete site where intersections are empty), but it is a well-motivated obstruction measure that captures genuine cohomological content on the Alexandrov site.

The distribution across the two corpora:

| H¹ | Corpus A (907) | Corpus B (887) |
|----|---------------|----------------|
| 0 | 692 (76.3%) | 181 (20.3%) |
| 1 | **0 (0.0%)** | **0 (0.0%)** |
| 2 | **0 (0.0%)** | **0 (0.0%)** |
| 3 | 64 (7.1%) | 353 (39.8%) |
| 4 | 12 (1.3%) | 14 (1.6%) |
| 5 | 94 (10.4%) | 320 (36.1%) |
| 6 | 45 (5.0%) | 19 (2.1%) |

**The gap at H¹ = 1, 2 (cross-model invariant).** No constraint in either corpus has exactly 1 or 2 disagreeing pairs. This is a structural consequence of the site's linear ordering and the classification cascade's threshold geometry, not an empirical accident. The four standard contexts are ordered on a one-dimensional power axis, and the classification cascade uses fixed thresholds on continuous metrics scaled by a monotone sigmoid of power. When a constraint's experienced extractiveness crosses a classification threshold, it crosses at a single power boundary — say, between U₂ and U₃. But because the power ordering is linear, this creates a 3+1 split (three contexts on one side, one on the other) or a 2+2 split, never a 1+3+0 or other configuration that would produce exactly 1 or 2 disagreeing pairs. A single threshold transition on a linearly ordered 4-element site generates exactly 3 or 4 disagreeing pairs — never 1 or 2.

This gap is a property of the measurement apparatus (the site geometry), not of the constraints. The framework predicts a specific falsification condition: modifying the sigmoid parameters — moving d_institutional toward d_moderate to reduce the sign-flip — would weaken the superselection gap; eliminating the sign-flip entirely would fill it. A richer site with non-linear power relationships could produce H¹ = 1 or 2 by enabling non-adjacent threshold crossings. The gap's dependence on site geometry is itself a testable claim, not an unfalsifiable structural assertion. The gap and its mechanism are formally derived from the site geometry [STRICT]. The superselection analogy is productive — both prohibit certain state combinations as a consequence of the ambient structure rather than of any particular state — but imports Hilbert-space connotations the framework does not support [STRUCTURAL].

**H¹ band structure.** The two-hub architecture (§2.7) gives the H¹ distribution an internal structure that tracks specific architectural mechanisms:

| H¹ band | Mechanism | Hub |
|---------|-----------|-----|
| 0 | Neither hub diverges. All observers agree. | — |
| 3 | Hub 1 sigmoid pushes χ across threshold at a single power boundary. | Hub 1 |
| 4 | Hub 2 immutability table flips between mountain and non-mountain. | Hub 2 |
| 5–6 | Multiple interactions between both hubs, or multiple threshold crossings. | Both |

The H¹ = 3 band corresponds to Hub 1-driven divergence: the dominant pattern is the institutional observer (U₃) seeing coordination where other observers see extraction, because the sigmoid suppresses experienced extractiveness at the institutional power level. The H¹ = 4 band corresponds to Hub 2-driven divergence: the immutability table flips between mountain and not-mountain at the powerless/moderate boundary. This band–hub correspondence means the cohomological formalism has decomposed observer-dependence into components that track specific architectural mechanisms [STRUCTURAL — empirical, not derived from formal categorical structure].

**The dominant mode.** The distribution of H¹ values varies substantially between corpora, reflecting their inverted spectral characters. In Corpus A, H¹ = 0 dominates (76.3%) and H¹ = 5 is the second-largest band (10.4%). In Corpus B, H¹ = 3 dominates (39.8%) with H¹ = 5 close behind (36.1%). Corpus A is overwhelmingly gauge-invariant (76% of constraints look the same from all perspectives); Corpus B is overwhelmingly gauge-variant (80% of constraints shift classification under observer change). Despite this inversion, the most common perspectival fracture pattern in both corpora involves the institutional observer (U₃) seeing coordination (rope) where other observers see extraction or entanglement. The institutional observer is the dominant dissenter.

**Maximal obstruction.** H¹ = 6 constraints — all pairs disagree — range from 19 (Corpus B) to 45 (Corpus A). In Corpus A, the H¹ = 6 population is larger than in the previous corpus build, reflecting the rebuilt corpus's redistribution of gauge-variant constraints. In Corpus B, H¹ = 6 is a smaller and less structured population. These represent the limiting case of perspectival dependence: power level completely determines what you see.

**H¹ is constant within orbit families.** Within any given orbit family (constraints sharing the same set of types in their orbit), H¹ is constant. This is a consequence of the site structure — the number of disagreeing pairs is fully determined by the set of distinct types and how they distribute across the four canonical positions. H¹ does not add information *within* orbit families; what it adds is a graduated numerical measure allowing comparison *across* families.

### 4.3 The Classical Oracle Gap

The MaxEnt classifier, operating on observer-independent metrics alone, detects only a small fraction of the observer-dependence that cohomological analysis detects. The gap is observer-position-dependent, largest where the observer shift is a smooth rescaling (analytical) and smallest where it is a qualitative transformation (institutional sign flip).

**Per-context T13 detection (Corpus A):**

| Context | T13 Fires | % of H¹ > 0 | Mean χ/ε | Mechanism |
|---------|-----------|-------------|----------|-----------|
| Institutional | ~864 | ~97% | −0.04 | Sign flip — no profile recalibration absorbs a sign change |
| Powerless | ~539 | ~61% | ~1.08 | Near identity — slight divergence detectable |
| Moderate | ~544 | ~61% | ~1.10 | Near identity — slight divergence detectable |
| Analytical | 6 | 2.8% | 1.37 | Smooth scaling — profiles absorb the shift |

At the analytical context — where the Prolog implementation runs T13 — only 6 constraints fire in Corpus A (2.8% of H¹ > 0) and 12 in Corpus B (1.7% of H¹ > 0). The smooth 1.37× scaling at the analytical context is absorbed by profile recalibration: when indexed profiles are recalibrated to chi values, the Gaussian likelihoods shift right by 37% but preserve their shape, and the relative ordering of type probabilities barely changes. Mean corrected total variation distance at analytical is 0.0006 (effectively zero).

At the institutional context (χ/ε ≈ −0.04, sign flip), the sigmoid maps positive extraction to negative experienced extractiveness. No profile recalibration can absorb a sign change — distributions for "negative extraction" look nothing like distributions for "positive extraction." MaxEnt detects approximately 97% of observer-dependence at this context.

The oracle gap ratio at the analytical context is approximately 36× (Corpus A) to 59× (Corpus B). The v3 "100x" figure reflected an earlier, smaller corpus; the mechanism is identical but the magnitude is corpus-dependent. The framework runs T13 at the analytical context — the worst case for MaxEnt detection — making the claim conservative: cohomological analysis adds the most diagnostic value precisely where MaxEnt is weakest.

The mechanism is precise: most observer-dependence produces categorical classification shifts (rope → snare, mountain → scaffold) without producing large probabilistic shifts in the MaxEnt distribution. The Gaussian likelihoods are broad enough that raw metrics are compatible with multiple types, so shifting from ε to χ does not move the probability mass much — but it *does* cross the deterministic thresholds. The classical oracle fails not by getting wrong answers but by failing to detect that answers *differ* across indices. T16 catches what T13 misses because categorical thresholds do not recalibrate [STRUCTURAL — the per-context pattern is corpus-specific; the mechanism is architectural].

**Epistemic restriction vs. frame-dependence independence.** Of (constraint, context) pairs showing restricted-view divergence and pairs showing gauge-fixedness, only approximately 1.5% overlap. Restricted-view divergences are information-loss errors (the observer lacks data); gauge-fixed constraints are frame errors (the observer has full data but processes it through a structural relationship). These are independent phenomena: epistemic restriction is having a reduced density matrix; frame-dependence is having a different measurement basis [STRUCTURAL].

### 4.4 Coalition Structure

The Galois connection between observer coalitions and consensus types provides a finer invariant than H¹ alone. For a given constraint, define the **agreement set** for type *T* as the set of contexts that classify the constraint as *T*, and the **consensus** of a coalition *S* as the type (if any) that all members of *S* agree on. These two maps form an antitone Galois connection between the lattice of observer coalitions and the lattice of types.

The Galois lattice per constraint captures the *structure* of observer agreement — not just whether observers disagree but *which groups form natural consensus blocs*. Two constraints with the same H¹ can have different Galois lattices: [snare, snare, rope, snare] (H¹ = 3, one dissenter against a bloc) has a qualitatively different politics of disagreement than a hypothetical [rope, snare, snare, rope] (H¹ = 4, two equal-sized blocs).

**The institutional observer as dominant dissenter (cross-model invariant).** Aggregating Galois-closed coalitions across both corpora reveals a stable pattern: the institutional observer (U₃) is the most frequent isolated dissenter. In Corpus A, the {U₁, U₂, U₄} vs. U₃ coalition structure accounts for the majority of H¹ = 3 constraints. In Corpus B, the same pattern is even more pronounced: the institutional context classifies 735 constraints as rope (vs. 7 at analytical), creating a sharp institutional dissent signature. The institutional context shows only 12 snare classifications compared to 152–448 at analytical across the two corpora.

In both corpora, the institutional observer sees coordination (rope) where all other observers see extraction (snare) or entanglement (tangled_rope). This is the Galois expression of a central claim: institutional power is the structurally decisive perspective. The institutional observer, with generational time horizon and arbitrage exit options, systematically reclassifies extraction as coordination. This is not a bias to be corrected; it is a structural consequence of the institutional observer's position. From a position with the power to reform a constraint, the constraint genuinely *functions* as coordination — the extractive features are either invisible (the observer benefits from them) or irrelevant (the observer can exit them). The classification is correct *from that position*. It is incorrect from others. (The lattice computation is STRICT; the corpus-level pattern is an empirical finding.)

The **splitting degree** — the minimum number of observers needed to fully determine a constraint's type profile — provides a measure of observer redundancy not captured by H¹ or orbit families. A constraint with splitting degree 1 can be fully characterized from any single observer position; a constraint with splitting degree 4 requires every observer to contribute unique information.

**Coalition structure: inverted corpora, convergent outputs.** The two corpora exhibit inverted coalition structures that mirror their inverted H¹ distributions:

| Coalition | Flash | Flash % | Haiku | Haiku % |
|-----------|-------|---------|-------|---------|
| institutional_dissent | 246 | 44.8% | 40 | 7.1% |
| split_field | 298 | 54.3% | 95 | 16.8% |
| uniform_tangled | 5 | 0.9% | 413 | 72.8% |

*Percentages are of the tangled_rope population in each corpus (Flash N=549, Haiku N=567).*

Flash is dominated by institutional_dissent and split_field (99% combined) — constraints where observers actively disagree about the type. Haiku is dominated by uniform_tangled (73%) — constraints where all observers agree on the tangled_rope classification. This is consistent with the H¹ distribution: uniform_tangled means all observers agree, producing H¹ = 0, which dominates Haiku. The inversion is complete: every corpus-dependent measure points in opposite directions, while every framework-dependent measure (eigenvalues, confidence bands, superselection gap) is stable.

**Tangled rope fiber decomposition.** The tangled_rope population — approximately 62% of both corpora after signature integration — admits a continuous decomposition via the MaxEnt snare-lean metric ψ = P(snare) / (P(rope) + P(snare) + ε). The decomposition reveals two qualitatively different internal structures depending on the generating model.

In the Flash corpus (N=549 tangled_ropes), the ψ distribution is strongly bimodal: mass concentrates at ψ ≈ 0 (rope-leaning, 34.6%) and ψ ≈ 1.0 (snare-leaning, 63.6%), with only 10 constraints (1.8%) in the genuinely tangled middle band (0.3 < ψ < 0.7). In the Haiku corpus (N=567), the distribution is unimodal: 416 constraints (73.4%) cluster in the genuinely tangled band, with only 28 rope-leaning (4.9%) and 123 snare-leaning (21.7%). The same framework machinery, applied to corpora from different generators, reveals that Flash constraints tend to be metrically decisive (the classifier is confident about rope or snare direction) while Haiku constraints are metrically ambiguous (the classifier genuinely cannot resolve the direction). Both populations are classified as tangled_rope, but for different reasons: Flash tangled_ropes are held in place by the signature override against a clear metric lean; Haiku tangled_ropes occupy the type because the metrics themselves are balanced.

The two mechanisms produce the same post-override population (~62% tangled_rope) through different paths. Flash's bimodal ψ distribution reflects forced reclassification: constraints that the metrics confidently classify as rope or snare are overridden to tangled_rope by the FCR signature. Haiku's unimodal ψ distribution reflects genuine metric ambiguity: MaxEnt assigns nearly equal probability to rope and snare, placing constraints in the tangled_rope category via the metric path rather than the override path. The convergence of different mechanisms to the same attractor population is the strongest evidence that the 62% tangled_rope rate is a framework property rather than a corpus artifact.

The decomposition method does not impose the structure it finds. The same ψ calculation on both corpora reveals the corpus-specific metric distribution within the tangled_rope category, validating that the decomposition is diagnostic rather than tautological.

**Institutional dissent: a binary split.** Within the Flash corpus, 246 tangled_rope constraints exhibit institutional dissent — the institutional observer classifies differently from the other three. These split into two clean subpopulations with zero overlap: 213 low-snare constraints (ψ < 0.006, orbit pattern tangled/tangled/rope/tangled — the institution sees coordination others miss) and 33 high-snare constraints (ψ > 0.995, orbit pattern snare/snare/rope/snare — the institution sees coordination that may not exist). The separation is perfect: rank-biserial r = 1.0 on suppression, confidence, and ψ. The Haiku corpus has 40 institutional dissent constraints, 39 of which fall in the low-snare group — too few high-snare cases (N=1) for the binary split to be confirmed as cross-corpus general. Whether the institutional observer's dissent represents informational advantage (seeing real coordination) or metric bias (the sigmoid suppressing real extraction) remains an interpretive question the framework surfaces but does not resolve.

### 4.5 Spectral Geometry and Cross-Model Convergence

The two-corpus design yields the paper's strongest empirical result: a sharp partition of findings into framework-stable topological invariants and corpus-dependent statistical properties. The partition is sharpened by the fact that the two corpora are *spectral inversions* of each other — opposite on every corpus-dependent measure, identical on every framework-dependent measure.

**The sheaf Laplacian as structural theorem.** The sheaf Laplacian L₀ on the 4-element linear site has eigenvalues λ = {0, 0.0152, 2.9953, 72.1839}, identical across every corpus analyzed. This identity is a structural theorem, not an empirical discovery: L₀ is fully determined by the restriction map ratios r_ij = σ(π(Uᵢ)) / σ(π(Uⱼ)), which depend only on the sigmoid parameters in the configuration — specifically the canonical directionality values and scope modifiers — not on any property of the corpus data.

| Property | Corpus A (Haiku 4.5) | Corpus B (Gemini Flash 2.0) |
|----------|----------------------|----------------------------|
| Eigenvalues | [0, 0.0152, 2.9953, 72.1839] | [0, 0.0152, 2.9953, 72.1839] |
| Spectral gap λ₂ | 0.0152 | 0.0152 |
| r₂₃² spectral weight | 97% | 97% |
| λ₃/λ₂ | 196.83 | 196.83 |
| λ₄/λ₂ | 4743.52 | 4743.52 |
| Restriction ratios r₁₂, r₂₃, r₃₄ | 1.42, −8.38, −0.10 | 1.42, −8.38, −0.10 |
| Institutional eigenvector loading | 0.9927 | 0.9927 |

The spectral structure reveals the framework's context geometry. The spectral gap λ₂ = 0.0152 is three orders of magnitude below λ₄ = 72.18, with the dominant eigenmode (mode 4, carrying 97% of spectral weight) localized on the institutional vertex (eigenvector loading 0.9927). The institutional observer occupies an isolated eigenspace, effectively decoupled from the other three. The superselection gap (H¹ = 1, 2 empty) is a direct spectral consequence: the institutional vertex contributes either 0 or 3 disagreeing pairs, with intermediate values forbidden by the eigenspace isolation. The moderate→institutional boundary is where the framework's context geometry concentrates its discriminating power — the institutional observer's sign flip creates the largest restriction-map discontinuity in the system.

**Per-constraint obstruction energy.** The per-constraint obstruction energy E(C) = vᵀL₀v, where v is the constraint's χ vector across four perspectives, correlates with H¹ at a strength that depends on corpus composition: Spearman ρ = 0.66 (p < 10⁻¹¹⁰) on Flash, but only ρ = 0.20 (p = 1.2 × 10⁻⁹) on Haiku. The Pearson correlation on Haiku is 0.06 (p = 0.064), nearly non-significant. The correlation weakens when most constraints are gauge-invariant (Haiku: 76% at H¹ = 0), because E(C) carries less H¹-discriminating information when most obstruction energies are near zero. The eigenvalues are a framework property; the E(C)–H¹ correlation strength is a corpus property that tracks the gauge-variance rate. The vector-valued upgrade (MaxEnt distributions as 8-dimensional stalks) would avoid the single-ratio dominance problem and is a natural next step (§5.5) [STRICT for the Laplacian construction; CORPUS-INDEPENDENT for the eigenvalues].

**Falsifiability.** The spectral structure is falsifiable. Changing the sigmoid parameters — moving d_institutional toward d_moderate — would reduce λ₄, broaden the institutional eigenspace, weaken the superselection gap, and eventually fill H¹ = 1 and 2. The framework predicts that its most striking structural invariant (the gap) depends on a specific measurable property (the institutional sign-flip), which can be modified and the consequences observed.

**The inversion test.** The strongest validation comes from the two corpora's inverted statistical profiles. Corpus A (Haiku) is 76% gauge-invariant, tangled_rope-dominated (68% metric), with a unimodal ψ distribution. Corpus B (Flash) is 80% gauge-variant, snare-dominated (51% metric), with a bimodal ψ distribution. Every corpus-dependent measure points in opposite directions. Yet the framework-level outputs are stable or convergent:

**Complete invariant table.** The following findings are identical or functionally equivalent across both corpora:

| Finding | Status |
|---------|--------|
| Sheaf Laplacian eigenvalues | **Identical** to 4 decimal places |
| Spectral gap λ₂ = 0.0152 | **Identical** |
| r₂₃² spectral weight = 97% | **Identical** |
| Restriction ratios r₁₂, r₂₃, r₃₄ | **Identical** |
| Institutional eigenvector loading 0.9927 | **Identical** |
| H¹ gap at values 1 and 2 | **Present in both** |
| Post-override tangled_rope rate | **~62% in both** (despite inverted pre-override distributions) |
| Confidence bands (deep/moderate/borderline) | **~75%/5%/20% in both** |
| Mountain population | **~14–16% in both** |
| Boolean independence (all features captured) | **Confirmed in both** |
| Index sufficiency | **Confirmed in both** (0–3 genuine collisions) |
| Boundary non-normality (all boundaries reject normal) | **Confirmed in both** |
| Institutional dissent direction (U₃ sees rope) | **Confirmed in both** |
| Zero non-mountain stability anomalies | **Confirmed in both** |
| Zero Type A hub conflicts | **Confirmed in both** |
| Scaffold/piton 100% borderline confidence | **Confirmed in both** |
| Snare ~89% borderline confidence | **Confirmed in both** |

These invariants hold despite severe input correlation: 43% and 75% d-pattern concentration in the two corpora respectively, different LLM architectures, different generation prompt versions, and radically different — indeed inverted — type distributions. **The framework's topological invariants are robust to pathological input distributions, including input distributions that are spectral inversions of each other.**

The corpus-dependent findings — descent rate (20–76%), gauge-variance rate (24–80%), tangled rope ψ structure (bimodal vs. unimodal), institutional dissent population (40–246), false mountain count (667–782), T13 corrected fires (6–12), coalition structure (institutional_dissent-dominated vs. uniform_tangled-dominated) — are statistical properties of the input rather than structural properties of the framework. The two-corpus design allows these to be reported as ranges rather than point estimates, with the variance itself carrying information about which findings generalize.

---

## 5. Honest Assessment

This section is what distinguishes the framework from marketing. Without it, the categorical vocabulary is decoration. With it, it earns its formal claims.

We use a three-level rigor classification throughout:

- **STRICT**: the categorical correspondence holds mathematically. The code implements the categorical structure, and the correspondence survives formal verification.
- **STRUCTURAL**: the analogy is productive and the behavior matches, but formal verification of the full categorical axioms is absent.
- **LOOSE**: the categorical language would mislead if taken literally.

### 5.1 What Is STRICT

The following correspondences hold formally:

- **The site.** The context poset is a genuine small category. The four standard contexts with the power ordering form a finite poset category equipped with a covering family. This is a site.
- **The presheaf.** `dr_type(C, Context, Type)` is a genuine contravariant functor from the site to **Set**. It assigns a type at each context, and the restriction maps (implicit in the power scaling) transform sections along morphisms.
- **Naturality.** The Boltzmann factorizability test is a genuine naturality condition. The factorization test χ(P, S) ≈ f(P) × g(S) is exactly the commutativity of a naturality square on the Power × Scope grid.
- **Boltzmann factorizability = functor axiom.** Multiplicative separability of χ into observer-independent × observer-dependent factors is exactly the condition that restriction maps compose telescopically (§2.4). The Boltzmann compliance test verifies the functor axiom. The chi-override population (0–12.3% of evaluations across corpora) constitutes the documented exceptions where the axiom fails and the presheaf structure is approximate.
- **Naturality witnesses.** FNL (false natural law) is a genuine naturality failure witness; CI Rope is a genuine naturality certificate. Both test well-defined conditions on the naturality square.
- **H⁰ and descent.** Global sections (H⁰) are precisely the constraints satisfying the descent condition. Descent ↔ H¹ = 0 is tautological on discrete covers.
- **Gauge orbits.** The Dirac orbit computation is standard orbit decomposition under the group of context automorphisms.
- **The three-way equivalence (two-out-of-three).** The Lawvere ↔ Grothendieck equivalence (naturality ↔ descent) is STRICT. The Noether column (symmetry conservation) is a productive STRUCTURAL parallel — same predicates, but mapping to a weaker mathematical condition than Noether's theorem proper (discrete group invariance, not continuous Lie symmetry with Lagrangian).
- **The Galois connection.** The coalition–consensus duality is a standard antitone Galois connection between two finite posets.
- **Hub 1 as restriction map.** The power-scaling function `derive_directionality/3` → `sigmoid_f/2` → χ implements a genuine restriction map: it transforms classification along morphisms in the site.
- **Hub 2 as classification gate.** The `effective_immutability_for_context/2` table implements a genuine classification gate: it partitions contexts into mountain-eligible and non-mountain-eligible subsets. Singleton stalks satisfy the functor axioms trivially.
- **Hub independence.** Zero Type A conflicts (mountain + snare-level extraction) verified empirically across both corpora. The BaseEps check prevents the pathological case by construction.
- **Binary gate computations.** Each binary gate is a well-defined predicate over testset declarations and relational data.
- **NMI analysis.** Normalized Mutual Information between binary gates and type space is a standard information-theoretic computation.
- **T13 divergence measurement.** The indexed-vs-classical MaxEnt L∞ divergence is a direct numerical measurement. The Hellinger decomposition Hᵢ²/H² identifies which types carry the divergence. Both are direct computations, not inferences.
- **Mountain zero-divergence.** All genuine mountains (H⁰, ε < 0.05) show exactly zero MaxEnt divergence under indexing in both corpora.
- **Sheaf Laplacian construction.** The Hansen-Ghrist sheaf Laplacian on P₄ with scalar χ stalks is a standard construction; its eigenvalues, spectral gap, and energy measure are direct computations.
- **Cross-model spectral invariance.** The sheaf Laplacian eigenvalues are identical to four decimal places across both corpora — a direct measurement, not an inference.
- **Confidence band distribution.** The MaxEnt classifier's certainty structure — approximately 75% deep confidence, 5% moderate, 20% borderline — is stable across both corpora despite radically different type distributions and H¹ profiles. Per-type confidence patterns also replicate: mountains at 100% deep, snares at ~89% borderline, scaffolds and pitons at 100% borderline, tangled_ropes at ~91% deep. This stability suggests the confidence structure is a framework property (determined by the MaxEnt parameterization and type-space geometry) rather than a corpus artifact.
- **Post-override attractor convergence.** Both corpora converge to ~62% tangled_rope after signature integration despite inverted pre-override distributions (50.5% snare on Flash, 68.2% tangled_rope on Haiku). The convergence is a direct measurement of the signature system's fixed-point behavior.

### 5.2 What Is STRUCTURAL

The following correspondences guide analysis productively but lack formal verification:

- **MaxEnt as Markov category.** The MaxEnt classifier assigns a probability distribution over the type space at each context, forming stochastic morphisms in a Markov category (Fritz 2020). The deterministic classifier embeds as the deterministic subcategory; copy (feeding a classification to multiple diagnostic subsystems) and delete (forgetting classification at one context) are well-defined. The Giry monad's multiplication map μ — requiring a distribution over distributions — is absent because constructing it would require a prior over observer positions, which indexical relativity refuses to posit. The Markov category is the correct abstraction for indexical systems. Pending verification: naturality of the delete map (whether marginalization commutes with restriction maps). If verified, this upgrades to STRICT.
- **Information geometry of T13.** The MaxEnt distributions live on (Δ⁵, g_Fisher). The L∞ divergence used by T13 defines a polytope threshold on this simplex. The Fisher-Rao geodesic ball interpretation applies to KL divergence, not to L∞. The Hellinger decomposition by type applies regardless of which divergence measure triggers the fire, but the geometric characterization of the threshold as a geodesic ball is structural rather than strict for the L∞ implementation.
- **FPN as terminal coalgebra.** The FPN equilibrium is the greatest fixed point of the contamination endofunctor, and convergence is proved via Knaster-Tarski. But the full coalgebra axioms have not been formally verified.
- **Abductive engine as naturality auditor.** The 15 trigger classes test cross-functor consistency — whether independent diagnostic views agree. The artifact/genuine distinction maps cleanly onto expected versus unexpected naturality failures. But the triggers are hand-crafted, not derived from formal categorical constructions.
- **Trajectories as natural transformation families.** Constraints with identical trajectories exhibit the same transformation behavior under context shifts, functioning as representatives of the same natural transformation. But the formal functor-category construction is absent.
- **Contamination as contravariant flow.** Contamination flows against the purity gradient, which is structurally analogous to contravariance. But the gradient reversal is of a scalar, not of categorical morphisms.
- **H¹ proxy.** The disagreeing-pairs count is a combinatorial descent-failure count on the poset site (see §4.2). It is not formal Čech H¹ — which requires the quotient ker(δ¹)/im(δ⁰) and is trivially 0 on a discrete site — but measures genuine obstruction to descent on the Alexandrov site.
- **H¹ band structure.** H¹ = 3 → Hub 1, H¹ = 4 → Hub 2 is an empirical correspondence verified against both corpora. It is not derived from formal categorical structure.
- **Oracle gap.** The gap is observer-position-dependent: 1.7–2.8% at analytical, ~97% at institutional. The ratio varies by corpus; the mechanism (profile recalibration absorbs smooth observer shifts, fails on qualitative transformations) is architectural.
- **Diagnostic verdict synthesis.** The GREEN/YELLOW/RED verdict is a threshold-based aggregation of subsystem signals. The thresholds are calibrated, not derived.
- **Expected conflict catalog (P1–P11).** The 11 patterns are hand-crafted meta-predicates matching known architectural artifacts. Each is validated by the selftest against the corpus.
- **Two-tier gate architecture.** The Tier 1/Tier 2 boundary is a design decision reflecting the structural-property vs. factorization distinction.
- **Epistemic restriction vs. frame-dependence independence.** The ~1.5% overlap is measured against these corpora. The independence claim generalizes the measurement.

### 5.3 What Is LOOSE

The following analogies would mislead if taken literally:

- **Type space as Heyting algebra.** Two absorbing elements (mountain and piton) prevent lattice structure. The composition is a priority monoid. A Heyting algebra claim would invite incorrect expectations about implication and complementation.
- **Power scaling as adjunction.** The sigmoid scaling creates a parametric family of type assignments, but the triangle identities (unit and counit) have not been verified. The existential/universal quantifier structure is suggestive but insufficient.
- **Signature resolution as lattice meet.** The conflict resolution predicate is a priority dispatch table lacking commutativity and associativity. It is not a lattice operation.
- **All five Girard/Linear Logic mappings.** An independent audit found that all five proposed correspondences between the codebase and Girard's linear logic are LOOSE. The systematic error across all five is the same: conflating *computing a quantity that describes a resource* with *consuming that resource*. The system is a calculator, not a cash register — it tracks costs but does not enforce budgets, which is exactly the gap identified in §5.4.
- **The quantum measurement analogy.** The presheaf structure is formally analogous to contextual truth in quantum topos theory (Isham and Butterfield 1998), but the analogy breaks at three critical points: DR classification is reversible (re-evaluate at a different context freely), deterministic (no Born rule), and local (no entanglement). The analogy is useful for intuition but actively misleading for formalism.
- **"Quantum" naming in quantum verification triggers.** Triggers T13–T16 are named "quantum verification triggers" because they were developed by applying concepts from Yuen's quantum complexity theory to the codebase architecture. The name is evocative but would mislead if taken as formal correspondence to quantum computing. The triggers test for classical oracle failures and observer-dependent classification shifts — phenomena that are *analogous* to quantum complexity gaps but do not involve quantum states, unitary transformations, or entanglement.

### 5.4 What the Framework Cannot Do

The framework classifies and diagnoses. It does not:

- **Plan under resource constraints.** The system computes costs (purity-adjusted energy for reform actions, scaffold urgency scores) but does not enforce budgets. There is no concept of finite enforcement capacity, agent attention, or reform allocation. This is the genuine gap identified by the Girard/Linear Logic analysis — the system would need an ontological expansion (resource annotations on agents) and a new operational layer to close it.
- **Perform metric-level sensitivity analysis.** The configuration sensitivity sweep (§2.6) tests parameter robustness — whether the classification cascade's thresholds and modifiers are stable under perturbation. What it does not test is the robustness of the input metrics themselves: do small changes in the author-assigned extractiveness, suppression, or resistance-to-change values cause constraints to reclassify? A constraint near a type boundary might shift type with plausible metric perturbations. This metric-level sensitivity analysis remains to be performed.
- **Extend to infinite or non-linear sites.** The current site is a 4-element linear poset. Extending to a richer site — adding temporal morphisms, scope morphisms as independent dimensions, or non-linear power relationships — would require non-trivial formal work. In particular, H² and higher cohomology become non-trivial on non-discrete sites, and the H¹ gap at 1 and 2 would not persist on a non-linearly ordered site.
- **Establish causation.** The framework detects structural patterns — which constraints are perspectivally fractured, which observer positions are structurally decisive, which constraints are structurally isomorphic across domains. It does not establish *why* a constraint is extractive, *how* extraction emerged, or *whether* reform would succeed. These are causal questions that require different methods.
- **Model lateral extraction.** The power axis in the context tuple is vertical: powerless → moderate → institutional → analytical. The binary gates fire correctly on lateral extraction (peer manipulation, workplace bullying, communal narcissism), and the framework produces correct classifications when observer positions are properly differentiated via exit options and beneficiary/victim declarations. But the representation of the power axis is inadequate: the victim must be coded as "powerless/trapped" and the extractor as "institutional," which forces the vertical machinery to produce the correct result at the cost of misrepresenting the actual structural geometry.

### 5.5 What Would Strengthen the Framework

**Progress since v2.** Item 1 (formal verification of restriction maps) has been closed: the Boltzmann factorizability = functor axiom equivalence (§2.4) proves that restriction maps compose telescopically for all Boltzmann-compliant constraints, with the chi-override population as the documented exceptions. The remaining items are open.

1. **Formal verification of restriction maps — CLOSED.** The power-scaling function's multiplicative factorization χ = ε · [f(d) · σ(S)] guarantees telescopic composition of restriction maps, satisfying the functor axiom. The Boltzmann compliance test is the formal verification. The chi-override population (0–12.3% across corpora) constitutes the documented boundary where the axiom fails. See §2.4. *(Closed in v4.)*
2. **Verification of the monoidal structure.** Determine the precise algebraic structure of the type space under composition. Is the priority monoid a bounded semilattice when the piton anomaly is addressed? *(v2 item, still open.)*
3. **Metric-level sensitivity analysis.** Systematically varying the input metrics (extractiveness, suppression, resistance-to-change) within plausible ranges to measure how many constraints reclassify. This would quantify robustness at the input layer rather than the parameter layer. The two-corpus design provides partial coverage: comparing type distributions across corpora reveals which classifications are sensitive to input variation. *(v2 item, still open.)*
4. **Extension to enriched sites.** Adding temporal morphisms would create a product site P₄ × T with bigraded cohomology H^{p,q}. The Künneth decomposition yields: H^{0,0} (observer-independent, temporally stable), H^{1,0} (current H¹ — spatial observer-dependence), H^{0,1} (observer-independent temporal drift), and H^{1,1} (emergent indexicality — constraints whose observer-dependence itself changes over time). H^{1,1} detects a genuinely new phenomenon: a constraint that was observer-independent in one era but observer-dependent in another. Legal corpora with temporal stratification (e.g., antitrust enforcement 1890–2024) are natural candidates. *(v2 item, expanded in v4.)*
5. **The sheafification question.** The presheaf F can be sheafified to produce the "closest" sheaf F⁺, for any Grothendieck topology J on the site. Sheafification forces descent — it produces a consensus classification by resolving perspectival disagreements into globally consistent sections. For the 20–76% of constraints already in H⁰, sheafification changes nothing; for the remainder with H¹ > 0, it would force a choice that the presheaf currently refuses to make. The question is whether the forced consensus is informative or destructive.

   A worked example clarifies the stakes. Consider the dominant orbit [snare, snare, rope, snare] (H¹ = 3). The institutional observer (U₃) sees coordination; the other three see extraction. Under topology J₁, where {U₁, U₂, U₄} is a covering family (majority rule), sheafification forces "snare" as the global type. Under topology J₂, where {U₃} alone is a covering family (institutional authority), sheafification forces "rope." The choice of Grothendieck topology IS the choice of whose perspective is definitive. The DR framework's decision to remain a presheaf — to refuse sheafification — is the decision to preserve the diagnostic signal rather than force a consensus. *(v2 item, still open.)*

6. **Clean corpus without d-pattern anchoring.** The two existing corpora both exhibit severe d-pattern concentration (43% and 75% on a single pattern). A corpus with proper perspective diversity — d-pattern concentration below 30% — would enable meta-observation claims about the constraint landscape that current corpora cannot support. The surgical prompt fixes identified in the cross-corpus analysis (perspective diversity section, anti-anchoring instruction, emerges_naturally checklist) are designed to produce such a corpus. *(New to v4.)*
7. **Vector-valued sheaf Laplacian.** Replacing scalar χ stalks with MaxEnt distribution vectors (Δ⁷ ⊂ ℝ⁸) yields a 32×32 Laplacian that captures both Hub 1 and Hub 2 contributions, avoiding the single-ratio dominance that limits the current scalar construction. *(New to v4.)*
8. **Financial regulation beta corpus.** An A-S-informed financial regulation corpus (position limits, capital requirements, circuit breakers) with observer positions mapped to market maker / retail trader / regulator / HFT firm would test framework portability to a domain with quantitative ground truth. *(New to v4.)*
9. **Lateral extraction formalization.** A relational dimension in the context tuple — representing the relationship between two same-level observers, not the power level of either — might address the lateral extraction limitation without distorting the existing geometry. *(v3 item, still open.)*
10. **Diagnostic summary → abductive feedback loop.** A constraint with a red verdict that fires zero abductive triggers would itself be a meta-anomaly worth flagging. *(v3 item, still open.)*

---

## 6. Related Work

The framework sits at the intersection of several traditions. Positioning it precisely — what it borrows, what it adds, what it lacks — is essential for honest scholarship.

**Standpoint epistemology** (Harding 1986; Haraway 1988) argues that knowledge is perspectival — that the social position of the knower shapes what can be known. DR formalizes this claim by providing a presheaf-theoretic framework where the "standpoint" is a point of the site and the "knowledge" is the stalk of the presheaf at that point. The formalization adds what standpoint epistemology lacks: quantitative invariants. The descent rate measures *how much* of a domain is perspectival. The H¹ distribution measures the *structure* of perspectival dependence. The Galois lattice identifies *which* standpoints are structurally decisive. What DR lacks is standpoint epistemology's rich account of how standpoints are constituted — the site's four standard contexts are stipulated, not derived from social theory.

**Social choice theory** (Arrow 1951; Sen 1970) studies the aggregation of individual preferences into collective decisions. DR's type space has some structure in common with preference aggregation — the composition operation resolves conflicts between types — but the type space is not a preference ordering and the composition is not a social welfare function. Arrow's impossibility theorem shows that no aggregation rule satisfies a set of desirable axioms; DR's analogue is the near-absence of snares from H⁰, which shows that no observer position universalizes the extractive classification. The formal structures are different (preference lattices vs. presheaves on sites), but the impossibility results are spiritually related.

**Institutional analysis** (Ostrom 1990) classifies institutions by their rules, boundaries, and governance structures. DR classifies constraints by their structural character as seen from different observer positions. The two approaches are complementary: Ostrom asks "what kind of institution is this?" from a single analytical perspective; DR asks "what kind of institution does this look like to different observers?" Ostrom's framework could serve as the institutional semantics that DR currently lacks — providing rich descriptions of the constraints that DR classifies structurally.

**Topos-theoretic approaches in physics** (Isham and Butterfield 1998; Döring and Isham 2008) apply presheaf theory to quantum mechanics, modeling contextual truth — the idea that the truth value of a proposition depends on the measurement context. DR applies the same formal structure to a different domain: the "measurement context" is an observer's social position rather than an experimental apparatus, and the "proposition" is a constraint classification rather than a quantum observable. The formal parallels are genuine (both are presheaves on sites of contexts), and the philosophical parallels are instructive (both formalize the idea that truth is local to a context). The key disanalogy is that quantum measurement involves irreversibility (wave-function collapse), stochasticity (Born rule), and entanglement (non-local correlations), none of which are present in DR.

**Wheeler's participatory universe** (Wheeler 1989) posits that physical reality is constituted by information and that the observer's choice of measurement apparatus determines the observed reality. DR's formal structure matches Wheeler's thesis at the level of presheaf evaluation: the "apparatus" is the observer context, the "measurement" is classification, and the "reality" depends on the choice. The descent rate is the quantitative answer to Wheeler's question — how much of a domain requires an observer to determine the facts? — for the domain of social constraints. The disanalogy is equally informative: Wheeler insists on *free choice* of measurement, while DR has *constrained positionality* (a powerless observer does not *choose* to see a snare; they see it because their structural position constrains their perspective).

**Computational social science** typically treats classification as a supervised learning problem: given labeled training data, learn a classifier that generalizes. DR takes a fundamentally different approach: classification is not learned from data but computed from continuous metrics via a hand-designed, deterministic rule cascade (not a trained classifier), and the central question is not "which label is correct?" but "how does the label depend on who is labeling?" The framework is computational — it is implemented as a Prolog codebase of 77 modules that runs on corpora of nearly 1,000 constraints — but it is not machine learning. Its invariants (descent rate, H¹ distribution, structural families) are formal properties of the classification presheaf, not performance metrics of a learned model.

**Information geometry** (Amari and Nagaoka 2000; Čencov 1982) provides the natural geometric framework for the probability simplex on which MaxEnt distributions live. The Hellinger distance decomposition identifies which types carry the divergence under observer shift, extending the scalar T13 diagnostic to a per-type profile. The Fisher-Rao geodesic distance provides a reparametrization-invariant and symmetric alternative to the directional KL divergence. Čencov's uniqueness theorem establishes the Fisher information metric as the unique contractive Riemannian metric on the simplex, giving the geometric framework its canonical status.

**Markov categories** (Fritz 2020) provide the correct categorical abstraction for systems with probabilistic morphisms that lack a distribution-over-distributions structure. DR's MaxEnt layer forms a Markov category where the deterministic classifier embeds as the subcategory of deterministic morphisms — capturing compositionality without requiring the Giry monad's multiplication map, which would demand a prior over observer positions.

**Sheaf Laplacians** (Hansen and Ghrist 2019) extend spectral graph theory to cellular sheaves, providing continuous obstruction measures and eigenvalue decompositions. Applied to DR's path-graph site with scalar χ stalks, the sheaf Laplacian confirms the institutional phase transition as the dominant spectral feature (r₂₃² ≈ 70, carrying 97% of spectral weight) and provides a continuous energy measure E(C) complementing the discrete H¹. The cross-model invariance of the eigenvalues (§4.5) establishes the spectral structure as a property of the framework's context geometry.

**Quantum complexity theory.** Yuen's unitary synthesis problem asks whether unlimited classical computational power can simulate quantum state transformations. The classical oracle gap (§4.3) is a corpus-level empirical instance of an analogous structural principle: MaxEnt, which is maximally capable at point-wise context evaluation, detects only 1.7–2.8% of the observer-dependence that cohomological analysis detects at the analytical context, because observer-dependence is relational structure between contexts rather than a property of any individual context. The shared principle — that cross-context coherence is irreducible to within-context power — is **STRUCTURAL**. The disanalogies are significant: Yuen's gap is conjectured, involves formal complexity classes, and concerns an infinitely powerful classical oracle; the DR gap is empirical, concerns diagnostic coverage, and uses a specific finite classifier.

---

## 7. Conclusion

Classification of social structures depends irreducibly on who is observing. This paper has shown that this dependence has formal mathematical structure: it is a presheaf on a site of observer positions, and the standard tools of topos theory — cohomology, descent, naturality — apply and produce quantitative invariants.

The framework's formal spine is the three-way equivalence: Boltzmann factorizability = Lawvere naturality = Grothendieck descent = Noether symmetry conservation. The v4 contribution closes the formal loop: the Boltzmann factorizability test already running in the classification pipeline is proved equivalent to the functor axiom for the presheaf's restriction maps (§2.4), the MaxEnt probabilistic layer is identified as a Markov category rather than an incomplete Giry monad (§3.2), and the sheaf Laplacian spectral structure is established as a framework invariant through cross-model validation (§4.5). The v5 contribution is the convergence-under-inversion result: corpora with opposite input distributions converge to identical framework outputs, establishing the framework's structural properties as fixed-point attractors rather than artifacts of any particular corpus.

Applied to two independently generated corpora (907 and 887 social constraints, produced by different LLMs with inverted input distributions), the framework yields a descent rate of 20–76% and the near-absence of extractive constraints from H⁰. The second finding carries the heavier philosophical weight: extraction is almost never observer-independent because it structurally requires at least one powerful observer position from which it is reclassified as coordination. But a clarification is warranted: the framework does not explain *why* extraction requires perspectival cover — that remains a sociological claim requiring domain theory. What the framework provides is the formal machinery to establish *that* it does, to measure *how much* it does, and to identify the specific coalition structure of the cover. The categorical vocabulary organizes the diagnostic machinery; explanation requires a theory of the domain.

The classical oracle gap — 1.7–2.8% at the analytical context, where MaxEnt is weakest, versus approximately 97% at the institutional context, where the sigmoid sign-flips — establishes that cohomological analysis is not optional. The gap is observer-position-dependent, largest where observer shift is a smooth rescaling and smallest where it is a qualitative transformation. The framework measures the gap at its worst case, making the claim conservative: cohomological analysis adds the most diagnostic value precisely where MaxEnt is weakest.

The paper's strongest empirical result is cross-model structural convergence: two corpora with inverted input distributions — one snare-dominated, the other tangled_rope-dominated — converge to identical sheaf Laplacian eigenvalues, identical superselection gaps, identical institutional dominance signatures, identical confidence band distributions, and identical post-override type populations. These are fixed-point properties of the framework's context geometry, not of any particular corpus. The two-corpus design establishes a sharp partition between topological invariants (framework-stable, corpus-independent) and statistical properties (corpus-dependent, properly reported as ranges).

These numbers are properties of two corpora under one calibration. The framework itself is general. Wherever classification depends on observer position — medical diagnosis varying with clinical perspective, legal interpretation varying with jurisdictional context, risk assessment varying with stakeholder position — the same presheaf construction applies. A different domain would produce a different descent rate, a different H¹ profile, different structural families — but the formal machinery that computes them is domain-independent. The site, the type space, and the metric assignments are domain-specific; the invariants they yield are instances of a general theory.

We close with the open question that the framework itself raises: should the presheaf be sheafified? Sheafification would force descent — it would produce a "consensus classification" where perspectival disagreements are resolved into globally consistent sections. But forcing consensus would destroy the diagnostic signal. The framework's value lies precisely in measuring perspectival fracture — in quantifying the gap between local truth and global truth, and in identifying the structural patterns in that gap. The descent rate, the H¹ distribution, the near-absence of snares from H⁰, the institutional observer as dominant dissenter, the oracle gap, the spectral invariants — these are features of the presheaf's failure to be a sheaf. Sheafification would erase them. The truth of a social system, on this account, is not the consensus but the fracture itself — the specific pattern of who sees what, from where, and the structural geometry that makes their disagreement inevitable rather than accidental.

---

## References

Amari, S., & Nagaoka, H. (2000). *Methods of Information Geometry*. Translations of Mathematical Monographs, vol. 191. American Mathematical Society.

Arrow, K. J. (1951). *Social Choice and Individual Values*. Wiley.

Čencov, N. N. (1982). *Statistical Decision Rules and Optimal Inference*. Translations of Mathematical Monographs, vol. 53. American Mathematical Society.

Döring, A., & Isham, C. J. (2008). "What is a thing?": Topos theory in the foundations of physics. In B. Coecke (Ed.), *New Structures for Physics*, Lecture Notes in Physics, vol. 813, pp. 753–937. Springer.

Fritz, T. (2020). A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics. *Advances in Mathematics*, 370, 107239.

Girard, J.-Y. (1987). Linear logic. *Theoretical Computer Science*, 50(1), 1–102.

Hansen, J., & Ghrist, R. (2019). Toward a spectral theory of cellular sheaves. *Journal of Applied and Computational Topology*, 3, 315–358.

Haraway, D. (1988). Situated knowledges: The science question in feminism and the privilege of partial perspective. *Feminist Studies*, 14(3), 575–599.

Harding, S. (1986). *The Science Question in Feminism*. Cornell University Press.

Isham, C. J., & Butterfield, J. (1998). A topos perspective on the Kochen–Specker theorem: I. Quantum states as generalized valuations. *International Journal of Theoretical Physics*, 37(11), 2669–2733.

Lawvere, F. W. (1969). Adjointness in foundations. *Dialectica*, 23(3–4), 281–296.

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic: A First Introduction to Topos Theory*. Springer.

Noether, E. (1918). Invariante Variationsprobleme. *Nachrichten von der Gesellschaft der Wissenschaften zu Göttingen*, 235–257.

Ostrom, E. (1990). *Governing the Commons: The Evolution of Institutions for Collective Action*. Cambridge University Press.

Sen, A. K. (1970). *Collective Choice and Social Welfare*. Holden-Day.

Wheeler, J. A. (1989). Information, physics, quantum: The search for links. In W. H. Zurek (Ed.), *Complexity, Entropy, and the Physics of Information*, pp. 3–28. Addison-Wesley.

Yuen, H. (2023). A quantum complexity-theoretic reduction for the unitary synthesis problem. arXiv:2306.13073.
