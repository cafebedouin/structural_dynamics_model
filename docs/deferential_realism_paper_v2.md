# Presheaf Classification on a Power-Indexed Site: A Framework for Observer-Dependent Structural Analysis

**Abstract.** We present a formal framework for analyzing classification systems where the result depends irreducibly on the observer's position. The framework models classification as a presheaf on a site of observer contexts, deliberately violating the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved. Applied to a corpus of 1,023 social constraints (laws, norms, institutions), the framework yields quantitative invariants: a descent rate of 0.2072 (the fraction admitting observer-independent classification), a cohomological obstruction measure H¹ capturing the structure of observer disagreement, and 26 structural families of constraints with identical transformation behavior under observer shift. The most philosophically significant finding is that no extractive constraint admits a global section — extraction is never observer-independent. We establish a three-way equivalence between Boltzmann factorizability, Lawvere naturality, and Grothendieck descent as the formal spine of the framework, and provide an honest assessment distinguishing strict categorical correspondences from productive analogies and loose metaphors.

---

## 1. Introduction

The classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects. This is not a failure of classification but a structural feature of the domain: different observer positions have access to different information, different time horizons, different exit options, and different power to act on what they observe.

The standard response to perspectival dependence is to resolve it — to identify the "correct" classification by privileging one observer position or aggregating across positions. This paper takes the opposite approach. We argue that perspectival dependence has formal mathematical structure, that the structure is best captured by presheaf theory, and that the standard tools of topos theory — cohomology, descent, naturality — produce quantitative invariants that characterize any domain where classification depends on perspective.

The framework, which we call *Deferential Realism* (DR), models observer positions as objects of a small category (a *site*), classification at each position as a *presheaf* on that site, and the degree of observer disagreement as *cohomological invariants* of the presheaf. The presheaf is emphatically not a sheaf: the gluing axiom is intentionally violated because perspectival disagreement is a diagnostic signal, not a defect.

Applied to a corpus of 1,023 social constraints with quantitative metrics for extractiveness, suppression, and structural properties, the framework produces three headline findings:

1. **The 20/80 split.** Of 1,023 constraints, 212 (20.7%) admit a global section — a classification that is the same from every observer position. The remaining 811 (79.3%) have irreducible perspectival dependence. Classification that is independent of who is observing is the exception, not the rule.

2. **The absence of snares from H⁰.** No constraint classified as extractive (type "snare") from any observer position is classified as extractive from *every* position. Extraction is never observer-independent. There is always at least one position from which the extraction is invisible — reclassified as legitimate coordination or immutable constraint. The persistence of extraction *requires* this cover story.

3. **The superselection gap.** The distribution of observer disagreement is not continuous. No constraint has exactly 1 or 2 disagreeing observer-pairs (out of 6 possible pairs). When perspectival dependence emerges, it emerges in blocs of at least 3 disagreeing pairs, a consequence of the site's linear power ordering and the classification cascade's threshold geometry.

This paper does not argue that category theory should replace political philosophy, institutional analysis, or any existing approach to understanding social structures. It argues something more specific: that when classification depends on observer position, presheaf theory provides the correct formal framework, cohomology measures the structure of disagreement, and the resulting invariants characterize the domain in ways that informal analysis cannot. The framework is general — it applies wherever classification is perspectival — but the evidence is drawn from the specific domain of social constraints, where the phenomena are vivid and the stakes are clear.

Concretely, given any domain with perspectival classifications, the framework computes: (a) a *descent rate* measuring what fraction of the domain admits observer-independent truth, (b) a *cohomological fracture profile* (the H¹ distribution) characterizing the structure of disagreement — not just that observers disagree but how many disagree and in what pattern, (c) *structural families* of objects with identical transformation behavior under observer shift, including cross-domain twins from unrelated domains that turn out to be structurally isomorphic, and (d) a *coalition lattice* revealing which groups of observers form natural consensus blocs and which observer is the structurally decisive dissenter. These are portable invariants: the formal machinery applies wherever classification depends on who is classifying.

---

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

The choice of site is normative. Different political theories would induce different site factorizations: a Marxist analysis might separate epistemic access from material power as independent morphism dimensions; a liberal framework might make Scope a proper morphism generator rather than a parameter; a feminist standpoint epistemology might add an embodiment axis absent from the current parametrization. The framework is a functor from site choices to invariants — disagreements about what counts as a "standpoint" translate into disagreements about which invariants the framework computes. The current 4-element linear poset is the simplest non-trivial choice, and several of the empirical results depend on its linearity: the H¹ gap at 1 and 2 (§4.2) is a consequence of linear ordering, not a universal property of perspectival dependence. The site is where political commitments enter the mathematics, and the framework makes that entry point explicit rather than hiding it behind a claim of objectivity.

### 2.2 The Presheaf: Classification as Local Truth

The core construction is the classification presheaf. For each constraint *C* in the corpus, we define a contravariant functor:

$$F_C : \mathbf{C}^{op} \to \mathbf{Set}$$

by setting $F_C(U_i) = \text{dr\_type}(C, U_i)$, where dr_type computes the classification of constraint *C* as observed from context $U_i$. The result is an element of the eight-element type space $\Omega$ (defined in §2.3).

This is a presheaf by construction: for each object of the site, it assigns a set (here, a single element of $\Omega$); for each morphism $U_i \to U_j$ in the site, there is an implicit restriction map $F_C(U_j) \to F_C(U_i)$ determined by the classification cascade's response to the change in observer context [STRICT].

The presheaf is **not** a sheaf. The gluing axiom requires that if local sections agree on overlaps, they extend to a global section. The entire diagnostic infrastructure of the framework exists precisely because local sections *fail to glue*: a constraint may classify as "mountain" (immutable natural law) from U₄ and "rope" (legitimate coordination mechanism) from U₃, with no consistent global classification available. This **perspectival gap** is the framework's central diagnostic signal, not a defect to be resolved [STRICT].

**The restriction map.** The mechanism by which classification changes across observer positions is the power-scaling function. Base extractiveness $\varepsilon$ is a context-independent property of the constraint. The experienced extractiveness at a given context is:

$$\chi = \varepsilon \times \sigma(\pi(P)) \times \sigma(S)$$

where $\pi(P)$ is a power index associated with the observer's power level, $\sigma$ denotes a sigmoid scaling, and $S$ contributes a scope modifier. Higher power generally reduces experienced extractiveness (the sigmoid is calibrated so that institutional and analytical observers perceive less extraction), which means the classification presheaf is sensitive to the observer's structural position. The restriction map — the effect of following a morphism $U_i \to U_j$ in the site — is determined by this scaling: moving to a higher-power context generally decreases experienced extractiveness, potentially reclassifying a "snare" as a "rope."

### 2.3 The Type Space

The codomain of the classification presheaf is the eight-element type space:

$$\Omega = \{\text{mountain}, \text{rope}, \text{tangled\_rope}, \text{snare}, \text{scaffold}, \text{piton}, \text{indexically\_opaque}, \text{unknown}\}$$

Each type has a structural interpretation:

- **Mountain**: immutable natural constraint (thermodynamic laws, impossibility theorems). Observer-independent by design.
- **Rope**: legitimate coordination mechanism. Changes the constraint landscape but serves coordination.
- **Tangled rope**: coordination mechanism with embedded extraction. The entanglement of legitimate and extractive functions is irreducible.
- **Snare**: extractive trap. The constraint exists primarily to extract from those subject to it.
- **Scaffold**: temporary coordination mechanism with an explicit sunset clause.
- **Piton**: performative constraint with high theater ratio (more display than substance).
- **Indexically opaque**: contradictory metrics — the constraint cannot be classified from this position.
- **Unknown**: residual category when no classification threshold fires.

The type space carries a binary composition operation defined by priority rules. Importantly, this composition has **two absorbing elements**: both "mountain" and "piton" absorb all other types under composition. A Heyting algebra — the subobject classifier of a topos — requires a unique top element. The presence of two absorbing elements means the type space is a **priority monoid**, not a Heyting algebra (see §5.3). What is present is an associative, idempotent composition with a fallback identity ("unknown") and priority-based conflict resolution — not the implication and complementation structure that a Heyting algebra would provide.

### 2.4 Naturality: The Boltzmann Factorizability Test

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
| Classification factorizes across P × S | Naturality square commutes | Čech cocycle condition | Separability of the Lagrangian |
| Failure detected | Naturality failure witness | Descent obstruction | Broken symmetry |

These are three names for the same mathematical condition [STRICT]. The equivalence holds because naturality (Lawvere), descent (Grothendieck), and symmetry conservation (Noether) are all consequences of invariance of a functor under a group action on its domain. Every other diagnostic layer in the framework — MaxEnt entropy, abductive analysis, trajectory mining, drift detection, cohomological computation — is a different way of measuring proximity to or deviation from this invariance condition.

**Naturality certificates and failure witnesses.** Three derived signatures operationalize the naturality test:

- **Coupling-Invariant Rope (CI Rope)**: passes all four naturality conditions (Boltzmann compliance, scope invariance, absence of nonsensical coupling, no excess extraction). This is a naturality certificate — formal evidence that the classification is well-behaved.
- **False Natural Law (FNL)**: the constraint presents as a natural law (mountain-like metrics) but fails the Boltzmann factorizability test. Cross-index coupling is detected where none should exist. The constraint is "physics-washed" — constructed but disguised as natural. This is a naturality failure witness.
- **False CI Rope (FCR)**: the constraint appears to be a structurally sound rope but fails one or more structural tests. The apparent coordination is itself illusory — a doubly-broken classification.

All three classifications are STRICT: they test well-defined conditions on the naturality square.

### 2.5 Purity and Contamination

Constraints do not exist in isolation. They share agents — individuals or institutions that enforce, benefit from, or are subject to them — creating a network of influence. The **purity score** is a composite measure of a constraint's naturality health, combining four subscores: factorizability (Power × Scope independence), scope invariance, coupling cleanliness, and excess extraction above a thermodynamic floor. The score ranges from 0.0 (complete naturality failure) to 1.0 (perfect naturality).

The **contamination network** propagates purity through the agent-sharing graph. A high-purity constraint (structurally clean coordination) that shares an agent with a low-purity constraint (contaminated extraction) has its effective purity reduced. Crucially, contamination flows *against* the purity gradient — from lower-purity to higher-purity constraints — giving the propagation a contravariant character. Mountains are immune to contamination (immunity = 0.0); ropes are fully susceptible (STRUCTURAL).

The **Fixed-Point Network (FPN)** computes the equilibrium distribution of effective purities by iterating the contamination operator until convergence. The operator is monotone (contamination is non-decreasing) and bounded below (purity is floored at 0.0), so by the Knaster-Tarski theorem, iteration from intrinsic purities converges to a greatest fixed point. In coalgebraic terms, this equilibrium is the terminal coalgebra of the contamination endofunctor on the purity lattice — a suggestive coalgebraic reading, though the operator itself is just an order-theoretic monotone map on a finite lattice (STRUCTURAL; see §5.2).

### 2.6 Corpus Provenance and Calibration

The empirical results reported in this paper are drawn from a corpus of 1,023 social constraints — laws, norms, institutions, regulatory mechanisms — hand-constructed as a prototype and proof of concept. The continuous metrics assigned to each constraint (extractiveness ε, suppression σ, theater ratio θ, resistance-to-change ρ) are author-assigned based on domain analysis. They have not been subjected to inter-rater reliability testing. A hostile reader could reasonably ask: how much of the output is a property of the formal framework, and how much is an artifact of the calibration?

The answer has two parts. First, the formal framework — presheaf structure, naturality testing, cohomological computation, Galois connection — is independent of the particular corpus and its metric assignments. A different corpus with different metrics would produce different empirical numbers (a different descent rate, a different H¹ distribution, different structural families) but the same formal machinery would apply. The framework's contribution is the diagnostic vocabulary, not the specific invariants computed for this corpus.

Second, a configuration sensitivity sweep provides direct evidence of calibration robustness. All 118 numeric parameters governing classification thresholds, power modifiers, and coupling gates were perturbed at ±10% and ±25%, with each of the 472 resulting configurations running the full 733-test validation suite. Of the 118 parameters, 103 (87%) were inert at ±25% perturbation — the classification output did not change. Eight parameters were moderate: stable at ±10% but breaking at ±25%, including the Boltzmann coupling threshold (current value 0.25, floor approximately 0.19–0.22). Of the seven parameters initially flagged as critical, six were reclassified upon investigation as timeout artifacts or integer-rounding effects (continuous perturbation of discrete parameters that rounded back to the original value), leaving one genuinely critical parameter: the analytical power modifier, a structural constant that scales every experienced-extractiveness calculation. The system's stability is a consequence of design rather than luck: the critical parameter is load-bearing by construction, not by calibration fragility.

The empirical findings reported in §4 — the 20/80 split, the absence of snares from H⁰, the institutional observer as dominant dissenter — are properties of this corpus under this calibration. The formal framework's value lies in providing the diagnostic machinery to compute such invariants for any corpus.

---

## 3. The Diagnostic Layers

The framework is not a single classifier but a stack of five diagnostic layers, each providing a different view of the same underlying presheaf structure. The classification presheaf provides the foundation; the diagnostic layers measure its properties.

### 3.1 Gauge Orbits (Dirac Classification)

The **gauge orbit** of a constraint is the complete set of (type, context) pairs across all standard contexts — the full evaluation of the presheaf at every object of the site. Two constraints with identical orbits exhibit the same perspectival profile; they are, from the presheaf-theoretic viewpoint, sections of the same functor.

The orbit decomposition classifies constraints by the structure of their perspectival variation:

- **Singleton orbits** (one type across all contexts): gauge-invariant constraints. Classification is observer-independent. These are the global sections, the constraints satisfying descent.
- **Multi-type orbits**: constraints with perspectival dependence. The Dirac classification module further distinguishes first-class constraints (where the type change represents a genuine shift in structural character as power changes) from second-class constraints (where the type change reflects a classification artifact) [STRICT — the orbit computation under a finite group is standard].

The orbit is the constraint's identity in the presheaf topos. This implements Grothendieck's relative viewpoint: a constraint IS its behavior under observer shifts, not its classification at any single context.

### 3.2 Probabilistic Shadow (MaxEnt Classification)

The deterministic classifier assigns a single type at each context. The Maximum Entropy (MaxEnt) shadow classifier runs alongside it and assigns a *probability distribution* over all types. Where the deterministic classifier evaluates the presheaf, the MaxEnt classifier evaluates a probabilistic extension — for each constraint-context pair, it assigns a point in the probability simplex $\Delta^5$.

The construction uses Gaussian log-likelihoods for each type based on the constraint's metrics, combined with prior weights and boolean features, normalized via log-sum-exp to a proper distribution. The Shannon entropy of this distribution, normalized by $\log(6)$, measures classification uncertainty: 0 means the probabilistic and deterministic classifiers agree completely; 1 means classification is maximally uncertain.

In Wheeler's interpretive framework, the MaxEnt classifier performs "soft measurement" — probabilistic determination that has not yet collapsed to a definite classification. Constraints with high MaxEnt entropy sit near classification boundaries; the "measurement" (observer position) has not provided enough information to fix a single type [STRUCTURAL — the distribution on $\Omega$ is genuine, but the full Giry monad structure is incomplete].

The diagnostic value of the MaxEnt layer lies in disagreement detection. Three levels of disagreement are tracked:

- **Hard disagreement**: the probabilistic top type differs from the deterministic classification. The two classifiers see different constraints.
- **Soft disagreement**: the top types agree, but the probabilistic confidence is low (the second-ranked type has nearly as much probability mass). The deterministic classifier commits to a type that the probabilistic classifier considers marginal.
- **Entropy flag**: the normalized Shannon entropy exceeds a threshold, indicating that probability mass is spread across multiple types. The constraint sits near a multi-way classification boundary.

Hard disagreements are the strongest diagnostic signal; entropy flags are the most common. Together, they identify constraints where the presheaf evaluation is sensitive to small perturbations in the underlying metrics — the constraints most likely to reclassify under metric revision or site enrichment.

### 3.3 Abductive Synthesis

The abductive engine synthesizes signals from all other subsystems into structured hypotheses. Its 8 trigger classes function as cross-functor consistency checks: each trigger examines whether two or more independent diagnostic views of a constraint agree, and generates a hypothesis when they disagree [STRUCTURAL].

The critical distinction is between **artifacts** and **genuine findings**. An artifact is an expected disagreement — the deterministic classifier intentionally overrides the metric classification via a structural signature, so the MaxEnt model (which does not know about overrides) naturally disagrees. A genuine finding is an unexpected disagreement, revealing structural anomalies that no single diagnostic layer would detect.

The corpus yields six classes of genuine findings:

1. **Deep deception**: a constraint passes profile tests for natural law (mountain) but fails Boltzmann compliance. The MaxEnt classifier independently confirms mountain-like metrics. The constraint is physics-washed — it looks natural from every diagnostic angle except the factorizability test that detects hidden cross-index coupling.
2. **Metric-structural divergence**: high MaxEnt entropy (the metrics are ambiguous) but a stable Dirac orbit (the discrete classification is consistent). The constraint sits on a classification boundary but has not yet crossed it.
3. **Confirmed liminal**: three independent diagnostics (MaxEnt, Dirac, drift) all detect transition. The constraint is actively shifting type.
4. **Coverage gap**: the Dirac orbit detects perspectival variation that the mismatch detector (which relies on stored facts) misses. A discrepancy between on-the-fly computation and cached data.
5. **Accelerating pathology**: the FPN equilibrium and temporal drift agree that contamination is worsening. A dynamic finding about trajectory, not just state.
6. **Dormant extraction**: clean metrics but extractive structural voids. The constraint passes surface-level tests but has the fingerprint pattern of extraction without the metric signature — a sleeper.

### 3.4 Trajectory Mining and Structural Families

A constraint's **trajectory** is its complete presheaf evaluation enriched with continuous diagnostics: for each of the four standard contexts, the trajectory records the type, experienced extractiveness $\chi$, MaxEnt entropy, and classification confidence. Where the gauge orbit records only the discrete type at each context, the trajectory captures the full quantitative profile.

The **trajectory distance** between two constraints is a weighted 4-component metric:

| Component | Weight | What it measures |
|-----------|--------|-----------------|
| Shift distance | 0.35 | Agreement of type profiles across contexts |
| Metric distance | 0.25 | Similarity of continuous metrics ($\chi$, entropy) |
| Stability distance | 0.25 | Similarity of purity, coupling, and naturality health |
| Pathology distance | 0.15 | Similarity of drift counts and contamination status |

Hierarchical agglomerative clustering (HAC) with average linkage groups constraints into **structural families** — equivalence classes under trajectory similarity. A two-stage approach is used: first, constraints are grouped by their discrete shift pattern (the ordered type profile across contexts, yielding 36 groups); then HAC is applied within each shift group using the continuous metric components. This is semantically sound because constraints with different shift patterns have high shift-distance by definition, so they would rarely merge before the clustering cut level.

The corpus yields 26 structural families from 24 orbit families, with 8 of 24 orbit families (33%) split by the continuous-metric resolution. The largest family contains 315 constraints sharing the shift pattern [snare, snare, rope, snare]; the smallest families are 4 singletons with unique metric profiles.

The most striking product of trajectory mining is **cross-domain twins**: constraints from entirely unrelated domains that belong to the same structural family because they transform identically under observer shifts. A tax regulation and a technology governance mechanism, originating in different legal systems and serving different functions, may be structurally isomorphic — invisible from any single observer position, visible only by examining how both vary across contexts. This is Grothendieck's relative viewpoint implemented as computation: identity is determined by behavior under morphisms, not by intrinsic properties [STRUCTURAL].

---

## 4. Cohomological Results

This section presents the empirical core of the framework. Every number traces to the corpus computation over 1,023 constraints with extractiveness metrics.

### 4.1 H⁰: Global Sections

The zeroth Čech cohomology $H^0(\mathcal{U}, F_C)$ counts the global sections of the classification presheaf — constraints whose classification is the same from every observer position. A global section exists if and only if the gauge orbit is a singleton: all four standard contexts agree on the type.

**Result.** Of 1,023 constraints, 212 admit a global section (descent rate = 0.2072). The breakdown by type:

| Type | Count | % of H⁰ |
|------|-------|----------|
| tangled_rope | 143 | 67.5% |
| rope | 34 | 16.0% |
| mountain | 21 | 9.9% |
| scaffold | 14 | 6.6% |
| snare | **0** | **0.0%** |

The 21 mountains in H⁰ are the genuine natural laws of the corpus: thermodynamic constraints, impossibility theorems (Arrow's, Gödel's), information-theoretic limits (Chaitin's omega, halting problem). These constraints are context-invariant by their nature — the second law of thermodynamics does not depend on who is observing.

The 34 ropes are universally recognized coordination mechanisms: standard protocols (TCP/IP, metric system), well-established legal frameworks. The 14 scaffolds are temporary coordination mechanisms universally recognized as temporary — they have sunset clauses, and sunset clauses are context-invariant properties.

The largest H⁰ group is the 143 tangled ropes: constraints where the entanglement of coordination and extraction is visible from every observer position. These constraints descend — every observer sees them as tangled — but the *structure* of the tangling may vary across the Power × Scope grid. Descent tests consistency of the type; naturality tests consistency of the mechanism.

**The absence of snares.** No extractive constraint appears in H⁰. This is the most philosophically significant finding of the cohomological computation. It means that extraction is *never* observer-independent: every constraint that any observer classifies as a snare is classified differently by at least one other observer. In presheaf-theoretic terms, "this constraint is extractive" is never a global section — it is always a local truth that fails to glue.

The mechanism is structural, not accidental. Extraction persists because it has a cover story. A constraint that appears as a snare from the powerless context (U₁) may appear as legitimate coordination (rope) from the institutional context (U₃), where the observer has the generational time horizon and arbitrage exit options that make the extractive features invisible or irrelevant. If every observer could see the snare from every position, the constraint would face reform pressure from all directions. The persistence of extraction *requires* descent failure — it requires at least one powerful observer position from which the extraction is reclassified as something benign.

### 4.2 H¹: Perspectival Fracture

For each constraint, the H¹ proxy counts the number of disagreeing context-pairs among the $\binom{4}{2} = 6$ unordered pairs of standard contexts. A pair $(U_i, U_j)$ disagrees when $F_C(U_i) \neq F_C(U_j)$. The distribution across the corpus:

| H¹ | Count | % of corpus |
|----|-------|-------------|
| 0 | 212 | 20.7% |
| 1 | 0 | 0.0% |
| 2 | 0 | 0.0% |
| 3 | 515 | 50.3% |
| 4 | 52 | 5.1% |
| 5 | 240 | 23.5% |
| 6 | 4 | 0.4% |

**The gap at H¹ = 1, 2.** No constraint has exactly 1 or 2 disagreeing pairs. This is a structural consequence of the site's linear ordering and the classification cascade's threshold geometry, not an empirical accident. The four standard contexts are ordered on a one-dimensional power axis, and the classification cascade uses fixed thresholds on continuous metrics scaled by a monotone sigmoid of power. When a constraint's experienced extractiveness crosses a classification threshold, it crosses at a single power boundary — say, between U₂ and U₃. But because the power ordering is linear, this creates a 3+1 split (three contexts on one side, one on the other) or a 2+2 split, never a 1+3+0 or other configuration that would produce exactly 1 or 2 disagreeing pairs. A single threshold transition on a linearly ordered 4-element site generates exactly 3 or 4 disagreeing pairs — never 1 or 2.

This gap is a property of the measurement apparatus (the site geometry), not of the constraints. A richer site with non-linear power relationships could in principle produce H¹ = 1 or 2. The gap and its mechanism are formally derived from the site geometry [STRICT]. The superselection analogy is productive — both prohibit certain state combinations as a consequence of the ambient structure rather than of any particular state — but imports Hilbert-space connotations the framework does not support [STRUCTURAL].

On a non-linear site — for instance, a branching poset where U₂ and U₃ are incomparable rather than ordered — the same classification cascade would generically produce nonzero mass at H¹ = 1 and H¹ = 2. With two incomparable mid-level contexts, a single threshold crossing could separate one context from the other three, producing exactly one disagreeing pair. The gap is therefore diagnostic of how power is organized in the model (linearly, as a chain), not merely that perspectives differ. This gives practitioners a clear interpretive lever: if the domain under study lacks a linear power hierarchy, expect qualitatively different disagreement geometry.

**The dominant mode.** H¹ = 3 accounts for 50.3% of the corpus. The most common pattern is [snare, snare, rope, snare] — three contexts see extraction, but the institutional observer (U₃), with generational time horizon and arbitrage exit options, sees legitimate coordination. The institutional observer is the dominant dissenter.

**Maximal obstruction.** Four constraints achieve H¹ = 6 (all pairs disagree). All four share the orbit [indexically_opaque, tangled_rope, rope, snare] — four distinct types, one per context. These are constraints where power level completely determines what you see: the powerless observer cannot even classify the constraint (indexically opaque); the moderate observer sees entanglement; the institutional observer sees coordination; the analyst sees extraction. They represent the limiting case of perspectival dependence: `ai_performance_watermark`, `openai_prism_development`, `semiconductor_fabrication_chokepoint`, `us_usmca_china_leverage`.

**H¹ is constant within orbit families.** A key structural observation: within any given orbit family (constraints sharing the same set of types in their orbit), H¹ is constant. This is a consequence of the site structure — the number of disagreeing pairs is fully determined by the set of distinct types and how they distribute across the four canonical positions. The largest orbit families with their H¹ values:

| Orbit family | H¹ | Count |
|---|---|---|
| {rope, snare} | 3 | 315 |
| {rope, snare, tangled_rope} | 5 | 164 |
| {tangled_rope} | 0 | 143 |
| {rope, tangled_rope} | 3 | 135 |
| {mountain, unknown} | 4 | 50 |

This means H¹ does not add information *within* orbit families; its value is a function of the orbit type-set. What H¹ adds is a graduated numerical measure that allows comparison *across* families — the {rope, snare} family (H¹ = 3) is less fractured than {rope, snare, tangled_rope} (H¹ = 5).

### 4.3 Descent Rate and Corpus Invariants

The **descent rate** — the fraction of the corpus admitting global sections — is 0.2072. This single number characterizes the corpus: roughly four in five social constraints have irreducible perspectival dependence.

**Mean H¹ by analytical-context type** reveals a gradient:

| Analytical type | Mean H¹ | N |
|----------------|---------|---|
| scaffold | 0.00 | 14 |
| rope | 1.04 | 130 |
| tangled_rope | 1.78 | 412 |
| mountain | 2.82 | 71 |
| snare | 3.75 | 272 |
| unknown | 3.85 | 124 |

Scaffolds always descend (mean H¹ = 0). Ropes mostly descend (mean H¹ = 1.04). Snares have high fracture (mean H¹ = 3.75). The ordering is structurally informative: temporary coordination is universally recognized as temporary; extraction mechanisms look different depending on who is observing.

The surprising finding is that mountains have non-trivial mean H¹ (2.82). Many constraints classified as mountains from the analytical perspective are *not* classified as mountains from other perspectives — these are "contested mountains," constraints where the analyst's assessment of immutability is not shared by other observers.

**H¹ versus purity.** There is a negative correlation between H¹ and purity score:

| H¹ | Mean purity | N |
|----|------------|---|
| 0 | 0.713 | 155 |
| 3 | 0.526 | 486 |
| 4 | 0.975 | 51 |
| 5 | 0.342 | 233 |
| 6 | 0.348 | 4 |

The general trend confirms that cohomological fracture and naturality health measure related phenomena: higher perspectival obstruction correlates with lower structural purity. The anomaly at H¹ = 4 (mean purity 0.975) is explained by the orbit pattern [mountain, unknown, unknown, mountain]: these constraints are classified as mountain from U₁ and U₄ but unknown from U₂ and U₃, and mountains have purity ≈ 1.0 because they are immune to contamination. This reveals that purity and cohomology measure different things: purity measures structural health *at a single context*, while cohomology measures *cross-context consistency*.

### 4.4 Coalition Structure

The Galois connection between observer coalitions and consensus types provides a finer invariant than H¹ alone. For a given constraint, define the **agreement set** for type *T* as the set of contexts that classify the constraint as *T*, and the **consensus** of a coalition *S* as the type (if any) that all members of *S* agree on. These two maps form an antitone Galois connection between the lattice of observer coalitions and the lattice of types.

The Galois lattice per constraint captures the *structure* of observer agreement — not just whether observers disagree but *which groups form natural consensus blocs*. Two constraints with the same H¹ can have different Galois lattices: [snare, snare, rope, snare] (H¹ = 3, one dissenter against a bloc) has a qualitatively different politics of disagreement than a hypothetical [rope, snare, snare, rope] (H¹ = 4, two equal-sized blocs).

**The institutional observer as dominant dissenter.** Aggregating Galois-closed coalitions across the corpus reveals a pattern: the institutional observer (U₃) is the most frequent isolated dissenter. The data from the largest orbit families:

| Orbit family | Count | Isolated observer | Coalition bloc |
|---|---|---|---|
| {rope, snare} | 315 | U₃ (institutional) | {U₁, U₂, U₄} |
| {rope, tangled_rope} | 135 | U₃ (institutional) | {U₁, U₂, U₄} |
| {mountain, unknown} | 50 | U₂, U₃ | {U₁, U₄} |

In the two largest families — together comprising roughly 44% of the corpus — the institutional observer is the lone dissenter, seeing coordination (rope) where all other observers see extraction (snare) or entanglement (tangled_rope). This is the Galois expression of a central claim: institutional power is the structurally decisive perspective. The institutional observer, with generational time horizon and arbitrage exit options, systematically reclassifies extraction as coordination. This is not a bias to be corrected; it is a structural consequence of the institutional observer's position. From a position with the power to reform a constraint, the constraint genuinely *functions* as coordination — the extractive features are either invisible (the observer benefits from them) or irrelevant (the observer can exit them). The classification is correct *from that position*. It is incorrect from others. (The lattice computation is STRICT; the corpus-level pattern is an empirical finding.)

The **splitting degree** — the minimum number of observers needed to fully determine a constraint's type profile — provides a measure of observer redundancy not captured by H¹ or orbit families. A constraint with splitting degree 1 can be fully characterized from any single observer position; a constraint with splitting degree 4 requires every observer to contribute unique information. The splitting degree and H¹ are related but not identical: H¹ counts disagreeing pairs (a function of the orbit type-set), while splitting degree measures the minimum observers needed to reconstruct the full type profile (a function of the shift pattern's frequency in the corpus).

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
- **Naturality.** The Boltzmann factorizability test is a genuine naturality condition. The factorization test $\chi(P, S) \approx f(P) \times g(S)$ is exactly the commutativity of a naturality square on the Power × Scope grid.
- **Naturality witnesses.** FNL (false natural law) is a genuine naturality failure witness; CI Rope is a genuine naturality certificate. Both test well-defined conditions on the naturality square.
- **H⁰ and descent.** Global sections (H⁰ = 212) are precisely the constraints satisfying the descent condition. Descent $\leftrightarrow$ H¹ = 0 is tautological on discrete covers.
- **Gauge orbits.** The Dirac orbit computation is standard orbit decomposition under the group of context automorphisms.
- **The three-way equivalence.** Boltzmann compliance = Lawvere naturality = Grothendieck descent = Noether symmetry conservation. These are three names for invariance of a functor under a group action.
- **The Galois connection.** The coalition–consensus duality is a standard antitone Galois connection between two finite posets.

### 5.2 What Is STRUCTURAL

The following correspondences guide analysis productively but lack formal verification:

- **MaxEnt as distribution on $\Omega$.** The MaxEnt classifier assigns a probability distribution over the type space at each context, which is structurally analogous to the Giry monad's action. But the full Giry monad structure — unit (Dirac embedding), multiplication (distribution over distributions), naturality of both — is not present.
- **FPN as terminal coalgebra.** The FPN equilibrium is the greatest fixed point of the contamination endofunctor, and convergence is proved via Knaster-Tarski. But the full coalgebra axioms have not been formally verified.
- **Abductive engine as naturality auditor.** The 8 trigger classes test cross-functor consistency — whether independent diagnostic views agree. The artifact/genuine distinction maps cleanly onto expected versus unexpected naturality failures. But the triggers are hand-crafted, not derived from formal categorical constructions.
- **Trajectories as natural transformation families.** Constraints with identical trajectories exhibit the same transformation behavior under context shifts, functioning as representatives of the same natural transformation. But the formal functor-category construction is absent.
- **Contamination as contravariant flow.** Contamination flows against the purity gradient, which is structurally analogous to contravariance. But the gradient reversal is of a scalar, not of categorical morphisms.
- **H¹ proxy.** The disagreeing-pairs count coincides with formal Čech H¹ on discrete sites but is not the full quotient ker($\delta^1$)/im($\delta^0$) in general.

### 5.3 What Is LOOSE

The following analogies would mislead if taken literally:

- **Type space as Heyting algebra.** Two absorbing elements (mountain and piton) prevent lattice structure. The composition is a priority monoid. A Heyting algebra claim would invite incorrect expectations about implication and complementation.
- **Power scaling as adjunction.** The sigmoid scaling creates a parametric family of type assignments, but the triangle identities (unit and counit) have not been verified. The existential/universal quantifier structure is suggestive but insufficient.
- **Signature resolution as lattice meet.** The conflict resolution predicate is a priority dispatch table lacking commutativity and associativity. It is not a lattice operation.
- **All five Girard/Linear Logic mappings.** An independent audit found that all five proposed correspondences between the codebase and Girard's linear logic are LOOSE. The systematic error across all five is the same: conflating *computing a quantity that describes a resource* with *consuming that resource*. The system is a calculator, not a cash register — it tracks costs but does not enforce budgets, which is exactly the gap identified in §5.4.
- **The quantum measurement analogy.** The presheaf structure is formally analogous to contextual truth in quantum topos theory (Isham and Butterfield 1998), but the analogy breaks at three critical points: DR classification is reversible (re-evaluate at a different context freely), deterministic (no Born rule), and local (no entanglement). The analogy is useful for intuition but actively misleading for formalism.

### 5.4 What the Framework Cannot Do

The framework classifies and diagnoses. It does not:

- **Plan under resource constraints.** The system computes costs (purity-adjusted energy for reform actions, scaffold urgency scores) but does not enforce budgets. There is no concept of finite enforcement capacity, agent attention, or reform allocation. This is the genuine gap identified by the Girard/Linear Logic analysis — the system would need an ontological expansion (resource annotations on agents) and a new operational layer to close it.
- **Perform metric-level sensitivity analysis.** The configuration sensitivity sweep (§2.6) tests parameter robustness — whether the classification cascade's thresholds and modifiers are stable under perturbation. What it does not test is the robustness of the input metrics themselves: do small changes in the author-assigned extractiveness, suppression, or resistance-to-change values cause constraints to reclassify? A constraint near a type boundary might shift type with plausible metric perturbations. This metric-level sensitivity analysis remains to be performed.
- **Extend to infinite or non-linear sites.** The current site is a 4-element linear poset. Extending to a richer site — adding temporal morphisms, scope morphisms as independent dimensions, or non-linear power relationships — would require non-trivial formal work. In particular, H² and higher cohomology become non-trivial on non-discrete sites, and the H¹ gap at 1 and 2 would not persist on a non-linearly ordered site.
- **Establish causation.** The framework detects structural patterns — which constraints are perspectivally fractured, which observer positions are structurally decisive, which constraints are structurally isomorphic across domains. It does not establish *why* a constraint is extractive, *how* extraction emerged, or *whether* reform would succeed. These are causal questions that require different methods.

### 5.5 What Would Strengthen the Framework

Several well-defined formal tasks remain:

1. **Formal verification of restriction maps.** Prove that the power-scaling function satisfies the functor axioms for the presheaf's restriction maps. This requires showing that the composition of scalings along a chain $U_1 \to U_2 \to U_3$ equals the direct scaling $U_1 \to U_3$.
2. **Verification of the monoidal structure.** Determine the precise algebraic structure of the type space under composition. Is the priority monoid a bounded semilattice when the piton anomaly is addressed?
3. **Metric-level sensitivity analysis.** The configuration parameter sweep (§2.6) establishes that 87% of internal parameters are inert at ±25% perturbation. The remaining gap is metric-level sensitivity: systematically varying the input metrics (extractiveness, suppression, resistance-to-change) within plausible ranges to measure how many constraints reclassify. This would quantify robustness at the input layer rather than the parameter layer.
4. **Extension to enriched sites.** Adding temporal and scope dimensions as independent morphisms would create a product site with non-trivial higher cohomology. H² would measure whether patterns of perspectival disagreement are themselves perspectival — a second-order invariant.
5. **The sheafification question.** The presheaf $F$ can be sheafified to produce the "closest" sheaf $F^+$, for any Grothendieck topology $J$ on the site. Sheafification forces descent — it produces a consensus classification by resolving perspectival disagreements into globally consistent sections. The kernel and cokernel of the sheafification map $F \to F^+$ would quantify how much perspectival information is lost by demanding global truth. For the 212 constraints already in H⁰, sheafification changes nothing; for the 811 with H¹ > 0, it would force a choice that the presheaf currently refuses to make. The question is whether the forced consensus is informative (revealing what "the truth" would be if we demanded one) or destructive (erasing the diagnostic signal). For an analyst, the answer likely depends on the application: consensus classification may be useful for policy recommendation, while presheaf classification is necessary for structural diagnosis.

   A worked example clarifies the stakes. Consider the dominant orbit [snare, snare, rope, snare] (315 constraints, H¹ = 3). The institutional observer (U₃) sees coordination; the other three see extraction. Under topology J₁, where {U₁, U₂, U₄} is a covering family (majority rule), sheafification forces "snare" as the global type — three observers override one, and the consensus is that the constraint is extractive. Under topology J₂, where {U₃} alone is a covering family (institutional authority), sheafification forces "rope" — the institutional perspective, with its generational time horizon and arbitrage exit options, overrides all others. The choice of Grothendieck topology IS the choice of whose perspective is definitive. J₁ is democracy; J₂ is institutional authority; and the formal framework makes the consequences of each choice computable. The DR framework's decision to remain a presheaf — to refuse sheafification — is the decision to preserve the diagnostic signal of disagreement rather than force a consensus that would erase the very structure the framework exists to measure.

---

## 6. Related Work

The framework sits at the intersection of several traditions. Positioning it precisely — what it borrows, what it adds, what it lacks — is essential for honest scholarship.

**Standpoint epistemology** (Harding 1986; Haraway 1988) argues that knowledge is perspectival — that the social position of the knower shapes what can be known. DR formalizes this claim by providing a presheaf-theoretic framework where the "standpoint" is a point of the site and the "knowledge" is the stalk of the presheaf at that point. The formalization adds what standpoint epistemology lacks: quantitative invariants. The descent rate measures *how much* of a domain is perspectival. The H¹ distribution measures the *structure* of perspectival dependence. The Galois lattice identifies *which* standpoints are structurally decisive. What DR lacks is standpoint epistemology's rich account of how standpoints are constituted — the site's four standard contexts are stipulated, not derived from social theory.

**Social choice theory** (Arrow 1951; Sen 1970) studies the aggregation of individual preferences into collective decisions. DR's type space has some structure in common with preference aggregation — the composition operation resolves conflicts between types — but the type space is not a preference ordering and the composition is not a social welfare function. Arrow's impossibility theorem shows that no aggregation rule satisfies a set of desirable axioms; DR's analogue is the absence of snares from H⁰, which shows that no observer position universalizes the extractive classification. The formal structures are different (preference lattices vs. presheaves on sites), but the impossibility results are spiritually related.

**Institutional analysis** (Ostrom 1990) classifies institutions by their rules, boundaries, and governance structures. DR classifies constraints by their structural character as seen from different observer positions. The two approaches are complementary: Ostrom asks "what kind of institution is this?" from a single analytical perspective; DR asks "what kind of institution does this look like to different observers?" Ostrom's framework could serve as the institutional semantics that DR currently lacks — providing rich descriptions of the constraints that DR classifies structurally.

**Topos-theoretic approaches in physics** (Isham and Butterfield 1998; Döring and Isham 2008) apply presheaf theory to quantum mechanics, modeling contextual truth — the idea that the truth value of a proposition depends on the measurement context. DR applies the same formal structure to a different domain: the "measurement context" is an observer's social position rather than an experimental apparatus, and the "proposition" is a constraint classification rather than a quantum observable. The formal parallels are genuine (both are presheaves on sites of contexts), and the philosophical parallels are instructive (both formalize the idea that truth is local to a context). The key disanalogy is that quantum measurement involves irreversibility (wave-function collapse), stochasticity (Born rule), and entanglement (non-local correlations), none of which are present in DR.

**Wheeler's participatory universe** (Wheeler 1989) posits that physical reality is constituted by information and that the observer's choice of measurement apparatus determines the observed reality. DR's formal structure matches Wheeler's thesis at the level of presheaf evaluation: the "apparatus" is the observer context, the "measurement" is classification, and the "reality" depends on the choice. The 20/80 split is the quantitative answer to Wheeler's question — how much of a domain requires an observer to determine the facts? — for the domain of social constraints. The disanalogy is equally informative: Wheeler insists on *free choice* of measurement, while DR has *constrained positionality* (a powerless observer does not *choose* to see a snare; they see it because their structural position constrains their perspective). This identifies exactly where DR formalizes participatory observation and where it formalizes standpoint epistemology.

**Computational social science** typically treats classification as a supervised learning problem: given labeled training data, learn a classifier that generalizes. DR takes a fundamentally different approach: classification is not learned from data but computed from continuous metrics via a hand-designed, deterministic rule cascade (not a trained classifier), and the central question is not "which label is correct?" but "how does the label depend on who is labeling?" The framework is computational — it is implemented as a Prolog codebase that runs on corpora of over 1,000 constraints — but it is not machine learning. Its invariants (descent rate, H¹ distribution, structural families) are formal properties of the classification presheaf, not performance metrics of a learned model.

---

## 7. Conclusion

Classification of social structures depends irreducibly on who is observing. This paper has shown that this dependence has formal mathematical structure: it is a presheaf on a site of observer positions, and the standard tools of topos theory — cohomology, descent, naturality — apply and produce quantitative invariants.

The framework's formal spine is the three-way equivalence: Boltzmann factorizability = Lawvere naturality = Grothendieck descent = Noether symmetry conservation. Every diagnostic layer — MaxEnt entropy, abductive analysis, trajectory mining, cohomological computation — measures proximity to or deviation from this invariance condition, which connects the framework's most rigorous computational test to three of the deepest ideas in twentieth-century mathematics.

Applied to the social-constraints corpus, the framework yields a descent rate of 0.2072 and the complete absence of extractive constraints from H⁰. The second finding carries the heavier philosophical weight: extraction is never observer-independent because it structurally requires at least one powerful observer position from which it is reclassified as coordination. But a clarification is warranted: the framework does not explain *why* extraction requires perspectival cover — that remains a sociological claim requiring domain theory. What the framework provides is the formal machinery to establish *that* it does, to measure *how much* it does, and to identify the specific coalition structure of the cover. The categorical vocabulary organizes the diagnostic machinery; explanation requires a theory of the domain.

These numbers are properties of one corpus under one calibration. The framework itself is general. Wherever classification depends on observer position — medical diagnosis varying with clinical perspective, legal interpretation varying with jurisdictional context, risk assessment varying with stakeholder position — the same presheaf construction applies. A different domain would produce a different descent rate, a different H¹ profile, different structural families — but the formal machinery that computes them is domain-independent. The site, the type space, and the metric assignments are domain-specific; the invariants they yield are instances of a general theory.

We close with the open question that the framework itself raises: should the presheaf be sheafified? Sheafification would force descent — it would produce a "consensus classification" where perspectival disagreements are resolved into globally consistent sections. This is a well-defined mathematical operation (for any Grothendieck topology on the site, there exists a unique closest sheaf). But forcing consensus would destroy the diagnostic signal. The framework's value lies precisely in measuring perspectival fracture — in quantifying the gap between local truth and global truth, and in identifying the structural patterns in that gap. The descent rate, the H¹ distribution, the absence of snares from H⁰, the institutional observer as dominant dissenter — these are features of the presheaf's failure to be a sheaf. Sheafification would erase them. The framework measures perspectival fracture, not to resolve it, but because the fracture itself is where the structural information lives.

---

## References

Arrow, K. J. (1951). *Social Choice and Individual Values*. Wiley.

Döring, A., & Isham, C. J. (2008). "What is a thing?": Topos theory in the foundations of physics. In B. Coecke (Ed.), *New Structures for Physics*, Lecture Notes in Physics, vol. 813, pp. 753–937. Springer.

Girard, J.-Y. (1987). Linear logic. *Theoretical Computer Science*, 50(1), 1–102.

Haraway, D. (1988). Situated knowledges: The science question in feminism and the privilege of partial perspective. *Feminist Studies*, 14(3), 575–599.

Harding, S. (1986). *The Science Question in Feminism*. Cornell University Press.

Isham, C. J., & Butterfield, J. (1998). A topos perspective on the Kochen–Specker theorem: I. Quantum states as generalized valuations. *International Journal of Theoretical Physics*, 37(11), 2669–2733.

Lawvere, F. W. (1969). Adjointness in foundations. *Dialectica*, 23(3–4), 281–296.

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic: A First Introduction to Topos Theory*. Springer.

Noether, E. (1918). Invariante Variationsprobleme. *Nachrichten von der Gesellschaft der Wissenschaften zu Göttingen*, 235–257.

Ostrom, E. (1990). *Governing the Commons: The Evolution of Institutions for Collective Action*. Cambridge University Press.

Sen, A. K. (1970). *Collective Choice and Social Welfare*. Holden-Day.

Wheeler, J. A. (1989). Information, physics, quantum: The search for links. In W. H. Zurek (Ed.), *Complexity, Entropy, and the Physics of Information*, pp. 3–28. Addison-Wesley.
