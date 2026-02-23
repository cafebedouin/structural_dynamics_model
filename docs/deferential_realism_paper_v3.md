# Presheaf Classification on a Power-Indexed Site: A Framework for Observer-Dependent Structural Analysis (v3)

**Abstract.** We present a formal framework for analyzing classification systems where the result depends irreducibly on the observer's position. The framework models classification as a presheaf on a site of observer contexts, deliberately violating the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved. Applied to a corpus of 1,142 social constraints (laws, norms, institutions), the framework yields quantitative invariants: a descent rate of [COMPUTE: H⁰/1142] (the fraction admitting observer-independent classification), a cohomological obstruction measure H¹ capturing the structure of observer disagreement, and [COMPUTE: family count] structural families of constraints with identical transformation behavior under observer shift. Observer-dependence enters the system through exactly two independent mechanisms — a continuous power-scaled extraction hub and a discrete effective immutability hub — whose independence is verified empirically (zero Type A conflicts) and whose interaction produces the framework's most diagnostic findings. The most philosophically significant finding is that no extractive constraint admits a global section — extraction is never observer-independent. A classical MaxEnt oracle, given access to all non-indexed structural data, detects only ~1% of the observer-dependence that cohomological analysis detects — a 100x gap that establishes cross-context coherence as irreducible to within-context evaluation. We establish a three-way equivalence between Boltzmann factorizability, Lawvere naturality, and Grothendieck descent as the formal spine of the framework. A diagnostic integration system comprising 12 subsystems, 15 abductive trigger classes, and 11 expected conflict patterns synthesizes cross-subsystem signals into a traffic-light verdict per constraint, without altering classification. We provide an honest assessment distinguishing strict categorical correspondences from productive analogies and loose metaphors.

---

## 1. Introduction

The classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects. This is not a failure of classification but a structural feature of the domain: different observer positions have access to different information, different time horizons, different exit options, and different power to act on what they observe.

The standard response to perspectival dependence is to resolve it — to identify the "correct" classification by privileging one observer position or aggregating across positions. This paper takes the opposite approach. We argue that perspectival dependence has formal mathematical structure, that the structure is best captured by presheaf theory, and that the standard tools of topos theory — cohomology, descent, naturality — produce quantitative invariants that characterize any domain where classification depends on perspective.

The framework, which we call *Deferential Realism* (DR), models observer positions as objects of a small category (a *site*), classification at each position as a *presheaf* on that site, and the degree of observer disagreement as *cohomological invariants* of the presheaf. The presheaf is emphatically not a sheaf: the gluing axiom is intentionally violated because perspectival disagreement is a diagnostic signal, not a defect.

Applied to a corpus of 1,142 social constraints with quantitative metrics for extractiveness, suppression, and structural properties, the framework produces five headline findings:

1. **The descent split.** Of 1,142 constraints, [COMPUTE: H⁰ count] ([COMPUTE: descent rate as %]%) admit a global section — a classification that is the same from every observer position. The remaining [COMPUTE: non-H⁰ count] ([COMPUTE: 100 - descent rate]%) have irreducible perspectival dependence. Classification that is independent of who is observing is the exception, not the rule.

2. **The absence of snares from H⁰.** No constraint classified as extractive (type "snare") from any observer position is classified as extractive from *every* position. Extraction is never observer-independent. There is always at least one position from which the extraction is invisible — reclassified as legitimate coordination or immutable constraint. The persistence of extraction *requires* this cover story.

3. **The superselection gap.** The distribution of observer disagreement is not continuous. No constraint has exactly 1 or 2 disagreeing observer-pairs (out of 6 possible pairs). When perspectival dependence emerges, it emerges in blocs of at least 3 disagreeing pairs, a consequence of the site's linear power ordering and the classification cascade's threshold geometry.

4. **The 100x classical oracle gap.** A Maximum Entropy classifier operating on observer-independent metrics — a "classical oracle" with access to all structural data but no indexing by observer position — detects only ~1% of the observer-dependence that cohomological analysis detects. The oracle fails not by producing wrong answers but by being unable to detect that answers differ across observer positions. Cross-context coherence is irreducible to within-context evaluation.

5. **The independence of epistemic restriction and frame-dependence.** Two phenomena that both look like "observer-dependence" — restricted information access (what an observer can see) and structural frame (how an observer processes what they see) — are nearly disjoint, with only 1.6% overlap. The system has two independent kinds of observer-dependent classification error.

The framework comprises a classification presheaf evaluated at four standard observer contexts (§2.1–2.3), a naturality test connecting Boltzmann factorizability, Lawvere naturality, and Grothendieck descent (§2.4), a contamination network propagating structural health through agent-sharing graphs (§2.5), a two-hub architecture localizing all observer-dependence to exactly two input channels (§2.7), ~65 binary structural gates encoding pre-metric structural identity (§2.8), and a diagnostic stack comprising gauge orbits (§3.1), MaxEnt shadow classification (§3.2), abductive synthesis with 15 trigger classes (§3.3), trajectory mining (§3.4), and a 12-subsystem diagnostic integration engine producing traffic-light verdicts (§3.5). Cohomological results appear in §4, an honest assessment in §5, and related work in §6.

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

The two-hub architecture (§2.7) reveals that observer-dependence enters the system through exactly two channels, both localized in the context tuple: Hub 1 reads Power to derive a directionality value, Hub 2 reads (Time, Exit) to derive an immutability perception. Everything else in the classification pipeline is observer-independent data or deterministic transformation.

### 2.2 The Presheaf: Classification as Local Truth

The core construction is the classification presheaf. For each constraint *C* in the corpus, we define a contravariant functor:

$$F_C : \mathbf{C}^{op} \to \mathbf{Set}$$

by setting $F_C(U_i) = \text{dr\_type}(C, U_i)$, where dr_type computes the classification of constraint *C* as observed from context $U_i$. The result is an element of the eight-element type space $\Omega$ (defined in §2.3).

This is a presheaf by construction: for each object of the site, it assigns a set (here, a single element of $\Omega$); for each morphism $U_i \to U_j$ in the site, there is an implicit restriction map $F_C(U_j) \to F_C(U_i)$ determined by the classification cascade's response to the change in observer context [STRICT].

The presheaf is **not** a sheaf. The gluing axiom requires that if local sections agree on overlaps, they extend to a global section. The entire diagnostic infrastructure of the framework exists precisely because local sections *fail to glue*: a constraint may classify as "mountain" (immutable natural law) from U₄ and "rope" (legitimate coordination mechanism) from U₃, with no consistent global classification available. This **perspectival gap** is the framework's central diagnostic signal, not a defect to be resolved [STRICT].

**The restriction map.** The restriction map has two independent channels, corresponding to the two hubs of the classification architecture (§2.7). The mechanism by which classification changes across observer positions involves both a continuous power-scaling function and a discrete immutability lookup:

**Channel 1 (Hub 1 — continuous).** Base extractiveness $\varepsilon$ is a context-independent property of the constraint. The experienced extractiveness at a given context is:

$$\chi = \varepsilon \times \sigma(\pi(P)) \times \sigma(S)$$

where $\pi(P)$ is a power index associated with the observer's power level, $\sigma$ denotes a sigmoid scaling, and $S$ contributes a scope modifier. Higher power generally reduces experienced extractiveness (the sigmoid is calibrated so that institutional and analytical observers perceive less extraction), which means the classification presheaf is sensitive to the observer's structural position. The restriction map — the effect of following a morphism $U_i \to U_j$ in the site — is determined by this scaling: moving to a higher-power context generally decreases experienced extractiveness, potentially reclassifying a "snare" as a "rope."

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
| Classification factorizes across P × S | Naturality square commutes | Product-site factorizability | Separability of the Lagrangian |
| Failure detected | Naturality failure witness | Descent obstruction | Broken symmetry |

The Lawvere ↔ Grothendieck equivalence (naturality ↔ descent on this site) is STRICT — these are genuinely the same mathematical condition. The Noether column uses "conservation" as shorthand for invariance under a discrete group action, which is the precondition of Noether's theorem rather than the theorem itself. The overall equivalence is therefore STRUCTURAL (two-out-of-three: Lawvere ↔ Grothendieck is STRICT; Noether is a productive structural parallel). Every other diagnostic layer in the framework — MaxEnt entropy, abductive analysis, trajectory mining, drift detection, cohomological computation, diagnostic integration — is a different way of measuring proximity to or deviation from this invariance condition.

**Naturality certificates and failure witnesses.** Three derived signatures operationalize the naturality test:

- **Coupling-Invariant Rope (CI Rope)**: passes all four naturality conditions (Boltzmann compliance, scope invariance, absence of nonsensical coupling, no excess extraction). This is a naturality certificate — formal evidence that the classification is well-behaved.
- **False Natural Law (FNL)**: the constraint presents as a natural law (mountain-like metrics) but fails the Boltzmann factorizability test. Cross-index coupling is detected where none should exist. The constraint is "physics-washed" — constructed but disguised as natural. This is a naturality failure witness.
- **False CI Rope (FCR)**: the constraint appears to be a structurally sound rope but fails one or more structural tests. The apparent coordination is itself illusory — a doubly-broken classification. The FCR gate includes a deferral mechanism: when metric perspectival variance is detected, the override is deferred, allowing the raw metric-based type to stand. This prevents FCR from forcing a reclassification when the constraint genuinely exhibits perspectival variation that the override would suppress.

All three classifications are STRICT: they test well-defined conditions on the naturality square.

### 2.5 Purity and Contamination

Constraints do not exist in isolation. They share agents — individuals or institutions that enforce, benefit from, or are subject to them — creating a network of influence. The **purity score** is a composite measure of a constraint's naturality health, combining four subscores: factorizability (Power × Scope independence), scope invariance, coupling cleanliness, and excess extraction above a thermodynamic floor. The score ranges from 0.0 (complete naturality failure) to 1.0 (perfect naturality).

The **contamination network** propagates purity through the agent-sharing graph. A high-purity constraint (structurally clean coordination) that shares an agent with a low-purity constraint (contaminated extraction) has its effective purity reduced. Crucially, contamination flows *against* the purity gradient — from lower-purity to higher-purity constraints — giving the propagation a contravariant character. Mountains are immune to contamination (immunity = 0.0); ropes are fully susceptible (STRUCTURAL).

The **Fixed-Point Network (FPN)** computes the equilibrium distribution of effective purities by iterating the contamination operator until convergence. The operator is monotone (contamination is non-decreasing) and bounded below (purity is floored at 0.0), so by the Knaster-Tarski theorem, iteration from intrinsic purities converges to a greatest fixed point. In coalgebraic terms, this equilibrium is the terminal coalgebra of the contamination endofunctor on the purity lattice — a suggestive coalgebraic reading, though the operator itself is just an order-theoretic monotone map on a finite lattice (STRUCTURAL; see §5.2). In v5.3, the FPN graduated from shadow diagnostic to active component: its output feeds into the diagnostic integration system (§3.5) and serves as an input to abductive triggers T6 (accelerating pathology) and T7 (contamination cascade).

### 2.6 Corpus Provenance and Calibration

The empirical results reported in this paper are drawn from a corpus of 1,142 social constraints — laws, norms, institutions, regulatory mechanisms. The corpus grew from 1,023 constraints in v2 through iterative expansion driven by a specific workflow: an LLM (Gemini) generates "constraint stories" — Prolog files modeling power dynamics and structural relationships — which the Prolog infrastructure evaluates via independent analytical tools. An analyst receives both the story and the diagnostic report, then writes a "diff-essay" analyzing the gap between what the LLM assumed and what the structural analysis reveals. Constraint stories being wrong in characteristic LLM ways is a feature: the diagnostic infrastructure surfaces *where and why* stories fail.

The continuous metrics assigned to each constraint (extractiveness ε, suppression σ, theater ratio θ, resistance-to-change ρ) are specified in the constraint stories and validated against the analytical pipeline. They have not been subjected to inter-rater reliability testing. A hostile reader could reasonably ask: how much of the output is a property of the formal framework, and how much is an artifact of the calibration?

The answer has two parts. First, the formal framework — presheaf structure, naturality testing, cohomological computation, Galois connection — is independent of the particular corpus and its metric assignments. A different corpus with different metrics would produce different empirical numbers (a different descent rate, a different H¹ distribution, different structural families) but the same formal machinery would apply. The framework's contribution is the diagnostic vocabulary, not the specific invariants computed for this corpus.

Second, a configuration sensitivity sweep provides direct evidence of calibration robustness. At v2 (1,023 constraints), all 118 numeric parameters governing classification thresholds, power modifiers, and coupling gates were perturbed at ±10% and ±25%, with each of the 472 resulting configurations running the full validation suite. Of the 118 parameters, 103 (87%) were inert at ±25% perturbation — the classification output did not change. Eight parameters were moderate: stable at ±10% but breaking at ±25%, including the Boltzmann coupling threshold (current value 0.25, floor approximately 0.19–0.22). Of the seven parameters initially flagged as critical, six were reclassified upon investigation as timeout artifacts or integer-rounding effects, leaving one genuinely critical parameter: the analytical power modifier, a structural constant that scales every experienced-extractiveness calculation. The system's stability is a consequence of design rather than luck: the critical parameter is load-bearing by construction, not by calibration fragility.

The parameter count has grown from 118 to approximately 170 with the addition of abductive engine thresholds, diagnostic integration parameters, and quantum verification trigger configurations. [COMPUTE: re-run sensitivity sweep against 1,142-constraint corpus with 170 parameters, or note this as open validation work.]

**Type distribution in the current corpus:**

| Type (analytical context) | Count | % of corpus |
|---|---|---|
| tangled_rope | 550 | 48.2% |
| snare | 392 | 34.3% |
| mountain | 139 | 12.2% |
| rope | 54 | 4.7% |
| naturalized | 5 | 0.4% |
| scaffold | 1 | 0.1% |
| unknown | 1 | 0.1% |

**Test suite.** The validation suite comprises 1,152 test cases [COMPUTE: verify exact count], covering classification logic, naturality testing, cohomological computation, abductive trigger firing, and diagnostic integration.

**Codebase scope.** The Prolog implementation comprises 77 modules covering classification (drl_core, constraint_indexing), diagnostics (maxent_classifier, boltzmann_compliance, dirac_classification, grothendieck_cohomology, purity_scoring, logical_fingerprint, drl_lifecycle), synthesis (abductive_engine, abductive_triggers, diagnostic_summary), reporting (json_report, report_generator, enhanced output), and configuration (config, config_validation, validation_suite). Additional modules support specialized analyses not detailed in this paper: psychological pattern classification (psych_bridge), counterfactual analysis (drl_counterfactual), intent-interval classification (intent_engine), network phase-transition analysis (giant_component_analysis), invertibility analysis, covering analysis, coercion projection, and scenario management for corpus loading. These peripheral modules extend the framework's analytical reach but are not part of the core presheaf-diagnostic architecture documented here.

The empirical findings reported in §4 are properties of this corpus under this calibration. The formal framework's value lies in providing the diagnostic machinery to compute such invariants for any corpus.

### 2.7 Two-Hub Architecture

Every classification in DR flows through `classify_from_metrics/6`. But the observer-dependent inputs to that function come from two independent subsystems that share no internal logic. This section documents why they are independent, why they should not be unified, and what their independence reveals about where indexicality lives in the system.

**Hub 1: Power-Scaled Extraction (the sigmoid).** The directionality derivation chain — `derive_directionality/3` → `sigmoid_f/2` → `extractiveness_for_agent/3` — maps observer position to an experienced extraction value χ. This is a continuous transformation: small changes in power position produce small changes in χ. The mechanism is structural. An observer's relationship to the constraint (beneficiary, victim, neither) determines a directionality value *d*, and the sigmoid amplifies or suppresses base extraction accordingly. All threshold-based classification differences (snare vs. tangled_rope vs. rope) are downstream of this hub [STRICT].

**Hub 2: Effective Immutability (the lookup table).** The `effective_immutability_for_context/2` table maps 18 (TimeHorizon, ExitOptions) pairs to a perceived mutability level. This is a discrete function: 18 hard-coded facts that produce categorical outputs (mountain, rope, or neither). No sigmoid, no continuous transformation. The mechanism is perceptual. An observer with biographical time horizon and no exit options perceives a constraint as immutable; an observer with generational time horizon and systemic exit perceives the same constraint as mutable. The mountain gate and the rope gate in `classify_from_metrics/6` both check this table independently of χ [STRICT].

**Why the hubs should not be unified.** Three reasons prevent collapsing Hub 2 into Hub 1:

1. *They measure different things.* Hub 1 measures how power position transforms the *experience* of extraction — a structural-economic relationship. Hub 2 measures how temporal horizon and escape options transform the *perception* of mutability — a structural-epistemic relationship. An observer can experience high extraction (Hub 1 says snare) while perceiving the constraint as mutable (Hub 2 says not-mountain). This combination is precisely the tangled rope — the most common constraint type in the corpus.

2. *They have different mathematical characters.* Hub 1 is continuous, differentiable, and admits sensitivity analysis. Hub 2 is discrete and categorical — there is no meaningful "halfway between mountain and rope" immutability perception. Forcing a continuous parameterization onto an inherently discrete judgment would introduce false precision.

3. *They interact productively at specific points.* The mountain gate requires BOTH low χ (Hub 1) AND effective_immutability = mountain (Hub 2). False mountains — actively enforced extraction that an observer perceives as natural law — are only detectable *because* the two hubs are independent. A unified hub would blend the signals and lose the diagnostic.

**Indexicality localization.** Indexicality enters the system through exactly two input channels:

- **Hub 1 channel:** `derive_directionality/3` produces *d* from the observer's structural relationship to the constraint. This is the primary indexical input.
- **Hub 2 channel:** The (TimeHorizon, ExitOptions) pair determines which row of the immutability table applies. This is the secondary indexical input.

Everything else — the sigmoid, the thresholds, the structural signatures, the MaxEnt likelihoods, the ~65 binary structural gates (§2.8) — is either observer-independent data or deterministic transformation of indexed inputs. The system's observer-dependence is fully localized to these two input channels [STRICT].

**Hub independence: empirical verification.** Zero Type A conflicts exist — no constraint in the corpus is simultaneously classified as mountain by the immutability table and at snare-level extraction by the sigmoid. The mountain gate's BaseEps check prevents this by requiring low base extraction as a precondition for mountain classification. Hub-conflict constraints — where the immutability table flips between mountain and non-mountain across contexts — cluster at exactly H¹ = 4 (§4.2), forming a distinct cohomological band that corresponds to Hub 2's discrete transition. This means the cohomological formalism has decomposed observer-dependence into bands corresponding to specific architectural mechanisms: H¹ = 3 for Hub 1-driven divergence, H¹ = 4 for Hub 2-driven divergence [STRICT for hub independence and zero Type A conflicts; STRUCTURAL for H¹ band → hub correspondence].

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

The corpus yields [COMPUTE: orbit family count] orbit families from which [COMPUTE: structural family count] structural families are derived (§3.4).

### 3.2 Probabilistic Shadow (MaxEnt Classification)

The deterministic classifier assigns a single type at each context. The Maximum Entropy (MaxEnt) shadow classifier runs alongside it and assigns a *probability distribution* over all types. Where the deterministic classifier evaluates the presheaf, the MaxEnt classifier evaluates a probabilistic extension — for each constraint-context pair, it assigns a point in the probability simplex $\Delta^5$.

The construction uses Gaussian log-likelihoods for each type based on the constraint's metrics, combined with prior weights and boolean features, normalized via log-sum-exp to a proper distribution. The Shannon entropy of this distribution, normalized by $\log(6)$, measures classification uncertainty: 0 means the probabilistic and deterministic classifiers agree completely; 1 means classification is maximally uncertain.

In Wheeler's interpretive framework, the MaxEnt classifier performs "soft measurement" — probabilistic determination that has not yet collapsed to a definite classification. Constraints with high MaxEnt entropy sit near classification boundaries; the "measurement" (observer position) has not provided enough information to fix a single type [STRUCTURAL — the distribution on $\Omega$ is genuine, but the full Giry monad structure is incomplete].

The diagnostic value of the MaxEnt layer lies in disagreement detection. Three levels of disagreement are tracked:

- **Hard disagreement**: the probabilistic top type differs from the deterministic classification. The two classifiers see different constraints.
- **Soft disagreement**: the top types agree, but the probabilistic confidence is low (the second-ranked type has nearly as much probability mass). The deterministic classifier commits to a type that the probabilistic classifier considers marginal.
- **Entropy flag**: the normalized Shannon entropy exceeds a threshold, indicating that probability mass is spread across multiple types. The constraint sits near a multi-way classification boundary.

**Indexed MaxEnt variant.** An extended MaxEnt computation uses power-scaled χ instead of raw ε, producing a probability distribution that accounts for observer position. The divergence between classical MaxEnt (using ε) and indexed MaxEnt (using χ) directly measures the probabilistic effect of observer-dependence. This divergence is the foundation for trigger T13 (§3.3) and the 100x oracle gap finding (§4.3). Constraints where the indexed and classical distributions diverge are exactly the constraints where power-scaling changes the probabilistic answer — where the numbers look innocuous from a neutral perspective but become significant when evaluated from a specific power position [STRICT — the divergence is a direct measurement].

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

**T9: MaxEnt Shadow Divergence (~207 fires, ~18%).** MaxEnt strongly favors a type different from the constraint's signature override target. Catches FCR-gated constraints where the probabilistic model rejects the override intent — the FCR gate deferred the override because perspectival variance exists, but MaxEnt's probability mass points away from what the override *would have* reclassified to [STRUCTURAL].

**T10: Convergent Structural Stress (~232 fires, ~20%).** A "common core + rare gate" design. The common core requires ≥4 of 5 stress indicators (false signature, low purity, drift events, high coupling, elevated entropy). The rare gate requires at least one genuinely tail-distributed anomaly signal (MaxEnt hard disagreement OR H¹ ≥ 4). The common core confirms "broadly stressed"; the rare gate establishes "anomalously so." This trigger underwent three design iterations to reduce false positive rate from 84% (when based on correlated count-based indicators) to ~20% [STRUCTURAL].

**T11: Snare-Leaning Tangled (~192 fires, ~17%).** Override target is tangled_rope but MaxEnt ψ (snare-lean ratio) exceeds threshold: P(snare) > 0.85 AND ψ = P(snare)/(P(rope) + P(snare) + 0.001) > 0.90. The probabilistic model overwhelmingly classifies these as snares despite the tangled_rope classification [STRUCTURAL].

**T6: Accelerating Pathology.** FPN zone migration + purity drift event. The static equilibrium analysis shows contamination AND temporal dynamics confirm it is actively worsening. Requires FPN subsystem [STRUCTURAL].

**T7: Contamination Cascade.** FPN effective purity divergence + network drift. Distinguishes theoretical vulnerability from ongoing contamination propagation. Requires FPN subsystem [STRUCTURAL].

**T8: Dormant Extraction.** Low MaxEnt entropy + clean type (rope or mountain) + extractive fingerprint voids + coupling above threshold. The constraint looks clean by every metric but has the structural fingerprint pattern of hidden extraction — a sleeper [STRUCTURAL].

#### Category B — Agreement Verification (T13–T16)

After Category A establishes which diagnostics disagree, Category B asks a different question: *is the remaining agreement misleading?* These triggers detect cases where the framework's classical tools are systematically blind to structure that the indexed and cohomological tools can see. This represents a qualitative expansion of the engine — it now audits its own confidence.

**T13: MaxEnt Divergence (~11 constraints, smallest population, strongest signal).** The classical MaxEnt (using raw ε) and the indexed MaxEnt (using power-scaled χ) produce divergent probability distributions, with divergence exceeding the configured threshold (default: 0.05). All high-divergence constraints have H¹ > 0 — 100% correlation between probabilistic divergence and cohomological obstruction. These are constraints where the numbers look innocuous from a neutral perspective but change character when evaluated from a specific power position. Confidence: 0.80 [STRICT — the divergence is a direct measurement, not an inference].

**T14: Hub-Conflict (~23 constraints).** Constraints where Hub 2's immutability table flips the classification. These sit at exactly H¹ = 4: the discrete (TimeHorizon × ExitOptions) lookup table switches between "mountain" and "not mountain" as the observer position changes. The classification change is driven by a discrete table lookup, not by the continuous extraction metrics. Example: a neuroscientist at exit_options(analytical) sees biological curiosity as immutable law (mountain); a trapped subject sees it as inescapable suffering (snare). The extraction metrics are identical; the immutability perception is different. Confidence: 0.75 [STRUCTURAL].

**T15: Epistemic Trap (~293 constraints, of which ~65 are "dangerous").** The powerless observer's restricted information set produces a different classification than the full-information classifier. The gap between `classify_from_restricted/3` and `dr_type/3` measures the epistemic cost of the observer's position. Three distortion patterns:

| Pattern | Count | What happens |
|---|---|---|
| mountain → rope (~114) | Natural laws appear as coordination choices. The observer may waste effort trying to change something unchangeable. |
| tangled_rope → snare (~82) | Mixed systems appear purely extractive. The observer sees more extraction than exists. |
| tangled_rope → rope (~65) | Extraction becomes invisible. The restricted view shows only the coordination function. |

The third pattern is the dangerous one: "it's just the rules" is literally what the powerless observer perceives, because the features that reveal extraction are not in their accessible information set. This is the structural mechanism behind cover stories. Confidence: 0.70 [STRUCTURAL].

**T16: Classical Oracle Failure (~870 constraints, largest trigger class).** MaxEnt entropy is low (oracle is confident) but H¹ > 0 (cohomological obstruction confirms observer-dependence). The classical oracle thinks it has a clear answer, but the structural analysis reveals the answer changes with observer position. This trigger catches the ~99% of observer-dependent constraints that T13 misses — constraints where observer-dependence produces categorical classification shifts without producing large probabilistic shifts. The Gaussian likelihoods are broad enough that raw metrics are compatible with multiple types, so shifting from ε to χ doesn't move the probability mass much — but it *does* cross the deterministic thresholds. Confidence: 0.55 + min(0.15, H¹ × 0.03), scaling with H¹ severity [STRUCTURAL].

Together, T13 and T16 cover ~98.9% of all H¹ > 0 constraints. The ~1% uncovered have MaxEnt entropy above 0.40 and no indexed divergence — genuinely ambiguous cases where neither trigger should fire.

**Note on T12.** T12 (post-synthesis divergence) exists as a separate module in the post-synthesis phase, not as a standard trigger in the main engine.

**Trigger Summary Table**

| ID | Name | Category | Population | Signal Type | Confidence | Rigor |
|---|---|---|---|---|---|---|
| T1 | Signature Override Artifact | A (artifact filter) | ~25 | MaxEnt vs. override | 0.85 | STRUCTURAL |
| T2 | Deep Deception | A | rare | FNL + mountain metrics | 0.70 | STRUCTURAL |
| T3 | Metric-Structural Divergence | A | [COMPUTE] | entropy vs. orbit | 0.65 | STRUCTURAL |
| T4 | Confirmed Liminal | A | ~10 | triple confirmation | 0.75 | STRUCTURAL |
| T5 | Coverage Gap | A | [COMPUTE] | orbit vs. mismatch | 0.60 | STRUCTURAL |
| T6 | Accelerating Pathology | A | [COMPUTE] | FPN + drift | 0.70 | STRUCTURAL |
| T7 | Contamination Cascade | A | [COMPUTE] | FPN + network drift | 0.65 | STRUCTURAL |
| T8 | Dormant Extraction | A | [COMPUTE] | clean type + extractive voids | 0.50–0.70 | STRUCTURAL |
| T9 | MaxEnt Shadow Divergence | A | ~207 | MaxEnt vs. override target | 0.75 | STRUCTURAL |
| T10 | Convergent Structural Stress | A | ~232 | multi-signal + rare gate | 0.50–0.70 | STRUCTURAL |
| T11 | Snare-Leaning Tangled | A | ~192 | MaxEnt ψ ratio | 0.65 | STRUCTURAL |
| T13 | MaxEnt Divergence | B | ~11 | indexed vs. classical MaxEnt | 0.80 | STRICT |
| T14 | Hub-Conflict | B | ~23 | H¹ = 4 band | 0.75 | STRUCTURAL |
| T15 | Epistemic Trap | B | ~293 | restricted vs. full view | 0.70 | STRUCTURAL |
| T16 | Classical Oracle Failure | B | ~870 | confident oracle + H¹ > 0 | 0.55–0.70 | STRUCTURAL |

### 3.4 Trajectory Mining and Structural Families

A constraint's **trajectory** is its complete presheaf evaluation enriched with continuous diagnostics: for each of the four standard contexts, the trajectory records the type, experienced extractiveness $\chi$, MaxEnt entropy, and classification confidence. Where the gauge orbit records only the discrete type at each context, the trajectory captures the full quantitative profile.

The **trajectory distance** between two constraints is a weighted 4-component metric:

| Component | Weight | What it measures |
|-----------|--------|-----------------|
| Shift distance | 0.35 | Agreement of type profiles across contexts |
| Metric distance | 0.25 | Similarity of continuous metrics ($\chi$, entropy) |
| Stability distance | 0.25 | Similarity of purity, coupling, and naturality health |
| Pathology distance | 0.15 | Similarity of drift counts and contamination status |

Hierarchical agglomerative clustering (HAC) with average linkage groups constraints into **structural families** — equivalence classes under trajectory similarity. A two-stage approach is used: first, constraints are grouped by their discrete shift pattern (the ordered type profile across contexts, yielding [COMPUTE: shift group count] groups); then HAC is applied within each shift group using the continuous metric components. This is semantically sound because constraints with different shift patterns have high shift-distance by definition, so they would rarely merge before the clustering cut level.

The corpus yields [COMPUTE: structural family count] structural families from [COMPUTE: orbit family count] orbit families, with [COMPUTE: split count] orbit families split by the continuous-metric resolution.

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

**Verdict computation.**

- **GREEN**: Only agreements and expected conflicts. No subsystem raises an unexplained concern. 577 constraints (50.5%).
- **YELLOW**: Has tensions or convergent rejections, but below critical mass (<3 tensions, or convergent rejection with <3 subsystems). 546 constraints (47.8%).
- **RED**: ≥3 tensions, OR convergent rejection with ≥3 independent subsystems pointing to the same alternative type. 19 constraints (1.7%).

[STRUCTURAL — threshold-based verdict computation.]

**Critical design constraint.** The verdict does NOT change classification. Red means "analyst should investigate," not "reclassify." The deterministic Tier 1 classifier remains the source of truth. The diagnostic integration system provides meta-analytical commentary on how confident the analyst should be in the classification, not a competing classification. This separation is deliberate: the verdict is an observation about the coherence of diagnostic signals, not a correction of classification.

---

## 4. Cohomological Results

This section presents the empirical core of the framework. Every number traces to the corpus computation over 1,142 constraints with extractiveness metrics.

### 4.1 H⁰: Global Sections

The zeroth Čech cohomology $H^0(\mathcal{U}, F_C)$ counts the global sections of the classification presheaf — constraints whose classification is the same from every observer position. A global section exists if and only if the gauge orbit is a singleton: all four standard contexts agree on the type.

**Result.** Of 1,142 constraints, [COMPUTE: H⁰ count] admit a global section (descent rate = [COMPUTE: H⁰/1142]). The breakdown by type:

| Type | Count | % of H⁰ |
|------|-------|----------|
| tangled_rope | [COMPUTE] | [COMPUTE]% |
| rope | [COMPUTE] | [COMPUTE]% |
| mountain | [COMPUTE] | [COMPUTE]% |
| scaffold | [COMPUTE] | [COMPUTE]% |
| snare | **0** | **0.0%** |

The mountains in H⁰ are the genuine natural laws of the corpus: thermodynamic constraints, impossibility theorems (Arrow's, Godel's), information-theoretic limits (Chaitin's omega, halting problem). These constraints are context-invariant by their nature — the second law of thermodynamics does not depend on who is observing.

The ropes in H⁰ are universally recognized coordination mechanisms: standard protocols (TCP/IP, metric system), well-established legal frameworks. The scaffolds are temporary coordination mechanisms universally recognized as temporary — they have sunset clauses, and sunset clauses are context-invariant properties.

The largest H⁰ group is the tangled ropes: constraints where the entanglement of coordination and extraction is visible from every observer position. These constraints descend — every observer sees them as tangled — but the *structure* of the tangling may vary across the Power × Scope grid. Descent tests consistency of the type; naturality tests consistency of the mechanism.

**The absence of snares.** No extractive constraint appears in H⁰. This is the most philosophically significant finding of the cohomological computation. It means that extraction is *never* observer-independent: every constraint that any observer classifies as a snare is classified differently by at least one other observer. In presheaf-theoretic terms, "this constraint is extractive" is never a global section — it is always a local truth that fails to glue.

The mechanism is structural, not accidental. Hub 1's sigmoid amplifies extraction differently at each power level: the power-scaling function maps different observer positions to different experienced-extractiveness values through the directionality derivation chain. For a constraint to appear as a snare at every context, the sigmoid would need to push χ above the snare threshold at every power level — but the sigmoid is calibrated so that higher power systematically reduces experienced extractiveness, making this impossible for any constraint with finite base extractiveness [STRUCTURAL].

**Mountain stability.** All genuine mountains — constraints in H⁰ with ε < 0.05 — show exactly zero MaxEnt divergence between classical and indexed runs. The sigmoid produces no effect on near-zero extraction. Mountains and snares occupy opposite poles of the indexicality spectrum: mountains are fully classical (recoverable from non-indexed data), snares are fully indexed (invisible without observer position) [STRICT — zero divergence is a measurement, not an inference].

### 4.2 H¹: Perspectival Fracture

For each constraint, the H¹ proxy counts the number of disagreeing context-pairs among the $\binom{4}{2} = 6$ unordered pairs of standard contexts. A pair $(U_i, U_j)$ disagrees when $F_C(U_i) \neq F_C(U_j)$.

**Why disagreeing-pairs is a meaningful cohomological measure.** The site is a linear poset (powerless < moderate < institutional < analytical), not a discrete set. Equipped with the Alexandrov topology (upper sets are open), $U_i \cap U_j = U_{\max(i,j)}$ is non-empty for every pair — all elements are comparable in a linear order. The disagreeing-pairs count therefore measures the number of restriction morphisms along which the presheaf's type assignment fails to be compatible. This is a combinatorial descent-failure count on the poset site: it records how many morphisms in the site category witness a failure of the presheaf to satisfy the descent condition. It is not formal Čech H¹ (which would require the quotient $\ker(\delta^1)/\operatorname{im}(\delta^0)$ and is trivially 0 on a discrete site where intersections are empty), but it is a well-motivated obstruction measure that captures genuine cohomological content on the Alexandrov site.

The distribution across the corpus:

| H¹ | Count | % of corpus |
|----|-------|-------------|
| 0 | [COMPUTE] | [COMPUTE]% |
| 1 | 0 | 0.0% |
| 2 | 0 | 0.0% |
| 3 | [COMPUTE] | [COMPUTE]% |
| 4 | [COMPUTE] | [COMPUTE]% |
| 5 | [COMPUTE] | [COMPUTE]% |
| 6 | [COMPUTE] | [COMPUTE]% |

**The gap at H¹ = 1, 2.** No constraint has exactly 1 or 2 disagreeing pairs. This is a structural consequence of the site's linear ordering and the classification cascade's threshold geometry, not an empirical accident. The four standard contexts are ordered on a one-dimensional power axis, and the classification cascade uses fixed thresholds on continuous metrics scaled by a monotone sigmoid of power. When a constraint's experienced extractiveness crosses a classification threshold, it crosses at a single power boundary — say, between U₂ and U₃. But because the power ordering is linear, this creates a 3+1 split (three contexts on one side, one on the other) or a 2+2 split, never a 1+3+0 or other configuration that would produce exactly 1 or 2 disagreeing pairs. A single threshold transition on a linearly ordered 4-element site generates exactly 3 or 4 disagreeing pairs — never 1 or 2.

This gap is a property of the measurement apparatus (the site geometry), not of the constraints. A richer site with non-linear power relationships could in principle produce H¹ = 1 or 2. The gap and its mechanism are formally derived from the site geometry [STRICT]. The superselection analogy is productive — both prohibit certain state combinations as a consequence of the ambient structure rather than of any particular state — but imports Hilbert-space connotations the framework does not support [STRUCTURAL].

**H¹ band structure.** The two-hub architecture (§2.7) gives the H¹ distribution an internal structure that tracks specific architectural mechanisms:

| H¹ band | Mechanism | Hub |
|---------|-----------|-----|
| 0 | Neither hub diverges. All observers agree. | — |
| 3 | Hub 1 sigmoid pushes χ across threshold at a single power boundary. | Hub 1 |
| 4 | Hub 2 immutability table flips between mountain and non-mountain. | Hub 2 |
| 5–6 | Multiple interactions between both hubs, or multiple threshold crossings. | Both |

The H¹ = 3 band corresponds to Hub 1-driven divergence: the dominant pattern is the institutional observer (U₃) seeing coordination where other observers see extraction, because the sigmoid suppresses experienced extractiveness at the institutional power level. The H¹ = 4 band corresponds to Hub 2-driven divergence: the immutability table flips between mountain and not-mountain at the powerless/moderate boundary. This band–hub correspondence means the cohomological formalism has decomposed observer-dependence into components that track specific architectural mechanisms [STRUCTURAL — empirical, not derived from formal categorical structure].

**The dominant mode.** H¹ = 3 accounts for [COMPUTE]% of the corpus. The most common pattern is [snare, snare, rope, snare] — three contexts see extraction, but the institutional observer (U₃), with generational time horizon and arbitrage exit options, sees legitimate coordination. The institutional observer is the dominant dissenter.

**Maximal obstruction.** [COMPUTE: count] constraints achieve H¹ = 6 (all pairs disagree). These share the orbit [naturalized, tangled_rope, rope, snare] — four distinct types, one per context. They represent the limiting case of perspectival dependence: power level completely determines what you see.

**H¹ is constant within orbit families.** Within any given orbit family (constraints sharing the same set of types in their orbit), H¹ is constant. This is a consequence of the site structure — the number of disagreeing pairs is fully determined by the set of distinct types and how they distribute across the four canonical positions. H¹ does not add information *within* orbit families; what it adds is a graduated numerical measure allowing comparison *across* families.

### 4.3 Descent Rate and Corpus Invariants

The **descent rate** — the fraction of the corpus admitting global sections — is [COMPUTE: H⁰/1142]. This single number characterizes the corpus: roughly [COMPUTE: round to nearest description] of social constraints have irreducible perspectival dependence.

**Mean H¹ by analytical-context type** reveals a gradient:

| Analytical type | Mean H¹ | N |
|----------------|---------|---|
| scaffold | [COMPUTE] | [COMPUTE] |
| rope | [COMPUTE] | [COMPUTE] |
| tangled_rope | [COMPUTE] | [COMPUTE] |
| mountain | [COMPUTE] | [COMPUTE] |
| snare | [COMPUTE] | [COMPUTE] |
| unknown | [COMPUTE] | [COMPUTE] |

The structural prediction from v2 holds: scaffolds should always descend, ropes mostly descend, and snares have high fracture. The ordering is structurally informative: temporary coordination is universally recognized as temporary; extraction mechanisms look different depending on who is observing.

**The 100x classical oracle gap.** The MaxEnt classifier, operating on observer-independent metrics alone, detects only a small fraction of the observer-dependence that cohomological analysis detects. Of [COMPUTE: H¹>0 count] constraints with H¹ > 0, only ~11 show high MaxEnt divergence between classical and indexed runs (caught by trigger T13) — a ratio of approximately [COMPUTE: H¹>0 count / 11]:1, the classical oracle gap. At the v2 corpus scale (~790 constraints with H¹ > 0, 8 with high divergence), this ratio was approximately 100:1. The remaining ~99% have observer-dependence that the probabilistic formalism cannot detect.

The mechanism is precise: most observer-dependence produces categorical classification shifts (rope → snare, mountain → scaffold) without producing large probabilistic shifts in the MaxEnt distribution. The Gaussian likelihoods are broad enough that raw metrics are compatible with multiple types, so shifting from ε to χ doesn't move the probability mass much — but it does cross the deterministic thresholds. The classical oracle fails not by getting wrong answers but by failing to detect that answers *differ* across indices. It thinks the classification is ambiguous either way. The indexed classifier knows it is different.

This is a corpus-level empirical instance of a structural principle: cross-context coherence is irreducible to within-context evaluation. MaxEnt, which is maximally capable at point-wise context evaluation, cannot see structure that exists *between* contexts [STRUCTURAL — the ratio is corpus-specific; the principle is structural].

**Epistemic restriction vs. frame-dependence independence.** Of 910 (constraint, context) pairs showing restricted-view divergence and 1,231 pairs showing gauge-fixedness, only 15 overlap — 1.6% in one direction, 1.2% in the other. Restricted-view divergences are information-loss errors (the observer lacks data); gauge-fixed constraints are frame errors (the observer has full data but processes it through a structural relationship). These are independent phenomena: epistemic restriction is having a reduced density matrix; frame-dependence is having a different measurement basis [STRUCTURAL].

### 4.4 Coalition Structure

The Galois connection between observer coalitions and consensus types provides a finer invariant than H¹ alone. For a given constraint, define the **agreement set** for type *T* as the set of contexts that classify the constraint as *T*, and the **consensus** of a coalition *S* as the type (if any) that all members of *S* agree on. These two maps form an antitone Galois connection between the lattice of observer coalitions and the lattice of types.

The Galois lattice per constraint captures the *structure* of observer agreement — not just whether observers disagree but *which groups form natural consensus blocs*. Two constraints with the same H¹ can have different Galois lattices: [snare, snare, rope, snare] (H¹ = 3, one dissenter against a bloc) has a qualitatively different politics of disagreement than a hypothetical [rope, snare, snare, rope] (H¹ = 4, two equal-sized blocs).

**The institutional observer as dominant dissenter.** Aggregating Galois-closed coalitions across the corpus reveals a pattern: the institutional observer (U₃) is the most frequent isolated dissenter. The data from the largest orbit families:

| Orbit family | Count | Isolated observer | Coalition bloc |
|---|---|---|---|
| {rope, snare} | [COMPUTE] | U₃ (institutional) | {U₁, U₂, U₄} |
| {rope, tangled_rope} | [COMPUTE] | U₃ (institutional) | {U₁, U₂, U₄} |
| {mountain, unknown} | [COMPUTE] | U₂, U₃ | {U₁, U₄} |

In the two largest families — together comprising a substantial fraction of the corpus — the institutional observer is the lone dissenter, seeing coordination (rope) where all other observers see extraction (snare) or entanglement (tangled_rope). This is the Galois expression of a central claim: institutional power is the structurally decisive perspective. The institutional observer, with generational time horizon and arbitrage exit options, systematically reclassifies extraction as coordination. This is not a bias to be corrected; it is a structural consequence of the institutional observer's position. From a position with the power to reform a constraint, the constraint genuinely *functions* as coordination — the extractive features are either invisible (the observer benefits from them) or irrelevant (the observer can exit them). The classification is correct *from that position*. It is incorrect from others. (The lattice computation is STRICT; the corpus-level pattern is an empirical finding.)

The **splitting degree** — the minimum number of observers needed to fully determine a constraint's type profile — provides a measure of observer redundancy not captured by H¹ or orbit families. A constraint with splitting degree 1 can be fully characterized from any single observer position; a constraint with splitting degree 4 requires every observer to contribute unique information.

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
- **H⁰ and descent.** Global sections (H⁰) are precisely the constraints satisfying the descent condition. Descent $\leftrightarrow$ H¹ = 0 is tautological on discrete covers.
- **Gauge orbits.** The Dirac orbit computation is standard orbit decomposition under the group of context automorphisms.
- **The three-way equivalence (two-out-of-three).** The Lawvere ↔ Grothendieck equivalence (naturality ↔ descent) is STRICT. The Noether column (symmetry conservation) is a productive STRUCTURAL parallel — same predicates, but mapping to a weaker mathematical condition than Noether's theorem proper (discrete group invariance, not continuous Lie symmetry with Lagrangian).
- **The Galois connection.** The coalition–consensus duality is a standard antitone Galois connection between two finite posets.
- **Hub 1 as restriction map.** The power-scaling function `derive_directionality/3` → `sigmoid_f/2` → χ implements a genuine restriction map: it transforms classification along morphisms in the site.
- **Hub 2 as classification gate.** The `effective_immutability_for_context/2` table implements a genuine classification gate: it partitions contexts into mountain-eligible and non-mountain-eligible subsets.
- **Hub independence.** Zero Type A conflicts (mountain + snare-level extraction) verified empirically across the full corpus. The BaseEps check prevents the pathological case by construction.
- **Binary gate computations.** Each binary gate is a well-defined predicate over testset declarations and relational data.
- **NMI analysis.** Normalized Mutual Information between binary gates and type space is a standard information-theoretic computation.
- **T13 divergence measurement.** The indexed-vs-classical MaxEnt divergence is a direct numerical measurement, not an inference.
- **Mountain zero-divergence.** All genuine mountains (H⁰, ε < 0.05) show exactly zero MaxEnt divergence under indexing.

### 5.2 What Is STRUCTURAL

The following correspondences guide analysis productively but lack formal verification:

- **MaxEnt as distribution on $\Omega$.** The MaxEnt classifier assigns a probability distribution over the type space at each context, which is structurally analogous to the Giry monad's action. But the full Giry monad structure — unit (Dirac embedding), multiplication (distribution over distributions), naturality of both — is not present.
- **FPN as terminal coalgebra.** The FPN equilibrium is the greatest fixed point of the contamination endofunctor, and convergence is proved via Knaster-Tarski. But the full coalgebra axioms have not been formally verified.
- **Abductive engine as naturality auditor.** The 15 trigger classes test cross-functor consistency — whether independent diagnostic views agree. The artifact/genuine distinction maps cleanly onto expected versus unexpected naturality failures. But the triggers are hand-crafted, not derived from formal categorical constructions.
- **Trajectories as natural transformation families.** Constraints with identical trajectories exhibit the same transformation behavior under context shifts, functioning as representatives of the same natural transformation. But the formal functor-category construction is absent.
- **Contamination as contravariant flow.** Contamination flows against the purity gradient, which is structurally analogous to contravariance. But the gradient reversal is of a scalar, not of categorical morphisms.
- **H¹ proxy.** The disagreeing-pairs count is a combinatorial descent-failure count on the poset site (see §4.2). It is not formal Čech H¹ — which requires the quotient ker($\delta^1$)/im($\delta^0$) and is trivially 0 on a discrete site — but measures genuine obstruction to descent on the Alexandrov site.
- **H¹ band structure.** H¹ = 3 → Hub 1, H¹ = 4 → Hub 2 is an empirical correspondence verified against the corpus. It is not derived from formal categorical structure.
- **T9–T11 triggers.** Category A disagreement detection triggers test cross-subsystem consistency. The patterns are empirically validated but hand-crafted.
- **T14–T16 triggers.** Category B agreement verification triggers test for systematic blindness in classical tools. The patterns match specific architectural findings but are not derived from formal axioms.
- **Diagnostic verdict synthesis.** The GREEN/YELLOW/RED verdict is a threshold-based aggregation of subsystem signals. The thresholds are calibrated, not derived.
- **Expected conflict catalog (P1–P11).** The 11 patterns are hand-crafted meta-predicates matching known architectural artifacts. Each is validated by the selftest against the corpus.
- **Two-tier gate architecture.** The Tier 1/Tier 2 boundary is a design decision reflecting the structural-property vs. factorization distinction.
- **100x oracle gap.** The ratio is corpus-specific; the principle (cross-context coherence irreducible to within-context evaluation) is structural.
- **Epistemic restriction vs. frame-dependence independence.** The 1.6% overlap is measured against this corpus. The independence claim generalizes the measurement.

### 5.3 What Is LOOSE

The following analogies would mislead if taken literally:

- **Type space as Heyting algebra.** Two absorbing elements (mountain and piton) prevent lattice structure. The composition is a priority monoid. A Heyting algebra claim would invite incorrect expectations about implication and complementation.
- **Power scaling as adjunction.** The sigmoid scaling creates a parametric family of type assignments, but the triangle identities (unit and counit) have not been verified. The existential/universal quantifier structure is suggestive but insufficient.
- **Signature resolution as lattice meet.** The conflict resolution predicate is a priority dispatch table lacking commutativity and associativity. It is not a lattice operation.
- **All five Girard/Linear Logic mappings.** An independent audit found that all five proposed correspondences between the codebase and Girard's linear logic are LOOSE. The systematic error across all five is the same: conflating *computing a quantity that describes a resource* with *consuming that resource*. The system is a calculator, not a cash register — it tracks costs but does not enforce budgets, which is exactly the gap identified in §5.4.
- **The quantum measurement analogy.** The presheaf structure is formally analogous to contextual truth in quantum topos theory (Isham and Butterfield 1998), but the analogy breaks at three critical points: DR classification is reversible (re-evaluate at a different context freely), deterministic (no Born rule), and local (no entanglement). The analogy is useful for intuition but actively misleading for formalism.
- **"Quantum" naming in quantum verification triggers.** Triggers T13–T16 are named "quantum verification triggers" because they were developed by applying concepts from Yuen's quantum complexity theory to the codebase architecture. The name is evocative but would mislead if taken as formal correspondence to quantum computing. The triggers test for classical oracle failures and observer-dependent classification shifts — phenomena that are *analogous* to quantum complexity gaps but do not involve quantum states, unitary transformations, or entanglement. The structural parallel (classical tools cannot see cross-context coherence) is genuine; the formal correspondence is absent.

### 5.4 What the Framework Cannot Do

The framework classifies and diagnoses. It does not:

- **Plan under resource constraints.** The system computes costs (purity-adjusted energy for reform actions, scaffold urgency scores) but does not enforce budgets. There is no concept of finite enforcement capacity, agent attention, or reform allocation. This is the genuine gap identified by the Girard/Linear Logic analysis — the system would need an ontological expansion (resource annotations on agents) and a new operational layer to close it.
- **Perform metric-level sensitivity analysis.** The configuration sensitivity sweep (§2.6) tests parameter robustness — whether the classification cascade's thresholds and modifiers are stable under perturbation. What it does not test is the robustness of the input metrics themselves: do small changes in the author-assigned extractiveness, suppression, or resistance-to-change values cause constraints to reclassify? A constraint near a type boundary might shift type with plausible metric perturbations. This metric-level sensitivity analysis remains to be performed.
- **Extend to infinite or non-linear sites.** The current site is a 4-element linear poset. Extending to a richer site — adding temporal morphisms, scope morphisms as independent dimensions, or non-linear power relationships — would require non-trivial formal work. In particular, H² and higher cohomology become non-trivial on non-discrete sites, and the H¹ gap at 1 and 2 would not persist on a non-linearly ordered site.
- **Establish causation.** The framework detects structural patterns — which constraints are perspectivally fractured, which observer positions are structurally decisive, which constraints are structurally isomorphic across domains. It does not establish *why* a constraint is extractive, *how* extraction emerged, or *whether* reform would succeed. These are causal questions that require different methods.
- **Model lateral extraction.** The power axis in the context tuple is vertical: powerless → moderate → institutional → analytical. The binary gates fire correctly on lateral extraction (peer manipulation, workplace bullying, communal narcissism), and the framework produces correct classifications when observer positions are properly differentiated via exit options and beneficiary/victim declarations. But the representation of the power axis is inadequate: the victim must be coded as "powerless/trapped" and the extractor as "institutional," which forces the vertical machinery to produce the correct result at the cost of misrepresenting the actual structural geometry. The coupling fingerprint will show coupling, but along an axis that does not correspond to the actual power relationship. Whether this requires a relational dimension in the context tuple or can be resolved within the existing parameter space (via exit options and the immutability table) is not yet determined.

### 5.5 What Would Strengthen the Framework

Several well-defined formal tasks remain. Items 1–5 were identified in v2; all remain open. Items 6–8 are new to v3.

**Progress since v2.** While none of the five v2 items have been formally closed, the framework's diagnostic capacity has expanded substantially in directions that partially address them. The abductive engine grew from 8 to 15 trigger classes, with the new triggers (T13–T16) providing the first formal bridge between classical MaxEnt evaluation and cohomological analysis — a capability that item 4 (enriched sites) would eventually formalize. The diagnostic integration system (§3.5) provides a unified assessment layer that item 3 (metric-level sensitivity) would strengthen. The two-hub architecture analysis (§2.7) and binary gate inventory (§2.8) establish the structural foundations that items 1 and 2 (restriction map verification, monoidal structure) would formalize.

1. **Formal verification of restriction maps.** Prove that the power-scaling function satisfies the functor axioms for the presheaf's restriction maps. This requires showing that the composition of scalings along a chain $U_1 \to U_2 \to U_3$ equals the direct scaling $U_1 \to U_3$. *(v2 item, still open.)*
2. **Verification of the monoidal structure.** Determine the precise algebraic structure of the type space under composition. Is the priority monoid a bounded semilattice when the piton anomaly is addressed? *(v2 item, still open.)*
3. **Metric-level sensitivity analysis.** The configuration parameter sweep (§2.6) establishes that 87% of internal parameters are inert at ±25% perturbation at the v2 corpus size. The remaining gap is metric-level sensitivity: systematically varying the input metrics (extractiveness, suppression, resistance-to-change) within plausible ranges to measure how many constraints reclassify. This would quantify robustness at the input layer rather than the parameter layer. *(v2 item, still open. The diagnostic integration system provides partial coverage: RED verdicts may correlate with metric-boundary proximity.)*
4. **Extension to enriched sites.** Adding temporal and scope dimensions as independent morphisms would create a product site with non-trivial higher cohomology. H² would measure whether patterns of perspectival disagreement are themselves perspectival — a second-order invariant. *(v2 item, still open.)*
5. **The sheafification question.** The presheaf $F$ can be sheafified to produce the "closest" sheaf $F^+$, for any Grothendieck topology $J$ on the site. Sheafification forces descent — it produces a consensus classification by resolving perspectival disagreements into globally consistent sections. The kernel and cokernel of the sheafification map $F \to F^+$ would quantify how much perspectival information is lost by demanding global truth. For the [COMPUTE: H⁰ count] constraints already in H⁰, sheafification changes nothing; for the [COMPUTE: non-H⁰ count] with H¹ > 0, it would force a choice that the presheaf currently refuses to make. The question is whether the forced consensus is informative (revealing what "the truth" would be if we demanded one) or destructive (erasing the diagnostic signal). *(v2 item, still open.)*

   A worked example clarifies the stakes. Consider the dominant orbit [snare, snare, rope, snare] ([COMPUTE: count] constraints, H¹ = 3). The institutional observer (U₃) sees coordination; the other three see extraction. Under topology J₁, where {U₁, U₂, U₄} is a covering family (majority rule), sheafification forces "snare" as the global type. Under topology J₂, where {U₃} alone is a covering family (institutional authority), sheafification forces "rope." The choice of Grothendieck topology IS the choice of whose perspective is definitive. The DR framework's decision to remain a presheaf — to refuse sheafification — is the decision to preserve the diagnostic signal rather than force a consensus.

6. **Versioned corpus analysis.** Drift and trajectory analysis become most valuable when comparing across corpus versions. The corpus grew from 1,023 (v2) to 1,142 (v3) constraints; tracking how constraint classifications change as stories are revised would enable temporal trajectory analysis. Currently the drift/trajectory subsystem is calibrated for within-version analysis. *(New to v3.)*
7. **Lateral extraction formalization.** The framework handles lateral extraction correctly when inputs are properly specified (§5.4), but the power axis must be distorted to produce correct results. A relational dimension in the context tuple — representing the relationship between two same-level observers, not the power level of either — might address this without distorting the existing geometry. *(New to v3.)*
8. **Diagnostic summary → abductive feedback loop.** A constraint with a red verdict that fires zero abductive triggers would itself be a meta-anomaly worth flagging. This creates a feedback loop between the synthesis layer and the diagnostic layer — the diagnostic integration system would become both a consumer and a producer of abductive signals. *(New to v3.)*

---

## 6. Related Work

The framework sits at the intersection of several traditions. Positioning it precisely — what it borrows, what it adds, what it lacks — is essential for honest scholarship.

**Standpoint epistemology** (Harding 1986; Haraway 1988) argues that knowledge is perspectival — that the social position of the knower shapes what can be known. DR formalizes this claim by providing a presheaf-theoretic framework where the "standpoint" is a point of the site and the "knowledge" is the stalk of the presheaf at that point. The formalization adds what standpoint epistemology lacks: quantitative invariants. The descent rate measures *how much* of a domain is perspectival. The H¹ distribution measures the *structure* of perspectival dependence. The Galois lattice identifies *which* standpoints are structurally decisive. What DR lacks is standpoint epistemology's rich account of how standpoints are constituted — the site's four standard contexts are stipulated, not derived from social theory.

**Social choice theory** (Arrow 1951; Sen 1970) studies the aggregation of individual preferences into collective decisions. DR's type space has some structure in common with preference aggregation — the composition operation resolves conflicts between types — but the type space is not a preference ordering and the composition is not a social welfare function. Arrow's impossibility theorem shows that no aggregation rule satisfies a set of desirable axioms; DR's analogue is the absence of snares from H⁰, which shows that no observer position universalizes the extractive classification. The formal structures are different (preference lattices vs. presheaves on sites), but the impossibility results are spiritually related.

**Institutional analysis** (Ostrom 1990) classifies institutions by their rules, boundaries, and governance structures. DR classifies constraints by their structural character as seen from different observer positions. The two approaches are complementary: Ostrom asks "what kind of institution is this?" from a single analytical perspective; DR asks "what kind of institution does this look like to different observers?" Ostrom's framework could serve as the institutional semantics that DR currently lacks — providing rich descriptions of the constraints that DR classifies structurally.

**Topos-theoretic approaches in physics** (Isham and Butterfield 1998; Doring and Isham 2008) apply presheaf theory to quantum mechanics, modeling contextual truth — the idea that the truth value of a proposition depends on the measurement context. DR applies the same formal structure to a different domain: the "measurement context" is an observer's social position rather than an experimental apparatus, and the "proposition" is a constraint classification rather than a quantum observable. The formal parallels are genuine (both are presheaves on sites of contexts), and the philosophical parallels are instructive (both formalize the idea that truth is local to a context). The key disanalogy is that quantum measurement involves irreversibility (wave-function collapse), stochasticity (Born rule), and entanglement (non-local correlations), none of which are present in DR.

**Wheeler's participatory universe** (Wheeler 1989) posits that physical reality is constituted by information and that the observer's choice of measurement apparatus determines the observed reality. DR's formal structure matches Wheeler's thesis at the level of presheaf evaluation: the "apparatus" is the observer context, the "measurement" is classification, and the "reality" depends on the choice. The descent rate is the quantitative answer to Wheeler's question — how much of a domain requires an observer to determine the facts? — for the domain of social constraints. The disanalogy is equally informative: Wheeler insists on *free choice* of measurement, while DR has *constrained positionality* (a powerless observer does not *choose* to see a snare; they see it because their structural position constrains their perspective). This identifies exactly where DR formalizes participatory observation and where it formalizes standpoint epistemology.

**Computational social science** typically treats classification as a supervised learning problem: given labeled training data, learn a classifier that generalizes. DR takes a fundamentally different approach: classification is not learned from data but computed from continuous metrics via a hand-designed, deterministic rule cascade (not a trained classifier), and the central question is not "which label is correct?" but "how does the label depend on who is labeling?" The framework is computational — it is implemented as a Prolog codebase of 77 modules that runs on corpora of over 1,000 constraints — but it is not machine learning. Its invariants (descent rate, H¹ distribution, structural families) are formal properties of the classification presheaf, not performance metrics of a learned model.

**Quantum complexity theory.** Yuen's unitary synthesis problem asks whether unlimited classical computational power can simulate quantum state transformations, conjecturing that classical and quantum complexity may be logically independent. The 100x classical oracle gap (§4.3) is a corpus-level empirical instance of an analogous structural principle: MaxEnt, which is maximally capable at point-wise context evaluation, detects ~1% of the observer-dependence that cohomological analysis detects, because observer-dependence is relational structure between contexts rather than a property of any individual context. The shared principle — that cross-context coherence is irreducible to within-context power — is **STRUCTURAL**. The disanalogies are significant: Yuen's gap is conjectured, involves formal complexity classes, and concerns an infinitely powerful classical oracle; the DR gap is empirical, concerns diagnostic coverage, and uses a specific finite classifier. The analogy clarifies *why* the oracle gap exists (MaxEnt is structurally blind to cross-context coherence) without importing formal claims from quantum complexity theory.

---

## 7. Conclusion

Classification of social structures depends irreducibly on who is observing. This paper has shown that this dependence has formal mathematical structure: it is a presheaf on a site of observer positions, and the standard tools of topos theory — cohomology, descent, naturality — apply and produce quantitative invariants.

The framework's formal spine is the three-way equivalence: Boltzmann factorizability = Lawvere naturality = Grothendieck descent = Noether symmetry conservation. Every diagnostic layer — MaxEnt entropy, abductive analysis, trajectory mining, cohomological computation, diagnostic integration — measures proximity to or deviation from this invariance condition, which connects the framework's most rigorous computational test to three of the deepest ideas in twentieth-century mathematics.

Applied to the 1,142-constraint social-constraints corpus, the framework yields a descent rate of [COMPUTE: H⁰/1142] and the complete absence of extractive constraints from H⁰. The second finding carries the heavier philosophical weight: extraction is never observer-independent because it structurally requires at least one powerful observer position from which it is reclassified as coordination. But a clarification is warranted: the framework does not explain *why* extraction requires perspectival cover — that remains a sociological claim requiring domain theory. What the framework provides is the formal machinery to establish *that* it does, to measure *how much* it does, and to identify the specific coalition structure of the cover. The categorical vocabulary organizes the diagnostic machinery; explanation requires a theory of the domain.

The two-hub architecture (§2.7) is a stronger result than v2's implied single mechanism for observer-dependence. Observer-dependence enters through exactly two independent channels — continuous power-scaled extraction and discrete immutability perception — whose independence is verified empirically and whose interaction produces the framework's most diagnostic findings (false mountains, hub-conflict constraints, the H¹ band structure). The architecture reveals that mountains and snares occupy opposite poles of an indexicality spectrum: mountains are fully classical (recoverable from non-indexed data), snares are fully indexed (invisible without observer position).

The 100x classical oracle gap (§4.3) establishes that cohomological analysis is not optional. A maximally capable within-context classifier detects only ~1% of the observer-dependence that cross-context analysis reveals. The gap exists because observer-dependence is relational structure between contexts, not a property visible from any single context. This finding transforms the framework's cohomological layer from a theoretically motivated addition into a practically essential component.

The diagnostic integration system (§3.5) moves the framework from individual diagnostic layers to unified assessment. Twelve subsystems, 15 abductive trigger classes, and 11 expected conflict patterns synthesize into a traffic-light verdict — green for consistent, yellow for tensioned, red for multiply contradicted — without altering classification. The 50.5/47.8/1.7% green/yellow/red distribution provides analysts with a principled triage: red constraints are where the gap between hypothesis and structural analysis is largest.

These numbers are properties of one corpus under one calibration. The framework itself is general. Wherever classification depends on observer position — medical diagnosis varying with clinical perspective, legal interpretation varying with jurisdictional context, risk assessment varying with stakeholder position — the same presheaf construction applies. A different domain would produce a different descent rate, a different H¹ profile, different structural families — but the formal machinery that computes them is domain-independent. The site, the type space, and the metric assignments are domain-specific; the invariants they yield are instances of a general theory.

We close with the open question that the framework itself raises: should the presheaf be sheafified? Sheafification would force descent — it would produce a "consensus classification" where perspectival disagreements are resolved into globally consistent sections. But forcing consensus would destroy the diagnostic signal. The framework's value lies precisely in measuring perspectival fracture — in quantifying the gap between local truth and global truth, and in identifying the structural patterns in that gap. The descent rate, the H¹ distribution, the absence of snares from H⁰, the institutional observer as dominant dissenter, the 100x oracle gap — these are features of the presheaf's failure to be a sheaf. Sheafification would erase them. The framework measures perspectival fracture, not to resolve it, but because the fracture itself is where the structural information lives.

---

## References

Arrow, K. J. (1951). *Social Choice and Individual Values*. Wiley.

Doring, A., & Isham, C. J. (2008). "What is a thing?": Topos theory in the foundations of physics. In B. Coecke (Ed.), *New Structures for Physics*, Lecture Notes in Physics, vol. 813, pp. 753–937. Springer.

Girard, J.-Y. (1987). Linear logic. *Theoretical Computer Science*, 50(1), 1–102.

Haraway, D. (1988). Situated knowledges: The science question in feminism and the privilege of partial perspective. *Feminist Studies*, 14(3), 575–599.

Harding, S. (1986). *The Science Question in Feminism*. Cornell University Press.

Isham, C. J., & Butterfield, J. (1998). A topos perspective on the Kochen–Specker theorem: I. Quantum states as generalized valuations. *International Journal of Theoretical Physics*, 37(11), 2669–2733.

Lawvere, F. W. (1969). Adjointness in foundations. *Dialectica*, 23(3–4), 281–296.

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic: A First Introduction to Topos Theory*. Springer.

Noether, E. (1918). Invariante Variationsprobleme. *Nachrichten von der Gesellschaft der Wissenschaften zu Gottingen*, 235–257.

Ostrom, E. (1990). *Governing the Commons: The Evolution of Institutions for Collective Action*. Cambridge University Press.

Sen, A. K. (1970). *Collective Choice and Social Welfare*. Holden-Day.

Wheeler, J. A. (1989). Information, physics, quantum: The search for links. In W. H. Zurek (Ed.), *Complexity, Entropy, and the Physics of Information*, pp. 3–28. Addison-Wesley.

Yuen, H. (2023). A quantum complexity-theoretic reduction for the unitary synthesis problem. arXiv:2306.13073.
