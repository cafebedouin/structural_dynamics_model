# Axioms and Consequences of Observer-Dependent Classification

**A Formal Framework for Systems Where What You See Depends on Where You Stand**

**v6.6 — Cognitive displacement analysis, observer calibration infrastructure, H¹ robustness confirmation**

---

**Abstract.** We present a formal framework for classification systems where the result depends irreducibly on the observer's position. The framework models classification as a presheaf on a site of observer contexts, deliberately refusing the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved.

We separate the framework into three layers: (1) axioms — the design commitments that define the measurement apparatus, (2) theorems — structural consequences that follow deductively from the axioms without reference to any corpus, and (3) empirical observations — corpus-dependent findings that validate the engine and characterize specific datasets.

The axioms encode a single core hypothesis: power modulates the perception of extraction. The theorems derive non-obvious consequences. Extraction necessarily requires a cover story — it can never be universally recognized as extraction under power-modulated perception. Observer disagreement clusters in discrete blocs rather than distributing smoothly. The institutional observer carries 97% of the spectral weight in classification disputes. Single-position analysis with full information detects less than 3% of the observer-dependent structure that cross-position analysis reveals. These are properties of the axioms, not of any dataset.

Two independently generated corpora with inverted input distributions confirm the engine correctly computes these consequences. Structural invariants — the H¹ gap, spectral eigenvalues, contextuality fraction gap, and institutional dissent direction — are identical across both corpora and survive FCR override ablation, confirming they are fixed-point attractors of the axioms. Corpus-dependent statistics (type distributions, descent rates, coalition structure) vary between corpora as expected.

New diagnostics extend the empirical record on a 3,254-constraint corpus. Wasserstein L¹ transport provides a continuous complement to H¹, detecting sub-threshold distributional shift invisible to cohomological obstruction counting. Variance decomposition of the Axiom 2 formula confirms that directionality f(d(P)) accounts for 98.6% of inter-observer χ variance — the first direct metric-level validation that power-modulated directionality drives observer-dependent classification divergence. Parametric persistence barcodes confirm that the 14 constraints near the snare_chi_floor threshold have robust perspectival fracture across the full parameter range; the threshold crossing is a classification boundary artifact, not a distributional regime change. A game-theoretic analysis formalizes the presheaf structure as a classification game: 69.3% of non-constant orbits are Nash-stable (no unilateral reclassification resolves the disagreement), all FCR-detected cover stories are structurally forced rather than contingently manufactured, and Nash distance and H¹ persistence are sharply anti-correlated — a phase transition rather than a gradient — identifying two structurally distinct extraction regimes. The institutional observer's vulnerability concentration (94% of resolvable cases) is a structural property confirmed across the full range of power modifier calibrations. An honest assessment distinguishes strict categorical correspondences from structural analogies.

A cognitive displacement analysis introduces an intra-observer perturbation parameter δ, modeling systematic perceptual bias within a structural position. A δ-sweep across [−0.15, +0.15] confirms that all structural invariants — the H¹ gap, institutional spectral dominance, and the sign-flip mechanism — are robust to observer calibration variation. The sweep reveals that sensitivity to cognitive orientation is concentrated at the moderate observer position (U₂), where the sigmoid derivative is 4.5× higher than at the power extremes, inverting the paper's prior characterization of U₂ as the "classification anchor." The institutional observer (U₃) is the most classification-sensitive position by flip count despite having low sigmoid slope, because threshold density in the negative-χ regime amplifies small perturbations — a mechanism invisible to derivative analysis alone. The institutional Nash vulnerability concentration (94%) declines to a floor of 83% under δ perturbation, with the decline driven by a shift of vulnerability share to the analytical position rather than a redistribution across all observers. H¹ = 6 (maximum perspectival fracture) is structurally robust: 89% of the population survives the full δ range, with minor fragility concentrated in constraints where the moderate observer sits near the tangled_rope/rope boundary.

---

## 1. Introduction

The classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects. This is not a failure of classification but a structural feature of the domain.

The standard response to perspectival dependence is to resolve it — to identify the "correct" classification by privileging one observer position or aggregating across positions. This paper takes the opposite approach. We model perspectival dependence using presheaf theory, where disagreement has formal mathematical structure, and the standard tools of topos theory — cohomology, descent, naturality — produce quantitative invariants that characterize any domain where classification depends on perspective.

The framework, *Deferential Realism* (DR), is *realist* in that it treats constraints as having objective structural properties (extractiveness, suppression, coordination function) that exist independently of any observer; it is *deferential* in that it treats the *classification* of those properties as irreducibly dependent on the observer's structural position. The presheaf is emphatically not a sheaf: the gluing axiom is intentionally violated because perspectival disagreement is a diagnostic signal, not a defect. DR is a meter for perspectival fracture, not a machine for identifying the correct political line. The invariants it produces measure how disagreement is structured; they do not adjudicate whose classification is right.

The paper separates cleanly what earlier versions interleaved: §2 states the axioms as design commitments, §3 derives the theorems that follow from those axioms alone, §4 presents the computational engine, §5 reports empirical findings including a cognitive displacement analysis that tests structural invariants against intra-observer perturbation, §6 provides the honest assessment, §7 discusses related work, and §8 connects the formal results to broader implications.

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

**What would change.** The site choice is normative — it is where political commitments enter the mathematics. A Marxist analysis might separate epistemic access from material power as independent morphism dimensions. A feminist standpoint epistemology might add an embodiment axis. A non-linear site (a DAG, a branching lattice, a site with overlapping jurisdictions where a worker is powerless economically but powerful legally) would produce different invariants. The gap structure derived in Theorem 2 depends on linearity and would not hold on a non-linear site. The framework is a functor from site choices to invariants; the current 4-element chain is one instantiation. The invariants are geometry-relative — properties of this site — not world-relative assertions about the constraints themselves.

### Axiom 2: Power-Modulated Perception of Extraction ⚓ [Empirical Anchor]

The core empirical commitment: **power reduces experienced extraction.** Everything downstream of this axiom inherits its empirical status. If the real-world relationship between power and perception does not match this axiom, the theorems remain valid within the model but the model no longer describes the world.

Each constraint has a base extractiveness ε that is observer-independent — a design axiom (ε-invariance). The *experienced* extractiveness varies by observer:

$$\chi = \varepsilon \times f(d(P)) \times \sigma(S(P))$$

where d(P) is a directionality value derived from the observer's structural relationship to the constraint (beneficiary, victim, neither), f is a sigmoid function, and σ is a scope modifier encoding verification difficulty. The formula factors as (observer-independent) × (observer-dependent).

The directionality sigmoid: f(d) = L + (U−L)/(1 + e^(−k(d−d₀))) with L=−0.20, U=1.50, d₀=0.50, k=6.0. At the canonical institutional d=0.00, f(d) ≈ −0.12 — making χ negative, pushing the institutional observer below the rope threshold. This is the only observer position where f(d) can go negative under the canonical calibration, and it is the structural source of the institutional phase transition.

**Worked example.** Consider a non-compete agreement with ε = 0.70 (high extraction, worker cannot exit the constraint). At U₁ (powerless, d=1.00): χ = 0.70 × 1.42 × 0.8 = 0.79 — above the snare threshold, classified as snare. At U₃ (institutional, d=0.00): χ = 0.70 × (−0.12) × 1.0 = −0.08 — negative, below the rope ceiling, classified as rope (coordination mechanism). Same constraint, same metrics, opposite classifications: Theorem 1 instantiated in a single pair of observers.

The scope modifier σ = [0.8, 1.0, 1.0, 1.2] for [U₁, U₂, U₃, U₄] is observer-specific and constraint-independent: it encodes verification difficulty (harder to verify extraction claims at global scope) rather than constraint visibility. Making σ constraint-specific would reframe it from observer verification capacity to constraint visibility, which is a different theoretical concept requiring new per-constraint metadata.

**Why this choice.** Power asymmetry is the domain's central structural feature. The multiplicative formula ensures that observer-independent extractiveness is preserved while experienced extractiveness varies continuously with position.

**What would change.** If the empirical anchor fails — if power does not in fact reduce experienced extraction — the theorems remain derivable within the model but lose their world-referential claim. The model would remain formally coherent but empirically vacuous. A concrete falsification condition: if domain experts with direct knowledge of a constraint domain — including experts who occupy powerless structural positions — consistently rated as non-extractive the same constraints that trapped workers identify as total barriers to exit, that finding would challenge Axiom 2's claim that power position modulates perception of extraction rather than access to information. The axiom predicts a systematic pattern in which the institutional sign-flip is a structural consequence of position, not ignorance; evidence that the institutional classification is simply more accurate (that U₃ is right and U₁ is wrong about the same constraint) would require revising the empirical anchor.

### Axiom 3: Six Structural Types

Constraints are classified into six types organized around two axes: extraction and coordination. The extraction chain (mountain < rope < tangled_rope < snare) is totally ordered. Scaffold and piton are diagnostic categories rather than extraction-chain members.

**Mountain:** Constraints that appear as natural law across all observer positions. ε near zero; no extraction visible from any perspective.

**Rope:** Coordination constraints with low extraction. Observer agreement is relatively high; the constraint is experienced as legitimate structure.

**Tangled Rope:** Constraints where coordination and extraction are simultaneously present. The FCR (False CI Rope) signature detects this: cross-perspectival coupling alongside extraction.

**Snare:** Pure extraction constraints. High χ, high ε, no coordination cover. The institutional observer typically cannot classify these as snares — the cover story mechanism (Theorem 1) operates most visibly here.

**Scaffold:** Temporary structural constraints in formation. Not yet stabilized into the extraction chain.

**Piton:** Local anchoring constraints. Observer-position-specific; typically piton at U₁, rope or snare at U₃.

### Axiom 4: MaxEnt Shadow Classification

For each observer position, a maximum entropy distribution over the six types is computed alongside the deterministic classification. The MaxEnt shadow encodes classification uncertainty: when the deterministic rule cascade fires confidently, the shadow distribution is peaked; when metrics fall near threshold boundaries, the distribution is flatter.

The Boltzmann weight for each type is proportional to e^(−β × cost(type, metrics)), where cost encodes deviation from the type's metric signature. This is equivalent to requiring the functor axiom: the MaxEnt layer is the unique probability distribution compatible with the metric constraints that maximizes entropy. Boltzmann factorizability is the functor axiom in probabilistic form.

### Axiom 5: Six-Type Space Sufficiency

The six-type space is sufficient: no additional dimensions are needed to capture the observable boolean structure of constraints. Boolean feature independence testing across 3,254 constraints confirms that all six observable boolean features (emerges_naturally, requires_active_enforcement, has_coordination_function, has_asymmetric_extraction, natural_law_without_beneficiary, is_constructed) have normalized mutual information >0.3 with the type assignment and independence scores <0.15. No boolean feature meets the independence criteria that would indicate a missing dimension.

### Axiom 6: FCR Priority

When the Boltzmann independence test detects cross-perspectival coupling alongside extraction, the False CI Rope override applies a 3× boost to tangled_rope probability. This encodes the framework's prioritization of coordinated extraction detection. It is a property of the rule cascade's prioritization, not a direct measurement of social reality.

---

## 3. The Theorems

The following consequences hold within the model defined by Axioms 1–6. We call them theorems because they follow deductively from the axioms; their status as claims about the world depends entirely on Axiom 2's empirical correspondence. If the empirical anchor maps correctly to the domain, the theorems describe structural necessities; if it does not, they remain valid within the model but describe a model that does not correspond to reality. Each theorem is confirmed empirically on the current corpus, but confirmation is not what makes it true within the model — the axioms do.

A brief orientation: H¹ measures how large disagreement clusters must be (on this site, always 3 or more observers); spectral dominance measures which observer position controls the variance in disputes (97% concentrated at U₃); W₁ transport measures how far apart the underlying probability distributions are even when discrete type assignments agree; Nash distance measures the minimum coordinated reclassification effort required to make extraction invisible. These four invariants characterize the same presheaf from different angles.

### Theorem 1: Extraction Requires Perspectival Cover

Under Axiom 2 (power-modulated perception), extraction cannot be universally recognized as extraction. For any constraint with ε above the snare threshold, there exists at least one observer position at which χ falls below the snare classification boundary. The institutional observer (f(d) ≈ −0.12 at canonical d=0.00) is structurally guaranteed to see coordination rather than extraction for constraints where other observers see snares.

**Corpus confirmation:** All 560 snares in the 3,254-constraint corpus have H¹ > 0 — no snare achieves a global section (H⁰ = 1). The contextuality fraction for snares is 1.0: every snare lacks global perceptual agreement. This is a necessary consequence of Theorem 1, not a finding.

### Theorem 2: H¹ Gap Structure

On the 4-element linear site, H¹ values 1 and 2 are forbidden. Disagreeing observer pairs form blocs of size ≥ 3 (in the combinatorial H¹ proxy sense). Perspectival disagreement does not distribute smoothly — it clusters in discrete blocs determined by the site geometry.

**Corpus confirmation:** The H¹ distribution takes values {0, 3, 4, 5, 6} exclusively. Values 1 and 2 are empirically absent. The contextuality fraction (CF = H¹/6) takes values only at {0, 0.5, 0.667, 0.833, 1.0}, confirming that values 1/6 and 1/3 are structurally forbidden.

**Independent corroboration:** Boundary non-normality testing provides independent empirical evidence that types are discrete structural categories rather than arbitrary cuts through a continuous distribution. All type boundaries reject normality (Shapiro-Wilk p = 0.0 for all testable boundaries). The tangled_rope→snare boundary follows beta(0.16, 1.33) with skewness 4.34: a spike near zero with a thin tail. If the type space were a continuous manifold, boundary distributions would be approximately normal by the central limit theorem. The beta distribution fit is the signature of discrete structural categories separated by a genuine gap — consistent with Theorem 2's prediction.

**Game-theoretic interpretation:** The forbidden H¹ values {1, 2} correspond to Nash distances that cannot exist on a 4-observer extraction-chain site. A constraint with H¹ = 1 would require a single disagreeing pair, but the site geometry forces disagreement into blocs of at least 3. This gives Theorem 2 an independent game-theoretic derivation from the Nash distance formalism.

### Theorem 3: Institutional Spectral Dominance

The institutional observer (U₃) carries 97% of the spectral weight in classification disputes. The Sheaf Laplacian applied to the path-graph site has eigenvalue structure dominated by the U₂→U₃ edge, reflecting the phase transition at the institutional position.

**Corpus confirmation:** r₂₃² spectral weight = 97%, identical across both corpora. The institutional observer is the dominant dissenter in classification disputes, confirmed in the band-hub correspondence: H¹ = 3 band is dominated by Hub 1 sigmoid-driven divergence (institutional observer sees rope where others see extraction).

**Game-theoretic extension:** The institutional observer is the vulnerable position in 94% of resolvable Nash cases (237/252 cases where a single-observer type change would achieve H¹ = 0). A π sensitivity sweep confirms this concentration is structural: it holds as a plateau across f(d) ∈ [−0.30, +0.60], with the sign change at zero producing no detectable effect. The concentration is maintained by two independent mechanisms — metric inversion (f(d) negative at canonical d places χ below rope_chi_ceiling) and signature assignment (naturalized/scaffold types at U₃ are π-invariant). The null model at f(d) = +1.50 produces equipartition (~33% each) among U₁, U₃, U₄, with U₂ near zero throughout the sweep. The moderate observer is the classification anchor of the system: its d value places it in the sigmoid's stable region where perturbations rarely cross thresholds.

### Theorem 4: Oracle Gap

Single-position analysis with complete information detects less than 3% of the cross-position structure revealed by multi-position analysis. An analyst at any single observer position — even with full information about the constraint's metrics — cannot recover the perspectival disagreement structure without access to other observer positions' classifications.

**Corpus confirmation:** The oracle gap rate (fraction of cross-position structure invisible from any single position) is confirmed in both corpora. The gap concentrates in the upper power chain: boundary density at positions 2 (U₂→U₃) and 3 (U₃→U₄) accounts for 80% of all type transitions, with position 1 (U₁→U₂) accounting for 20%.

---

## 4. The Computational Engine

### 4.1 The Rule Cascade

The engine implements Axioms 1–6 as a deterministic rule cascade over continuous metrics. For each constraint × observer pair, it computes χ = ε × f(d(P)) × σ(S(P)), then applies threshold gates in priority order: mountain (ε near zero), snare (χ and ε above floor), tangled_rope (χ in mid-range with coordination), rope (χ below ceiling). Structural signatures (false_natural_law, false_ci_rope, institutional_dissent) override metric-based classification when detected.

The cascade is deterministic: given the same metrics and parameters, it always produces the same output. The MaxEnt shadow runs in parallel, computing probability distributions over types at each observer position. When the shadow disagrees with the deterministic classification (568 hard disagreements across 3,254 constraints), this flags a constraint near a classification boundary.

### 4.2 Structural Signatures

Three signatures detect structural patterns that override metric-based classification:

**False Natural Law (FNL):** Detects constraints classified as mountain by metric but exhibiting beneficiary structure inconsistent with natural law. Reclassifies to naturalized. This is Hub 2 — the immutability gate.

**False CI Rope:** Detects cross-perspectival coupling alongside extraction. The Boltzmann independence test measures whether the joint distribution of observer types deviates from independence; when it does alongside extraction, the constraint has the tangled_rope signature regardless of metric position. This is Hub 1 — the sigmoid-driven divergence gate.

**Institutional Dissent:** Flags constraints where the institutional observer's classification diverges from all other observers. Within the 109 institutional_dissent constraints, a binary split emerges: 105 low-snare (P(snare) = 0.0, orbits = rope + tangled_rope) versus 4 high-snare (P(snare) = 0.917, orbits = rope + snare). The split is driven by base_extractiveness with rank-biserial r = 0.9976 (p < 0.001): the institutional observer's informational advantage operates in a specific extractiveness range (ε < 0.62). Above that range, extraction dominates all perspectives.

### 4.3 Corpus Provenance

The corpus is generated by large language models given constraint descriptions and asked to assign metrics (ε, suppression, theater ratio) and structural properties. Two corpora were generated with inverted input distributions: Corpus A (Haiku, tangled_rope-dominated inputs) and Corpus B (Flash, snare-dominated inputs). The inversion tests whether structural invariants are properties of the engine or properties of the input distribution.

**Living corpus note.** The corpus is not static. Each analytical run generates new constraint stories added to the active testset. The corpus contained 907 (Haiku) and 887 (Flash) constraints when the cross-corpus comparison was first computed; at time of writing it contains 3,254 active constraints. Corpus-level statistics reported in §5 are snapshots at the time of computation. Structural invariants (§5.2 invariant table) are stable across corpus growth; corpus-dependent statistics (§5.3) shift as the corpus evolves.

**Deduplication note.** A bug in `known_constraint/1` caused Prolog backtracking to yield each constraint ID multiple times. Fixed in v4.2 via findall/sort wrapper. All statistics use deduplicated counts.

**LLM priors note.** Both corpora inherit whatever latent political grammar their training data shares. The structural invariants under inversion demonstrate that axiom-derived properties are stable across different LLM-generated corpora. The statistics that vary are properties of the data; the invariants that hold are properties of the axioms.

---

## 5. Empirical Findings

### 5.1 Engine Validation

**Structural stability.** A bifurcation sweep over [0.5×, 2.0×] of baseline values finds that 148 of 154 numeric parameters produce zero type-label flips across the entire range. Six parameters have critical values within range. The asymmetry in rope_chi_ceiling — six times more sensitive upward than downward — indicates corpus clustering near the upper boundary of rope classification. The parameter previously flagged as critical (power_modifier_analytical) was a timeout artifact: 37 tests that never ran within the 600-second wall, not 37 classification failures.

**Parametric persistence.** A grid sweep (60 points per parameter with adaptive refinement near critical values) produces persistence barcodes: birth/death intervals for H¹ features as parameters vary. For the 14 constraints that flip at snare_chi_floor (0.8% below baseline), H¹ persists across 100% of the sweep range [0.33, 1.32]. The H¹ phase structure — H¹=5 at low values, H¹=6 in a narrow band near the critical value, H¹=5 above — shows that the type flip changes the analytical context's label without altering the constraint's perspectival fracture structure. W₁ transport on the U₃→U₄ edge stays below 0.01 throughout: the threshold crossing is a classification boundary artifact, not a distributional regime change. These constraints are structurally robust; the parameter sensitivity is superficial.

**Wasserstein L¹ transport.** The Wasserstein distance W₁ between adjacent observers' MaxEnt distributions provides a continuous complement to discrete H¹ counting. For discrete distributions over the extraction chain, W₁ reduces to L¹ distance between CDFs — computable without linear programming. H¹ and W₁ measure genuinely different features and diverge in both directions. Constraints with H¹ = 3 can have W₁ ≈ 0 (discrete type-switching invisible to continuous distributions — the classifier commits to a different label but with similar uncertainty profiles, indicating a classification boundary artifact). Constraints with H¹ = 0 can have W₁ = 0.99 (unanimous discrete classification masking substantial distributional shift — observers agree on the label but their underlying probability mass is moving, a sub-threshold fracture H¹ cannot detect). The boundary distribution confirms that transport costs concentrate at the U₂→U₃ and U₃→U₄ edges (80% of transitions), consistent with the spectral dominance of the institutional phase transition.

**Axiom 2 metric validation.** Variance decomposition of χ = ε × f(d(P)) × σ(S(P)) across 3,252 constraints (1 null excluded, 1 missing data) confirms that directionality f(d(P)) accounts for 98.6% of inter-observer variance in experienced extractiveness, with scope σ(S(P)) contributing 6.5% and a cross-term of −5.1%. The negative cross-term reflects the site geometry's partial cancellation: observers with high f(d) (U₁, victim position) have low σ (local scope), while U₄ (global scope) has the largest σ but a directionality value closer to neutral. Critically, the 98.6% figure is not merely a consequence of f(d) varying more than σ in magnitude: it reflects the sign reversal at U₃ — the qualitative inversion that drives the cover story mechanism — which σ, a positive scalar bounded between 0.8 and 1.2, cannot produce. The pattern holds across all constraint types: 95.4% of constraints are directionality-dominated, and no type falls below 94.6% f(d)-dominance. The scope modifier σ is architecturally constant ([0.8, 1.0, 1.0, 1.2] for all 3,253 constraints): it encodes observer verification capacity, not constraint visibility. A constraint-specific σ would require new per-constraint scope metadata and is expected to have minimal classification impact given that σ accounts for less than 7% of inter-observer χ variance and all scope parameters are classification-inert under ±25% perturbation. This decomposition is the first direct metric-level confirmation that power-modulated directionality, not scope amplification, is the primary driver of observer-dependent classification divergence.

### 5.2 Convergence Under Inversion

**Structural invariants under inversion.** The strongest empirical validation: structural invariants derived from the axioms are identical across two corpora with opposite input distributions. Corpus-dependent statistics diverge as expected, confirming that the invariants are properties of the framework rather than properties of any particular dataset.

**Type distribution:**

| Type | Flash metric | Flash post | Haiku metric | Haiku post |
|------|-------------|-----------|-------------|-----------|
| snare | 448 (50.5%) | 109 (12.3%) | 152 (16.8%) | 175 (19.3%) |
| tangled_rope | 287 (32.4%) | 549 (62.0%) | 619 (68.2%) | 567 (62.5%) |
| mountain | 139 (15.7%) | 139 (15.7%) | 129 (14.2%) | 129 (14.2%) |

The type distributions after signature integration differ between corpora. Mountains are invariant under signature integration (Δ = 0 in both). This separation is the point: what the axioms guarantee is invariant; what the corpus contributes varies.

**Role of the FCR override.** The FCR applies a 3× boost to tangled_rope probability when the Boltzmann independence test detects cross-perspectival coupling alongside extraction. FCR ablation shows the override shifts type distributions asymmetrically (greater effect on snare-heavy corpora) without altering structural invariants. The H¹ gap holds in both corpora with FCR disabled. The FCR redistributes type labels within existing orbit families without altering the presheaf's disagreement topology. The FCR operates within the structure the axioms create rather than generating that structure.

**Game-theoretic confirmation:** Cover story analysis confirms that all 810 FCR-detected constraints with H¹ > 0 are Nash-forced: disabling the FCR override changes only 38/810 orbits, and even those maintain their H¹ value. The institutional "cover story" is structurally compelled, not contingently manufactured. FCR is a detection mechanism identifying constraints where the institutional position is playing a structurally forced strategy, not a game-theoretic move that creates advantageous classifications.

**Complete invariant table:**

| Finding | Status |
|---------|--------|
| Sheaf Laplacian eigenvalues | Identical to 4 decimal places |
| Spectral gap λ₂ = 0.0152 | Identical |
| r₂₃² spectral weight = 97% | Identical |
| H¹ gap at values 1 and 2 | Present in both |
| CF distribution gap (no values at 1/6, 1/3) | Confirmed in both |
| FCR ablation invariance of H¹ gap | Confirmed in both |
| Mountain population | ~14–16% in both |
| Institutional dissent direction (U₃ sees rope) | Confirmed in both |
| Zero Type A hub conflicts | Confirmed in both |
| Monotone orbit rate among non-constant comparable | ~1.1% in both |
| Boundary density: positions 2+3 vs. position 1 | ~80% vs. ~20% in both |
| H¹ = 6 population: 100% incomparable | Confirmed in both |

**Monotonicity invariants.** Power-chain monotonicity analysis finds that monotone orbits are vanishingly rare: 3 of 274 non-constant comparable orbits (1.1%). The dominant non-constant pattern is non-monotone (116/274 = 42.3%). Classification along the power chain is not a simple gradient; the sigmoid's nonlinearity produces direction reversals in the majority of gauge-variant constraints. Boundary distribution confirms that type transitions concentrate in the upper half of the power chain: positions 2 and 3 account for 80% of all 661 boundaries. The spectral dominance of the institutional observer expresses itself empirically as boundary density concentration in the upper power chain.

### 5.3 Corpus-Dependent Findings

**Type distribution (3,253 classified constraints; 1 null excluded).** Tangled_rope is the dominant type at 68.3% (2,221 constraints), followed by snare at 17.2% (560), mountain at 12.3% (401), rope at 1.4% (46), piton at 0.4% (14), and scaffold at 0.3% (11). Type counts reflect pipeline claimed_type at time of analysis. The tangled_rope population decomposes bimodally on the coordination-extraction balance: 454 rope-lean (20.4%), 1,620 genuine (72.9%), 147 snare-lean (6.6%).

**Descent and H¹ distribution.** Descent rate (fraction of constraints achieving H⁰ = 1, global perceptual agreement) is 74.8%. The H¹ distribution: {0: 2,434, 3: 255, 4: 50, 5: 362, 6: 153}. Values 1 and 2 are absent, confirming the gap structure at corpus scale. All 153 maximal-obstruction constraints (H¹ = 6) are incomparable — classified outside the extraction ordering at at least one observer position.

**Coalition structure.** The institutional observer is the dominant dissenter. The spectral decomposition confirms that U₃ accounts for 97% of the variance in classification disputes. Coalition patterns cluster around the institutional phase transition: most disagreements oppose U₃ to {U₁, U₂, U₄}.

**Post-override tangled_rope rate.** After FCR integration: ~62% in Corpus A, ~63% in Corpus B. This is a corpus-dependent statistic, not a structural invariant.

### 5.4 Extended Diagnostics

**Boundary non-normality and Theorem 2.** If types are discrete structural categories (as Theorem 2 implies), then the MaxEnt P(rival) distribution at type boundaries should be non-normal — constraints should cluster definitively on one side of a boundary, not scatter continuously across it. Raw P(rival) distributions at all type boundaries universally reject normality (Shapiro-Wilk p = 0.0 for all testable boundaries). The tangled_rope→snare boundary (N = 1,403) follows beta(0.16, 1.33) with skewness 4.34: 1,200 of 1,403 constraints have P(snare) < 0.05, while 46 have P(snare) > 0.85. The snare→tangled_rope boundary (N = 513) is bimodal, best fit by beta(0.14, 0.24): 298 constraints cluster below P(tangled_rope) = 0.05, and 107 above 0.90. Coalition type and snare-cluster membership are not independent (χ² = 217.39, df = 4, p < 0.001). The beta distribution fit is the signature of discrete structural categories separated by a genuine gap — consistent with Theorem 2's prediction and independent of the cohomological derivation.

**Institutional dissent binary split.** Within the 109 institutional_dissent constraints, base_extractiveness discriminates the groups with r = 0.9976 (p < 0.001). The institutional observer's informational advantage operates in the range ε < 0.62; above that threshold, extraction dominates all perspectives. The low-snare group spans 81 unique domains; the high-snare group concentrates in 8 geopolitical/economics/labor domains, suggesting the extractiveness threshold interacts with domain structure.

**Classification confidence architecture.** Mountain classification is 100% deep-band, tangled_rope 98.7% deep, but snare is only 20.5% deep and 77.9% borderline. The MaxEnt shadow assigns higher probability to tangled_rope than to snare for 540 of 560 snare constraints. The confidence asymmetry reflects the FCR override's structural role: it reclassifies metric-snares that exhibit cross-perspectival coupling, producing confident tangled_rope assignments from uncertain snare inputs. The corpus-wide distribution is strongly bimodal: 462 constraints at confidence 0–5% and 2,033 at 90–95%.

**Boolean feature independence.** All six observable boolean features have normalized mutual information > 0.3 with the type assignment and independence scores < 0.15 (N = 3,253 — 1 constraint excluded for missing boolean features; χ² p = 0.0 for all features). No boolean feature meets the independence criteria that would indicate a missing dimension in the type space. The type space captures the observable boolean structure without requiring additional dimensions.

**Network topology and contamination propagation.** The coupling graph has 31,215 edges across 3,254 constraints, forming 127 connected components at threshold 0.500. The coupling graph extends beyond direct constraint pairs to include multi-hop reachable nodes, yielding a largest component of 8,650 nodes. Multi-hop contamination simulation: 6,033 unique nodes reached within 3 hops (69.7% of the giant component). Fixed-point network purity propagation (Knaster-Tarski) converges in 18 iterations with 1,568 zone migrations. The contamination is type-selective: mountains have zero average EP shift, while snare and tangled_rope shift ~0.14 — consistent with mountains being pure natural-law constraints insulated from the coupling network.

### 5.5 Game-Theoretic Structure

Von Neumann's insight in game theory is that when the environment contains other optimizing agents, the interaction requires a mediating structure irreducible to either player's perspective. DR's presheaf is the same structure: the classification of a constraint is mediated by the site, not reducible to any single observer's view. H¹ obstruction measures how far local classifications fail to compose into a global one. This section formalizes the game-theoretic structure latent in the presheaf. Observers are not players with utility functions; the classification game is not a strategic interaction in the classical sense. The Nash distance formalism measures structural distance to consensus — the minimum coordinated reclassification effort required to make extraction globally invisible — which is a related but distinct concept from Nash equilibrium.

**Nash equilibrium orbits.** For each non-constant orbit (820 constraints), the structural Nash distance is the minimum number of single-observer reclassifications (to any chain type) required to achieve global consistency (H¹ = 0). Of 820 non-constant orbits, 568 (69.3%) are Nash-stable (distance ≥ 2): no single observer's unilateral reclassification can render the extraction invisible. Nash-stable disagreement orbits correspond to extraction disputes that cannot be dissolved by any single actor changing their story — the disagreement is structurally locked. The institutional position is the uniquely vulnerable observer in 237 of 252 resolvable cases (94%), confirming that the institutional phase transition is the structurally thinnest point of the presheaf. H¹ perfectly determines Nash distance: H¹ ∈ {3} maps to distance 1, H¹ ∈ {4,5} to distance 2, and H¹ ∈ {6} to distance 3. The forbidden H¹ values {1, 2} correspond to Nash distances that cannot exist on a 4-observer extraction-chain site — a game-theoretic derivation of the H¹ gap theorem independent of the cohomological argument.

**Institutional vulnerability is structural.** A π sensitivity sweep varying the institutional power modifier f(d) across [−0.30, +1.50] confirms the 94% concentration is not a calibration artifact. U₃ vulnerability remains at 99.2% for f(d) ∈ [−0.30, +0.60]: the sign change at zero produces no detectable effect. Two independent mechanisms sustain the concentration throughout this plateau: 131 metric-based cases where institutional χ falls below rope_chi_ceiling regardless of sign, and 106 signature-based cases where naturalized/scaffold assignments are π-invariant by construction. The transition at f(d) ≈ 0.65 increases rather than decreases the resolvable count (252 → 321), as resolving some disagreements reconfigures others — the classification game is not monotone in the institutional power modifier. A cognitive displacement sweep (§5.6) provides independent confirmation: under uniform δ ∈ [−0.10, +0.10], where all four observer positions shift simultaneously, institutional vulnerability remains above 83%, declining as a gentle gradient rather than a cliff. The decline is driven by a specific mechanism: at negative δ, the analytical observer absorbs vulnerability share (rising from 2.4% to 14.1% at δ = −0.10), while the moderate observer is never the vulnerable position at any δ value. The two sweep types — π (single-position f(d) variation) and δ (all-position d variation) — test different structural claims and both confirm institutional vulnerability concentration as a robust property.

**Persistence-Nash anti-correlation and two extraction regimes.** Nash distance and H¹ persistence resistance are strongly anti-correlated (Spearman ρ = −0.657, p = 1.2×10⁻¹⁰²). Resistance rates: nash=1 at 83.3%, nash=2 at 13.2%, nash=3 at 2.4%. The nash=1→2 transition is non-linear: linear interpolation predicts 42.9% resistance at nash=2; actual is 13.2%. The drop is six times steeper from nash=1→2 than nash=2→3, identifying a phase boundary rather than a continuous gradient — consistent with the H¹ gap theorem marking the boundary between structurally distinct stability regimes.

The anti-correlation supports a two-regime interpretation with structural backing. The type distribution does not distinguish the regimes (both ~83% tangled_rope, ~15% snare, chi-square p = 0.60): the regimes differ in disagreement geometry, not in the structural category of the constraint.

*Positional extraction* (nash=1, 255 cases): the institutional observer holds a lone outlier classification in 94% of cases. The disagreement is structurally robust because it depends on the institutional phase transition rather than a specific metric configuration. Simple disagreements persist across parameter perturbation.

*Distributed extraction* (nash=3, 153 cases): 59% follow the single orbit pattern (naturalized, tangled_rope, rope, snare). This is not chaotic fragmentation but a stereotyped disagreement signature in which each observer position expresses a distinct relationship to extraction: naturalization at U₁ (extraction accepted as background reality), coordination-extraction at U₂, institutional cover at U₃, and full analytical visibility at U₄. Theorems 1 and 3 are simultaneously instantiated in this pattern. A cognitive displacement analysis (§5.6) confirms that H¹ = 6 is structurally robust: 89% of the population survives the full δ ∈ [−0.15, +0.15] range, with minor fragility concentrated in constraints where d(moderate) ∈ [0.50, 0.62] — near the tangled_rope/rope boundary. The four-way disagreement is maintained by the sigmoid's geometric separation of all four observer positions into distinct classification regimes.

**Mixed strategy absence.** The MaxEnt distribution at each observer position is formally a mixed strategy over types. No mixed-strategy Nash equilibrium exists for the classification game: the max total variation distance between any observer's distribution and the mean across observers is ≥ 0.75 for 770 of 820 non-constant orbits. The sole partial exception is H¹ = 4 constraints (n = 50): exactly two types split 2-vs-2 across observers, admitting a loose 50-50 mixed strategy. Perspectival fracture in DR is genuinely irreducible — it cannot be dissolved into a shared probabilistic assessment.

**Cover story analysis.** All 810 FCR-detected constraints with H¹ > 0 are Nash-forced: the institutional reclassification of extraction as coordination is structurally compelled. Disabling FCR changes only 38 of 810 orbits, and even those maintain H¹ > 0. FCR detects genuine structural extraction; it never manufactures perspectival disagreement. The 810/820 overlap between H¹ > 0 and FCR detection (98.8%) means the game-theoretic analysis of perspectival fracture is almost entirely an analysis of the FCR subpopulation — the two diagnostic systems are measuring the same structural phenomenon from different angles.

### 5.6 Cognitive Displacement Analysis

The framework treats each observer position as a point producing a single deterministic classification. But observers at the same structural position may have systematic perceptual biases — cognitive orientations that shift their effective directionality. A risk-sensitive classifier systematically over-detects extraction; a harmony-oriented classifier over-detects coordination. This is not noise but directional, characteristic bias in the classification instrument. A cognitive displacement parameter δ tests whether the framework's structural claims survive this source of variation.

**Implementation.** The directionality value d(P) is perturbed before entering the sigmoid: d_eff = clamp(d + δ, 0, 1). The perturbation is additive to d, multiplicative to nothing else. At δ = 0, the system is identical to baseline. The infrastructure supports both uniform δ (same displacement at all positions) and per-position δ (different displacements via a dynamic fact table), enabling both global sensitivity sweeps and position-isolated perturbation analysis.

**Sigmoid sensitivity is the wrong predictor of classification sensitivity.** The sigmoid derivative f'(d) at each canonical position predicts which observers are most affected by δ: U₂ (moderate, f' = 2.10) should be most sensitive, U₁ and U₃ (f' = 0.46 each) least sensitive. The actual classification flip counts under a uniform δ-sweep across [−0.15, +0.15] invert this prediction at the institutional position: U₃ produces 834 flips (most of any position) despite having the flattest sigmoid slope. The mechanism is threshold density: the institutional observer's negative-χ regime (f(d) ≈ −0.155 at canonical d = 0.00) clusters constraints near classification boundaries, so small changes in f(d) — even changes that are small because the slope is flat — cross thresholds for many constraints simultaneously. Classification sensitivity is not f'(d) but f'(d) × ε × (density of constraints near thresholds at that position). The derivative captures the first factor; the third factor dominates at U₃.

**The moderate observer is the position most vulnerable to cognitive orientation.** At U₂ (canonical d = 0.6459), a δ = 0.15 shifts f(d) by approximately 0.31 — enough to change χ by ~0.155 for a constraint with ε = 0.5. The rope_chi_ceiling (0.35) and snare_chi_floor (0.46) are only 0.11 apart, so this shift can move a constraint across the full tangled_rope band. A sharp threshold crossing at δ = +0.03 reveals that approximately 348 constraints are packed within a δ-band of 0.015 at the moderate position — constraints where two moderate-power observers with slightly different cognitive orientations would disagree on classification. This finding qualifies the characterization of U₂ as the "classification anchor" (§5.5): U₂ is the parameter-stable anchor (all 170+ parameters are classification-inert at ±25%) but the cognitively unstable position (the position where observer calibration has maximum leverage on classification output).

**Clamp boundary asymmetry.** At d = 0.00 (institutional), negative δ is clamped to zero — only positive δ has effect. At d = 1.00 (powerless), positive δ is clamped — only negative δ has effect. Each position has a "sensitive side" determined by whether δ pushes d toward or away from the sigmoid's steep center (d₀ = 0.50): powerless is sensitive to negative δ only (467 vs. 26 flips), institutional to positive δ only (653 vs. 193 flips), and moderate to both directions with asymmetry (452 positive vs. 276 negative).

**Structural invariant robustness.** A uniform δ-sweep across [−0.15, +0.15] confirms that all structural invariants survive cognitive displacement:

| Invariant | Status | Mechanism |
|-----------|--------|-----------|
| H¹ gap (values {1, 2} forbidden) | Robust | Protected by the binary nature of the institutional sign-flip |
| Institutional sign-flip (f(d) < 0) | Robust | Zero-crossing at d ≈ 0.29 requires δ > 0.29, well outside sweep range |
| Spectral dominance (r₂₃² = 97%) | Robust | Sigmoid flatness at d < 0.3 prevents institutional threshold crossings |
| Contextuality fraction gap | Robust | Follows from H¹ gap robustness |
| Orbit monotonicity rate (~1.1%) | Robust | Institutional dip too far from sigmoid midpoint to perturb |

The invariants are protected by a specific geometric mechanism: the sigmoid's shape places the institutional observer (d = 0.00) and powerless observer (d = 1.00) in flat regions where δ perturbation has minimal effect on f(d). The structural claims are genuinely properties of the axioms, not artifacts of a particular observer calibration — but this robustness depends on the sigmoid's shape, which is itself a design choice (Axiom 2).

**H¹ = 6 robustness.** A fine-grained δ-sweep targeting the 153 constraints with maximum perspectival fracture (H¹ = 6, all four observers disagree) confirms that H¹ = 6 is structurally robust. Under positive δ, all 153 constraints survive through δ = +0.15. Under negative δ, 17 of 153 (11%) drop to H¹ = 5 at δ = −0.15. The collapsing observer pair is 100% moderate↔institutional: negative δ reduces d(moderate), pushing f(d) down and dropping χ below the tangled_rope threshold, so the moderate observer falls into the institutional observer's rope classification. The 17 fragile constraints all have d(moderate) in the range 0.50–0.62 (near the tangled_rope/rope boundary), while the robust majority has d(moderate) = 0.70. Negative δ simultaneously creates up to 93 new H¹ = 6 constraints — cognitive displacement redistributes maximum perspectival fracture across the constraint space rather than eliminating it. The H¹ gap (forbidden values {1, 2}) is fully preserved across all δ values.

**Nash vulnerability and classification sensitivity are decoupled populations.** The 834 institutional classification flips and the 252 Nash-resolvable cases are largely disjoint: only 187 of 834 flips (22.4%) overlap with the resolvable set. The remaining 77% of institutional flips occur in constraints that are either constant-orbit (dominant strategy exists) or Nash-stable (distance ≥ 2). Classification sensitivity at the institutional position does not propagate into Nash vulnerability structure because the two phenomena occupy different regions of the constraint space: threshold-density flips concentrate in the high-H¹ region, while Nash resolvability lives in the low-H¹ region (distance = 1).

---

## 6. Honest Assessment

This section audits the framework: what is strict, what is analogy, what is missing, and what would strengthen it. Everything labeled STRICT is verified mechanically by the codebase. STRUCTURAL is interpretive but grounded. LOOSE should be read as metaphor only.

### 6.1 Three-Level Rigor Classification

- **STRICT**: the categorical correspondence holds mathematically. The code implements the categorical structure, and the correspondence survives formal verification.
- **STRUCTURAL**: the analogy is productive and behavior matches, but full categorical verification of the formal axioms is absent.
- **LOOSE**: the categorical language would mislead if taken literally.

### 6.2 What Is STRICT

The site, the presheaf, the naturality condition, the Boltzmann = functor axiom equivalence, the naturality witnesses (FNL, CI Rope), H⁰ and descent, gauge orbits, the Galois connection, Hub 1 as restriction map, Hub 2 as classification gate, hub independence, binary gate computations, NMI analysis, T13 divergence measurement, mountain zero-divergence, the sheaf Laplacian construction, cross-model spectral invariance, confidence band distribution, post-override attractor convergence, contextuality fraction computation (CF = 1 − descent_rate; graded CF = H¹/6; CF gap at 1/6 and 1/3), orbit monotonicity classification, incomparable orbit decomposition, bifurcation sweep critical values, FCR ablation invariance of H¹ gap, boundary non-normality at type boundaries, boolean feature redundancy with type space, W₁ transport computation and H¹/W₁ divergence, χ variance decomposition (f(d) dominance at 98.6%), persistence barcode robustness (14 snare_chi_floor constraints robust across full sweep range), Nash distance computation (structural), institutional vulnerability plateau across f(d) ∈ [−0.30, +0.60], persistence-Nash anti-correlation (ρ = −0.657), Nash-forced cover story confirmation (0 contingent cases), cognitive displacement invariant robustness (H¹ gap, sign-flip, spectral dominance all preserved across δ ∈ [−0.15, +0.15]), H¹ = 6 survival rate (89% across full δ range), Nash vulnerability δ-robustness (institutional concentration ≥ 83% across δ ∈ [−0.10, +0.10]), moderate-institutional pair as sole collapsing pair in H¹ = 6 fragility (100% of dropouts).

The three-way equivalence (Lawvere ↔ Grothendieck ↔ Noether) is two-out-of-three STRICT: naturality ↔ descent is strict; the Noether column maps to discrete group invariance, which is the precondition of Noether's theorem rather than the theorem itself.

### 6.3 What Is STRUCTURAL

MaxEnt as Markov category (pending delete-map naturality verification). Information geometry of T13 (Fisher-Rao geodesic ball applies to KL divergence, not L∞). FPN as terminal coalgebra (convergence proved via Knaster-Tarski, but full coalgebra axioms unverified). Abductive engine as naturality auditor (triggers are hand-crafted, not derived from categorical constructions). H¹ proxy (combinatorial descent-failure count on the Alexandrov site, not formal Čech H¹). H¹ band-hub correspondence (empirical, not derived). Oracle gap mechanism (architectural, corpus-specific magnitudes). Diagnostic verdict synthesis (threshold-based, not derived). AB-framework connection (the equivalence of "no global section" and "contextual" is strict; the correspondence between DR's graded CF and Abramsky-Brandenburger's contextuality fraction is structural — DR's site is more constrained than AB's general sheaf-cohomological setup, and the CF gap result has no direct analogue in the quantum contextuality literature). Two-regime extraction interpretation (positional vs. distributed) — the anti-correlation and phase transition are STRICT; the regime labels and their theoretical interpretation are STRUCTURAL. Cognitive displacement as observer calibration (the δ perturbation mechanism is strict; the interpretation that δ models cognitive orientation rather than some other source of intra-position variance is structural — the mathematical infrastructure is agnostic about the source of the perturbation). Threshold-density explanation for institutional sensitivity (the flip counts are strict; the causal attribution to density near classification boundaries is structural, inferred from the distribution of flip magnitudes rather than derived from axioms).

### 6.4 What Is LOOSE

Type space as Heyting algebra (two absorbing elements prevent it). Power scaling as adjunction (triangle identities unverified). Signature resolution as lattice meet (priority dispatch table, not lattice operation). All five Girard/Linear Logic mappings (confusing computing a quantity with consuming a resource). The quantum measurement analogy (breaks at reversibility, determinism, and locality). "Quantum" naming in verification triggers (evocative but misleading if taken formally).

### 6.5 What the Framework Cannot Do

**Plan under resource constraints.** Computes costs but does not enforce budgets — the genuine gap identified by Girard analysis.

**Validate ε against ground truth.** Axiom 2's directionality mechanism is now validated at the metric level: f(d(P)) accounts for 98.6% of inter-observer χ variance across 3,252 constraints. What remains unvalidated is whether the input metrics (ε, base suppression) accurately measure the real-world structural properties they are intended to capture. This validation requires domain-expert ground truth rather than corpus analysis: a study comparing LLM-generated ε against expert judgment on a held-out sample would test whether the empirical anchor maps to the world, which the current corpus-level validation cannot establish.

**Model intra-level dynamics.** The power chain is a static site. Power and benefit asymmetries enter as parameters of perception, not as generators of temporal flows at the same observer level. The framework captures what different observers see at an instant; it does not model U₃ broadcasting a narrative, U₁ partially internalizing it, and the type labels changing through those interactions. The cognitive displacement infrastructure (§5.6) provides a first-order model of intra-position variance — two observers at U₂ with different cognitive orientations can now produce different classifications — but it models the variance as a static perturbation, not as a dynamic process. A fully dynamic model would require temporal site extension.

**Extend to infinite or non-linear sites.** The H¹ gap, spectral structure, and Nash distance formalism all depend on the specific site geometry. Results are geometry-relative. Any 4-element linear site with a sign-reversing observer will produce a structurally similar gap pattern; a different site geometry — lattice, DAG, overlapping jurisdictions — will produce different invariants. This is a feature, not a limitation: the framework is a functor from site choices to invariants, and the current results characterize the linear site.

**Distinguish framework properties from LLM priors.** The structural invariants under inversion demonstrate that axiom-derived properties are stable across different LLM-generated corpora. Both corpora inherit whatever latent political grammar their training data shares. The invariants that hold are properties of the axioms; the statistics that vary are properties of the data.

### 6.6 What Would Strengthen the Framework

1. **Metric-level sensitivity analysis.** Testing whether input metrics (ε, suppression) are robust to small perturbations — whether classifying a constraint as snare vs. tangled_rope changes under ε perturbation of ±10%. This is the most important missing validation.

2. **Real-world corpus.** The current corpus is LLM-generated. A corpus of constraints drawn from legal, regulatory, and institutional documents — with metrics assigned by domain experts — would test whether the framework's classifications match judgments made by people with direct knowledge of the constraints.

3. **Corpus diversity.** The corpus is living and grows with each analytical run. D-pattern concentration should be monitored; new constraints generated from analytical use naturally diversify the distribution.

4. **Non-linear site extension.** A DAG site (where a worker can be economically powerless but legally powerful) would produce different invariants. Understanding which results survive the linearity relaxation would identify which theorems are fundamental and which are geometry-relative.

5. **Temporal site extension.** Constraints change over time: a tangled_rope may naturalize into a mountain, or a rope may tighten into a snare. The coupling protocol is implemented but dormant — it requires multi-snapshot corpus data. Longitudinal measurement data and a site with time as a morphism dimension would enable the temporal dynamics the current framework cannot model.

6. **Per-constraint metric robustness.** Testing whether the classification of individual constraints is stable under small ε perturbations, going beyond the parameter-level bifurcation sweep to the input-metric level.

7. **Scope modifier refinement.** The scope modifier σ is observer-specific and constraint-independent; a constraint-specific variant (encoding intrinsic jurisdictional reach) would require new per-constraint scope metadata and is expected to have minimal classification impact given that σ accounts for less than 7% of inter-observer χ variance and all scope parameters are classification-inert under ±25% perturbation. Whether the architecturally fixed σ values ([0.8, 1.0, 1.0, 1.2]) mask meaningful per-constraint variation is an open empirical question; an audit comparing constraint-specific scope estimates against the fixed values would determine whether the constancy assumption is justified or merely convenient.

8. **Persistence barcodes across all parameters.** The current persistence infrastructure sweeps 6 of 154 parameters. Extending to all parameters where classification changes occur would give a complete picture of which constraints are structurally robust and which are parameter-sensitive.

9. **Nash distance metric-constrained version.** The current Nash distance uses conservative structural computation (any chain type allowed). A metric-constrained version — allowing only type changes supportable by the constraint's χ value under parameter perturbation — would give a tighter measure of true manipulability.

10. **Per-constraint diagnostic walkthrough.** A structured walkthrough of 3–4 constraints — showing the full diagnostic stack, theorem instantiation, game-theoretic profile, and the contrast between a positional extraction case (nash=1, persistent) and a distributed extraction case (nash=3, fragile, stereotyped orbit) — would demonstrate the engine's analytical output more concretely than corpus-level statistics.

11. **Temporal coupling inference.** The infer_structural_coupling/3 mechanism produces 0 inferred edges on a single-snapshot corpus. Longitudinal measurement data would activate this mechanism and enable the temporal dynamics analysis the current framework cannot provide.

12. **Per-position cognitive displacement profiles.** The current δ infrastructure supports both uniform (global δ) and positional (per-position δ) modes. The uniform sweep establishes invariant robustness; the per-position mode would enable modeling of specific cognitive profiles — e.g., a Deliberative classifier with δ = +0.10 at moderate power but δ = +0.03 at powerless power (where experiential data overwhelms cognitive bias). Mapping StrengthsFinder profiles or similar cognitive orientation instruments to empirical δ values would connect the framework to psychometric measurement, though it is outside the framework's current scope.

13. **Boundary thickness as classification quality metric.** The δ-sweep produces a per-constraint "boundary thickness" — the minimum δ at which classification first changes. Thin-boundary constraints are cognitively fragile (two observers at the same structural position but different cognitive orientations would disagree). Reporting boundary thickness alongside classification type would give users a built-in confidence measure distinct from the MaxEnt shadow: the shadow measures metric uncertainty, while boundary thickness measures observer-calibration sensitivity.

---

## 7. Related Work

**Topos-theoretic approaches in physics** (Isham and Butterfield 1998; Döring and Isham 2008). Both are presheaves on sites of contexts; both formalize context-dependent truth. Key disanalogy: quantum measurement involves irreversibility, stochasticity, and entanglement, none of which are present in DR.

**Contextuality and sheaf cohomology** (Abramsky and Brandenburger 2011). Abramsky and Brandenburger formalize quantum contextuality using sheaf cohomology: a system is contextual iff the obstruction to a global hidden-variable assignment is non-trivial in H¹. DR's refusal of sheafification is structurally parallel — perspectival disagreement is the obstruction, and the contextuality fraction (CF = H¹/6 per constraint) operationalizes this obstruction in DR's discrete setting. The CF gap result — values forbidden at 1/6 and 1/3 by the site geometry — has no direct analogue in the quantum contextuality literature, where the admissible contextuality fractions depend on the specific measurement scenario rather than on a fixed site structure. Key disanalogy: AB contextuality arises from incompatible measurement bases in quantum mechanics; DR contextuality arises from power-modulated perception across structurally distinct observer positions. The mathematics is similar; the source of incompatibility differs.

**Game theory and mediating structures** (von Neumann and Morgenstern 1944). Von Neumann's insight that the game — the mediating structure — cannot be eliminated by solving from either player's perspective corresponds structurally to DR's refusal of sheafification. The Nash equilibrium orbit analysis (§5.5) formalizes this connection: the presheaf is the game, H¹ measures how far the game is from having a dominant-strategy equilibrium, and Nash distance measures the minimum coordination required to achieve global consistency. The connection to Peirce's Thirdness (a relation not decomposable into dyadic components) applies in both cases: neither the game nor the presheaf reduces to the sum of its participants' perspectives.

**Computational social science.** DR is not machine learning. Classification is computed from continuous metrics via a hand-designed deterministic rule cascade, and the central question is not "which label is correct?" but "how does the label depend on who is labeling?"

**Markov categories** (Fritz 2020). The correct abstraction for DR's MaxEnt layer — capturing compositionality without requiring the Giry monad's distribution-over-distributions structure.

**Sheaf Laplacians** (Hansen and Ghrist 2019). Applied to DR's path-graph site, confirms the institutional phase transition as the dominant spectral feature.

**Topological data analysis** (Edelsbrunner and Harer 2010). Persistence barcodes applied to DR's parameter space characterize the structural stability of each theorem's predictions. The novelty is applying persistence to the parameter space of a classification presheaf rather than to a point cloud; the methodology is transferable to any classification system with threshold parameters.

---

## 8. Conclusion

Classification of social structures depends irreducibly on who is observing. This paper shows that this dependence has formal mathematical structure and derives specific consequences from a small set of axioms.

The framework's core commitment is a single empirical hypothesis: power modulates the perception of extraction. Encoded as a presheaf on a site of observer positions, this hypothesis produces consequences not visible from informal intuition alone. Extraction cannot be universally perceived as such. Disagreement clusters in discrete blocs. The institutional observer carries 97% of the spectral weight. Single-position analysis is provably almost blind to cross-position structure. These are theorems, not findings.

The empirical record has deepened substantially since the framework's initial formulation. Structural invariants survive FCR ablation, corpus growth from 887 to 3,254 constraints, and inversion of input distributions — confirming they are fixed-point attractors of the axioms. Wasserstein transport detects sub-threshold distributional fracture invisible to H¹. Variance decomposition provides the first direct metric-level validation of Axiom 2's directionality mechanism. Persistence barcodes distinguish robust perspectival fracture from threshold artifacts. The game-theoretic analysis reveals that 69.3% of non-constant orbits are Nash-stable, that all FCR cover stories are structurally forced, and that the anti-correlation between Nash distance and persistence resistance identifies a phase transition between two structurally distinct extraction regimes. The institutional observer's vulnerability is not a calibration artifact — it persists across the full range of power modifier calibrations as a consequence of two independent structural mechanisms. A cognitive displacement analysis tests whether these claims survive intra-observer variation: a perturbation parameter δ modeling systematic perceptual bias within a structural position. All structural invariants — the H¹ gap, spectral dominance, and the institutional sign-flip — are robust across δ ∈ [−0.15, +0.15]. The analysis reveals that classification sensitivity to cognitive orientation is concentrated at the moderate observer position, where the sigmoid derivative is highest, and that the institutional observer's classification sensitivity and Nash vulnerability concentration are decoupled phenomena occupying different regions of the constraint space.

Three open questions define the frontier. The ε validation problem remains: whether LLM-generated base extractiveness correlates with domain-expert judgment requires a targeted empirical study with held-out constraints and human raters. The non-linear site extension would determine which results are fundamental and which are artifacts of the current 4-element chain — the H¹ gap and spectral structure both depend on linearity. The temporal coupling mechanism is implemented but dormant; activating it requires longitudinal measurement data and would transform the framework from a static classifier into a dynamic model of how constraints evolve through the observer positions they exploit. A fourth question has been partially answered: whether the framework's point-observer model (one classification per position) is adequate, or whether intra-position variance requires a fiber-bundle extension. The δ analysis shows that point-observer adequacy is position-dependent — structural invariants are robust to intra-position variance, but individual constraint classifications at the moderate position are not. Per-position cognitive displacement profiles would extend the model from points to fibers without requiring restructuring of the categorical machinery.

The framework does not explain why extraction requires perspectival cover. That remains a sociological claim requiring domain theory. What the framework provides is formal machinery to establish that perceptual non-universality holds under power-modulated classification, to measure how much of a domain is perspectivally fractured, and to identify the specific geometric structure of that fracture — including which disagreements are Nash-stable, which cover stories are structurally forced, and which constraints sit in regimes where the extraction cannot be classified away by any unilateral reclassification.

**Broader stakes.** The formal results have implications beyond the social-constraint domain. For democratic theory: if institutional perception is spectrally decoupled from other positions (Theorem 3), institutional actors designing reforms are working from a classification structurally orthogonal to the experience of those affected — not because they ignore the data, but because their position transforms it. For regulatory design: if single-position analysis misses more than 97% of cross-position structure (Theorem 4), regulatory impact assessments conducted from a single vantage point are provably almost blind to the effects that matter most. For epistemic justice: if extraction structurally requires perceptual non-universality (Theorem 1), the demand to "prove extraction exists" from the perspective of its beneficiaries is not a neutral epistemic standard but a structural impossibility — the beneficiary's position is precisely where extraction is invisible. For intervention design: the two-regime finding has practical consequences — positional extraction (Nash-stable, parameter-robust) resists reclassification strategies while distributed extraction (stereotyped four-way disagreement, parameter-fragile) may be amenable to targeted metric adjustment. For institutional epistemology: the cognitive displacement analysis shows that moderate-power actors — committee members, middle managers, regulators — are the observers whose cognitive orientation has maximum leverage on classification output; a harmony-oriented classification population systematically increases Nash stability (making extraction harder to dissolve through single-position reclassification), while a risk-sensitive population slightly increases the number of resolvable configurations. These connections are interpretive rather than formal, but they indicate where the framework's mathematical results make contact with questions that matter.

The presheaf should not be sheafified. The framework's value lies precisely in measuring perspectival fracture — in quantifying the gap between local truth and global truth, and in identifying the structural patterns in that gap. The descent rate, the H¹ distribution, the near-absence of snares from H⁰, the institutional observer as dominant dissenter, the oracle gap, the contextuality fraction, the Nash-stable orbits, the stereotyped distributed extraction signature — these are features of the presheaf's failure to be a sheaf. Sheafification would erase them. The truth of a social system, on this account, is not the consensus but the fracture itself.

---

## References

Abramsky, S., & Brandenburger, A. (2011). The sheaf-theoretic structure of non-locality and contextuality. *New Journal of Physics*, 13(11).

Amari, S., & Nagaoka, H. (2000). *Methods of Information Geometry*. American Mathematical Society.

Arrow, K. J. (1951). *Social Choice and Individual Values*. Wiley.

Čencov, N. N. (1982). *Statistical Decision Rules and Optimal Inference*. American Mathematical Society.

Döring, A., & Isham, C. J. (2008). "What is a thing?": Topos theory in the foundations of physics. In *New Structures for Physics*, Springer.

Edelsbrunner, H., & Harer, J. (2010). *Computational Topology: An Introduction*. American Mathematical Society.

Fritz, T. (2020). A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics. *Advances in Mathematics*, 370.

Hansen, J., & Ghrist, R. (2019). Toward a spectral theory of cellular sheaves. *Journal of Applied and Computational Topology*, 3.

Haraway, D. (1988). Situated knowledges. *Feminist Studies*, 14(3).

Harding, S. (1986). *The Science Question in Feminism*. Cornell University Press.

Isham, C. J., & Butterfield, J. (1998). A topos perspective on the Kochen-Specker theorem. *International Journal of Theoretical Physics*, 37(11).

Lawvere, F. W. (1969). Adjointness in foundations. *Dialectica*, 23(3–4).

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic*. Springer.

Noether, E. (1918). Invariante Variationsprobleme. *Nachrichten von der Gesellschaft der Wissenschaften zu Göttingen*.

Ostrom, E. (1990). *Governing the Commons*. Cambridge University Press.

Sen, A. K. (1970). *Collective Choice and Social Welfare*. Holden-Day.

von Neumann, J., & Morgenstern, O. (1944). *Theory of Games and Economic Behavior*. Princeton University Press.

Wheeler, J. A. (1989). Information, physics, quantum. In *Complexity, Entropy, and the Physics of Information*, Addison-Wesley.

Yuen, H. (2023). A quantum complexity-theoretic reduction for the unitary synthesis problem. arXiv:2306.13073.

---

**What changed from v6.5:**

New section (§5.6) and targeted updates across five existing sections. No structural changes to axioms, theorems, or the computational engine.

1. **Abstract:** Added paragraph summarizing cognitive displacement analysis — δ-sweep robustness confirmation, moderate-position sensitivity finding, institutional threshold-density mechanism, Nash/classification-sensitivity decoupling, H¹ = 6 robustness.

2. **§1 Introduction, roadmap:** Added mention of §5.6 cognitive displacement analysis in the section overview.

3. **§5.5, institutional vulnerability paragraph:** Extended with δ-sweep confirmation — institutional vulnerability ≥ 83% across δ ∈ [−0.10, +0.10], moderate never vulnerable, analytical absorbs share under negative δ. Two sweep types (π and δ) now independently confirm structural concentration.

4. **§5.6 Cognitive Displacement Analysis (new section):** Full empirical section covering: sigmoid derivative as wrong sensitivity predictor (threshold density dominates at U₃), moderate observer as cognitively unstable position (348 constraints in δ-band of 0.015), clamp boundary asymmetry, structural invariant robustness table, H¹ = 6 fine-grained fragility probe (89% survival, 100% moderate↔institutional collapse pair, negative δ creates 93 new H¹ = 6 entries), and Nash/classification-sensitivity decoupling (77% of institutional flips outside resolvable set).

5. **§6.2 STRICT list:** Added cognitive displacement invariant robustness, H¹ = 6 survival rate, Nash δ-robustness, and collapsing pair identity.

6. **§6.3 STRUCTURAL list:** Added cognitive displacement as observer calibration (mechanism strict, interpretation structural) and threshold-density institutional sensitivity explanation.

7. **§6.5, "Model intra-level dynamics":** Updated to acknowledge δ infrastructure as partial first-order model of intra-position variance.

8. **§6.6 items 12–13:** Added per-position cognitive displacement profiles and boundary thickness as classification quality metric to the strengthening list.

9. **§8 Conclusion:** Added cognitive displacement robustness confirmation and moderate-position finding. Extended open questions from three to four (adding point-observer adequacy as partially answered by δ analysis). Added institutional epistemology implication to broader stakes.
