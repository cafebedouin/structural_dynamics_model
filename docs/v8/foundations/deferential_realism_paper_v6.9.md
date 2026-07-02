# Axioms and Consequences of Observer-Dependent Classification

**A Formal Framework for Systems Where What You See Depends on Where You Stand**

**Version: v6.9

---

**Abstract.** We present a formal framework for classification systems where the result depends irreducibly on the observer's position. The framework models classification as a presheaf on a site of observer contexts, deliberately refusing the sheaf gluing axiom so that perspectival disagreement becomes a measurable structural feature rather than a defect to be resolved.

We separate the framework into three layers: (1) axioms — the design commitments that define the measurement apparatus, (2) theorems — structural consequences that follow deductively from the axioms without reference to any corpus, and (3) empirical observations — corpus-dependent findings that validate the engine and characterize specific datasets.

The axioms encode a single core hypothesis: power modulates the perception of extraction. The theorems derive non-obvious consequences. Extraction necessarily requires a cover story — it can never be universally recognized as extraction under power-modulated perception. Observer disagreement clusters in discrete blocs rather than distributing smoothly. The institutional observer carries 97% of the spectral weight in classification disputes. Single-position analysis with full information detects less than 3% of the observer-dependent structure that cross-position analysis reveals. These are properties of the axioms, not of any dataset.

Two independently generated corpora with inverted input distributions confirm the engine correctly computes these consequences. Structural invariants — the H¹ gap, spectral eigenvalues, contextuality fraction gap, and institutional dissent direction — are identical across both corpora and survive FCR override ablation, confirming they are fixed-point attractors of the axioms. Corpus-dependent statistics (type distributions, descent rates, coalition structure) vary between corpora as expected.

New diagnostics extend the empirical record on a 3,254-constraint corpus. Wasserstein L¹ transport and H¹ are incommensurable measures of perspectival fracture: H¹ counts threshold crossings in the discrete classification orbit while W₁ measures transport in continuous MaxEnt distributions, and neither bounds the other. A constraint can have maximal discrete obstruction (H¹ = 6) with minimal distributional shift (median W₁ = 0.005), or zero discrete obstruction with W₁ ≈ 1.0. The apparent Spearman correlation (ρ = 0.91) is an artifact of the H¹ = 0 mass constituting 74% of the corpus. Variance decomposition of the Axiom 2 formula confirms that directionality f(d(P)) accounts for 98.6% of inter-observer χ variance — the first direct metric-level validation that power-modulated directionality drives observer-dependent classification divergence. Parametric persistence barcodes confirm that the 14 constraints near the snare_chi_floor threshold have robust perspectival fracture across the full parameter range; the threshold crossing is a classification boundary artifact, not a distributional regime change. A game-theoretic analysis formalizes the presheaf structure as a classification game: 69.3% of non-constant orbits are Nash-stable (no unilateral reclassification resolves the disagreement), all FCR-detected cover stories are structurally forced rather than contingently manufactured, and Nash distance and H¹ persistence are sharply anti-correlated — a phase transition rather than a gradient — identifying two structurally distinct extraction regimes. The institutional observer's vulnerability concentration (94% of resolvable cases) is a structural property confirmed across the full range of power modifier calibrations. An honest assessment distinguishes strict categorical correspondences from structural analogies.

A cognitive displacement analysis introduces an intra-observer perturbation parameter δ, modeling systematic perceptual bias within a structural position. A δ-sweep across [−0.15, +0.15] confirms that all structural invariants — the H¹ gap, institutional spectral dominance, and the sign-flip mechanism — are robust to observer calibration variation. The sweep reveals that sensitivity to cognitive orientation is concentrated at the moderate observer position (U₂), where the sigmoid derivative is 4.5× higher than at the power extremes, inverting the paper's prior characterization of U₂ as the "classification anchor." The institutional observer (U₃) is the most classification-sensitive position by flip count despite having low sigmoid slope, because threshold density in the negative-χ regime amplifies small perturbations — a mechanism invisible to derivative analysis alone. The institutional Nash vulnerability concentration (94%) declines to a floor of 83% under δ perturbation, with the decline driven by a shift of vulnerability share to the analytical position rather than a redistribution across all observers. H¹ = 6 (maximum perspectival fracture) is structurally robust: 89% of the population survives the full δ range, with minor fragility concentrated in constraints where the moderate observer sits near the tangled_rope/rope boundary.

A δ-band population analysis identifies which constraints are sensitive to intra-position cognitive orientation — where two observers at the same structural position but different cognitive dispositions would disagree on classification. Of 822 H¹ ≥ 3 constraints, 498 (60.6%) have at least one observer position where χ falls within 0.10 of a classification boundary; the remaining 324 (39.4%) are in a deep fracture regime where perspectival disagreement is entirely position-determined and no amount of intra-position cognitive variation alters classification. The deep fracture/sensitive split correlates sharply with H¹ band: H¹ = 3 constraints are 70% deep fracture (position-locked disagreement), while H¹ = 6 constraints are 97% sensitive (cognitively modulated disagreement). The powerless observer (U₁) dominates δ-band activity — 93% of sensitive constraints have their δ-band at U₁ — because constraint-specific effective directionality values cluster near the sigmoid midpoint at that position, a finding invisible to the canonical-d analysis. Under extended δ (±0.25, modeling atypical observers), 90% of sensitive positions span the full rope-to-snare classification range, restating Theorem 1 at the intra-position level: the cover story mechanism operates not only across structural positions but within a single position across cognitive orientations. Kahan's Cultural Cognition framework provides a candidate psychometric bridge: its hierarchy-egalitarianism axis maps structurally to δ (orientation toward power as coordination versus extraction) and its individualism-communitarianism axis maps to contamination network sensitivity (whether coupling to neighboring constraints is perceptually visible).

An Arakelov height diagnostic reveals a second institutional boundary phenomenon invisible to all existing diagnostics. The height function combines base extractiveness with pre-correction MaxEnt uncertainty and conditional signature pressure, computed as the max over all four observer contexts. Of 381 high-complexity constraints, 238 have H¹ = 0, W₁ = 0, Nash distance = 0 — clean presheaves with no observer disagreement — yet the generative model's pre-correction probability mass was genuinely split before the structural signature forced a classification. These 238 constraints are disjoint from the 162 institutional_dissent constraints by construction (Nash distance > 0 requires observer disagreement, which excludes a constraint from the Arakelov unique set), revealing two structurally independent mechanisms by which the institutional phase transition (π = −0.2) generates boundary uncertainty: a *dissent route* where the sign-flip produces actual type disagreement, and an *uncertainty route* where the sign-flip operates near but not across the classification threshold, producing fragile consensus invisible to perspectival diagnostics. Together, the Nash diagnostic and the Arakelov height partition the space of boundary-proximate constraints into two exhaustive regimes — the disagreement manifold and the consensus manifold. A Fisher information analysis of the consensus manifold reveals a further partition: 143 fragile-consensus constraints sit on a steep MaxEnt ridge (high ε-curvature, 83% snare), while 89 robust-consensus constraints are signature-anchored in a flat curvature region (70% snare + 28% tangled_rope). Fisher curvature is uncorrelated with confidence margin (r = 0.064), confirming the diagnostic is non-redundant. The observer curvature hierarchy — analytical >> institutional by 20× — confirms that the uncertainty route operates through signature-dependence rather than ε-sensitivity: the institutional observer's near-zero f(d) collapses ε variation, so the structural correction is load-bearing in a region where the continuous classifier is confidently wrong rather than uncertainly split.

---

## 1. Introduction

The classification of social structures — laws, norms, institutions, regulatory mechanisms — depends on who is classifying. A labor regulation that appears as an immutable feature of the economic landscape to a worker trapped within it may appear as a reformable coordination mechanism to a legislator, and as an extractive rent-seeking device to an analyst examining its distributional effects. This is not a failure of classification but a structural feature of the domain.

The standard response to perspectival dependence is to resolve it — to identify the "correct" classification by privileging one observer position or aggregating across positions. This paper takes the opposite approach. We model perspectival dependence using presheaf theory, where disagreement has formal mathematical structure, and the standard tools of topos theory — cohomology, descent, naturality — produce quantitative invariants that characterize any domain where classification depends on perspective.

The framework, *Deferential Realism* (DR), is *realist* in that it treats constraints as having objective structural properties (extractiveness, suppression, coordination function) that exist independently of any observer; it is *deferential* in that it treats the *classification* of those properties as irreducibly dependent on the observer's structural position. The presheaf is emphatically not a sheaf: the gluing axiom is intentionally violated because perspectival disagreement is a diagnostic signal, not a defect. DR is a meter for perspectival fracture, not a machine for identifying the correct political line. The invariants it produces measure how disagreement is structured; they do not adjudicate whose classification is right.

The paper separates cleanly what earlier versions interleaved: §2 states the axioms as design commitments, §3 derives the theorems that follow from those axioms alone, §4 presents the computational engine, §5 reports empirical findings including a cognitive displacement analysis that tests structural invariants against intra-observer perturbation, a δ-band population analysis that identifies where intra-position cognitive orientation modulates classification, and an Arakelov height diagnostic that reveals a second institutional boundary regime invisible to all perspectival diagnostics, §6 provides the honest assessment, §7 discusses related work, and §8 connects the formal results to broader implications.

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

**What would change.** The site choice is normative — it is where political commitments enter the mathematics. A Marxist analysis might separate epistemic access from material power as independent morphism dimensions. A feminist standpoint epistemology might add an embodiment axis. A non-linear site (a DAG, a branching lattice, a site with overlapping jurisdictions where a worker is powerless economically but powerful legally) would change certain invariants — specifically those that depend on morphism structure (spectral decomposition, restriction map composition). The gap structure derived in Theorem 2 depends on the number of observer positions (|Ob(C)| = 4), not on linearity, and holds on any 4-object category regardless of morphism structure. A DAG with constant-coefficient cohomology would not produce different H¹ invariants — a finite poset with a minimum element has contractible nerve regardless of morphism count, so adding direct shortcuts between observer positions generates no new topological obstructions. Topology-sensitive invariants would require either non-poset site structure (multiple incomparable intermediate observers creating genuine loops in the nerve) or path-dependent restriction maps, where direct and composed perceptual transitions are allowed to disagree. The latter — measuring ρ₁₃ − ρ₁₂ ∘ ρ₂₃ as a discrete holonomy of perceptual composability — is a specific candidate for future development that the current framework cannot express. The framework is a functor from site choices to invariants; the current 4-element chain is one instantiation. The invariants are geometry-relative — properties of this site — not world-relative assertions about the constraints themselves.

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

With four observer positions (|Ob(C)| = 4), H¹ values 1 and 2 are forbidden. Perspectival disagreement does not distribute smoothly — it clusters in discrete blocs determined by the number of observers, not by the morphism structure of the site.

**Proof sketch.** H¹ counts disagreeing observer pairs. With 4 observers each assigned a type, the number of disagreeing pairs = C(4,2) − Σ C(nᵢ,2), where nᵢ are the sizes of the agreement clusters (groups of observers that assign the same type). The achievable partitions of 4 observers into agreement clusters are (4), (3,1), (2,2), (2,1,1), (1,1,1,1), yielding disagreeing pair counts {0, 3, 4, 5, 6} respectively. Values 1 and 2 are unreachable by any partition of 4 elements, regardless of site morphisms.

**Corollary (n=5 extension).** For a 5-observer system, the achievable H¹ values are {0, 4, 6, 7, 8, 9, 10} and the forbidden set expands to {1, 2, 3, 5}, including a new interior gap at 5. This follows from the same partition argument applied to partitions of 5. The interior gap at 5 is qualitatively new: it cannot appear in any 4-observer system.

**Corpus confirmation:** The H¹ distribution takes values {0, 3, 4, 5, 6} exclusively. Values 1 and 2 are empirically absent. The contextuality fraction (CF = H¹/6) takes values only at {0, 0.5, 0.667, 0.833, 1.0}, confirming that values 1/6 and 1/3 are structurally forbidden.

**Exhaustive reachability verification.** An axiom reachability analysis enumerates all orbit patterns (4-element type tuples) producible by the classify_from_metrics cascade under any valid input combination — sweeping ε, suppression, theater_ratio, directionality (including per-observer overrides), all boolean feature combinations, and all signature overrides. Of 4,096 theoretical orbit patterns, 1,404 are reachable and 2,692 are axiom-impossible. The reachable H¹ values are exactly {0, 3, 4, 5, 6}; no reachable orbit has H¹ = 1 or H¹ = 2. This confirms the gap as a structural consequence of the cascade architecture — not a property of the corpus or the generation process, but of Axioms 1–6 as implemented. The proof method is machine-verified exhaustive search over the engine's input space, not deductive proof from axioms; the distinction matters because the exhaustive search holds for this specific cascade with these specific threshold values, while a deductive proof would hold for any cascade with the same structural properties.

**Independent corroboration:** Boundary non-normality testing provides independent empirical evidence that types are discrete structural categories rather than arbitrary cuts through a continuous distribution. All type boundaries reject normality (Shapiro-Wilk p = 0.0 for all testable boundaries). The tangled_rope→snare boundary follows beta(0.16, 1.33) with skewness 4.34: a spike near zero with a thin tail. If the type space were a continuous manifold, boundary distributions would be approximately normal by the central limit theorem. The beta distribution fit is the signature of discrete structural categories separated by a genuine gap — consistent with Theorem 2's prediction.

**Game-theoretic interpretation:** The forbidden H¹ values {1, 2} correspond to Nash distances that cannot exist on a 4-observer extraction-chain site. A constraint with H¹ = 1 would require a single disagreeing pair, but the partition arithmetic forces disagreement into blocs of at least 3. This gives Theorem 2 an independent game-theoretic derivation from the Nash distance formalism.

**Methodological note.** The current H¹ is a partition functional, not a sheaf-theoretic cohomological invariant. It counts disagreeing observer pairs and its spectrum is determined entirely by the number of observers, not by the site's morphism structure — the same gap holds on any 4-object category regardless of whether the site is a linear chain, a DAG, or a complete graph. Genuine sheaf cohomology H¹ (descent obstruction) would depend on morphisms and restriction maps, and would only activate on failure of composability: when the direct restriction ρ₁₃ ≠ ρ₁₂ ∘ ρ₂₃. Standard Čech cohomology on a finite poset with a minimum element is trivial regardless of morphism structure, so topology-sensitive invariants would require either non-poset site structure or path-dependent restriction maps as an explicit new modeling commitment. The current proxy captures genuine empirical structure; the distinction matters for interpreting what that structure is evidence of.

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

**Wasserstein L¹ transport and H¹–W₁ complementarity.** The Wasserstein distance W₁ between adjacent observers' MaxEnt distributions provides a continuous complement to discrete H¹ counting. For discrete distributions over the extraction chain, W₁ reduces to L¹ distance between CDFs — computable without linear programming. Wasserstein and H¹ are incommensurable measures of perspectival fracture. H¹ counts threshold crossings in the discrete classification orbit; W₁ measures transport in the continuous MaxEnt distributions. A constraint can have H¹ = 6 with W₁ < 0.01 (the 154 split-field cases, where maximal discrete obstruction coexists with minimal distributional shift) or H¹ = 0 with W₁ ≈ 1.0 (21 uniform-tangled cases, where distribution shape shifts substantially along the power axis while the type label stays constant).

Two structural mechanisms produce the 101 falsifying cases (H¹ > 0, W₁ = 0). First, non-chain type invisibility (39 cases): when the orbit contains naturalized, scaffold, or piton at one context, H¹ registers the disagreement but W₁ — which projects onto the 4-type extraction chain and renormalizes — sees nothing. This is an architectural property of how W₁ is computed. Second, threshold-MaxEnt decoupling (62 cases): all orbit types are chain types but per-context MaxEnt distributions are nearly identical (total variation < 0.01). The threshold classifier `classify_from_metrics` flips the discrete label when χ crosses a boundary; the MaxEnt distribution, being smooth, barely changes. Classification sensitivity exceeds distributional sensitivity near type boundaries.

The H¹ = 6 anomaly sharpens the finding: the 154 split-field constraints have the *lowest* mean W₁ (0.047) of any H¹ > 0 band, despite maximal discrete obstruction. All four observers assign distinct types, but the underlying probability masses shift minimally — the type disagreement is so fundamental that each observer commits to a different extraction category without requiring large probability transport to do so. W₁ means are non-monotone across H¹ bands (f(3) = 0.097, f(4) = 0.252, f(5) = 0.200, f(6) = 0.047), ruling out any comparison theorem.

The apparent Spearman correlation (ρ = 0.91) is an artifact of the H¹ = 0 mass constituting 74% of the corpus; within H¹ > 0 the correlation falls to 0.40, and within H¹ ≥ 4 it inverts (ρ = −0.19). The measures are complementary diagnostics, not redundant ones. The boundary distribution confirms that transport costs concentrate at the U₂→U₃ and U₃→U₄ edges (80% of transitions), consistent with the spectral dominance of the institutional phase transition.

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

**Institutional dissent binary split.** Within the 109 institutional_dissent constraints, base_extractiveness discriminates the groups with r = 0.9976 (p < 0.001). The institutional observer's informational advantage operates in the range ε < 0.62; above that threshold, extraction dominates all perspectives. The low-snare group spans 81 unique domains; the high-snare group concentrates in 8 geopolitical/economics/labor domains, suggesting the extractiveness threshold interacts with domain structure. **Reachability note:** the institutional dissent direction (U₃ always sees lower extraction than other observers) is calibration-dependent, not cascade-structural. The axiom reachability analysis finds 222 reachable orbits where U₃ dissents *upward* (sees higher extraction than all other observers), but all require directionality_override values pushing d_institutional above ~0.5, far from the standard heuristic d ≈ 0.10–0.15. Under the standard power_role_heuristic, institutional upward dissent is unreachable because f(d) < 0 at canonical d = 0.00 forces χ below all extraction thresholds. The finding holds for the standard calibration; it is not a property of the threshold geometry alone. Contrast with the H¹ gap (Theorem 2), which holds for any directionality assignment.

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

### 5.7 Intra-Position Cognitive Orientation

The δ-sweep in §5.6 establishes that structural invariants survive intra-position perturbation. A different question: for which specific constraints does intra-position cognitive orientation determine classification — and at which observer positions does the orientation have leverage? The δ-band population analysis answers this by identifying constraints where χ falls within 0.10 of a classification boundary at any observer position.

**δ-band population and the deep fracture regime.** Of 822 constraints with H¹ ≥ 3 (perspectival fracture present), 498 (60.6%) have at least one observer position where χ is within 0.10 of a classification boundary — these are the constraints where cognitive orientation can shift classification. The remaining 324 (39.4%) have all χ values far from any boundary at every observer position. For these deep fracture constraints, the perspectival disagreement is entirely position-determined: no amount of intra-position cognitive variation alters the classification at any observer position. The disagreement exists because different structural positions compute different χ values, and those values land in different classification zones with comfortable margins.

The deep fracture / sensitive split correlates sharply with H¹ band:

| H¹ | Total | Sensitive | % | Deep Fracture |
|---:|------:|----------:|--:|--------------:|
| 3 | 256 | 77 | 30% | 179 |
| 4 | 50 | 41 | 82% | 9 |
| 5 | 363 | 232 | 64% | 131 |
| 6 | 153 | 148 | 97% | 5 |

The H¹ = 3 population is 70% deep fracture — the institutional observer disagrees with everyone else, and the disagreement is structurally locked. The H¹ = 6 population is 97% sensitive — the maximum-fracture constraints are almost all in the regime where cognitive orientation has classification leverage. This connects to the two-regime interpretation from §5.5: positional extraction (nash = 1, H¹ = 3) is cognitively invariant, while distributed extraction (nash = 3, H¹ = 6) is cognitively modulated. The practical implication is that disagreement about positional extraction constraints cannot be resolved by changing how people think — it requires changing where they stand — while disagreement about distributed extraction constraints is partially amenable to cognitive reorientation.

**The powerless observer dominates δ-band activity.** Among the 498 sensitive constraints, the δ-band active positions are:

| Observer | δ-Band Active | % of Sensitive |
|----------|-------------:|---------------:|
| Powerless (U₁) | 463 | 93.0% |
| Moderate (U₂) | 93 | 18.7% |
| Institutional (U₃) | 53 | 10.6% |
| Analytical (U₄) | 36 | 7.2% |

This finding qualifies the analysis in §5.6, which used canonical directionality values and identified U₂ as the cognitively unstable position. The δ-band analysis uses the engine's actual per-constraint effective directionality values, back-computed from the stored χ output. Approximately half of the powerless-position constraints have an effective d near 0.50 (the sigmoid midpoint, where the derivative is maximal) rather than the canonical d = 0.90 (where the sigmoid is nearly flat). The effective d values cluster near the midpoint because the engine assigns constraint-specific directionality based on the observer's structural relationship to each constraint, and the powerless observer's victim relationship to many constraints maps to directionality values in the sigmoid's steepest region. The canonical-d analysis in §5.6 sees the powerless position as insensitive (flat sigmoid at d = 0.90); the effective-d analysis reveals it as the most sensitive position in the corpus (steep sigmoid at d ≈ 0.50 for many constraints).

The substantive finding: among the people most affected by a constraint — those at the powerless position — cognitive orientation produces the widest classification disagreement. Two workers looking at the same workplace arrangement, one hierarchically oriented and one egalitarian, would disagree about whether it constitutes coordination or extraction. The institutional observer's disagreement with everyone else is positional and cognitively invariant; the powerless observer's internal disagreement is cognitively determined.

**Extended δ and the classification range.** Under extended δ (±0.15 to ±0.25, modeling atypical observers such as those with institutional capture, narcissistic investment in coordination narratives, or trauma-informed extraction sensitivity), 90% of δ-band active positions span the full rope-to-snare classification range. This restates Theorem 1 at the intra-position level: the cover story mechanism — extraction cannot be universally recognized as extraction — operates not only across structural positions (the institutional sign-flip) but within a single structural position across cognitive orientations. The full classification range is in play for observers with atypical orientation, meaning that observers with strong hierarchical orientation (δ ≈ −0.20) and observers with strong egalitarian orientation (δ ≈ +0.20) at the same structural position would produce opposite classifications of the same constraint.

The range analysis reveals that 61% of classification boundary crossings occur within the standard δ range (±0.08) while 39% require extended δ (±0.15 to ±0.25). The 39% represents classification discretion invisible to normal-range cognitive orientation variation — constraints that appear to have settled classifications under ordinary disagreement but are fully contested when observers have atypical orientation profiles. Reporting the boundary-crossing δ value per constraint provides a classification quality metric distinct from the MaxEnt shadow: the shadow measures metric uncertainty (how peaked the probability distribution is), while the boundary-crossing δ measures observer-calibration sensitivity (how much cognitive orientation an observer would need before their classification changes).

**Psychometric bridge.** Kahan's Cultural Cognition framework (Kahan, Braman, et al.) provides the most structurally productive candidate instrument for empirical δ calibration. The framework measures two axes of cultural worldview: hierarchy-egalitarianism and individualism-communitarianism. The hierarchy-egalitarianism axis maps to δ: hierarchical orientation (existing power structures are legitimate coordination) corresponds to negative δ, while egalitarian orientation (existing power structures are potential extraction) corresponds to positive δ. The individualism-communitarianism axis maps to contamination network sensitivity: communitarian observers weight collective effects and are predicted to be more sensitive to coupling between neighboring constraints (effective purity dominates classification), while individualist observers classify constraints in isolation (intrinsic purity dominates). The individualism-communitarianism axis is active for 295 of 498 sensitive constraints (59%) — those with a purity gap (difference between intrinsic and effective purity) exceeding 0.10.

This mapping is STRUCTURAL: Cultural Cognition was designed to explain why people classify the same policy risks differently based on cultural worldview, which is structurally parallel to DR's question of why observers classify the same constraint differently. The hierarchy-egalitarianism axis has construct validity for the relevant dimension (orientation toward power as coordination versus extraction) in a way that personality instruments (CliftonStrengths, Big Five, VIA) do not — those measure habitual cognitive process, not perceptual orientation toward social structure. Social Dominance Orientation (Pratto, Sidanius) provides the cleanest single-parameter mapping (high SDO ≈ negative δ, low SDO ≈ positive δ); Moral Foundations Theory (Haidt, Graham) provides the richest moral-reasoning context but requires dimensional reduction from six foundations to a single δ estimate. The empirical study that would ground δ in psychometric measurement is specified but not yet conducted: subjects with known Cultural Cognition scores would classify constraints near the δ-band boundaries at multiple simulated structural positions, testing whether the hierarchy-egalitarianism axis predicts classification behavior as the mapping claims.

### 5.8 Arakelov Height and the Two Boundary Regimes

The diagnostics in §§5.4–5.7 operate on constraints where observer disagreement is visible — where H¹ > 0, W₁ > 0, or δ-perturbation produces classification flips. A different question: are there constraints where the engine's classification appears confident from every existing diagnostic but the generative model's pre-correction probability mass was fragile? The Arakelov height diagnostic answers this by combining base extractiveness with MaxEnt classifier uncertainty and structural signature pressure into a single boundary-complexity scalar.

**Construction.** For each constraint C and observer context U_i, define:

- *Raw confidence margin* = P(claimed_type) − P(rival_type) from the pre-override MaxEnt distribution (before structural signatures force a decision).
- *Signature pressure* = |raw_margin − post_margin| for conditional overrides (×3 boost signatures: false_ci_rope, constructed_*); 0 for unconditional overrides (natural_law, false_natural_law), which set target probability to 0.95 regardless and whose pressure is binary rather than diagnostic.
- *Arakelov height* = max over all 4 canonical contexts of ε × ((1 − |raw_margin|) + signature_pressure).

The height is high when extraction is significant AND the MaxEnt classifier was genuinely uncertain AND (for conditional overrides) the structural correction had to do real work. The max-over-contexts ensures that uncertainty at any observer position is captured, not just at the analytical observer. The context that produces the maximum is recorded as a free diagnostic.

**Distribution.** Across 3,274 constraints: mountains produce height ≈ 0 (max 0.006), snares produce the highest mean height (0.271), tangled_rope intermediate (0.047). The height threshold (p75 of non-trivial heights) is 0.254, flagging 381 constraints as high-complexity.

**The key finding: 238 high-complexity constraints are invisible to all existing perspectival diagnostics.** These constraints have W₁ = 0, H¹ = 0 — all four observers agree on the classification, the presheaf structure is clean, Nash distance is zero, and no existing diagnostic flags them. But the MaxEnt generative model's pre-correction probability mass was fragile: before the structural signature forced a decision, the model was genuinely uncertain about the type assignment. Of these 238, 188 are classified as snare and 48 as tangled_rope — all sitting on the extraction boundary.

**The institutional phase transition has two distinct boundary signatures.** The π = −0.2 transition — where f(d) goes negative at U₃ — generates institutional boundary uncertainty through two structurally independent mechanisms:

The *dissent route* (§5.4): The institutional observer actively disagrees with the consensus classification. H¹ > 0, Nash distance > 0, W₁ > 0 for 100 of 162 (the remaining 62 have W₁ = 0 — a consequence of the H¹–W₁ complementarity established in §5.1, where threshold-based type disagreement can occur without distributional shift). Visible to H¹ and Nash diagnostics in all cases. The 162 institutional_dissent constraints concentrate in geopolitical/economics/labor domains. The sign-flip is strong enough to produce a different type assignment. Vulnerable positions are 100% institutional.

The *uncertainty route* (new): All four observers agree on classification, H¹ = 0, W₁ = 0, Nash distance = 0, presheaf structure is clean — but the MaxEnt model's probability mass at U₃ is genuinely split before structural correction forces a decision. The sign-flip is operating *near* the threshold without crossing it. Previously invisible to all existing diagnostics. Broader domain distribution. The 42 institutional-max constraints in the Arakelov unique set are 100% uniform_tangled coalition — zero overlap with institutional_dissent.

**The two populations are disjoint by construction.** Nash distance > 0 requires observer disagreement, which requires H¹ > 0 or W₁ > 0, which by definition excludes a constraint from the Arakelov unique set (defined as high height AND H¹ = 0 AND W₁ = 0). This is not an empirical observation about this corpus; the definitions make overlap impossible. The result holds for any corpus run through the DR engine.

**Context distribution.** Among the 238 unique constraints, the max-height context is: powerless 66.8%, institutional 17.6%, analytical 8.0%, moderate 7.6%. The powerless position dominates but does not monopolize — a contrast with the δ-band finding (§5.7) where 93% of sensitive constraints have boundary activity at U₁. The institutional position's 17.6% contribution reflects the π = −0.2 phase transition operating near but not across classification thresholds. The domain intersection between the dissent-route and uncertainty-route institutional populations — {economic, AI, geopolitical, labor_economics, political_economy, healthcare, technology} — names the domain space where the institutional phase transition is active in both modes.

**Nash complementarity.** The Arakelov unique set has Nash distance = 0.000 for every constraint — zero max_deviation, no vulnerable positions. The institutional dissent set has mean Nash distance = 1.108, with vulnerable positions 100% institutional. The two diagnostics are complementary by construction: Nash distance measures *disagreement effort* (how much reclassification is needed to reach consensus); Arakelov height measures *decision fragility* (how close the pre-correction probability mass was to flipping, even when consensus was achieved). Together they partition the space of boundary-proximate constraints into two mechanistically distinct regimes — the disagreement manifold (non-trivial cohomology, measurable reclassification effort) and the consensus manifold (clean presheaf, fragile pre-correction probability).

**ε-sensitivity and the three-regime partition.** The consensus manifold admits further structure. A Fisher information analysis — computing I(ε) = Σ (1/P)(∂P/∂ε)² on the raw (pre-override) MaxEnt distribution via central finite differences at h = 0.01 — measures how rapidly the generative model's type probabilities change under small perturbations to base extractiveness. The raw MaxEnt path (Gaussian log-likelihood → log-sum-exp softmax) is C^∞ in ε, so Fisher information is well-defined everywhere on the pre-override distribution. (Unconditional signature overrides produce constant post-override distributions with I(ε) = 0; Fisher is computed on the pre-correction distribution that the override acts on.)

Across the 243 Arakelov unique constraints, Fisher curvature is essentially uncorrelated with confidence margin (r = 0.064) — the two diagnostics measure different aspects of boundary proximity. Confidence margin measures how close the current distribution is to flipping; Fisher curvature measures how steeply the distribution changes as ε varies. Two constraints with identical confidence margins can have Fisher curvature differing by an order of magnitude.

The Fisher distribution is bimodal, splitting the consensus manifold into two sub-populations. The *fragile consensus* regime (143 constraints, Fisher above median) sits on a steep MaxEnt ridge: small ε revision would substantially change the analytical-position distribution. Composition: 83% snare. The *robust consensus* regime (89 constraints, Fisher below median) is stable under ε perturbation: the signature override anchors the classification in a region where the continuous classifier is not near a transition. Composition: 70% snare + 28% tangled_rope. The type composition difference is the strongest evidence this partition is not an artifact — fragile consensus constraints are held in place by ε-dependent Gaussian likelihoods, while robust consensus constraints are held by signature-mediated coordination detection that does not scale with ε.

**Observer curvature hierarchy.** Extending the Fisher computation to all four observer positions — using χ = ε × f(d) × σ(S) as the extractiveness feature, with f(d) and σ(S) read from per-constraint pipeline output — reveals a strict hierarchy: analytical (mean I = 158.6) > moderate (69.5) > powerless (64.0) >> institutional (7.8). This hierarchy tracks |f(d)|, confirming it is structural rather than accidental. The institutional position is 20× less sensitive than analytical because f(d_institutional) ≈ −0.115 compresses all ε variation into a narrow band of near-zero χ values, flattening the Gaussian landscape. Only 20 of 243 constraints have higher Fisher curvature at U₃ than at U₄.

This clarifies the mechanism by which the two boundary regimes operate. The *dissent route* works because the sign-flip pushes U₃'s deterministic classification across a threshold — a discrete jump that does not require the continuous MaxEnt landscape to be steep. The *uncertainty route* works because the structural signature overrides a flat-but-confident MaxEnt prediction — the signature is doing heavy lifting precisely because the MaxEnt landscape gives no curvature signal that anything is contested. The Arakelov height diagnostic catches these constraints not because they are on a MaxEnt ridge, but because the pre-correction margin is small despite the post-correction landscape being flat. Fisher curvature being low at U₃ for these constraints is the finding that makes the Arakelov diagnostic non-redundant with curvature measures: it detects a kind of fragility — signature-dependence in a region where the continuous classifier is confidently wrong — that ε-sensitivity analysis cannot see.

The three regimes are now: the **disagreement manifold** (162 constraints, Nash > 0, observer type disagreement), the **fragile consensus manifold** (143 constraints, Nash = 0, high Fisher, ε-sensitive agreement), and the **robust consensus manifold** (89 constraints, Nash = 0, low Fisher, signature-anchored agreement).

**Corpus construction caveat.** Three ε values (0.58, 0.62, 0.68) account for 94% of the Arakelov unique set — a consequence of the LLM-generated corpus producing heavily quantized extractiveness metrics. Fisher curvature varies within each ε group (driven by suppression, theater, and boolean features), but between-group variation is where most of the Fisher spread lives. This limits the resolution of the continuous diagnostic and connects to §6.6 item 1 (metric-level sensitivity): the framework can compute ε-sensitivity in principle, but ε-diversity in the current corpus constrains the information content in practice. A corpus with greater ε diversity would sharpen the fragile/robust partition.

**Epistemic risk.** The uncertainty-route population (238 constraints) represents constraints where the structural signature is doing the most consequential epistemic work. Without the false_natural_law override, these constraints would be misclassified — but there is no perspectival disagreement signal to alert that anything is wrong. The engine looks confident from every diagnostic except this one. This is a distinct kind of institutional risk that the dissent-route framing does not capture: not a constraint where observers disagree, but a constraint where observers agree only because the structural correction preempted disagreement.

---

## 6. Honest Assessment

This section audits the framework: what is strict, what is analogy, what is missing, and what would strengthen it. Everything labeled STRICT is verified mechanically by the codebase. STRUCTURAL is interpretive but grounded. LOOSE should be read as metaphor only.

### 6.1 Three-Level Rigor Classification

- **STRICT**: the categorical correspondence holds mathematically. The code implements the categorical structure, and the correspondence survives formal verification.
- **STRUCTURAL**: the analogy is productive and behavior matches, but full categorical verification of the formal axioms is absent.
- **LOOSE**: the categorical language would mislead if taken literally.

### 6.2 What Is STRICT

The site, the presheaf, the naturality condition, the Boltzmann = functor axiom equivalence, the naturality witnesses (FNL, CI Rope), H⁰ and descent, gauge orbits, the Galois connection, Hub 1 as restriction map, Hub 2 as classification gate, hub independence, binary gate computations, NMI analysis, T13 divergence measurement, mountain zero-divergence, the sheaf Laplacian construction, cross-model spectral invariance, confidence band distribution, post-override attractor convergence, contextuality fraction computation (CF = 1 − descent_rate; graded CF = H¹/6; CF gap at 1/6 and 1/3), orbit monotonicity classification, incomparable orbit decomposition, bifurcation sweep critical values, FCR ablation invariance of H¹ gap, boundary non-normality at type boundaries, boolean feature redundancy with type space, W₁ transport computation and H¹/W₁ divergence, χ variance decomposition (f(d) dominance at 98.6%), persistence barcode robustness (14 snare_chi_floor constraints robust across full sweep range), Nash distance computation (structural), institutional vulnerability plateau across f(d) ∈ [−0.30, +0.60], persistence-Nash anti-correlation (ρ = −0.657), Nash-forced cover story confirmation (0 contingent cases), cognitive displacement invariant robustness (H¹ gap, sign-flip, spectral dominance all preserved across δ ∈ [−0.15, +0.15]), H¹ = 6 survival rate (89% across full δ range), Nash vulnerability δ-robustness (institutional concentration ≥ 83% across δ ∈ [−0.10, +0.10]), moderate-institutional pair as sole collapsing pair in H¹ = 6 fragility (100% of dropouts), δ-band trigger computation (498/822 at thresholds 0.35/0.46 with band width 0.10), powerless-position δ-band dominance (93% of 498 sensitive constraints), H¹-band trigger distribution (H¹ = 3 at 30% vs. H¹ = 6 at 97%), effective d back-computation from engine χ output, extended-range classification span (90% of δ-band positions span rope-to-snare under δ ± 0.25), boundary-crossing δ precision via binary search (zero monotonicity violations across all computed ranges), contamination sensitivity gate (purity gap > 0.10 threshold, active in 295/498 sensitive constraints), Arakelov height computation (max over 4 contexts of ε × (raw_uncertainty + conditional_pressure)), raw confidence margin from pre-override MaxEnt distribution, conditional-only signature pressure (unconditional overrides zeroed), memoized corpus-wide p75 threshold, disjointness of Arakelov unique set and Nash distance > 0 population (by construction: H¹ = 0 ∧ W₁ = 0 entails Nash distance = 0), zero identity overlap between Arakelov institutional-max and institutional_dissent populations (238 vs. 162, intersection = 0), Nash distance = 0.000 for all 238 Arakelov unique constraints. Fisher information I(ε) via central finite differences on the raw (pre-override) MaxEnt distribution (C^∞ path: Gaussian LL → log-sum-exp softmax), Fisher-vs-confidence_margin correlation r = 0.064 (non-redundant), observer curvature hierarchy tracking |f(d)| (analytical 158.6 > moderate 69.5 > powerless 64.0 >> institutional 7.8), three-regime partition counts (disagreement 162, fragile consensus 143, robust consensus 89), type composition divergence between fragile (83% snare) and robust (70% snare + 28% tangled_rope) regimes, H¹ gap exhaustive reachability verification (machine-verified: no orbit in the 1,404 reachable patterns has H¹ ∈ {1, 2}; grid sweep over ε × suppression × theater × 256 boolean combinations × 26 directionality values per observer × 10 signature overrides).

The three-way equivalence (Lawvere ↔ Grothendieck ↔ Noether) is two-out-of-three STRICT: naturality ↔ descent is strict; the Noether column maps to discrete group invariance, which is the precondition of Noether's theorem rather than the theorem itself.

### 6.3 What Is STRUCTURAL

MaxEnt as Markov category (pending delete-map naturality verification). Information geometry of T13 (Fisher-Rao geodesic ball applies to KL divergence, not L∞). FPN as terminal coalgebra (convergence proved via Knaster-Tarski, but full coalgebra axioms unverified). Abductive engine as naturality auditor (triggers are hand-crafted, not derived from categorical constructions). H¹ proxy (combinatorial descent-failure count on the Alexandrov site, not formal Čech H¹). H¹ band-hub correspondence (empirical, not derived). Oracle gap mechanism (architectural, corpus-specific magnitudes). Diagnostic verdict synthesis (threshold-based, not derived). AB-framework connection (the equivalence of "no global section" and "contextual" is strict; the correspondence between DR's graded CF and Abramsky-Brandenburger's contextuality fraction is structural — DR's site is more constrained than AB's general sheaf-cohomological setup, and the CF gap result has no direct analogue in the quantum contextuality literature). Two-regime extraction interpretation (positional vs. distributed) — the anti-correlation and phase transition are STRICT; the regime labels and their theoretical interpretation are STRUCTURAL. Cognitive displacement as observer calibration (the δ perturbation mechanism is strict; the interpretation that δ models cognitive orientation rather than some other source of intra-position variance is structural — the mathematical infrastructure is agnostic about the source of the perturbation). Threshold-density explanation for institutional sensitivity (the flip counts are strict; the causal attribution to density near classification boundaries is structural, inferred from the distribution of flip magnitudes rather than derived from axioms). Deep fracture / sensitive regime distinction (the trigger computation and population counts are strict; the interpretation that deep fracture constraints require positional rather than cognitive intervention is structural). Cultural Cognition as candidate δ instrument (the mapping of hierarchy-egalitarianism to δ sign and individualism-communitarianism to contamination network sensitivity is structural — the dimensional correspondence is productive and generates testable predictions, but no empirical calibration has been conducted; the δ magnitudes ±0.08 for standard CC range and ±0.15 to ±0.25 for atypical observers are structural estimates, not psychometric measurements). Intra-position Theorem 1 restatement (the 90% full-span result is strict; the interpretation that the cover story mechanism operates within positions across cognitive orientations, not only across structural positions, is structural — it follows from the extended-range computation but restates a cross-position theorem in an intra-position context). Two-regime institutional boundary interpretation (the population counts, zero identity overlap, and Nash property divergence are strict; the interpretation that the dissent route and uncertainty route are two manifestations of the same π = −0.2 phase transition operating at different distances from the classification threshold is structural — the mechanism is inferred from the sign-flip geometry rather than derived from axioms). Domain intersection interpretation (the 8-domain intersection is strict; the claim that these domains name the space where the institutional phase transition is simultaneously active in both modes is structural). Epistemic risk interpretation (the claim that uncertainty-route constraints represent cases where structural correction preempts disagreement is structural — the pre-correction probability mass is observed, but the counterfactual "would be misclassified without override" is inferred from the distribution shape rather than tested by ablation). Three-regime partition interpretation (the fragile/robust consensus distinction via Fisher median split is structural — the computation is strict, but the interpretation that fragile consensus constraints are held by ε-dependent likelihoods while robust consensus constraints are held by signature-mediated coordination detection is inferred from the type composition divergence rather than derived from axioms). Signature-dependence vs. ε-sensitivity interpretation (the claim that the uncertainty route operates through a mechanistically different channel than the dissent route — signature load-bearing in a flat curvature region vs. threshold crossing in a steep curvature region — is structural, inferred from the observer curvature hierarchy rather than proved).

### 6.4 What Is LOOSE

Type space as Heyting algebra (two absorbing elements prevent it). Power scaling as adjunction (triangle identities unverified). Signature resolution as lattice meet (priority dispatch table, not lattice operation). All five Girard/Linear Logic mappings (confusing computing a quantity with consuming a resource). The quantum measurement analogy (breaks at reversibility, determinism, and locality). "Quantum" naming in verification triggers (evocative but misleading if taken formally).

### 6.5 What the Framework Cannot Do

**Plan under resource constraints.** Computes costs but does not enforce budgets — the genuine gap identified by Girard analysis.

**Flag overdetermined convergence within linked constraint sets.** When the engine produces asymmetric findings across a set of constraints sharing a beneficiary — Mountain + extractive types, convergent drift, convergent abductive signals — the current architecture presents each constraint's report independently. The scenario_convergence module (v6.9) partially addresses this as a Python post-processor, but the convergence diagnostic does not feed back into per-constraint classification or report generation. A fully integrated version would aggregate cross-constraint signals into the per-constraint diagnostic verdict, flagging when the engine's own outputs have narrowed the conclusion space beyond genuine ambiguity. This remains a post-processing capability rather than an engine-level feature.

**Validate ε against ground truth.** Axiom 2's directionality mechanism is now validated at the metric level: f(d(P)) accounts for 98.6% of inter-observer χ variance across 3,252 constraints. What remains unvalidated is whether the input metrics (ε, base suppression) accurately measure the real-world structural properties they are intended to capture. This validation requires domain-expert ground truth rather than corpus analysis: a study comparing LLM-generated ε against expert judgment on a held-out sample would test whether the empirical anchor maps to the world, which the current corpus-level validation cannot establish.

**Model intra-level dynamics.** The power chain is a static site. Power and benefit asymmetries enter as parameters of perception, not as generators of temporal flows at the same observer level. The framework captures what different observers see at an instant; it does not model U₃ broadcasting a narrative, U₁ partially internalizing it, and the type labels changing through those interactions. The cognitive displacement infrastructure (§5.6) provides a first-order model of intra-position variance — two observers at U₂ with different cognitive orientations can now produce different classifications — and the δ-band population analysis (§5.7) identifies which constraints are sensitive to this variance and at which positions. The margin analysis infrastructure extends the range to atypical observers (δ ± 0.25), producing per-constraint classification ranges that bracket the full discretion available across cognitive orientations. But all of this models the variance as static perturbation: what observers with different orientations would see at a single moment, not how one observer's classification changes another's through interaction. A fully dynamic model would require temporal site extension.

**Extend to infinite or non-linear sites.** The H¹ gap depends on observer count (|Ob(C)| = 4), not on morphism structure, and holds on any 4-object category. The spectral structure and Nash distance formalism depend on the specific site geometry. Results are geometry-relative. Any 4-element site with a sign-reversing observer will produce the same H¹ gap pattern; spectral decomposition and restriction map composition would change under different morphism structures. This is a feature, not a limitation: the framework is a functor from site choices to invariants, and the current results characterize the 4-element linear site.

**Distinguish framework properties from LLM priors.** The structural invariants under inversion demonstrate that axiom-derived properties are stable across different LLM-generated corpora. Both corpora inherit whatever latent political grammar their training data shares. The invariants that hold are properties of the axioms; the statistics that vary are properties of the data.

### 6.6 What Would Strengthen the Framework

1. **Metric-level sensitivity analysis (partially addressed).** Testing whether input metrics (ε, suppression) are robust to small perturbations — whether classifying a constraint as snare vs. tangled_rope changes under ε perturbation of ±10%. The Fisher information analysis (§5.8) computes ε-sensitivity of the raw MaxEnt distribution for the 243 Arakelov unique constraints, finding bimodal curvature and a three-regime partition. What remains is extending the analysis to the full corpus and to suppression/theater perturbation, and addressing the ε-quantization limitation: three ε values cover 94% of the Arakelov set, constraining the resolution of continuous sensitivity measures. A corpus with greater ε diversity would provide a sharper test.

2. **Real-world corpus.** The current corpus is LLM-generated. A corpus of constraints drawn from legal, regulatory, and institutional documents — with metrics assigned by domain experts — would test whether the framework's classifications match judgments made by people with direct knowledge of the constraints.

3. **Corpus diversity (quantified).** Axiom reachability analysis finds 1,404 orbit patterns (4-element type tuples) reachable by the classification cascade under any valid input, of which the corpus exercises 42 (3.0%). Under the standard power_role_heuristic (without per-constraint directionality overrides), ~217 orbits are reachable and the corpus covers ~19%. The 562 metric-only unrealized orbits — reachable through continuous (ε, suppression, theater) variation without requiring specific signatures — represent the most actionable corpus gaps: regions of metric space the LLM generation process systematically avoids. The 800 signature-gated unrealized orbits require specific boolean feature combinations that may be less natural for LLMs to generate. The corpus is living and grows with each analytical run; the unrealized orbit list provides a concrete development roadmap specifying what input regions would populate each structural gap.

4. **Non-linear site extension.** A DAG site (where a worker can be economically powerless but legally powerful) would change spectral and restriction-map-dependent invariants but not the H¹ gap, which depends on observer count rather than morphism structure. The substantive extension is either non-poset site structure (multiple incomparable intermediate observers creating genuine loops in the nerve) or path-dependent restriction maps (measuring ρ₁₃ − ρ₁₂ ∘ ρ₂₃ as discrete holonomy). Understanding which results depend on observer count versus morphism structure would sharpen the distinction between cardinality-relative and geometry-relative theorems.

5. **Temporal site extension.** Constraints change over time: a tangled_rope may naturalize into a mountain, or a rope may tighten into a snare. The coupling protocol is implemented but dormant — it requires multi-snapshot corpus data. Longitudinal measurement data and a site with time as a morphism dimension would enable the temporal dynamics the current framework cannot model.

6. **Per-constraint metric robustness (partially addressed).** Testing whether the classification of individual constraints is stable under small ε perturbations, going beyond the parameter-level bifurcation sweep to the input-metric level. The Fisher curvature analysis (§5.8) provides per-constraint ε-sensitivity for the Arakelov unique set, revealing that sensitivity is uncorrelated with confidence margin (r = 0.064) and bimodally distributed. Extension to the full corpus and to other input metrics (suppression, theater) would complete this item.

7. **Scope modifier refinement.** The scope modifier σ is observer-specific and constraint-independent; a constraint-specific variant (encoding intrinsic jurisdictional reach) would require new per-constraint scope metadata and is expected to have minimal classification impact given that σ accounts for less than 7% of inter-observer χ variance and all scope parameters are classification-inert under ±25% perturbation. Whether the architecturally fixed σ values ([0.8, 1.0, 1.0, 1.2]) mask meaningful per-constraint variation is an open empirical question; an audit comparing constraint-specific scope estimates against the fixed values would determine whether the constancy assumption is justified or merely convenient.

8. **Persistence barcodes across all parameters.** The current persistence infrastructure sweeps 6 of 154 parameters. Extending to all parameters where classification changes occur would give a complete picture of which constraints are structurally robust and which are parameter-sensitive.

9. **Nash distance metric-constrained version.** The current Nash distance uses conservative structural computation (any chain type allowed). A metric-constrained version — allowing only type changes supportable by the constraint's χ value under parameter perturbation — would give a tighter measure of true manipulability.

10. **Per-constraint diagnostic walkthrough.** A structured walkthrough of 3–4 constraints — showing the full diagnostic stack, theorem instantiation, game-theoretic profile, and the contrast between a positional extraction case (nash=1, persistent) and a distributed extraction case (nash=3, fragile, stereotyped orbit) — would demonstrate the engine's analytical output more concretely than corpus-level statistics.

11. **Temporal coupling inference.** The infer_structural_coupling/3 mechanism produces 0 inferred edges on a single-snapshot corpus. Longitudinal measurement data would activate this mechanism and enable the temporal dynamics analysis the current framework cannot provide.

12. **Per-position cognitive displacement profiles (partially addressed).** The δ infrastructure now includes a δ-band population analysis identifying which constraints are sensitive to cognitive orientation at each observer position, and a psychometric mapping to Cultural Cognition's hierarchy-egalitarianism (→ δ) and individualism-communitarianism (→ contamination network sensitivity) axes. The margin analysis infrastructure computes per-constraint classification ranges under extended δ (±0.25), with per-zone evidence signatures describing conversational fingerprints that identify where a specific observer sits within the range. What remains open is empirical calibration: mapping actual Cultural Cognition scores (or SDO / Moral Foundations scores) to δ magnitudes requires the study design specified in §5.7 — subjects with known psychometric profiles classifying constraints near the δ-band at multiple simulated structural positions. The framework predicts the direction of the mapping; the magnitude is ungrounded.

13. **Boundary thickness as classification quality metric (partially addressed).** The margin analysis now computes the exact δ value at which each classification boundary is crossed (via binary search on the sigmoid), giving per-constraint boundary thickness. Thin-boundary constraints are cognitively fragile; thick-boundary constraints are robust to observer orientation. This metric is computed for all 498 sensitive constraints and is distinct from both the MaxEnt shadow (metric uncertainty) and the parametric persistence barcodes (parameter sensitivity). What remains is integration into the standard report output alongside type classification, so that users see boundary thickness as a confidence measure without needing to consult the margin analysis infrastructure directly.

14. **Empirical δ calibration study.** The most important outstanding validation for the intra-position analysis. The δ-band population analysis generates specific predictions: subjects with hierarchical Cultural Cognition profiles should classify δ-band constraints toward coordination, subjects with egalitarian profiles toward extraction, and the split should concentrate at the powerless observer position where 93% of δ-band activity occurs. A study design with N = 100+ subjects, known CC/SDO scores, classifying 30–50 constraints near the δ-band at simulated U₁ and U₂ positions would test whether the hierarchy-egalitarianism axis predicts classification behavior, whether δ magnitude scales with CC score intensity, and whether the position-dependence of δ (the interaction between trait orientation and structural position) is as the framework predicts. This study would either ground δ in psychometric measurement or reveal that the intra-position variance has a different source than cognitive orientation toward power — either outcome advances the framework.

15. **Temporal reclassification trigger (hard override).** T17 (v6.9) fires as an advisory trigger when a Mountain shows rising extractiveness. A hard override — where extraction_accumulation at critical severity triggers automatic reclassification — would require drift scan to precede classification in the pipeline, reversing the current execution order. The temporal_signature mechanism would: (1) persist drift facts as assertz'd temporal_override/2 facts after drift scan, (2) add a constraint_signature/2 clause for temporal_mountain_accumulation, (3) add a resolve_modal_signature_conflict case. This is the architectural change that would make temporal reclassification automatic rather than advisory.

16. **Cross-constraint classification propagation.** The contamination network propagates purity but not classification implications. Reclassifying one constraint does not affect linked constraints' classifications or omega spaces at the engine level. The scenario_convergence and omega_cross_constraint modules (v6.9, Python post-processors) partially address this, but a fully integrated version would extend drl_purity_network.pl to propagate classification pressure — where a reclassification of constraint A narrows the omega space of constraints that cite A as justification. Major complication: Mountain immunity (contamination_strength = 0.0) would need to be differentiated — immune to purity contamination but not necessarily to classification pressure from reclassified neighbors.

---

## 7. Related Work

**Topos-theoretic approaches in physics** (Isham and Butterfield 1998; Döring and Isham 2008). Both are presheaves on sites of contexts; both formalize context-dependent truth. Key disanalogy: quantum measurement involves irreversibility, stochasticity, and entanglement, none of which are present in DR.

**Contextuality and sheaf cohomology** (Abramsky and Brandenburger 2011). Abramsky and Brandenburger formalize quantum contextuality using sheaf cohomology: a system is contextual iff the obstruction to a global hidden-variable assignment is non-trivial in H¹. DR's refusal of sheafification is structurally parallel — perspectival disagreement is the obstruction, and the contextuality fraction (CF = H¹/6 per constraint) operationalizes this obstruction in DR's discrete setting. The CF gap result — values forbidden at 1/6 and 1/3 by the site geometry — has no direct analogue in the quantum contextuality literature, where the admissible contextuality fractions depend on the specific measurement scenario rather than on a fixed site structure. Key disanalogy: AB contextuality arises from incompatible measurement bases in quantum mechanics; DR contextuality arises from power-modulated perception across structurally distinct observer positions. The mathematics is similar; the source of incompatibility differs.

**Game theory and mediating structures** (von Neumann and Morgenstern 1944). Von Neumann's insight that the game — the mediating structure — cannot be eliminated by solving from either player's perspective corresponds structurally to DR's refusal of sheafification. The Nash equilibrium orbit analysis (§5.5) formalizes this connection: the presheaf is the game, H¹ measures how far the game is from having a dominant-strategy equilibrium, and Nash distance measures the minimum coordination required to achieve global consistency. The connection to Peirce's Thirdness (a relation not decomposable into dyadic components) applies in both cases: neither the game nor the presheaf reduces to the sum of its participants' perspectives.

**Computational social science.** DR is not machine learning. Classification is computed from continuous metrics via a hand-designed deterministic rule cascade, and the central question is not "which label is correct?" but "how does the label depend on who is labeling?"

**Markov categories** (Fritz 2020). The correct abstraction for DR's MaxEnt layer — capturing compositionality without requiring the Giry monad's distribution-over-distributions structure.

**Sheaf Laplacians** (Hansen and Ghrist 2019). Applied to DR's path-graph site, confirms the institutional phase transition as the dominant spectral feature.

**Topological data analysis** (Edelsbrunner and Harer 2010). Persistence barcodes applied to DR's parameter space characterize the structural stability of each theorem's predictions. The novelty is applying persistence to the parameter space of a classification presheaf rather than to a point cloud; the methodology is transferable to any classification system with threshold parameters.

**Cultural cognition and risk perception** (Kahan, Braman, et al. 2007, 2012). Cultural Cognition theory explains why individuals with different cultural worldviews classify the same risk or policy differently, using two axes: hierarchy-egalitarianism and individualism-communitarianism. The structural parallel to DR is precise: both frameworks model classification disagreement as a function of the classifier's orientation rather than as an error to be resolved by better information. DR's δ parameter (intra-position cognitive displacement) corresponds to the hierarchy-egalitarianism axis; DR's contamination network sensitivity corresponds to the individualism-communitarianism axis. Key disanalogy: Cultural Cognition was designed for policy risk perception and measures trait-level cognitive orientation, while DR models structural position as the primary classification determinant with cognitive orientation as a secondary perturbation. The frameworks occupy complementary levels — DR provides the formal machinery for computing where classification disagreement concentrates, while Cultural Cognition provides a psychometric instrument for predicting which individuals disagree. The δ-band population analysis (§5.7) identifies the specific constraints and observer positions where Cultural Cognition scores should predict classification behavior, generating testable predictions for an empirical bridge study.

**Social dominance and system justification** (Pratto, Sidanius et al. 1994; Jost, Banaji 1994). Social Dominance Orientation measures preference for group-based hierarchy; System Justification measures the tendency to perceive existing arrangements as fair. Both map to DR's δ parameter: high SDO / high system justification corresponds to negative δ (perceiving hierarchy as coordination), low SDO / low system justification to positive δ (perceiving hierarchy as extraction). System Justification theory's prediction that disadvantaged groups sometimes justify extractive systems corresponds to a specific δ configuration (negative δ at U₁) that DR models as a clamp-boundary effect — the powerless observer's effective d cannot go below zero, but negative δ pushes d toward the sigmoid center where the coordination classification becomes more accessible. The empirical finding that SDO is relatively stable across framing while system justification varies with structural position makes them complementary instruments for calibrating DR's trait-level versus position-dependent δ components.

**Standpoint epistemology** (Harding 1986; Haraway 1988). Both are already cited but not discussed. Standpoint epistemology's central claim — that structural position shapes epistemic access, and that marginalized positions can expose features of social arrangements invisible from dominant positions — is the informal thesis that Axiom 2 formalizes. The deep fracture regime (§5.7: 39.4% of fractured constraints where disagreement is entirely position-determined and no amount of shared information resolves it) is a presheaf-level restatement of the standpoint claim: for these constraints, the institutional observer's classification is structurally correct from that position and structurally wrong from the powerless position, and the disagreement cannot be dissolved by cognitive reorientation within either position. The δ-band concentration at U₁ (93% of sensitive constraints) formalizes a second standpoint prediction: among position-sensitive constraints, it is the powerless observer's classification that is most modulated by cognitive orientation, not because U₁ lacks information but because constraint-specific directionality values cluster near the sigmoid midpoint at that position. Key disanalogy: standpoint epistemology typically privileges the marginalized perspective as epistemically superior. DR does not privilege any position — it measures the structure of their disagreement. The oracle gap (Theorem 4) shows that cross-position analysis dominates any single position, including U₁. Standpoint theory identifies which positions see what; DR measures why they must disagree and by how much.

**Foucault and power-conditioned visibility** (Foucault 1975, 1980). Axiom 2's empirical anchor — power reduces experienced extraction — and the cover story mechanism (Theorem 1) formalize a specific dynamic that Foucault theorizes qualitatively: that power operates partly by configuring what counts as visible. The institutional sign-flip (f(d) < 0 at U₃) is a presheaf-level analogue of Foucault's claim that power produces knowledge formations in which extraction appears as legitimate coordination — not through deception but through the structural optics of the position itself. The institutional dissenter finding (44% of the corpus) and spectral dominance of U₃ (97%) confirm that this is not a marginal effect but the dominant structural feature of classification disputes. Key disanalogy: Foucault's method is genealogical and discursive — it traces how historically specific formations produce specific knowledge effects. DR is axiomatic — it derives these effects as consequences of a sigmoid function and observer geometry, without historical specificity. Foucault would likely resist DR's commitment to observer-independent base extractiveness (ε-invariance), which presupposes exactly the stable substrate his genealogies aim to dissolve. The frameworks are complementary rather than nested: Foucault explains which constraints become naturalized in which societies and through which institutional mechanisms; DR provides formal machinery to detect that naturalization has occurred, measure its depth, and identify its structural signatures (FNL, FCR, FSM).

**Ecological psychology and observer-relative structure** (Gibson 1979). Gibson's affordances — properties of the environment specified relative to an organism's capacities — share DR's commitment to observer-relative objectivity: an affordance is a real relational property, not a subjective projection. DR's χ = ε × f(d(P)) × σ(S(P)) is structurally an affordance-like quantity, a position-indexed evaluative function over an invariant substrate. The disanalogy is fundamental: Gibson treats affordances as directly perceivable and non-ideological, whereas DR's central result (Theorem 1) is that power-modulated perception produces systematic misclassification that is structurally invisible to the observer experiencing it. Citing Gibson too strongly would imply a commitment to perceptual veridicality that the cover story mechanism explicitly denies.

**Habitus and structured disposition** (Bourdieu 1990). DR's δ parameter (intra-position cognitive displacement) occupies a structural role similar to Bourdieu's habitus: both treat within-position perceptual variance as the product of structured dispositions rather than random noise. The δ-band analysis (§5.7) shows that cognitive orientation at a fixed structural position systematically shifts classification output, generating genuine disagreement among observers who share both position and information — a finding Bourdieu would recognize as position-conditioned misrecognition. DR deliberately abstracts from the historical genesis and embodied reproduction that define habitus; δ is an adjustable parameter, not a temporally sedimented disposition. The 4-object linear site is a minimal classification apparatus, not a model of a Bourdieuian field with its multi-dimensional capital structure and competitive dynamics.

**Social choice and classification aggregation** (Arrow 1951; Sen 1970). Both are already cited but not discussed in relation to the presheaf structure. Arrow's impossibility theorem shows that aggregating individual preference orderings into a consistent social ordering under plausible axioms is structurally blocked. DR's H¹ gap theorem is an analogous result for classification rather than preference: aggregating observer-relative type assignments into a consistent global section is forbidden for specific cohomological values determined by the site geometry. The oracle gap (Theorem 4) plays the role of Arrow's dictator result inverted — rather than a single position dominating, DR shows that no single position captures more than 3% of the cross-position classification structure. The analogy is structural, not formal: Arrow operates on ordinal preferences over alternatives with independence and unanimity axioms; DR operates on categorical type assignments over observer contexts with presheaf naturality. But the shared lesson — that perspectival aggregation under plausible structural constraints is provably obstructed — places DR in a lineage of impossibility results about multi-agent classification.

**Pyrrhonian skepticism and constrained non-closure** (Sextus Empiricus, *Outlines of Pyrrhonism*). The paper uses the phrase "Pyrrhonist commitment" (§8) to describe the refusal to collapse perspectival fracture into a single correct classification. The connection is precise but limited. DR shares with Pyrrhonism an aversion to premature adjudication: the refusal of sheafification, the contextuality fraction, and the STRICT/STRUCTURAL/LOOSE taxonomy implement a disciplined suspension of the question "whose classification is right?" while still permitting strong structural claims about how disagreement must organize. This is selective non-closure — epochē applied to adjudication, not to structure. DR is emphatically not globally Pyrrhonian: it asserts ε-invariance as a design axiom, derives theorems as structural necessities, and treats the empirical anchor as testable. The "Pyrrhonist commitment" is better understood as a methodological discipline — measuring the fracture rather than resolving it — than as a skeptical stance toward the framework's own claims. The scope limit identified in §8 (where convergent asymmetric measurement narrows the conclusion space) marks the boundary where even this selective non-closure becomes untenable: when the meter's readings constrain which evaluative conclusions are defensible, continued suspension is itself a positional claim.

## 8. Conclusion

Classification of social structures depends irreducibly on who is observing. This paper shows that this dependence has formal mathematical structure and derives specific consequences from a small set of axioms.

The framework's core commitment is a single empirical hypothesis: power modulates the perception of extraction. Encoded as a presheaf on a site of observer positions, this hypothesis produces consequences not visible from informal intuition alone. Extraction cannot be universally perceived as such. Disagreement clusters in discrete blocs. The institutional observer carries 97% of the spectral weight. Single-position analysis is provably almost blind to cross-position structure. These are theorems, not findings.

The empirical record has deepened substantially since the framework's initial formulation. Structural invariants survive FCR ablation, corpus growth from 887 to 3,254 constraints, and inversion of input distributions — confirming they are fixed-point attractors of the axioms. Wasserstein transport and H¹ are incommensurable measures of perspectival fracture — neither bounds the other, and the apparent correlation (ρ = 0.91) is an artifact of the H¹ = 0 mass. Variance decomposition provides the first direct metric-level validation of Axiom 2's directionality mechanism. Persistence barcodes distinguish robust perspectival fracture from threshold artifacts. The game-theoretic analysis reveals that 69.3% of non-constant orbits are Nash-stable, that all FCR cover stories are structurally forced, and that the anti-correlation between Nash distance and persistence resistance identifies a phase transition between two structurally distinct extraction regimes. The institutional observer's vulnerability is not a calibration artifact — it persists across the full range of power modifier calibrations as a consequence of two independent structural mechanisms. A cognitive displacement analysis tests whether these claims survive intra-observer variation: a perturbation parameter δ modeling systematic perceptual bias within a structural position. All structural invariants — the H¹ gap, spectral dominance, and the institutional sign-flip — are robust across δ ∈ [−0.15, +0.15]. The analysis reveals that classification sensitivity to cognitive orientation is concentrated at the moderate observer position, where the sigmoid derivative is highest, and that the institutional observer's classification sensitivity and Nash vulnerability concentration are decoupled phenomena occupying different regions of the constraint space. A δ-band population analysis extends the displacement work from invariant robustness to constraint-level classification: 498 of 822 fractured constraints (60.6%) are sensitive to intra-position cognitive orientation, while the remaining 324 (39.4%) are in a deep fracture regime where disagreement is entirely position-determined. The sensitive population concentrates at the powerless observer (93%), because constraint-specific effective directionality values cluster near the sigmoid midpoint at that position — a finding invisible to the canonical-d analysis. Under extended δ modeling atypical observers, 90% of sensitive positions span the full rope-to-snare classification range, restating Theorem 1 at the intra-position level.

Three open questions define the frontier. The ε validation problem remains: whether LLM-generated base extractiveness correlates with domain-expert judgment requires a targeted empirical study with held-out constraints and human raters. The non-linear site extension would determine which results are fundamental and which are artifacts of the current 4-element chain — the H¹ gap depends on observer count (not linearity) and would persist on any 4-object site, while spectral structure and restriction map composition depend on morphism geometry and would change under a DAG or non-poset site. The temporal coupling mechanism is implemented but dormant; activating it requires longitudinal measurement data and would transform the framework from a static classifier into a dynamic model of how constraints evolve through the observer positions they exploit. A fourth question has been partially answered: whether the framework's point-observer model (one classification per position) is adequate, or whether intra-position variance requires a fiber-bundle extension. The δ-band population analysis shows that the adequacy question has a structured answer — 39.4% of fractured constraints are in the deep fracture regime where point-observer classification is adequate (cognitive orientation is irrelevant), while 60.6% are in the sensitive regime where the point-observer model compresses genuine intra-position classification variance into a single label. For the sensitive population, the margin analysis provides the fiber content (the full range of classifications available at each position under varying cognitive orientation) without requiring restructuring of the categorical machinery. A fifth question is now specified: whether the δ parameter can be grounded in psychometric measurement. Cultural Cognition's hierarchy-egalitarianism axis is the strongest candidate instrument; the δ-band analysis generates the specific predictions (which constraints, at which positions, with which classification splits) that a calibration study would test.

The framework does not explain why extraction requires perspectival cover. That remains a sociological claim requiring domain theory. What the framework provides is formal machinery to establish that perceptual non-universality holds under power-modulated classification, to measure how much of a domain is perspectivally fractured, and to identify the specific geometric structure of that fracture — including which disagreements are Nash-stable, which cover stories are structurally forced, and which constraints sit in regimes where the extraction cannot be classified away by any unilateral reclassification.

**Broader stakes.** The formal results have implications beyond the social-constraint domain. For democratic theory: if institutional perception is spectrally decoupled from other positions (Theorem 3), institutional actors designing reforms are working from a classification structurally orthogonal to the experience of those affected — not because they ignore the data, but because their position transforms it. For regulatory design: if single-position analysis misses more than 97% of cross-position structure (Theorem 4), regulatory impact assessments conducted from a single vantage point are provably almost blind to the effects that matter most. For epistemic justice: if extraction structurally requires perceptual non-universality (Theorem 1), the demand to "prove extraction exists" from the perspective of its beneficiaries is not a neutral epistemic standard but a structural impossibility — the beneficiary's position is precisely where extraction is invisible. For intervention design: the two-regime finding has practical consequences — positional extraction (Nash-stable, parameter-robust, deep fracture) resists both reclassification strategies and cognitive reorientation, while distributed extraction (stereotyped four-way disagreement, parameter-fragile, cognitively sensitive) may be amenable to targeted metric adjustment or cognitive reframing within positions. The deep fracture / sensitive distinction tells practitioners which tool applies: for deep fracture constraints, changing how people think about the constraint is structurally futile — intervention requires changing structural position (policy, institutional reform, redistribution of power); for sensitive constraints, cognitive orientation is a lever, and the margin analysis specifies which orientations produce which classifications. For institutional epistemology: the cognitive displacement analysis shows that moderate-power actors — committee members, middle managers, regulators — are the observers whose cognitive orientation has maximum leverage on classification output; a harmony-oriented classification population systematically increases Nash stability (making extraction harder to dissolve through single-position reclassification), while a risk-sensitive population slightly increases the number of resolvable configurations. For the powerless: the δ-band population analysis reveals that among the people most affected by constraints, cognitive orientation produces the widest classification disagreement — two workers at the same structural position with different cultural worldviews would disagree about whether the same arrangement constitutes coordination or extraction. This is not a finding about ignorance or misinformation; it is a structural prediction that the cover story mechanism operates within positions, generating genuine perceptual disagreement among people who share both structural position and informational access. These connections are interpretive rather than formal, but they indicate where the framework's mathematical results make contact with questions that matter.

The presheaf should not be sheafified. The framework's value lies precisely in measuring perspectival fracture — in quantifying the gap between local truth and global truth, and in identifying the structural patterns in that gap. The descent rate, the H¹ distribution, the near-absence of snares from H⁰, the institutional observer as dominant dissenter, the oracle gap, the contextuality fraction, the Nash-stable orbits, the stereotyped distributed extraction signature — these are features of the presheaf's failure to be a sheaf. Sheafification would erase them. The truth of a social system, on this account, is not the consensus but the fracture itself.

**Scope limit of the meter framing.** This paper describes DR as "a meter for perspectival fracture, not a machine for identifying the correct political line." That self-description is accurate and important — the framework measures how disagreement is structured without adjudicating whose classification is right. However, the Zionism-domain analysis (v6.9) identified a scope limit: when the meter's own diagnostics converge asymmetrically across a linked constraint set — false summit detection on a Mountain, coordination-washing on a transfer consensus, naturalization on a historiographical narrative, all sharing a beneficiary and all showing rising extraction — the meter has measured something. Treating a convergent, asymmetric measurement as ambiguous is itself a measurement error. The scenario_convergence module flags this case; the framework documents (ethics.md v4.2, core.md v4.3) now distinguish genuine equipoise from false equipoise. The meter-not-compass framing holds for cases where observer positions diverge for independent structural reasons. It does not hold for cases where the meter's own readings have narrowed the conclusion space to the point where stated neutrality is a positional claim. Identifying this boundary — where the framework's structural findings constrain which evaluative conclusions are defensible — is not a departure from the Pyrrhonist commitment. It is what the Pyrrhonist commitment requires when applied to the framework's own outputs.

---

## References

Abramsky, S., & Brandenburger, A. (2011). The sheaf-theoretic structure of non-locality and contextuality. *New Journal of Physics*, 13(11).

Amari, S., & Nagaoka, H. (2000). *Methods of Information Geometry*. American Mathematical Society.

Arrow, K. J. (1951). *Social Choice and Individual Values*. Wiley.

Bourdieu, P. (1990). *The Logic of Practice*. Stanford University Press.

Čencov, N. N. (1982). *Statistical Decision Rules and Optimal Inference*. American Mathematical Society.

Döring, A., & Isham, C. J. (2008). "What is a thing?": Topos theory in the foundations of physics. In *New Structures for Physics*, Springer.

Edelsbrunner, H., & Harer, J. (2010). *Computational Topology: An Introduction*. American Mathematical Society.

Foucault, M. (1975). *Discipline and Punish: The Birth of the Prison*. Gallimard. [English trans. 1977, Pantheon.]

Foucault, M. (1980). *Power/Knowledge: Selected Interviews and Other Writings, 1972–1977*. Ed. C. Gordon. Pantheon.

Fritz, T. (2020). A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics. *Advances in Mathematics*, 370.

Gibson, J. J. (1979). *The Ecological Approach to Visual Perception*. Houghton Mifflin.

Hansen, J., & Ghrist, R. (2019). Toward a spectral theory of cellular sheaves. *Journal of Applied and Computational Topology*, 3.

Haraway, D. (1988). Situated knowledges. *Feminist Studies*, 14(3).

Harding, S. (1986). *The Science Question in Feminism*. Cornell University Press.

Isham, C. J., & Butterfield, J. (1998). A topos perspective on the Kochen-Specker theorem. *International Journal of Theoretical Physics*, 37(11).

Jost, J. T., & Banaji, M. R. (1994). The role of stereotyping in system-justification and the production of false consciousness. *British Journal of Social Psychology*, 33(1).

Kahan, D. M., Braman, D., Gastil, J., Slovic, P., & Mertz, C. K. (2007). Culture and identity-protective cognition: Explaining the white-male effect in risk perception. *Journal of Empirical Legal Studies*, 4(3).

Kahan, D. M., Peters, E., Wittlin, M., Slovic, P., Ouellette, L. L., Braman, D., & Mandel, G. (2012). The polarizing impact of science literacy and numeracy on perceived climate change risks. *Nature Climate Change*, 2.

Lawvere, F. W. (1969). Adjointness in foundations. *Dialectica*, 23(3–4).

Mac Lane, S., & Moerdijk, I. (1992). *Sheaves in Geometry and Logic*. Springer.

Noether, E. (1918). Invariante Variationsprobleme. *Nachrichten von der Gesellschaft der Wissenschaften zu Göttingen*.

Ostrom, E. (1990). *Governing the Commons*. Cambridge University Press.

Pratto, F., Sidanius, J., Stallworth, L. M., & Malle, B. F. (1994). Social dominance orientation: A personality variable predicting social and political attitudes. *Journal of Personality and Social Psychology*, 67(4).

Sen, A. K. (1970). *Collective Choice and Social Welfare*. Holden-Day.

Sextus Empiricus. *Outlines of Pyrrhonism*. [Trans. R. G. Bury, Loeb Classical Library, 1933.]

von Neumann, J., & Morgenstern, O. (1944). *Theory of Games and Economic Behavior*. Princeton University Press.

Wheeler, J. A. (1989). Information, physics, quantum. In *Complexity, Entropy, and the Physics of Information*, Addison-Wesley.

Yuen, H. (2023). A quantum complexity-theoretic reduction for the unitary synthesis problem. arXiv:2306.13073.

---

**What changed from v6.6:**

New section (§5.7) and targeted updates across eight existing sections. No structural changes to axioms, theorems, or the computational engine. All new findings are computed from the existing corpus via a post-processing module (cc_diagnostic.py) that reads enriched pipeline output without modifying the Prolog engine.

1. **Abstract:** Added paragraph summarizing δ-band population analysis — 498/822 sensitive constraints, deep fracture regime identification, powerless-position dominance (93%), extended-range 90% full-span result, Cultural Cognition as candidate psychometric bridge.

2. **§1 Introduction, roadmap:** Added mention of §5.7 δ-band population analysis in the section overview.

3. **§5.7 Intra-Position Cognitive Orientation (new section):** δ-band population analysis (498/822 sensitive, 324/822 deep fracture); H¹-band correlation (H¹ = 3 at 70% deep fracture vs. H¹ = 6 at 97% sensitive); powerless-position dominance (93% of δ-band activity, driven by effective d clustering near sigmoid midpoint rather than canonical d); extended-range classification span (90% span rope-to-snare under δ ± 0.25); 39% of boundary crossings require extended δ beyond standard range; boundary-crossing δ as classification quality metric; Cultural Cognition psychometric mapping (hierarchy-egalitarianism → δ, individualism-communitarianism → contamination network sensitivity); comparison with SDO and Moral Foundations; empirical study design specification.

4. **§6.2 STRICT list:** Added δ-band trigger computation, powerless-position dominance, H¹-band trigger distribution, effective d back-computation, extended-range classification span, boundary-crossing δ precision (zero monotonicity violations), contamination sensitivity gate.

5. **§6.3 STRUCTURAL list:** Added deep fracture / sensitive regime distinction, Cultural Cognition as candidate δ instrument, intra-position Theorem 1 restatement.

6. **§6.5, "Model intra-level dynamics":** Updated to acknowledge δ-band population analysis and margin analysis infrastructure as extensions of intra-position variance modeling.

7. **§6.6 items 12–14:** Updated items 12 and 13 from speculative to partially addressed (per-position profiles via Cultural Cognition mapping, boundary thickness via binary-search crossing computation). Added item 14: empirical δ calibration study as most important outstanding validation.

8. **§7 Related Work:** Added Cultural Cognition (Kahan et al.) as structural parallel for explaining classification disagreement via observer worldview. Added Social Dominance Orientation (Pratto, Sidanius) and System Justification (Jost, Banaji) as complementary psychometric instruments with specific δ correspondences.

**v6.8 amendment:** Theorem 2 gap attribution corrected from linearity to cardinality (|Ob(C)| = 4); proof sketch added showing partition argument; n=5 corollary added with forbidden set {1, 2, 3, 5}; methodological note distinguishing partition functional from sheaf-theoretic cohomology added after Theorem 2; DAG future-work claims in §2, §6.5, §6.6, and §8 made mathematically precise (contractible nerve, discrete holonomy as candidate extension).

9. **§8 Conclusion:** Added δ-band population findings and intra-position Theorem 1 restatement to empirical record. Extended open questions from four to five (adding empirical δ calibration). Updated fourth question (point-observer adequacy) with structured answer from deep fracture / sensitive analysis. Extended broader stakes with deep fracture / sensitive intervention distinction and powerless-position finding.

10. **References:** Added Jost & Banaji (1994), Kahan et al. (2007, 2012), Pratto et al. (1994).

---

**What changed from v6.7:**

New section (§5.8) and targeted updates to abstract, §1 roadmap, §6.2 STRICT list, and §6.3 STRUCTURAL list. New Prolog module (`arakelov_height.pl`) added to the engine. Pipeline output schema extended with three new per-constraint fields.

1. **Abstract:** Added paragraph summarizing Arakelov height diagnostic — 238 high-complexity constraints invisible to H¹/W₁/Nash diagnostics, two-regime institutional boundary analysis (dissent route vs. uncertainty route), constructive disjointness proof, consensus-manifold fragility.

2. **§1 Introduction, roadmap:** Added mention of §5.8 Arakelov height diagnostic in section overview.

3. **§5.8 Arakelov Height and the Two Boundary Regimes (new section):** Arakelov height construction (ε × (raw_uncertainty + conditional_pressure), max over 4 contexts); 238 unique high-complexity constraints with H¹ = 0, W₁ = 0, Nash = 0; two-regime institutional phase transition (dissent route: 162 constraints with observer disagreement; uncertainty route: 238 constraints with fragile consensus); constructive disjointness (Nash > 0 requires H¹ > 0 or W₁ > 0, excluding Arakelov unique set by definition); zero identity overlap between populations; context distribution (powerless 66.8%, institutional 17.6%); domain intersection analysis; Nash complementarity (disagreement manifold vs. consensus manifold); epistemic risk of signature-dependent classifications invisible to perspectival diagnostics.

4. **§6.2 STRICT list:** Added Arakelov height computation, raw confidence margin, conditional-only signature pressure, memoized threshold, constructive disjointness proof, zero identity overlap, Nash distance = 0 for all 238 Arakelov unique constraints.

5. **§6.3 STRUCTURAL list:** Added two-regime institutional boundary interpretation, domain intersection interpretation, epistemic risk interpretation (signature preempting disagreement).

6. **Engine:** New Prolog module `prolog/arakelov_height.pl` with predicates: `arakelov_height_pair/3`, `arakelov_height/2`, `arakelov_height_context/2`, `raw_confidence_margin/3`, `post_confidence_margin/3`, `signature_pressure/3`, `high_complexity_constraint/1`, `arakelov_threshold/1`. Added to `stack.pl` and `json_report.pl`. Schema updated in `python/shared/schemas.py`. Pipeline output fields: `arakelov_height`, `arakelov_height_context`, `signature_pressure`.

---

**What changed from v6.8 (previous section above):**

H¹–W₁ complementarity analysis. No code changes — paper-only update based on corpus-wide joint distribution analysis of 3,274 constraints.

1. **Abstract:** Replaced one-directional Wasserstein claim ("detecting sub-threshold distributional shift invisible to H¹") with bidirectional complementarity statement. Added Spearman confound note.

2. **§5.1 Wasserstein paragraph:** Expanded from single qualitative paragraph to four paragraphs with quantitative backing. Added: two falsification mechanisms (non-chain type invisibility: 39 cases; threshold-MaxEnt decoupling: 62 cases), H¹=6 anomaly (lowest mean W₁ despite maximal obstruction), non-monotone W₁ means across H¹ bands, Spearman decomposition (ρ = 0.91 overall → 0.40 within H¹ > 0 → −0.19 within H¹ ≥ 4).

3. **§5.8 dissent route:** Corrected "H¹ > 0, W₁ > 0" to note that 62 of 162 institutional_dissent constraints have W₁ = 0, with cross-reference to §5.1 complementarity.

4. **§8 Conclusion:** Updated one-directional Wasserstein claim to complementarity statement.

---

**What changed from v6.8 (Fisher ε-sensitivity amendment):**

Fisher information analysis of the consensus manifold. New Python diagnostic (`python/epsilon_sensitivity.py`). Paper updates to §5.8, abstract, §6.2, §6.3, §6.6. Output: `outputs/epsilon_sensitivity_results.json`.

1. **Abstract:** Added three-regime partition (disagreement / fragile consensus / robust consensus), Fisher-confidence_margin non-redundancy (r = 0.064), observer curvature hierarchy (analytical >> institutional by 20×), signature-dependence mechanistic interpretation.

2. **§5.8:** Added five new paragraphs after Nash complementarity: (a) Fisher information construction on raw MaxEnt distribution (C^∞ differentiability, h = 0.01 finite differences); (b) non-redundancy result (r = 0.064 vs. confidence_margin) and bimodal Fisher distribution; (c) three-regime partition — fragile consensus (143, 83% snare) vs. robust consensus (89, 70% snare + 28% tangled_rope); (d) observer curvature hierarchy confirming |f(d)| structural dependence, U₃ prediction failure (institutional curvature 20× lower than analytical), mechanistic interpretation of dissent route vs. uncertainty route; (e) corpus construction caveat (ε-quantization: 3 values cover 94%).

3. **§6.2 STRICT list:** Added Fisher information computation, finite difference method, correlation coefficient, observer curvature hierarchy, three-regime partition counts and type compositions.

4. **§6.3 STRUCTURAL list:** Added three-regime partition interpretation (fragile/robust via Fisher median), signature-dependence vs. ε-sensitivity interpretation.

5. **§6.6 items 1 and 6:** Updated from unaddressed to partially addressed, noting Fisher analysis scope and ε-quantization limitation.

6. **Engine:** New Python module `python/epsilon_sensitivity.py` (~220 lines). Uses `python/shared/maxent.py` shadow classifier with ε perturbation. Multi-observer extension via `perspective_chi` pipeline fields. Output: `outputs/epsilon_sensitivity_results.json`.

---

**What changed in v6.9 — False Summit Override and Scenario Diagnostics:**

Engine additions motivated by analysis of three Zionism-domain constraint reports (demographic_elimination_imperative, transfer_as_policy_consensus, historiographical_legitimation_struggle) which revealed five architectural gaps where diagnostic signals did not feed back into classification.

1. **False Summit Mountain (FSM) signature override:** New detection predicate `false_summit_mountain/2` in `signature_detection.pl`. Fires when: metric-based type = mountain AND `narrative_ontology:constraint_beneficiary(C, B)` succeeds (at least one beneficiary declared). Coupling (`cross_index_coupling`) is collected as diagnostic evidence but is NOT a gate — Mountain immunity prevents the contamination network from registering the structure, so coupling scores are typically zero on false summits. Wired through `constraint_signature/2` and `resolve_modal_signature_conflict/3` following the FNL/FCR reference pattern. Override target: tangled_rope (configurable via `false_summit_override_target`). New config param: `false_summit_override_target` (default: tangled_rope). Authoring layer updated: schema requires omegas when Mountain declares beneficiaries; generation prompt documents FSM authoring pattern.

2. **T17 abductive trigger (mountain_extraction_accumulation):** New trigger in `abductive_triggers.pl`. Fires when: `dr_type(C, Context, mountain)` AND `drift_event(C, extraction_accumulation, evidence(...))` at warning or critical severity. Advisory only — produces hypothesis for investigation, does not override classification. Hard override deferred: would require drift scan to precede classification in the pipeline, reversing current execution order.

3. **Cross-constraint omega narrowing (`omega_cross_constraint.py`):** New Python post-processor. Groups constraints by shared beneficiary. Annotates per-constraint omega spaces with cross-constraint findings (extractive type counts, convergent abductive triggers). Output: `outputs/omega_cross_constraint.json`, `outputs/omega_cross_constraint_report.md`.

4. **Scenario-level convergence diagnostic (`scenario_convergence.py`):** New Python post-processor. Detects four asymmetric convergence patterns across beneficiary groups: `mountain_extraction_cover` (Mountain + extractive types sharing beneficiary), `coordinated_extraction` (all group members extractive), `convergent_extraction_accumulation` (≥2 constraints with extraction_accumulation drift), `convergent_abductive_signals` (dominant triggers across group). Output: `outputs/scenario_convergence.json`, `outputs/scenario_convergence_report.md`.

5. **Cross-constraint classification propagation (Gap C — deferred):** Specified but not implemented. Would require: `neighbor_classification_implication/4` in `drl_purity_network.pl`, modification of `integrate_signature_with_modal/3` to accept network-derived classification pressures, and differentiation of Mountain immunity (immune to purity contamination but not necessarily to classification pressure from neighbors). Architectural impact too high for this session; FSM override covers the most important Mountain reclassification cases.

6. **Authoring layer alignment:** `constraint_story_schema.json` updated with FSM-candidate rule (Mountain + beneficiaries → require omegas). Generation prompt updated: beneficiary guidance, FSM subsection, checklist, corpus balance table. Compiler (`generate_constraint_pl.py`) required no changes — already emits `constraint_beneficiary/2` facts correctly.

---


Bourdieu, P. (1990). *The Logic of Practice*. Stanford University Press.

Foucault, M. (1975). *Discipline and Punish: The Birth of the Prison*. Gallimard. [English trans. 1977, Pantheon.]

Foucault, M. (1980). *Power/Knowledge: Selected Interviews and Other Writings, 1972–1977*. Ed. C. Gordon. Pantheon.

Gibson, J. J. (1979). *The Ecological Approach to Visual Perception*. Houghton Mifflin.

Sextus Empiricus. *Outlines of Pyrrhonism*. [Trans. R. G. Bury, Loeb Classical Library, 1933.]


## Changelog entry for v6.10

**What changed in v6.10 — Related Work expansion:**

1. **§7 Related Work:** Added six entries positioning the framework relative to
   literatures identified by external review as missing or under-discussed:
   (a) Standpoint epistemology (Harding, Haraway — already cited, now discussed:
   deep fracture regime as presheaf-level standpoint claim, δ-band concentration
   at U₁); (b) Foucault (power-conditioned visibility as informal thesis
   formalized by Axiom 2 + Theorem 1; institutional sign-flip as presheaf
   analogue of disciplinary optics; disanalogy: genealogical vs. axiomatic method,
   ε-invariance vs. anti-realism); (c) Gibson (observer-relative objectivity
   shared, but cover story mechanism denies perceptual veridicality); (d) Bourdieu
   (δ as thin formal cousin of habitus; structured disposition vs. random noise;
   disanalogy: no historical genesis, no field dynamics); (e) Arrow/Sen (already
   cited, now discussed: H¹ gap as impossibility result for classification
   aggregation); (f) Pyrrhonism (grounding the "Pyrrhonist commitment" phrase:
   selective non-closure applied to adjudication not structure, bounded by
   scope-limit finding).

2. **References:** Added Bourdieu (1990), Foucault (1975, 1980), Gibson (1979),
   Sextus Empiricus (*Outlines of Pyrrhonism*).

---

## Notes on what was considered and excluded

- **Adorno (non-identity):** Considered (Gemini flagged as essential). Excluded
  because DR does not reject totality — it measures specific obstructions to
  totality and quantifies their structure. Negative dialectics refuses to
  formalize the residue; DR's H¹ is precisely a formalization of the residue.
  Citing Adorno would invite a reading that the framework considers
  sheafification *wrong in principle* rather than *destructive of diagnostic
  signal*.

- **Kuhn (incommensurability):** Considered (ChatGPT flagged). Excluded because
  Kuhnian incommensurability is about temporal paradigm succession; DR models
  simultaneous coexistence across positions. The parallel would invite exactly
  the wrong reading.

- **Fricker (epistemic injustice):** Considered (ChatGPT flagged). Excluded
  because Harding/Haraway already cover the standpoint angle and are already
  cited. Fricker's specific contribution (credibility deficit tied to social
  identity) maps to Axiom 2 but would open a subfield discussion without adding
  structural precision the paper needs at this stage.

- **Cybernetics (Wiener beyond Ashby):** Considered (all four assessments rated
  useful or optional, none essential). Excluded from the paper because DR is not
  a feedback-control framework; the existing Ashby reference is sufficient.

- **Latour (ANT):** Flagged as high-risk false parallel by ChatGPT. ANT
  collapses structure into networks of associations; DR requires stable
  structural properties (ε-invariance). Correctly excluded.

- **Paraconsistent logic (Priest, da Costa):** Considered (Perplexity flagged).
  Excluded because the analogy (local inconsistency tracked without forcing
  global resolution) is real but DR is not a non-classical logic and citing it
  would invite demands about proof theory the paper cannot satisfy.

- **Contextualist epistemology (DeRose, Lewis):** Considered (Perplexity
  flagged). The Abramsky-Brandenburger entry already covers the formal
  contextualist angle; adding DeRose/Lewis would duplicate without adding
  structural precision.

- **Measurement theory (Krantz, Luce, Suppes, Tversky):** Considered
  (Perplexity flagged as nice-to-have). Real parallel to the STRICT/STRUCTURAL
  distinction. Deferred — could be added in a future version if the paper
  develops its representation-theorem claims more explicitly.
