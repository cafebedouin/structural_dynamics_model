# Deferential Realism: v3 → v4 Transition Document

## Purpose

This document assembles the theoretical developments, computational audit findings, and proposed revisions that constitute the v3 → v4 transition. It originated in a pen-and-paper exercise comparing the Avellaneda-Stoikov reservation price formula to the DR sigmoid, which revealed that Boltzmann factorizability *is* the functor axiom for the presheaf's restriction maps. That discovery prompted a systematic search for other external frameworks whose known formal properties map onto existing DR machinery, which in turn motivated a computational audit to verify whether the theoretical predictions hold empirically.

The document is organized as: (1) the theoretical developments that motivated the audit, (2) the audit findings, (3) the specific v3 text changes implied, and (4) forward-looking extensions that belong in future work rather than v4.

Items marked **[CORPUS-DEPENDENT]** will need recomputation on the reconciled v4 corpus. Items marked **[CORPUS-INDEPENDENT]** are framework constants or theoretical results that hold regardless of corpus state.

---

## Part I: Theoretical Developments

### 1. Boltzmann Factorizability = Functor Axiom (Closes Open Item 1)

**[CORPUS-INDEPENDENT]**

**Origin.** The Avellaneda-Stoikov reservation price at inventory q is r(q) = s − qκ, where the observer-dependent part (−qκ) is linear in the inventory position q. Modeled as a presheaf on a linear poset of inventory levels, the restriction maps compose by additive telescoping: Δ(q₁→q₂) + Δ(q₂→q₃) = Δ(q₁→q₃). This works because the factorization is additive: observer-independent + observer-dependent.

**The DR analogue.** Hub 1's experienced extractiveness is χ(P) = ε · σ(π(P)) · σ(S), where ε is observer-independent and σ(π(P)) · σ(S) is observer-dependent. The restriction map from context Pⱼ to Pᵢ is the ratio:

$$\rho(P_j \to P_i): \chi \mapsto \chi \cdot \frac{\sigma(\pi(P_i))}{\sigma(\pi(P_j))}$$

Composition check:

$$\rho(P_2 \to P_3) \circ \rho(P_1 \to P_2)(\chi_1) = \chi_1 \cdot \frac{\sigma(\pi(P_2))}{\sigma(\pi(P_1))} \cdot \frac{\sigma(\pi(P_3))}{\sigma(\pi(P_2))} = \chi_1 \cdot \frac{\sigma(\pi(P_3))}{\sigma(\pi(P_1))} = \rho(P_1 \to P_3)(\chi_1) \quad \checkmark$$

The σ(π(P₂)) terms cancel telescopically. This works because χ factors *multiplicatively* as (observer-independent) × (observer-dependent), and restriction maps are determined by the *ratio* of observer-dependent parts. Ratios compose telescopically: (a/b)(b/c) = a/c regardless of whether a, b, c are linear, sigmoidal, or anything else.

**The punchline.** Boltzmann factorizability — the test that χ separates into observer-independent and observer-dependent factors — *is* the functor axiom for Hub 1's restriction maps. The Boltzmann compliance test already running in the pipeline is doing the formal verification listed as v3 open item 1, just without the categorical label.

Specifically:
- Boltzmann-compliant constraints → multiplicatively separable χ → restriction maps compose → F is a well-defined functor → presheaf structure is STRICT
- Boltzmann-non-compliant constraints → coupled dimensions → restriction maps may not compose → presheaf structure is approximate

**The scope modifier closure.** The v4 notes initially flagged a remaining gap: whether σ(S) introduces coupling between the power axis and a scope axis. The chi variance decomposition resolves this: σ(S) is assigned per-perspective as a structural constant (powerless → local = 0.8, analytical → global = 1.2), independent of constraint content. So σ(S) = σ(S(P)), which folds into the observer-dependent factor: χ(P) = ε · [f(d(P)) · σ(S(P))]. No coupling. The factorization holds.

**The 25 chi overrides.** The audit identified 25 constraints where χ_actual ≠ ε · f(d) · σ(S), all violated at the powerless context, all with large errors (0.27–0.57). These are intentional overrides where the sigmoid's generic power-scaling fails to capture specific vulnerability — the framework assigns experienced extractiveness based on structural position rather than the formula. For these 25, the restriction maps don't compose cleanly, and the presheaf structure is approximate. This is a well-characterized population (2.2% of corpus) mapping to the same category as Boltzmann-non-compliant constraints.

**Comparison: A-S additive vs DR multiplicative.** A-S factorizes additively (ℝ acting on valuations by translation); DR factorizes multiplicatively (ℝ₊ acting on extractiveness by scaling). The multiplicative version guarantees χ ≥ 0 by construction (sigmoid outputs are positive and ε ≥ 0), whereas the additive version allows negative valuations.

**Hub 2.** The 18-row lookup table with discrete outputs satisfies the functor axioms trivially (singleton stalks, unique maps). The interesting property of Hub 2 is independence from Hub 1, already verified empirically (zero Type A conflicts).

**Assessment.** Open item 1 from v2/v3 can be closed. The argument chain is: Boltzmann compliance test → multiplicative separability → telescopic composition of restriction maps → functor axioms satisfied. The chi variance decomposition provides the σ(S) independence evidence. The 25 chi overrides are the documented exceptions. **[STRICT]**


### 2. Markov Category as the Correct Abstraction for MaxEnt

**[CORPUS-INDEPENDENT]**

**The problem.** v3 §5.2 describes the MaxEnt classifier as "structurally analogous to the Giry monad's action" but notes that "the full Giry monad structure — unit (Dirac embedding), multiplication (distribution over distributions), naturality of both — is not present." This framing presents the MaxEnt layer as incomplete.

**The reframing.** The Giry monad requires a multiplication map μ: G(G(Ω)) → G(Ω) — a distribution over distributions, collapsed to a single distribution. Constructing μ in DR would require a *prior over observer positions*, which is precisely what indexical relativity refuses to posit. Putting a probability measure over {powerful, powerless, analytical, neutral} would collapse the perspectival structure into a single "expected" distribution, destroying the diagnostic signal.

The correct abstraction is Fritz's (2020) Markov category framework, which requires:
- A symmetric monoidal category with copy-delete structure
- Stochastic morphisms (Markov kernels)
- **No multiplication map**, no distribution-over-distributions

What DR has:
- Deterministic classifier = deterministic subcategory (definite type assignments compose via the presheaf functor)
- MaxEnt layer = stochastic morphisms (probability distributions at each context)
- Copy: a definite classification feeds multiple independent diagnostic subsystems (gauge orbits, MaxEnt, abductive engine)
- Delete: forgetting classification at one context is well-defined

What must be verified for STRICT status:
1. Deterministic sub-category exists — definite type assignments compose. ✓ (presheaf functor)
2. Stochastic morphisms compose with deterministic ones — feeding a definite type into MaxEnt likelihoods produces a well-defined distribution. ✓ (Dirac input to Gaussians)
3. Marginalization (delete) is natural — forgetting a component commutes with restriction maps. Requires explicit verification: does marginalizing over types at one context commute with the restriction map to another context?
4. Positivity — all stochastic morphisms assign positive probability to all outcomes. ✓ (Gaussian likelihoods with log-sum-exp; every type gets positive probability)

**The upgrade.** "Incomplete Giry monad [STRUCTURAL]" becomes "Complete Markov category [STRICT, pending naturality of delete]." The Giry monad is too strong — it requires observer-position-averaging that the framework correctly refuses. The Markov category is the right abstraction for indexical systems.

**References.** Fritz, T. (2020). A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics. *Advances in Mathematics*, 370, 107239.


### 3. Information Geometry of T13

**[CORPUS-INDEPENDENT for the identity; CORPUS-DEPENDENT for empirical validation]**

The MaxEnt distributions live on the probability simplex (Δ⁵, g_Fisher), where g_Fisher is the Fisher information metric. The divergence measures used in T13 have precise geometric meanings:

- **KL divergence** (what T13 computes as L∞, not KL — see audit correction below): asymmetric, directional
- **Fisher-Rao geodesic distance**: d_FR(p,q) = 2 arccos(Σᵢ √(pᵢqᵢ)) — symmetric, reparametrization-invariant
- **Hellinger distance**: H²(p,q) = Σᵢ (√pᵢ - √qᵢ)² — decomposes additively by type

The second-order identity KL(p‖q) ≈ ½ d_FR(p,q)² + O(δ³) is textbook (Amari, *Methods of Information Geometry*, Chapter 3; Čencov's uniqueness theorem for the Fisher metric).

The Hellinger decomposition is the key practical consequence: for each T13-firing constraint, Hᵢ²/H²_total gives a distribution over types showing *where the divergence lives*. This is finer-grained than the scalar divergence — "68% along the snare-rope axis" versus "divergence = 0.07."

**Note on T13's actual measure.** T13 uses L∞ (max absolute probability difference across types), not KL divergence. The geodesic ball interpretation applies to KL; the L∞ threshold defines a polytope (L∞ ball) on the simplex, not a geodesic ball. The information-geometric reframing is therefore STRUCTURAL rather than STRICT for the threshold interpretation, though the Hellinger decomposition applies regardless of which divergence measure triggers the fire.


### 4. Sheaf Laplacian and the Tree Correction

**[CORPUS-INDEPENDENT for the construction; CORPUS-DEPENDENT for the empirical results]**

**The construction.** The Hansen-Ghrist sheaf Laplacian on the path graph P₄ with scalar χ stalks and multiplicative restriction maps produces the 4×4 tridiagonal matrix:

$$L_0 = \begin{pmatrix} 1 & -r_{12} & 0 & 0 \\ -r_{12} & r_{12}^2+1 & -r_{23} & 0 \\ 0 & -r_{23} & r_{23}^2+1 & -r_{34} \\ 0 & 0 & -r_{34} & r_{34}^2 \end{pmatrix}$$

where r₁₂ ≈ 1.42, r₂₃ ≈ -8.38, r₃₄ ≈ -0.10.

**The tree correction.** The site P₄ is a tree — no cycles. The "detailed balance / entropy production" framing initially proposed for the Laplacian energy E(C) = ‖δ₀x‖² is vacuously true on a tree because all entropy production on trees is gradient flow (no cycle flows). The correct physical interpretation is: E(C) measures how far the constraint is from the nearest global section, and the spectral gap governs how fast perturbations decay under diffusion on the site. This is Fokker-Planck, not Onsager.

The spectral gap is still the inverse relaxation time, but it's the relaxation rate of a *diffusion* process, not an equilibrium fluctuation. "Diffusion toward consensus" is the right narrative for a framework about observer disagreement.

**Audit finding: institutional dominance.** The signed restriction map r₂₃ ≈ -8.38 produces r₂₃² ≈ 70, which dominates the Laplacian spectrum. The eigenvalues are {0, 0.015, 2.995, 72.184}. Mode 4 (λ = 72.18) loads almost entirely on the institutional component (eigenvector weight 0.993). The moderate→institutional boundary carries 97% of the spectral weight.

Consequence: E(C) correlates with H¹ at Spearman ρ = 0.307 corpus-wide but drops to ρ = 0.09 within the dominant d-pattern group. The scalar Laplacian adds a continuous energy measure but does not provide independent structural information beyond H¹ for the current site geometry. The vector-valued upgrade (MaxEnt distributions as stalks) would avoid the single-ratio dominance problem.


### 5. Per-Context Oracle Gap

**[CORPUS-DEPENDENT]**

**The original claim.** v3 §4.3 states that MaxEnt detects ~1% of observer-dependence that cohomological analysis detects — "the 100x classical oracle gap."

**The corrected finding.** The audit revealed that T13's diagnostic power varies dramatically by observer position:

| Context | T13 Fires | % of H¹>0 | Mean χ/ε | Mechanism |
|---------|-----------|-----------|----------|-----------|
| Institutional | 864 | 97.2% | -0.04 | Sign flip — no profile recalibration can normalize |
| Powerless | 539 | 60.6% | 1.08 | Near identity — slight divergence detectable |
| Moderate | 544 | 61.2% | 1.10 | Near identity — slight divergence detectable |
| Analytical | 3 | 0.3% | 1.37 | Smooth scaling — profiles absorb the shift |

The oracle gap is a function of observer position. At analytical context (where Prolog runs T13), χ ≈ 1.37ε. When indexed profiles are recalibrated to chi values, the Gaussian likelihoods shift right by 37% but preserve their shape — the relative ordering of type probabilities barely changes. Mean corrected TVD at analytical is 0.0006 (effectively zero).

At institutional context (χ/ε = -0.04, sign flip), the sigmoid maps positive extraction to negative experienced extractiveness. No profile recalibration can absorb a sign change — distributions for "negative extraction" look nothing like distributions for "positive extraction." MaxEnt detects 97% of observer-dependence at this context.

**The reframing.** The oracle gap measures how far the observer shift is from a simple rescaling that profile recalibration can absorb. The framework runs T13 at the analytical context — the worst case for MaxEnt detection — making the oracle gap a *conservative bound*. The Prolog design is not a bug; it tests MaxEnt at the context where MaxEnt is weakest, establishing that cohomological analysis adds the most value precisely where probabilistic methods fail.

**The T13 population.** Under the Prolog-faithful criterion, T13 fires on 3 constraints (v3 corpus), all sharing the false_ci_rope conditional signature override. The paper's ~11 reflects the v2 corpus. The reduction from ~11 to 3 is corpus drift (fewer false_ci_rope constraints near classification boundaries in v3). The mechanism is precise: the false_ci_rope conditional override applies a 3× boost to tangled_rope probability, amplifying small pre-override divergences above the 0.05 threshold.

**The 100x gap restated.** T16 catches what T13 misses because categorical thresholds don't recalibrate. A constraint can change type under observer shift (rope → snare) without producing measurable probabilistic divergence at the analytical context, because the profile shift makes the new type probabilities look "normal" for the new chi value. The gap is between absolute divergence (does the distribution change?) and relative divergence (does the distribution look unusual for its chi value?). The Prolog T13 measures the latter. Cohomology measures the former.


### 6. Hellinger Decomposition by Type

**[CORPUS-DEPENDENT — the mean fractions; CORPUS-INDEPENDENT — the decomposition method]**

The Hellinger distance decomposes additively: H² = Σᵢ Hᵢ² where Hᵢ² = (√pᵢ^cl − √pᵢ^idx)². The ratio Hᵢ²/H² gives a distribution over types showing where observer shift moves probability mass.

**Corpus-level finding (inflated T13, all 4 contexts, pre-reconciliation corpus):**

| Type | Mean Fraction | Interpretation |
|------|---------------|----------------|
| tangled_rope | 44% | Hub 1 primary: sigmoid moves mass along the rope spectrum |
| snare | 32% | Hub 1 secondary: extraction boundary |
| rope | 17% | Hub 1 tertiary: low-extraction end |
| piton | 5% | Theater-driven minority |
| scaffold | 3% | Rare |
| mountain | 0.1% | Confirms mountain stability |

The dominant divergence axis is snare ↔ tangled_rope, which is exactly what Hub 1 predicts: the sigmoid suppresses experienced extraction at higher power levels, moving constraints from "looks like a snare" to "looks like a tangled_rope/rope." The near-zero mountain contribution confirms mountain stability — observer shift doesn't move probability mass into or out of the mountain basin.


### 7. Gate-Space Architecture

**[CORPUS-DEPENDENT]**

**FCA finding.** The binary gate matrix (1150 × 33 extractable gates) has GF(2) rank 30/33 — only 3 gates are linearly redundant. The concept lattice contains 1,865 formal concepts (far fewer than 2³³ but larger than expected for aggressive compression).

**The structural finding.** The type-separation table reveals that snare vs. tangled_rope has *zero* perfect gate separators. No single gate or gate combination in the extracted set cleanly distinguishes snares from tangled ropes. This confirms the two-hub architecture from a new angle: the snare/tangled_rope distinction is driven by continuous χ thresholds (Hub 1), not binary structural features. The gate space captures Hub 2's contribution (mountain separation: 7 perfect separators) but is structurally blind to Hub 1's continuous gradation.

**Limitation.** Only 33 of ~65 gates are extractable from the JSON export. The missing ~32 are primarily Hub 2 features (immutability, temporality checks) computed inside Prolog modules. Full FCA requires Prolog-side extraction.


---

## Part II: v3 Section-by-Section Revision Map

### §2.7 (Two-Hub Architecture) — Add functor axiom equivalence

**Insert after the current Hub 1 description (line ~185):**

> The multiplicative factorization χ(P) = ε · [f(d(P)) · σ(S(P))] is exactly the condition that guarantees the restriction maps compose: the ratio σ(π(Pᵢ))/σ(π(Pⱼ)) cancels telescopically under composition, satisfying the functor axiom. Boltzmann factorizability — already tested by the Boltzmann compliance module — is therefore equivalent to the presheaf's functor axiom for Hub 1 [STRICT]. The 25 chi-override constraints (2.2% of corpus [COMPUTE on v4 corpus]) where χ_actual ≠ ε · f(d) · σ(S) are the exact population where the functor axiom fails and the presheaf structure is approximate. All 25 are overridden at the powerless context, where the sigmoid's generic power-scaling fails to capture specific vulnerability [CORPUS-DEPENDENT — recount on v4].


### §3.2 (MaxEnt Shadow Classification) — Add Hellinger decomposition and information-geometric note

**Insert after the current "Indexed MaxEnt variant" paragraph (line ~269):**

> **Information-geometric interpretation.** The MaxEnt distributions at each context live on the probability simplex (Δ⁵, g_Fisher). The L∞ divergence used by T13 defines a polytope threshold on this simplex. The Hellinger distance H²(p_cl, p_idx) = Σᵢ (√pᵢ^cl − √pᵢ^idx)² provides a reparametrization-invariant alternative that decomposes additively by type: the ratio Hᵢ²/H² identifies which types carry the divergence. Corpus-level decomposition shows that observer shift moves probability mass primarily along the snare↔tangled_rope axis (tangled_rope: [COMPUTE]%, snare: [COMPUTE]%), confirming that Hub 1's sigmoid is the primary mechanism. Mountain contributes < 0.2% of Hellinger divergence, confirming mountain stability from a new angle [STRUCTURAL — the decomposition is a standard construction; the corpus fractions are empirical].


### §4.3 (100x Oracle Gap) — Reframe as per-context

**Replace the current oracle gap paragraph (lines ~507–511) with:**

> **The classical oracle gap.** The MaxEnt classifier's diagnostic power varies dramatically by observer context. At the analytical context (χ/ε ≈ 1.37), where the framework runs T13, only [COMPUTE] constraints fire — [COMPUTE]% of H¹ > 0. The smooth 1.37× scaling is absorbed by profile recalibration: the indexed profiles shift right by 37%, preserving the relative shape of the Gaussian likelihoods. At the institutional context (χ/ε ≈ -0.04), T13 fires on [COMPUTE] constraints — [COMPUTE]% of H¹ > 0 — because the sign flip cannot be absorbed by any profile shift. The oracle gap is a function of observer position, largest where the observer shift is a smooth rescaling (analytical) and smallest where it is a qualitative transformation (institutional sign flip). The framework measures the gap at its worst case, making the claim conservative: cohomological analysis adds the most diagnostic value precisely where MaxEnt is weakest.
>
> The mechanism is precise: most observer-dependence produces categorical classification shifts (rope → snare) without producing probabilistic shifts that survive profile recalibration at the analytical context. The indexed profiles see the new chi value as statistically normal for its type group. T16 catches what T13 misses because categorical thresholds don't recalibrate [STRUCTURAL — the per-context pattern is corpus-specific; the mechanism is architectural].


### §5.1 (What Is STRICT) — Add functor axiom closure, update T13

**Add to the STRICT list:**

> - **Boltzmann factorizability = functor axiom.** Multiplicative separability of χ into observer-independent × observer-dependent factors is exactly the condition that restriction maps compose telescopically. The Boltzmann compliance test verifies the functor axiom. The 25 chi-override constraints are the documented exceptions [CORPUS-DEPENDENT — recount on v4].

**Modify the T13 entry:**

> - **T13 divergence measurement.** The indexed-vs-classical MaxEnt L∞ divergence is a direct numerical measurement. The Hellinger decomposition Hᵢ²/H² identifies which types carry the divergence. Both are direct computations, not inferences.


### §5.2 (What Is STRUCTURAL) — Upgrade MaxEnt from Giry to Markov category

**Replace the current MaxEnt bullet (line ~569):**

> - **MaxEnt as Markov category.** The MaxEnt classifier assigns a probability distribution over the type space at each context, forming stochastic morphisms in a Markov category (Fritz 2020). The deterministic classifier embeds as the deterministic subcategory; copy (feeding a classification to multiple diagnostic subsystems) and delete (forgetting classification at one context) are well-defined. The Giry monad's multiplication map μ — requiring a distribution over distributions — is absent because constructing it would require a prior over observer positions, which indexical relativity refuses to posit. The Markov category is the correct abstraction: it captures pointwise probability assignment without requiring observer-position averaging. Pending verification: naturality of the delete map (whether marginalization commutes with restriction maps). If verified, this upgrades to STRICT.

**Modify the 100x oracle gap bullet (line ~581):**

> - **100x oracle gap.** The gap is observer-position-dependent: [COMPUTE]% at analytical, [COMPUTE]% at institutional. The ratio varies by corpus; the mechanism (profile recalibration absorbs smooth observer shifts, fails on qualitative transformations) is architectural.


### §5.5 (What Would Strengthen) — Close item 1, update item 4

**Replace item 1:**

> 1. **Formal verification of restriction maps — CLOSED.** The power-scaling function's multiplicative factorization χ = ε · [f(d) · σ(S)] guarantees telescopic composition of restriction maps, satisfying the functor axiom. The Boltzmann compliance test is the formal verification. The 25 chi-override constraints (2.2%) are the documented boundary where the axiom fails. See §2.7. *(Closed in v4.)*

**Update item 4 to reference bigraded cohomology:**

> 4. **Extension to enriched sites.** Adding temporal morphisms would create a product site P₄ × T with bigraded cohomology H^{p,q}. The Künneth decomposition yields: H^{0,0} (observer-independent, temporally stable), H^{1,0} (current H¹ — spatial observer-dependence), H^{0,1} (observer-independent temporal drift), and H^{1,1} (emergent indexicality — constraints whose observer-dependence itself changes over time). H^{1,1} detects a genuinely new phenomenon: a constraint that was observer-independent in one era but observer-dependent in another. Legal corpora with temporal stratification (e.g., antitrust enforcement 1890–2024) are natural candidates. *(v2 item, expanded in v4.)*


### §6 (Related Work) — Add references

**Add after the quantum complexity theory paragraph:**

> **Information geometry** (Amari and Nagaoka 2000; Čencov 1982) provides the natural geometric framework for the probability simplex on which MaxEnt distributions live. The Hellinger distance decomposition identifies which types carry the divergence under observer shift, extending the scalar T13 diagnostic to a per-type profile. The Fisher-Rao geodesic distance provides a reparametrization-invariant and symmetric alternative to the directional KL divergence.
>
> **Markov categories** (Fritz 2020) provide the correct categorical abstraction for systems with probabilistic morphisms that lack a distribution-over-distributions structure. DR's MaxEnt layer forms a Markov category where the deterministic classifier embeds as the subcategory of deterministic morphisms — capturing compositionality without requiring the Giry monad's multiplication map, which would demand a prior over observer positions.
>
> **Sheaf Laplacians** (Hansen and Ghrist 2019) extend spectral graph theory to cellular sheaves, providing continuous obstruction measures and eigenvalue decompositions. Applied to DR's path-graph site with scalar χ stalks, the sheaf Laplacian confirms the institutional phase transition as the dominant spectral feature (r₂₃² ≈ 70, carrying 97% of spectral weight) but does not add independent structural information beyond H¹ for the scalar case.

**New references:**

> Amari, S., & Nagaoka, H. (2000). *Methods of Information Geometry*. Translations of Mathematical Monographs, vol. 191. American Mathematical Society.
>
> Čencov, N. N. (1982). *Statistical Decision Rules and Optimal Inference*. Translations of Mathematical Monographs, vol. 53. American Mathematical Society.
>
> Fritz, T. (2020). A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics. *Advances in Mathematics*, 370, 107239.
>
> Hansen, J., & Ghrist, R. (2019). Toward a spectral theory of cellular sheaves. *Journal of Applied and Computational Topology*, 3, 315–358.


### §7 (Conclusion) — Update oracle gap language

**Replace** "A maximally capable within-context classifier detects only ~1% of the observer-dependence that cross-context analysis reveals" **with:**

> The classical oracle gap — MaxEnt's failure to detect observer-dependence — is observer-position-dependent, ranging from 0.3% detection at the analytical context to 97% at the institutional context. The framework measures the gap at its worst case: the analytical context, where the smooth 1.37× observer scaling is absorbed by profile recalibration. Cohomological analysis adds the most diagnostic value precisely where MaxEnt is weakest.


---

## Part III: H¹=6 and Other Minor Findings

### H¹=6 Constraints

All 5 H¹=6 constraints are structurally identical: ε = 0.55, d-pattern = (0.9, 0.7, 0.12, 0.72), false_ci_rope signature, E(C) = 0.5082. H¹=6 in this corpus is a single structural archetype, not a diverse population. Worth a footnote in §4.2. **[CORPUS-DEPENDENT — check if v4 corpus changes this.]**

### Chi Validation Count

The audit found 25 chi-override constraints; v3 references 19. The 6 additional overrides were likely added during corpus expansion from v2 to v3. v4 should use the audited count. **[CORPUS-DEPENDENT — recount on v4 corpus.]**

### Scalar Laplacian Assessment

The scalar sheaf Laplacian on P₄ with the current sigmoid parameters is dominated by the institutional edge (r₂₃² ≈ 70). It confirms the institutional phase transition as the primary architectural feature and provides a continuous energy measure E(C), but E(C) vs H¹ correlation within d-pattern groups is weak (ρ ≈ 0.09). The construction is mathematically correct and STRICT, but does not currently warrant a dedicated paper section. If the vector-valued upgrade (MaxEnt distributions as 8-dimensional stalks) is implemented and shows better hub decomposition, it would merit a section. **[CORPUS-INDEPENDENT for the construction; CORPUS-DEPENDENT for the correlations.]**

---

## Part IV: Forward-Looking Extensions (Future Work, Not v4)

### Bigraded Cohomology (H^{1,1})

With temporal data, the presheaf on P₄ × T yields bigraded cohomology. H^{1,1} detects emergent indexicality — constraints whose observer-dependence changes over time. A law that was observer-independent in 1890 but observer-dependent in 2024 has a nonzero H^{1,1} class. Data requirements: a stratified legal corpus sampled at temporal intervals, with observer positions held constant. This is a second paper, not a v4 addition.

### Vector-Valued Sheaf Laplacian

Replacing scalar χ stalks with MaxEnt distribution vectors (Δ⁷ ⊂ ℝ⁸) yields a 32×32 Laplacian that captures both Hub 1 (continuous χ shifts as probability vector changes) and Hub 2 (immutability flips as discrete vector jumps). This avoids the single-ratio dominance problem because the restriction maps act on 8-dimensional vectors, distributing spectral weight more evenly. Implementation requires defining the linear maps T_{ij}: ℝ⁸ → ℝ⁸ on each edge, either empirically (best-fit transformation across corpus) or structurally (diagonal from Hub 1 ratios). Not needed for v4 but is the natural next step for spectral analysis.

### Enriched Site with Cycles

Adding a direct powerless→analytical edge would create a cycle in the site, enabling nontrivial cycle-based entropy production (genuine detailed balance, not the vacuous tree version). The loop cost around powerless→moderate→institutional→analytical→powerless would test whether the "shortcut" from powerless to analytical agrees with the "long way around." This is site design work, not corpus work.

### Galois Connection on Full Gate Space

Full FCA on all ~65 gates (requiring Prolog-side extraction) would determine whether the gate space compresses significantly and identify the essential discriminating set. The 33-gate subset already shows the key qualitative finding (snare/tangled_rope inseparability), but the quantitative compression ratio requires the full matrix.

### Financial Regulation Beta Corpus

An A-S-informed financial regulation corpus (position limits, capital requirements, circuit breakers) with observer positions mapped to market maker / retail trader / regulator / HFT firm would test framework portability to a domain with quantitative ground truth. This is the beta corpus question from the v4 notes — strongest after the v4 corpus rebuild is complete.

---

## Appendix: Audit Data Sources

All audit computations were performed on the pre-reconciliation v3 corpus (1,150 constraints). Numbers marked [CORPUS-DEPENDENT] will shift when recomputed on the reconciled v4 corpus. Framework constants and theoretical results are stable.

**Audit output files** (in `audit/outputs/`):
- `spectral_audit_report.md` — full report with Sections 1–8
- `constraint_energy.csv` — per-constraint Laplacian energies
- `divergence_measures.csv` — per-constraint divergence measures (4 contexts)
- `t13_reconciliation_corrected_t13.csv` — corrected T13 with Prolog-faithful criterion
- `t13_reconciliation_populations.csv` — Group A/B/C characterization
- `followup_per_context_summary.csv` — per-context T13 summary
- `followup_chi_violations.csv` — 25 chi-override constraints
- `followup_h1_6_constraints.csv` — 5 H¹=6 constraints
- `gate_matrix.csv` — 33-gate binary matrix
- `hellinger_decomposition.csv` — per-type Hellinger fractions
