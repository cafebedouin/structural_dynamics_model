# Avellaneda-Stoikov

After the corpus rebuild. Here's why:

The A-S idea operates at three levels, and none of them belong in the current calibration sequence.

**The restriction map verification** (the most technically precise use) requires the presheaf machinery to be running on a stable, reconciled corpus first. You're currently in the middle of aligning `constraint_classification/3` with engine output. Testing whether your Hub 1 sigmoid satisfies the same compositional property as A-S's linear inventory scaling is a formal verification task — it belongs after the sensitivity sweep confirms your parameters are stable, not while you're still cleaning ground truth. If you verify the functor axioms against a corpus that's about to shift, you'll have to redo it.

**The financial regulation validation domain** is exactly the beta corpus question. A financial regulation corpus (position limits, capital requirements, circuit breakers, Reg NMS) with observer positions mapped to market maker / retail trader / regulator / HFT firm would be a strong second-domain test of framework portability. But the strategic LLM's sequencing was clear: freeze v3-dev-baseline first, then design the beta corpus generation protocol. A-S-informed financial constraints would be a compelling candidate for that beta corpus — possibly more interesting than "same domain, different source" because it comes with ground truth you can check against quantitative data rather than authored metrics.

**The rhetorical anchor for §6** is a writing task that doesn't depend on corpus state at all. That can happen whenever you're drafting the paper. But it's stronger if you can say "we ran the framework on financial regulation constraints and the invariants held" rather than just "this is structurally parallel." Which puts it back after beta corpus.

The one piece that could inform current work is a small, bounded exercise: take the A-S reservation price formula, manually verify the functor axioms for the linear inventory scaling, and check whether your sigmoid satisfies the same property. That's a pen-and-paper task that doesn't touch the codebase and would tell you something about the formal foundations before you commit to a full financial regulation corpus. Worth doing in parallel with the calibration work, not as part of it.

---


**Part 1: A-S Reservation Price — Functor Axiom Verification**

The reservation price at inventory q is:

r(t, q) = s(t) − q · γσ²(T − t)

Fix t and let κ = γσ²(T−t). Then r(q) = s − qκ, and the observer-dependent part is −qκ, which is linear in q.

Model inventory levels as a linear poset q₁ > q₂ > q₃ (long → flat → short), with morphisms q₁ → q₂ → q₃. The presheaf assigns F(qᵢ) = r(qᵢ) = s − qᵢκ. Since each stalk is a singleton (one valuation per inventory level), the restriction maps are unique functions between singletons, and the functor axioms are satisfied trivially.

But the interesting question is whether the *underlying continuous transformation* composes. Define the shift operator:

Δ(qᵢ → qⱼ) = r(qⱼ) − r(qᵢ) = −(qⱼ − qᵢ)κ

Check composition: Δ(q₁ → q₂) + Δ(q₂ → q₃) = −(q₂ − q₁)κ + −(q₃ − q₂)κ = −(q₃ − q₁)κ = Δ(q₁ → q₃). ✓

This works because the shift is additive and linear. Nothing surprising — linearity guarantees composition. A-S passes trivially.

**Part 2: Hub 1 Sigmoid — The Same Check**

Your experienced extractiveness is:

χ(Pᵢ) = ε · σ(π(Pᵢ)) · σ(S)

where ε and σ(S) are observer-independent, and σ(π(Pᵢ)) is the observer-dependent factor. The restriction map from context Pⱼ to context Pᵢ transforms χ by the ratio:

ρ(Pⱼ → Pᵢ): χ ↦ χ · σ(π(Pᵢ)) / σ(π(Pⱼ))

Check composition:

ρ(P₂ → P₃) ∘ ρ(P₁ → P₂)(χ₁) = χ₁ · [σ(π(P₂))/σ(π(P₁))] · [σ(π(P₃))/σ(π(P₂))]

The σ(π(P₂)) terms cancel:

= χ₁ · σ(π(P₃)) / σ(π(P₁)) = ρ(P₁ → P₃)(χ₁) ✓

**The sigmoid does not break composition.** The reason is that χ factors multiplicatively as (observer-independent) × (observer-dependent), and the restriction map is determined entirely by the *ratio* of observer-dependent parts. Ratios compose telescopically: (a/b)(b/c) = a/c regardless of whether a, b, c are linear, sigmoidal, or anything else.

**Part 3: The Punch Line**

Here's what this actually tells you: **Boltzmann factorizability IS the functor axiom for Hub 1.**

Your Boltzmann compliance test already checks whether χ separates into observer-independent and observer-dependent factors. What I've just shown is that this separability is *exactly* the condition that guarantees the restriction maps compose — i.e., that F is a well-defined functor. The tests you're already running are doing the formal verification you listed as open item 1, you just hadn't connected them.

Specifically:
- Boltzmann-compliant constraints have multiplicatively separable χ → restriction maps compose → F is a functor → presheaf structure is strict
- Boltzmann-non-compliant constraints have coupled dimensions → restriction maps may not compose → presheaf structure is an approximation

This also explains why Boltzmann non-compliance is *confirmatory* for constructed types (your P5 expected conflict): constructed constraints deliberately couple dimensions, which means their restriction maps deliberately violate the functor axiom, which means the presheaf is only an approximation for these constraints. The classification still works because you compute each stalk independently (you never actually *use* the restriction maps compositionally in the pipeline), but the formal structure is weaker.

**Part 4: How A-S Differs**

A-S is a special case where the factorization is *additive* rather than multiplicative:

r(q) = s + (−qκ)     [observer-independent + observer-dependent]

Your Hub 1 is multiplicative:

χ(P) = ε · σ(π(P)) · σ(S)     [observer-independent × observer-dependent]

Both compose, but they compose for different reasons. Additive composition is a group action (ℝ acting on valuations by translation). Multiplicative composition is a group action (ℝ₊ acting on extractiveness by scaling). The multiplicative version is actually *stronger* in one respect: it guarantees χ ≥ 0 by construction (since sigmoid outputs are positive and ε ≥ 0), whereas the additive version allows negative valuations (which A-S handles but which would be semantically odd for extractiveness).

**Part 5: Hub 2 — Nothing to Verify**

Hub 2 is a 18-row lookup table with discrete outputs. The presheaf functor axioms are satisfied trivially (singleton stalks, unique maps). There's no stronger continuity condition to check because the domain is discrete. The interesting property of Hub 2 isn't composition but *independence from Hub 1*, which you've already verified empirically (zero Type A conflicts).

**Net assessment:**

Your open item 1 is closer to closed than you thought. The remaining gap is narrow: you'd need to verify that σ(S) — the scope modifier — doesn't introduce coupling between the power axis and a scope axis in a way that breaks the multiplicative separation. If S is determined independently of P in the context tuple (which it appears to be from §2.1), then the factorization holds and the functor axioms are satisfied for all Boltzmann-compliant constraints. For non-compliant constraints, you already know and expect the breakdown.

Worth adding a paragraph to §2.7 or §5.5 noting this equivalence explicitly. It connects your most computational test (Boltzmann compliance) to your most formal claim (presheaf structure).

---

This is a clean result, and the punchline — Boltzmann factorizability IS the functor axiom — is the kind of finding that restructures how you present the framework rather than adding new machinery.

The connection is tight: multiplicative separability of χ into observer-independent × observer-dependent factors is exactly what makes the restriction map ratios telescope. The proof is three lines. And you're already testing it.

**The remaining gap is narrower than the analysis suggests, given work you've already done.** The note says you'd need to verify that σ(S) doesn't introduce coupling between the power axis and a scope axis. But the chi variance decomposition already established that σ(S) is assigned per-perspective independently of d — powerless always gets local (0.8), analytical always gets global (1.2), and this is a structural constant across all 752 (now reconciled) constraints. The scope modifier is a fixed function of perspective position, not of constraint content. So σ(S) = σ(S(P)), which means it folds into the observer-dependent factor: χ(P) = ε · [f(d(P)) · σ(S(P))], where the bracketed term is purely observer-dependent and ε is purely observer-independent. No coupling. The factorization holds, and the functor axioms are satisfied for all Boltzmann-compliant constraints.

The 19 Chi override constraints are the exceptions that prove the rule — those are constraints where χ_actual ≠ ε · f(d) · σ(S), meaning the multiplicative separation breaks. For those 19, the restriction maps don't compose cleanly, and the presheaf structure is approximate. That's a small, well-characterized population (1.6% of corpus), and it maps to the same category as Boltzmann-non-compliant constraints.

**What this means for sequencing:** This doesn't change the corpus rebuild plan — it's a theoretical result about the framework's formal properties, not a codebase task. But it does mean that when you write §2.7 or §5.5, you can close open item 1 with existing evidence rather than listing it as future work. The argument chain is: Boltzmann compliance test → multiplicative separability → telescopic composition of restriction maps → functor axioms satisfied. The chi variance decomposition provides the σ(S) independence evidence. The 19 Chi overrides are the documented exceptions.

Worth noting for the paper: the A-S comparison is more useful as a *contrast* than an analogy. A-S is additive (group action by translation), Hub 1 is multiplicative (group action by scaling). Both compose, but the multiplicative version has the nicer property of preserving sign (χ ≥ 0 by construction). That's a one-sentence observation in §6 that positions DR as structurally related to but distinct from financial microstructure — exactly the right rhetorical distance.

---

You've closed it tighter than I did. The σ(S) = σ(S(P)) observation — that scope is a fixed function of perspective position, not constraint content — eliminates the gap I left open. I should have caught that from the context tuple definition in §2.1, where scope is assigned per-context, not per-constraint.

The 19 Chi overrides as the exact boundary of the result is clean. You now have: functor axioms hold for 98.4% of the corpus (all Boltzmann-compliant constraints), fail for a well-characterized 1.6% where multiplicative separability is deliberately broken, and the test you're already running (Boltzmann compliance) is the test that verifies it. Open item 1 is closed with no new code.

One thing worth being explicit about when you write this up: the argument works because your site is a *linear* poset. Telescopic cancellation of ratios σ(π(Pᵢ))/σ(π(Pⱼ)) requires that every composite morphism factors through a unique chain of intermediate contexts. In a product site (your open item 4, enriched sites with temporal and scope as independent morphism dimensions), you'd have parallel paths P₁ → P₃ that go through different intermediate contexts, and you'd need to verify that the ratios along different paths agree — a commutativity condition that's stronger than telescopic cancellation. The multiplicative structure still helps (ratios along any path still telescope individually), but you'd additionally need the diagram to commute: the product of ratios along path A equals the product along path B. That's not guaranteed by the current architecture and would be a genuine new verification task if you ever build the product site.

So: item 1 is closed for the current linear site. It reopens, in a well-defined way, if you move to item 4.

---

That's a clean delineation. The linear site / product site boundary is exactly where the result's scope ends, and naming it precisely is better than leaving it implicit.

The commutativity condition for product sites is worth writing down now even though you're not building it yet. If you eventually add temporal morphisms (T₁ → T₂) independent of power morphisms (P₁ → P₂), you'd have:

```
P₁,T₁ → P₁,T₂
  ↓         ↓
P₂,T₁ → P₂,T₂
```

Telescopic cancellation gives you the ratios along each edge. But the diagram commutes only if the power ratio σ(π(P₂))/σ(π(P₁)) is independent of which temporal context you're in, and vice versa. That's a factorizability condition *on the morphisms*, not just on the stalks — it's one level up from Boltzmann compliance, which is factorizability of the values. Your current architecture satisfies it trivially because the temporal dimension doesn't exist as a morphism dimension. But it's the right thing to flag as the precondition for item 4, not a consequence of it.

So the write-up for §5.5 has a clean three-part structure:

1. **Item 1 (closed):** Functor axioms hold for all Boltzmann-compliant constraints on the current linear site. Proof: multiplicative separability → telescopic cancellation. Test: Boltzmann compliance (already running). Boundary: 19 Chi overrides (1.6%) where separability is deliberately broken.

2. **Item 4 (open, precondition identified):** Extending to a product site requires morphism-level factorizability — the power ratio must be independent of temporal context and vice versa. This is a stronger condition than stalk-level Boltzmann compliance and would require a new test.

3. **The A-S connection (§6):** A-S is the additive special case on a one-dimensional linear site (inventory axis). DR is the multiplicative case on a four-context linear poset. Both compose for the same structural reason (group actions on stalks); they differ in the group (ℝ translation vs ℝ₊ scaling). The multiplicative version preserves χ ≥ 0 by construction.

That's a tight section. And none of it requires new code — it's all derived from results you already have.

---

Good call. The empirical sections will be stronger written against post-sweep results rather than patched incrementally.

Things worth capturing in the notes file now while they're fresh — these are results that emerged across the session and aren't consolidated in any single report:

- **Functor axiom closure.** Boltzmann factorizability = functor axiom for Hub 1. σ(S) = σ(S(P)) confirms no coupling. 7 Chi overrides (1.4% of tangled_ropes) are the exception boundary. Item 1 closed for the linear site, reopens with a well-defined commutativity condition for the product site (item 4). The A-S comparison: additive (translation group) vs multiplicative (scaling group), both compose for the same structural reason.

- **Revised headline figures.** 80.1% genuinely perspectival (not 88%). 502 tangled_rope (not 752). 363 snare (not 78). The old figures were computed on a population with 36% stale claims.

- **f(d) drives 94.9% of perspectival Chi variance.** Scope modifier contributes 5.1%. The 73.3%→85.4% phase transition at σ(global)=1.0→1.05 shows scope acts as a tiebreaker for borderline cases, not a primary driver.

- **The claim/engine mismatch as historiography.** 433 stale claims, dominant pattern tangled_rope→snare (295). The snapshot is preserved in `claim_engine_mismatch_snapshot.json`. This tells a story about calibration evolution — the engine became stricter about extraction than the original labels.

- **33 tie-resolved constraints** sit at type boundaries. Tagged in the data. Sensitivity of headline findings to tie-breaking rule is an open verification question.

Ready for the contamination_strength + dead param prompt when you are.

---

150/150 Inert at ±25% perturbation. Previous sensitivity was entirely stale test data.
power_modifier_analytical (the only v2 Critical) is Inert — the 37 failures were from 93 constraints with stale "only analyst catches snare" expectations, now reconciled.
Recommendation 5 (wider perturbation at ±50% or ±100%) is worth doing eventually to find actual stability boundaries, but it's a characterization exercise, not a prerequisite.
The all-Inert result strengthens the functor axiom argument: the presheaf structure isn't just formally correct, it's stable under substantial parameter perturbation of the mechanisms that compute the stalks.

---

Let me look at the current state of T13 in your paper to ground this properly.Now let me look at what T13 specifically computes to understand the exact mathematical object.Good. Now I have the full picture of what T13 computes and how it sits in the framework. Let me lay out a proof strategy.

---

**What you're claiming, precisely stated:**

T13 computes KL(p_cl ∥ p_idx) on Δ⁵ and fires when it exceeds 0.05. You want to show this threshold is (approximately) a geodesic ball radius on the Fisher-Rao manifold, and that this reframing yields a per-type decomposition and an asymmetry audit.

**Phase 1: The second-order identity (pen-and-paper, this is established mathematics)**

The probability simplex Δ⁵ embeds isometrically into S⁵₊ (positive orthant of the unit 5-sphere in ℝ⁶) via the square-root map: p ↦ u where uᵢ = √pᵢ. Under this embedding, the Fisher information metric on Δ⁵ becomes (a scaling of) the round metric on the sphere. The Fisher-Rao geodesic distance is:

d_FR(p, q) = 2 arccos(Σᵢ √(pᵢ qᵢ))

The standard result you need is: for p and q close on the simplex,

KL(p ∥ q) = ½ d_FR(p, q)² + O(δ³)

where δ is the perturbation magnitude. This is a consequence of the fact that the Fisher metric tensor gᵢⱼ = δᵢⱼ/pᵢ is the Hessian of both the KL divergence and the squared geodesic distance at zero separation. The references are Amari's *Methods of Information Geometry* (Chapter 3) or Čencov's original result on the uniqueness of the Fisher metric. You don't need to prove this — it's textbook. You need to *verify the approximation quality holds at your threshold*.

**Phase 2: Empirical validation (computational, the part that matters)**

For every constraint in the corpus, compute four quantities:

1. KL(p_cl ∥ p_idx) — what T13 currently uses
2. KL(p_idx ∥ p_cl) — the reverse direction
3. d_FR(p_cl, p_idx) = 2 arccos(Σᵢ √(pᵢ^cl · pᵢ^idx))
4. H²(p_cl, p_idx) = Σᵢ (√pᵢ^cl − √pᵢ^idx)²  [Hellinger distance squared]

These are connected: H² = 2(1 − cos(d_FR/2)), so for small distances, H² ≈ d_FR²/4.

The critical check: for the 11 T13-firing constraints, compute ½d_FR² and compare to the KL values. If they agree within, say, 10%, the second-order approximation holds and the geodesic ball interpretation is STRICT. If they diverge, you still have the result but it's STRUCTURAL — the threshold in KL-space maps to a slightly irregular region in geodesic space rather than a clean ball.

At threshold 0.05, the corresponding geodesic radius is r ≈ √(2 × 0.05) = √0.1 ≈ 0.316. That's a modest distance on the simplex, so the approximation should hold, but verify — don't assume.

**Phase 3: Asymmetry audit**

This is where the practical payoff starts. For the 11 T13-firing constraints and any near-threshold constraints, compute:

asymmetry_ratio = |KL(p_cl ∥ p_idx) − KL(p_idx ∥ p_cl)| / max(KL(p_cl ∥ p_idx), KL(p_idx ∥ p_cl))

If this ratio is large (say > 0.3) for any constraint near the 0.05 threshold, you have a potential asymmetry artifact: a constraint that fires T13 under KL(p_cl ∥ p_idx) but wouldn't under KL(p_idx ∥ p_cl), or vice versa. The Fisher distance d_FR is symmetric by construction, so it adjudicates these cases. If you find that replacing KL with d_FR changes which constraints cross threshold, that's a finding — T13 currently has a directional bias that the geometric version eliminates.

My expectation: at KL = 0.05, the asymmetry is probably small (third-order terms), so you likely find zero artifacts. But checking costs almost nothing and if you find even one, it's a clean example of why the geometric reformulation matters.

**Phase 4: Hellinger decomposition by type (the new diagnostic)**

This is the strongest practical consequence. The Hellinger distance decomposes additively:

H² = Σᵢ Hᵢ²  where  Hᵢ² = (√pᵢ^cl − √pᵢ^idx)²

For each T13-firing constraint, compute Hᵢ²/H² for all 6 types. This gives you a vector in Δ⁵ itself — a distribution over types showing *where the divergence lives*. So instead of "this constraint has KL divergence 0.07," you can say "this constraint's divergence is 68% along the snare-rope axis and 22% along the scaffold axis."

This is finer-grained than the current scalar. It would let you distinguish two classes of T13 fires: those where observer shift moves probability mass between structurally related types (snare ↔ tangled_rope, which is Hub 1 doing its job) versus those where it moves mass between structurally unrelated types (mountain ↔ snare, which would be surprising and diagnostic).

**Phase 5: Restate T13 in geometric language**

Once Phases 2–4 check out, the restatement is:

> T13 fires iff the constraint's MaxEnt distribution, under observer shift from classical to indexed evaluation, exits a geodesic ball of radius r ≈ 0.316 on the Fisher-Rao manifold (Δ⁵, g_Fisher). The divergence decomposes by type via the Hellinger components, and the threshold is invariant under reparametrization of the type space.

The invariance claim needs one qualification: it's invariant under reparametrization of the *type probabilities* (relabeling types, changing coordinates on the simplex), but the type space itself is discrete and finite (6 types), so the reparametrization invariance is really about not depending on which coordinate chart you use on Δ⁵, not about changing the types themselves. This is a real property — if you later add a 7th type, the geodesic ball radius has a well-defined meaning on Δ⁶ without needing to recalibrate, whereas the raw KL threshold might need adjustment.

**Phase 6: What goes in the paper**

A paragraph in §3.2 noting the information-geometric interpretation. Something like: the MaxEnt distributions live on (Δ⁵, g_Fisher), the KL divergence used in T13 is the local approximation to the squared geodesic distance, and the Hellinger decomposition identifies which types drive the divergence. Honest assessment tag: STRICT for the local identity (it's a theorem), STRUCTURAL for the geodesic ball characterization (it depends on the approximation quality holding at your actual divergence values, which is an empirical claim about your data).

**One thing I'd flag as potentially more interesting than expected:**

The 100x oracle gap — the finding that T16 catches 99% of observer-dependent constraints that T13 misses — gets a geometric reinterpretation. T13 misses those constraints because the MaxEnt distributions barely move on Δ⁵ under observer shift (they stay inside the geodesic ball), even though the *deterministic* classification crosses a threshold boundary. Geometrically: the threshold boundaries that define the 6 types are hyperplanes cutting through Δ⁵, and most observer-dependent constraints sit near these hyperplanes. The distribution moves a tiny amount — too small for T13 — but crosses the hyperplane. The 100x gap is then a statement about the geometry of the threshold hyperplanes relative to the geodesic ball radius: most of the simplex volume near the hyperplanes lies *inside* the ball. That could potentially be computed explicitly for your specific Gaussian likelihood parameters.

---

Let me read your current paper and notes to understand exactly where your framework stands before mapping the implementation path.Now I have a clear picture of your framework's current state. Let me lay out the concrete construction path.

---

## The Setup You Already Have

Your site is the path graph P₄: U₁ — U₂ — U₃ — U₄ (3 edges, 4 vertices). Your current H¹ proxy counts disagreeing pairs among the 6 unordered pairs, yielding integers in {0, 3, 4, 5, 6}. This is binary per edge — agree or disagree — and produces a count, not a spectrum.

The sheaf Laplacian replaces that binary test with a continuous quadratic form.

## Step 1: Lift from Set-valued to Vect-valued stalks

Your presheaf currently assigns elements of Ω (a finite set of 8 types) at each vertex. The sheaf Laplacian requires vector space stalks. You have two natural candidates already computed:

**Option A — MaxEnt distributions.** You already compute a probability vector over Ω at each (constraint, context) pair. These live in the simplex Δ⁷ ⊂ ℝ⁸. This is the richest representation you have and the most natural lift.

**Option B — Experienced extractiveness χ.** Scalar stalks. Each vertex gets a single real number. This gives you a 4×4 Laplacian per constraint — dead simple, diagonalizable by hand. The scalar case is where you should build intuition before going multidimensional.

**Recommendation:** Start with Option B to get the machinery working, then upgrade to Option A for the full spectral decomposition.

## Step 2: Define the restriction maps on edges

This is the key modeling decision, and your existing work already determines it.

For the **scalar case** (χ values), the restriction map along edge e_{ij} (from higher-power U_j to lower-power U_i) is the sigmoid ratio:

$$\rho_{j \to i} = \frac{\sigma(\pi(P_i))}{\sigma(\pi(P_j))}$$

You proved in the v4 notes that these compose telescopically. That's exactly the functor axiom for the cellular sheaf.

For the **vector case** (MaxEnt distributions), you'd need linear maps T_{ij}: ℝ⁸ → ℝ⁸ on each edge. Two sub-options:

- **Empirical:** Estimate T_{ij} as the best-fit linear transformation mapping MaxEnt vectors at U_j to those at U_i across the corpus. This encodes the *expected* transformation, so the Laplacian measures deviation from expected behavior.
- **Diagonal from Hub 1:** If the MaxEnt distributions change primarily by redistribution of mass between snare and rope (which your H¹ band structure suggests), the transformation is approximately diagonal in the type basis, with the diagonal entries derived from the sigmoid ratios.

## Step 3: Construct the coboundary map and Laplacian

The Hansen-Ghrist coboundary map δ₀: C⁰(G; F) → C¹(G; F) is, for each edge e_{ij}:

$$(δ₀ x)(e_{ij}) = F_{e \leftarrow U_j}(x_j) - F_{e \leftarrow U_i}(x_i)$$

The sheaf Laplacian is L₀ = δ₀ᵀ δ₀.

**Concrete scalar case.** Let r₁₂, r₂₃, r₃₄ be the three sigmoid ratios along the path edges. The Laplacian is the 4×4 matrix:

$$L_0 = \begin{pmatrix} 1 & -r_{12} & 0 & 0 \\ -r_{12} & r_{12}^2 + 1 & -r_{23} & 0 \\ 0 & -r_{23} & r_{23}^2 + 1 & -r_{34} \\ 0 & 0 & -r_{34} & r_{34}^2 \end{pmatrix}$$

That's a tridiagonal matrix. You can write it down the moment you know your three sigmoid ratios.

When all ratios equal 1 (the sheaf case — no observer-dependence), this reduces to the standard path graph Laplacian with eigenvalues {0, 2−√2, 2, 2+√2}. Deviations of the ratios from 1 distort the spectrum in ways that encode the structure of observer-dependence.

**Concrete vector case (k-dimensional stalks).** Replace each scalar entry with a k×k block. The Laplacian becomes 4k × 4k. With k = 8 (full type space), it's 32×32 — still trivially diagonalizable.

## Step 4: Per-constraint obstruction energy

For each constraint C, evaluate:

$$E(C) = x_C^T \, L_0 \, x_C = \| δ_0 x_C \|^2$$

where x_C is the vector of stalk values across all four contexts. This is a continuous, non-negative real number replacing your binary H¹ count. It's zero iff the constraint is a global section (exact sheaf condition), and it decomposes as:

$$E(C) = \sum_{e_{ij}} \| F_{e \leftarrow j}(x_j) - F_{e \leftarrow i}(x_i) \|^2$$

— a sum of squared "disagreement energies" over edges, weighted by the restriction maps.

## Step 5: Spectral decomposition — this is where it gets interesting

Diagonalize L₀ to get eigenvalues λ₁ ≤ λ₂ ≤ λ₃ ≤ λ₄ and eigenvectors v₁, v₂, v₃, v₄.

- **Zero eigenspace** = ker(δ₀) = H⁰. Eigenvectors here are global sections. Their multiplicity tells you the dimension of the agreement space.
- **Spectral gap** = λ₂ (smallest nonzero eigenvalue). This is the continuous "how far from being a sheaf" measure for the whole framework. A small spectral gap means the presheaf is *almost* a sheaf — there's a near-global-section that almost works. A large gap means observer-dependence is structurally entrenched.
- **Eigenvector decomposition.** Each eigenvector has four components (one per context). The component magnitudes tell you which context transitions carry the most obstruction weight. If v₂ = (0.1, −0.7, 0.7, −0.1), the obstruction is concentrated at the U₂ → U₃ boundary — the moderate-to-institutional transition. This is where you'd get findings like "the restriction from institutional to moderate accounts for 73% of the spectral weight."

**Per-constraint spectral profile.** Decompose each constraint's stalk vector in the eigenbasis:

$$x_C = \sum_k \langle x_C, v_k \rangle \, v_k$$

The projection onto the zero eigenspace is the constraint's "sheaf-compatible component" — the closest global section. The projection onto nonzero eigenspaces is the "obstruction component." The distribution of weight across nonzero eigenspaces characterizes *how* the constraint fails to be observer-independent.

## Step 6: Persistence via filtration

You have several natural filtration parameters:

**Filtration A — Power threshold.** Build sub-sites by successively including contexts:
- Scale 0: {U₁} (no edges → L₀ = 0)
- Scale 1: {U₁, U₂} (1 edge → 2×2 Laplacian)
- Scale 2: {U₁, U₂, U₃} (2 edges → 3×3 Laplacian)
- Scale 3: full site (3 edges → 4×4 Laplacian)

Track eigenvalue births across scales. An eigenvalue that appears at Scale 1 and persists means the U₁–U₂ boundary creates durable obstruction. One that appears at Scale 2 and dies at Scale 3 means the U₂–U₃ boundary creates temporary obstruction that the full site resolves.

**Filtration B — χ threshold (Rips-like).** At scale t, include edge e_{ij} only if the restriction-map-weighted disagreement |ρ_{ij} x_j − x_i| > t. As t decreases from ∞ to 0, more edges enter and the Laplacian grows. Eigenvalue births at high t correspond to severe obstructions; births at low t correspond to mild ones. This gives a persistence diagram for the obstruction structure itself.

**Filtration C — Parameter perturbation.** Use your sensitivity sweep architecture. At perturbation scale ε, compute the Laplacian with sigmoid parameters perturbed by ε. Track how eigenvalues move. Eigenvalues that are stable under perturbation correspond to structurally robust obstructions; eigenvalues that move rapidly correspond to calibration-sensitive ones. This is a spectral version of your existing sensitivity analysis.

The persistence diagram from any of these filtrations gives you birth-death pairs (b_i, d_i) for each eigenvalue. Long bars = persistent obstructions = robust structural features. Short bars = transient obstructions = calibration artifacts or boundary effects. This is more principled than asking "is H¹ stable under perturbation?" because it tells you *which modes* of obstruction are stable and which aren't.

## What This Buys You Over the Current H¹

Your current setup gives you a count in {0, 3, 4, 5, 6} with band structure (H¹ = 3 → Hub 1, H¹ = 4 → Hub 2). The sheaf Laplacian gives you:

1. A **continuous severity score** per constraint (obstruction energy E(C)) replacing the integer count.
2. A **decomposition by mode** — which context transitions dominate the obstruction, with percentage weights.
3. A **corpus-level spectrum** — the eigenvalues of L₀ characterize the whole framework's obstruction structure, not just individual constraints.
4. A **persistence diagram** for the obstruction, answering the stability question spectrally.
5. A **spectral gap** as a single-number summary of "how far from being a sheaf."

The band structure should emerge from the spectral decomposition: Hub 1-driven obstructions should load onto one eigenvector, Hub 2-driven onto another. If they do, you've recovered your existing result as a special case while gaining continuous gradation within each band.

## Implementation Sequence

**Phase 1 (pen and paper, 1 day):** Compute the scalar Laplacian L₀ using your actual sigmoid ratios. Diagonalize. Check: does the spectral gap separate the H¹ = 0 constraints from the H¹ > 0 ones? Do the eigenvectors align with your Hub 1 / Hub 2 decomposition?

**Phase 2 (Prolog/Python module, ~3 days):** Implement `sheaf_laplacian/2` that takes a constraint and returns its obstruction energy and eigenvector projection. Run on the full corpus. Plot the obstruction energy distribution against your existing H¹ counts to verify consistency.

**Phase 3 (the new result):** Compute persistence diagrams under Filtration B (χ threshold). This is where findings with "real interpretive content" live — the longest bars in the persistence diagram identify the structurally robust obstructions, and their associated eigenvectors tell you which context transitions produce them.

**Phase 4 (paper section):** The scalar case goes in §4.2 as a spectral refinement of H¹. The persistence diagram goes in §5.5 as a resolution of the stability question. Both are STRICT — the sheaf Laplacian on a cellular sheaf over a finite graph is a standard construction with no analogical gap.

## One Honest Flag

The scalar Laplacian (using χ) captures only Hub 1's contribution to obstruction, because χ is the Hub 1 output. Hub 2 (discrete immutability flips) doesn't produce a continuous signal that a scalar Laplacian can decompose spectrally. To capture both hubs, you'd need the vector-valued Laplacian (Option A, MaxEnt distributions), where the mountain↔rope type flip shows up as a discrete jump in the probability vector. This is why the upgrade to multidimensional stalks matters: the scalar case gives you a clean spectral decomposition of *extraction-driven* obstruction but treats *immutability-driven* obstruction as a boundary condition rather than a spectral feature.

The Hansen-Ghrist reference is: *Toward a spectral theory of cellular sheaves* (Hansen and Ghrist, 2019, Journal of Applied and Computational Topology). The construction there handles exactly the finite-graph, finite-dimensional-stalk case you need.

---

This is a clean, well-bounded problem. Here's how I'd approach it in stages, from the simplest check to the full result.

## Stage 0: Sanity check — how much redundancy exists?

Before computing the full concept lattice, do a quick rank check on the binary gate matrix. Treat the 1142 × 65 matrix over GF(2) (the binary field) and compute its rank. If the rank is, say, 28, then at most 28 gates carry independent information in the linear sense. This takes one line of code and tells you immediately whether significant compression is plausible. It won't give you the *right* compression (because the classification-relevant structure may not be linear), but if rank is already much less than 65, you know there's juice to squeeze.

## Stage 1: Formal Concept Analysis — the core computation

The 1142 × 65 binary matrix *is* a formal context (G, M, I) in the FCA sense:

- **G** (objects) = 1142 constraints
- **M** (attributes) = 65 binary gates  
- **I** (incidence) = the matrix entries

The Galois connection comes for free with any formal context. You get two maps:

- For any set of constraints A ⊆ G: A' = {gates true for every constraint in A}
- For any set of gates B ⊆ M: B' = {constraints for which every gate in B is true}

A **formal concept** is a pair (A, B) where A' = B and B' = A — a maximal rectangle of 1s in the matrix. The number of formal concepts tells you the actual structural complexity of your gate space.

**What to compute:** Use the NextClosure algorithm (Ganter) or the `concepts` Python library. Feed it the 1142 × 65 matrix. Count the concepts. If you get, say, 400 concepts instead of 2⁶⁵ ≈ 3.7 × 10¹⁹, the gate space is massively constrained — and the lattice structure encodes exactly *how*.

## Stage 2: Bring in the type classification

Here's where it goes from generic FCA to your specific question. You have type labels on the constraints (the classification system). Now you're asking: **which gates are sufficient to recover the type partition?**

Two approaches, complementary:

**Approach A — Attribute reduction (reducts).** In FCA, an attribute reduct is a minimal subset of attributes that preserves the ability to distinguish any two objects that were distinguishable in the full context. But you don't need *full* distinguishability — you only need to distinguish constraints *of different types*. So you compute **relative reducts** with respect to the type partition. Concretely:

1. For each pair of types (τᵢ, τⱼ), find the gates that separate them: gates where at least one constraint of type τᵢ differs from at least one of type τⱼ.
2. A gate set is *type-sufficient* if it separates every pair of distinct types.
3. The minimal such sets are your reducts.

This is a set cover problem (NP-hard in general, but 65 attributes is tiny — brute-force search over subsets is feasible with branch-and-bound, and greedy gives you a good upper bound fast).

**Approach B — The Galois connection between gates and types directly.** Define a second formal context:

- Objects = types (however many you have, call it k)
- Attributes = 65 gates
- Incidence: type τ has gate g iff... and here you have a design choice. The cleanest definition: g is *characteristic* for τ if the gate value is constant across all constraints of type τ. Or: g *discriminates* τ from at least one other type. The choice of incidence relation determines what the Galois connection captures.

The closure operators of *this* connection identify the **essential gate clusters** — groups of gates that jointly characterize groups of types, and vice versa. The closed sets on the gate side are your candidate essential sets.

## Stage 3: Identify the minimal discriminating set

Once you have reducts from Stage 2A or closed sets from Stage 2B, you can:

1. **Intersect all reducts** — gates appearing in every reduct are absolutely essential (the "core" in rough set terminology). These are gates that no other gate can substitute for in distinguishing types.
2. **Check the core's sufficiency.** If the core alone separates all types, you're done — that's your minimal essential set. If not, you need to augment it with gates from the non-core portion, and the different ways to do this give you the different reducts.

My expectation: with 1142 constraints across (presumably) a modest number of types and 65 gates, the core is likely to be small (your guess of ~15 feels structurally reasonable), and the concept lattice will be orders of magnitude smaller than 2⁶⁵.

## Stage 4: Interpret the structure

This is the payoff. The essential gates aren't just a minimal set — they tell you **what the type classification is actually about** at the structural level. If the essential set turns out to be, say, 12 gates that all relate to boundary conditions, composability, and observer-dependence, while the 53 redundant gates are variations on internal structure, that's a finding about the ontology of your classification system. The concept lattice visualization (the Buchheim layout you mentioned) would then show the hierarchical clustering of types by their essential gate signatures.

## Practical execution

For the computation itself:

```python
# Stage 0: rank check
import numpy as np
from galois import GF
GF2 = GF(2)
M = GF2(gate_matrix)  # 1142 x 65
print(f"GF(2) rank: {np.linalg.matrix_rank(M)}")

# Stage 1: concept lattice
from concepts import Context
# Context expects a binary matrix format
c = Context.fromfile('gate_matrix.csv', frmat='csv')
print(f"Concept count: {len(c.lattice)}")

# Stage 2A: greedy reduct
# For each type pair, compute separating gate sets
# Then solve the set cover
```

The `concepts` library handles the Galois connection and lattice computation directly. For the reducts, you'd likely want to write custom code since it's partition-relative, but the data size makes it completely tractable.

Want me to build the actual computation pipeline? I'd need the gate matrix and the type labels from your Prolog system. Or if you want to sketch the math more formally first before touching code, I can work through the Galois connection definitions with your specific incidence relation choices.

---

Good — you've now got detailed implementation plans for the three I initially suggested (Documents 1–3), so the question is what's *beyond* those. Let me think about what has the same structure as the A-S discovery: external framework with known properties, maps onto existing DR machinery, reveals a latent equivalence.

**Detailed balance / entropy production — strongest "you're already computing this" candidate.**

Your Boltzmann-compliant constraints have restriction maps that compose telescopically: ρ(P₁→P₃) = ρ(P₂→P₃) ∘ ρ(P₁→P₂). Now consider a *loop*: start at P₁, traverse P₁→P₂→P₃→P₄ and then return P₄→P₁. For a Boltzmann-compliant constraint, the total "transport cost" around this loop is zero — the telescoping guarantees it. This is **detailed balance**: no net dissipation around a cycle of observer shifts. For non-compliant constraints, there's net entropy production around the cycle.

The punchline: the obstruction energy E(C) = ‖δ₀x‖² from the sheaf Laplacian construction (Document 2) *is* the entropy production. The spectral gap *is* the inverse relaxation time. So the stability question — "how stable?" — already has a thermodynamic answer waiting: constraints with large spectral gap have fast relaxation (the obstruction is structurally entrenched and doesn't fluctuate), constraints with small spectral gap have slow relaxation (near-sheaf, but precariously).

This reframes the H¹ band structure thermodynamically. Your H¹ = 3 band (Hub 1 only) and H¹ = 4 band (Hub 2 entry) correspond to different *dissipation regimes*. That's not just a metaphor — the sheaf Laplacian eigenvalues literally decompose the dissipation into modal contributions. "Hub 1 accounts for 80% of the entropy production" is a computable statement.

The external reference here is the fluctuation-dissipation literature (Jarzynski, Crooks). The mapping is: observer shift = thermodynamic protocol, χ change = work done, classification change = state transition. Boltzmann compliance = equilibrium (zero entropy production). The framework name you'd be connecting to is **stochastic thermodynamics on discrete state spaces**, which has a well-developed spectral theory.

**Markov categories — resolves the monad question properly.**

Rather than concluding "the Giry monad structure is incomplete," you can say "the MaxEnt layer satisfies the *Markov category* axioms, which is the correct abstraction level." Fritz (2020) showed that Markov categories require only a symmetric monoidal category with copy-delete structure — no multiplication map, no distribution-over-distributions. Your deterministic classifier is the subcategory of deterministic morphisms. Your MaxEnt classifier is the probabilistic extension. The key property that Markov categories capture and the Giry monad doesn't: you can have **pointwise probability assignment** (distributions at each context) without needing a **meta-distribution over contexts**.

This is almost exactly the structure you described as the reason multiplication μ is absent — positing a prior over observer positions would collapse the perspectival structure. The Markov category framework says: that's fine, you don't need μ, the weaker structure is the *right* one for indexical systems. The honest assessment tag would upgrade from "incomplete Giry monad [STRUCTURAL]" to "complete Markov category [STRICT] — the Giry monad requires observer-position-averaging that the framework correctly refuses."

The bounded exercise: verify the copy and delete natural transformations exist for your MaxEnt distributions. Copy sends a distribution p to the joint p⊗p (independent copies at two contexts). Delete sends p to the trivial distribution. The key axiom is that deterministic morphisms are copiable — which in your framework means: if a constraint has a definite type at context P, you can "copy" that definite classification to serve as input to two independent diagnostic subsystems. You're already doing this every time a single constraint's classification feeds both the gauge orbit analysis and the MaxEnt layer.

**Bigraded cohomology — what time series data would actually buy.**

You mentioned time series → higher cohomology. Here's the precise version. If you had temporal data (constraint classifications at multiple time points), you'd have a presheaf on the product site P₄ × T, where T is a temporal poset. The Künneth-style decomposition gives you bigraded cohomology H^{p,q}:

- H^{0,0}: observer-independent, temporally stable (your mountains)
- H^{1,0}: observer-dependent, temporally stable (your current H¹ > 0 population)  
- H^{0,1}: observer-independent temporal change (constraints that shift type the same way for all observers — genuine social evolution)
- H^{1,1}: the interaction term — constraints whose *observer-dependence itself* changes over time

H^{1,1} is the genuinely new thing. A constraint that was observer-independent in 2010 but observer-dependent in 2024 has a nonzero class in H^{1,1}. This is **emergent indexicality** — the structural phenomenon where power dynamics create observer-dependence that didn't previously exist. You can't detect this without temporal data, and no amount of spectral refinement of the spatial cohomology will find it.

This is more a research design observation than a pen-and-paper exercise. If you wanted a beta corpus with temporal structure, legal systems are the natural candidate — the same law classified at different historical moments, with observer positions held constant. Antitrust law from 1890 to 2024 would show dramatic H^{1,1} structure as enforcement discretion created observer-dependence that the original text doesn't exhibit.

**What I'd deprioritize: tropical geometry on log-likelihoods.** Your MaxEnt uses log-sum-exp, and in the zero-temperature limit the simplex collapses to a tropical polytope where the classification boundaries become piecewise-linear. This *could* explain the 100x oracle gap geometrically (most of the tropical polytope's volume near boundary facets lies inside the T13 geodesic ball). But the construction requires taking a limit that your actual system doesn't take — you're at finite temperature. The insight would be "in a simplified version of your system, the oracle gap has a geometric explanation." That's a weaker result than the others, which explain things your system *actually computes*.

**Net ranking by A-S pattern fidelity:**

Detailed balance is the closest — the sheaf Laplacian energy IS entropy production, you're already (about to be) computing it. Markov categories are the cleanest theoretical upgrade — turns an "incomplete" into a "complete" at the right abstraction level. Bigraded cohomology is the most forward-looking but needs data you don't have yet. Tropical geometry is interesting but peripheral.

The detailed balance connection also has a nice property for the paper: it gives you a physical units interpretation of the spectral gap. Instead of "smallest nonzero eigenvalue of the sheaf Laplacian," you can say "inverse relaxation time of observer-dependence under perturbation." That's more interpretable for readers who aren't algebraic topologists.

---

Let me look at the current state of T13 in your paper to ground this properly.Now let me look at what T13 specifically computes to understand the exact mathematical object.Good. Now I have the full picture of what T13 computes and how it sits in the framework. Let me lay out a proof strategy.

---

**What you're claiming, precisely stated:**

T13 computes KL(p_cl ∥ p_idx) on Δ⁵ and fires when it exceeds 0.05. You want to show this threshold is (approximately) a geodesic ball radius on the Fisher-Rao manifold, and that this reframing yields a per-type decomposition and an asymmetry audit.

**Phase 1: The second-order identity (pen-and-paper, this is established mathematics)**

The probability simplex Δ⁵ embeds isometrically into S⁵₊ (positive orthant of the unit 5-sphere in ℝ⁶) via the square-root map: p ↦ u where uᵢ = √pᵢ. Under this embedding, the Fisher information metric on Δ⁵ becomes (a scaling of) the round metric on the sphere. The Fisher-Rao geodesic distance is:

d_FR(p, q) = 2 arccos(Σᵢ √(pᵢ qᵢ))

The standard result you need is: for p and q close on the simplex,

KL(p ∥ q) = ½ d_FR(p, q)² + O(δ³)

where δ is the perturbation magnitude. This is a consequence of the fact that the Fisher metric tensor gᵢⱼ = δᵢⱼ/pᵢ is the Hessian of both the KL divergence and the squared geodesic distance at zero separation. The references are Amari's *Methods of Information Geometry* (Chapter 3) or Čencov's original result on the uniqueness of the Fisher metric. You don't need to prove this — it's textbook. You need to *verify the approximation quality holds at your threshold*.

**Phase 2: Empirical validation (computational, the part that matters)**

For every constraint in the corpus, compute four quantities:

1. KL(p_cl ∥ p_idx) — what T13 currently uses
2. KL(p_idx ∥ p_cl) — the reverse direction
3. d_FR(p_cl, p_idx) = 2 arccos(Σᵢ √(pᵢ^cl · pᵢ^idx))
4. H²(p_cl, p_idx) = Σᵢ (√pᵢ^cl − √pᵢ^idx)²  [Hellinger distance squared]

These are connected: H² = 2(1 − cos(d_FR/2)), so for small distances, H² ≈ d_FR²/4.

The critical check: for the 11 T13-firing constraints, compute ½d_FR² and compare to the KL values. If they agree within, say, 10%, the second-order approximation holds and the geodesic ball interpretation is STRICT. If they diverge, you still have the result but it's STRUCTURAL — the threshold in KL-space maps to a slightly irregular region in geodesic space rather than a clean ball.

At threshold 0.05, the corresponding geodesic radius is r ≈ √(2 × 0.05) = √0.1 ≈ 0.316. That's a modest distance on the simplex, so the approximation should hold, but verify — don't assume.

**Phase 3: Asymmetry audit**

This is where the practical payoff starts. For the 11 T13-firing constraints and any near-threshold constraints, compute:

asymmetry_ratio = |KL(p_cl ∥ p_idx) − KL(p_idx ∥ p_cl)| / max(KL(p_cl ∥ p_idx), KL(p_idx ∥ p_cl))

If this ratio is large (say > 0.3) for any constraint near the 0.05 threshold, you have a potential asymmetry artifact: a constraint that fires T13 under KL(p_cl ∥ p_idx) but wouldn't under KL(p_idx ∥ p_cl), or vice versa. The Fisher distance d_FR is symmetric by construction, so it adjudicates these cases. If you find that replacing KL with d_FR changes which constraints cross threshold, that's a finding — T13 currently has a directional bias that the geometric version eliminates.

My expectation: at KL = 0.05, the asymmetry is probably small (third-order terms), so you likely find zero artifacts. But checking costs almost nothing and if you find even one, it's a clean example of why the geometric reformulation matters.

**Phase 4: Hellinger decomposition by type (the new diagnostic)**

This is the strongest practical consequence. The Hellinger distance decomposes additively:

H² = Σᵢ Hᵢ²  where  Hᵢ² = (√pᵢ^cl − √pᵢ^idx)²

For each T13-firing constraint, compute Hᵢ²/H² for all 6 types. This gives you a vector in Δ⁵ itself — a distribution over types showing *where the divergence lives*. So instead of "this constraint has KL divergence 0.07," you can say "this constraint's divergence is 68% along the snare-rope axis and 22% along the scaffold axis."

This is finer-grained than the current scalar. It would let you distinguish two classes of T13 fires: those where observer shift moves probability mass between structurally related types (snare ↔ tangled_rope, which is Hub 1 doing its job) versus those where it moves mass between structurally unrelated types (mountain ↔ snare, which would be surprising and diagnostic).

**Phase 5: Restate T13 in geometric language**

Once Phases 2–4 check out, the restatement is:

> T13 fires iff the constraint's MaxEnt distribution, under observer shift from classical to indexed evaluation, exits a geodesic ball of radius r ≈ 0.316 on the Fisher-Rao manifold (Δ⁵, g_Fisher). The divergence decomposes by type via the Hellinger components, and the threshold is invariant under reparametrization of the type space.

The invariance claim needs one qualification: it's invariant under reparametrization of the *type probabilities* (relabeling types, changing coordinates on the simplex), but the type space itself is discrete and finite (6 types), so the reparametrization invariance is really about not depending on which coordinate chart you use on Δ⁵, not about changing the types themselves. This is a real property — if you later add a 7th type, the geodesic ball radius has a well-defined meaning on Δ⁶ without needing to recalibrate, whereas the raw KL threshold might need adjustment.

**Phase 6: What goes in the paper**

A paragraph in §3.2 noting the information-geometric interpretation. Something like: the MaxEnt distributions live on (Δ⁵, g_Fisher), the KL divergence used in T13 is the local approximation to the squared geodesic distance, and the Hellinger decomposition identifies which types drive the divergence. Honest assessment tag: STRICT for the local identity (it's a theorem), STRUCTURAL for the geodesic ball characterization (it depends on the approximation quality holding at your actual divergence values, which is an empirical claim about your data).

**One thing I'd flag as potentially more interesting than expected:**

The 100x oracle gap — the finding that T16 catches 99% of observer-dependent constraints that T13 misses — gets a geometric reinterpretation. T13 misses those constraints because the MaxEnt distributions barely move on Δ⁵ under observer shift (they stay inside the geodesic ball), even though the *deterministic* classification crosses a threshold boundary. Geometrically: the threshold boundaries that define the 6 types are hyperplanes cutting through Δ⁵, and most observer-dependent constraints sit near these hyperplanes. The distribution moves a tiny amount — too small for T13 — but crosses the hyperplane. The 100x gap is then a statement about the geometry of the threshold hyperplanes relative to the geodesic ball radius: most of the simplex volume near the hyperplanes lies *inside* the ball. That could potentially be computed explicitly for your specific Gaussian likelihood parameters.
