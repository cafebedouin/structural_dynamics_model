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
