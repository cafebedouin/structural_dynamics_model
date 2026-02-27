# V5 Surgical Patch Document

**Sources:** Cross-corpus comparison (Flash 887 / Haiku 907), four-model review commentary (Gemini, Copilot, Perplexity, ChatGPT), Claude Code verification matrix, v4 paper additions drafts.

**Convention:** Each patch gives (a) location, (b) source of the edit, (c) the existing text to find, (d) the replacement or insertion. "INSERT AFTER" means add new text after the quoted passage; "REPLACE" means swap old for new.

---

## PATCH 1 — §1 Introduction: "Deferential" and "Realist" operational definitions

**Source:** Perplexity review — "add one sentence early defining what's 'realist' and what's 'deferential' operationally."

**Location:** §1 Introduction, first paragraph or wherever the framework is first named.

**INSERT** (wherever the term "Deferential Realism" first appears, add one sentence):

> The framework is *realist* in that it treats constraints as having objective structural properties (extractiveness, suppression, coordination function) that exist independently of any observer; it is *deferential* in that it treats the *classification* of those properties as irreducibly dependent on the observer's structural position, deferring to each perspective's local truth rather than asserting a global one.

---

## PATCH 2 — §2.1: Site as one instantiation in a family

**Source:** Gemini review — the framing "one instantiation in a family" sharpens the site commitment language. Also addresses ChatGPT's "geometry-relative, not world-relative" pressure point and Perplexity's "a correct formalization" vs "the correct abstraction."

**Location:** §2.1, after the paragraph beginning "The choice of site is normative."

**FIND:**
> The current 4-element linear poset is the simplest non-trivial choice, and several of the empirical results depend on its linearity: the H¹ gap at 1 and 2 (§4.2) is a consequence of linear ordering, not a universal property of perspectival dependence. The site is where political commitments enter the mathematics, and the framework makes that entry point explicit rather than hiding it behind a claim of objectivity.

**REPLACE WITH:**
> The current 4-element linear poset is one instantiation in a family of possible sites, chosen as the simplest non-trivial member. Several empirical results depend on its linearity: the H¹ gap at 1 and 2 (§4.2) is a consequence of linear ordering, not a universal property of perspectival dependence; the eigenvalue spectrum and superselection structure (§4.5) are determined by this site's restriction ratios. The invariants the framework computes are geometry-relative — properties of this site's measurement apparatus — not world-relative assertions about the constraints themselves. Different site choices would produce different invariants, and the framework provides *a* correct formalization of observer-dependent classification, not *the* correct abstraction. The site is where political commitments enter the mathematics, and the framework makes that entry point explicit rather than hiding it behind a claim of objectivity.

---

## PATCH 3 — §2.2: ε-invariance as design axiom + informal χ gloss

**Sources:** Gap analysis item #2 (ε-invariance principle, DP-001). Perplexity review — "add an informal gloss before the χ formula."

**Location:** §2.2, just before or within the Channel 1 description.

**FIND:**
> **Channel 1 (Hub 1 — continuous).** Base extractiveness $\varepsilon$ is a context-independent property of the constraint.

**REPLACE WITH:**
> **Channel 1 (Hub 1 — continuous).** Base extractiveness $\varepsilon$ is a context-independent property of the constraint — this is a design axiom (ε-invariance): the same constraint has the same base extractiveness regardless of who is observing it, though the *experienced* extractiveness varies by observer. The context tuple is closed at arity 4: given fixed ε and a fixed (Power, TimeHorizon, ExitOptions, Scope) context, χ is fully determined.

Then, directly before the χ formula, **INSERT:**

> Informally: how much extraction you experience depends on how powerful you are. A powerless observer trapped in a constraint feels the full force of its extractiveness; an institutional observer with exit options and long time horizons experiences the same constraint as substantially less extractive — possibly reclassifying it from "snare" to "rope." The formula:

---

## PATCH 4 — §2.4: Covering analysis scope redundancy

**Source:** Gap analysis item #4 (covering_analysis.md). Cross-corpus confirmation.

**Location:** §2.4, after the naturality certificates paragraph or after the three-way equivalence table.

**FIND:**
> All three classifications are STRICT: they test well-defined conditions on the naturality square.

**INSERT AFTER:**

> An empirical covering analysis independently confirms the scope modifier's limited effect. The institutional scope cells (in/loc, in/nat, in/glo) produce 100% identical classifications — all three are observationally redundant. The minimum discriminating subset of the Power × Scope grid requires only 8 of the 12 cells, confirming that the scope modifier σ(S) does not introduce coupling at the institutional context. This result is consistent across both corpora (Flash and Haiku) and independently validates the χ variance decomposition finding that f(d) accounts for 94.8% of classification variance, with scope contributing the remaining 5.2%.

---

## PATCH 5 — §2.6: Dual type distribution table with attractor convergence + index sufficiency

**Sources:** Cross-corpus comparison §§3-4, gap analysis item #7 (index sufficiency). This is the largest single edit.

**Location:** §2.6, replace the existing type distribution table and add new material.

**FIND the existing type distribution table** (lines 163-174):
> **Type distribution in the current corpus:**
>
> | Type (analytical context) | Count | % of corpus |
> |---|---|---|
> | tangled_rope | 550 | 48.2% |
> [etc.]

**REPLACE WITH:**

> **Type distribution depends on whether structural signatures are integrated.** Table 1 reports both the metric-only classification (from the deterministic cascade before signature resolution) and the post-override classification (after signature integration), for each of two independently generated corpora.
>
> **Table 1: Dual Type Distribution**
>
> | Type | Flash metric | Flash post | Haiku metric | Haiku post |
> |------|-------------|-----------|-------------|-----------|
> | snare | 448 (50.5%) | 109 (12.3%) | 152 (16.8%) | 175 (19.3%) |
> | tangled_rope | 287 (32.4%) | 549 (62.0%) | 619 (68.2%) | 567 (62.5%) |
> | mountain | 139 (15.7%) | 139 (15.7%) | 129 (14.2%) | 129 (14.2%) |
> | rope | 10 (1.1%) | 62 (7.0%) | 7 (0.8%) | 18 (2.0%) |
> | scaffold | 2 (0.2%) | 10 (1.1%) | 0 (0.0%) | 9 (1.0%) |
> | piton | 0 (0.0%) | 17 (1.9%) | 0 (0.0%) | 9 (1.0%) |
>
> *Flash: Gemini Flash 2.0, N=887. Haiku: Claude Haiku 4.5, N=907.*
>
> The two corpora have opposite metric-only distributions: Flash is snare-dominated (50.5%), Haiku is tangled_rope-dominated (68.2%). Yet after signature integration, both converge to approximately 62% tangled_rope. The signature system — principally the false_ci_rope override, which applies a 3× boost to tangled_rope probability when the Boltzmann independence test detects cross-perspectival coupling alongside extraction — acts as a fixed-point attractor: different input distributions converge to similar post-override outputs. This convergence reframes the signature system from a correction mechanism to an architectural constraint of the framework. Mountains are invariant under signature integration (Δ = 0 in both corpora), as expected: natural law constraints receive the natural_law signature, which reinforces rather than overrides their metric classification.
>
> **Index sufficiency.** Testing across all index configurations on the Flash corpus, only 3 genuine classification collisions occur out of 887 constraints (0.3%), with 19 additional collisions representing expected perspectival variance. The 4-axis context tuple captures classification-relevant variation without gaps: the non-mountain anomaly rate is 0.0%.

---

## PATCH 6 — §3.3: T2–T8 = 0 note and trigger population ranges

**Source:** Cross-corpus comparison. Claude Code verification.

**Location:** §3.3, Category A section, after the T1–T11 trigger descriptions.

**INSERT** (after the trigger inventory but before Category B):

> **Corpus sensitivity note.** Triggers T2–T8 fire zero times on the Flash corpus; they require multi-temporal or FPN data not present in single-snapshot analysis. Triggers T9–T11 fire at meaningful rates (T9: ~207, T10: ~232, T11: ~192 on Flash; lower on Haiku due to different metric distributions). Trigger populations are corpus-dependent; the trigger *definitions* are framework properties.

---

## PATCH 7 — §4.2: Falsifiability criterion for the H¹ gap

**Source:** ChatGPT review — "change the site geometry → the gap should change" as explicit falsifiability criterion. Already present in the eigenvalue reframing draft; this is the placement in §4.2.

**Location:** §4.2, after the paragraph explaining the H¹ gap at 1, 2.

**FIND:**
> This gap is a property of the measurement apparatus (the site geometry), not of the constraints. A richer site with non-linear power relationships could in principle produce H¹ = 1 or 2.

**REPLACE WITH:**
> This gap is a property of the measurement apparatus (the site geometry), not of the constraints. The framework predicts a specific falsification condition: modifying the sigmoid parameters — moving d_institutional toward d_moderate to reduce the sign-flip — would weaken the superselection gap; eliminating the sign-flip entirely would fill it. A richer site with non-linear power relationships could produce H¹ = 1 or 2 by enabling non-adjacent threshold crossings. The gap's dependence on site geometry is itself a testable claim, not an unfalsifiable structural assertion.

---

## PATCH 8 — §4.3: Descent rate range update

**Source:** Cross-corpus comparison. Haiku descent rate = 76.3% (H¹=0).

**Location:** §4.3, wherever the descent rate is reported.

**FIND:**
> The **descent rate** — the fraction of the corpus admitting global sections — is [COMPUTE: H⁰/1142].

**REPLACE WITH:**
> The **descent rate** — the fraction of the corpus admitting global sections — ranges from 20% (Flash corpus) to 76% (Haiku corpus). This wide range is itself informative: the descent rate is a property of the corpus-generator pair, not a framework constant. Models that generate more nuanced constraint stories (with extraction and coordination balanced at similar levels) produce higher gauge-variance; models that generate more polarized stories (constraints that are clearly one thing or another) produce higher gauge-invariance.

---

## PATCH 9 — §4.4: Tangled rope fiber decomposition (DUAL structures) + Institutional dissent binary split

**Source:** Cross-corpus comparison §§5-6. Gap analysis items #1 and #3. This replaces the Claude Code draft, which assumed bimodality was universal.

**Location:** §4.4, after the existing coalition structure discussion.

**INSERT** (new paragraphs):

> **Tangled rope fiber decomposition.** The tangled_rope population — approximately 62% of both corpora after signature integration — admits a continuous decomposition via the MaxEnt snare-lean metric ψ = P(snare) / (P(rope) + P(snare) + ε). The decomposition reveals two qualitatively different internal structures depending on the generating model.
>
> In the Flash corpus (N=549 tangled_ropes), the ψ distribution is strongly bimodal: mass concentrates at ψ ≈ 0 (rope-leaning, 34.6%) and ψ ≈ 1.0 (snare-leaning, 63.6%), with only 10 constraints (1.8%) in the genuinely tangled middle band (0.3 < ψ < 0.7). In the Haiku corpus (N=567), the distribution is unimodal: 416 constraints (73.4%) cluster in the genuinely tangled band, with only 28 rope-leaning (4.9%) and 123 snare-leaning (21.7%). The same framework machinery, applied to corpora from different generators, reveals that Flash constraints tend to be metrically decisive (the classifier is confident about rope or snare direction) while Haiku constraints are metrically ambiguous (the classifier genuinely cannot resolve the direction). Both populations are classified as tangled_rope, but for different reasons: Flash tangled_ropes are held in place by the signature override against a clear metric lean; Haiku tangled_ropes occupy the type because the metrics themselves are balanced.
>
> The decomposition method does not impose the structure it finds. The same ψ calculation on both corpora reveals the corpus-specific metric distribution within the tangled_rope category, validating that the decomposition is diagnostic rather than tautological.
>
> **Institutional dissent: a binary split.** Within the Flash corpus, 246 tangled_rope constraints exhibit institutional dissent — the institutional observer classifies differently from the other three. These split into two clean subpopulations with zero overlap: 213 low-snare constraints (ψ < 0.006, orbit pattern tangled/tangled/rope/tangled — the institution sees coordination others miss) and 33 high-snare constraints (ψ > 0.995, orbit pattern snare/snare/rope/snare — the institution sees coordination that may not exist). The separation is perfect: rank-biserial r = 1.0 on suppression, confidence, and ψ. The Haiku corpus has 40 institutional dissent constraints, 39 of which fall in the low-snare group — too few high-snare cases (N=1) for the binary split to be confirmed as cross-corpus general. Whether the institutional observer's dissent represents informational advantage (seeing real coordination) or metric bias (the sigmoid suppressing real extraction) remains an interpretive question the framework surfaces but does not resolve.

---

## PATCH 10 — New §4.5: Spectral Geometry (eigenvalue reframing + E(C) range)

**Source:** Claude Code v4 additions draft #3 (eigenvalue reframing), cross-corpus E(C) data. Also addresses ChatGPT's falsifiability point.

**Location:** After §4.4, as new §4.5. (Renumber subsequent sections.)

**INSERT new section:**

> ### 4.5 Spectral Geometry
>
> The sheaf Laplacian L₀ on the 4-element linear site has eigenvalues λ = {0, 0.0152, 2.9953, 72.1839}, identical across every corpus analyzed. This identity is a structural theorem, not an empirical discovery: L₀ is fully determined by the restriction map ratios r_ij = σ(π(Uᵢ)) / σ(π(Uⱼ)), which depend only on the sigmoid parameters in the configuration — specifically the canonical directionality values and scope modifiers — not on any property of the corpus data.
>
> The spectral structure reveals the framework's context geometry. The spectral gap λ₂ = 0.0152 is three orders of magnitude below λ₄ = 72.18, with the dominant eigenmode (mode 4, carrying 97% of spectral weight) localized on the institutional vertex (eigenvector loading 0.9927). The institutional observer occupies an isolated eigenspace, effectively decoupled from the other three. The superselection gap (H¹ = 1, 2 empty) is a direct spectral consequence: the institutional vertex contributes either 0 or 3 disagreeing pairs, with intermediate values forbidden by the eigenspace isolation.
>
> The per-constraint obstruction energy E(C) = vᵀL₀v, where v is the constraint's χ vector across four perspectives, correlates with H¹ at a strength that depends on corpus composition: Spearman ρ = 0.66 (p < 10⁻¹¹⁰) on Flash, but only ρ = 0.20 (p = 1.2 × 10⁻⁹) on Haiku. The correlation weakens when most constraints are gauge-invariant (Haiku: 76% at H¹ = 0), because E(C) carries less H¹-discriminating information when most obstruction energies are near zero. The eigenvalues are a framework property; the E(C)–H¹ correlation strength is a corpus property that tracks the gauge-variance rate.
>
> The spectral structure is falsifiable. Changing the sigmoid parameters — moving d_institutional toward d_moderate — would reduce λ₄, broaden the institutional eigenspace, weaken the superselection gap, and eventually fill H¹ = 1 and 2. The framework predicts that its most striking structural invariant (the gap) depends on a specific measurable property (the institutional sign-flip), which can be modified and the consequences observed.

---

## PATCH 11 — §5.1: Confidence band stability as cross-corpus validation

**Source:** Cross-corpus comparison confidence analysis.

**Location:** §5.1, at the end of the STRICT list.

**INSERT** (as new bullet or paragraph):

> - **Confidence band distribution.** The MaxEnt classifier's certainty structure — approximately 75% deep confidence, 5% moderate, 20% borderline — is stable across both corpora despite radically different type distributions and H¹ profiles. Per-type confidence patterns also replicate: mountains at 100% deep, snares at ~89% borderline, scaffolds and pitons at 100% borderline, tangled_ropes at ~91% deep. This stability suggests the confidence structure is a framework property (determined by the MaxEnt parameterization and type-space geometry) rather than a corpus artifact.

---

## PATCH 12 — §7 Conclusion: Copilot's cleaner sheafification framing

**Source:** Copilot review — "the truth of a social system is not the consensus, but the fracture itself."

**Location:** §7, the final sentence.

**FIND:**
> The framework measures perspectival fracture, not to resolve it, but because the fracture itself is where the structural information lives.

**REPLACE WITH:**
> The truth of a social system, on this account, is not the consensus but the fracture itself — the specific pattern of who sees what, from where, and the structural geometry that makes their disagreement inevitable rather than accidental.

---

## PATCH 13 — Abstract: Update corpus size and key numbers

**Location:** Abstract.

The abstract currently references "1,142 social constraints." Update to reflect the two-corpus methodology:

**Key changes:**
- "1,142 social constraints" → "two independently generated corpora (887 and 907 social constraints)"
- Add: "a descent rate ranging from 20% to 76%"
- Add mention of signature attractor convergence
- Keep the 100x oracle gap (still valid)
- Keep "no extractive constraint admits a global section" (confirmed on both)

---

## PATCH 14 — Throughout: [COMPUTE] placeholders

**Note:** The v3 paper has ~15 [COMPUTE] placeholders. These should be filled with ranges from the two corpora rather than single numbers. Key fills:

| Placeholder | Flash value | Haiku value | Recommended text |
|-------------|-------------|-------------|-----------------|
| H⁰/corpus | 181/887 (20.3%) | 692/907 (76.3%) | "20–76%" |
| H¹=3 count | 353 (39.8%) | 64 (7.1%) | "7–40% of corpus" |
| H¹=5 count | 320 (36.1%) | 94 (10.4%) | "10–36%" |
| H¹=6 count | 19 (2.1%) | 45 (5.0%) | "2–5%" |
| {rope, snare} orbit | from orbit reports | from orbit reports | provide ranges |
| False mountain count | 667 | 782 | "667–782" |

---

## Summary: Edit Inventory

| # | Section | Type | ~Words | Source |
|---|---------|------|--------|--------|
| 1 | §1 | INSERT 1 sentence | 50 | Perplexity |
| 2 | §2.1 | REPLACE 1 paragraph | 100 | Gemini + ChatGPT + Perplexity |
| 3 | §2.2 | REPLACE + INSERT | 80 | ε-invariance + Perplexity |
| 4 | §2.4 | INSERT 1 paragraph | 80 | Covering analysis |
| 5 | §2.6 | REPLACE table + INSERT 2 paragraphs | 300 | Cross-corpus attractor |
| 6 | §3.3 | INSERT 1 paragraph | 60 | Cross-corpus triggers |
| 7 | §4.2 | REPLACE 2 sentences | 60 | ChatGPT falsifiability |
| 8 | §4.3 | REPLACE 1 paragraph | 80 | Cross-corpus descent |
| 9 | §4.4 | INSERT 4 paragraphs | 400 | Cross-corpus PSI + dissent |
| 10 | §4.5 | INSERT new section | 300 | Eigenvalue reframing |
| 11 | §5.1 | INSERT 1 bullet | 80 | Confidence stability |
| 12 | §7 | REPLACE 1 sentence | 40 | Copilot |
| 13 | Abstract | REVISE | 50 | Corpus update |
| 14 | Throughout | FILL placeholders | varies | Both corpora |

**Total new material:** ~1,680 words
**Total revised material:** ~340 words
**Net effect on 770-line paper:** approximately +60–80 lines

---

## Items from reviews NOT integrated (and why)

- **Site normativity** (all four flagged): Already in §2.1, strengthened by Patch 2. No additional text needed.
- **H¹ proxy terminology** (all four flagged): The term "H¹ proxy" is accurate and the paper already explains why it's a proxy (§4.2). Changing terminology mid-paper would be more confusing than clarifying.
- **Metric calibration** (all four flagged): §2.6 already addresses this; the sensitivity sweep evidence is adequate. Adding more would be defensive.
- **Eigenvalue identity mechanism** (missed by all four): Patch 10 explicitly addresses this ("structural theorem, not empirical discovery"), which is the correct framing my earlier commentary identified as the one place all four were too generous.
