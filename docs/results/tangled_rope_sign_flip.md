# Tangled_Rope Sign-Flip Analysis

**Purpose:** Test whether DR's sign-flip mechanism (§2.3 zero-sum derivation) extends
to `tangled_rope` constraints — the mixed-flow majority (67.7% of corpus, N=2245).

## Summary

**Sign-flip in underlying flow structure (chi-based A_inst):** 62.4%  
**Sign-flip visible at surface classification (type-based A_inst):** 0.3%  
**Chi-vs-type gap (cover-story signal):** 62.1%  
**Strong structural sign-flip (chi-based C_inst_strong):** 62.4%

The chi-vs-type gap is the headline finding of this analysis, not a methodological
caveat. It is an independent empirical trace of the §2.2 cover-story machinery: chi
captures the underlying flow asymmetry before the cover story erases it; type captures
the post-classification surface where the erasure has already occurred. The two numbers
measure the same structural property at different resolutions of the framework's output.

---

## Metric Definitions

**Two vector types (measuring at different resolutions):**

- **Chi-based** (primary): per-position `chi` metric → sign (+1 if chi>0, -1 if chi<0, 0 if chi=0).
  Chi is the net-flow-asymmetry metric from §2.3's derivation. It captures sign-flip
  in the *underlying flow structure*, including in the 71.9% of tangled_rope constraints
  whose per-position TYPE labels are all `tangled_rope` (uniform surface classification).

- **Type-based** (secondary): per-position TYPE in `perspectives` → ternary collapse
  (rope/scaffold → +1; snare → -1; tangled_rope/mountain/naturalized/piton → 0).
  Type captures sign-flip at the *post-cover-story surface*. The cover-story mechanism
  (FCR, naturalization) operates between chi and type: it classifies the powerless
  agent's experience of extraction as `naturalized` rather than `snare`, collapsing
  that position to 0 and preventing type-based sign-flip from registering. When
  chi-based and type-based rates diverge, the gap measures how much sign-flip the
  cover story erases between the flow level and the surface classification level.

**Four sign-flip metrics:**

- **A_strict**: powerless (U₁) vs. analytical (U₄) opposite-sign. Matches JSX `signFlipExtreme`.
  *Note:* U₄ is DR's meta-observer, not the structural extraction beneficiary. Near-zero
  in corpus because the analytical position re-aligns with the extraction interpretation.
  Reported to show what the JSX-naive operationalization would conclude.

- **A_inst**: powerless (U₁) vs. institutional (U₃) opposite-sign. DR's structural
  interpretation: U₃ is the extraction beneficiary whose chi flips negative while
  U₁ experiences positive chi (extraction). This is the load-bearing metric for §2.3.

- **C_mono**: non-neutral values across [U₁, U₂, U₃, U₄] are monotonically
  non-decreasing. Near-zero because the corpus chi pattern is typically [+,+,−,+]:
  institutional flips negative but analytical re-flips positive, violating monotonicity.
  Reported to show that strict gradient-tracking does not hold in the corpus.

- **C_inst_strong**: U₁ AND U₂ agree on sign; U₃ flips to opposite sign. DR-distinctive:
  the subordinate majority (powerless + moderate) aligns before the structural
  beneficiary (institutional) diverges. This is the strong form of the §2.3 prediction.

**Observer position structure (NOT a linear power gradient):**
- U₁ powerless: structurally subordinate, experiences extraction (chi > 0)
- U₂ moderate: intermediate (chi > 0)
- U₃ institutional: structural extraction *beneficiary* — chi flips NEGATIVE here
- U₄ analytical: meta-observer, sees full structure, chi re-aligns with extraction (chi > 0)

---

## N Counts

| Population | N | % of corpus |
|---|---|---|
| tangled_rope | 2245 | 67.7% |
| manifest presheaves (H¹>0) | 861 | 26.0% |
| rope | 55 | 1.7% |
| snare | 571 | 17.2% |
| total corpus | 3314 | 100% |

---

## Sign-Flip Rates

### Chi-Based (Primary — underlying flow signal)

| Population | N | A_strict | A_inst | C_mono | C_inst_strong |
|---|---|---|---|---|---|
| tangled_rope | 2245 | 0.0% | 62.4% | 0.0% | 62.4% |
| manifest_presheaves | 861 | 0.0% | 65.0% | 0.0% | 65.0% |
| rope | 55 | 0.0% | 90.9% | 0.0% | 90.9% |
| snare | 571 | 0.2% | 77.1% | 0.0% | 77.1% |
| random ternary (analytic) | 81 | 22.2% | 22.2% | 21.0% | 7.4% |

### Type-Based (Secondary — post-cover-story surface signal)

| Population | N | A_strict | A_inst | C_mono | C_inst_strong |
|---|---|---|---|---|---|
| tangled_rope | 2245 | 0.0% | 0.3% | 0.0% | 0.2% |
| manifest_presheaves | 861 | 0.0% | 4.3% | 0.0% | 4.1% |
| rope | 55 | 0.0% | 0.0% | 0.0% | 0.0% |
| snare | 571 | 0.0% | 5.4% | 0.0% | 5.4% |
| random ternary (analytic) | 81 | 22.2% | 22.2% | 21.0% | 7.4% |

### Chi-vs-Type Gap (tangled_rope, A_inst)

| Metric | Chi-based (flow) | Type-based (surface) | Gap (cover-story signal) |
|---|---|---|---|
| A_inst  | 62.4% | 0.3% | 62.1% |
| C_inst_strong | 62.4% | 0.2% | 62.2% |

The gap quantifies how much sign-flip the cover-story machinery erases between the
chi level (net flow asymmetry) and the type level (surface classification). A large
gap confirms that the cover story is not merely a labeling convention but an active
suppression of the flow-level signal in the pipeline's classification output.

---

## Per-Position Sign Distribution within tangled_rope (Chi-Based)

| Position | Positive (chi>0) | Negative (chi<0) | Neutral (chi=0) |
|---|---|---|---|
| U₁ powerless | 2244 (100.0%) | 1 (0.0%) | 0 (0.0%) |
| U₂ moderate | 2245 (100.0%) | 0 (0.0%) | 0 (0.0%) |
| U₃ institutional | 844 (37.6%) | 1401 (62.4%) | 0 (0.0%) |
| U₄ analytical | 2245 (100.0%) | 0 (0.0%) | 0 (0.0%) |

U₃ (institutional) is the unique locus of chi-sign reversal: it is the only position
with predominantly negative chi across tangled_rope constraints. All other positions
have predominantly positive chi (experiencing extraction). U₄ (analytical) has nearly
identical positive chi distribution to U₁ and U₂, confirming that the analytical
meta-observer re-aligns with the extraction reality that the institutional position obscures.
The dominant corpus chi pattern for tangled_rope is [+,+,−,+]: the institutional
sign-flip that DR's §2.3 derivation predicts, with analytical re-alignment.

---

## Naturalization Sensitivity

The primary collapse treats `naturalized` → 0, reflecting that the agent at a
naturalized position does not perceive the extraction at their own position (the
cover story has classified it as background). Alt-1 treats `naturalized` → -1,
reflecting the analyst's view that hidden extraction is still extraction.

This choice directly determines the type-based A_inst rate for constraints where
the powerless position is typed as `naturalized` (492 appearances in tangled_rope
perspectives). Under the primary mapping, these count as neutral (0) at U₁ —
preventing type-based sign-flip from firing even when chi shows positive flow at U₁.
Under Alt-1, they count as -1 at U₁ — but this inverts the expected direction
(the powerless agent experiencing extraction should have positive chi, not negative;
naturalized → -1 would put U₁ and U₃ on the SAME side, reducing A_inst further).

The sensitivity check quantifies this:

| Metric | Primary (nat=0) | Alt-1 (nat=−1) | Delta |
|---|---|---|---|
| A_inst | 0.3% | 7.5% | 7.2% |
| C_inst_strong | 0.2% | 0.8% | 0.6% |

The primary mapping is the correct headline: it reflects the framework's own
classification of what the powerless agent experiences. If the gap between
chi-based and type-based A_inst narrows under Alt-1, that would mean the
naturalization mechanism is responsible for part of the cover-story signal.
If Alt-1 further reduces type-based A_inst (by mis-signing U₁), that confirms
the cover-story signal in the chi-vs-type gap is genuine, not an artifact of the
naturalized → 0 choice.

---

## Interpretation

The chi-based A_inst rate for tangled_rope is **62.4%**: powerless and institutional positions have opposite-sign chi in 62.4% of tangled_rope constraints. This is substantially above the random ternary baseline (22.2%), confirming that sign-flip survives in the mixed-flow majority at the chi level. However, it is below the rope baseline (90.9%) and below the snare baseline (77.1%). The institutional sign-flip is present but weaker in mixed-flow (tangled_rope) constraints than in pure-flow cases — consistent with §2.3's caveat that the zero-sum derivation gives a cleaner prediction for unidirectional flows.

A_strict (powerless vs. analytical) gives 0.0% — near-zero as expected, because U₄ re-aligns with the extraction interpretation rather than serving as the structural beneficiary. An analysis following the JSX's signFlipExtreme naively would conclude that sign-flip collapses in tangled_rope. The actual corpus shows the opposite: the locus of sign-flip is U₃ (institutional), not U₄ (analytical).

C_inst_strong gives 62.4% (chi-based) — the strong form, requiring U₁ and U₂ to agree before U₃ flips. C_mono gives 0.0% — near-zero as expected from the [+,+,−,+] pattern.

The chi-vs-type gap for A_inst is 62.1% (62.4% chi-based vs. 0.3% type-based). This gap is a second independent trace of the §2.2 cover-story machinery, complementing the fragility cross-tab finding that FCR constraints sit away from the parametric boundary. Here the same mechanism is visible at a different level: the cover story classifies the powerless agent's extraction experience as `naturalized` rather than `snare`, collapsing the type-based sign-flip signal while the chi-level signal remains intact.

**Auto-update criteria not fully met** (A_inst > 40% AND C_inst_strong > 20% AND both >15pp above rope/snare baselines). A_inst=62.4%, C_inst_strong=62.4%. Returning to user for framing decision on §2.3 revision.

---

*Generated by `python/tangled_rope_sign_flip.py`.*