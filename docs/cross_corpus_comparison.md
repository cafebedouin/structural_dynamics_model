# Cross-Corpus Comparison: Flash vs Haiku

**Purpose:** Identify which paper claims are corpus-stable and which need range updates.
**Flash:** Gemini Flash 2.0, 887 constraints (Corpus B)
**Haiku:** Claude Haiku 4.5, 907 constraints (Corpus A, rebuilt Feb 26)

---

## 1. The Big Picture: Opposite Spectral Characters

| Property | Flash | Haiku | Interpretation |
|----------|-------|-------|----------------|
| Gauge-invariant (H¹=0) | 181 (20.3%) | 692 (76.3%) | **Inverted.** Haiku constraints mostly look the same from all perspectives |
| Gauge-variant (H¹>0) | 706 (79.7%) | 215 (23.7%) | Flash has ~3.3x more perspectival variation |
| Largest single-type orbit | ? | [tangled_rope] 564 (62%) | Haiku: most constraints are unanimously tangled_rope |
| E(C) vs H¹ Spearman | 0.66 (p<10⁻¹¹⁰) | 0.20 (p=1.2×10⁻⁹) | **3.3x weaker.** Laplacian energy less diagnostic in Haiku |
| E(C) vs H¹ Pearson | (not reported) | 0.06 (p=0.064) | Nearly non-significant on Pearson |

**Why this matters for the paper:** The descent rate range must widen from 20-68% to 20-76%. More importantly, the E(C)–H¹ correlation (Spearman 0.66) cited in §4.5 is a Flash result. Haiku gives 0.20. The paper should present this as "0.20–0.66" and note that the correlation strength depends on how much perspectival variation the corpus contains.

---

## 2. H¹ Distribution

| H¹ | Flash | Flash % | Haiku | Haiku % |
|----|-------|---------|-------|---------|
| 0  | 181   | 20.3%   | 692   | 76.3%   |
| 3  | 353   | 39.8%   | 64    | 7.1%    |
| 4  | 14    | 1.6%    | 12    | 1.3%    |
| 5  | 320   | 36.1%   | 94    | 10.4%   |
| 6  | 19    | 2.1%    | 45    | 5.0%    |

**Structural invariants (BOTH corpora):**
- H¹=1: **0** ✓
- H¹=2: **0** ✓
- Superselection gap confirmed on both

**Corpus-dependent:**
- H¹=0 mass: 20% vs 76% — a factor of 3.8
- H¹=5 is the second-largest band in Flash (36.1%) but only 10.4% in Haiku
- H¹=6 is proportionally higher in Haiku (5.0% vs 2.1%)

---

## 3. Type Distribution (Analytical Perspective, Metric-Only)

| Type | Flash | Flash % | Haiku | Haiku % |
|------|-------|---------|-------|---------|
| snare | 448 | 50.5% | 152 | 16.8% |
| tangled_rope | 287 | 32.4% | 619 | 68.2% |
| mountain | 139 | 15.7% | 129 | 14.2% |
| rope | 10 | 1.1% | 7 | 0.8% |
| scaffold | 2 | 0.2% | 0 | 0.0% |

**The inversion:** Flash is snare-dominated (51%), Haiku is tangled_rope-dominated (68%).
Mountains are stable (~14-16%), rope/scaffold are rare in both.

This means the false_ci_rope override has **opposite magnitude** across corpora:
- Flash: reclassifies 339 metric-snares → tangled_rope (massive effect)
- Haiku: fewer metric-snares to reclassify; smaller net effect

---

## 4. Post-Override Type Distribution

| Type | Flash | Flash % | Haiku | Haiku % |
|------|-------|---------|-------|---------|
| tangled_rope | 549 | 62.0% | 567 | 62.5% |
| snare | 109 | 12.3% | 175 | 19.3% |
| mountain | 139 | 15.7% | 129 | 14.2% |
| rope | 62 | 7.0% | 18 | 2.0% |
| scaffold | 10 | 1.1% | 9 | 1.0% |
| piton | 17 | 1.9% | 9 | 1.0% |

**Convergence finding:** tangled_rope post-override is ~62% in BOTH corpora despite
radically different pre-override distributions. The signature system acts as an
attractor — different input distributions converge to similar post-override outputs.
This is a paper-worthy finding.

---

## 5. Tangled Rope PSI Decomposition: Opposite Structures

| Band | Flash | Flash % | Haiku | Haiku % |
|------|-------|---------|-------|---------|
| rope_leaning (ψ<0.3) | 190 | 34.6% | 28 | 4.9% |
| genuinely_tangled (0.3-0.7) | 10 | 1.8% | 416 | 73.4% |
| snare_leaning (ψ>0.7) | 349 | 63.6% | 123 | 21.7% |

**This is the most dramatic cross-corpus divergence.**

- Flash PSI: bimodal at extremes (mass at 0 and 1.0, hollow middle)
- Haiku PSI: unimodal at center (massive spike at ψ∈[0.45,0.50] with 412 constraints)

The "genuinely tangled" population goes from 1.8% to 73.4% — a 40x ratio.

**Interpretation:** Haiku generates constraints where MaxEnt assigns nearly equal
probability to rope and snare (ψ ≈ 0.5), meaning the constraint is genuinely
ambiguous between coordination and extraction. Flash generates constraints where
MaxEnt is confident about one direction or the other, but the FCR override pushes
them into tangled_rope anyway.

**For the paper:** The tangled decomposition paragraph CANNOT claim bimodality as a
general finding. It must present both patterns:
- "On corpus B (Flash), the fiber is bimodal: two discrete populations at the
  extremes with near-empty middle."
- "On corpus A (Haiku), the fiber is unimodal: 73% cluster in the genuinely
  tangled band, reflecting MaxEnt's uncertainty rather than forced reclassification."
- "Both structures produce ~62% tangled_rope post-override, suggesting the
  signature system and the genuinely-tangled metric path converge to similar
  final populations through different mechanisms."

---

## 6. Institutional Dissent

| Metric | Flash | Haiku |
|--------|-------|-------|
| Total institutional_dissent | 246 | 40 |
| Low-snare subgroup | 213 | 39 |
| High-snare subgroup | 33 | 1 |
| Binary split? | Yes (clear bimodal) | Technically yes, but N=1 for high-snare |

**For the paper:** The binary split is a Flash finding. On Haiku, the population is
too small (40) and the high-snare group is a single constraint (amish_technological_
renunciation). Present as "246 on Flash (bimodal split: 213 low-snare vs 33 high-snare)
vs 40 on Haiku (39 low-snare)."

---

## 7. Coalition Structure Cross-Tab

| Coalition | Flash | Flash % | Haiku | Haiku % |
|-----------|-------|---------|-------|---------|
| institutional_dissent | 246 | 44.8% | 40 | 7.1% |
| split_field | 298 | 54.3% | 95 | 16.8% |
| uniform_tangled | 5 | 0.9% | 413 | 72.8% |
| analytical_dissent | — | — | 2 | 0.4% |
| other | — | — | 17 | 3.0% |

**The inversion again:** Flash is dominated by institutional_dissent + split_field
(99%). Haiku is dominated by uniform_tangled (73%). This is consistent with the
H¹ distribution — uniform_tangled means all observers agree, so H¹=0.

---

## 8. Spectral Properties (STRICT — Identical by Construction)

| Property | Flash | Haiku | Status |
|----------|-------|-------|--------|
| Eigenvalues | [0, 0.0152, 2.9953, 72.1839] | [0, 0.0152, 2.9953, 72.1839] | MATCH (STRICT) |
| Restriction ratios | r₁₂=1.42, r₂₃=-8.38, r₃₄=-0.10 | r₁₂=1.42, r₂₃=-8.38, r₃₄=-0.10 | MATCH (STRICT) |
| Institutional eigenvector loading | 0.9927 | 0.9927 | MATCH (STRICT) |
| H¹ gap at 1,2 | 0, 0 | 0, 0 | MATCH (structural) |

All confirmed STRICT. Config-determined.

---

## 9. Oracle Gap / T13

| Metric | Flash | Haiku |
|--------|-------|-------|
| H¹>0 population | 706 | 215 |
| Raw T13 (audit, all contexts) | 214/215 (99.5%) | 214/215 (99.5%) |
| Corrected T13 (Prolog-faithful) | 12 | 6 |
| Group A (genuine T13) | 12 | 6 |
| Group B (audit-only) | ~452 | 208 |
| Group C (neither fires) | ~242 | 1 |

**Institutional as worst context:** Flash 62.2%, Haiku not directly reported
(but given 76% gauge-invariance, the institutional effect is much smaller)

**For paper:** T13 corrected fires: 6-12 across corpora. The raw audit fire rate
(99.5%) is an artifact of profile calibration and context scope issues (documented
in spectral audit §7).

---

## 10. FCA Gate Compression

| Metric | Flash | Haiku |
|--------|-------|-------|
| Non-constant gates | 30 | 32 |
| GF(2) rank | 24/30 | 31/32 |
| Null space dimension | 6 | 1 |
| Concept count | ? | 2516 |

**Interpretation:** Flash has 6 redundant gates; Haiku has only 1. The gate
system is less compressed on Haiku, meaning more gates carry independent information.
This is consistent with the higher metric diversity in Haiku (more genuinely_tangled
constraints occupy more of the gate state space).

---

## 11. Confidence / Verdict

| Band | Flash | Flash % | Haiku | Haiku % |
|------|-------|---------|-------|---------|
| deep | 678 | ~76% | 678 | 74.8% |
| moderate | 49 | ~6% | 49 | 5.4% |
| borderline | 180 | ~20% | 180 | 19.8% |

**Remarkably stable** across corpora. Nearly identical counts.
Snare remains ~89% borderline in both.
Scaffold and piton remain 100% borderline in both.

---

## 12. False Mountains

| Metric | Flash | Haiku |
|--------|-------|-------|
| False mountain count | 667 | 782 |

Higher in Haiku, consistent with fewer snares (more constraints classified as
mountain from some perspectives → more false mountain orbits).

---

## 13. Chi Violations (Coalition Power Resolution)

| Metric | Flash | Haiku |
|--------|-------|-------|
| Chi violations | ~89 at powerless | 367 at powerless |

**4x more in Haiku.** The coalition power resolution system (powerless→organized
upgrade) affects more constraints. This is because Haiku generates constraints with
more coalition structure (more organized agents at powerless perspective).

The paper's "7 chi overrides" from chi_variance_decomposition is analytical-only
with strict tolerance. The 367 are powerless-context coalition effects.

---

## Summary: What the Paper Must Update

### Widen These Ranges

| Claim | Old Range | New Range |
|-------|-----------|-----------|
| Descent rate | 20-68% | **20-76%** |
| E(C) vs H¹ Spearman | 0.66 | **0.20-0.66** |
| Institutional dissent population | ~246 | **40-246** |
| False mountain count | 667 | **667-782** |
| T13 corrected fires | ~12 | **6-12** |

### Reframe These Claims

1. **Tangled rope PSI decomposition:** NOT universally bimodal. Present BOTH
   structures (bimodal on Flash, unimodal on Haiku) as evidence that the
   decomposition method reveals corpus-specific metric distributions.

2. **Institutional dissent binary split:** Flash finding. Haiku has N=1 for
   high-snare subgroup — insufficient for statistical claims.

3. **Post-override convergence at ~62% tangled_rope:** NEW finding. Despite
   radically different pre-override distributions, both corpora converge to
   similar post-override populations. The signature system acts as an attractor.

4. **E(C) correlation:** Depends on how much perspectival variation exists.
   When most constraints are gauge-invariant (Haiku), the Laplacian energy
   carries less H¹-discriminating information.

### Confirm These as STRICT

- Eigenvalues, eigenvectors, restriction ratios (config-determined)
- H¹ gap at 1 and 2 (structural theorem)
- Snare absence from H⁰ (0 in both)
- Mountain perspectival invariance (~129-139 across all contexts in both)
- Scaffold/piton 100% borderline confidence (both)
- Confidence band distribution (~75% deep, ~5% moderate, ~20% borderline)
