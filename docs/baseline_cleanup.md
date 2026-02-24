# Baseline Cleanup Report

**Date:** 2026-02-24
**Purpose:** Clear final actionable issues before tagging v3-dev-baseline

---

## Part 1: Reclassifications (tangled_rope → rope)

Four constraints identified by `rope_dominant_spot_check.py` with `recommendation == "reclassify"` (epsilon ≤ `reclassify_epsilon_ceiling` of 0.10, all perspectives agree on rope).

| Constraint ID | ε | File | Status |
|---|---|---|---|
| e2ee_digital_privacy_2026 | 0.05 | `prolog/testsets/e2ee_digital_privacy_2026.pl` | Done |
| platform_cooperativism_governance | 0.05 | `prolog/testsets/platform_cooperativism_governance.pl` | Done |
| legacy_system_technical_debt | 0.03 | `prolog/testsets/legacy_system_technical_debt.pl` | Done |
| public_domain_commons | 0.00 | `prolog/testsets/public_domain_commons.pl` | Done |

**Change:** `constraint_claim(ID, tangled_rope)` → `constraint_claim(ID, rope)` in each file.

**Safety checks:**
- No overlap with previous reclassification set (portuguese_presidential_term_limits, thai_article_112_mountain, sts86_ascent_checklist, decentralized_infrastructure_rope)
- All 4 files pass `swipl` syntax verification
- `constraint_classification/3` facts updated by testset rebuild (Part 3)

---

## Part 2: Semantic Duplicate Analysis

**Pair:** `continuum_hypothesis_undecidability.pl` vs `suslin_hypothesis_undecidability.pl`
**Similarity:** 0.836 | **ε difference:** 0.03

### Side-by-Side Comparison

| Aspect | CH | SH |
|---|---|---|
| **Human readable** | Undecidability of the Continuum Hypothesis in ZFC | Undecidability of Suslin's Hypothesis in ZFC |
| **Mathematical object** | Cardinality hierarchy (size of infinite sets) | Topological uniqueness (structure of ordered lines) |
| **Proof timeline** | Gödel/Cohen 1940–1963 | Solovay/Tennenbaum 1971 |
| **ε** | 0.02 | 0.05 |
| **suppression** | 0.01 | 0.00 |
| **theater_ratio** | 0.00 | 0.00 |
| **claimed_type** | mountain | mountain |
| **All perspectives** | mountain (4/4) | mountain (4/4) |
| **signature** | natural_law | natural_law |
| **emerges_naturally** | true | true |
| **requires_active_enforcement** | false | false |
| **beneficiaries** | model_theorists | (none listed) |
| **victims** | hilbertian_formalists | (none listed) |
| **topic_domain** | mathematical/logical | mathematical/logical |

### Decision: Keep Both (Intentional Variants)

Despite high lexical similarity (0.836), these encode structurally distinct constraints:

1. **CH** concerns cardinality independence — whether a set exists between ℵ₀ and the continuum. The extraction is the loss of completeness in the deductive universe.
2. **SH** concerns topological uniqueness — whether any countable-chain-condition linear order is isomorphic to ℝ. The extraction is the loss of a singular continuum definition.

Both are NL-certified Mountains (accessibility_collapse = 1.0, resistance = 0.0) because they are pure logical limits — metric similarity is expected and structurally appropriate.

**No files modified.**

---

## Part 3: Pipeline Re-run + Delta Reconciliation

### Pipeline
Full 8-phase pipeline completed: **36/36 steps OK** in 52.6s.

### Testset Rebuild
5 classification fact mismatches found and resolved:

| Constraint | Perspective | Old | New |
|---|---|---|---|
| academic_peer_review_gatekeeping | analytical | snare | tangled_rope |
| castration_longevity_choice | analytical | snare | tangled_rope |
| happiness_of_others | analytical | snare | tangled_rope |
| knowledge_action_gap | analytical | snare | tangled_rope |
| neuroplasticity_plateau | analytical | snare | tangled_rope |

All 5 are cascade effects from the refreshed engine state (analytical perspective reclassifications), not direct consequences of the 4 claim changes. All 5 files passed swipl verification. Engine agreement: 100% (20/20 sample).

4 test body references also updated (castration_longevity_choice, happiness_of_others, neuroplasticity_plateau ×2).

### Downstream Analyses
All three completed without errors:
- `tangled_gradient.py`: 502 tangled_rope constraints decomposed
- `chi_variance_decomposition.py`: 502 constraints analyzed
- `rope_dominant_spot_check.py`: 36 rope-dominant constraints checked

---

## Part 4: Baseline Verification

### Checklist

| Check | Result |
|---|---|
| `testset_rebuild.py --dry-run` → 0 mismatches | **PASS** (0 mismatched facts) |
| `rope_dominant_spot_check.py` → 0 reclassify-tier | **PASS** (0 reclassify, 27 keep, 9 investigate) |
| Consistent tangled_rope pool across analyses | **PASS** (502 in all three) |
| Dedup resolved | **PASS** (1 semantic pair → keep both) |
| Engine agreement | **PASS** (100%) |

### Final Corpus Profile

| Type | Count | % |
|---|---:|---:|
| tangled_rope | 502 | 43.6% |
| snare | 363 | 31.5% |
| mountain | 127 | 11.0% |
| piton | 76 | 6.6% |
| rope | 58 | 5.0% |
| scaffold | 22 | 1.9% |
| unknown | 2 | 0.2% |
| [social_governance] | 1 | 0.1% |
| **Total** | **1151** | |

### Tangled Rope Subtype Distribution

| Subtype | Count | % |
|---|---:|---:|
| genuinely_perspectival | 402 | 80.1% |
| structurally_ambiguous | 62 | 12.4% |
| rope_dominant | 36 | 7.2% |
| snare_dominant | 2 | 0.4% |
| **Total** | **502** | |

### Delta from Previous Baseline

| Metric | Before | After | Δ |
|---|---:|---:|---:|
| tangled_rope | 506 | 502 | -4 |
| rope | 54 | 58 | +4 |
| rope_dominant (subtype) | 40 | 36 | -4 |
| reclassify-tier | 4 | 0 | -4 |

---

## Files Modified

**Claim edits (4):**
- `prolog/testsets/e2ee_digital_privacy_2026.pl`
- `prolog/testsets/platform_cooperativism_governance.pl`
- `prolog/testsets/legacy_system_technical_debt.pl`
- `prolog/testsets/public_domain_commons.pl`

**Classification reconciliation (5):**
- `prolog/testsets/academic_peer_review_gatekeeping.pl`
- `prolog/testsets/castration_longevity_choice.pl`
- `prolog/testsets/happiness_of_others.pl`
- `prolog/testsets/knowledge_action_gap.pl`
- `prolog/testsets/neuroplasticity_plateau.pl`

**Regenerated outputs:**
- All `outputs/` artifacts (pipeline, enrichment, downstream analyses)
- `docs/tangled_gradient_analysis.md`
- `docs/chi_variance_decomposition.md`
- `docs/rope_dominant_spot_check.md`
- `docs/testset_rebuild_dedup.md`

---

## Corpus Status

Zero known actionable issues. Ready for `git tag v3-dev-baseline`.
