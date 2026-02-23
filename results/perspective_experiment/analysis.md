# LLM Presheaf Diagnostic — Analysis Report

**Generated:** reconstructed from JSON scan
**Constraints:** 5
**Perspectives:** ['u1', 'u2', 'u3', 'u4']
**Framing:** experiential, structural
**Runs:** 5
**Model:** unknown (reconstructed)

**Results:** 86/86 generations succeeded, 0 lint failures, 0 reports generated
**Total tokens:** 0 -> 0
**Duration:** 0.0s

---

## 1. Epsilon Drift (delta_epsilon)

The headline test: does `delta_epsilon = max(eps) - min(eps)` across perspectives
exceed the stochastic baseline?

| Constraint | U1 mean | U2 mean | U3 mean | U4 mean | delta_eps | Status |
|---|---|---|---|---|---|---|
| 26usc469_real_estate_exemption | 0.654 | 0.642 | 0.663 | 0.668 | 0.0260 | STABLE |
| academic_peer_review_gatekeeping | 0.760 | 0.680 | 0.709 | 0.720 | 0.0800 | DRIFT |
| antifragility | 0.750 | 0.750 | 0.750 | 0.750 | 0.0000 | STABLE |
| epistemic_process_of_verification | 0.474 | 0.494 | 0.530 | 0.530 | 0.0560 | DRIFT |
| subscription_economy_model | 0.680 | 0.680 | 0.680 | 0.680 | 0.0000 | STABLE |

**Aggregate:** mean delta_epsilon = 0.0324, max = 0.0800, 2/5 constraints show drift > 0.05

**Directional test (sympathy bias):** epsilon_U1 > epsilon_U4 in 1 constraints, epsilon_U4 > epsilon_U1 in 2

### Variance Decomposition (ANOVA-style)

Tests whether between-perspective variance exceeds within-perspective (stochastic) variance. F > 1 suggests a perspective effect beyond noise.

| Constraint | Within-var | Between-var | F-ratio | Signal? |
|---|---|---|---|---|
| 26usc469_real_estate_exemption | 0.002069 | 0.000130 | 0.063 | NO |
| academic_peer_review_gatekeeping | 0.001845 | 0.001099 | 0.596 | NO |
| antifragility | 0.000000 | 0.000000 | 0.000 | NO |
| epistemic_process_of_verification | 0.001957 | 0.000772 | 0.395 | NO |
| subscription_economy_model | 0.000000 | 0.000000 | 0.000 | NO |

**0/5 constraints show F > 1** (between-perspective variance exceeds within-perspective noise).

Note: With k=4 groups and n=2-5 per group, critical F(3, ~16) at p=0.05 is approximately 3.24. F-ratios below this are not statistically significant.

---

## 2. Classification Matrices (4x4)

### 26usc469_real_estate_exemption

| Story \ Eval | U1 | U2 | U3 | U4 |
|---|---|---|---|---|
| u1_exp_r1 | snare | - | rope | tangled_rope |
| u1_exp_r2 | snare | rope | tangled_rope | tangled_rope |
| u1_exp_r3 | snare | rope | tangled_rope | tangled_rope |
| u1_exp_r4 | snare | - | rope | tangled_rope |
| u1_exp_r5 | snare | rope | tangled_rope | tangled_rope |
| u2_exp_r1 | snare | rope | tangled_rope | tangled_rope |
| u2_exp_r2 | snare | rope | tangled_rope | tangled_rope |
| u2_exp_r3 | snare | rope | tangled_rope | tangled_rope |
| u2_exp_r4 | snare | rope | tangled_rope | tangled_rope |
| u2_exp_r5 | snare | rope | tangled_rope | tangled_rope |
| u3_exp_r1 | snare | - | rope | tangled_rope |
| u3_exp_r2 | snare | rope | tangled_rope | tangled_rope |
| u3_exp_r3 | snare | rope | tangled_rope | tangled_rope |
| u3_exp_r4 | snare | rope | tangled_rope | tangled_rope |
| u3_exp_r5 | snare | rope | tangled_rope | tangled_rope |
| u3_str_r1 | snare | rope | tangled_rope | tangled_rope |
| u3_str_r2 | snare | rope | tangled_rope | tangled_rope |
| u4_exp_r1 | snare | - | rope | tangled_rope |
| u4_exp_r2 | snare | rope | tangled_rope | tangled_rope |
| u4_exp_r3 | snare | rope | tangled_rope | tangled_rope |
| u4_exp_r4 | snare | - | rope | tangled_rope |
| u4_exp_r5 | snare | - | rope | tangled_rope |

### academic_peer_review_gatekeeping

| Story \ Eval | U1 | U2 | U3 | U4 |
|---|---|---|---|---|
| u1_exp_r1 | snare | - | rope | tangled_rope |
| u1_exp_r2 | snare | - | rope | tangled_rope |
| u1_exp_r3 | snare | - | rope | tangled_rope |
| u1_exp_r4 | snare | - | rope | tangled_rope |
| u1_exp_r5 | snare | - | rope | tangled_rope |
| u2_exp_r1 | snare | - | rope | tangled_rope |
| u2_exp_r2 | snare | - | rope | tangled_rope |
| u2_exp_r3 | snare | - | rope | tangled_rope |
| u2_exp_r4 | snare | tangled_rope | rope | tangled_rope |
| u2_exp_r5 | snare | - | rope | tangled_rope |
| u3_exp_r1 | snare | - | rope | tangled_rope |
| u3_exp_r2 | snare | - | rope | tangled_rope |
| u3_exp_r3 | snare | - | rope | tangled_rope |
| u3_exp_r4 | snare | - | rope | tangled_rope |
| u3_exp_r5 | snare | - | rope | tangled_rope |
| u3_str_r1 | snare | - | rope | tangled_rope |
| u3_str_r2 | snare | - | rope | tangled_rope |
| u4_exp_r1 | snare | - | rope | tangled_rope |
| u4_exp_r2 | snare | - | rope | tangled_rope |
| u4_exp_r3 | snare | - | rope | tangled_rope |
| u4_exp_r4 | snare | - | rope | tangled_rope |
| u4_exp_r5 | snare | - | rope | tangled_rope |

### antifragility

| Story \ Eval | U1 | U2 | U3 | U4 |
|---|---|---|---|---|
| u1_exp_r1 | snare | rope | tangled_rope | mountain |
| u1_exp_r2 | snare | rope | tangled_rope | mountain |
| u2_exp_r1 | snare | rope | tangled_rope | mountain |
| u2_exp_r2 | snare | rope | tangled_rope | mountain |
| u3_exp_r1 | snare | rope | tangled_rope | mountain |
| u3_exp_r2 | snare | rope | tangled_rope | mountain |
| u3_str_r1 | snare | rope | tangled_rope | mountain |
| u3_str_r2 | snare | rope | tangled_rope | tangled_rope |
| u4_exp_r1 | snare | rope | tangled_rope | mountain |
| u4_exp_r2 | snare | rope | tangled_rope | mountain |

### epistemic_process_of_verification

| Story \ Eval | U1 | U2 | U3 | U4 |
|---|---|---|---|---|
| u1_exp_r1 | snare | tangled_rope | rope | tangled_rope |
| u1_exp_r2 | snare | - | rope | tangled_rope |
| u1_exp_r3 | snare | - | rope | tangled_rope |
| u1_exp_r4 | snare | - | rope | tangled_rope |
| u1_exp_r5 | snare | - | snare | tangled_rope |
| u2_exp_r1 | snare | - | rope | tangled_rope |
| u2_exp_r2 | snare | snare | rope | tangled_rope |
| u2_exp_r3 | snare | - | snare | tangled_rope |
| u2_exp_r4 | snare | tangled_rope | rope | tangled_rope |
| u2_exp_r5 | snare | snare | rope | tangled_rope |
| u3_exp_r1 | snare | tangled_rope | rope | tangled_rope |
| u3_exp_r2 | snare | scaffold | rope | tangled_rope |
| u3_exp_r3 | snare | - | rope | tangled_rope |
| u3_exp_r4 | snare | tangled_rope | rope | tangled_rope |
| u3_exp_r5 | snare | - | rope | tangled_rope |
| u3_str_r1 | snare | rope | rope | tangled_rope |
| u3_str_r2 | snare | scaffold | rope | tangled_rope |
| u4_exp_r1 | snare | rope | rope | tangled_rope |
| u4_exp_r2 | snare | rope | rope | tangled_rope |
| u4_exp_r3 | snare | piton | rope | tangled_rope |
| u4_exp_r4 | snare | - | rope | tangled_rope |
| u4_exp_r5 | snare | snare | rope | tangled_rope |

### subscription_economy_model

| Story \ Eval | U1 | U2 | U3 | U4 |
|---|---|---|---|---|
| u1_exp_r1 | snare | - | rope | tangled_rope |
| u1_exp_r2 | snare | - | rope | tangled_rope |
| u2_exp_r1 | snare | - | rope | tangled_rope |
| u2_exp_r2 | snare | - | rope | tangled_rope |
| u3_exp_r1 | snare | - | rope | tangled_rope |
| u3_exp_r2 | snare | - | rope | tangled_rope |
| u3_str_r1 | snare | - | rope | tangled_rope |
| u3_str_r2 | snare | tangled_rope | rope | tangled_rope |
| u4_exp_r1 | snare | - | rope | tangled_rope |
| u4_exp_r2 | snare | - | rope | tangled_rope |

---

### Classification Stability (per evaluation context)

Measures what fraction of stories receive the same type at each evaluation context. Stability = 1.0 means all stories agree. 'Chaos' flags columns with 3+ distinct types.

| Constraint | U1 | U2 | U3 | U4 | Overall |
|---|---|---|---|---|---|
| 26usc469_real_estate_exemption | 1.00 (snare) | 1.00 (rope) | 0.73 (tangled_rope) | 1.00 (tangled_rope) | 0.932 |
| academic_peer_review_gatekeeping | 1.00 (snare) | 1.00 (tangled_rope) | 1.00 (rope) | 1.00 (tangled_rope) | 1.000 |
| antifragility | 1.00 (snare) | 1.00 (rope) | 1.00 (tangled_rope) | 0.90 (mountain) | 0.975 |
| epistemic_process_of_verification | 1.00 (snare) | 0.31 (tangled_rope) **CHAOS** | 0.91 (rope) | 1.00 (tangled_rope) | 0.804 |
| subscription_economy_model | 1.00 (snare) | 1.00 (tangled_rope) | 1.00 (rope) | 1.00 (tangled_rope) | 1.000 |

**Chaotic columns** (3+ distinct types — model doesn't know what this constraint is here):

- **epistemic_process_of_verification** at **U2**: tangled_rope:4, snare:3, scaffold:2, rope:3, piton:1

---

## 3. Gate Flip Rates (Boolean Structural Properties)

### academic_peer_review_gatekeeping

- **mandatrophy_resolved**: {'u1': True, 'u2': False, 'u3': True, 'u4': False}

---

## 4. Cross-Run Consistency

**Threshold:** delta > 0.05
**Total pairs:** 25
**Flagged:** 12 (48.0%)

| Key | Runs | Delta |
|---|---|---|
| 26usc469_real_estate_exemption_u1_exp | r1=0.680, r2=0.680, r3=0.680, r4=0.550, r5=0.680 | 0.1300 |
| 26usc469_real_estate_exemption_u2_exp | r1=0.680, r2=0.550, r3=0.620, r4=0.680, r5=0.680 | 0.1300 |
| 26usc469_real_estate_exemption_u3_exp | r1=0.680, r2=0.620, r3=0.680, r4=0.680, r5=0.620 | 0.0600 |
| 26usc469_real_estate_exemption_u4_exp | r1=0.680, r2=0.680, r3=0.680, r4=0.620, r5=0.680 | 0.0600 |
| academic_peer_review_gatekeeping_u1_exp | r1=0.780, r2=0.780, r3=0.680, r4=0.780, r5=0.780 | 0.1000 |
| academic_peer_review_gatekeeping_u3_exp | r1=0.780, r2=0.680, r3=0.680, r4=0.680, r5=0.680 | 0.1000 |
| academic_peer_review_gatekeeping_u3_str | r1=0.680, r2=0.780 | 0.1000 |
| academic_peer_review_gatekeeping_u4_exp | r1=0.680, r2=0.680, r3=0.680, r4=0.780, r5=0.780 | 0.1000 |
| epistemic_process_of_verification_u1_exp | r1=0.550, r2=0.480, r3=0.380, r4=0.480, r5=0.480 | 0.1700 |
| epistemic_process_of_verification_u2_exp | r1=0.480, r2=0.480, r3=0.480, r4=0.550, r5=0.480 | 0.0700 |
| epistemic_process_of_verification_u3_exp | r1=0.550, r2=0.480, r3=0.550, r4=0.480, r5=0.550 | 0.0700 |
| epistemic_process_of_verification_u4_exp | r1=0.550, r2=0.550, r3=0.450, r4=0.550, r5=0.550 | 0.1000 |

**WARNING:** More than 30% of condition-pairs exceed the consistency threshold.
The stochastic noise floor may be too high for this experiment as designed.
Consider additional runs.

---

## 5. Missing Cells

*All cells present — no missing data.*

---

## 6. Power Atom Usage (Perspective Coverage)

**Stories analyzed:** 86
**Stories missing `moderate` power atom:** 54/86 (63%)

**Power atom frequency across all story perspectives:**

| Power Atom | Count |
|---|---|
| institutional (canonical) | 105 |
| powerless (canonical) | 86 |
| analytical (canonical) | 86 |
| moderate (canonical) | 32 |
| organized | 22 |
| powerful | 9 |

**Perspective count per story:**

- 3 perspectives: 4 stories
- 4 perspectives: 82 stories

**FINDING:** The LLM systematically skips the `moderate` (U2) observer position. It prefers the three structurally extreme positions: powerless, institutional, analytical. The U2 column dashes in classification matrices above reflect genuine missing perspectives in the generated stories, not analysis artifacts.

---

## 7. Framing Comparison (Experiential vs Structural)

Direct comparison of experiential vs structural framing for overlapping perspective × constraint cells. The key question: does structural framing produce the `moderate` power atom more reliably (is the missing middle a framing effect or a capacity limitation)?

| Constraint | Persp | Exp ε (n) | Str ε (n) | Δε | Exp mod% | Str mod% | Exp type | Str type | Match? |
|---|---|---|---|---|---|---|---|---|---|
| 26usc469_real_estate_exemption | u3 | 0.656 (5) | 0.680 (2) | 0.024 | 80% | 100% | tangled_rope | tangled_rope | YES |
| academic_peer_review_gatekeeping | u3 | 0.700 (5) | 0.730 (2) | 0.030 | 0% | 0% | tangled_rope | tangled_rope | YES |
| antifragility | u3 | 0.750 (2) | 0.750 (2) | 0.000 | 100% | 100% | tangled_rope | tangled_rope | YES |
| epistemic_process_of_verification | u3 | 0.522 (5) | 0.550 (2) | 0.028 | 20% | 0% | tangled_rope | tangled_rope | YES |
| subscription_economy_model | u3 | 0.680 (2) | 0.680 (2) | 0.000 | 0% | 50% | tangled_rope | tangled_rope | YES |

**Aggregate moderate rate:** experiential = 40%, structural = 50%
**Type agreement:** 100% of cells match across framings

**FINDING:** Structural framing does NOT significantly change moderate atom production. The missing middle appears to be a **capacity limitation** — the LLM struggles to conceptualize a moderate observer position regardless of how it's framed.

---

## 8. Linter Failure Patterns

*No linter pattern data available.*

---

*Analysis generated by `python/perspective_analysis.py`*
