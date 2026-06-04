# Audit 3 T-E Covariation Robustness Check

## Summary

n_internal (total): 81
n_e_diff_0 (exit options match): 29
n_e_diff_1 (exit options differ): 52

## Metric A: Extractive fraction

| Cell | n | T partial ρ | T partial p | T zero-order ρ |
|---|---|---|---|---|
| E_diff=0 (E fixed) | 29 | 0.855 | 0.000 | 0.866 |
| E_diff=1 (E varies) | 52 | 0.282 | 0.043 | 0.288 |

**Verdict: robust**

## Metric B: Type entropy

| Cell | n | T partial ρ | T partial p | T zero-order ρ |
|---|---|---|---|---|
| E_diff=0 (E fixed) | 29 | 0.440 | 0.017 | 0.429 |
| E_diff=1 (E varies) | 52 | 0.616 | 0.000 | 0.627 |

**Verdict: robust**

## Metric D: Total variation distance

| Cell | n | T partial ρ | T partial p | T zero-order ρ |
|---|---|---|---|---|
| E_diff=0 (E fixed) | 29 | 0.865 | 0.000 | 0.866 |
| E_diff=1 (E varies) | 52 | 0.461 | 0.001 | 0.429 |

**Verdict: robust**

## Interpretation

T partial ρ under Metric A in the E_diff=0 cell is 0.855 (n=29), remaining large after isolating pairs where exit options are held constant. This rules out hypothesis (b): the 0.577 was not inflated by T-E joint variation in Tier-1 rope-rope pairs. T-axis variation within the rope immutability group produces extractive-fraction differences independently of E. The original §5.3 mechanistic claim stands.

*Robustness check conducted 2026-05-08. Script: python/audit3_te_robustness.py.*

