# Abductive Reasoning Report

*Generated: cross-subsystem anomaly synthesis via abductive_engine:abductive_run/2*

## Summary

| Property | Value |
|----------|-------|
| **Total hypotheses** | 35 |
| **Genuine findings** | 10 |
| **Override artifacts** | 25 |
| **Subsystems available** | [maxent,dirac,drift,signature,mismatch,fingerprint] |

### Hypothesis Counts by Class

| Class | Count | Category |
|-------|-------|----------|
| signature_override_artifact | 25 | artifact |
| deep_deception | 0 | genuine |
| metric_structural_divergence | 0 | genuine |
| confirmed_liminal | 10 | genuine |
| coverage_gap | 0 | genuine |
| accelerating_pathology | 0 | genuine |
| contamination_cascade | 0 | genuine |
| dormant_extraction | 0 | genuine |

> **Note:** FPN data not available — 2 trigger classes inactive (accelerating_pathology, contamination_cascade).

## Artifact Census

How many MaxEnt hard disagreements are explained by known signature overrides?

| Metric | Count |
|--------|-------|
| **Total hard disagreements** | 182 |
| **Explained by override** | 25 |
| **Unexplained** | 157 |
| **Explanation rate** | 13.7% |

A significant portion of hard disagreements are mechanistic artifacts of known signature override rules. These constraints have metrics that predict one type, but a structural signature unconditionally forces a different classification.

### Artifacts by Override Signature

| Signature | Count |
|-----------|-------|
| false_ci_rope | 25 |

## Genuine Findings

### confirmed_liminal (10)

Triple-confirmed liminality: high entropy + multi-type orbit + active drift events.

| Constraint | Confidence | Anomaly | Key Evidence |
|------------|------------|---------|--------------|
| astm_d638_tensile_testing | 0.85 | triple_confirmed_liminality | H=0.40 orbit=[rope,tangled_rope] |
| boiled_pineapple_trend_2026 | 0.85 | triple_confirmed_liminality | H=0.45 orbit=[rope,snare,tangled_rope] |
| cuban_missile_crisis_excomm_deliberation | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,tangled_rope] |
| dexy_gold_protocol | 0.85 | triple_confirmed_liminality | H=0.43 orbit=[rope,tangled_rope] |
| djia_as_economic_barometer | 0.85 | triple_confirmed_liminality | H=0.41 orbit=[rope,tangled_rope] |
| dldr_information_policy | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,tangled_rope] |
| fiscal_equalization_friction | 0.85 | triple_confirmed_liminality | H=0.42 orbit=[scaffold,tangled_rope] |
| france_cordon_sanitaire_2026 | 0.85 | triple_confirmed_liminality | H=0.42 orbit=[rope,tangled_rope] |
| quine_self_replication | 0.85 | triple_confirmed_liminality | H=0.51 orbit=[mountain,rope] |
| silklink_2026 | 0.85 | triple_confirmed_liminality | H=0.47 orbit=[rope,scaffold] |

## Highest-Confidence Hypotheses (Top 20)

| Rank | Constraint | Class | Confidence | Anomaly |
|------|------------|-------|------------|---------|
| 1 | silklink_2026 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 2 | quine_self_replication | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 3 | france_cordon_sanitaire_2026 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 4 | fiscal_equalization_friction | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 5 | dldr_information_policy | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 6 | djia_as_economic_barometer | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 7 | dexy_gold_protocol | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 8 | cuban_missile_crisis_excomm_deliberation | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 9 | boiled_pineapple_trend_2026 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 10 | astm_d638_tensile_testing | confirmed_liminal | 0.85 | triple_confirmed_liminality |

## Investigation Queue

Suggested next steps grouped by investigation action, sorted by priority.

### monitor_drift (10 constraints)

Track temporal evolution of these constraints across measurement periods.

| Constraint | Class | Confidence |
|------------|-------|------------|
| silklink_2026 | confirmed_liminal | 0.85 |
| quine_self_replication | confirmed_liminal | 0.85 |
| france_cordon_sanitaire_2026 | confirmed_liminal | 0.85 |
| fiscal_equalization_friction | confirmed_liminal | 0.85 |
| dldr_information_policy | confirmed_liminal | 0.85 |
| djia_as_economic_barometer | confirmed_liminal | 0.85 |
| dexy_gold_protocol | confirmed_liminal | 0.85 |
| cuban_missile_crisis_excomm_deliberation | confirmed_liminal | 0.85 |
| boiled_pineapple_trend_2026 | confirmed_liminal | 0.85 |
| astm_d638_tensile_testing | confirmed_liminal | 0.85 |

---
*End of abductive report*
