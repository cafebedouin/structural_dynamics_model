# Abductive Reasoning Report

*Generated: cross-subsystem anomaly synthesis via abductive_engine:abductive_run/2*

## Summary

| Property | Value |
|----------|-------|
| **Total hypotheses** | 32 |
| **Genuine findings** | 11 |
| **Override artifacts** | 21 |
| **Subsystems available** | [maxent,dirac,drift,signature,mismatch,fingerprint] |

### Hypothesis Counts by Class

| Class | Count | Category |
|-------|-------|----------|
| signature_override_artifact | 21 | artifact |
| deep_deception | 0 | genuine |
| metric_structural_divergence | 0 | genuine |
| confirmed_liminal | 11 | genuine |
| coverage_gap | 0 | genuine |
| accelerating_pathology | 0 | genuine |
| contamination_cascade | 0 | genuine |
| dormant_extraction | 0 | genuine |

> **Note:** FPN data not available — 2 trigger classes inactive (accelerating_pathology, contamination_cascade).

## Artifact Census

How many MaxEnt hard disagreements are explained by known signature overrides?

| Metric | Count |
|--------|-------|
| **Total hard disagreements** | 163 |
| **Explained by override** | 21 |
| **Unexplained** | 142 |
| **Explanation rate** | 12.9% |

A significant portion of hard disagreements are mechanistic artifacts of known signature override rules. These constraints have metrics that predict one type, but a structural signature unconditionally forces a different classification.

### Artifacts by Override Signature

| Signature | Count |
|-----------|-------|
| false_ci_rope | 21 |

## Genuine Findings

### confirmed_liminal (11)

Triple-confirmed liminality: high entropy + multi-type orbit + active drift events.

| Constraint | Confidence | Anomaly | Key Evidence |
|------------|------------|---------|--------------|
| astm_d638_tensile_testing | 0.85 | triple_confirmed_liminality | H=0.41 orbit=[rope,unknown] |
| boiled_pineapple_trend_2026 | 0.85 | triple_confirmed_liminality | H=0.42 orbit=[rope,snare,tangled_rope] |
| clawderberg_recursive_slop | 0.85 | triple_confirmed_liminality | H=0.41 orbit=[rope,unknown] |
| dexy_gold_protocol | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,unknown] |
| dldr_information_policy | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,unknown] |
| exoplanetary_habitability_arbitrage | 0.85 | triple_confirmed_liminality | H=0.40 orbit=[scaffold,unknown] |
| france_cordon_sanitaire_2026 | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,tangled_rope] |
| institutional_trust_decay | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,tangled_rope,unknown] |
| moltbook_agent_theater | 0.85 | triple_confirmed_liminality | H=0.44 orbit=[rope,tangled_rope,unknown] |
| quine_self_replication | 0.85 | triple_confirmed_liminality | H=0.48 orbit=[mountain,rope] |
| silklink_2026 | 0.85 | triple_confirmed_liminality | H=0.46 orbit=[rope,scaffold] |

## Highest-Confidence Hypotheses (Top 20)

| Rank | Constraint | Class | Confidence | Anomaly |
|------|------------|-------|------------|---------|
| 1 | silklink_2026 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 2 | quine_self_replication | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 3 | moltbook_agent_theater | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 4 | institutional_trust_decay | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 5 | france_cordon_sanitaire_2026 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 6 | exoplanetary_habitability_arbitrage | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 7 | dldr_information_policy | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 8 | dexy_gold_protocol | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 9 | clawderberg_recursive_slop | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 10 | boiled_pineapple_trend_2026 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 11 | astm_d638_tensile_testing | confirmed_liminal | 0.85 | triple_confirmed_liminality |

## Investigation Queue

Suggested next steps grouped by investigation action, sorted by priority.

### monitor_drift (11 constraints)

Track temporal evolution of these constraints across measurement periods.

| Constraint | Class | Confidence |
|------------|-------|------------|
| silklink_2026 | confirmed_liminal | 0.85 |
| quine_self_replication | confirmed_liminal | 0.85 |
| moltbook_agent_theater | confirmed_liminal | 0.85 |
| institutional_trust_decay | confirmed_liminal | 0.85 |
| france_cordon_sanitaire_2026 | confirmed_liminal | 0.85 |
| exoplanetary_habitability_arbitrage | confirmed_liminal | 0.85 |
| dldr_information_policy | confirmed_liminal | 0.85 |
| dexy_gold_protocol | confirmed_liminal | 0.85 |
| clawderberg_recursive_slop | confirmed_liminal | 0.85 |
| boiled_pineapple_trend_2026 | confirmed_liminal | 0.85 |
| ... | (1 more) | |

---
*End of abductive report*
