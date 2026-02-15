# Abductive Reasoning Report

*Generated: cross-subsystem anomaly synthesis via abductive_engine:abductive_run/2*

## Summary

| Property | Value |
|----------|-------|
| **Total hypotheses** | 56 |
| **Genuine findings** | 6 |
| **Override artifacts** | 50 |
| **Subsystems available** | [maxent,dirac,drift,signature,mismatch,fingerprint] |

### Hypothesis Counts by Class

| Class | Count | Category |
|-------|-------|----------|
| signature_override_artifact | 50 | artifact |
| deep_deception | 0 | genuine |
| metric_structural_divergence | 4 | genuine |
| confirmed_liminal | 2 | genuine |
| coverage_gap | 0 | genuine |
| accelerating_pathology | 0 | genuine |
| contamination_cascade | 0 | genuine |
| dormant_extraction | 0 | genuine |

> **Note:** FPN data not available — 2 trigger classes inactive (accelerating_pathology, contamination_cascade).

## Artifact Census

How many MaxEnt hard disagreements are explained by known signature overrides?

| Metric | Count |
|--------|-------|
| **Total hard disagreements** | 151 |
| **Explained by override** | 50 |
| **Unexplained** | 101 |
| **Explanation rate** | 33.1% |

A significant portion of hard disagreements are mechanistic artifacts of known signature override rules. These constraints have metrics that predict one type, but a structural signature unconditionally forces a different classification.

### Artifacts by Override Signature

| Signature | Count |
|-----------|-------|
| constructed_low_extraction | 4 |
| false_ci_rope | 46 |

## Genuine Findings

### metric_structural_divergence (4)

High entropy (metric boundary) but preserved single-type Dirac orbit (structurally unambiguous).

| Constraint | Confidence | Anomaly | Key Evidence |
|------------|------------|---------|--------------|
| decentralized_infrastructure_rope | 0.73 | high_entropy_preserved_orbit | H=0.44 |
| fair_use_doctrine | 0.73 | high_entropy_preserved_orbit | H=0.50 |
| noethers_theorem_symmetry | 0.73 | high_entropy_preserved_orbit | H=0.40 |
| reciprocity_laws_math | 0.73 | high_entropy_preserved_orbit | H=0.40 |

### confirmed_liminal (2)

Triple-confirmed liminality: high entropy + multi-type orbit + active drift events.

| Constraint | Confidence | Anomaly | Key Evidence |
|------------|------------|---------|--------------|
| moltbook_agent_theater | 0.85 | triple_confirmed_liminality | H=0.50 orbit=[rope,tangled_rope,unknown] |
| ulysses_calypso_1904 | 0.85 | triple_confirmed_liminality | H=0.41 orbit=[rope,tangled_rope] |

## Highest-Confidence Hypotheses (Top 20)

| Rank | Constraint | Class | Confidence | Anomaly |
|------|------------|-------|------------|---------|
| 1 | ulysses_calypso_1904 | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 2 | moltbook_agent_theater | confirmed_liminal | 0.85 | triple_confirmed_liminality |
| 3 | reciprocity_laws_math | metric_structural_divergence | 0.73 | high_entropy_preserved_orbit |
| 4 | noethers_theorem_symmetry | metric_structural_divergence | 0.73 | high_entropy_preserved_orbit |
| 5 | fair_use_doctrine | metric_structural_divergence | 0.73 | high_entropy_preserved_orbit |
| 6 | decentralized_infrastructure_rope | metric_structural_divergence | 0.73 | high_entropy_preserved_orbit |

## Investigation Queue

Suggested next steps grouped by investigation action, sorted by priority.

### monitor_drift (2 constraints)

Track temporal evolution of these constraints across measurement periods.

| Constraint | Class | Confidence |
|------------|-------|------------|
| ulysses_calypso_1904 | confirmed_liminal | 0.85 |
| moltbook_agent_theater | confirmed_liminal | 0.85 |

### inspect_metrics (4 constraints)

Examine raw metric values and threshold proximity for these constraints.

| Constraint | Class | Confidence |
|------------|-------|------------|
| reciprocity_laws_math | metric_structural_divergence | 0.73 |
| noethers_theorem_symmetry | metric_structural_divergence | 0.73 |
| fair_use_doctrine | metric_structural_divergence | 0.73 |
| decentralized_infrastructure_rope | metric_structural_divergence | 0.73 |

---
*End of abductive report*
