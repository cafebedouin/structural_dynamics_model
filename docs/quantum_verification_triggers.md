# Quantum Verification Triggers (T13–T16)

## Prerequisites

- `docs/binary_structural_gates.md` — Two-tier architecture, Boltzmann cross-index tests
- `docs/observer_position_same_level_actors.md` — Same-level actor guidance
- `outputs/quantum_verification_report.md` — Empirical findings these triggers operationalize
- `notes/grothendieck_framing.md` — Cohomological formalism (H¹ invariant)

## Summary

The abductive engine originally had 11 trigger classes (T1–T11, plus T12 in post_synthesis) that detect when diagnostic subsystems disagree with each other. Triggers T13–T16 extend the engine with a different capability: detecting when diagnostic *agreement* is misleading — when the framework's classical tools are systematically blind to structure that the indexed and cohomological tools can see.

Each trigger operationalizes a specific finding from the quantum complexity verification report.

## The Four Triggers

### T13: MaxEnt Divergence

**Module:** `abductive_triggers.pl:trigger_maxent_divergence/3`

**What it catches:** Constraints where power-scaling changes the probabilistic answer. The classical MaxEnt (using raw ε) and the indexed MaxEnt (using power-scaled χ) produce divergent probability distributions, with divergence exceeding the configured threshold (default: 0.05).

**Population:** ~11 constraints. Smallest population, strongest per-constraint signal.

**What it means:** The raw, observer-independent metrics say one thing about this constraint's type probabilities. Once you account for the observer's structural position (via the sigmoid directionality function), the probabilistic answer changes. These are constraints where the numbers look innocuous from a neutral perspective but become significant — or change category entirely — when evaluated from a specific power position.

**Empirical basis:** Query 1 of the verification report. 8/8 high-divergence constraints have H¹ > 0, confirming that MaxEnt divergence correlates with structural observer-dependence.

**Data dependencies:** `maxent_indexing_divergence/3`, `cohomological_obstruction/3`. Requires `maxent_indexed_run/2` to have been called (handled by `abductive_report.pl` which runs indexed MaxEnt before the trigger phase).

**Confidence:** 0.80 (high — the divergence is a direct measurement, not an inference).

---

### T14: Hub-Conflict

**Module:** `abductive_triggers.pl:trigger_hub_conflict/3`

**What it catches:** Constraints where Hub 2's immutability table flips the classification. These sit at exactly H¹ = 4 (configurable), meaning the discrete (TimeHorizon × ExitOptions) lookup table switches between "mountain" and "not mountain" as the observer position changes. Hub 1 (the sigmoid/power-scaling machinery) and Hub 2 (the immutability table) give conflicting signals about whether this constraint is changeable.

**Population:** ~23 constraints.

**What it means:** Whether this constraint looks permanent or mutable depends entirely on which row of the immutability table applies to you. From one position, it is an unchangeable feature of reality. From another, it is a construct that could be different. The classification change is driven by a discrete table lookup, not by the continuous extraction metrics — the constraint's numerical profile is identical across positions, but the perception of changeability flips.

**The biological_curiosity example:** A neuroscientist at `exit_options(analytical)` sees curiosity as an immutable law of neurobiology (mountain). A trapped subject at `exit_options(trapped)` sees it as an inescapable source of suffering (snare). The extraction metrics are the same. The immutability perception is different.

**Empirical basis:** Query 2 of the verification report. 15/15 hub-conflict constraints cluster at H¹ = 4 (expanded to ~23 with full corpus). Zero Type A conflicts (mountain with Chi above snare threshold) — the mountain gate's BaseEps check prevents the most pathological hub-conflicts from arising.

**Data dependencies:** `cohomological_obstruction/3`.

**Confidence:** 0.75.

---

### T15: Epistemic Trap

**Module:** `abductive_triggers.pl:trigger_epistemic_trap/3`

**What it catches:** Constraints where the powerless observer's restricted information set produces a different classification than the full-information classifier. The restricted view (what is structurally accessible from the powerless position) leads to a wrong answer — not a different-but-valid perspectival answer, but a classification based on incomplete data.

**Population:** ~293 constraints, breaking into three distortion patterns:

| Pattern | Count | What happens |
|---|---|---|
| mountain → rope (114) | Natural laws appear as coordination choices. The restricted view *upgrades* apparent mutability. The powerless observer may waste effort trying to change something unchangeable. |
| tangled_rope → snare (82) | Mixed systems appear purely extractive. The restricted view loses the coordination function. The powerless observer sees more extraction than exists. |
| tangled_rope → rope (65) | Extraction becomes invisible. The restricted view shows only the coordination function, hiding asymmetric benefit flow. The powerless observer perceives "just the rules." |

**The third pattern is the dangerous one.** These 65 constraints are where "it's just the rules" is literally what the powerless observer perceives, because the features that reveal extraction are not in their accessible information set. This is the structural mechanism behind cover stories: the cover story works because the restricted view genuinely does not include the evidence that would contradict it.

**Connection to same-level actor dynamics:** The communal narcissist pattern is structurally identical to the tangled_rope → rope epistemic trap. The target (coded as powerless/trapped) sees only the generosity (rope). The extraction (narcissistic supply, social control) operates through the same channel and is invisible from the restricted view. T15 should fire on any correctly-authored constraint story involving coordination-washed lateral extraction.

**Empirical basis:** Query 3 of the verification report (disconfirmed the prediction that restricted-view divergence would overlap with gauge_fixed, revealing they are independent phenomena) and the T15 population analysis post-implementation.

**Data dependencies:** `classify_from_restricted/3`, `dr_type/3`. No subsystem gate — `constraint_indexing` is always available.

**Confidence:** 0.70.

---

### T16: Classical Oracle Failure

**Module:** `abductive_triggers.pl:trigger_classical_oracle_failure/3`

**What it catches:** Constraints where the MaxEnt probabilistic model is confident (low entropy, below the configured ceiling of 0.40) but the cohomological formalism detects structural observer-dependence (H¹ > 0). The classical oracle thinks it has a clear answer. The indexed formalism knows the answer depends on who is asking.

**Population:** ~870 constraints. Largest trigger class by far.

**What it means:** Most observer-dependence in the corpus does not produce large probabilistic shifts. The Gaussian likelihoods in the MaxEnt model are broad enough that the raw metrics are compatible with multiple types, so shifting from ε to χ does not move the probability mass much — but it does cross the deterministic classification thresholds. The classical oracle thinks the answer is unambiguous either way. The indexed classifier knows it is different.

This is the 99% that MaxEnt misses. The verification report's reverse finding showed that only 1.0% of H¹ > 0 constraints have high MaxEnt divergence (caught by T13). The remaining 99% have observer-dependence that the probabilistic formalism cannot detect. T16 catches these.

**Exclusion gate:** T16 does not fire on constraints already flagged by T13. The ordering constraint (T13 runs before T16) ensures zero overlap. T13 catches the cases where the classical oracle actively disagrees with the indexed oracle. T16 catches the cases where the classical oracle agrees but is blind to the observer-dependence.

**Confidence:** 0.55 + min(0.15, H¹ × 0.03). Scales with H¹ severity — higher cohomological obstruction produces higher confidence. The base confidence is lower than T13–T15 because the trigger fires on a weaker signal (oracle confidence + obstruction, rather than direct divergence or classification mismatch).

**Empirical basis:** Query 1 reverse direction. The cohomological formalism detects 100× more observer-dependence than the probabilistic formalism.

**Data dependencies:** `maxent_entropy/3`, `cohomological_obstruction/3`. Requires both `maxent` and `cohomology` subsystems.

---

## Architectural Role

The original 11 triggers answer: **"Do the diagnostics disagree with each other?"**

T13–T16 answer a different question: **"Is the agreement misleading?"**

| Trigger | What agreement it tests | What blindness it detects |
|---|---|---|
| T13 | Classical vs. indexed MaxEnt | Power-scaling changes the probabilistic answer |
| T14 | Hub 1 (sigmoid) vs. Hub 2 (immutability table) | Discrete perception flip not visible in continuous metrics |
| T15 | Full-information vs. restricted-information classifier | Observer's accessible data produces wrong classification |
| T16 | MaxEnt confidence vs. cohomological obstruction | Probabilistic model confident about observer-independent answer that is actually observer-dependent |

Together with T13, T16 covers 98.9% of all H¹ > 0 constraints (881/891). The 10 uncovered constraints have MaxEnt entropy above 0.40 and no indexed divergence — genuinely ambiguous cases where neither trigger should fire.

## Interaction with Other Triggers

T13–T16 can fire alongside the original 11 triggers. The `biological_curiosity` constraint fires four triggers simultaneously: T9 (shadow divergence), T14 (hub-conflict), T15 (epistemic trap), and T16 (classical oracle failure). Multiple simultaneous triggers indicate genuine structural complexity, not diagnostic noise — independent analytical frameworks are converging on the same constraint as anomalous.

## Configuration

Three config parameters control trigger thresholds:

| Parameter | Default | Trigger | What it controls |
|---|---|---|---|
| `abductive_maxent_divergence_threshold` | 0.05 | T13 | Minimum indexed MaxEnt divergence to fire |
| `abductive_hub_conflict_h1_threshold` | 4 | T14 | Exact H¹ band for hub-conflict detection |
| `abductive_oracle_entropy_ceiling` | 0.40 | T16 | Maximum normalized entropy for "confident oracle" |

T15 has no configurable threshold — it fires whenever restricted and full classifications diverge.

---

*Abductive engine extension documentation.*
*Based on quantum complexity verification report findings.*
*February 2026.*
