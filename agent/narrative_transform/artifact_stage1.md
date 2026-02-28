## STAGE 1: FORMAL SPECIFICATION

**Role:** The Logician
**Model:** Copilot (conservative, rule-bound, integrity-first; whatever it produces, other models can follow; 6/6 authority resistance means it won't fabricate metrics to please you)
**Input:** Stage 0 constraint map
**Output:** Formal specification with transformation rules, error manifestations, attractor, constraint network

Identical to UKE_Narrative v1.2, Stage 1. Key addition for artifacts: **the constraint network (couplings) becomes the causal DAG of the software.** Ensure:
- Every coupling has a clear trigger and consequence
- Transformation rules specify state transitions with calculable thresholds
- The attractor defines the terminal state of the system
- **Every coupling specifies propagation direction and strength** (required for software; optional for stories)

### Underspecified Constraint Zones (UCZs)

Constraints or couplings that are structurally real but intentionally unpinned in behavior may be marked as UCZs. UCZs must participate in causal propagation, affect system state, and be experientially legible — but they may not resolve to a single stable behavior across runs or indices.

**Each UCZ must specify one ambiguity mechanism:**

```
□ Stochastic — random within bounded distribution
    Implementation: RNG within defined range per interaction
□ Index-dependent — resolves differently per index position
    Implementation: Same state variable, different read functions per index
□ Temporal — resolves differently over time
    Implementation: Behavior function includes time parameter
□ Observational — same state, different measurement
    Implementation: Multiple read interfaces for same underlying value
□ Threshold-chaotic — small input changes flip outcome
    Implementation: Sensitivity parameter near bifurcation point
```

This prevents models from either over-resolving UCZs into deterministic behavior or hand-waving them as "sometimes does X, sometimes Y" without a procedural basis.

**UCZ Definition Template (required for each UCZ):**

```
UCZ: [C_id or coupling_id]
  Underlying variable: [what is ambiguous]
  Range: [min, max]
  Mechanism: [stochastic | index_dependent | temporal | observational | threshold_chaotic]
  Parameters: [mechanism-specific]
  Coupling participation: [which coupling(s) it affects]
  Index appearance: [how it manifests per index position]
```

**UCZ Implementation Patterns (for Stage 4 Engine):**

```javascript
// STOCHASTIC: Random within bounded distribution
const stochasticUCZ = (base, variance) => {
  return () => base + (Math.random() * 2 - 1) * variance;
};

// INDEX-DEPENDENT: Same variable, different read per index
const indexDependentUCZ = (baseValue, multipliers) => {
  // multipliers = { indexA: 1.0, indexB: 0.7, indexC: 1.3 }
  return (indexPosition) => baseValue * (multipliers[indexPosition] || 1.0);
};

// TEMPORAL: Drifts over virtual time
const temporalUCZ = (initialValue, driftRate) => {
  return (elapsedTime) => initialValue + (elapsedTime * driftRate);
};

// OBSERVATIONAL: Measurement changes the value
const observationalUCZ = (trueValue, observationDecay) => {
  let observations = 0;
  return () => {
    observations++;
    return trueValue * Math.pow(1 + observationDecay, observations);
  };
};

// THRESHOLD-CHAOTIC: Near bifurcation point, noise dominates
const thresholdChaoticUCZ = (threshold, sensitivity) => {
  return (inputValue) => {
    const noise = Math.abs(inputValue - threshold) < sensitivity
      ? (Math.random() * 2 - 1) * sensitivity
      : 0;
    return (inputValue + noise) > threshold ? "STATE_A" : "STATE_B";
  };
};
```

### BMK Gate (Stage 1)

After formal specification, before proceeding:

```
□ Does the specification contain at least 2 transformation rules?
□ Does the constraint network contain at least 1 coupling?
□ Is there at least 1 perspectival gap (same C, different type from different index)?
□ Is the attractor non-trivial (requires state changes to reach)?

If any answer is NO → invoke BMK. Document why.
Recommend alternative (UKE_Narrative, Resonance Engine, or topology augmentation).
```

