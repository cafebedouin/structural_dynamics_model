## STAGE 6: VALIDATION

**Role:** The Auditor
**Model:** Claude (side-by-side contrast; explicit about own susceptibility) for model-checkable tests. Human operator for experiential tests.
**Input:** Stage 5 artifact + Stage 1 specification (both original AND relabeled)
**Output:** Pass/Fail with specific issues

### Division: Model-Checkable vs. Human-Required

Stage 6 splits into tests that a model can meaningfully execute and tests that require human perception. Models should not pretend to run tests they cannot run.

### 6.1 Model-Checkable Tests

These can be validated by structural analysis of the artifact code:

**Constraint Preservation:**
```
For each constraint in Stage 1:
  □ Present in artifact as functional element?
  □ Metrics causally integrated? (trace: change value → observe propagation)
  □ Indexed types preserved?
  □ Transformation rules executable?
  □ If spectral: anomaly documented and justified?
```

**Network Fidelity:**
```
For each coupling in Stage 1:
  □ Causal propagation implemented?
  □ Trigger fires correctly?
  □ Consequence changes system state as specified?
```

**Air Gap Fidelity (Path A, C):**
```
  □ Grep for banned tokens from relabeling → all return 0?
  □ Grep for source title, author, character names → all return 0?
  □ No source-specific setting details in UI text?
```

**Framework Visibility:**
```
Path A: grep for framework terms → all return 0
Path B: framework terms only in controlled reveal contexts
Path C: grep → all return 0
Path D: framework terms are UI vocabulary (expected)
Path E: framework terms only as bridge labels
```

**UCZ Implementation:**
```
For each UCZ:
  □ Specified mechanism implemented? (stochastic/index-dependent/temporal/etc.)
  □ Produces genuine variance, not deterministic behavior?
  □ Participates in causal propagation?
```

**Personality Consistency (partial — model can check structure, not feel):**
```
  □ Error messages use vocabulary from Stage 3.7?
  □ Idle behavior exists and matches emotional baseline?
  □ Response patterns for success/failure/ignored/probed exist?
```

**Simulation Detection (from MCK Reality Invariant):**

Spec-artifact mismatch reveals performative instantiation. For each claimed feature, verify execution rather than presence:

```
  □ Spec claims causal integration → Change a metric value in code.
    Does the system behave differently? If no → simulation.
  □ Spec claims hysteresis → Trigger perspective shift and return.
    Do available actions or metric readings change? If only visuals change → simulation.
  □ Spec claims UCZ variance → Run the same interaction 5 times.
    Do outcomes differ? If identical → simulation (secretly deterministic).
  □ Spec claims personality → Read error messages without context.
    Could you identify the system's temperament? If generic → simulation.
  □ Spec claims anti-help → Navigate a misrecognition-tolerant constraint.
    Does the system ever correct you? If yes → simulation.
```

**Principle:** If a feature is claimed in the spec but the artifact only *describes* or *decorates* the feature rather than *executing* it, the Reality Invariant is violated. Execution > Simulation.

### 6.2 Human-Required Tests

These require perception, interaction, or judgment that models cannot reliably simulate. The model's role is to flag these for human review, not to pretend to execute them.

**The Residue Test:**
Strip all text labels from the artifact. Look only at how it *behaves*. Does the behavior alone tell a structural story? Could someone who speaks no English feel whether they are in a Snare or Rope position based on interaction feel?

**The Stranger Test:**
Show the artifact to someone with no context. After 3 minutes, ask: "What is this thing about?"

Acceptable: concrete wrong guess, felt experience statement, question probing core dilemma.
Unacceptable: "I don't know," "It's a data visualization," "It's a game about constraints."

**Hysteresis Verification:**
Does the perspective shift actually change the user's experience on return? Does it affect available actions, metric trustworthiness, or information access — not just visual overlay?

**Indexical Variance (qualitative):**
```
  □ Both/all index experiences are internally coherent?
  □ No position is privileged as "correct"?
  □ The difference *feels* structural, not just cosmetic?
```

### 6.3 Art Supremacy Clause

If an artifact fails a model-checkable test but produces a strong moment of recognition, the failure may be documented and preserved. Art outranks protocol. But the failure must be *defended* — not ignored.

### 6.4 BMK Final Gate

```
If the artifact passes all model-checkable tests but the human operator finds it
experientially inert → the topology may not be viable as software.

Recommendation: "This constraint topology produced a correct but inert artifact.
Consider UKE_Narrative (story) or Resonance Engine (diegetic log) instead."
```

Some stories want to be stories, not software.

### 6.5 Fault Recovery

When validation fails, different failures need different recovery:

```
FAILURE: Causal integration broken (metrics don't propagate)
  → Regenerate Engine (Step 1). Focus prompt on coupling implementation.

FAILURE: Index views use separate state (not derived from canonical state)
  → Regenerate Binding (Step 3) with explicit wiring to shared state.

FAILURE: Personality in spec but missing in artifact
  → Regenerate Viewports (Step 2) with personality as primary requirement.

FAILURE: UCZs behave deterministically
  → Check Engine UCZ implementation. Run smoke test (5x dispatch).
    Regenerate UCZ functions with explicit randomness.

FAILURE: Air gap leak (source terms in artifact)
  → Full regeneration from Stage 2 in fresh session with stricter relabeling.

FAILURE: All tests pass but artifact feels inert (BMK INERT)
  → Topology may not be viable as software. Consider decomposition
    or alternative protocol. Do not regenerate — the problem is structural.
```

**Recovery prompt pattern:** "The previous attempt failed [specific failure]. Regenerate [specific step] with emphasis on [recovery focus]. Do not repeat [specific mistake]."

---
