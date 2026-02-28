---

## STAGE 2: VALIDATION & RELABELING

**Role:** The Sentinel
**Model:** Perplexity (structured, audit-focused, most granular mapping of requirement resilience; or Lumo for privacy-first containment)
**Input:** Stage 0 output + Stage 1 output
**Output:** PASS/FAIL gate + relabeled specification (if air gap applies)

### Validation Checks

```
TOPOLOGY CONSISTENCY:
  □ Every constraint in Stage 0 appears in Stage 1
  □ χ calculations are arithmetically correct
  □ Type classifications follow from χ values per logic_thresholds.md
  □ Transformation rules reference only constraints that exist

COUPLING VALIDITY:
  □ Every coupling has a mechanism (not just "affects")
  □ No circular dependencies without explicit feedback loop designation
  □ Propagation directions are consistent

ARTIFACT VIABILITY:
  □ At least 2 transformation rules with calculable triggers
  □ At least 1 perspectival gap
  □ Attractor is reachable via transformation rule chain
  □ Constraint network is connected (no orphaned constraints)
  □ Complexity within soft cap (≤4 constraints, ≤3 indices) or decomposition planned

UCZ VALIDITY:
  □ Each UCZ specifies exactly one ambiguity mechanism
  □ Each UCZ participates in at least one coupling
  □ No UCZ is secretly deterministic (mechanism must produce genuine variance)
```

**If FAIL:** Return to Stage 1 with specific issues. Do not proceed.

### Air Gap Preparation

If full or partial air gap will be applied:

1. Produce relabeled specification using Affective Vector protocol (see Architectural Principles)
2. Apply Relabeling Decision Tree to every term
3. Verify: no banned tokens remain
4. Human reviews affective quality: do the labels preserve emotional texture?

**Output:** Both original and relabeled specifications.

---
