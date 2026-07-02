# Metrics as Routing: Why Thresholds Are Governance Stands, Not Truth Claims

**Foundation document — applies to all metrics across the Deferential Realism framework and its tooling**

---

## The Core Distinction

**Common Misinterpretation:** Treating framework metrics (extraction coefficients, power modifiers, purity scores, confidence levels, classification thresholds) as epistemic claims that measure objective truth or probability.

**Correct Interpretation:** Framework metrics are **routing mechanisms** and **decision interfaces** that trigger behaviors and enable consistent comparison, not truth measurements.

This distinction is not a technicality. Getting it wrong makes the entire framework look like precision theater. Getting it right reveals why explicit-but-provisional thresholds are better than hidden judgment calls.

---

## What Framework Metrics ARE

- **Behavioral routing mechanisms** — thresholds trigger different responses (accept, reform, resist)
- **Comparison interfaces** — formulas create comparable numbers across cases for decision-making
- **Governance stands** — explicit lines drawn on uncertain ground ("this is where we choose to act differently")
- **Uncertainty management tools** — Omega variables route unresolved questions to appropriate handlers
- **Decision scaffolding** — structure for making judgment explicit and auditable

## What Framework Metrics ARE NOT

- **Truth measurement instruments** — they don't claim "this constraint is 46% extractive" as objective fact
- **Empirically validated boundaries** — threshold locations are calibrated to separate observed clusters, not derived from first principles
- **Self-enforcing mechanisms** — behavioral drift requires the practitioner to maintain discipline
- **Replacements for judgment** — they make your reasoning visible, not unnecessary

---

## How This Applies Across the Framework

### Extraction Coefficients (ε)

**Wrong Frame:**
- "ε = 0.42 means this constraint extracts exactly 42% of value from participants"
- "The threshold at 0.46 has been empirically validated as the Snare boundary"

**Correct Frame:**
- "ε ≈ 0.42 places this constraint in the Tangled Rope range, routing toward reform rather than resistance or maintenance"
- "The threshold at 0.46 is a governance stand: *above this line, we treat the constraint as primarily extractive and route toward resistance*"
- "The value of the threshold is in making the cutoff explicit, consistent, and auditable — not in claiming it maps to objective reality at two decimal places"

**Implication:** When two analysts estimate different ε values for the same constraint, the productive question isn't "who's right?" but "do both estimates route to the same action?" If ε = 0.38 and ε = 0.44 both land in Tangled Rope territory and both route toward reform, the disagreement is operationally irrelevant. If one lands at 0.44 (Tangled Rope) and the other at 0.48 (Snare), *that's* where the routing divergence matters and further investigation is warranted.

---

### Power Modifiers (π)

**Wrong Frame:**
- "π = 1.5 means powerless people experience exactly 50% more extraction"
- "These multipliers need empirical validation before they're legitimate"

**Correct Frame:**
- "π = 1.5 routes the powerless position toward experiencing higher effective extraction, which may shift the constraint's classification for that index"
- "The modifier makes a structural claim explicit: *power position changes experienced extraction*. The specific multiplier is calibrated to produce useful category separation, not to claim measurement precision"

**Implication:** The analytical modifier π = 1.15 doesn't claim analysts perceive exactly 15% more extraction than moderates. It claims that observation without stakes adds a small perceptual friction — enough to break the moderate-analytical degeneracy in classification and route analysts toward slightly different constraint experiences than participants. The value is in the routing, not the precision.

---

### Purity Scores

**Wrong Frame:**
- "Purity = 0.55 means this institution is 55% healthy"
- "The 0.40 threshold for Snare reclassification needs clinical validation"

**Correct Frame:**
- "Purity ≈ 0.55 routes to *monitor with concern* — the coordination function is present but significantly compromised"
- "Below 0.40, we route to *intervene or abandon* — the extraction has consumed enough of the coordination function that the constraint is approaching Snare behavior"
- "The score enables tracking over time: is this constraint getting healthier or sicker? The direction matters more than the absolute number"

---

### Confidence and Omega Variables

**Wrong Frame:**
- "Confidence 0.64 means I'm 64% certain this classification is correct"
- "Omega variables are admissions of failure"

**Correct Frame:**
- "Confidence 0.64 routes to the medium bin — triggers additional scrutiny (assumption testing, alternative perspective check) before acting"
- "Omega variables route uncertainty to appropriate handlers: Ω_E (go measure something), Ω_C (go define a term), Ω_P (go ask stakeholders what they value)"
- "Marking uncertainty explicitly is better than hiding it behind false confidence"

---

## Why The Frame Matters

### Routing Frame Strengths

1. **Thresholds become defensible:** Making an arbitrary-but-explicit cutoff is better governance than hiding judgment calls behind vague language. "We draw the Snare line at ε ≥ 0.46" is auditable. "It felt really extractive" is not.

2. **Disagreements become productive:** When two people disagree on ε, the routing frame asks "does this disagreement change the action?" — which often dissolves the debate.

3. **Formulas create decision infrastructure:** χ = ε × π doesn't measure "true extraction." It creates comparable inputs across cases and positions, the way money enables comparison without claiming to measure objective value.

4. **Omega tracking makes sense:** Routing unresolved questions to explicit handlers (measure, define, decide) preserves forward motion instead of blocking on uncertainty.

5. **Practitioner authority is preserved:** The system makes your judgment explicit and systematic, not replaced by measurement.

### Truth Frame Problems

1. **Creates unfalsifiable expectations:** If metrics claim truth, every subjective input becomes "precision theater" and every threshold demands empirical validation that may be impossible or unnecessary.

2. **Makes the framework seem broken:** "These thresholds are arbitrary" looks like a devastating critique in the truth frame. In the routing frame, it's the point — the value is in making the arbitrary explicit.

3. **Hides the actual function:** The framework routes attention, triggers appropriate rigor, enables comparison, and catches errors. None of these require objective truth measurement.

4. **Invites the wrong criticism:** Every cross-model review of this framework has included some version of "the thresholds seem arbitrary" or "the calibration needs more justification." This is the truth-frame error in action. The thresholds are governance stands. Their value is in consistency and explicitness, not in precision.

---

## The Self-Application Test

When this framework was evaluated by multiple AI models, every model initially made the truth-frame error on at least one metric:

- Questioned whether ε thresholds had "empirical validation"
- Called power modifiers "subjective-ish calibration"
- Asked whether purity weights were "theoretically motivated"
- Suggested thresholds need "deeper rationale"

The correction: these are routing mechanisms. ε ≥ 0.46 doesn't claim to have measured the objective boundary between hybrid and extractive constraints. It claims that *at this line, we change our recommended response from "reform" to "resist."* The threshold's value is in making that governance decision explicit, consistent, and revisable — not in claiming it corresponds to some natural joint in reality.

What looked like framework weaknesses were features being misunderstood.

---

## The Failure Mode This Principle Prevents: A Real Case

The gate violation is not hypothetical. During engine development (April 2026),
a systematic audit of the Prolog classification engine found three instances where
diagnostic test results had been written as hard blocking gates inside certificate
predicates — the exact violation this principle prohibits.

**The pattern in each case:**

`boltzmann_floor_override(C, Value)` → `boltzmann_floor_for/2` → `excess_extraction/2`
→ threshold comparison → gate condition inside a certificate predicate → classification
suppressed

**What this meant in practice:** An editorial override on a per-constraint basis
(intended as a soft-fail mechanism to prevent pipeline blockage) could propagate
through the excess extraction calculation and suppress a certification result — turning
a routing input into a verdict. The soft-fail ethic, which was correct, had been
partially undone by implementation drift.

**How it was caught:** The CI_Rope gate (`coupling_invariant_rope/2` in
`boltzmann_compliance.pl`) was found by inspection during an unrelated analysis.
That finding motivated a systematic audit, which found two further violations in the
FCR pipeline (`false_ci_rope/2` and `fcr_test_failure/2` in `signature_detection.pl`).

**All three fixes were identical in structure:** demote the test result from blocking
gate to diagnostic payload in the evidence record; let the certificate predicate
succeed or fail on structural conditions only.

**All three violations were latent** — no current constraint in the corpus had its
classification actively suppressed. The fixes matter because the redundancy that
protected the corpus was incidental, not designed. A constraint where only the
floor-overridable signal was present would have been silently miscertified.

**The architectural check that catches this pattern:** If a predicate issues a
classification certificate or type override, every `if-then-fail` condition in its
body should be a structural fact about the constraint (e.g., `has_coordination_function(C)`,
`Beneficiaries \= []`) — not the output of a test or diagnostic predicate whose
value can be influenced by an override input. Test outputs belong in the evidence
record, not in the gate sequence.

---

## Operational Implications

### When evaluating framework outputs:

**Don't evaluate:**
- "Is this extraction coefficient objectively accurate?"
- "Do these thresholds have empirical validation?"
- "Can we prove these metrics measure what they claim?"

**Do evaluate:**
- "Did this coefficient route to an appropriate response?"
- "Do these thresholds create consistent, auditable decision points?"
- "Do these metrics enable useful comparison and tracking across cases?"

### When using the framework:

**Don't:**
- Treat extraction coefficients as precision measurements
- Expect formulas to produce "true" values
- Assume thresholds represent validated empirical boundaries
- Argue about the third decimal place

**Do:**
- Use thresholds to trigger appropriate rigor levels
- Use formulas to create comparable decision inputs across cases
- Set thresholds as explicit governance stands that can be revised
- Focus on whether routing disagreements change the action

---

## The Analogy

Thresholds in this framework work like a thermostat setting, not a thermometer reading.

A thermometer claims to measure objective temperature. A thermostat setting *routes behavior*: below this number, the heater turns on; above it, it turns off. You don't criticize a thermostat for not being a "true" temperature. You evaluate it by whether it keeps the room comfortable — whether the routing produces good outcomes.

Similarly: ε ≥ 0.46 routes toward resistance. Purity < 0.40 routes toward intervention. π = 1.5 routes powerless positions toward experiencing higher extraction. These settings can be adjusted. Their value is in the consistency and explicitness of the routing, not in claiming to have measured reality at two decimal places.

---

## Bottom Line

**The framework works by making your judgment explicit and systematic, not by replacing your judgment with measurement.**

Metrics provide:
- Behavioral routing (what response to trigger)
- Decision scaffolding (how to compare across cases)
- Uncertainty tracking (what remains unresolved)
- Governance explicitness (where you're drawing lines and why)
- Authority preservation (you remain the judge)

Metrics don't provide (and don't claim to):
- Objective truth measurement
- Empirically validated boundaries
- Self-enforcement
- Epistemic certainty

When you find yourself thinking "these numbers seem arbitrary" — stop. You're in the wrong frame. Ask instead: "do these numbers route to appropriate action?" That's what they're for.

---

**Origin:** This principle was first articulated in the UKE Protocol Framing Guide
after Claude made the truth-frame error while auditing its own protocol outputs. The
error was so consistent across AI models that it warranted foundational status. The
principle was operationally confirmed in April 2026 when a systematic engine audit
found three gate violations in the Prolog classification pipeline — all caught by
applying this principle as a structural check. For UKE-specific application
(confidence bins, IWBI formulas, fidelity thresholds, T1/T2 distinctions), see the
original `uke_protocol_framing_guide.md`.

**Cross-references:**
- `core.md` — "On metrics and thresholds" paragraph applies this principle to the gateway document
- `logic_thresholds.md` — Canonical parameter registry (all threshold values in one place)
- `epistemology.md` — Methodology discussion of calibration and validation
- `uke_protocol_framing_guide.md` — Original document; UKE-specific application with detailed examples
