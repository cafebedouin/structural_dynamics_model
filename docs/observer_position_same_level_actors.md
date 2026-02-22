# Observer Position Specification for Same-Level Actors

## The Problem

The context tuple `(P, T, E, S)` uses `agent_power` as its first axis, with values ranging from `powerless` through `moderate`, `institutional`, to `analytical`. This creates a natural reading where extraction flows vertically — institutions extract from individuals, the powerful extract from the powerless.

But many real constraints involve extraction between actors at the same nominal power level:

- Peer manipulation (two individuals, one extracting from the other)
- Workplace bullying between colleagues of equal rank
- Market manipulation between similarly-sized firms
- Academic gatekeeping between peers
- Interstate regulatory arbitrage
- Communal narcissism (an individual extracting social capital from peers through performative generosity)

In all these cases, the actors would naively receive the same `agent_power` value. If both are coded as `moderate`, the directionality function sees no power asymmetry, and the classification fails to capture the extraction.

## The Solution Already Exists

The framework already handles this. The mechanism is not `agent_power` alone — it is the combination of `exit_options`, beneficiary/victim declarations, and the directionality derivation chain. These three together differentiate actors who share a nominal power level.

### What `agent_power` Actually Means

`agent_power` does not mean "how powerful is this actor in general." It means "what is this actor's structural relationship to *this specific constraint*."

Two individuals of equal social standing can have different `agent_power` values relative to the same constraint:

- The communal narcissist is `moderate/mobile` — they can exit the relationship whenever the supply dries up. Their exit options are unconstrained.
- The target is `powerless/trapped` — social entanglement, shared community, reputational consequences of leaving mean their exit options are severely limited *with respect to this specific dynamic*.

The target is not globally powerless. They may be professionally successful, financially independent, socially connected. But relative to the specific constraint of the narcissistic relationship, their structural position is powerless/trapped because the features that would allow exit (social independence from the narcissist's network, willingness to bear reputational cost) are not available to them.

### The Directionality Function Does the Work

Once `exit_options` and beneficiary/victim declarations are correctly specified, the sigmoid directionality function `f(d)` produces the power asymmetry automatically:

- The narcissist is declared as `constraint_beneficiary` (receives social capital, admiration, control).
- The target is declared as `constraint_victim` (bears psychological cost, social obligation, reduced autonomy).
- The directionality derivation combines these with `exit_options` to produce different `d` values at each observer position.
- Power-scaled extraction `χ = ε × f(d) × σ(S)` then produces different classifications: rope for the narcissist (coordination tool for managing social capital), snare for the target (extraction mechanism they cannot exit).

No new axis is needed. The existing machinery handles lateral extraction when the inputs are correctly specified.

### The Immutability Table Completes the Picture

Hub 2's `effective_immutability_for_context/2` also differentiates same-level actors. The narcissist at `exit_options(mobile)` perceives the relationship as changeable (rope). The target at `exit_options(trapped)` perceives it as unchangeable (mountain-like). This perception is structurally accurate — the narcissist *can* change the dynamic by leaving; the target cannot, at least not without bearing costs the narcissist does not face.

## Authoring Guidance

When writing a constraint story about interactions between actors at the same nominal power level:

### 1. Do Not Default Both Actors to the Same Context Tuple

The most common error is assigning both peers `agent_power(moderate)` with identical exit options. If both actors have the same context tuple, the framework cannot see the asymmetry. The whole point of indexed classification is that the *same constraint* looks different from different positions.

### 2. Derive Exit Options Independently for Each Actor

Ask: relative to this specific constraint, what are each actor's actual options?

| Actor | Global Power | Constraint-Specific Exit Options | Why |
|---|---|---|---|
| Communal narcissist | Moderate | `mobile` | Can exit the relationship at will; social network is a tool, not a dependency |
| Target | Moderate | `trapped` | Social network is shared with narcissist; leaving means losing community |
| Bullying colleague | Moderate | `arbitrage` | Can escalate to management, transfer teams, or frame the dynamic differently |
| Bullied colleague | Moderate | `constrained` | Escalation risks retaliation; transfer is costly; power to reframe is limited |
| Dominant firm | Institutional | `arbitrage` | Can lobby regulators, absorb fines, restructure to avoid constraint |
| Smaller firm | Institutional | `constrained` | Regulatory costs are proportionally higher; lobbying access is limited |

### 3. Declare Beneficiaries and Victims from the Constraint's Structure

Beneficiary/victim declarations are about the constraint's structural properties, not about who is "good" or "bad." The communal narcissist's generosity is a real coordination function — it genuinely helps people. The extraction (social control, narcissistic supply) operates *through* the same channel. Both are true simultaneously. Declare both:

```prolog
constraint_beneficiary(communal_narcissism, narcissist_social_capital).
constraint_beneficiary(communal_narcissism, community_recipients).  % real coordination
constraint_victim(communal_narcissism, primary_targets).            % extraction
```

### 4. Let the Classification Reveal the Perspectival Gap

If the constraint story is correctly specified, the framework should produce a perspectival gap: the same constraint classifies differently at different observer positions. For lateral extraction between peers, expect:

- **Extractor's position:** rope or scaffold (coordination tool or temporary structure)
- **Target's position:** snare or tangled_rope (extraction mechanism, possibly with real coordination mixed in)
- **Analytical position:** tangled_rope (sees both functions)
- **Institutional position:** rope (sees only the coordination function — this is the T15 epistemic trap)

If the classification produces the same type at all four positions, either the constraint genuinely has no asymmetry (it is a true rope), or the observer positions were not correctly differentiated.

### 5. Watch for the T15 Epistemic Trap

The communal narcissist case is structurally identical to the T15 epistemic trap pattern: `tangled_rope → rope` from the restricted view. The institutional observer (or any external observer without access to the target's private experience) sees only the coordination function. The extraction is invisible from that position because it operates through the same channel as the coordination.

If your constraint story involves lateral extraction with a cover story, T15 should fire on it. If it doesn't, check whether the observer positions are sufficiently differentiated.

## The General Principle

The context tuple is not a description of the actor. It is a description of the actor's *structural relationship to the constraint*. Two actors with identical global power, identical time horizons, and identical spatial scope can have completely different context tuples for the same constraint if their exit options and beneficiary/victim status differ.

This applies at every scale:

- **Interpersonal:** Narcissist vs. target (same social class, different exit options)
- **Organizational:** Gatekeeping colleague vs. blocked colleague (same rank, different access to institutional mechanisms)
- **Interstate:** Regulatory hegemon vs. regulatory taker (both sovereign states, different capacity to shape the constraint)
- **Intercivilizational:** Norm-setter vs. norm-receiver (both civilizations, different structural positions relative to the norm)

The framework does not need a new axis for lateral extraction. It needs correct specification of existing axes. The work is in the authoring, not the architecture.

---

*Framework guidance for constraint story authoring.*
*Prompted by binary structural gates audit and T15 epistemic trap findings.*
*February 2026.*
