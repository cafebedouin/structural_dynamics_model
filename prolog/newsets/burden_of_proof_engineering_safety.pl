% ============================================================================
% CONSTRAINT STORY: burden_of_proof_engineering_safety
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burden_of_proof_engineering_safety, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burden_of_proof_engineering_safety
 *   human_readable: Reversal of the Engineering Burden of Proof
 *   domain: technological/safety_engineering
 *
 * SUMMARY:
 *   This constraint models the institutional and cultural shift within
 *   high-stakes engineering organizations where the burden of proof for
 *   safety is reversed. Instead of engineers being required to prove a system
 *   is safe to operate, a culture emerges where managers and marketers demand
 *   that engineers prove a system is definitively unsafe to delay or stop its
 *   launch. This transfers immense, often hidden, risk from the organization
 *   to its end-users. The Boeing 737 MAX crashes are a canonical example of
 *   this dynamic, where internal pressure to compete with Airbus led to the
 *   suppression of engineering concerns and a reversal of the normal safety
 *   validation process.
 *
 * KEY AGENTS:
 *   - Corporate Management: Primary beneficiary (institutional/arbitrage) - Benefits from faster time-to-market and lower development costs.
 *   - End-Users (Passengers, Operators): Primary victim (powerless/trapped) - Bear the ultimate physical risk without knowledge or consent.
 *   - Dissenting Engineers: Secondary victim (moderate/constrained) - Face career risk for upholding traditional engineering ethics.
 *   - Captured Regulators: Institutional actor (institutional/constrained) - Maintain the performance of oversight while lacking the resources or independence to enforce standards, becoming a Piton.
 *   - Engineering Epistemic Integrity: Abstract victim (powerless/trapped) - The professional standard itself is degraded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_engineering_safety, 0.65).
domain_priors:suppression_score(burden_of_proof_engineering_safety, 0.75).
domain_priors:theater_ratio(burden_of_proof_engineering_safety, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, extractiveness, 0.65).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_engineering_safety, tangled_rope).
narrative_ontology:human_readable(burden_of_proof_engineering_safety, "Reversal of the Engineering Burden of Proof").
narrative_ontology:topic_domain(burden_of_proof_engineering_safety, "technological/safety_engineering").

domain_priors:requires_active_enforcement(burden_of_proof_engineering_safety).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burden_of_proof_engineering_safety, corporate_management).
narrative_ontology:constraint_beneficiary(burden_of_proof_engineering_safety, shareholders).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, end_users).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, flight_crews).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, dissenting_engineers).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, engineering_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END-USER (SNARE) — The end-user is unaware of the reversed burden of proof and has no ability to exit the system or assess the increased risk. They bear the full, uncompensated cost of a failure. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.24. This is extreme extraction.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CORPORATE MANAGEMENT (ROPE) — From this perspective, the constraint is a coordination mechanism to overcome 'excessive' engineering caution, meet deadlines, and deliver shareholder value. It aligns the organization toward market goals. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08. A net subsidy.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: DISSENTING ENGINEER (TANGLED ROPE) — This agent understands both the coordination function (shipping a product) and the severe extraction of safety margins. They are constrained by career risk and organizational pressure, unable to easily exit or reverse the policy. d≈0.85, f(d)≈1.32, σ=1.0 → χ≈0.86.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPTURED REGULATOR (PITON) — The regulatory body maintains the rituals of safety certification, but its function has atrophied due to industry pressure or resource starvation. The process is performative, not functional. The high theater_ratio (0.75) satisfies the piton gate.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's objective classification. It recognizes the genuine (if perverse) coordination function of aligning the company to a goal, but also the severe, asymmetric extraction of safety from victims. This is the canonical definition of a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_engineering_safety_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(burden_of_proof_engineering_safety, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(burden_of_proof_engineering_safety, TR),
    TR >= 0.70.

:- end_tests(burden_of_proof_engineering_safety_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the value being extracted is the safety margin, a critical component of product value, which is transferred as risk to users. Suppression (0.75) is high, reflecting the organizational mechanisms used to silence dissent, ignore negative data, and push projects forward against the warnings of technical experts. Theater Ratio (0.75) is high because the formal safety review processes are maintained for legal and regulatory compliance, but their actual function of ensuring safety is hollowed out, becoming a box-checking exercise.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Management perceives a necessary coordination tool (Rope) for achieving business objectives. The powerless end-user, if they knew the facts, would perceive a deadly trap (Snare). The engineer caught in the system sees the conflict clearly: a system that coordinates action (the 'rope' part) but does so by extracting a critical good—safety (the 'tangled' part). The regulator's perspective as a Piton shows institutional decay, where form persists long after function has ceased.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (management) have arbitrage exit options and an immediate time horizon, leading to a low 'd' value and a Rope classification. Victims (end-users) are trapped with no exit, leading to a high 'd' value and a Snare classification. The dissenting engineer is constrained, not fully trapped, placing them in the Tangled Rope category. This distribution of directionality based on structural position is what allows a single set of base metrics to generate multiple, valid classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a prime example of resolving mandatrophy. A naive analysis might label the corporation's actions as purely evil (Snare) or the process as simply 'how business is done' (Rope). Deferential Realism shows that both are structurally correct perspectives. The system *is* a Snare to the passenger and *is* a Rope to the executive. The analytical classification of Tangled Rope correctly identifies the core structure: a mechanism that combines a genuine coordination function with a severe, asymmetric extraction of value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_emergence,
    'Was the reversal of the burden of proof a deliberate, conscious decision by management for profit, or an emergent cultural property born from market pressures and organizational complexity?',
    'Internal communications, whistleblower testimony, and board-level meeting minutes.',
    'If deliberate, the constraint is closer to a pure Snare. If emergent, it is more accurately a Tangled Rope, where the coordination function is a genuine (though misguided) aspect of the system''s behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_emergence, conceptual, 'Distinguishing between deliberate extraction and emergent institutional failure.').

omega_variable(
    restoration_pathway,
    'Can external events (e.g., a catastrophic failure, new legislation) successfully and permanently restore the original ''prove it is safe'' burden of proof?',
    'Longitudinal study of organizational behavior and safety metrics following a major incident and regulatory intervention.',
    'If restoration is possible, the constraint can be dismantled. If the culture is permanently altered, the constraint becomes a persistent Piton, with safety processes remaining theatrical despite reforms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_pathway, empirical, 'Feasibility of reversing the cultural shift in engineering safety standards.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_engineering_safety, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burd_tr_t1990, burden_of_proof_engineering_safety, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(burd_tr_t2005, burden_of_proof_engineering_safety, theater_ratio, 2005, 0.5).
narrative_ontology:measurement(burd_tr_t2020, burden_of_proof_engineering_safety, theater_ratio, 2020, 0.75).

% Extraction over time
narrative_ontology:measurement(burd_be_t1990, burden_of_proof_engineering_safety, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(burd_be_t2005, burden_of_proof_engineering_safety, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(burd_be_t2020, burden_of_proof_engineering_safety, base_extractiveness, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burden_of_proof_engineering_safety, enforcement_mechanism).
narrative_ontology:affects_constraint(burden_of_proof_engineering_safety, regulatory_capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
