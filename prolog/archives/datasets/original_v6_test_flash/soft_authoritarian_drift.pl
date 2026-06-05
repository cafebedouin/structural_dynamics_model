% ============================================================================
% CONSTRAINT STORY: soft_authoritarian_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soft_authoritarian_drift, []).

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
 *   constraint_id: soft_authoritarian_drift
 *   human_readable: The Incremental Compliance Web
 *   domain: political/social/technological
 *
 * SUMMARY:
 *   The Incremental Compliance Web describes a scenario where a democratic or
 *   open system gradually adopts restrictive measures—often under the guise
 *   of safety, efficiency, or "community standards"—that erode individual
 *   autonomy. This drift presents a complex mix of coordination and
 *   extraction, where perceived benefits mask the gradual loss of freedoms
 *   and increased surveillance.
 *
 * KEY AGENTS:
 *   - Governing Authorities: Benefit from increased control and perceived stability.
 *   - Dominant Platforms: Benefit from enhanced user engagement and perceived safety.
 *   - Individual Autonomy: Suffers from gradual erosion of freedoms and increased surveillance.
 *   - Dissenting Voices: Experience silencing and marginalization due to restrictive measures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soft_authoritarian_drift, 0.55).
domain_priors:suppression_score(soft_authoritarian_drift, 0.65).
domain_priors:theater_ratio(soft_authoritarian_drift, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soft_authoritarian_drift, extractiveness, 0.55).
narrative_ontology:constraint_metric(soft_authoritarian_drift, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(soft_authoritarian_drift, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soft_authoritarian_drift, tangled_rope).
narrative_ontology:human_readable(soft_authoritarian_drift, "The Incremental Compliance Web").
narrative_ontology:topic_domain(soft_authoritarian_drift, "political/social/technological").

domain_priors:requires_active_enforcement(soft_authoritarian_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, governing_authorities).
narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, dominant_platforms).
narrative_ontology:constraint_victim(soft_authoritarian_drift, individual_autonomy).
narrative_ontology:constraint_victim(soft_authoritarian_drift, dissenting_voices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Silenced Dissenter (Snare) - Lacking resources and avenues for effective resistance, they bear the full cost of the constraint.
constraint_indexing:constraint_classification(soft_authoritarian_drift, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Compliant Citizen (Tangled Rope) - While benefiting from perceived safety and efficiency, they are constrained by the erosion of freedoms and face increasing extraction.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: Governing Authority (Rope) - Sees the constraint as a coordination mechanism for maintaining order and safety, benefiting from increased control.
constraint_indexing:constraint_classification(soft_authoritarian_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: Dominant Platform (Tangled Rope) - Benefits from increased user engagement and perceived safety, but faces extraction through regulatory pressure and reputational risk.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 5: Analytical Observer (Tangled Rope) - Analyzes the mixed coordination and extraction aspects across all levels, identifying the structural drivers of the authoritarian drift.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soft_authoritarian_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soft_authoritarian_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soft_authoritarian_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soft_authoritarian_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(soft_authoritarian_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate, reflecting the gradual erosion of individual autonomy through various policies and technologies. Suppression (0.65) is high, as dissenting voices are increasingly marginalized, and avenues for resistance are limited. Theater ratio (0.40) is relatively low, suggesting the performative aspect of justifying restrictions is less prominent than their actual effects on limiting freedom.
 *
 * PERSPECTIVAL GAP:
 *   The governing authority views the system as a coordination mechanism for maintaining order and safety, while the individual dissenter experiences it as a snare, restricting their freedom of expression. The dominant platform recognizes both the benefits of increased engagement and the extractions of regulatory pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Governing authorities and dominant platforms benefit from the increased control and user engagement respectively. In both cases there are pressures to further increase engagement, leading to increased extraction. Individual autonomy and dissenting voices bear the costs of increased surveillance and limited freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by recognizing the perceived benefits and trade-offs involved in maintaining order. The governing body claims they are keeping order and safety. It is not pure extraction because a base amount of stability can be produced. A complete perspective shows this as a tangled rope where some of the affected groups benefit, while other groups are victimized by the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_creep_threshold,
    'What is the critical threshold of individual autonomy erosion beyond which the system irreversibly becomes authoritarian?',
    'Longitudinal analysis of historical examples, measuring civic engagement, political participation, and freedom of expression.',
    'Determines the urgency and scope of interventions required to reverse the drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_threshold, empirical, 'Threshold of acceptable individual autonomy erosion.').

omega_variable(
    intentionality_attribution,
    'To what extent is the drift the result of deliberate planning versus emergent consequences of decentralized decisions?',
    'Network analysis of policy decisions, tracing influence, identifying key actors and their motives.',
    'Informs strategies for accountability and intervention, focusing on systemic flaws vs individual actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_attribution, conceptual, 'Intentionality behind the authoritarian drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soft_authoritarian_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, soft_authoritarian_drift, theater_ratio, 0, 0.25).
narrative_ontology:measurement(soft_tr_t5, soft_authoritarian_drift, theater_ratio, 5, 0.32).
narrative_ontology:measurement(soft_tr_t10, soft_authoritarian_drift, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, soft_authoritarian_drift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t5, soft_authoritarian_drift, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(soft_be_t10, soft_authoritarian_drift, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soft_authoritarian_drift, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
