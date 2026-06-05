% ============================================================================
% CONSTRAINT STORY: infrastructure_interoperability_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_interoperability_decay, []).

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
 *   constraint_id: infrastructure_interoperability_decay
 *   human_readable: The Protocol Silo Trap
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Protocol Silo Trap describes the scenario where a universal
 *   communication or transport standard (Rope) fragments into proprietary,
 *   incompatible sub-layers as providers seek to 'lock in' users. This leads
 *   to a loss of interoperability, reduced user agency, and potential
 *   stifling of innovation.
 *
 * KEY AGENTS:
 *   - Dominant Platform Providers: Primary beneficiary (institutional/arbitrage) – gains from lock-in.
 *   - End Users: Primary victim (powerless/trapped) – lose interoperability and agency.
 *   - Smaller Innovators: Secondary victim (moderate/constrained) – face challenges in achieving interoperability.
 *   - Standards Bodies: Diminished influence (analytical/analytical) – promote open standards but proprietary solutions dominate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_interoperability_decay, 0.6).
domain_priors:suppression_score(infrastructure_interoperability_decay, 0.7).
domain_priors:theater_ratio(infrastructure_interoperability_decay, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, extractiveness, 0.6).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(infrastructure_interoperability_decay, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_interoperability_decay, tangled_rope).
narrative_ontology:human_readable(infrastructure_interoperability_decay, "The Protocol Silo Trap").
narrative_ontology:topic_domain(infrastructure_interoperability_decay, "technological/economic").

domain_priors:requires_active_enforcement(infrastructure_interoperability_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_interoperability_decay, dominant_platform_providers).
narrative_ontology:constraint_victim(infrastructure_interoperability_decay, end_users).
narrative_ontology:constraint_victim(infrastructure_interoperability_decay, smaller_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% End users, once reliant on a universal standard, find themselves trapped within a platform's ecosystem with limited interoperability, high switching costs, and reduced agency. Experience the constraint as a Snare.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Smaller innovators are constrained by the dominant platform's ecosystem and face challenges in achieving interoperability. However, they also benefit from a large user base provided by dominant platform. Experience the constraint as a Tangled Rope.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Dominant platform providers benefit from locking in users within their siloed ecosystems. They retain the ability to arbitrage by switching to the most advantageous proprietary sub-layer, but still coordinate through the base layer. Experience the constraint as a Rope.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Standards bodies may initially promote open standards but find their influence diminishes as platforms prioritize proprietary solutions, rendering the initial standard a piton.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% The analytical observer recognizes the initial coordination benefits of the standard, the subsequent fragmentation and extraction, and the long-term consequences of reduced interoperability and innovation.
constraint_indexing:constraint_classification(infrastructure_interoperability_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_interoperability_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_interoperability_decay, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_interoperability_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_interoperability_decay, TR),
    TR >= 0.70.

:- end_tests(infrastructure_interoperability_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The dominant platforms extract value from their locked-in user base, suppressing alternatives and charging rents on the ecosystem. Suppression (0.70): High due to network effects and switching costs. Users are effectively locked in. Theater ratio (0.30): Relatively low performative activity. Most effort is directed to genuine product and network development to keep users within the silo. The system benefits dominant firms by network effects.
 *
 * PERSPECTIVAL GAP:
 *   End users experience a Snare as they become trapped within a platform's ecosystem. Dominant platform providers see a Rope because they still co-ordinate through the base layer and can arbitrage between proprietary options. Smaller innovators find themselves in Tangled Rope as they are constrained by but also benefit from larger platform's user base. Standards bodies observe the decay of open standards into a Piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant platform providers benefit (low d) from the lock-in, thus experiencing it as coordination. End users bear the cost of being trapped (high d). The analytical observer captures the full picture to classify. Small innovators have a mixed experience (moderate d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_lock_in_intensity,
    'How strong are network effects and switching costs that create platform lock-in?',
    'Empirical analysis of user behavior, market share dynamics, and switching costs.',
    'High lock-in leads to stronger Snare for users and Tangled Rope for smaller innovators; low lock-in allows for greater user mobility and competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_lock_in_intensity, empirical, 'Intensity of platform lock-in effects').

omega_variable(
    regulatory_intervention_feasibility,
    'Can regulatory interventions effectively promote interoperability without stifling innovation?',
    'Case studies of regulatory interventions in similar markets.',
    'Effective regulation leads to a shift towards Rope or Scaffold; ineffective regulation exacerbates the Snare and Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_feasibility, preference, 'Feasibility of regulatory intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_interoperability_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infr_tr_t0, infrastructure_interoperability_decay, theater_ratio, 0, 0.1).
narrative_ontology:measurement(infr_tr_t5, infrastructure_interoperability_decay, theater_ratio, 5, 0.2).
narrative_ontology:measurement(infr_tr_t10, infrastructure_interoperability_decay, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(infr_be_t0, infrastructure_interoperability_decay, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(infr_be_t5, infrastructure_interoperability_decay, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(infr_be_t10, infrastructure_interoperability_decay, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_interoperability_decay, information_standard).
narrative_ontology:affects_constraint(infrastructure_interoperability_decay, data_portability_constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
