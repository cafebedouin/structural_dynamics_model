% ============================================================================
% CONSTRAINT STORY: yangtze_fishing_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_yangtze_fishing_ban, []).

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
 *   constraint_id: yangtze_fishing_ban
 *   human_readable: Yangtze River Fishing Ban
 *   domain: economic, environmental, political
 *
 * SUMMARY:
 *   The Yangtze River Fishing Ban is a permanent ban implemented by the
 *   Chinese government to restore biodiversity. It aims to reverse the
 *   effects of overfishing and ecological damage. The ban has significant
 *   socio-economic impacts, particularly on fishermen who have traditionally
 *   relied on the river for their livelihoods.
 *
 * KEY AGENTS:
 *   - Yangtze Fishermen: Primary target (powerless/trapped)
 *   - Local Governments: Implementing authority (moderate/constrained)
 *   - Central Government of China: Primary beneficiary (institutional/arbitrage)
 *   - Yangtze River Ecosystem: Ultimate beneficiary
 *   - International Environmental Organizations: Monitor and advocate (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yangtze_fishing_ban, 0.55).
domain_priors:suppression_score(yangtze_fishing_ban, 0.7).
domain_priors:theater_ratio(yangtze_fishing_ban, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yangtze_fishing_ban, extractiveness, 0.55).
narrative_ontology:constraint_metric(yangtze_fishing_ban, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(yangtze_fishing_ban, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(yangtze_fishing_ban, tangled_rope).
narrative_ontology:human_readable(yangtze_fishing_ban, "Yangtze River Fishing Ban").
narrative_ontology:topic_domain(yangtze_fishing_ban, "economic, environmental, political").

domain_priors:requires_active_enforcement(yangtze_fishing_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, yangtze_river_ecosystem).
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, future_generations).
narrative_ontology:constraint_victim(yangtze_fishing_ban, yangtze_fishermen).
narrative_ontology:constraint_victim(yangtze_fishing_ban, fish_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Fishermen who have lost their livelihoods due to the ban. They often lack alternative skills and resources for transition. Limited exit options and high dependence on the river make them vulnerable.
constraint_indexing:constraint_classification(yangtze_fishing_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Local governments are responsible for enforcing the ban and providing support to affected fishermen. They face pressure to balance economic development with environmental protection. Constrained by resources and policy directives from the central government.
constraint_indexing:constraint_classification(yangtze_fishing_ban, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The central government benefits from the ban through improved environmental reputation and long-term ecological sustainability. They have the power to implement and enforce the ban across the entire Yangtze River basin. Arbitrage via international praise and long-term resource security.
constraint_indexing:constraint_classification(yangtze_fishing_ban, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% International NGOs play a role in monitoring and advocating for the Yangtze River ecosystem. They can mobilize resources and expertise to support conservation efforts. Has a sunset clause tied to successful ecological restoration, transitioning to monitoring and support.
constraint_indexing:constraint_classification(yangtze_fishing_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Long-term observer analyzing the effectiveness of the ban in restoring biodiversity and the socio-economic impacts on affected communities. Sees both benefits and drawbacks, requiring a nuanced assessment. The fishing ban is an extraction from current fishing communities for the benefit of future generations and the environment, with uncertain long-term benefits. A tangled rope.
constraint_indexing:constraint_classification(yangtze_fishing_ban, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yangtze_fishing_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(yangtze_fishing_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yangtze_fishing_ban, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(yangtze_fishing_ban, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(yangtze_fishing_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because while the ban significantly impacts fishermen, there are attempts to provide alternative livelihoods. Suppression is high due to the strict enforcement of the ban and limited alternative options for fishermen. The theater ratio is relatively low as there's a genuine effort to improve the ecosystem beyond performative actions.
 *
 * PERSPECTIVAL GAP:
 *   The fishermen see the ban as a snare, taking away their livelihoods. The central government views it as a rope, providing long-term benefits to the ecosystem and the nation's environmental reputation. Local governments face a tangled rope situation, balancing enforcement with the need to support affected communities. International environmental organizations view it as a scaffold to preserve nature until the river is healthy again.
 *
 * DIRECTIONALITY LOGIC:
 *   The central government, as the implementer, benefits the most, experiencing the constraint as a rope. The fishermen, bearing the brunt of the economic impact and having limited exit options, experience it as a snare. Local governments are constrained by the central authority but do have a role in how the ban is executed.
 *
 * MANDATROPHY ANALYSIS:
 *   The fishing ban is a complex issue with both benefits and drawbacks, making it difficult to classify as purely extraction or pure coordination. The tangled rope classification reflects this complexity, recognizing the legitimate need to restore the ecosystem while also acknowledging the significant negative impacts on affected communities. It is important to consider the perspectives of all stakeholders involved to fully understand the consequences of the ban.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecosystem_recovery_rate,
    'How quickly will the Yangtze River ecosystem recover?',
    'Long-term monitoring of fish populations, water quality, and biodiversity.',
    'Determines the long-term effectiveness and justification for the ban.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_recovery_rate, empirical, 'Recovery rate of the Yangtze River ecosystem.').

omega_variable(
    livelihood_transition_success,
    'How successful are alternative livelihood programs for affected fishermen?',
    'Tracking employment rates, income levels, and social well-being of transitioned fishermen.',
    'Determines the social cost of the ban and the need for additional support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_transition_success, empirical, 'Success of livelihood transition programs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(yangtze_fishing_ban, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yang_tr_t0, yangtze_fishing_ban, theater_ratio, 0, 0.2).
narrative_ontology:measurement(yang_tr_t5, yangtze_fishing_ban, theater_ratio, 5, 0.3).
narrative_ontology:measurement(yang_tr_t10, yangtze_fishing_ban, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(yang_be_t0, yangtze_fishing_ban, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(yang_be_t5, yangtze_fishing_ban, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(yang_be_t10, yangtze_fishing_ban, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(yangtze_fishing_ban, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
