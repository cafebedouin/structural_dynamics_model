% ============================================================================
% CONSTRAINT STORY: individual_revolution_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_revolution_autonomy, []).

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
 *   constraint_id: individual_revolution_autonomy
 *   human_readable: The One-Man Revolution
 *   domain: political/social
 *
 * SUMMARY:
 *   This constraint explores the narrative that individual autonomy and
 *   'one-man revolutions' are the most effective path to social and political
 *   change, contrasting it with the pursuit of mass movements and collective
 *   action. It identifies a structural tension where the emphasis on
 *   individual solutions may inadvertently benefit incumbent power structures
 *   by diverting resources and energy away from organized challenges.
 *
 * KEY AGENTS:
 *   - Individual Revolutionary Actors: Primary victims (powerless/trapped) - those who seek radical social change through individual actions.
 *   - Potential Mass Movements: Secondary victims (moderate/constrained) - the collective potential undermined by the focus on individualism.
 *   - Incumbent Power Structures: Primary beneficiaries (institutional/arbitrage) - existing hierarchies that benefit from the division of potential opposition.
 *   - The Academic Analyst: Analytical perspective that sees the 'individual revolution' as a distraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_revolution_autonomy, 0.55).
domain_priors:suppression_score(individual_revolution_autonomy, 0.6).
domain_priors:theater_ratio(individual_revolution_autonomy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_revolution_autonomy, extractiveness, 0.55).
narrative_ontology:constraint_metric(individual_revolution_autonomy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(individual_revolution_autonomy, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_revolution_autonomy, tangled_rope).
narrative_ontology:human_readable(individual_revolution_autonomy, "The One-Man Revolution").
narrative_ontology:topic_domain(individual_revolution_autonomy, "political/social").

domain_priors:requires_active_enforcement(individual_revolution_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_revolution_autonomy, incumbent_power_structures).
narrative_ontology:constraint_victim(individual_revolution_autonomy, individual_revolutionary_actors).
narrative_ontology:constraint_victim(individual_revolution_autonomy, potential_mass_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Individual Revolutionary Actor (Snare) - Isolated individuals seeking radical social change often find themselves trapped by the overwhelming power of existing structures, facing social ostracism, legal repercussions, and a lack of resources. They are the primary target of the constraint, bearing the full cost of attempting revolution.
constraint_indexing:constraint_classification(individual_revolution_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Potential Mass Movements (Tangled Rope) - The focus on individual autonomy can fragment potential mass movements, hindering their ability to coalesce and challenge incumbent powers effectively. While individual autonomy is valuable, its prioritization over collective action can inadvertently strengthen existing hierarchies. These movements have some capacity to affect change, but are constrained by power structures.
constraint_indexing:constraint_classification(individual_revolution_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Incumbent Power Structures (Rope) - Existing power structures benefit from the narrative that individual autonomy is the most effective path to revolution. This diverts resources and energy away from organized collective action, which poses a more direct threat to their dominance. Focus on individual revolutions helps them to maintain their status quo through dividing any potential opposition.
constraint_indexing:constraint_classification(individual_revolution_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: The Academic Analyst (Piton) - Sees the 'individual revolution' as a degraded form of social change. A concept that once held promise now serves primarily as a distraction from structural inequalities. The emphasis on individual responsibility masks underlying systemic issues.
constraint_indexing:constraint_classification(individual_revolution_autonomy, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_revolution_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_revolution_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_revolution_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_revolution_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_revolution_autonomy, TR),
    TR >= 0.70.

:- end_tests(individual_revolution_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The constraint extracts resources and energy from potential mass movements, diverting them towards individualistic pursuits that are less likely to challenge the status quo. Suppression (0.60): Moderate. The narrative suppresses the idea that collective action is necessary for large-scale social change. Theater Ratio (0.40): Moderate-low. There is some performative aspect to advocating for individual revolution as an approach to solving systemic issues.
 *
 * PERSPECTIVAL GAP:
 *   The individual revolutionary actor experiences this as a snare, feeling trapped and powerless against the system. Incumbent power structures experience it as a rope, facilitating their continued dominance. Potential mass movements experience it as a tangled rope, with individual autonomy undermining the collectivism necessary for revolution. An academic analyst sees it as a piton: a relic of a bygone era.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic relies on the agents' structural positions. Individual revolutionary actors have low power and no exit options, leading to a snare classification. Incumbent power structures have high power and arbitrage opportunities, resulting in a rope. Potential mass movements are somewhat constrained and powerful, allowing them to be categorized as a tangled rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_effectiveness_collective_action,
    'Is organized collective action more effective than individual autonomy in achieving large-scale social and political change?',
    'Historical analysis of successful social movements and revolutions compared to the impact of individualistic approaches.',
    'If collective action is more effective: The ''one-man revolution'' narrative serves as a distraction. If individual autonomy is more effective: Incumbent powers face challenges to their authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_effectiveness_collective_action, empirical, 'Determining whether collective action or individual autonomy is more effective in causing major social and political change.').

omega_variable(
    tradeoff_individual_autonomy_collective_good,
    'To what extent does the pursuit of individual autonomy undermine the potential for collective action and the pursuit of the collective good?',
    'Sociological studies on the effects of individualism on social cohesion and political engagement.',
    'If individual autonomy undermines collective action: The constraint is highly extractive. If it does not undermine collective action: The constraint is less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradeoff_individual_autonomy_collective_good, empirical, 'The effect that individual autonomy has on potential collective action and its role in a common good.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_revolution_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, individual_revolution_autonomy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(indi_tr_t5, individual_revolution_autonomy, theater_ratio, 5, 0.3).
narrative_ontology:measurement(indi_tr_t10, individual_revolution_autonomy, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, individual_revolution_autonomy, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(indi_be_t5, individual_revolution_autonomy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(indi_be_t10, individual_revolution_autonomy, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_revolution_autonomy, information_standard).
narrative_ontology:affects_constraint(individual_revolution_autonomy, collective_action_problem).
narrative_ontology:affects_constraint(individual_revolution_autonomy, power_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
