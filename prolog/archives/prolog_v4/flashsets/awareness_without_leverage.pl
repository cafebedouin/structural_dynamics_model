% ============================================================================
% CONSTRAINT STORY: awareness_without_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_awareness_without_leverage, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: awareness_without_leverage
 *   human_readable: The Cassandra Paradox
 *   domain: social/political
 *
 * SUMMARY:
 *   This constraint occurs when an agent possesses high-fidelity information
 *   regarding a systemic failure but lacks any structural mechanism to
 *   influence the outcome. This often leads to a sense of frustration and
 *   helplessness, as the agent is aware of impending danger but unable to
 *   prevent it. The 'Cassandra' of Greek myth was cursed to utter true
 *   prophecies, but never to be believed. This constraint manifests in
 *   numerous contemporary challenges, such as climate change, economic
 *   inequality, and political polarization.
 *
 * KEY AGENTS:
 *   - Informed Public: Victims (moderate/constrained)
 *   - Future Generations: Victims (powerless/trapped)
 *   - Political/Economic Elites: Beneficiaries (institutional/arbitrage) - benefit from maintaining the status quo that leads to systemic failures.
 *   - International Organizations: Moderate (institutional/constrained)
 *   - Analytical Observer: Analytical (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(awareness_without_leverage, 0.55).
domain_priors:suppression_score(awareness_without_leverage, 0.7).
domain_priors:theater_ratio(awareness_without_leverage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(awareness_without_leverage, extractiveness, 0.55).
narrative_ontology:constraint_metric(awareness_without_leverage, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(awareness_without_leverage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(awareness_without_leverage, snare).
narrative_ontology:human_readable(awareness_without_leverage, "The Cassandra Paradox").
narrative_ontology:topic_domain(awareness_without_leverage, "social/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(awareness_without_leverage, informed_public).
narrative_ontology:constraint_victim(awareness_without_leverage, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations bear the costs of inaction on known systemic risks (e.g., climate change) and have no means of influencing present-day decisions.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% An informed segment of the public may be aware of a problem but lacks the political or economic leverage to effect change. They are constrained by existing power structures.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% International organizations may possess high-fidelity data on global systemic risks but lack the enforcement mechanisms or political capital to mandate effective action. The 'action' has become a theatrical performance rather than an actually impactful move.
constraint_indexing:constraint_classification(awareness_without_leverage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% From an analytical perspective, the Cassandra Paradox represents a systemic failure where accurate information is insufficient to drive corrective action. There is an element of coordination in that information is being produced and disseminated, but the lack of leverage and the extraction on future generations indicates it is a tangled rope.
constraint_indexing:constraint_classification(awareness_without_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(awareness_without_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(awareness_without_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(awareness_without_leverage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(awareness_without_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(awareness_without_leverage, TR),
    TR >= 0.70.

:- end_tests(awareness_without_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: High (0.55). The lack of action on known problems extracts a cost from future generations and the informed public. Suppression: High (0.70). Power structures suppress the ability of informed actors to translate awareness into action. Theater ratio: Moderate (0.30). Some performative action exists, but is largely ineffective.
 *
 * PERSPECTIVAL GAP:
 *   Future generations (snare) experience pure extraction as they have no agency. The informed public (snare) are similarly trapped, though they have some limited ability to organize. International organizations (piton) see their own efforts as ineffective and largely performative. The analytical observer (tangled rope) can see the mixed coordination and extraction, and the systemic nature of the problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims have high d values due to their lack of exit options. The analytical observer has a neutral d value. Beneficiaries, if explicitly declared, would have low d values due to their ability to arbitrage the situation. However, no beneficiaries were declared as the primary target is identifying the failure mode.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold,
    'What level of public awareness and concern is needed to trigger meaningful political or economic action?',
    'Empirical analysis of historical cases where awareness led to change vs. cases where it did not; identification of key tipping points.',
    'Determines whether the ''informed public'' can transition from ''powerless'' to ''organized'' and exert influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold, empirical, 'Threshold of public awareness for action.').

omega_variable(
    structural_interventions,
    'What types of structural interventions (e.g., campaign finance reform, alternative media ecosystems) can empower informed actors and increase their leverage?',
    'Comparative analysis of different intervention strategies and their effectiveness in different contexts; development of new governance models.',
    'Identifies potential pathways for escaping the Cassandra Paradox and translating awareness into action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_interventions, conceptual, 'Structural interventions to increase leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(awareness_without_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(awar_tr_t0, awareness_without_leverage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(awar_tr_t5, awareness_without_leverage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(awar_tr_t10, awareness_without_leverage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(awar_be_t0, awareness_without_leverage, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(awar_be_t5, awareness_without_leverage, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(awar_be_t10, awareness_without_leverage, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(awareness_without_leverage, collective_action_problem).
narrative_ontology:affects_constraint(awareness_without_leverage, tragedy_of_the_commons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
