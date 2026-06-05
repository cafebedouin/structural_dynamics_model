% ============================================================================
% CONSTRAINT STORY: trump_making_china_great_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_making_china_great_2026, []).

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
 *   constraint_id: trump_making_china_great_2026
 *   human_readable: The Trumpian Post-Western Order
 *   domain: political/economic
 *
 * SUMMARY:
 *   The return of Donald Trump has accelerated a shift from a US-led liberal
 *   international order to a multipolar "post-Western" world. This transition
 *   involves both coordination among rising powers and extraction from the
 *   established order. The decline of US influence and the fragmentation of
 *   international institutions create opportunities for other states to
 *   pursue their interests, but also pose risks to global stability.
 *
 * KEY AGENTS:
 *   - Liberal International Order: Primary victim (powerless/trapped) - Weakened and fragmented system.
 *   - Western Democracies: Secondary victim (institutional/constrained) - Internal divisions and declining influence.
 *   - China: Primary beneficiary (institutional/arbitrage) - Gaining influence and leveraging opportunities.
 *   - Illiberal States: Secondary beneficiary (powerful/mobile) - Finding more space to pursue their interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_making_china_great_2026, 0.55).
domain_priors:suppression_score(trump_making_china_great_2026, 0.45).
domain_priors:theater_ratio(trump_making_china_great_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_making_china_great_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(trump_making_china_great_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(trump_making_china_great_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_making_china_great_2026, tangled_rope).
narrative_ontology:human_readable(trump_making_china_great_2026, "The Trumpian Post-Western Order").
narrative_ontology:topic_domain(trump_making_china_great_2026, "political/economic").

domain_priors:requires_active_enforcement(trump_making_china_great_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_making_china_great_2026, china).
narrative_ontology:constraint_beneficiary(trump_making_china_great_2026, illiberal_states).
narrative_ontology:constraint_victim(trump_making_china_great_2026, liberal_international_order).
narrative_ontology:constraint_victim(trump_making_china_great_2026, western_democracies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The liberal international order is weakened and fragmented, with limited ability to resist the shift towards a multipolar world.
constraint_indexing:constraint_classification(trump_making_china_great_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Western democracies face internal divisions and declining influence, constrained in their ability to shape the global order but still possessing some agency.
constraint_indexing:constraint_classification(trump_making_china_great_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% China benefits from the shift, gaining influence and leveraging opportunities in a multipolar world.
constraint_indexing:constraint_classification(trump_making_china_great_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Illiberal states benefit from the decline of the liberal international order, finding more space to pursue their interests without external constraints.
constraint_indexing:constraint_classification(trump_making_china_great_2026, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees a complex shift with both coordination and extraction elements, where the decline of one order creates opportunities for others, but also poses risks to global stability.
constraint_indexing:constraint_classification(trump_making_china_great_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_making_china_great_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_making_china_great_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_making_china_great_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_making_china_great_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trump_making_china_great_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The shift extracts influence and resources from the existing order, but also creates opportunities for new forms of cooperation and competition. Suppression (0.45): Moderate. The decline of the liberal international order weakens its ability to suppress alternative models, but its norms and institutions still exert some influence. Theater ratio (0.30): Low. The shift is driven by real changes in power dynamics and interests, rather than performative actions.
 *
 * PERSPECTIVAL GAP:
 *   The liberal international order sees the shift as a snare, as it loses influence and cohesion. Western democracies experience it as a tangled rope, as they are constrained by internal divisions but still possess some agency. China and illiberal states see it as a rope, as it provides them with opportunities to expand their influence. The analytical observer sees a complex shift with both coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is based on the structural relationship of each agent to the constraint. The liberal international order is a victim, with limited ability to resist the shift. Western democracies are victims, but with some capacity to influence the outcome. China and illiberal states are beneficiaries, gaining influence and opportunities. The analytical observer sees a complex shift with both coordination and extraction elements.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by recognizing that the shift involves both coordination and extraction. The decline of one order creates opportunities for others, but also poses risks to global stability. The different types reflect the perspectives of different agents with varying structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_leadership_role,
    'Will the US continue to withdraw from its traditional leadership role in the international order?',
    'Observing US foreign policy decisions and commitments.',
    'If yes, the shift to a multipolar world will accelerate; if no, the liberal international order may be revitalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_leadership_role, empirical, 'The future of US leadership in the international order.').

omega_variable(
    china_global_ambitions,
    'What are the true extent and nature of China''s global ambitions?',
    'Analyzing China''s economic and military expansion, and its diplomatic initiatives.',
    'Determines whether China will become a responsible stakeholder or a revisionist power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_global_ambitions, empirical, 'The extent and nature of China''s global ambitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_making_china_great_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trum_tr_t0, trump_making_china_great_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(trum_tr_t5, trump_making_china_great_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(trum_tr_t10, trump_making_china_great_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(trum_be_t0, trump_making_china_great_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trum_be_t5, trump_making_china_great_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(trum_be_t10, trump_making_china_great_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_making_china_great_2026, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
