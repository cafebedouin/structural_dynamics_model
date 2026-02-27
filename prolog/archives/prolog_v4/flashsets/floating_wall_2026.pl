% ============================================================================
% CONSTRAINT STORY: floating_wall_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_floating_wall_2026, []).

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
 *   constraint_id: floating_wall_2026
 *   human_readable: The East China Sea Maritime Militia Barrier
 *   domain: geopolitical/maritime
 *
 * SUMMARY:
 *   China has mobilized massive fleets of fishing vessels to form "floating
 *   walls" near the Sino-Japanese median line. This tactic aims to assert
 *   sovereignty over disputed waters, control access to fishing grounds, and
 *   deter Japanese fishing fleets. The barrier raises regional tensions,
 *   increases the risk of maritime incidents, and challenges the existing
 *   maritime order.
 *
 * KEY AGENTS:
 *   - China Coast Guard: Benefits through increased control (institutional/arbitrage)
 *   - Chinese Fishing Fleets: Benefit through protected fishing grounds (powerful/mobile)
 *   - Japanese Fishing Fleets: Suffer restricted access (powerless/trapped)
 *   - Regional Maritime Security: Faces increased instability (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(floating_wall_2026, 0.55).
domain_priors:suppression_score(floating_wall_2026, 0.7).
domain_priors:theater_ratio(floating_wall_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(floating_wall_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(floating_wall_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(floating_wall_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(floating_wall_2026, tangled_rope).
narrative_ontology:human_readable(floating_wall_2026, "The East China Sea Maritime Militia Barrier").
narrative_ontology:topic_domain(floating_wall_2026, "geopolitical/maritime").

domain_priors:requires_active_enforcement(floating_wall_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(floating_wall_2026, china_coast_guard).
narrative_ontology:constraint_beneficiary(floating_wall_2026, chinese_fishing_fleets).
narrative_ontology:constraint_victim(floating_wall_2026, japanese_fishing_fleets).
narrative_ontology:constraint_victim(floating_wall_2026, regional_maritime_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Japanese fishing fleets are directly impacted by the barrier, limiting their access to fishing grounds and creating a constant threat of confrontation. They have limited exit options due to economic dependence and historical fishing rights.
constraint_indexing:constraint_classification(floating_wall_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Regional maritime security is negatively affected by the barrier, as it increases tensions and the risk of escalation. While maritime security actors have some influence, their exit options are limited by the need to maintain regional stability.
constraint_indexing:constraint_classification(floating_wall_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The China Coast Guard benefits from the barrier as it extends their operational range and strengthens their control over disputed waters. They have arbitrage options as they can re-deploy resources to other areas if the barrier is challenged.
constraint_indexing:constraint_classification(floating_wall_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Chinese fishing fleets benefit from the barrier as it provides them with protected access to fishing grounds and support from the coast guard. They have some mobility within the region, but are economically dependent on fishing.
constraint_indexing:constraint_classification(floating_wall_2026, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From an analytical perspective, the barrier represents a complex interplay of strategic interests, economic factors, and historical claims. It serves as a tool for asserting sovereignty but also carries the risk of escalation and destabilization.
constraint_indexing:constraint_classification(floating_wall_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(floating_wall_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(floating_wall_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(floating_wall_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(floating_wall_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(floating_wall_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The barrier extracts resources and access from Japanese fishing fleets, giving Chinese fleets a protected zone. Suppression is high due to the sheer number of vessels involved and the coast guard's backing. Theater ratio is relatively low, as the primary purpose is resource control, with signaling as a secondary concern.
 *
 * PERSPECTIVAL GAP:
 *   Japanese fleets experience a snare, as they are directly impacted and cannot easily exit. Regional maritime security sees a tangled rope, as the barrier both challenges and reinforces existing power dynamics. China's coast guard and fishing fleets perceive the barrier as a rope, coordinating and protecting their interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the relative power and exit options of the actors involved. Japanese fleets have limited power and exit options, resulting in a high d value. China's coast guard has significant power and arbitrage options, resulting in a low d value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barrier_sustainability,
    'How sustainable is the barrier in the long term, considering the economic and environmental costs?',
    'Analysis of the economic impact on Chinese fishing fleets, environmental impact assessments, and cost-benefit analysis.',
    'If unsustainable, the barrier may be abandoned or scaled down, reducing its impact on regional security. If sustainable, it may become a permanent feature of the East China Sea.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_sustainability, empirical, 'Sustainability of the barrier in the long term.').

omega_variable(
    escalation_threshold,
    'At what point does the barrier trigger a military response from Japan or other regional actors?',
    'Analysis of Japanese military doctrine, diplomatic statements, and historical responses to similar actions.',
    'If the threshold is low, the barrier could lead to a military confrontation. If the threshold is high, the barrier may be tolerated as a fait accompli.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_threshold, empirical, 'Threshold for military response to the barrier.').

omega_variable(
    legal_status,
    'What is the legal status of the barrier under international law, and how might this affect its legitimacy and acceptance?',
    'Legal analysis of maritime law, historical precedents, and opinions of international legal scholars.',
    'If the barrier is deemed illegal, it may face international condemnation and pressure to be removed. If it is deemed legal, it may be more widely accepted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_status, conceptual, 'Legal status of the barrier under international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(floating_wall_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(floa_tr_t0, floating_wall_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(floa_tr_t5, floating_wall_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(floa_tr_t10, floating_wall_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(floa_be_t0, floating_wall_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(floa_be_t5, floating_wall_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(floa_be_t10, floating_wall_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(floating_wall_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(floating_wall_2026, senkaku_islands_dispute).
narrative_ontology:affects_constraint(floating_wall_2026, south_china_sea_claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
