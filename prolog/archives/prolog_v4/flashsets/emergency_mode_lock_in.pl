% ============================================================================
% CONSTRAINT STORY: emergency_mode_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_mode_lock_in, []).

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
 *   constraint_id: emergency_mode_lock_in
 *   human_readable: The Perpetual Crisis Mandate
 *   domain: political/organizational
 *
 * SUMMARY:
 *   A scenario where emergency powers, originally designed for temporary
 *   crisis management, become the permanent operating substrate, creating a
 *   tangled rope characterized by increased executive power and the erosion
 *   of civil liberties. The perpetual state of exception benefits incumbent
 *   leadership and crisis management industry while extracting from civil
 *   liberties, checks and balances, and overall economic productivity.
 *
 * KEY AGENTS:
 *   - Incumbent Leadership: Primary beneficiary (institutional/arbitrage) - Consolidates power through emergency measures.
 *   - Crisis Management Industry: Secondary beneficiary (institutional/arbitrage) - Gains influence and resources due to increased demand.
 *   - Civil Liberties: Primary victim (powerless/trapped) - Suffers erosion due to emergency restrictions.
 *   - Institutional Checks and Balances: Secondary victim (moderate/constrained) - Weakened in their ability to limit executive power.
 *   - Economic Productivity: Victim (moderate/constrained) - Impacted by restrictions and resource misallocation.
 *   - Analytical Observer: Analyzes the overall impact and power dynamics (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_mode_lock_in, 0.55).
domain_priors:suppression_score(emergency_mode_lock_in, 0.65).
domain_priors:theater_ratio(emergency_mode_lock_in, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_mode_lock_in, extractiveness, 0.55).
narrative_ontology:constraint_metric(emergency_mode_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(emergency_mode_lock_in, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_mode_lock_in, tangled_rope).
narrative_ontology:human_readable(emergency_mode_lock_in, "The Perpetual Crisis Mandate").
narrative_ontology:topic_domain(emergency_mode_lock_in, "political/organizational").

domain_priors:requires_active_enforcement(emergency_mode_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, incumbent_leadership).
narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, crisis_management_industry).
narrative_ontology:constraint_victim(emergency_mode_lock_in, civil_liberties).
narrative_ontology:constraint_victim(emergency_mode_lock_in, institutional_checks_and_balances).
narrative_ontology:constraint_victim(emergency_mode_lock_in, economic_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Civil liberties are trapped in the perpetual crisis, with limited ability to exit or challenge the emergency measures.
constraint_indexing:constraint_classification(emergency_mode_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The traditional checks and balances are constrained. They still exist in name, but are weakened and largely performative, unable to effectively limit the executive's power.
constraint_indexing:constraint_classification(emergency_mode_lock_in, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Economic productivity is constrained by the crisis measures. While some sectors may benefit, overall productivity suffers due to restrictions and resource misallocation, but there is limited exit.
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The incumbent leadership benefits from the crisis, consolidating power and circumventing normal political processes, using the situation to their advantage.
constraint_indexing:constraint_classification(emergency_mode_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The crisis management industry benefits from increased demand for their services, gaining influence and resources.
constraint_indexing:constraint_classification(emergency_mode_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer sees the emergency measures as a tangled rope, with some coordination benefits but also significant extraction and suppression of alternatives.
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_mode_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_mode_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_mode_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_mode_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_mode_lock_in, TR),
    TR >= 0.70.

:- end_tests(emergency_mode_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate to high extraction from the population due to restrictions, resource misallocations, and erosion of rights. Suppression: 0.65 - Moderate to high suppression of alternatives and dissent due to the state of exception. Theater ratio: 0.75 - High theater due to constant media and public relations efforts to maintain fear and justify emergency measures.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent leadership and crisis management industry see the emergency measures as a rope, enabling swift and effective action. Civil liberties are trapped in a snare. Institutional checks become pitons. The analytical observer sees the tangled rope, where coordination benefits are intertwined with significant extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent leadership (institutional/arbitrage) and the crisis management industry (institutional/arbitrage) are beneficiaries. Civil liberties (powerless/trapped) and institutional checks (moderate/constrained) are victims. The directionality values reflect these relationships, leading to different classifications from each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how emergency measures, initially intended for coordination (Rope), can morph into a system of extraction and control (Tangled Rope/Snare) due to the incentives of incumbent leadership and the crisis management industry. It highlights the importance of sunset clauses and strong checks and balances to prevent this transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_level_validation,
    'Is the declared crisis a genuine existential threat, or is it being exaggerated for political gain?',
    'Independent verification of the crisis''s severity, comparing official claims with empirical data.',
    'If genuine threat: emergency measures are justified (Rope/Scaffold). If exaggerated: measures are purely extractive (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_level_validation, empirical, 'Validity of declared crisis threat level').

omega_variable(
    alternative_policy_options,
    'Are there less restrictive alternative policies that could address the crisis effectively?',
    'Comparative analysis of different policy options, assessing their costs, benefits, and impact on civil liberties.',
    'If viable alternatives exist: suppression is unjustified (Snare). If not: emergency measures may be necessary (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_policy_options, conceptual, 'Existence of viable alternative policy options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_mode_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emer_tr_t0, emergency_mode_lock_in, theater_ratio, 0, 0.3).
narrative_ontology:measurement(emer_tr_t5, emergency_mode_lock_in, theater_ratio, 5, 0.5).
narrative_ontology:measurement(emer_tr_t10, emergency_mode_lock_in, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(emer_be_t0, emergency_mode_lock_in, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(emer_be_t5, emergency_mode_lock_in, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(emer_be_t10, emergency_mode_lock_in, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_mode_lock_in, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
