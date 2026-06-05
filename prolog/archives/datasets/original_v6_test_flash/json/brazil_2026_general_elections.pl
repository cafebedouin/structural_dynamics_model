% ============================================================================
% CONSTRAINT STORY: brazil_2026_general_elections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_2026_general_elections, []).

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
 *   constraint_id: brazil_2026_general_elections
 *   human_readable: 2026 Brazilian General Election Structure
 *   domain: political
 *
 * SUMMARY:
 *   The 2026 Brazilian General Election operates as a complex political
 *   structure characterized by the ongoing tension between established
 *   political forces and emerging challenger movements, set against a
 *   backdrop of socioeconomic disparities and historical political
 *   polarization. The electoral system, while designed to ensure democratic
 *   transitions, also presents challenges in terms of voter access and
 *   equitable representation, leading to a dynamic interplay of coordination
 *   and extraction.
 *
 * KEY AGENTS:
 *   - Established Political Parties: Beneficiaries with resources and established networks.
 *   - Challenger Political Movements: Victims facing barriers to entry and unequal access to resources.
 *   - Disenfranchised Voters: Victims facing systemic barriers to participation.
 *   - Electoral Justice System: Institutional actor with a role in maintaining the system, for better or worse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_2026_general_elections, 0.55).
domain_priors:suppression_score(brazil_2026_general_elections, 0.65).
domain_priors:theater_ratio(brazil_2026_general_elections, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_2026_general_elections, extractiveness, 0.55).
narrative_ontology:constraint_metric(brazil_2026_general_elections, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(brazil_2026_general_elections, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_2026_general_elections, tangled_rope).
narrative_ontology:human_readable(brazil_2026_general_elections, "2026 Brazilian General Election Structure").
narrative_ontology:topic_domain(brazil_2026_general_elections, "political").

domain_priors:requires_active_enforcement(brazil_2026_general_elections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, established_political_parties).
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, electoral_justice_system).
narrative_ontology:constraint_victim(brazil_2026_general_elections, challenger_political_movements).
narrative_ontology:constraint_victim(brazil_2026_general_elections, disenfranchised_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of voters facing systematic barriers to participation, feeling trapped by the existing system.
constraint_indexing:constraint_classification(brazil_2026_general_elections, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of new or smaller political movements trying to gain traction within the existing system, facing constraints but with some agency.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of established political parties benefiting from the current electoral structure and having the resources to navigate it effectively.
constraint_indexing:constraint_classification(brazil_2026_general_elections, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of the electoral justice system, ostensibly impartial but possibly degraded by political influence or institutional inertia, maintaining the status quo.
constraint_indexing:constraint_classification(brazil_2026_general_elections, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees a tangled rope due to the mixed coordination and extraction within the Brazilian electoral system. It coordinates power transition, yet extracts from certain groups.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_2026_general_elections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_2026_general_elections, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brazil_2026_general_elections, TR),
    TR >= 0.70.

:- end_tests(brazil_2026_general_elections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The system extracts from challenger movements and disenfranchised voters due to unequal access to resources and barriers to participation. Suppression (0.65): High barriers for new political actors, combined with voter suppression tactics, create a significant suppression level. Theater ratio (0.75): High level of performative elements due to the highly polarized political climate.
 *
 * PERSPECTIVAL GAP:
 *   The established parties view the system as a rope enabling their continued influence. Challenger movements perceive it as a tangled rope, offering some opportunity but also imposing constraints. Disenfranchised voters experience it as a snare, trapping them in a cycle of political exclusion. The electoral justice system maintains the system, for better or for worse.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural positions of the agents within the electoral system. Established parties, as beneficiaries, have a low 'd' value. Challenger movements have a moderate 'd' value due to the constraints they face. Disenfranchised voters have a high 'd' value, reflecting their experience of being targeted by the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voter_access_inequality,
    'How significantly do geographical and socioeconomic factors affect voter access and participation?',
    'Statistical analysis of voter turnout data across different regions and demographic groups.',
    'If high inequality: reinforces snare perspective and highlights systemic disenfranchisement. If low inequality: suggests the system is more equitable than perceived.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_access_inequality, empirical, 'The degree to which access inequality suppresses voter participation.').

omega_variable(
    electoral_system_reform_viability,
    'What is the likelihood and potential impact of significant electoral system reforms before 2026?',
    'Political analysis of legislative proposals and public opinion regarding electoral reform.',
    'If high viability: could shift the system towards rope or scaffold. If low viability: reinforces the current tangled rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_system_reform_viability, conceptual, 'The probability and effect of electoral reform measures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_2026_general_elections, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(braz_tr_t0, brazil_2026_general_elections, theater_ratio, 0, 0.65).
narrative_ontology:measurement(braz_tr_t3, brazil_2026_general_elections, theater_ratio, 3, 0.7).
narrative_ontology:measurement(braz_tr_t6, brazil_2026_general_elections, theater_ratio, 6, 0.75).

% Extraction over time
narrative_ontology:measurement(braz_be_t0, brazil_2026_general_elections, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(braz_be_t3, brazil_2026_general_elections, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(braz_be_t6, brazil_2026_general_elections, base_extractiveness, 6, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_2026_general_elections, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
