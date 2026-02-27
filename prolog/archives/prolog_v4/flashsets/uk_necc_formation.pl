% ============================================================================
% CONSTRAINT STORY: uk_necc_formation
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_necc_formation, []).

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
 *   constraint_id: uk_necc_formation
 *   human_readable: UK National Economic Crime Centre (NECC) Formation
 *   domain: political/economic
 *
 * SUMMARY:
 *   The UK government is establishing a new law enforcement agency, the
 *   National Economic Crime Centre (NECC), modeled on the American FBI, to
 *   combat economic crimes such as fraud, money laundering, and kleptocracy.
 *   This constraint story examines the intended effects and potential
 *   unintended consequences of this initiative.
 *
 * KEY AGENTS:
 *   - UK Government: Primary beneficiary (institutional/arbitrage) - Establishes and controls the NECC, gaining increased power and legitimacy.
 *   - Economic Criminals: Primary victim (powerless/trapped) - Face increased scrutiny and prosecution.
 *   - Law-Abiding Businesses: Beneficiary (moderate/mobile) - Benefit from a more secure and stable economic environment.
 *   - Citizens: Beneficiary (moderate/constrained) - Benefit from reduced economic crime and increased financial stability.
 *   - Organized Crime Groups: Victim (moderate/constrained) - Face increased law enforcement pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_necc_formation, 0.35).
domain_priors:suppression_score(uk_necc_formation, 0.25).
domain_priors:theater_ratio(uk_necc_formation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_necc_formation, extractiveness, 0.35).
narrative_ontology:constraint_metric(uk_necc_formation, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(uk_necc_formation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_necc_formation, tangled_rope).
narrative_ontology:human_readable(uk_necc_formation, "UK National Economic Crime Centre (NECC) Formation").
narrative_ontology:topic_domain(uk_necc_formation, "political/economic").

domain_priors:requires_active_enforcement(uk_necc_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_necc_formation, uk_government).
narrative_ontology:constraint_beneficiary(uk_necc_formation, law_abiding_businesses).
narrative_ontology:constraint_beneficiary(uk_necc_formation, citizens).
narrative_ontology:constraint_victim(uk_necc_formation, economic_criminals).
narrative_ontology:constraint_victim(uk_necc_formation, organized_crime_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Economic criminals face increased scrutiny and risk of prosecution, with limited options to avoid the NECC's reach.
constraint_indexing:constraint_classification(uk_necc_formation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% The UK government benefits from a centralized agency to combat economic crime, improving its reputation, increasing tax revenues, and maintaining stability in the financial system.
constraint_indexing:constraint_classification(uk_necc_formation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the NECC formation as a tangled rope: it coordinates law enforcement efforts but also extracts resources and power, potentially leading to mission creep or abuse.
constraint_indexing:constraint_classification(uk_necc_formation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Law-abiding businesses benefit from a more secure economic environment and fairer competition.
constraint_indexing:constraint_classification(uk_necc_formation, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_necc_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_necc_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_necc_formation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(uk_necc_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.35 - The NECC extracts resources from the economy through its operating budget. It also extracts freedom from economic criminals through investigation and prosecution. Suppression: 0.25 - The NECC limits the options available to economic criminals and organized crime groups. Theater Ratio: 0.30 - The NECC's activities have a degree of performative action to demonstrate its effectiveness and deter crime, but primarily provides functional work.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap arises because economic criminals view the NECC as a pure snare, while the government views it as a rope for coordinating law enforcement. An analytical observer recognizes the tangled nature of the rope, understanding both coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic stems from the intended design of the NECC to curtail economic crime through increased resources and investigation. The UK Government and law-abiding businesses should benefit while criminals pay the cost. The analytical observer perspective must factor in the potential unintended consequences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mission_creep_scope,
    'Will the NECC''s mandate remain focused on core economic crimes, or will it expand into other areas of law enforcement?',
    'Legislative and budgetary oversight, independent audits, and public reporting on the NECC''s activities.',
    'A narrow scope will limit the NECC''s effectiveness against evolving threats. A broad scope risks overreach and potential abuses of power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_creep_scope, empirical, 'The scope of mission creep within NECC enforcement actions.').

omega_variable(
    resource_allocation_efficiency,
    'Will the NECC be adequately resourced and effectively managed to achieve its objectives?',
    'Performance metrics, cost-benefit analyses, and comparisons to similar agencies in other countries.',
    'Insufficient resources or poor management will undermine the NECC''s effectiveness. Excessive resources could lead to waste and inefficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'The efficiency of resource allocation within NECC operations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_necc_formation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_n_tr_t0, uk_necc_formation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uk_n_tr_t5, uk_necc_formation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(uk_n_tr_t10, uk_necc_formation, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(uk_n_be_t0, uk_necc_formation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(uk_n_be_t5, uk_necc_formation, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(uk_n_be_t10, uk_necc_formation, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_necc_formation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
