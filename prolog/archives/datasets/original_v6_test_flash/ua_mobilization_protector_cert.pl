% ============================================================================
% CONSTRAINT STORY: ua_mobilization_protector_cert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ua_mobilization_protector_cert, []).

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
 *   constraint_id: ua_mobilization_protector_cert
 *   human_readable: Ukrainian 'Protector' Certificate Mobilization System
 *   domain: political/technological
 *
 * SUMMARY:
 *   The 'Protector' certificate system is a mandatory digital ID for
 *   Ukrainian men to manage mobilization. The system grants the military
 *   increased efficiency, but suppresses the autonomy of Ukrainian men. Civil
 *   liberty groups are victims as the system infringes on fundamental rights,
 *   while the Ministry of Digital Transformation has to balance the system's
 *   utility with public trust.
 *
 * KEY AGENTS:
 *   - Ukrainian man eligible for service: Primary target (powerless/trapped)
 *   - Ukrainian Military: Primary beneficiary (institutional/constrained)
 *   - Ministry of Digital Transformation: Secondary actor (institutional/constrained)
 *   - Civil liberties groups: Organized opposition (organized/mobile)
 *   - Analytical Observer: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_mobilization_protector_cert, 0.6).
domain_priors:suppression_score(ua_mobilization_protector_cert, 0.7).
domain_priors:theater_ratio(ua_mobilization_protector_cert, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, extractiveness, 0.6).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ua_mobilization_protector_cert, snare).
narrative_ontology:human_readable(ua_mobilization_protector_cert, "Ukrainian 'Protector' Certificate Mobilization System").
narrative_ontology:topic_domain(ua_mobilization_protector_cert, "political/technological").

domain_priors:requires_active_enforcement(ua_mobilization_protector_cert).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, ukrainian_military).
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, ministry_of_digital_transformation).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, ukrainian_men_eligible_for_service).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, civil_liberties_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Ukrainian man eligible for military service: The system severely restricts their ability to avoid mobilization, effectively trapping them within its control.  High extraction and limited exit options due to mandatory certificate requirement.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Ukrainian Military: Benefits from increased mobilization efficiency, yet is constrained by potential for social unrest and circumvention attempts. The Protector system coordinates mobilization efforts but also extracts citizen autonomy. The military benefits from the system, but is also constrained by the political and social costs associated with its implementation.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Ministry of Digital Transformation: While benefiting from the expansion of digital governance and control, the ministry is constrained by public trust and potential for misuse of the system. The ministry benefits from enhanced control and data collection but may also face reputational damage and constrained flexibility due to public scrutiny and potential future policy changes. The system has degraded to a piton as it is now more about surveillance and control, with little coordination value.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Civil Liberties Groups: Actively opposing the system but are constrained by the government's security narrative. They can advocate for policy changes and offer assistance to those seeking to circumvent the system, but have only partial mobility. Limited success in altering policy given martial law.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Analytical Observer: Sees a system that provides mobilization benefits but carries significant risks for civil liberties and democratic values. The Protector system serves as a case study in the trade-offs between security and freedom, requiring careful calibration and oversight to prevent abuse. System provides necessary benefits but also carries significant risks for human and democratic values.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_mobilization_protector_cert_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ua_mobilization_protector_cert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ua_mobilization_protector_cert, TR),
    TR >= 0.70.

:- end_tests(ua_mobilization_protector_cert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate at 0.60 due to the significant limitations on individual autonomy. Suppression is high at 0.70 because of the restricted exit options for citizens. The theater_ratio is at 0.30, the system's purported purpose being military readiness, but the potential exists for function creep and abuse.
 *
 * PERSPECTIVAL GAP:
 *   The Ukrainian man sees a Snare, offering no escape. The Ukrainian Military sees a Tangled Rope. The Ministry of Digital Transformation sees a Piton. The Civil Liberties Groups see a Tangled Rope. The Analytical Observer sees a Tangled Rope as the system is an extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The military benefits from increased mobilization efficiency, while Ukrainian men bear the cost of restricted freedom. Civil liberties groups are victims. All actors have limited exit options. High extraction and suppression due to mandatory certificate requirement. Directionality is determined by the degree to which each actor benefits from or is harmed by the system, as well as their ability to influence or circumvent it.
 *
 * MANDATROPHY ANALYSIS:
 *   This system has a high likelihood of classifying as a Snare. The tradeoff between security and freedom is not a one-size-fits-all determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    circumvention_efficacy,
    'How effective are circumvention methods (e.g., VPNs, black market certificates) in bypassing the system?',
    'Analysis of black market data and surveys',
    'High circumvention reduces system effectiveness and undermines legitimacy. Low circumvention enforces compliance, but increases social unrest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circumvention_efficacy, empirical, 'Efficacy of circumvention methods').

omega_variable(
    data_misuse_potential,
    'What is the potential for misuse of the collected data (e.g., political targeting, discrimination)?',
    'Audits of data access and use',
    'High misuse increases civil liberties violations and reduces trust. Low misuse maintains legitimacy and public trust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_misuse_potential, conceptual, 'Potential for data misuse').

omega_variable(
    security_vs_freedom_tradeoff,
    'What level of security benefit justifies the infringement on civil liberties?',
    'Public debate and legislative review',
    'High benefit allows for greater infringement with public support. Low benefit results in public backlash and potential reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_vs_freedom_tradeoff, preference, 'Acceptable security vs. freedom tradeoff').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ua_mobilization_protector_cert, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ua_m_tr_t0, ua_mobilization_protector_cert, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ua_m_tr_t5, ua_mobilization_protector_cert, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ua_m_tr_t10, ua_mobilization_protector_cert, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ua_m_be_t0, ua_mobilization_protector_cert, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ua_m_be_t5, ua_mobilization_protector_cert, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ua_m_be_t10, ua_mobilization_protector_cert, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ua_mobilization_protector_cert, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
