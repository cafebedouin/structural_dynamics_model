% ============================================================================
% CONSTRAINT STORY: ai_driven_surveillance_sensor_layer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_driven_surveillance_sensor_layer, []).

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
 *   constraint_id: ai_driven_surveillance_sensor_layer
 *   human_readable: AI-Driven Real-Time Surveillance Sensor Layer
 *   domain: technological/security/political
 *
 * SUMMARY:
 *   This constraint represents the 'sensor layer' of modern governance—facial
 *   recognition, gait analysis, and automated behavioral detection. It
 *   examines the power dynamics and trade-offs involved in AI-driven
 *   real-time surveillance, particularly focusing on the impact on individual
 *   liberties and the potential for abuse. The system is deployed in global
 *   "smart cities".
 *
 * KEY AGENTS:
 *   - National Security Agencies: Primary beneficiary (institutional/arbitrage) — benefits from enhanced intelligence gathering.
 *   - Law Enforcement Agencies: Secondary beneficiary (moderate/constrained) — benefits from enhanced crime detection, but is also subject to oversight.
 *   - Monitored Citizen: Primary victim (powerless/trapped) — bears the cost of lost privacy and curtailed freedom.
 *   - Dissident Groups: Targeted victims (organized/mobile) – experience extraction, but have options for counter-surveillance.
 *   - Judicial Oversight Bodies: Institutional (institutional/constrained) – Intended to provide checks and balances, but effectiveness is degrading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, 0.6).
domain_priors:suppression_score(ai_driven_surveillance_sensor_layer, 0.7).
domain_priors:theater_ratio(ai_driven_surveillance_sensor_layer, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, extractiveness, 0.6).
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_driven_surveillance_sensor_layer, tangled_rope).
narrative_ontology:human_readable(ai_driven_surveillance_sensor_layer, "AI-Driven Real-Time Surveillance Sensor Layer").
narrative_ontology:topic_domain(ai_driven_surveillance_sensor_layer, "technological/security/political").

domain_priors:requires_active_enforcement(ai_driven_surveillance_sensor_layer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_driven_surveillance_sensor_layer, national_security_agencies).
narrative_ontology:constraint_beneficiary(ai_driven_surveillance_sensor_layer, law_enforcement_agencies).
narrative_ontology:constraint_victim(ai_driven_surveillance_sensor_layer, civil_liberties).
narrative_ontology:constraint_victim(ai_driven_surveillance_sensor_layer, privacy_rights).
narrative_ontology:constraint_victim(ai_driven_surveillance_sensor_layer, dissident_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Monitored Citizen (Snare). Powerless and trapped within the surveillance system, with no viable exit. Bears the full cost of lost privacy and curtailed freedom.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Law Enforcement Agencies (Tangled Rope). Benefits from enhanced crime detection and prevention, but is also constrained by the need for oversight and accountability. Coordination (crime reduction) is genuine but asymmetric extraction is present (potential for abuse).
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: National Security Agencies (Rope). Benefits from increased intelligence gathering and threat detection. The sensor layer enables the agency to perform their coordination function.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Dissident Groups (Tangled Rope). Targeted by the surveillance system, hindering their ability to organize and communicate. Extraction is high, but mobility allows for some countermeasures. Benefits are negligible.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 5: Judicial Oversight Bodies (Piton). Intended to provide checks and balances, but increasingly unable to keep pace with the rapid advancements and deployment of AI surveillance technologies. High theater_ratio. Constrained exit options.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical perspective viewing the system as a tangled rope. High extractiveness. There are beneficiaries and victims. Requires active enforcement.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_driven_surveillance_sensor_layer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_driven_surveillance_sensor_layer, TR),
    TR >= 0.70.

:- end_tests(ai_driven_surveillance_sensor_layer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. AI surveillance extracts significant amounts of personal data and behavioral information, potentially chilling free expression and assembly. The constant monitoring and analysis of individuals' actions constitute a substantial intrusion. Suppression (0.70): High. AI surveillance systems suppress alternative behaviors by creating a chilling effect on freedom of expression and assembly. Individuals are less likely to engage in activities that might draw unwanted attention from authorities. Theater ratio (0.30): Low. AI-driven surveillance does produce real results for national security, law enforcement, crime reduction and intelligence gathering.
 *
 * PERSPECTIVAL GAP:
 *   The Monitored Citizen experiences a snare, feeling trapped and powerless. Law Enforcement sees a Tangled Rope, gaining crime-fighting tools but also being constrained. National Security agencies see a Rope, gaining intelligence-gathering power. Dissident groups experience a tangled rope because they are targeted. Judicial Oversight sees a Piton because oversight capabilities are failing.
 *
 * DIRECTIONALITY LOGIC:
 *   The national security agencies are considered beneficiaries. Citizens are victims. Law enforcement is moderately benefited, but still constrained. Organized groups are both targets and have counter-surveillance mobile capabilities. The directionality calculation is derived from benefit-victim status coupled with exit power.
 *
 * MANDATROPHY ANALYSIS:
 *   AI surveillance resolves the mandatrophy problem by recognizing that real gains in law enforcement do not remove the rights of citizens. The analytical perspective accounts for the gains and harms of the perspective, recognizing that the system is a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_bias_amplification,
    'To what extent does biased training data amplify existing social inequalities through AI surveillance systems?',
    'Statistical analysis of demographic representation in training datasets and resulting disparities in surveillance outcomes.',
    'If high: Surveillance disproportionately targets marginalized communities (Snare). If low: Benefits and harms are more evenly distributed (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_bias_amplification, empirical, 'Assess the degree of bias amplification in the AI system.').

omega_variable(
    mission_creep_likelihood,
    'How likely is the expansion of AI surveillance beyond its initially intended scope (e.g., from counter-terrorism to petty crime enforcement)?',
    'Analysis of policy documents, budget allocations, and documented use cases of deployed surveillance systems.',
    'If high: Increased potential for abuse and erosion of civil liberties (Snare). If low: Scope remains limited and oversight is effective (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_creep_likelihood, empirical, 'Likelihood of surveillance mission creep.').

omega_variable(
    effectiveness_vs_privacy_tradeoff,
    'What is the actual crime reduction or threat detection effectiveness gained per unit of privacy lost through AI surveillance?',
    'Comparative analysis of crime statistics and threat levels in areas with and without AI surveillance, controlling for other factors.',
    'If high: Benefits outweigh costs (Rope). If low: Costs outweigh benefits (Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_vs_privacy_tradeoff, empirical, 'Measure the effectiveness/privacy tradeoff.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_driven_surveillance_sensor_layer, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_d_tr_t0, ai_driven_surveillance_sensor_layer, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_d_tr_t3, ai_driven_surveillance_sensor_layer, theater_ratio, 3, 0.2).
narrative_ontology:measurement(ai_d_tr_t6, ai_driven_surveillance_sensor_layer, theater_ratio, 6, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_d_be_t0, ai_driven_surveillance_sensor_layer, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_d_be_t3, ai_driven_surveillance_sensor_layer, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(ai_d_be_t6, ai_driven_surveillance_sensor_layer, base_extractiveness, 6, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_driven_surveillance_sensor_layer, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_driven_surveillance_sensor_layer, predictive_policing_algorithms).
narrative_ontology:affects_constraint(ai_driven_surveillance_sensor_layer, social_credit_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
