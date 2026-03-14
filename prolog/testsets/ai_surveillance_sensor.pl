% ============================================================================
% CONSTRAINT STORY: ai_surveillance_sensor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_surveillance_sensor, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_surveillance_sensor
 *   human_readable: AI Surveillance Sensor Infrastructure
 *   domain: technology/governance/privacy
 *
 * SUMMARY:
 *   AI surveillance sensor infrastructure represents a structural constraint
 *   that extracts behavioral control and privacy from populations while
 *   providing genuine but asymmetric coordination functions (public safety,
 *   infrastructure management, emergency response). The constraint exhibits
 *   characteristics of pure extraction (snare) from the perspective of
 *   surveilled populations with no viable exit; mixed coordination and
 *   extraction (tangled rope) from organized resistance movements; and
 *   coordination (rope) from surveillance apparatus operators. The
 *   extractiveness metric (0.68) reflects that the surveillance function,
 *   while operationally effective, extracts information asymmetrically and
 *   enables manipulation at scale. The suppression metric (0.72) reflects
 *   multiple reinforcing barriers: technical (surveillance is hidden), legal
 *   (weak data protection), informational (citizens cannot assess exposure),
 *   and internalized (normalization for digital natives). The constraint has
 *   degraded over the 15-year measurement interval as scope creep accumulates
 *   (security → commercial profiling → political targeting) and regulatory
 *   theater (GDPR, state privacy laws) proves ineffective at preventing data
 *   misuse. The theater ratio increase (0.32 → 0.58 → 0.68) reflects growing
 *   gap between regulatory appearance and operational reality.
 *
 * KEY AGENTS:
 *   - Surveilled populations: Primary victims (powerless/trapped) — cannot exit without abandoning essential civic/economic participation; bear full extraction cost
 *   - Surveillance apparatus operators: Primary beneficiaries (institutional/arbitrage) — police, intelligence agencies, military; benefit from data access and operational efficiency
 *   - Data aggregation platforms: Secondary beneficiaries (powerful/mobile) — commercial actors extracting behavioral data for targeting and manipulation; provide some coordination (fraud detection) alongside extraction
 *   - Privacy rights organizations: Organized victims (organized/constrained) — ACLU, EFF, international privacy coalitions; can mobilize but cannot fully exit or prevent deployment
 *   - Digital natives: Special victim class (powerless/identity_locked) — primary socialization within surveillance infrastructure; identity fused with trackable presence
 *   - Regulatory agencies: Degraded institutional actors (institutional/arbitrage) — maintain legal framework that is systematically violated; captured or outmatched by operators
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — sees constraint as snare at all scales; exit capacity approaches zero for modern populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_surveillance_sensor, 0.68).
domain_priors:suppression_score(ai_surveillance_sensor, 0.72).
domain_priors:theater_ratio(ai_surveillance_sensor, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_surveillance_sensor, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_surveillance_sensor, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_surveillance_sensor, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_surveillance_sensor, snare).
narrative_ontology:human_readable(ai_surveillance_sensor, "AI Surveillance Sensor Infrastructure").
narrative_ontology:topic_domain(ai_surveillance_sensor, "technology/governance/privacy").

domain_priors:requires_active_enforcement(ai_surveillance_sensor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_surveillance_sensor, surveillance_apparatus_operators).
narrative_ontology:constraint_beneficiary(ai_surveillance_sensor, data_aggregation_platforms).
narrative_ontology:constraint_victim(ai_surveillance_sensor, surveilled_populations).
narrative_ontology:constraint_victim(ai_surveillance_sensor, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Cannot exit the sensor infrastructure without abandoning basic civic participation (banking, transportation, employment, healthcare). Bears full extraction cost through behavioral modification, manipulation, and loss of privacy. Trapped by material dependence on surveilled systems with no viable alternatives.
constraint_indexing:constraint_classification(ai_surveillance_sensor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NORMALIZED DIGITAL NATIVE (SNARE) — Structurally mobile but identity-locked through primary socialization within surveillance infrastructure. Cannot imagine privacy as a lived state; their identity is constituted through trackable digital presence. Exit would require abandoning not just the system but their self-concept as a connected, documented person. Suppression is partially internalized.
constraint_indexing:constraint_classification(ai_surveillance_sensor, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: SURVEILLANCE APPARATUS OPERATOR (ROPE) — Benefits directly from sensor deployment through data access, operational efficiency, and threat identification. Experiences the constraint as coordination: real-time situational awareness solves genuine coordination problems (traffic management, public safety response, infrastructure monitoring). Net beneficiary with institutional capacity and arbitrage options.
constraint_indexing:constraint_classification(ai_surveillance_sensor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY RIGHTS COALITION (TANGLED ROPE) — Organized agent (constrained, not trapped) facing resource limits and legal barriers but retaining agency. Experiences mixed coordination (sensor data does enable some public safety functions) and extraction (scope creep, data misuse, normalization of total surveillance). Can mobilize but cannot fully exit; works within and against the system simultaneously.
constraint_indexing:constraint_classification(ai_surveillance_sensor, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Data protection regulations (GDPR, state privacy laws) persist as ceremonial compliance theater. Collection and retention vastly exceed what regulations formally permit, but technical enforcement is weak. The regulatory framework has become degraded apparatus — maintained through institutional inertia despite known inadequacy. Theater ratio high because the rules are visibly flouted without consequences.
constraint_indexing:constraint_classification(ai_surveillance_sensor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DATA AGGREGATION PLATFORM (TANGLED ROPE) — Commercial actor with mobile exit options (can relocate operations, can shift business models) but deeply integrated into surveillance infrastructure. Benefits from sensor data feed. Provides coordination function (targeted emergency alerts, fraud prevention, legitimate service personalization) alongside extraction (behavioral targeting, manipulation at scale, shadow profile construction). Powerful agent with genuine exit capacity but choosing deepened integration.
constraint_indexing:constraint_classification(ai_surveillance_sensor, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/universal view, AI surveillance sensors represent systematic extraction from populations through technical means that exceed transparency capacity and regulatory capacity. The constraint is snare-like at all scales: individual sensor events aggregate to permanent records; individual consent exceptions aggregate to universal monitoring. No exit except disconnection from modern infrastructure. High suppression because the technical and informational asymmetries prevent meaningful resistance.
constraint_indexing:constraint_classification(ai_surveillance_sensor, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_surveillance_sensor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_surveillance_sensor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_surveillance_sensor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_surveillance_sensor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_surveillance_sensor, TR),
    TR >= 0.70.

:- end_tests(ai_surveillance_sensor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Initial deployment (t=0, ε≈0.38) focused on genuine security/safety coordination with moderate extraction. Mid-period (t=5, ε≈0.52) shows scope creep toward commercial use and behavioral profiling. Current state (t=10-15, ε≈0.68-0.72) reflects systematic expansion beyond stated purposes, shadow profile construction, manipulation at scale, and political applications. The metric reflects not technical capability alone but the extractive use of that capability. Suppression (0.72): High and stable. Technical suppression (hidden sensors, opaque algorithms), legal suppression (weak enforcement of existing rules), informational suppression (citizens cannot assess their own exposure), and psychological suppression (normalization, particularly for digital natives). Theater ratio (0.32 → 0.68): Sharply increasing. Regulatory framework (GDPR, state privacy laws, transparency reports) provides appearance of control while collection and use vastly exceed formal permissions. The gap between regulatory theater and operational reality has widened as AI capabilities and scope have expanded.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence: Surveillance operators experience rope (coordination function for public safety; data enables legitimate threat detection and emergency response). Surveilled populations experience snare (no exit, maximum extraction, behavioral modification through asymmetric information). Privacy coalitions experience tangled rope (real coordination benefits mixed with real extraction; can organize but cannot prevent deployment). Regulatory agencies experience piton (maintain ceremonial compliance framework that everyone knows is inadequate; enforcement is degraded). Digital natives experience snare with internalized suppression (cannot imagine exit; have fused their identity with transparent surveillance). Analytical observer experiences snare at all scales (aggregation of individual sensors creates panopticon; no exit except disconnection from modern infrastructure). The gap between beneficiary and victim perspectives is near-maximal: operators see valuable coordination; populations see pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position within the extraction flow. Surveilled powerless populations with trapped exit (no viable alternative to surveilled systems) occupy d ≈ 0.92-0.98 (near-maximum target). Surveillance operators with institutional power and arbitrage options occupy d ≈ 0.08-0.15 (near-maximum beneficiary). Privacy coalitions that are organized but constrained occupy d ≈ 0.55-0.65 (moderate target, some agency). Data platforms that are powerful but intentionally integrated occupy d ≈ 0.35-0.45 (moderate extraction despite exit capacity). Digital natives with identity lock occupy d ≈ 0.85-0.90 (high target status despite structural mobility, because identity fusion prevents meaningful exit). The regulatory framework, despite nominal institutional power, has experienced d-creep: captured by operators, it functions as a tool of the extractive apparatus rather than as a counterbalance. Effective d for regulators is high (~0.70) despite formal power.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY (extractiveness > 0.70): The constraint classifies as snare despite genuine coordination functions (public safety, infrastructure management, emergency response) because the extractive mechanism (behavioral control, manipulation, political targeting, information asymmetry) has become dominant and structural. The coordination is real but insufficient to offset the extraction. Snare classification is appropriate because: (1) victims (surveilled populations) have no meaningful exit; (2) suppression is high and multifaceted (technical, legal, informational, psychological); (3) existence relies on suppressing privacy alternatives (decentralized/private-preserving architectures remain underfunded and deliberately marginalized); (4) beneficiaries (surveillance apparatus, data platforms) have strong incentives to expand scope and deepen extraction; (5) regulatory framework designed to limit extraction has been rendered performative. The false-summit risk (classification as mountain — surveillance is inherent to complex societies) is rejected: surveillance infrastructure is contingent institutional choice, not natural law. The false-coordination risk (classification as tangled rope or rope) is rejected: the coordination benefits are real but are overwhelmed by extractive mechanisms. The constraint is snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_fictiveness,
    'Is individual consent to sensor data collection meaningful when the alternative is exclusion from essential systems?',
    'Empirical measurement of actual consent rates; analysis of genuine exit capacity (can individuals refuse without material harm?); comparison with coerced consent models in other domains',
    'If consent is fictive: suppression metric should be higher (~0.85), pushing classification toward maximum snare. If consent is meaningful: suppression could be lower (~0.60), potentially allowing tangled rope classification from some perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_fictiveness, empirical, 'Whether consent mechanisms are functionally meaningful or purely performative').

omega_variable(
    scope_creep_inevitability,
    'Is the expansion of surveillance scope (from stated security use to commercial profiling to political control) a contingent institutional failure or inherent to sensor architecture?',
    'Comparative analysis of surveillance systems with strong data governance vs. weak governance; identification of technical barriers vs. policy barriers to scope creep; historical trajectory analysis',
    'If contingent: regulation and governance reform could reduce extractiveness to rope/scaffold range. If inherent: extractiveness ~0.68 is structural minimum; no policy reform can fix fundamental architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_inevitability, empirical, 'Whether surveillance scope creep is contingent or architecturally necessary').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For digital natives, is the identity lock driven by genuine preference (they prefer transparent living) or by internalized suppression (they cannot imagine privacy)?',
    'Longitudinal psychological study of privacy attitudes across generational cohorts; analysis of stated preferences vs. revealed preferences (how people respond when offered genuine privacy); cross-cultural comparison with low-surveillance societies',
    'If genuine preference: identity_locked classification is incorrect; should be constrained or mobile. If internalized suppression: identity_locked is accurate; indicates extraction mechanism operates through cognitive capture rather than just material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether digital native identity lock reflects preference or internalized suppression').

omega_variable(
    regulatory_capture_depth,
    'Are data protection regulators captured by surveillance apparatus operators (intentional capture) or simply outmatched by technical complexity (structural capture)?',
    'Analysis of regulator hiring patterns; correlation between regulator enforcement and operator lobbying; comparative strength of regulators in jurisdictions with strong independence norms vs. weak independence',
    'If intentional capture: piton classification is accurate — regulations are deliberately made performative. If structural capture: regulations could theoretically be strengthened; piton theater_ratio might be lower with better technical capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether regulatory capture is intentional or structural').

omega_variable(
    alternative_infrastructure_viability,
    'Can decentralized, privacy-preserving sensor networks perform equivalent coordination functions (public safety, traffic management, emergency response) as centralized AI surveillance?',
    'Technical comparison of distributed vs. centralized sensor architectures; pilot programs testing privacy-preserving alternatives; analysis of coordination problems that require centralization vs. those that don''t',
    'If viable alternatives exist: extractiveness could drop to ~0.35 (rope range) through architectural substitution. If centralization is necessary: extractiveness ~0.68 is inescapable structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_viability, empirical, 'Whether viable alternatives to centralized surveillance exist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_surveillance_sensor, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aisurveil_tr_t0, ai_surveillance_sensor, theater_ratio, 0, 0.32).
narrative_ontology:measurement(aisurveil_tr_t5, ai_surveillance_sensor, theater_ratio, 5, 0.45).
narrative_ontology:measurement(aisurveil_tr_t10, ai_surveillance_sensor, theater_ratio, 10, 0.58).
narrative_ontology:measurement(aisurveil_tr_t15, ai_surveillance_sensor, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(aisurveil_be_t0, ai_surveillance_sensor, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aisurveil_be_t5, ai_surveillance_sensor, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(aisurveil_be_t10, ai_surveillance_sensor, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(aisurveil_be_t15, ai_surveillance_sensor, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_surveillance_sensor, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_surveillance_sensor, digital_identity_infrastructure).
narrative_ontology:affects_constraint(ai_surveillance_sensor, behavioral_data_commodification).
narrative_ontology:affects_constraint(ai_surveillance_sensor, state_capacity_centralization).

% DUAL FORMULATION NOTE:
% AI surveillance sensor infrastructure is upstream of downstream constraints: behavioral targeting relies on sensor data; political control via digital surveillance feeds upstream extraction. The sensor infrastructure itself can be decomposed into technical coordination function (hardware/software for legitimate monitoring) vs. extractive function (scope creep, commercial use, political targeting). This story models the integrated constraint; technical architecture decomposition would be separate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_surveillance_sensor, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
