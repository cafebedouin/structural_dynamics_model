% ============================================================================
% CONSTRAINT STORY: social_media_participation_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_media_participation_threshold, []).

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
 *   constraint_id: social_media_participation_threshold
 *   human_readable: The 2025 Digital Participation Threshold
 *   domain: social/technological
 *
 * SUMMARY:
 *   As of 2025, major social media platforms function as de facto mandatory
 *   infrastructure for social and informational participation. Individuals
 *   excluded from these platforms face significant barriers to civic
 *   engagement, economic opportunity, and social inclusion. This creates a
 *   tangled rope dynamic where platforms and advertisers benefit from
 *   increased participation, while users are subject to data extraction and
 *   manipulation, and the digitally excluded are left behind.
 *
 * KEY AGENTS:
 *   - Social Media Platforms: Primary beneficiaries (institutional/arbitrage) - benefit from increased user participation and data collection.
 *   - Advertising Networks: Secondary beneficiaries (institutional/arbitrage) - benefit from the data and reach provided by social media platforms.
 *   - Digitally Excluded: Primary victims (powerless/trapped) - lack access to essential information and services.
 *   - Informed Public Discourse: Secondary victims (powerless/constrained) - fragmented and manipulated by algorithms and echo chambers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_media_participation_threshold, 0.55).
domain_priors:suppression_score(social_media_participation_threshold, 0.6).
domain_priors:theater_ratio(social_media_participation_threshold, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_media_participation_threshold, extractiveness, 0.55).
narrative_ontology:constraint_metric(social_media_participation_threshold, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(social_media_participation_threshold, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_media_participation_threshold, tangled_rope).
narrative_ontology:human_readable(social_media_participation_threshold, "The 2025 Digital Participation Threshold").
narrative_ontology:topic_domain(social_media_participation_threshold, "social/technological").

domain_priors:requires_active_enforcement(social_media_participation_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, social_media_platforms).
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, advertising_networks).
narrative_ontology:constraint_victim(social_media_participation_threshold, digitally_excluded).
narrative_ontology:constraint_victim(social_media_participation_threshold, informed_public_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Digitally Excluded (Snare). For individuals without access or ability to use social media, participation in modern society is significantly hampered. They are trapped without viable alternatives for accessing essential information and services.
constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: The Average Social Media User (Tangled Rope). Users benefit from social connection and information access, but are simultaneously subject to data extraction, algorithmic manipulation, and echo chambers. They are constrained by network effects and the lack of easily available alternative platforms.
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Social Media Platforms (Rope). Platforms benefit from increased user participation, which drives advertising revenue and network effects. They experience this constraint as a coordination mechanism, as they are the primary beneficiaries of increased participation.
constraint_indexing:constraint_classification(social_media_participation_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Analytical Observer (Tangled Rope). From a long-term perspective, the increasing reliance on social media for participation creates a structural imbalance. While it provides benefits in terms of connectivity, it also leads to data extraction, manipulation, and the potential for social division.
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_media_participation_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_media_participation_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_media_participation_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_media_participation_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(social_media_participation_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Social media platforms extract data from users, contributing to targeted advertising and potential manipulation. Suppression (0.60): Moderate-High. The network effects of dominant platforms and lack of readily available alternatives create a high barrier to exit. Theater ratio (0.30): Low. While there is some performative element to social media activity, the primary function is information access and social connection.
 *
 * PERSPECTIVAL GAP:
 *   The digitally excluded perceive the system as a Snare because they are trapped without access to essential services. The average social media user experiences a Tangled Rope, as they benefit from connection and information but are subject to data extraction and manipulation. Platforms experience a Rope, as they are the primary beneficiaries of increased participation.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural position of each agent. Platforms with arbitrage options experience low extraction, while the digitally excluded with no exit bear the highest cost. The average user experiences a mixed extraction dynamic due to their constrained exit and the conflicting benefits and costs of platform usage.
 *
 * MANDATROPHY ANALYSIS:
 *   This scenario resolves the mandatrophy by considering the different perspectives. What may appear as a pure coordination mechanism (Rope) from the platform's perspective is actually a Tangled Rope or Snare from the perspective of those with limited access or agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    access_affordability_gap,
    'What level of digital access and affordability is sufficient for equitable participation?',
    'Empirical study on the impact of access initiatives on civic engagement and economic opportunity.',
    'Determines whether interventions should focus on infrastructure, affordability, or digital literacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_affordability_gap, empirical, 'Determining sufficient digital access and affordability.').

omega_variable(
    alternative_platform_viability,
    'Can alternative, decentralized platforms effectively compete with dominant social media networks?',
    'Longitudinal analysis of adoption rates, feature parity, and content moderation effectiveness of alternative platforms.',
    'Impacts the viability of escape routes for users concerned about data privacy and platform manipulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Assessing viability of competing decentralized platforms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_media_participation_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soci_tr_t0, social_media_participation_threshold, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soci_tr_t5, social_media_participation_threshold, theater_ratio, 5, 0.22).
narrative_ontology:measurement(soci_tr_t10, social_media_participation_threshold, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(soci_be_t0, social_media_participation_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soci_be_t5, social_media_participation_threshold, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(soci_be_t10, social_media_participation_threshold, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(social_media_participation_threshold, digital_divide).
narrative_ontology:affects_constraint(social_media_participation_threshold, misinformation_ecosystem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
