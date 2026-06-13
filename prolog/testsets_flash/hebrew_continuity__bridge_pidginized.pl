% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as a Bridge Pidgin in Diaspora
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes Hebrew's role as a functional contact language
 *   for Jewish diaspora communities, distinct from its liturgical use or its
 *   status as a native, generative language in Israel. It serves as a 'bridge
 *   pidgin,' facilitating inter-community interaction and cultural exchange
 *   without demanding full linguistic mastery. This reading acknowledges a
 *   sparse number of native speakers and a focus on high-register written
 *   production alongside a marketplace pidgin, where instrumental utility
 *   drives its adoption. Both the liturgical preservation and native
 *   generative readings often dismiss this form as 'not really Hebrew,'
 *   highlighting the contest over the language's authentic form and function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.3).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.1).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.3).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as a Bridge Pidgin in Diaspora").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'fb555217-fdec-4008-bfcf-166ee163ad20').
narrative_ontology:cs_kernel_codification('fb555217-fdec-4008-bfcf-166ee163ad20', distributed).
narrative_ontology:cs_authority_grounding('fb555217-fdec-4008-bfcf-166ee163ad20', practice).
narrative_ontology:cs_interpretation_layer_present('fb555217-fdec-4008-bfcf-166ee163ad20').
narrative_ontology:cs_reading_relation('fb555217-fdec-4008-bfcf-166ee163ad20', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('fb555217-fdec-4008-bfcf-166ee163ad20', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('fb555217-fdec-4008-bfcf-166ee163ad20', foundational, hebrew_as_functional_bridge).
narrative_ontology:cs_axiom_status(hebrew_as_functional_bridge, holdable).
narrative_ontology:cs_axiom_grounding('fb555217-fdec-4008-bfcf-166ee163ad20', hebrew_as_functional_bridge, conventional).
narrative_ontology:cs_axiom('fb555217-fdec-4008-bfcf-166ee163ad20', secondary, linguistic_utility_over_purity).
narrative_ontology:cs_axiom_status(linguistic_utility_over_purity, holdable).
narrative_ontology:cs_axiom_grounding('fb555217-fdec-4008-bfcf-166ee163ad20', linguistic_utility_over_purity, instrumental).
narrative_ontology:cs_reference_frame('fb555217-fdec-4008-bfcf-166ee163ad20', diaspora_inter_community_communication).
narrative_ontology:cs_drift_state('fb555217-fdec-4008-bfcf-166ee163ad20', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fb555217-fdec-4008-bfcf-166ee163ad20', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, inter_community_scholars).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, cultural_exchange_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize Hebrew as a common, albeit simplified, linguistic bridge for communication, cultural exchange, and shared identity across diverse geographic and linguistic backgrounds. They benefit from the ease of interaction but are constrained by the limited expressive range of a pidginized form.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Leverage pidginized Hebrew for academic and cultural discourse, enabling cross-community research and collaboration. They find it instrumentally useful for communication but often maintain proficiency in more formal or liturgical forms.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, inter_community_scholars, beneficiary,
    powerful, biographical, mobile, global).

% Actively promote and facilitate the use of simplified Hebrew as a contact language to foster connection and shared identity among Jewish youth and adults in the diaspora. They design curricula and events around this functional use.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, cultural_exchange_programs, agenda_setter,
    institutional, biographical, mobile, global).

% Reject the pidginized form of Hebrew as a degradation of the language, arguing it lacks the grammatical complexity and lexical richness of 'true' Hebrew. They are excluded from the mainstream promotion of this form and often advocate for either liturgical or native generative use.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, linguistic_purists, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, accessible linguistic medium for Jewish people across the diaspora to interact, share culture, and maintain a sense of collective identity, overcoming barriers of diverse local languages.
% TRANSFER_FUNCTION: Facilitates the transfer of cultural knowledge, social connection, and shared identity among diaspora communities, using a simplified form of Hebrew as the vehicle.
% ABSENT_VOICES: Linguistic purists and proponents of 'pure' Hebrew (either liturgical or native generative) are largely absent from the discourse promoting Hebrew as a bridge pidgin. They would argue that this form dilutes the language's authenticity and historical depth.
% DISAPPEARANCE_RATIONALE: If Hebrew ceased to function as a contact language, diaspora communities would lose a significant, accessible tool for inter-community connection and cultural transmission. While other languages might fill the gap, the unique symbolic and historical resonance of Hebrew would be lost, requiring a substantial reorganization of cultural and social practices.
% FOUNDING_PROBLEM: The challenge of maintaining a shared Jewish identity and facilitating communication across geographically dispersed communities speaking diverse local languages, without requiring full fluency in classical or modern Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Educational and cultural organizations, as well as sociological studies of diaspora communities, corroborate the ongoing need for an accessible linguistic bridge. While some purists contest its legitimacy, the functional utility for inter-community connection is widely attested by those engaged in cultural preservation and exchange.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely solves a coordination problem (inter-community communication) with relatively low extraction and suppression. The 'extraction' (0.3) is primarily the cost of simplification and the potential loss of linguistic depth, which some purists perceive as a 'cost' to the language itself. Suppression (0.1) is low, as participation is largely voluntary, driven by utility rather than coercion. Theater ratio (0.2) is also low, reflecting a genuine functional use, though some performative aspects of 'speaking Hebrew' for identity purposes exist. Accessibility collapse is moderate (0.4) because while it offers a simplified entry point, it also limits the full range of linguistic expression.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of Hebrew as a bridge pidgin view it as a pragmatic and effective solution to a real-world coordination problem, enabling broad participation. Linguistic purists, however, perceive this same constraint as a degradation or 'snare' for the language itself, extracting its authenticity and replacing it with a less rich form. The engine's classification as a Rope from the perspective of the beneficiaries highlights its functional utility, while the existence of excluded purists points to the contested nature of its legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities, inter-community scholars, and cultural exchange programs are beneficiaries, as they gain a functional tool for connection and cultural transmission. Cultural exchange programs also act as agenda-setters, actively promoting this form. Linguistic purists are excluded, as their definition of 'true' Hebrew is not accommodated by this functional, pidginized approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_vs_utility,
    'Is the functional utility of Hebrew as a bridge pidgin a legitimate form of language continuity, or does it compromise the language''s authenticity and historical depth?',
    'Longitudinal sociolinguistic studies tracking the evolution of pidginized Hebrew and its impact on cultural transmission, alongside community-level acceptance metrics.',
    'If authenticity is deemed paramount, this reading might be reclassified as a Snare (extracting linguistic integrity). If utility is prioritized, its Rope classification is reinforced, potentially influencing policy towards language education.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_vs_utility, conceptual, 'Contest over the definition of ''authentic'' language continuity.').

omega_variable(
    pidgin_to_creole_potential,
    'Does the pidginized form of Hebrew have the potential to creolize and develop into a more grammatically complex, generative language in diaspora communities?',
    'Empirical linguistic analysis of grammatical expansion and lexical innovation in pidginized Hebrew over several generations in diaspora contexts.',
    'If creolization occurs, this reading''s status as a ''bridge pidgin'' would evolve, potentially influencing its classification towards a more ''native_generative'' form, albeit in a diaspora context. If not, its instrumental, simplified nature is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pidgin_to_creole_potential, empirical, 'Potential for linguistic evolution of the pidginized form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1950, hebrew_continuity__bridge_pidginized, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_continuity__bridge_pidginized, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_continuity__bridge_pidginized, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_continuity__bridge_pidginized, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__bridge_pidginized, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__bridge_pidginized, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(hebr_be_t1970, hebrew_continuity__bridge_pidginized, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(hebr_be_t1990, hebrew_continuity__bridge_pidginized, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(hebr_be_t2010, hebrew_continuity__bridge_pidginized, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__bridge_pidginized, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__bridge_pidginized, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hebr_su_t1970, hebrew_continuity__bridge_pidginized, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(hebr_su_t1990, hebrew_continuity__bridge_pidginized, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(hebr_su_t2010, hebrew_continuity__bridge_pidginized, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__bridge_pidginized, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel, focusing on its role as a bridge pidgin. It is linked to the 'liturgical_preservation' and 'native_generative' readings, which represent alternative understandings of Hebrew's continuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
