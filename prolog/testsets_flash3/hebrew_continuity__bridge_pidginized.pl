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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint describes the role of Hebrew as a functional, pidginized
 *   contact language within Jewish diaspora communities, distinct from its
 *   liturgical use or its status as a native language in Israel. It serves as
 *   a 'bridge' for interaction, characterized by simplified grammar and
 *   vocabulary, and is neither purely sacred nor fully generative. Both
 *   liturgical purists and native speakers often dismiss this form as 'not
 *   real Hebrew,' but it persists due to its instrumental utility. This story
 *   is one reading of the 'hebrew_continuity' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.3).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.15).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.3).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as a Bridge Pidgin in Diaspora").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '1c4cc929-5012-4264-b800-248ed9cdfa86').
narrative_ontology:cs_kernel_codification('1c4cc929-5012-4264-b800-248ed9cdfa86', distributed).
narrative_ontology:cs_authority_grounding('1c4cc929-5012-4264-b800-248ed9cdfa86', practice).
narrative_ontology:cs_interpretation_layer_present('1c4cc929-5012-4264-b800-248ed9cdfa86').
narrative_ontology:cs_reading_relation('1c4cc929-5012-4264-b800-248ed9cdfa86', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('1c4cc929-5012-4264-b800-248ed9cdfa86', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('1c4cc929-5012-4264-b800-248ed9cdfa86', foundational, hebrew_as_functional_bridge).
narrative_ontology:cs_axiom_status(hebrew_as_functional_bridge, holdable).
narrative_ontology:cs_axiom_grounding('1c4cc929-5012-4264-b800-248ed9cdfa86', hebrew_as_functional_bridge, instrumental).
narrative_ontology:cs_reference_frame('1c4cc929-5012-4264-b800-248ed9cdfa86', diaspora_functional_utility).
narrative_ontology:cs_drift_state('1c4cc929-5012-4264-b800-248ed9cdfa86', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1c4cc929-5012-4264-b800-248ed9cdfa86', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, inter_community_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize Hebrew as a functional, albeit simplified, contact language for communication across diverse linguistic backgrounds, facilitating cultural and religious exchange without requiring full fluency or liturgical expertise. They benefit from the low barrier to entry for inter-community interaction.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Use this form of Hebrew for academic and informal communication, particularly in fields like Jewish studies, where it serves as a lingua franca. They appreciate its instrumental utility for knowledge sharing and networking.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, inter_community_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Adhere to the view that Hebrew's primary and legitimate form is its classical, liturgical manifestation. They view the pidginized form as a degradation or 'not really Hebrew,' and actively resist its recognition as a valid expression of the language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_purists, excluded,
    organized, generational, identity_locked, global).

% Primarily speak modern Israeli Hebrew as a native language. They often find the pidginized diaspora Hebrew difficult to understand or consider it an 'impure' form, advocating for the generative, native-speaker standard as the only true living Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speakers, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables basic, functional communication and cultural exchange among Jewish diaspora communities and scholars who do not share a common native language or deep liturgical knowledge of Hebrew.
% TRANSFER_FUNCTION: Facilitates the transfer of cultural knowledge, religious concepts, and social cohesion across linguistically diverse Jewish communities, by lowering the linguistic barrier to entry.
% ABSENT_VOICES: Liturgical purists and native Hebrew speakers are often absent from the discourse around the utility and legitimacy of pidginized Hebrew, as they dismiss its validity. They would argue for stricter adherence to classical or modern native standards, respectively.
% DISAPPEARANCE_RATIONALE: If this pidginized form of Hebrew vanished, diaspora communities would lose a significant, low-friction bridge for inter-community interaction. Communication would become more fragmented, relying on shared national languages or requiring higher levels of classical/native Hebrew proficiency, leading to a decrease in cross-cultural engagement.
% FOUNDING_PROBLEM: The need for a common, accessible linguistic bridge for Jewish diaspora communities to interact and maintain cultural ties, given the diversity of native languages and the high barrier to entry for classical or native Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguistic studies of diaspora communities and ethnographic accounts from community leaders attest to the ongoing need for such a bridge, confirming that the problem remains live. This is corroborated by the continued use and adaptation of Hebrew in informal, functional contexts.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.3) because this form of Hebrew primarily serves a coordination function, enabling communication with minimal overhead. There's no active enforcement or significant cost imposed on users; rather, it offers a benefit. Suppression is low (0.15) as its use is voluntary and driven by utility, not coercion. Theater ratio is low (0.1) as its function is genuinely instrumental, not performative. Accessibility collapse is low (0.2) because alternatives (other shared languages) exist, but this form offers a unique cultural bridge. Resistance is low (0.05) because its users are beneficiaries, though it faces conceptual resistance from other readings.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the users of this pidginized Hebrew (who see it as a valuable, functional tool) and those who adhere to the liturgical or native-speaker readings (who often view it as an 'impure' or 'degraded' form). The engine's classification as a Rope reflects its coordination function for its users, while the excluded parties would likely classify it as a Piton or even a Snare if they perceived it as actively undermining their preferred forms of Hebrew.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities and inter-community scholars are beneficiaries, gaining a functional communication tool. Liturgical purists and native Hebrew speakers are excluded, as their definitions of 'true' Hebrew lead them to dismiss this form, but they are not 'victims' in the extractive sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_pidginized_hebrew,
    'Is pidginized Hebrew a legitimate form of the language, or a degraded version?',
    'Sociolinguistic acceptance: if a critical mass of linguists and community leaders formally recognize its functional validity, its legitimacy would be established.',
    'If recognized as legitimate, its status as a coordination mechanism would be strengthened, potentially increasing its adoption. If deemed degraded, its use might be suppressed by purist factions, pushing it towards a more constrained or even snared classification for its users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_pidginized_hebrew, conceptual, 'Ambiguity regarding the linguistic legitimacy of pidginized Hebrew.').

omega_variable(
    impact_on_sibling_readings,
    'Does the prevalence of pidginized Hebrew dilute the commitment to liturgical preservation or native generative use?',
    'Longitudinal studies tracking proficiency and engagement in liturgical and native Hebrew among communities that extensively use pidginized Hebrew.',
    'If dilution is significant, this reading could be seen as ''influencing'' or even ''foreclosing'' the other readings by drawing resources or attention away from them. If not, it coexists as a distinct, complementary function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_sibling_readings, empirical, 'Whether the bridge pidgin form negatively impacts other forms of Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__bridge_pidginized, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(hebr_be_t1970, hebrew_continuity__bridge_pidginized, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(hebr_be_t1990, hebrew_continuity__bridge_pidginized, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(hebr_be_t2010, hebrew_continuity__bridge_pidginized, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__bridge_pidginized, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__bridge_pidginized, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(hebr_su_t1970, hebrew_continuity__bridge_pidginized, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(hebr_su_t1990, hebrew_continuity__bridge_pidginized, suppression_requirement, 1990, 0.14).
narrative_ontology:measurement(hebr_su_t2010, hebrew_continuity__bridge_pidginized, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__bridge_pidginized, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel, focusing on its role as a bridge pidgin. It coexists with and influences the 'liturgical_preservation' and 'native_generative' readings by offering an alternative mode of engagement with the language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
