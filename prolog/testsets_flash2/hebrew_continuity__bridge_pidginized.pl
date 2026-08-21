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
 *   This constraint describes Hebrew's role as a 'bridge pidgin' for Jewish
 *   diaspora interaction, distinct from its liturgical or native generative
 *   uses. It facilitates communication and cultural exchange among
 *   communities with diverse native languages, operating as an instrumental
 *   utility rather than a sacred or primary language. The low extractiveness
 *   and suppression reflect its voluntary adoption and functional utility,
 *   making it a 'rope' that coordinates interaction.
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
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as a Bridge Pidgin in Diaspora").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'f97411fa-06d1-4dbb-b9d1-304df521e2d6').
narrative_ontology:cs_kernel_codification('f97411fa-06d1-4dbb-b9d1-304df521e2d6', distributed).
narrative_ontology:cs_authority_grounding('f97411fa-06d1-4dbb-b9d1-304df521e2d6', practice).
narrative_ontology:cs_interpretation_layer_present('f97411fa-06d1-4dbb-b9d1-304df521e2d6').
narrative_ontology:cs_reading_relation('f97411fa-06d1-4dbb-b9d1-304df521e2d6', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('f97411fa-06d1-4dbb-b9d1-304df521e2d6', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('f97411fa-06d1-4dbb-b9d1-304df521e2d6', foundational, hebrew_as_instrumental_bridge).
narrative_ontology:cs_axiom_status(hebrew_as_instrumental_bridge, holdable).
narrative_ontology:cs_axiom_grounding('f97411fa-06d1-4dbb-b9d1-304df521e2d6', hebrew_as_instrumental_bridge, conventional).
narrative_ontology:cs_axiom('f97411fa-06d1-4dbb-b9d1-304df521e2d6', secondary, linguistic_utility_over_purity).
narrative_ontology:cs_axiom_status(linguistic_utility_over_purity, holdable).
narrative_ontology:cs_axiom_grounding('f97411fa-06d1-4dbb-b9d1-304df521e2d6', linguistic_utility_over_purity, instrumental).
narrative_ontology:cs_reference_frame('f97411fa-06d1-4dbb-b9d1-304df521e2d6', diaspora_contact_language_utility).
narrative_ontology:cs_drift_state('f97411fa-06d1-4dbb-b9d1-304df521e2d6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f97411fa-06d1-4dbb-b9d1-304df521e2d6', '').
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

% Utilize Hebrew in a pidginized form for communication across diverse linguistic backgrounds, facilitating cultural and religious exchange without requiring full fluency or liturgical adherence. They benefit from a shared, accessible linguistic bridge.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Use this form of Hebrew for academic and cultural discourse, appreciating its instrumental utility for broader engagement beyond strict liturgical or modern native contexts. They gain a common ground for intellectual exchange.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, inter_community_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Adhere to a view of Hebrew as primarily a sacred language for prayer and study, dismissing pidginized forms as inauthentic or degraded. They are excluded from the conversation about its instrumental use as a bridge language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_purists, excluded,
    organized, generational, identity_locked, global).

% Primarily use modern Hebrew as a living, generative language, often viewing pidginized forms as grammatically incorrect or lacking the full expressive range of native speech. They are not directly involved in the maintenance or use of Hebrew as a diaspora bridge pidgin.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speakers, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, accessible linguistic medium for Jewish individuals and communities in the diaspora to interact, share culture, and maintain a sense of collective identity, overcoming barriers of diverse native languages.
% TRANSFER_FUNCTION: Facilitates the transfer of cultural knowledge, social cohesion, and shared identity across geographically dispersed and linguistically varied Jewish communities, without requiring full mastery of classical or modern Hebrew.
% ABSENT_VOICES: Liturgical purists and native Hebrew speakers are largely absent from the discourse on Hebrew as a bridge pidgin, as their definitions of 'authentic' Hebrew often exclude such instrumental, contact-language forms. They would argue for stricter linguistic standards or a focus on native generative use.
% DISAPPEARANCE_RATIONALE: If Hebrew ceased to function as a bridge pidgin, diaspora communities would lose a vital, low-friction means of inter-community communication and cultural transmission. New, less efficient, or less culturally resonant linguistic bridges would need to emerge, or communities would become more isolated, leading to a significant rearrangement of diaspora interaction patterns.
% FOUNDING_PROBLEM: The challenge of maintaining a shared Jewish identity and facilitating communication across a globally dispersed diaspora with diverse native languages, where full mastery of classical or modern Hebrew was not universally attainable or desired.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguistic studies of diaspora communities and ethnographic accounts from inter-community organizations consistently corroborate the ongoing need for an accessible contact language. This is attested by scholars of Jewish studies and community leaders who are not direct beneficiaries of the pidgin's maintenance.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.3) because participation is largely voluntary and driven by instrumental utility; there are no significant coercive mechanisms. Suppression is also low (0.1) as alternatives (other lingua francas) exist, but Hebrew offers unique cultural resonance. Theater ratio is moderate (0.2) reflecting some performative aspects of maintaining a 'shared' language that is not deeply generative for most users, but its functional utility remains primary. Accessibility collapse is moderate (0.4) as it simplifies access to Jewish cultural content for non-fluent speakers, but doesn't fully collapse the need for deeper linguistic engagement for other purposes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of diaspora communities, this pidginized Hebrew is a valuable, low-cost coordination mechanism. However, from the perspective of liturgical purists or native speakers, it might be seen as a degradation of the language, leading to a 'not really Hebrew' assessment. The engine's classification will reflect the instrumental utility, while the omegas capture the conceptual contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities and inter-community scholars are clear beneficiaries, gaining a functional communication tool. There are no direct 'victims' as participation is voluntary and alternatives exist. Liturgical purists and native Hebrew speakers are 'excluded' in the sense that their definitions of Hebrew do not encompass this form, but they are not directly harmed by its existence.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a functional, albeit non-traditional, coordination mechanism as either a purely inert 'piton' or an extractive 'snare'. Its persistence is due to its ongoing instrumental utility, not inertia or coercion. The 'contested' status of the founding problem acknowledges the ongoing debate about what constitutes 'true' Hebrew continuity, but affirms the live problem this specific reading addresses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_of_pidginized_hebrew,
    'Is pidginized Hebrew a legitimate form of Hebrew, or a degraded version that undermines the language''s ''true'' essence?',
    'Sociolinguistic acceptance over time, and shifts in community-wide definitions of linguistic authenticity. If the pidginized form gains wider recognition as a valid expression of Hebrew, the conceptual ambiguity resolves.',
    'If deemed ''degraded'', the perceived value and long-term viability of this form of Hebrew might diminish, potentially increasing resistance from purists. If accepted as legitimate, its coordination function would be strengthened, and its ''rope'' classification would be more robust against conceptual challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_pidginized_hebrew, conceptual, 'Conceptual contest over the linguistic authenticity and legitimacy of pidginized Hebrew.').

omega_variable(
    impact_on_native_generative_hebrew,
    'Does the widespread use of pidginized Hebrew in the diaspora dilute efforts to promote native, generative Hebrew fluency, or does it serve as an entry point?',
    'Empirical studies tracking language acquisition pathways: if pidginized use correlates with later full fluency, it''s an entry point. If it correlates with stagnation at a pidgin level, it''s a diluting factor.',
    'If it dilutes, the ''native_generative'' reading might experience increased pressure and resource diversion, potentially shifting its own classification towards ''snare'' if it must actively suppress pidgin use. If it''s an entry point, the two readings could be seen as complementary, strengthening the overall ''rope'' classification of Hebrew continuity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_native_generative_hebrew, empirical, 'Empirical question about the long-term impact of pidginized Hebrew on native fluency efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_continuity__bridge_pidginized, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(hebr_tr_t1930, hebrew_continuity__bridge_pidginized, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_continuity__bridge_pidginized, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_continuity__bridge_pidginized, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__bridge_pidginized, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_continuity__bridge_pidginized, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(hebr_be_t1930, hebrew_continuity__bridge_pidginized, base_extractiveness, 1930, 0.25).
narrative_ontology:measurement(hebr_be_t1960, hebrew_continuity__bridge_pidginized, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(hebr_be_t1990, hebrew_continuity__bridge_pidginized, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__bridge_pidginized, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_continuity__bridge_pidginized, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(hebr_su_t1930, hebrew_continuity__bridge_pidginized, suppression_requirement, 1930, 0.08).
narrative_ontology:measurement(hebr_su_t1960, hebrew_continuity__bridge_pidginized, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(hebr_su_t1990, hebrew_continuity__bridge_pidginized, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__bridge_pidginized, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'hebrew_continuity' family, representing the 'bridge_pidginized' reading. It is distinct from the 'liturgical_preservation' and 'native_generative' readings, which focus on different modes of Hebrew's existence and have different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
