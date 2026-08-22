% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' if its sacred texts are
 *   continuously recited, studied, and used in ritual, asserting that
 *   liturgical transmission alone suffices for vitality. This reading is
 *   often held by traditional religious authorities and communities who
 *   prioritize the preservation of sacred tradition over daily, generative
 *   use. It implicitly delegitimizes secular or modern uses of the language
 *   as insufficient for 'living' status. The constraint is claimed as a Rope,
 *   reflecting its coordination function for religious communities, but its
 *   metrics show a low level of extraction from secular users and a moderate
 *   level of suppression of alternative definitions of vitality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.15).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.25).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status: Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '96215bb1-3942-43d2-99a5-81936a48e0e0').
narrative_ontology:cs_kernel_codification('96215bb1-3942-43d2-99a5-81936a48e0e0', fixed_text).
narrative_ontology:cs_authority_grounding('96215bb1-3942-43d2-99a5-81936a48e0e0', lineage).
narrative_ontology:cs_interpretation_layer_present('96215bb1-3942-43d2-99a5-81936a48e0e0').
narrative_ontology:cs_reading_relation('96215bb1-3942-43d2-99a5-81936a48e0e0', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('96215bb1-3942-43d2-99a5-81936a48e0e0', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('96215bb1-3942-43d2-99a5-81936a48e0e0', foundational, liturgical_transmission_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('96215bb1-3942-43d2-99a5-81936a48e0e0', liturgical_transmission_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('96215bb1-3942-43d2-99a5-81936a48e0e0', foundational, sacred_texts_are_the_language_kernel).
narrative_ontology:cs_axiom_status(sacred_texts_are_the_language_kernel, holdable).
narrative_ontology:cs_axiom_grounding('96215bb1-3942-43d2-99a5-81936a48e0e0', sacred_texts_are_the_language_kernel, theological).
narrative_ontology:cs_reference_frame('96215bb1-3942-43d2-99a5-81936a48e0e0', continuous_liturgical_transmission).
narrative_ontology:cs_drift_state('96215bb1-3942-43d2-99a5-81936a48e0e0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('96215bb1-3942-43d2-99a5-81936a48e0e0', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, religious_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, tradition_as_continuity).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sacred_text_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive monopoly over sacred texts and rituals. This reading of 'living language' validates their role as custodians of tradition, ensuring the continuity of their authority and the relevance of the liturgical corpus. Their identity is fused with the preservation of this specific linguistic and religious practice.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the stability and continuity of their religious practices and identity. The liturgical use of the language provides a direct link to their heritage and sacred texts, reinforcing communal bonds and a sense of timeless tradition. Their self-concept is deeply intertwined with this form of linguistic preservation.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, religious_communities, beneficiary,
    organized, generational, identity_locked, global).

% Their use of the language for daily communication, modern literature, or secular education is implicitly delegitimized or deemed 'less authentic' by this reading. They bear the cost of a diminished claim to the language's 'living' status, potentially facing cultural marginalization or accusations of desecration from traditionalists. Their exit is constrained by the desire to participate in the broader cultural sphere.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Analyze the linguistic properties and social functions of the language. They observe the dynamics of liturgical use versus secular use and often find themselves mediating between competing claims of linguistic vitality. Their role is to describe, not to adjudicate, the 'living' status.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous use and study of sacred texts within religious communities, ensuring the transmission of religious knowledge and ritual practice across generations.
% TRANSFER_FUNCTION: Transfers cultural and religious authority to those who maintain liturgical practice, while implicitly transferring a diminished status to secular uses of the language.
% ABSENT_VOICES: Advocates for modern, secular uses of the language, who would argue that vitality requires generative use beyond ritual, are often excluded from the discourse on 'living language' status within traditional religious institutions.
% DISAPPEARANCE_RATIONALE: If this understanding of 'living language' vanished, the authority of rabbinical institutions would be challenged, religious communities would lose a key anchor for their identity, and the perceived value of liturgical practice would diminish, leading to a significant reorganization of religious and cultural life.
% FOUNDING_PROBLEM: The problem of ensuring the continuity and sanctity of a language primarily associated with sacred texts, preventing its complete secularization or obsolescence.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders and community members attest to the ongoing challenge of maintaining tradition in a modern world. Linguistic scholars, while not endorsing the normative claim, corroborate the historical and sociological reality of liturgical transmission as a mechanism for linguistic continuity within these communities.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is coordination around a fixed corpus, not direct material extraction. Suppression is moderate (0.25) as this reading actively suppresses alternative definitions of linguistic vitality, particularly those emphasizing native generation or modern literary output. Theater ratio is low (0.1) because the liturgical practices are genuinely functional for religious communities, not merely performative. Accessibility collapse is high (0.7) because for those who accept this definition, the path to 'living language' status is clear and narrow, collapsing other alternatives. Resistance is low (0.1) because the primary beneficiaries (religious authorities) face little internal resistance to this definition, and external resistance from secular communities is often dismissed as irrelevant to the sacred domain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinical authority, this is a pure Rope, coordinating the preservation of a sacred language. From the perspective of the secular speech community, it is a subtle Snare, extracting legitimacy from their linguistic practices. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and religious communities are beneficiaries, as this reading validates their roles and practices (d near 0.0). The secular speech community is a payer, as their linguistic practices are devalued (d near 1.0). Linguistic scholars are observers, analyzing the phenomenon without being directly subject to its normative claims (d near 0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality,
    'Is ''living'' status for a language fundamentally about its functional use in daily life and generative output, or about its continuous transmission within a sacred tradition?',
    'Conceptual clarification and consensus within sociolinguistics and religious studies, or a shift in cultural values regarding linguistic heritage.',
    'If functional use is prioritized, this reading''s claim to ''living'' status would be weakened, potentially reclassifying it as a Piton (theatrical preservation). If liturgical transmission is universally accepted, its Rope classification would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality, conceptual, 'Ambiguity in the core definition of linguistic vitality.').

omega_variable(
    secular_use_legitimacy,
    'To what extent does the liturgical preservation reading actively suppress or merely ignore the legitimacy of secular, generative uses of the language?',
    'Empirical study of discourse within religious communities: analysis of sermons, texts, and pronouncements regarding secular linguistic initiatives.',
    'If active suppression is high, the constraint''s ''suppression'' metric would be higher, pushing it towards a Tangled Rope or Snare classification for the secular speech community. If merely ignored, the current low suppression is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_use_legitimacy, empirical, 'Degree of active suppression vs. passive disregard for secular language use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1800, living_language_status__liturgical_preservation_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(livi_tr_t1850, living_language_status__liturgical_preservation_reading, theater_ratio, 1850, 0.09).
narrative_ontology:measurement(livi_tr_t1900, living_language_status__liturgical_preservation_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(livi_tr_t1950, living_language_status__liturgical_preservation_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(livi_tr_t2000, living_language_status__liturgical_preservation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(livi_tr_t2024, living_language_status__liturgical_preservation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t1800, living_language_status__liturgical_preservation_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(livi_be_t1850, living_language_status__liturgical_preservation_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(livi_be_t1900, living_language_status__liturgical_preservation_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(livi_be_t1950, living_language_status__liturgical_preservation_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(livi_be_t2000, living_language_status__liturgical_preservation_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(livi_be_t2024, living_language_status__liturgical_preservation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1800, living_language_status__liturgical_preservation_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(livi_su_t1850, living_language_status__liturgical_preservation_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement(livi_su_t1900, living_language_status__liturgical_preservation_reading, suppression_requirement, 1900, 0.23).
narrative_ontology:measurement(livi_su_t1950, living_language_status__liturgical_preservation_reading, suppression_requirement, 1950, 0.24).
narrative_ontology:measurement(livi_su_t2000, living_language_status__liturgical_preservation_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(livi_su_t2024, living_language_status__liturgical_preservation_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'living language status' kernel, each offering a different criterion for linguistic vitality. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
