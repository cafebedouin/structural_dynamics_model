% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew's Liturgical Continuity as Living Language
 *   domain: historical_linguistics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the 'liturgical continuity' reading of
 *   Hebrew's status as a living language. It asserts that Hebrew remains a
 *   living language due to its unbroken use in religious liturgy and textual
 *   study across Jewish diaspora communities, independent of its status as a
 *   natively spoken language. This reading emphasizes symbolic preservation
 *   and cultural transmission over daily generative speech. The constraint is
 *   claimed as a Mountain due to its perceived natural emergence from
 *   continuous practice and its low extractiveness, serving as a foundational
 *   element of identity rather than an extractive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.02).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, mountain).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew's Liturgical Continuity as Living Language").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/commitment_systems").

domain_priors:emerges_naturally(hebrew_living_language__liturgical_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '8c9b4848-948d-4d4f-b940-f76020d543a6').
narrative_ontology:cs_kernel_codification('8c9b4848-948d-4d4f-b940-f76020d543a6', fixed_text).
narrative_ontology:cs_authority_grounding('8c9b4848-948d-4d4f-b940-f76020d543a6', lineage).
narrative_ontology:cs_interpretation_layer_present('8c9b4848-948d-4d4f-b940-f76020d543a6').
narrative_ontology:cs_reading_relation('8c9b4848-948d-4d4f-b940-f76020d543a6', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c9b4848-948d-4d4f-b940-f76020d543a6', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('8c9b4848-948d-4d4f-b940-f76020d543a6', foundational, unbroken_liturgical_chain_constitutes_life).
narrative_ontology:cs_axiom_status(unbroken_liturgical_chain_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('8c9b4848-948d-4d4f-b940-f76020d543a6', unbroken_liturgical_chain_constitutes_life, deontological).
narrative_ontology:cs_axiom('8c9b4848-948d-4d4f-b940-f76020d543a6', secondary, sacred_text_study_sustains_linguistic_essence).
narrative_ontology:cs_axiom_status(sacred_text_study_sustains_linguistic_essence, holdable).
narrative_ontology:cs_axiom_grounding('8c9b4848-948d-4d4f-b940-f76020d543a6', sacred_text_study_sustains_linguistic_essence, theological).
narrative_ontology:cs_reference_frame('8c9b4848-948d-4d4f-b940-f76020d543a6', ancient_diaspora_continuity).
narrative_ontology:cs_drift_state('8c9b4848-948d-4d4f-b940-f76020d543a6', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8c9b4848-948d-4d4f-b940-f76020d543a6', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, religious_scholars).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, divine_covenant_continuity).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, cultural_heritage_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities maintain their cultural and religious identity through the continuous use of Hebrew in prayer and study, seeing it as an unbroken chain of tradition. The constraint provides a stable linguistic anchor for their collective identity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Scholars of Jewish texts and liturgy rely on the continuity of Hebrew for their work, interpreting ancient and medieval texts. The constraint ensures the accessibility and relevance of their field of study.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, religious_scholars, beneficiary,
    moderate, biographical, constrained, global).

% These historians study the evolution and persistence of languages. From their perspective, the unbroken liturgical and textual use of Hebrew represents a unique case of language preservation, even without daily spoken use.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of a sacred language and cultural identity across diverse and geographically dispersed communities by establishing a shared, continuous practice of recitation and study.
% TRANSFER_FUNCTION: Transfers a sense of historical and religious continuity, shared identity, and access to sacred texts across generations and geographies, from the past to present and future communities.
% ABSENT_VOICES: Secular linguists who define 'living language' strictly by native, generative spoken use might argue that liturgical use alone is insufficient, but their definition is not central to the communities' self-understanding of Hebrew's vitality.
% DISAPPEARANCE_RATIONALE: If the practice of liturgical recitation and textual study of Hebrew vanished, Jewish diaspora communities would lose a foundational pillar of their religious and cultural identity, leading to a profound reorganization of their communal life and self-conception.
% FOUNDING_PROBLEM: The challenge of maintaining a distinct religious and cultural identity, and access to sacred texts, for a dispersed people without a common spoken language or territorial base.
% FOUNDING_PROBLEM_CORROBORATION: Jewish religious authorities and community leaders universally attest to the ongoing vitality of this problem and the efficacy of liturgical continuity as its solution. Independent historical and sociological studies of diaspora communities corroborate the role of Hebrew in identity preservation.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_living_language__liturgical_continuity_reading),
    narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) as participation is voluntary and primarily for spiritual/cultural benefit, not material gain. Suppression is minimal (0.02) as there's no coercion to participate beyond communal norms. Theater ratio is negligible (0.01) because the practice is deeply meaningful and functional for its participants. Accessibility collapse is high (0.9) because for those within the tradition, the continuity is self-evident and alternatives for maintaining this specific form of identity are few. Resistance is low (0.01) as the practice is widely accepted within the communities it serves.
 *
 * PERSPECTIVAL GAP:
 *   While external linguistic observers might define 'living language' differently, from the perspective of the participating communities, the liturgical continuity is a self-evident and non-extractive 'natural law' of their cultural existence. The engine's classification will reflect this internal coherence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities and religious scholars are direct beneficiaries, deriving identity and professional purpose from this continuity. There are no identifiable victims, as participation is voluntary and perceived as beneficial. The constraint subsidizes the cultural and religious life of its participants.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving identity and access to sacred texts) remains live and actively fulfilled. There is no evidence of mandatrophy; the function has not atrophied, and the constraint is not maintained for theatrical reasons. Its persistence is due to its ongoing utility and deep integration into communal life.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is ''living language'' defined by continuous liturgical and textual use, or by native, generative spoken use?',
    'Conceptual clarification and agreement on definitional criteria within the field of historical linguistics and cultural studies. This is a definitional, not empirical, question.',
    'If the ''native generation'' definition prevails, this constraint would be reclassified as a ''snare'' or ''piton'' (a performative maintenance of a ''dead'' language), with high conceptual extraction from those who believe it to be living. If the ''liturgical continuity'' definition is accepted, it remains a ''mountain'' or ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of what constitutes a ''living language''.').

omega_variable(
    cultural_vs_linguistic_function,
    'Is the primary function of Hebrew''s liturgical continuity cultural/identity preservation, or strictly linguistic vitality?',
    'Sociolinguistic studies examining the actual use and perception of Hebrew within diaspora communities, distinguishing between its symbolic and communicative roles.',
    'If primarily cultural, the low extractiveness and ''mountain'' classification hold. If a claim of full linguistic vitality is made and found wanting, it could expose a ''theater'' component or a ''snare'' of false promise for linguistic revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_linguistic_function, empirical, 'Distinguishing cultural/identity function from strict linguistic function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(hebr_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.01).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.01).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.01).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(hebr_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(hebr_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(hebr_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(hebr_su_t1000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(hebr_su_t1500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2000, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_living_language' kernel. This 'liturgical continuity' reading emphasizes the unbroken tradition of religious use. It influences the other readings by providing a baseline for historical continuity, but does not foreclose them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
