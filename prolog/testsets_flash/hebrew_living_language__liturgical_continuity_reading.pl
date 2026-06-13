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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew as a Living Language: Liturgical Continuity Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines Hebrew as a living language through its unbroken
 *   liturgical recitation and textual study across the Jewish diaspora. It
 *   asserts that continuous ritual and scholarly engagement, rather than
 *   daily native speech, is the criterion for linguistic vitality. This
 *   reading emphasizes the preservation of a sacred and scholarly tradition,
 *   with low extractiveness and suppression, as participation is largely
 *   voluntary and identity-bound.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, mountain).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as a Living Language: Liturgical Continuity Reading").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:emerges_naturally(hebrew_living_language__liturgical_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, 'ef6f190e-e8f0-4a6d-b42b-3c56745f0951').
narrative_ontology:cs_kernel_codification('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', fixed_text).
narrative_ontology:cs_authority_grounding('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', lineage).
narrative_ontology:cs_interpretation_layer_present('ef6f190e-e8f0-4a6d-b42b-3c56745f0951').
narrative_ontology:cs_reading_relation('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', foundational, unbroken_transmission_is_vitality).
narrative_ontology:cs_axiom_status(unbroken_transmission_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', unbroken_transmission_is_vitality, deontological).
narrative_ontology:cs_axiom('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', foundational, sacred_text_preservation_is_linguistic_life).
narrative_ontology:cs_axiom_status(sacred_text_preservation_is_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', sacred_text_preservation_is_linguistic_life, theological).
narrative_ontology:cs_reference_frame('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', ancient_liturgical_practice).
narrative_ontology:cs_drift_state('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ef6f190e-e8f0-4a6d-b42b-3c56745f0951', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_religious_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, divine_revelation_continuity).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, cultural_heritage_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities maintain Hebrew's 'living' status through continuous liturgical use and study, seeing it as essential for religious practice and identity. They benefit from the language's unbroken chain of transmission, which grounds their spiritual and cultural heritage.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_religious_communities, beneficiary,
    organized, generational, identity_locked, global).

% Academics and researchers who study ancient and medieval Hebrew texts. Their work relies on the continuous tradition of textual interpretation and recitation, which provides a living context for understanding the language's historical forms and meanings.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_scholars, beneficiary,
    moderate, biographical, constrained, global).

% Individuals who speak modern Hebrew as a native language but do not necessarily participate in liturgical or traditional textual study. They are excluded from this specific definition of 'living language' but are not harmed by it, as their own linguistic practice is distinct.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% Academics who analyze the concept of 'living language' and language death. They observe the various claims and practices surrounding Hebrew's status, seeking to understand the structural properties that define linguistic vitality across different contexts.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, linguistic_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous transmission of sacred texts and religious practices across generations and geographies, ensuring a shared linguistic and cultural foundation for Jewish communities worldwide.
% TRANSFER_FUNCTION: Transfers cultural and religious knowledge, identity, and a sense of historical continuity from past generations to present and future ones, through the medium of Hebrew.
% ABSENT_VOICES: Advocates for a 'native speaker' definition of living language, who would argue that liturgical use alone is insufficient for true linguistic vitality. They are absent from this specific framing, which prioritizes continuity over generative daily use.
% DISAPPEARANCE_RATIONALE: If the unbroken liturgical recitation and textual study of Hebrew vanished, it would fundamentally alter the identity and practices of Jewish religious communities, severing a core link to their historical and spiritual heritage. The language would become 'dead' in this specific sense, requiring a complete re-evaluation of religious and cultural continuity.
% FOUNDING_PROBLEM: The challenge of maintaining a distinct religious and cultural identity, and the transmission of sacred texts, across centuries of diaspora and persecution, without a continuous territorial base or daily native speech.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of Jewish communities globally, their continued use of Hebrew in prayer and study, and the historical record of its transmission, corroborated by historians, theologians, and linguists outside the immediate benefiting parties, all attest to the enduring nature of this problem and the constraint's role in addressing it.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).

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
 *   The constraint is classified as a Mountain because its persistence is seen as a structural feature of religious and cultural identity, not dependent on active enforcement or generating significant extraction. Extractiveness is low (0.15) as participation is voluntary and driven by identity, not coercion. Suppression is minimal (0.05) as there are no active mechanisms to prevent alternative definitions of 'living language.' Theater ratio is low (0.1) because the liturgical and scholarly activities are genuinely functional for the communities involved. Accessibility collapse is high (0.8) because, within this framework, the 'living' status of Hebrew is an inherent property of its continuous use, making alternatives to this definition largely irrelevant for those who hold it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious communities, this is an unchangeable truth about their heritage. From a purely linguistic perspective focused on native generative speech, this definition might be seen as a 'conceptual' constraint, but within its own frame, it is a natural consequence of historical continuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish religious communities and Hebrew scholars are beneficiaries, as their identity and work are directly sustained by this continuous linguistic tradition. There are no direct 'victims' as participation is voluntary and alternatives (like speaking other languages) are not suppressed. Secular Hebrew speakers are 'excluded' from this specific definition but are not harmed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a deeply identity-bound, voluntary cultural practice as a form of extraction. The 'mandate' of preserving the language through liturgy is still very much 'live' for its participants, and the constraint's low extractiveness and suppression confirm it is not a decaying or extractive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''Mountain'' of cultural continuity, or a ''conceptual'' framing that serves to maintain a specific group''s identity?',
    'Analysis of the ''native_generation_reading'' and ''literary_revival_reading'' siblings: if those readings demonstrate higher extractiveness or suppression, it would suggest this ''liturgical_continuity_reading'' is a less extractive, more ''natural'' form of language preservation.',
    'If reclassified as a conceptual framing, its ''Mountain'' status would be challenged, potentially shifting it towards a ''Rope'' or ''Tangled Rope'' if any subtle extraction or coordination costs are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''hebrew_living_language'' kernel, specifically the ''liturgical_continuity_reading''. Sibling readings include ''native_generation_reading'' (Hebrew lives through native speakers) and ''literary_revival_reading'' (Hebrew lives through literary production). This reading emphasizes unbroken transmission over generative speech.').

omega_variable(
    natural_vs_constructed_continuity,
    'Is the unbroken liturgical continuity of Hebrew a ''natural'' emergent property of its religious function, or a ''constructed'' outcome of active communal effort?',
    'Historical and sociological analysis of the mechanisms of transmission: if active, coordinated efforts (e.g., funding for yeshivas, specific educational curricula) are found to be essential, it leans towards ''constructed''.',
    'If ''constructed'', the ''emerges_naturally'' flag would be re-evaluated, potentially shifting the classification from Mountain to Rope, acknowledging the coordination effort involved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_continuity, empirical, 'Ambiguity between natural emergence and active construction of linguistic continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.09).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.09).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(hebr_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.13).
narrative_ontology:measurement(hebr_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(hebr_su_t1000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(hebr_su_t1500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_living_language' kernel. This 'liturgical_continuity_reading' emphasizes unbroken transmission through religious practice and study, distinct from definitions based on native generative speech or literary production.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
