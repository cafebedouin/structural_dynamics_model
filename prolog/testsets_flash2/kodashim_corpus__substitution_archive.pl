% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the reading of the Kodashim corpus (laws of
 *   sacrifices) within Rabbinic Judaism as a 'substitution archive.' In this
 *   reading, prayer and Torah study have replaced physical sacrifice, and
 *   Kodashim serves as a memorial documentation of what was superseded,
 *   rather than an active blueprint for future practice. This interpretation
 *   claims continuity with the past while denying the possibility or
 *   necessity of restoring literal sacrificial practice, thereby legitimizing
 *   the current rabbinic-led system of worship. The constraint is classified
 *   as a Tangled Rope because it genuinely coordinates religious life in the
 *   absence of the Temple, but simultaneously extracts from those who seek a
 *   more literal restoration of sacrificial practice, through an actively
 *   enforced interpretive framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.6).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.7).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'f5fb66e4-901f-4c4b-b93b-c30b43f0ee53').
narrative_ontology:cs_kernel_codification('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', fixed_text).
narrative_ontology:cs_authority_grounding('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', lineage).
narrative_ontology:cs_interpretation_layer_present('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53').
narrative_ontology:cs_reading_relation('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_axiom('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', foundational, prayer_and_study_as_sacrificial_substitute).
narrative_ontology:cs_axiom_status(prayer_and_study_as_sacrificial_substitute, holdable).
narrative_ontology:cs_axiom_grounding('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', prayer_and_study_as_sacrificial_substitute, theological).
narrative_ontology:cs_axiom('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', foundational, kodashim_as_memorial_archive).
narrative_ontology:cs_axiom_status(kodashim_as_memorial_archive, holdable).
narrative_ontology:cs_axiom_grounding('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', kodashim_as_memorial_archive, conventional).
narrative_ontology:cs_reference_frame('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', rabbinic_post_temple_continuity).
narrative_ontology:cs_drift_state('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f5fb66e4-901f-4c4b-b93b-c30b43f0ee53', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_restoration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, general_adherent_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the curriculum and interpretive framework for Torah study, including Kodashim. They benefit from the narrative that study is a valid, even superior, substitute for sacrifice, thereby legitimizing their central role in Jewish practice and intellectual life. Their authority is tied to this interpretive continuity.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% These adherents feel a spiritual longing for the restoration of physical sacrificial practice as commanded in the Torah. They are told by the dominant rabbinic institutions that their desire is either premature, misdirected, or fulfilled through prayer and study. They bear the cost of having their preferred mode of worship deemed obsolete or secondary, with no institutional pathway to actualize it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_restoration, payer,
    powerless, biographical, constrained, local).

% Benefits from a coherent, accessible, and portable system of worship (prayer and study) that does not depend on a physical temple or priesthood. This system provides spiritual continuity and meaning in the absence of sacrifice, but also implicitly accepts the substitution narrative.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, general_adherent_community, beneficiary,
    moderate, biographical, constrained, local).

% Advocate for the literal, physical restoration of the Temple and sacrificial system, viewing the current rabbinic interpretations as temporary or incomplete. They are often marginalized or dismissed by mainstream institutions, their voices absent from the dominant discourse that frames Kodashim as an archive of superseded practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restoration_advocates, excluded,
    organized, generational, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for Jewish religious practice and spiritual engagement in the absence of the Temple and physical sacrifices, ensuring continuity and accessibility for adherents globally through prayer and study.
% TRANSFER_FUNCTION: Transfers spiritual and communal authority from a Temple-centric, sacrificial system to a text-centric, rabbinic system, legitimizing rabbinic institutions as the primary interpreters and guides of Jewish life. It also transfers the 'performance' of mitzvot from physical acts to intellectual engagement.
% ABSENT_VOICES: Adherents and groups who believe in the imminent and literal restoration of the sacrificial system, and who view prayer and study as insufficient or temporary substitutes, are largely excluded from the interpretive authority that defines Kodashim's role. They would argue for active preparation for physical sacrifice, not its archival status.
% DISAPPEARANCE_RATIONALE: If the interpretive framework of Kodashim as a 'substitution archive' vanished, the entire structure of post-Temple Jewish practice would be destabilized. Rabbinic authority would be undermined, and the spiritual legitimacy of prayer and study as primary modes of worship would be called into question, leading to a profound reorganization of religious life.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship (sacrifices) impossible, creating an existential crisis for Jewish religious continuity and practice.
% FOUNDING_PROBLEM_CORROBORATION: The historical fact of the Temple's destruction and the subsequent need for new modes of worship is universally acknowledged across Jewish traditions. The ongoing spiritual challenge of connecting to ancient commandments without their literal performance is a live issue for many adherents, corroborated by theological discourse and communal introspection.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) because the interpretive framework channels spiritual energy and resources into rabbinic institutions (study, prayer) while denying the direct fulfillment of certain commandments for a segment of the community. Suppression is high (0.7) as the dominant rabbinic discourse actively marginalizes or reinterprets alternative views that advocate for literal sacrificial restoration. Theater ratio is moderate (0.4) because while the study of Kodashim is a genuine intellectual and spiritual pursuit, a significant portion of its 'function' is to perform continuity with a superseded practice, thereby maintaining the legitimacy of the substitution narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic institutions, this is a necessary and divinely sanctioned adaptation (Rope). From the perspective of those seeking literal restoration, it is an enforced reinterpretation that denies a core religious practice (Snare). The engine's classification as Tangled Rope captures this hybridity, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions are the primary beneficiaries and agenda-setters, as this reading solidifies their authority and central role. Adherents seeking sacrificial restoration are the victims, as their spiritual aspirations are deemed obsolete or misdirected. The general adherent community benefits from the stability and accessibility of the current system but also implicitly pays by accepting the substitution. Messianic restoration advocates are excluded, as their views directly challenge the foundational premise of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_substitution,
    'Is the substitution of prayer and study for sacrifice a divinely sanctioned and permanent change, or a temporary adaptation awaiting restoration?',
    'Theological consensus shift within major rabbinic authorities, or a widely accepted messianic event that re-establishes the Temple and sacrificial system.',
    'If permanent, the extractiveness from those seeking restoration is justified as adherence to evolved divine will. If temporary, the current system''s claims of continuity are theatrical, and the extraction is from those denied a legitimate, albeit deferred, practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_substitution, conceptual, 'Theological status of the substitution of prayer/study for sacrifice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of sacrificial restoration advocacy structural (institutional authority, social pressure) or internalized (adherents believe their desires are incorrect)?',
    'Post-exit suppression trajectory: if advocacy for restoration persists and gains traction after institutional barriers are lowered, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. If purely structural, removing institutional barriers would lead to a rapid increase in restoration advocacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for sacrificial restoration advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__substitution_archive, theater_ratio, 400, 0.2).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__substitution_archive, theater_ratio, 800, 0.3).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__substitution_archive, theater_ratio, 1200, 0.35).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__substitution_archive, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(koda_tr_t1950, kodashim_corpus__substitution_archive, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__substitution_archive, base_extractiveness, 400, 0.45).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__substitution_archive, base_extractiveness, 800, 0.55).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__substitution_archive, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__substitution_archive, base_extractiveness, 1600, 0.6).
narrative_ontology:measurement(koda_be_t1950, kodashim_corpus__substitution_archive, base_extractiveness, 1950, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(koda_su_t400, kodashim_corpus__substitution_archive, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__substitution_archive, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__substitution_archive, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(koda_su_t1600, kodashim_corpus__substitution_archive, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(koda_su_t1950, kodashim_corpus__substitution_archive, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Kodashim corpus kernel. Other readings, 'study_as_exercise' and 'performance_only', represent alternative interpretations of the corpus's function and status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
