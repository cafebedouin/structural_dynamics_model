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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint describes the Kodashim corpus (laws of sacrifices and the
 *   Temple) as functioning as a 'substitution archive' within Rabbinic
 *   Judaism. In this reading, prayer and Torah study are understood to have
 *   replaced actual sacrifices, and Kodashim serves as a memorial
 *   documentation of what was superseded, rather than an active blueprint for
 *   practice or a site of ongoing spiritual performance. The constraint is a
 *   Tangled Rope because it genuinely coordinates continuity of tradition but
 *   extracts from those who seek living sacrificial practice by declaring it
 *   obsolete, while benefiting rabbinic text-study institutions.
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
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '86c5ec1a-eb7f-47f5-bb7e-5a59fc246840').
narrative_ontology:cs_kernel_codification('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', fixed_text).
narrative_ontology:cs_authority_grounding('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', lineage).
narrative_ontology:cs_interpretation_layer_present('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840').
narrative_ontology:cs_reading_relation('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_axiom('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', foundational, prayer_and_study_replace_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_replace_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', prayer_and_study_replace_sacrifice, conventional).
narrative_ontology:cs_axiom('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', foundational, kodashim_as_memorial_documentation).
narrative_ontology:cs_axiom_status(kodashim_as_memorial_documentation, holdable).
narrative_ontology:cs_axiom_grounding('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', kodashim_as_memorial_documentation, conventional).
narrative_ontology:cs_reference_frame('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', rabbinic_post_temple_adaptation).
narrative_ontology:cs_drift_state('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', contemporary_zionist_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('86c5ec1a-eb7f-47f5-bb7e-5a59fc246840', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the curriculum and interpretive framework for Torah study, including Kodashim. They benefit from the constraint by positioning text study as the primary, and currently only, legitimate form of engagement with sacrificial law, thereby securing their central role in religious life and education. Their authority is grounded in this interpretive tradition.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% These adherents feel a spiritual longing or halakhic obligation for the restoration of physical sacrificial rites. They are told by the dominant interpretive framework that such practices are currently obsolete and replaced by prayer and study, effectively denying them a direct path to fulfilling this aspect of their faith. Their 'payment' is the deferral or sublimation of their religious impulse.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_practice, payer,
    powerless, biographical, identity_locked, local).

% The historical institution of the priesthood, whose primary function was to perform sacrifices, is now largely ceremonial. Its functional role has been superseded by rabbinic authority and text study, leaving it without its original purpose within the current framework. It is excluded from the active performance of the rites it was once central to.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_sacrificial_priesthood, excluded,
    institutional, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(kodashim_corpus__substitution_archive, historical_sacrificial_priesthood).

% These groups actively anticipate and advocate for the rebuilding of the Temple and the resumption of sacrificial worship. They observe the current rabbinic framework as a temporary, albeit necessary, substitute, but fundamentally believe in the eventual return to physical rites. They are 'observers' in that they do not currently perform sacrifices but actively critique the 'substitution' aspect of the archive.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restoration_advocates, observer,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for religious observance and continuity of tradition in the absence of the Temple and sacrificial cult, by re-channeling religious energy into prayer and study.
% TRANSFER_FUNCTION: Transfers the locus of religious authority and practice from a physical, Temple-centric cult to a text-based, rabbinic-led system, thereby transferring spiritual and social capital to rabbinic institutions.
% ABSENT_VOICES: The direct voice of the historical sacrificial priesthood is absent, as their function has been superseded. Adherents who prioritize immediate, physical sacrificial practice over textual engagement are marginalized within the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the interpretive framework of Kodashim as a substitution archive vanished, the entire structure of post-Temple Judaism would be destabilized. Rabbinic authority would be undermined, and there would be immense pressure to either restore sacrificial practice or develop entirely new modes of religious engagement, leading to a profound reorganization of religious life.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of sacrificial worship left a void in Jewish religious practice and a crisis of continuity for the covenantal relationship with God.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of the Temple's destruction and the subsequent rabbinic efforts to maintain Jewish life corroborates the founding problem. The ongoing spiritual longing for messianic restoration among many adherents further attests to the problem's continued, albeit transformed, existence, even if the 'solution' is contested.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.6) because the interpretive framework, while providing continuity, denies a direct path to a specific form of religious expression for some adherents. Suppression is high (0.7) as the rabbinic interpretive tradition actively enforces the 'substitution' narrative, making it difficult for alternative practices to gain legitimacy. Theater ratio is moderate (0.4) because while the study of Kodashim is presented as a form of engagement, it is not the actual performance of sacrifices, creating a performative aspect to its maintenance. The metrics reflect the long historical period of this interpretive framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic institutions, this is a necessary and legitimate adaptation ensuring the survival of Judaism. From the perspective of those longing for physical sacrifices, it is a form of spiritual extraction, where their direct religious expression is suppressed in favor of an intellectual substitute. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions are the primary beneficiaries (d near 0.0) as their authority and function are elevated by this interpretive framework. Adherents seeking sacrificial practice are the victims (d near 1.0) as their direct religious impulse is re-channeled or denied. The historical priesthood is excluded, its function superseded. Messianic restoration advocates are observers, critiquing the current state but not directly benefiting or paying in the same way.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_substitution,
    'Is the substitution of prayer and study for sacrifice a legitimate halakhic (Jewish law) development, or a temporary, post-destruction necessity elevated to a permanent status?',
    'Analysis of early rabbinic texts for explicit statements on the permanence vs. temporariness of substitution, and theological debate within contemporary halakhic discourse.',
    'If deemed a temporary necessity, the extractiveness from adherents seeking sacrificial practice would be re-evaluated as more severe, as the ''archive'' status would be seen as a deferral rather than a true replacement. If permanent, the current extractiveness would be seen as a legitimate cost of adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_substitution, conceptual, 'Theological and halakhic status of substitution.').

omega_variable(
    spiritual_fulfillment_metrics,
    'To what extent do prayer and Torah study genuinely fulfill the spiritual needs previously met by sacrificial practice for all adherents?',
    'Sociological and psychological studies of religious experience among adherents, comparing reported spiritual satisfaction across different modes of observance.',
    'If a significant portion of adherents report unmet spiritual needs, the ''substitution'' aspect of the archive would be seen as more extractive, as it fails to fully replace the original function. If satisfaction is high, the extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_fulfillment_metrics, empirical, 'Empirical measure of spiritual fulfillment from substitution.').

omega_variable(
    mandatrophy_of_priesthood,
    'Has the functional mandate of the historical sacrificial priesthood truly atrophied, or is its current ceremonial status a form of suppressed function awaiting restoration?',
    'Historical and anthropological analysis of the priesthood''s role in different eras, and analysis of contemporary movements advocating for priestly functional restoration.',
    'If the mandate is merely suppressed, the ''excluded'' status of the priesthood would be re-evaluated as a more active form of extraction, where a legitimate function is denied. If truly atrophied, the current status is a natural consequence of historical change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_priesthood, empirical, 'Status of the priesthood''s functional mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.3).
narrative_ontology:measurement(koda_tr_t300, kodashim_corpus__substitution_archive, theater_ratio, 300, 0.35).
narrative_ontology:measurement(koda_tr_t600, kodashim_corpus__substitution_archive, theater_ratio, 600, 0.38).
narrative_ontology:measurement(koda_tr_t900, kodashim_corpus__substitution_archive, theater_ratio, 900, 0.4).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__substitution_archive, theater_ratio, 1200, 0.42).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.41).
narrative_ontology:measurement(koda_tr_t1950, kodashim_corpus__substitution_archive, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(koda_be_t300, kodashim_corpus__substitution_archive, base_extractiveness, 300, 0.55).
narrative_ontology:measurement(koda_be_t600, kodashim_corpus__substitution_archive, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(koda_be_t900, kodashim_corpus__substitution_archive, base_extractiveness, 900, 0.6).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__substitution_archive, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.61).
narrative_ontology:measurement(koda_be_t1950, kodashim_corpus__substitution_archive, base_extractiveness, 1950, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(koda_su_t300, kodashim_corpus__substitution_archive, suppression_requirement, 300, 0.65).
narrative_ontology:measurement(koda_su_t600, kodashim_corpus__substitution_archive, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(koda_su_t900, kodashim_corpus__substitution_archive, suppression_requirement, 900, 0.7).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__substitution_archive, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.69).
narrative_ontology:measurement(koda_su_t1950, kodashim_corpus__substitution_archive, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Kodashim corpus kernel, focusing on its role as a substitution archive. It is linked to sibling readings that interpret the corpus as 'study_as_exercise' or 'performance_only'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
