% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of reading
 *   acquisition, emphasizing explicit, systematic instruction in
 *   grapheme-phoneme correspondence as a foundational skill. It is one
 *   reading of the broader 'reading_acquisition_mechanism' kernel, which also
 *   includes 'whole_language_reading' and 'balanced_literacy_reading'. This
 *   reading asserts that while there are costs (e.g., for teachers to
 *   retrain), the benefits for struggling readers and overall literacy
 *   outcomes justify the approach, making it a coordination mechanism rather
 *   than pure extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.3).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.6).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Phonics-First Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'fb5ab7dc-42c4-4052-bec9-a384593e824a').
narrative_ontology:cs_kernel_codification('fb5ab7dc-42c4-4052-bec9-a384593e824a', formalized).
narrative_ontology:cs_authority_grounding('fb5ab7dc-42c4-4052-bec9-a384593e824a', expertise).
narrative_ontology:cs_interpretation_layer_present('fb5ab7dc-42c4-4052-bec9-a384593e824a').
narrative_ontology:cs_reading_relation('fb5ab7dc-42c4-4052-bec9-a384593e824a', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('fb5ab7dc-42c4-4052-bec9-a384593e824a', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('fb5ab7dc-42c4-4052-bec9-a384593e824a', foundational, grapheme_phoneme_correspondence_is_foundational).
narrative_ontology:cs_axiom_status(grapheme_phoneme_correspondence_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('fb5ab7dc-42c4-4052-bec9-a384593e824a', grapheme_phoneme_correspondence_is_foundational, empirically_contingent).
narrative_ontology:cs_axiom('fb5ab7dc-42c4-4052-bec9-a384593e824a', foundational, explicit_systematic_instruction_is_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('fb5ab7dc-42c4-4052-bec9-a384593e824a', explicit_systematic_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('fb5ab7dc-42c4-4052-bec9-a384593e824a', science_of_reading_consensus).
narrative_ontology:cs_drift_state('fb5ab7dc-42c4-4052-bec9-a384593e824a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fb5ab7dc-42c4-4052-bec9-a384593e824a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, early_grade_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, cognitive_scientists).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, curriculum_publishers_of_other_methods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These readers disproportionately benefit from explicit, systematic phonics instruction, as it provides a clear, sequential pathway to decoding that other methods often fail to deliver. Their identity is locked into the educational system, with limited exit options from the instructional method.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, identity_locked, local).

% Teachers implementing this method must adhere to a structured curriculum, which narrows their discretion but provides a clear framework for instruction. They are responsible for delivering the systematic phonics lessons and assessing student progress. Their exit options are constrained by school district policies and professional development requirements.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, early_grade_teachers, agenda_setter,
    moderate, biographical, constrained, local).

% Research in cognitive science, particularly on the science of reading, strongly supports the efficacy of phonics-first approaches. This constraint aligns with and validates their scientific findings, reinforcing their influence in educational policy debates.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, cognitive_scientists, beneficiary,
    institutional, generational, analytical, global).

% Teachers previously trained in whole language or balanced literacy methods face significant professional development costs and a shift in pedagogical approach. This can lead to feelings of de-skilling or invalidation of prior expertise. Their exit options are constrained by professional identity and institutional inertia.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% Publishers whose materials emphasize whole language or balanced literacy face reduced market share and pressure to revise their offerings to align with phonics-first mandates. This requires substantial investment in new product development and marketing. Their exit options are constrained by market demand and regulatory changes.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_publishers_of_other_methods, payer,
    powerful, biographical, constrained, national).

% Parents often advocate for phonics-based instruction when their children struggle with reading, seeing it as a clear, evidence-based solution. They benefit from the perceived effectiveness and transparency of the method, though their options are limited by school choice.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a consistent, evidence-based approach to early reading instruction across classrooms and schools, ensuring that all students receive foundational decoding skills systematically.
% TRANSFER_FUNCTION: Transfers instructional resources, professional development, and curriculum design towards explicit phonics methods, away from approaches that de-emphasize decoding. It also transfers cognitive load from struggling readers by making decoding explicit.
% ABSENT_VOICES: Advocates for purely implicit, emergent literacy approaches (e.g., radical whole language proponents) are largely absent from policy discussions, as their methods have been widely discredited by scientific evidence. They would argue for child-led, meaning-focused engagement with text without explicit skill instruction.
% DISAPPEARANCE_RATIONALE: If the requirement for explicit systematic phonics instruction vanished, many schools would likely revert to less effective, mixed-methods approaches, leading to a resurgence in reading difficulties, particularly among vulnerable populations. Curriculum development and teacher training would also shift dramatically.
% FOUNDING_PROBLEM: A significant portion of students, especially those from disadvantaged backgrounds, failed to acquire basic reading skills under methods that did not prioritize explicit decoding instruction, leading to widespread literacy crises.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers, cognitive scientists, and parent advocacy groups consistently attest that the problem of reading failure, particularly for struggling learners, remains live and is best addressed by systematic phonics. This is corroborated by decades of empirical research and longitudinal studies, not just by those directly benefiting from the phonics industry.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively low (0.3) because the primary 'cost' is a shift in instructional method and curriculum, which is offset by improved outcomes for many students. Suppression is moderate (0.6) due to policy mandates, curriculum adoption cycles, and the strong scientific consensus that marginalizes alternative methods. Theater ratio is low (0.1) as the instruction is genuinely functional. Accessibility collapse is high (0.7) because once the scientific evidence is understood, alternatives for effective instruction are largely seen as collapsed. Resistance is moderate (0.4) from those whose prior training or commercial interests are challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of struggling readers and cognitive scientists, this is a highly beneficial coordination mechanism. From the perspective of teachers trained in other methods, it can feel like an extractive imposition that devalues their prior expertise and requires costly re-skilling. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers, cognitive scientists, and parents are beneficiaries, as the method directly addresses a critical need and aligns with scientific understanding. Early grade teachers are agenda-setters, implementing the method. Teachers trained in other methods and curriculum publishers of other methods are payers, bearing the costs of retraining and market shifts. The directionality for payers is higher due to the direct costs and professional identity challenges.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    teacher_discretion_cost,
    'Does the systematic nature of phonics instruction unduly suppress teacher autonomy and creativity, leading to hidden costs in pedagogical innovation?',
    'Longitudinal studies comparing teacher job satisfaction, retention, and innovative practice in phonics-mandated vs. more flexible instructional environments.',
    'If significant suppression of teacher autonomy is found, the effective extractiveness from teachers might be higher than currently measured, potentially shifting their seat classification towards a more extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_discretion_cost, empirical, 'Hidden costs of narrowed teacher discretion under phonics mandates.').

omega_variable(
    curriculum_market_capture,
    'To what extent has the ''science of reading'' movement led to market capture by a few large curriculum publishers, limiting pedagogical diversity and innovation?',
    'Market analysis of curriculum adoption patterns, pricing, and the entry/exit of new publishers in response to phonics mandates.',
    'If market capture is significant, the extractiveness from schools and smaller publishers could be higher, indicating a ''snare'' dynamic within the curriculum market, even if the instructional method itself is effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curriculum_market_capture, empirical, 'Market dynamics and potential capture within the phonics curriculum industry.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative reading methods primarily structural (policy mandates, funding) or internalized (teachers'' belief in phonics as the ''only'' way, professional identity tied to the ''science of reading'')?',
    'Post-mandate policy relaxation: if alternative methods do not re-emerge or are met with strong internal resistance from educators, it suggests a higher degree of internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the suppression persists even after external barriers are removed, making it harder to shift away from phonics-first approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative reading pedagogies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
