% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   instruction, which posits that reading acquisition requires both
 *   systematic phonics instruction and meaningful text engagement, viewing
 *   them as complementary. It emerged as a response to the 'reading wars'
 *   between phonics and whole language. This is one reading of the broader
 *   'literacy_acquisition_kernel'. The claimed type is 'tangled_rope' because
 *   it attempts a coordination function (synthesizing approaches) but
 *   exhibits asymmetric extraction, benefiting education institutions and
 *   publishers while potentially underserving struggling readers.
 *
 * KEY AGENTS:
 *   - education_schools: Agenda setter (institutional/constrained)
 *   - educational_publishers: Beneficiary (organized/mobile)
 *   - balanced_literacy_consultants: Beneficiary (moderate/mobile)
 *   - early_career_teachers: Payer (powerless/identity_locked)
 *   - struggling_readers: Payer (powerless/trapped)
 *   - cognitive_scientists: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.45).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Acquisition").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'b4ff989a-7edc-43dc-84a7-3647f7b9bd94').
narrative_ontology:cs_kernel_codification('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', formalized).
narrative_ontology:cs_authority_grounding('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', lineage).
narrative_ontology:cs_interpretation_layer_present('b4ff989a-7edc-43dc-84a7-3647f7b9bd94').
narrative_ontology:cs_reading_relation('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', foundational, reading_is_balanced_act).
narrative_ontology:cs_axiom_status(reading_is_balanced_act, holdable).
narrative_ontology:cs_axiom_grounding('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', reading_is_balanced_act, conventional).
narrative_ontology:cs_axiom('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', foundational, phonics_and_meaning_are_complementary).
narrative_ontology:cs_axiom_status(phonics_and_meaning_are_complementary, holdable).
narrative_ontology:cs_axiom_grounding('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', phonics_and_meaning_are_complementary, conventional).
narrative_ontology:cs_reference_frame('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', synthesis_of_reading_instruction).
narrative_ontology:cs_drift_state('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', contemporary_science_of_reading_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b4ff989a-7edc-43dc-84a7-3647f7b9bd94', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, educational_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_consultants).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, early_career_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and train teachers in balanced literacy approaches, benefiting from curriculum development and professional development revenue. They maintain a position of authority in pedagogical discourse.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, agenda_setter,
    institutional, generational, constrained, national).

% Develop and sell 'balanced' curriculum materials, often incorporating elements from both phonics and whole language, generating revenue from the continuous churn of instructional methods.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, educational_publishers, beneficiary,
    organized, biographical, mobile, global).

% Provide professional development and implementation support for balanced literacy programs to school districts, profiting from the ongoing need for training and interpretation of the approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_consultants, beneficiary,
    moderate, biographical, mobile, regional).

% Are trained in balanced literacy methods and are expected to implement them, often feeling unprepared for the diverse needs of students, particularly those struggling with decoding. Their professional identity is often tied to the methods taught in their training.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, early_career_teachers, payer,
    powerless, immediate, identity_locked, local).

% May not receive sufficient explicit, systematic phonics instruction under a balanced literacy model, leading to persistent decoding difficulties and a widening achievement gap. Their access to effective instruction is constrained by the dominant pedagogical approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Analyze the empirical evidence for different reading instruction methods, often finding that balanced literacy lacks a clear, consistent definition and may not adequately address the needs of all learners, particularly in phonics.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate instructional practices by integrating phonics and whole language approaches, providing a framework for teachers to address both decoding and comprehension in a holistic manner.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum development revenue to education schools and publishers, while transferring instructional burden and potential learning gaps to teachers and struggling readers.
% ABSENT_VOICES: Advocates for explicit, systematic phonics instruction, particularly those representing students with dyslexia, are often marginalized in balanced literacy discourse, arguing that the 'balance' often skews away from sufficient foundational skills.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, there would be a significant shift in teacher training, curriculum development, and classroom practice. Schools would likely adopt more explicitly phonics-focused or structured literacy approaches, leading to a reorganization of the literacy education landscape.
% FOUNDING_PROBLEM: The 'reading wars' between phonics and whole language created a polarized instructional landscape, leading to a desire for a synthetic approach that could address both decoding and comprehension without ideological conflict.
% FOUNDING_PROBLEM_CORROBORATION: Education schools and publishers claim the problem is live, as the need for a comprehensive approach persists. Cognitive scientists and advocates for structured literacy argue that the 'balance' often fails to adequately address the foundational skills gap, suggesting the original problem remains unresolved or has been reframed to maintain the status quo of certain pedagogical institutions.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) stems from the continuous demand for new curriculum materials and professional development, often without clear evidence of superior outcomes for all students. Suppression (0.45) is present through the institutional pressure on teachers to adopt this pedagogical framework, limiting their autonomy to implement alternative, potentially more effective, methods. The theater ratio (0.3) reflects that while some genuine coordination occurs, a portion of the activity serves to maintain the market for 'balanced' resources and training, rather than purely optimizing instructional efficacy. Accessibility collapse (0.4) is moderate, as alternative approaches exist but are often disfavored or actively suppressed within institutional settings. Resistance (0.5) is also moderate, coming from researchers and parent advocacy groups, but not strong enough to dislodge the dominant paradigm.
 *
 * PERSPECTIVAL GAP:
 *   Education schools and publishers perceive balanced literacy as a necessary, effective synthesis, a 'rope' that solves the 'reading wars'. Early career teachers and struggling readers, however, experience it as a 'snare' or 'tangled_rope', where the 'balance' often means insufficient explicit instruction, leading to professional frustration and academic struggle. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools, publishers, and consultants are beneficiaries, as they profit from the perpetuation and implementation of balanced literacy. Early career teachers and struggling readers are payers, bearing the costs of inadequate preparation and instructional gaps. Cognitive scientists act as observers, analyzing the empirical outcomes without direct benefit or cost from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling balanced literacy as pure coordination. While it purports to solve the 'reading wars' (a coordination problem), the rising extractiveness and suppression over time suggest it has accumulated rent-seeking layers. The 'contested' status of the founding problem further indicates that its mandate may have atrophied, with the original problem either solved or reframed to justify continued extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebrand,
    'Is balanced literacy a genuine pedagogical synthesis, or a rebranding of whole language with minimal phonics integration, designed to appease critics?',
    'Detailed content analysis of balanced literacy curricula and classroom observations to quantify the proportion and explicitness of phonics instruction versus immersion in meaningful texts, compared to the original whole language approach.',
    'If a rebrand, its extractiveness and suppression would be reclassified higher, aligning more closely with a ''snare'' due to the deceptive coordination claim. If a genuine synthesis, its coordination function would be stronger, potentially moving it closer to a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebrand, empirical, 'Assesses the authenticity of the ''synthesis'' claim in balanced literacy.').

omega_variable(
    instructional_efficacy_for_all,
    'Does balanced literacy effectively teach all students to read, particularly those with pre-existing decoding difficulties or dyslexia, compared to structured literacy approaches?',
    'Longitudinal studies comparing reading outcomes (decoding, fluency, comprehension) for diverse student populations under balanced literacy versus structured literacy instructional models.',
    'If balanced literacy consistently underperforms for struggling readers, its victim set would be more clearly defined and its extractiveness for those students would be higher, reinforcing a ''snare'' classification for that seat. If it performs comparably, its coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instructional_efficacy_for_all, empirical, 'Evaluates the instructional efficacy of balanced literacy for diverse learners.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression on early career teachers structural (institutional mandates, lack of alternative training) or internalized (professional identity tied to balanced literacy, fear of deviating from norms)?',
    'Post-exit suppression trajectory: if teachers who leave balanced literacy-dominant institutions continue to feel constrained in their pedagogical choices, it suggests internalized suppression. Surveys on teacher autonomy and perceived pressure.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — teachers carry the suppression with them after exit, making the ''identity_locked'' exit option more salient. If primarily structural, policy changes could more easily alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for teachers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(lite_tr_t1998, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1998, 0.23).
narrative_ontology:measurement(lite_tr_t2006, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2006, 0.26).
narrative_ontology:measurement(lite_tr_t2014, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(lite_be_t1998, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1998, 0.45).
narrative_ontology:measurement(lite_be_t2006, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2006, 0.5).
narrative_ontology:measurement(lite_be_t2014, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(lite_su_t1998, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement(lite_su_t2006, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2006, 0.41).
narrative_ontology:measurement(lite_su_t2014, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2014, 0.43).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', which also includes 'phonics_reading', 'whole_language_reading', and 'structured_literacy_reading'. Each represents a distinct pedagogical approach to reading acquisition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
