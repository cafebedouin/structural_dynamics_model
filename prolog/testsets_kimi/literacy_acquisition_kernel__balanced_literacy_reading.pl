% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Balanced Literacy Reading Instruction Mandate
 *   domain: educational/psychological
 *
 * SUMMARY:
 *   This constraint is the balanced_literacy_reading of the
 *   literacy_acquisition_kernel. It claims that reading acquisition requires
 *   both systematic phonics instruction and meaningful text engagement in
 *   complementary balance. In practice, this reading has become the dominant
 *   institutional framework in North American teacher preparation and
 *   district curriculum adoption. The source material identifies moderate
 *   extractiveness driven by method churn revenue for education publishers
 *   and schools of education, with unclear victim identity. The constraint is
 *   contested as either a genuine pedagogical synthesis or a rebranding of
 *   whole language designed to preserve its institutional infrastructure
 *   after phonics won political battles.
 *
 * KEY AGENTS:
 *   - education_publishers: Primary beneficiary (powerful/mobile) â captures method churn revenue via curriculum and assessment sales
 *   - teacher_training_institutions: Primary beneficiary/agenda setter (institutional/mobile) â certifies teachers in the framework and perpetuates professional development demand
 *   - classroom_educators: Primary payer (moderate/constrained) â bears implementation burden and professional confusion
 *   - struggling_readers: Target (powerless/trapped) â receive diluted phonics instruction in the name of balance
 *   - school_districts: Agenda setter (institutional/constrained) â mandates adoption, locked into contracts
 *   - cognitive_science_researchers: Analytical observer (analytical/analytical) â documents reading acquisition science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.6).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Instruction Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational/psychological").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'c8af690d-ca37-4ca5-b690-54266f7df72f').
narrative_ontology:cs_kernel_codification('c8af690d-ca37-4ca5-b690-54266f7df72f', formalized).
narrative_ontology:cs_authority_grounding('c8af690d-ca37-4ca5-b690-54266f7df72f', extraction).
narrative_ontology:cs_interpretation_layer_present('c8af690d-ca37-4ca5-b690-54266f7df72f').
narrative_ontology:cs_reading_relation('c8af690d-ca37-4ca5-b690-54266f7df72f', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('c8af690d-ca37-4ca5-b690-54266f7df72f', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('c8af690d-ca37-4ca5-b690-54266f7df72f', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('c8af690d-ca37-4ca5-b690-54266f7df72f', foundational, decoding_and_meaning_are_co_primary).
narrative_ontology:cs_axiom_status(decoding_and_meaning_are_co_primary, holdable).
narrative_ontology:cs_axiom_grounding('c8af690d-ca37-4ca5-b690-54266f7df72f', decoding_and_meaning_are_co_primary, empirically_contingent).
narrative_ontology:cs_axiom('c8af690d-ca37-4ca5-b690-54266f7df72f', foundational, teacher_autonomy_determines_balance).
narrative_ontology:cs_axiom_status(teacher_autonomy_determines_balance, holdable).
narrative_ontology:cs_axiom_grounding('c8af690d-ca37-4ca5-b690-54266f7df72f', teacher_autonomy_determines_balance, conventional).
narrative_ontology:cs_reference_frame('c8af690d-ca37-4ca5-b690-54266f7df72f', integrated_literacy_acquisition).
narrative_ontology:cs_drift_state('c8af690d-ca37-4ca5-b690-54266f7df72f', post_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c8af690d-ca37-4ca5-b690-54266f7df72f', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_institutions).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and sells balanced literacy curriculum packages, leveled readers, and assessment systems. Revenue depends on district adoption cycles and the perpetual updating of materials to reflect shifting definitions of balance. Can pivot to new market trends if district demand shifts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Certifies pre-service teachers and provides ongoing professional development in balanced literacy frameworks. Reputation and enrollment depend on maintaining the balanced literacy paradigm as the research consensus. Sets the pedagogical norm that new teachers carry into districts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_institutions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_institutions, beneficiary).

% Required to implement district-mandated balanced literacy programs. Must attend publisher-led professional development and purchase classroom libraries and assessment kits. Caught between directives to teach systematic phonics and directives to prioritize authentic text and student choice, with little guidance on how to weight the two.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_educators, payer,
    moderate, biographical, constrained, regional).

% Children with dyslexia or other reading difficulties who require explicit, systematic decoding instruction. In balanced literacy classrooms, they often receive incidental phonics mini-lessons rather than the cumulative, mastery-based instruction they need, and fall further behind grade level.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Adopt balanced literacy curricula based on state standards, publisher marketing, and teacher preparation pipelines. Locked into multi-year textbook contracts and professional development agreements. Face political pressure from parents and state legislators when reading scores stagnate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, school_districts, agenda_setter,
    institutional, biographical, constrained, regional).

% Conduct experimental and longitudinal research on reading acquisition. Increasingly find that explicit systematic phonics is non-negotiable for at-risk readers, and that the balance frame lacks empirical precision. Their findings often conflict with balanced literacy training materials but have begun influencing state policy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_science_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the teaching of reading by integrating explicit phonics instruction with meaningful text engagement, preventing over-reliance on either isolated skill drills or unstructured immersion.
% TRANSFER_FUNCTION: Moves district funding and teacher attention toward hybrid curriculum packages and professional development; moves a portion of that funding to education publishers and teacher training institutions as the cost of materials and certification.
% ABSENT_VOICES: Explicit systematic phonics advocates who view balance as dilution; whole language purists who view any phonics emphasis as harmful; parents of struggling readers often excluded from curriculum adoption decisions.
% DISAPPEARANCE_RATIONALE: If the balanced literacy mandate disappeared, districts would reorganize around either structured literacy or whole language programs, publisher revenue streams would shift, teacher training curricula would be rewritten, and classroom practice would lose its central organizing framework.
% FOUNDING_PROBLEM: How to teach reading effectively to diverse learners without over-relying on either rigid phonics drills that drain motivation or unstructured whole language exposure that leaves decoding to chance.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers and special education advocates outside the teacher-training and publishing industries attest that the founding problem is contested; they argue that explicit systematic phonics with integrated comprehension instruction already solves the decoding challenge, making the balanced synthesis unnecessary. The benefiting parties assert the problem remains live.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.6, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.60) is moderate-to-high because the inherent ambiguity of balance allows perpetual curriculum replacement and professional development cycles that transfer public funds to publishers and ed schools. Suppression (0.55) reflects the marginalization of structured literacy alternatives within mainstream teacher preparation. Theater ratio (0.45) captures the growing gap between the performative language of balance and actual classroom implementation, where teachers lack clear guidance on weighting. Accessibility collapse (0.65) models how alternatives exist but are structurally difficult to access for districts embedded in balanced literacy supply chains. Resistance (0.60) registers the science-of-reading pushback that has begun shifting state legislation. The measurement series run on a single shared grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher and ed-school seat, balanced literacy is necessary professional infrastructure producing educated teachers and literate children; from the classroom educator and struggling-reader seat, it is a vague mandate that consumes resources without clear efficacy. The engine computes this divergence from the structural asymmetry in power and exit: beneficiaries are mobile and organized, while victims are constrained or trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Education publishers and teacher training institutions are structural beneficiaries (low d): they collect revenue from the constraint's perpetuation and can pivot to alternative markets if demand shifts. Classroom educators and struggling readers are structural targets (high d): they bear the implementation and learning costs with constrained or trapped exit. School districts sit near symmetric (moderate d): they both mandate and are locked into the arrangement, experiencing it as institutional inertia rather than clear benefit or harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint could be misread as a Rope if one looks only at the coordination function (reading does require both phonics and meaning). It could be misread as a Snare if one looks only at the commercial extraction. The Tangled Rope classification captures that both are true: the constraint coordinates a real pedagogical need while asymmetrically enriching publishers and ed schools through perpetual methodological churn. The absence of a sunset clause and the active enforcement through teacher certification prevent Scaffold classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_or_rebrand,
    'Is balanced literacy a genuine pedagogical synthesis of phonics and whole language, or a rebranding of whole language that preserves its core tenets while avoiding political liability?',
    'Historical genealogy analysis of author networks, institutional funding flows, and curricular text lineage; if balanced literacy curricula trace intellectual ancestry primarily to whole_language thinkers with surface phonics additions, the rebrand thesis is strengthened.',
    'If rebrand, the constraint''s coordination function is cover for whole_language persistence and its extraction profile rises; if genuine synthesis, the moderate extraction may be the necessary cost of integrating competing evidence bases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_or_rebrand, conceptual, 'Whether balanced literacy is a genuine synthesis or a whole_language rebrand').

omega_variable(
    method_churn_extraction,
    'Do education publishers and teacher training institutions benefit from balanced literacy''s inherent ambiguityâwhat constitutes balanceâin ways that perpetuate curricular churn and professional development revenue?',
    'Economic analysis of textbook adoption cycles, professional development contracts, and ed-school enrollment tied to balanced literacy certification versus stable alternative programs.',
    'If method churn is a primary revenue driver, the constraint functions as institutional extraction; if commercialization is incidental, the constraint may be closer to a genuine coordination mechanism with low excess extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_churn_extraction, empirical, 'Whether commercial interests drive persistence through method churn').

omega_variable(
    victim_identity_ambiguity,
    'If balanced literacy fails to deliver adequate phonics instruction to struggling readers while also failing to deliver adequate meaning-focused engagement, who bears the structural cost?',
    'Longitudinal literacy outcome analysis comparing balanced literacy districts to structured literacy districts, disaggregated by student reading-risk profiles.',
    'If struggling readers show significantly worse outcomes under balanced literacy, they constitute a clear victim class shifting the constraint toward snare classification; if outcomes are equivalent, the extraction may be primarily financial with diffuse pedagogical costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_ambiguity, empirical, 'Identity and magnitude of the victim class under balanced literacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four structurally distinct readings: phonics_reading (decoding-first), whole_language_reading (meaning-first), structured_literacy_reading (explicit systematic cumulative), and balanced_literacy_reading (co-primary balance). Each reading has different epsilon, stakeholder structures, and empirical status. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
