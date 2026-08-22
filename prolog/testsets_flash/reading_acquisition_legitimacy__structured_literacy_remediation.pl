% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Mandate for Vulnerable Learners
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the policy mandate that reading instruction
 *   must prioritize the needs of the most vulnerable learners, specifically
 *   by adopting explicit, cumulative, and diagnostic principles derived from
 *   structured literacy. This approach, while highly beneficial for
 *   struggling readers, imposes significant costs and constraints on general
 *   education teachers and school districts, who must retrain and overhaul
 *   curricula. The constraint is claimed as a 'tangled_rope' because it
 *   genuinely coordinates effective instruction for a critical population
 *   (beneficiaries) but does so through a highly prescriptive and costly
 *   mandate that extracts from other stakeholders (victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.78).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate for Vulnerable Learners").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, 'f6da3c65-ae06-4baa-b453-093f89d1b0e0').
narrative_ontology:cs_kernel_codification('f6da3c65-ae06-4baa-b453-093f89d1b0e0', formalized).
narrative_ontology:cs_authority_grounding('f6da3c65-ae06-4baa-b453-093f89d1b0e0', expertise).
narrative_ontology:cs_interpretation_layer_present('f6da3c65-ae06-4baa-b453-093f89d1b0e0').
narrative_ontology:cs_reading_relation('f6da3c65-ae06-4baa-b453-093f89d1b0e0', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('f6da3c65-ae06-4baa-b453-093f89d1b0e0', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('f6da3c65-ae06-4baa-b453-093f89d1b0e0', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('f6da3c65-ae06-4baa-b453-093f89d1b0e0', foundational, universal_design_for_vulnerable_learners).
narrative_ontology:cs_axiom_status(universal_design_for_vulnerable_learners, holdable).
narrative_ontology:cs_axiom_grounding('f6da3c65-ae06-4baa-b453-093f89d1b0e0', universal_design_for_vulnerable_learners, deontological).
narrative_ontology:cs_axiom('f6da3c65-ae06-4baa-b453-093f89d1b0e0', foundational, explicit_diagnostic_cumulative_instruction_efficacy).
narrative_ontology:cs_axiom_status(explicit_diagnostic_cumulative_instruction_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('f6da3c65-ae06-4baa-b453-093f89d1b0e0', explicit_diagnostic_cumulative_instruction_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('f6da3c65-ae06-4baa-b453-093f89d1b0e0', intervention_grade_instruction_as_universal_standard).
narrative_ontology:cs_drift_state('f6da3c65-ae06-4baa-b453-093f89d1b0e0', contemporary_policy_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6da3c65-ae06-4baa-b453-093f89d1b0e0', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_and_reading_difficulties).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_science_researchers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, students_without_diagnosed_difficulties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students are the primary intended beneficiaries, receiving explicit, systematic, and diagnostic instruction tailored to their needs, which is often unavailable in general education settings. Their 'exit' from reading difficulties depends entirely on effective instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_and_reading_difficulties, beneficiary,
    powerless, biographical, trapped, local).

% Researchers, parent groups, and professional organizations who champion structured literacy principles. They actively lobby for policy changes, provide training, and publish research, seeking to embed these methods as the standard for all instruction, especially for vulnerable learners.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Their research on reading acquisition and brain function provides the scientific grounding for structured literacy. Policies adopting these principles validate their work and often lead to increased funding and influence for their field.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_science_researchers, beneficiary,
    institutional, generational, analytical, global).

% Required to adopt new, often intensive, instructional methods that demand significant retraining, curriculum overhaul, and continuous diagnostic assessment. This imposes substantial time and resource costs, often without adequate support or reduced class sizes. Their exit options are limited by employment contracts and professional norms.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Bear the financial and logistical burden of implementing structured literacy across all classrooms: purchasing new materials, funding extensive professional development, and managing resistance from staff. They face pressure from advocates and policymakers, with limited budget flexibility.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts, payer,
    institutional, biographical, constrained, regional).

% While potentially benefiting from explicit instruction, they may experience a curriculum that feels overly prescriptive or slow-paced, potentially stifling their engagement with literature or advanced comprehension skills if not balanced. They have no direct exit from the mandated instructional approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_without_diagnosed_difficulties, payer,
    powerless, biographical, trapped, local).

% Proponents of whole language or balanced literacy approaches, who argue for more holistic, meaning-centered instruction. Their pedagogical frameworks are increasingly marginalized or explicitly rejected by policies favoring structured literacy, limiting their influence and professional opportunities.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all students, particularly those at risk of reading failure, receive a consistent, evidence-based approach to literacy instruction that addresses foundational skills systematically.
% TRANSFER_FUNCTION: Transfers pedagogical authority and resources towards explicit, diagnostic, and cumulative instructional methods, away from more emergent or balanced approaches. It also transfers the burden of extensive retraining and curriculum change to general education teachers and school districts.
% ABSENT_VOICES: Advocates for whole language or balanced literacy, who would argue that an exclusive focus on remediation-grade structured literacy for all learners risks de-emphasizing meaning-making, critical thinking, and engagement with diverse texts, potentially harming advanced readers or those who learn differently. Their voices are often dismissed as 'unscientific' in the current policy climate.
% DISAPPEARANCE_RATIONALE: If this mandate vanished, many school districts would revert to less structured, less resource-intensive literacy programs, potentially leaving vulnerable learners without the explicit instruction they need. The pedagogical landscape would become more varied, but also less consistently evidence-based for struggling readers.
% FOUNDING_PROBLEM: A significant and persistent population of students, particularly those with dyslexia or other learning disabilities, were failing to acquire basic reading skills due to inadequate, unsystematic, or unscientific instructional methods in general education.
% FOUNDING_PROBLEM_CORROBORATION: Longitudinal studies on reading outcomes, diagnostic assessments showing persistent gaps in foundational skills, and advocacy groups representing parents of struggling readers consistently corroborate that the problem of reading failure for vulnerable learners remains live and urgent. This corroboration comes from independent researchers and parent organizations, not just the direct beneficiaries of structured literacy policy.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the high costs of retraining, curriculum changes, and the potential narrowing of pedagogical approaches for teachers and districts. Suppression (0.78) is high due to the strong policy push, often backed by legislation, which limits alternatives and penalizes non-compliance. Theater ratio (0.20) is relatively low, as the core function of improving reading outcomes for vulnerable learners is genuinely pursued, though some performative compliance may exist. Accessibility collapse (0.60) indicates that while alternative pedagogies are not entirely eliminated, their viability within mainstream policy is significantly reduced. Resistance (0.70) is substantial, coming from educators and advocates for other literacy approaches who feel their expertise is devalued or that the mandate is too rigid.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of structured literacy advocates and parents of struggling readers, this is a necessary 'rope' or even a 'mountain' of scientific consensus, ensuring equitable access to literacy. From the perspective of many general education teachers and advocates for broader literacy approaches, it functions as a 'snare' or 'tangled_rope,' imposing a narrow, costly, and potentially inappropriate pedagogical model on all learners.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with reading difficulties and structured literacy advocates are clear beneficiaries (low d). Cognitive science researchers also benefit from the validation and influence of their work. General education teachers and school districts are payers (high d) due to the imposed costs and loss of pedagogical autonomy. Students without diagnosed difficulties are also payers, as they may experience a less engaging curriculum. Whole language advocates are excluded, their pedagogical approach actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_remediation_for_all,
    'Is ''remediation-grade'' instruction appropriate and beneficial for all learners, including those without diagnosed reading difficulties, or does it impose unnecessary constraints?',
    'Longitudinal studies comparing outcomes for diverse learners in structured literacy-only environments versus those in more differentiated settings, measuring not just decoding but also comprehension, engagement, and critical thinking.',
    'If remediation-grade instruction proves universally beneficial, the ''payer'' status of students without difficulties would diminish. If it proves suboptimal for some, the constraint''s extractiveness and suppression would be re-evaluated as higher for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_remediation_for_all, empirical, 'Whether the ''vulnerable learners first'' principle scales effectively to all students without negative side effects.').

omega_variable(
    teacher_autonomy_vs_fidelity,
    'To what extent does the mandate for structured literacy stifle teacher autonomy and pedagogical innovation, and is this a necessary cost for fidelity to evidence-based practice?',
    'Qualitative studies on teacher morale, retention, and self-reported pedagogical effectiveness under strict structured literacy mandates, alongside analysis of innovative practices that emerge outside these mandates.',
    'If teacher autonomy is severely curtailed without clear benefits, the suppression metric for teachers would be higher. If fidelity to structured literacy is shown to be impossible without strict mandates, the suppression would be seen as a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_fidelity, conceptual, 'Balancing fidelity to evidence-based practice with teacher professionalism and autonomy.').

omega_variable(
    funding_adequacy_for_implementation,
    'Are school districts and teachers receiving adequate funding and resources to implement structured literacy mandates effectively, or are they being asked to do more with less?',
    'Audits of state and federal funding allocations for literacy initiatives, comparative analysis of implementation costs versus provided resources, and surveys of district and teacher resource needs.',
    'If funding is inadequate, the extractiveness from school districts and teachers is higher than currently measured, as they are forced to reallocate existing, insufficient resources. This would strengthen the ''snare'' or ''tangled_rope'' classification for these seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_adequacy_for_implementation, empirical, 'Whether the costs of implementation are fully covered by policy-makers or borne by implementers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel. Its emphasis on remediation-grade instruction for all learners influences the viability and legitimacy of other pedagogical approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
