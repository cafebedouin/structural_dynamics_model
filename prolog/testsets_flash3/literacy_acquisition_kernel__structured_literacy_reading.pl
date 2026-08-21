% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'structured literacy' approach to reading
 *   instruction, which mandates explicit, systematic, and cumulative teaching
 *   of foundational literacy skills (phonological awareness, phonics,
 *   fluency, vocabulary, comprehension). Originating from interventions for
 *   dyslexia (Orton-Gillingham tradition), it is increasingly advocated for
 *   universal application. While it genuinely coordinates effective
 *   instruction for struggling readers, its universal imposition creates
 *   significant training burdens for general education teachers and school
 *   districts, leading to a hybrid coordination/extraction dynamic. This is
 *   one reading of the broader 'literacy_acquisition_kernel', focusing on the
 *   specific structural implications of the structured literacy mandate.
 *
 * KEY AGENTS:
 *   - students_with_learning_disabilities: Primary beneficiary (powerless/identity_locked) — receives targeted intervention.
 *   - special_education_teachers: Agenda setter/beneficiary (organized/constrained) — expertise validated, but constrained by system.
 *   - general_education_teachers: Primary payer (moderate/constrained) — bears training burden, pedagogical shift.
 *   - school_districts: Payer (institutional/constrained) — bears financial and logistical costs.
 *   - structured_literacy_curriculum_providers: Beneficiary (powerful/arbitrage) — profits from increased demand.
 *   - students_without_diagnosed_disabilities: Payer/beneficiary (powerless/trapped) — universal application may not be optimal for all.
 *   - whole_language_advocates: Excluded (organized/constrained) — marginalized by the ascendant paradigm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Instruction Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '6b1608cc-213d-4e33-bcc1-3eb13423d9ab').
narrative_ontology:cs_kernel_codification('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', formalized).
narrative_ontology:cs_authority_grounding('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', expertise).
narrative_ontology:cs_interpretation_layer_present('6b1608cc-213d-4e33-bcc1-3eb13423d9ab').
narrative_ontology:cs_reading_relation('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', foundational, reading_is_a_learned_skill_not_natural).
narrative_ontology:cs_axiom_status(reading_is_a_learned_skill_not_natural, holdable).
narrative_ontology:cs_axiom_grounding('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', reading_is_a_learned_skill_not_natural, empirically_contingent).
narrative_ontology:cs_axiom('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', foundational, explicit_systematic_instruction_is_essential).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', explicit_systematic_instruction_is_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', orton_gillingham_principles).
narrative_ontology:cs_drift_state('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', contemporary_education_policy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6b1608cc-213d-4e33-bcc1-3eb13423d9ab', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_providers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, students_without_diagnosed_disabilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_without_diagnosed_disabilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive targeted, evidence-based instruction that directly addresses their specific learning needs, leading to improved literacy outcomes. Their 'identity_locked' status reflects their inherent learning profile, making alternative instructional approaches ineffective or harmful.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities, beneficiary,
    powerless, biographical, identity_locked, local).

% Are trained and certified in structured literacy approaches (e.g., Orton-Gillingham), advocating for its implementation and often leading professional development. They benefit from the validation of their specialized expertise but are constrained by funding and systemic resistance to broader adoption.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers, agenda_setter,
    organized, biographical, constrained, national).

% Develop and sell specialized curricula and training programs aligned with structured literacy principles. They profit from increased demand for their materials and services as this approach gains traction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_providers, beneficiary,
    powerful, generational, arbitrage, national).

% Are required to undergo extensive, often unfunded, professional development to implement structured literacy methods, which may conflict with their prior training or preferred pedagogical styles. Their exit options are limited by district mandates and professional expectations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Bear the financial and logistical burden of retraining staff, purchasing new curricula, and adapting instructional practices across all grade levels. They face pressure from parent advocacy groups and state mandates but are constrained by budgets and existing infrastructure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts, payer,
    institutional, generational, constrained, regional).

% May experience a slower pace of instruction or less exposure to diverse texts compared to other methods, potentially impacting their engagement or advanced literacy development, though many also benefit from the systematic approach. They are trapped by the universal application of the method.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_without_diagnosed_disabilities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, students_without_diagnosed_disabilities, beneficiary).

% Are marginalized in policy debates and curriculum adoption processes as structured literacy gains dominance. Their pedagogical philosophy is actively suppressed by the ascendant paradigm, limiting their influence and professional opportunities.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent, evidence-based framework for teaching reading that ensures all essential components of literacy are addressed systematically, reducing instructional variability and improving outcomes, particularly for struggling readers.
% TRANSFER_FUNCTION: Transfers instructional authority and resources towards specialized, explicit, and sequential teaching methods, requiring significant investment in teacher training and curriculum materials, from general education budgets and teachers' time to specialized providers and students with specific needs.
% ABSENT_VOICES: Advocates for whole language or purely balanced literacy approaches are increasingly excluded from policy-making and curriculum selection, as their foundational premises are seen as incompatible with the scientific evidence supporting structured literacy. They would argue for broader pedagogical freedom and less prescriptive methods.
% DISAPPEARANCE_RATIONALE: If the mandate for structured literacy vanished, school districts would revert to more varied, less systematic approaches, teacher training would diversify, and students with dyslexia would likely experience a return to less effective, fragmented interventions, leading to a significant reorganization of literacy instruction and outcomes.
% FOUNDING_PROBLEM: A significant number of students, particularly those with dyslexia, failed to acquire foundational reading skills due to inconsistent, unsystematic, or insufficient instruction, leading to widespread literacy crises and academic underperformance.
% FOUNDING_PROBLEM_CORROBORATION: Parent advocacy groups, educational psychologists, and longitudinal studies consistently corroborate the ongoing problem of reading failure and the efficacy of structured literacy for at-risk students. While some educators contest the universal applicability, the core problem for dyslexic students remains widely acknowledged outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is driven by the high cost of specialized teacher training and curriculum adoption, disproportionately borne by general education teachers and school districts. Suppression (0.75) is high due to the active marginalization of alternative pedagogical approaches and the top-down mandates for implementation. Theater ratio (0.15) is low because the core instructional function is genuinely pursued, though some 'implementation' may be superficial. The increasing extractiveness and suppression over time reflect the hardening of the mandate and the growing costs of universal adoption.
 *
 * PERSPECTIVAL GAP:
 *   Students with learning disabilities and special education teachers experience this as a beneficial rope, providing necessary coordination and effective intervention. General education teachers and school districts, however, experience it as a snare or tangled rope, due to the imposed costs and loss of pedagogical autonomy. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with learning disabilities are full beneficiaries (d=0.0) as the constraint directly addresses their needs. Special education teachers and curriculum providers are also beneficiaries (d low) due to validation of expertise and market growth. General education teachers and school districts are targets (d high) due to the imposed training and financial burdens. Students without diagnosed disabilities are also targets (d high) as the universal application may not be optimal for their learning styles, even if some benefit. Whole language advocates are excluded, their d is effectively 1.0 as the constraint actively suppresses their approach.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (effective literacy instruction for all, especially struggling readers) is still live. The classification as a tangled rope prevents mislabeling it as pure extraction, acknowledging its genuine coordination function for a specific population, while also highlighting the asymmetric costs imposed by its universal application. It avoids mislabeling as a pure rope by recognizing the active suppression of alternatives and the significant extraction from general education stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_applicability_efficacy,
    'Is the universal application of structured literacy genuinely optimal for all students, or does it create suboptimal outcomes for some, particularly those without diagnosed learning disabilities?',
    'Longitudinal comparative studies of diverse student populations under structured literacy vs. alternative evidence-based approaches, measuring engagement, motivation, and advanced literacy skills.',
    'If suboptimal for some, the ''payer'' status of students without diagnosed disabilities would be amplified, potentially shifting the constraint closer to a snare for that seat. If universally optimal, their ''beneficiary'' aspect would be amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_efficacy, empirical, 'Efficacy of structured literacy for the general student population.').

omega_variable(
    teacher_training_cost_justification,
    'Are the costs of mandated teacher training and curriculum adoption proportional to the incremental benefits for the general student population, or are they an extractive burden driven by specialized interests?',
    'Independent cost-benefit analysis comparing training costs against measured improvements in literacy outcomes across diverse student groups, factoring in opportunity costs of alternative pedagogical investments.',
    'If costs are disproportionate, the extractiveness on general education teachers and school districts would be re-evaluated as higher, strengthening the ''snare'' aspect for those seats. If proportional, the ''tangled_rope'' classification would be reinforced, emphasizing the coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_training_cost_justification, empirical, 'Cost-effectiveness of structured literacy implementation for universal application.').

omega_variable(
    reading_acquisition_kernel_framing,
    'Is ''structured_literacy_reading'' a distinct reading of the literacy acquisition kernel, or is it a more prescriptive variant of the ''phonics_reading''?',
    'Conceptual analysis of the core axioms and pedagogical scope: if it integrates broader components (fluency, vocabulary, comprehension) in a distinct, cumulative sequence beyond mere phonics, it''s distinct. If it primarily emphasizes phonics with minor additions, it''s a variant.',
    'If a variant, its relationship to ''phonics_reading'' would be ''influences'' rather than ''coexists_with'', and its unique axioms would be less foundational. If distinct, the current ''coexists_with'' relationship and foundational axioms are appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_acquisition_kernel_framing, conceptual, 'Whether structured literacy constitutes a distinct reading or a variant of phonics-first approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', focusing on the structured literacy approach. It is linked to other readings (phonics, whole language, balanced literacy) as part of a constraint family where different pedagogical philosophies compete for dominance in educational policy and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
