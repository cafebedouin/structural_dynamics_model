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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Structured Literacy Remediation Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'structured_literacy_remediation' reading
 *   of the 'reading_acquisition_legitimacy' kernel. It asserts that
 *   legitimate reading instruction must prioritize the needs of the most
 *   vulnerable learners, adhering to explicit, cumulative, and diagnostic
 *   principles derived from structured literacy. This approach emphasizes
 *   preventative, intervention-grade instruction for all students, involving
 *   high structure, multisensory explicit teaching, and continuous diagnostic
 *   assessment. The constraint is claimed as a Tangled Rope, reflecting its
 *   genuine coordination function (effective literacy for all) alongside
 *   significant extraction and suppression from implementers.
 *
 * KEY AGENTS:
 *   - structured_literacy_advocates: Primary agenda setter (institutional/analytical) — drives policy and pedagogical mandates.
 *   - vulnerable_learners / students_with_dyslexia: Primary beneficiaries (powerless/trapped) — receive targeted, effective instruction.
 *   - teachers_trained_in_other_methods / school_districts_with_limited_resources: Primary payers (moderate/institutional) — bear the costs of retraining, curriculum changes, and resource allocation.
 *   - publishers_of_whole_language_materials: Excluded (powerful/constrained) — their products and philosophies are actively suppressed.
 *   - cognitive_scientists: Analytical observer (analytical/universal) — provide the foundational evidence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.75).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '9b546f06-2e14-4962-acc1-dc0a9e6b8512').
narrative_ontology:cs_kernel_codification('9b546f06-2e14-4962-acc1-dc0a9e6b8512', formalized).
narrative_ontology:cs_authority_grounding('9b546f06-2e14-4962-acc1-dc0a9e6b8512', expertise).
narrative_ontology:cs_interpretation_layer_present('9b546f06-2e14-4962-acc1-dc0a9e6b8512').
narrative_ontology:cs_reading_relation('9b546f06-2e14-4962-acc1-dc0a9e6b8512', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('9b546f06-2e14-4962-acc1-dc0a9e6b8512', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('9b546f06-2e14-4962-acc1-dc0a9e6b8512', reading_acquisition_legitimacy__balanced_literacy_integration, forecloses).
narrative_ontology:cs_axiom('9b546f06-2e14-4962-acc1-dc0a9e6b8512', foundational, explicit_systematic_diagnostic_instruction_is_foundational).
narrative_ontology:cs_axiom_status(explicit_systematic_diagnostic_instruction_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('9b546f06-2e14-4962-acc1-dc0a9e6b8512', explicit_systematic_diagnostic_instruction_is_foundational, empirically_contingent).
narrative_ontology:cs_axiom('9b546f06-2e14-4962-acc1-dc0a9e6b8512', foundational, instruction_design_prioritizes_most_vulnerable_learners).
narrative_ontology:cs_axiom_status(instruction_design_prioritizes_most_vulnerable_learners, holdable).
narrative_ontology:cs_axiom_grounding('9b546f06-2e14-4962-acc1-dc0a9e6b8512', instruction_design_prioritizes_most_vulnerable_learners, deontological).
narrative_ontology:cs_reference_frame('9b546f06-2e14-4962-acc1-dc0a9e6b8512', evidence_based_literacy_science).
narrative_ontology:cs_drift_state('9b546f06-2e14-4962-acc1-dc0a9e6b8512', contemporary_policy_implementation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9b546f06-2e14-4962-acc1-dc0a9e6b8512', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_limited_resources).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, publishers_of_whole_language_materials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and enforce the adoption of structured literacy principles, advocating for policy changes and teacher training. They gain legitimacy and influence from the widespread adoption of their advocated methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Directly benefit from explicit, systematic, and diagnostic instruction designed to address their specific learning needs, providing a pathway to literacy that other methods often fail to deliver.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners, beneficiary,
    powerless, biographical, trapped, local).

% This instructional approach is specifically designed to address the neurological profile of dyslexia, offering a critical and often life-changing pathway to reading proficiency.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, local).

% Advocate for and see their children benefit from effective, evidence-based reading instruction, often after experiencing frustration with less effective methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Must undergo extensive retraining, adapt their established pedagogy, and potentially abandon long-held beliefs or practices about reading instruction, often with significant personal and professional cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% Bear the substantial financial and logistical burden of implementing new curricula, providing professional development, and acquiring specialized materials, often without commensurate increases in funding.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_limited_resources, payer,
    institutional, biographical, constrained, regional).

% Their educational products and pedagogical philosophies are de-emphasized or rejected by policy mandates, leading to loss of market share, influence, and a need to retool their offerings.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, publishers_of_whole_language_materials, excluded,
    powerful, biographical, constrained, national).

% Implement and fund policies that mandate or incentivize structured literacy, responding to research, advocacy, and public pressure to improve literacy outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, education_policymakers, agenda_setter,
    institutional, generational, mobile, national).

% Provide the foundational research on reading acquisition and effective instruction that underpins structured literacy principles, observing the policy and pedagogical landscape.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure all students, especially the most vulnerable, acquire foundational reading skills through evidence-based, systematic, explicit, and diagnostic instruction, thereby preventing widespread literacy failure and its associated societal costs.
% TRANSFER_FUNCTION: Transfers pedagogical authority and instructional design from individual teacher discretion or less structured methods to a standardized, explicit, diagnostic, and cumulative approach. It also transfers significant resources (funding, training, materials) towards structured literacy programs and away from other instructional approaches.
% ABSENT_VOICES: Teachers who prioritize holistic, child-centered learning over explicit phonics, or those who feel their professional autonomy is being eroded by prescriptive mandates. Publishers of non-structured literacy materials are also excluded from the policy conversation.
% DISAPPEARANCE_RATIONALE: If the mandate for structured literacy remediation vanished, many schools and districts would likely revert to less intensive or eclectic methods, potentially leading to a resurgence of literacy struggles for vulnerable learners and a fragmentation of instructional quality. The educational landscape would reorganize around diverse, less regulated pedagogical approaches.
% FOUNDING_PROBLEM: Persistent and widespread reading failure, particularly among socioeconomically disadvantaged students and those with learning disabilities (e.g., dyslexia), despite decades of varied and often ineffective instructional approaches.
% FOUNDING_PROBLEM_CORROBORATION: Literacy researchers, parent advocacy groups (e.g., Decoding Dyslexia), and national reading panels consistently highlight the ongoing crisis of reading proficiency and the robust evidence base for structured literacy's efficacy in addressing it. This corroboration comes from outside the direct beneficiaries of the policy's implementation.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The high extractiveness (0.65) reflects the substantial demands placed on teachers for retraining and fidelity, and on school districts for resource allocation to implement intensive, evidence-based programs. Suppression (0.75) is high due to the active de-emphasis and exclusion of alternative pedagogical approaches (e.g., whole language, less structured balanced literacy) through policy mandates and curriculum adoption processes. Theater ratio (0.30) is relatively low because structured literacy emphasizes explicit, diagnostic, and measurable outcomes, making superficial compliance harder to sustain without detection. However, some performative adoption may occur in resistant contexts. Accessibility collapse (0.70) is high as the mandate effectively closes off less structured instructional paths for both teachers and students. Resistance (0.70) is significant, stemming from educators' prior training, concerns about autonomy, and resource limitations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of structured literacy advocates and parents of struggling readers, this constraint is a necessary and beneficial coordination mechanism, ensuring equitable access to effective literacy instruction. From the perspective of teachers trained in other methods and resource-constrained school districts, it is an extractive mandate that imposes significant costs, limits professional autonomy, and suppresses alternative pedagogical philosophies. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Structured literacy advocates are beneficiaries as their expertise and methods are validated and adopted, increasing their influence. Vulnerable learners and students with dyslexia are clear beneficiaries, receiving instruction tailored to their needs. Teachers trained in other methods and school districts with limited resources are payers, bearing the costs of compliance and adaptation. Publishers of alternative materials are excluded, as their market is actively suppressed. Cognitive scientists serve as analytical observers, providing the evidence base without direct benefit or cost from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of widespread reading failure is widely attested as 'live,' indicating that the constraint is not mandatrophic. Its persistence is justified by an ongoing, critical societal need. However, the *intensity* of the extraction and suppression is contested, with some arguing it exceeds what is necessary for coordination and veers into rent-seeking (e.g., from specific curriculum providers or training organizations). The classification as Tangled Rope reflects this ongoing tension between genuine coordination and asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_effectiveness_vs_implementation_burden,
    'Is the high cost and implementation burden of structured literacy remediation justified by the measured improvements in literacy outcomes for all vulnerable learners?',
    'Longitudinal studies comparing literacy outcomes and societal costs (e.g., special education, incarceration rates) in jurisdictions with high-fidelity structured literacy implementation versus those with alternative approaches.',
    'If the benefits significantly outweigh the costs, the extractiveness might be re-evaluated as a necessary coordination cost. If costs are disproportionate to outcomes, the constraint''s extractive component would be further highlighted, potentially leading to calls for funding reform or alternative implementation models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_effectiveness_vs_implementation_burden, empirical, 'Assessing the balance between implementation costs and literacy outcome benefits.').

omega_variable(
    teacher_autonomy_vs_instructional_fidelity,
    'To what extent does the mandate for structured literacy suppress valuable teacher autonomy and pedagogical innovation, and is this suppression a necessary cost for instructional fidelity?',
    'Qualitative studies on teacher morale, professional identity, and observed classroom innovation under structured literacy mandates, alongside quantitative analysis of fidelity-to-outcome correlations.',
    'If suppression is found to stifle effective innovation without a clear fidelity benefit, the constraint''s suppression metric might be seen as an unnecessary extraction of professional agency. If fidelity is paramount for outcomes, the suppression would be viewed as a justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_instructional_fidelity, conceptual, 'Balancing teacher autonomy with the need for high-fidelity instructional methods.').

omega_variable(
    scope_of_vulnerable_learners,
    'How broadly is ''vulnerable learners'' defined in practice, and does the ''intervention-grade instruction for all'' mandate appropriately serve the full spectrum of learners, or does it over-prescribe for some?',
    'Analysis of student outcome data across different learner profiles (e.g., those with mild vs. severe learning challenges, English language learners) under structured literacy, compared to outcomes under differentiated instruction.',
    'If the mandate is found to be overly rigid for some learners, the extractiveness and suppression might be re-evaluated as unnecessary for those groups, suggesting a more nuanced application of the constraint. If it proves universally beneficial, the current scope is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_vulnerable_learners, empirical, 'Appropriateness of universal intervention-grade instruction for diverse learner needs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 4, 0.27).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 8, 0.28).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 12, 0.29).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 16, 0.3).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_training_curriculum_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, educational_materials_procurement_policies).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel, focusing on structured literacy for remediation. It is structurally distinct from other readings (phonics-decoding primacy, whole-language meaning primacy, balanced literacy integration) due to its specific emphasis on vulnerable learners and explicit, diagnostic, cumulative principles, leading to different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
