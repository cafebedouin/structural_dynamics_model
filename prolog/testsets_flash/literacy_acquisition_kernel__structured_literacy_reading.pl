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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Pedagogy
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the pedagogical approach known as 'Structured
 *   Literacy,' rooted in the Orton-Gillingham tradition, which posits that
 *   reading acquisition requires explicit, systematic, cumulative instruction
 *   across multiple foundational skills. While initially developed for
 *   students with dyslexia, its principles are increasingly advocated for
 *   universal application. This constraint is one reading of the broader
 *   'literacy_acquisition_kernel,' which is contested by other pedagogical
 *   philosophies like Whole Language and Balanced Literacy. The structured
 *   literacy reading emphasizes a scientific, evidence-based approach to
 *   reading instruction.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: Primary beneficiary (powerless/trapped) — gains reading skills
 *   - reading_specialists: Agenda setter (organized/mobile) — implements and advocates for the method
 *   - general_education_teachers: Payer (moderate/constrained) — bears training burden
 *   - school_districts_with_limited_budgets: Payer (institutional/constrained) — bears financial costs
 *   - structured_literacy_curriculum_providers: Beneficiary (powerful/arbitrage) — profits from adoption
 *   - advocates_for_whole_language: Excluded (moderate/constrained) — marginalized voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.7).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '70aecf4f-e7ca-4120-a2c5-c8eacdd8571a').
narrative_ontology:cs_kernel_codification('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', formalized).
narrative_ontology:cs_authority_grounding('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', expertise).
narrative_ontology:cs_interpretation_layer_present('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a').
narrative_ontology:cs_reading_relation('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', foundational, reading_is_unnatural_skill).
narrative_ontology:cs_axiom_status(reading_is_unnatural_skill, holdable).
narrative_ontology:cs_axiom_grounding('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', reading_is_unnatural_skill, empirically_contingent).
narrative_ontology:cs_axiom('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', foundational, explicit_systematic_instruction_is_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', explicit_systematic_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', orton_gillingham_principles).
narrative_ontology:cs_drift_state('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', contemporary_science_of_reading_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('70aecf4f-e7ca-4120-a2c5-c8eacdd8571a', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, reading_specialists).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_providers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts_with_limited_budgets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students are the primary beneficiaries, as structured literacy methods are specifically designed to address their learning needs, leading to improved reading outcomes and reduced academic struggle. Without this approach, their options for successful reading acquisition are severely limited.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, local).

% Professionals trained in Orton-Gillingham and other structured literacy approaches. They advocate for and implement these methods, often requiring specialized certification and commanding higher salaries. They benefit from the demand for their expertise and the efficacy of the methods for specific populations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_specialists, agenda_setter,
    organized, generational, mobile, national).

% Face increased training burdens and professional development costs to implement structured literacy, especially if their initial training was in other methodologies. They are often required to adopt these methods by district policy, with limited resources or time for retraining.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Bear the financial cost of new curriculum materials, specialized teacher training, and potentially hiring more reading specialists. They are constrained by state mandates and parental pressure to adopt effective reading instruction, but face budget limitations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts_with_limited_budgets, payer,
    institutional, generational, constrained, regional).

% Benefit from the availability of effective interventions for their children, often advocating strongly for structured literacy approaches. They may also bear direct costs for private tutoring if school resources are insufficient.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Develop and sell curriculum materials, training programs, and diagnostic tools aligned with structured literacy principles. They profit directly from the adoption of these methods by schools and districts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_providers, beneficiary,
    powerful, generational, arbitrage, national).

% Academics and educators who believe reading is a natural process best learned through immersion in meaningful texts, with minimal explicit phonics. Their pedagogical approach is increasingly marginalized by the 'Science of Reading' movement, which heavily favors structured literacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, advocates_for_whole_language, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a comprehensive, evidence-based approach to reading instruction, ensuring that all foundational literacy skills are taught explicitly and systematically, particularly for students who do not acquire reading naturally.
% TRANSFER_FUNCTION: Transfers significant training and curriculum costs to general education teachers and school districts, while transferring effective pedagogical methods and improved reading outcomes to students, especially those with learning disabilities. It also transfers revenue to specialized curriculum providers and reading specialists.
% ABSENT_VOICES: Advocates for whole language or balanced literacy approaches are increasingly excluded from policy discussions and curriculum adoption processes, despite their historical influence. They would argue for a more holistic, less prescriptive approach to reading instruction, emphasizing meaning-making over decoding drills.
% DISAPPEARANCE_RATIONALE: If structured literacy vanished, students with dyslexia and other reading difficulties would likely revert to struggling with less effective, less systematic methods. The entire ecosystem of specialized training, curriculum development, and intervention services would collapse, forcing a reorganization of how reading is taught and how learning disabilities are addressed.
% FOUNDING_PROBLEM: The persistent failure of many students, particularly those with dyslexia, to acquire reading skills through traditional or less systematic instructional methods, leading to significant academic and life disadvantages.
% FOUNDING_PROBLEM_CORROBORATION: The problem of reading failure, especially for dyslexic students, remains a live issue, corroborated by ongoing educational research, literacy statistics, and the lived experience of countless families and educators. The efficacy of structured literacy for these populations is widely supported by scientific evidence from cognitive psychology and neuroscience, attested by independent researchers and medical professionals.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).

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
 *   The extractiveness (0.65) is primarily due to the high cost of specialized training and curriculum materials, which are often mandatory for teachers and districts. Suppression (0.70) reflects the strong pressure on educators to adopt this method, often marginalizing alternative pedagogies and limiting teacher autonomy. The theater ratio (0.10) is low, indicating that the constraint is genuinely functional, delivering on its promise of improved reading outcomes for its target population, rather than being performative. The rising extractiveness and suppression over time reflect the increasing institutionalization and mandating of structured literacy, moving from a specialized intervention to a broader, more enforced pedagogical standard.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of students with dyslexia and their parents, this constraint is a clear Rope or even a Mountain, providing essential support and a path to literacy. For general education teachers and school districts, it can feel like a Snare or Tangled Rope, imposing significant costs and training requirements without always providing adequate resources or acknowledging the complexity of classroom implementation. Reading specialists and curriculum providers experience it as a clear Rope or even a subsidy, benefiting from the demand for their expertise and products.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia are full beneficiaries (d=0.0) as the method directly addresses their needs. Reading specialists and curriculum providers are also beneficiaries (d=0.1-0.2) due to increased demand for their services and products. General education teachers and school districts are targets (d=0.7-0.8) due to the imposed costs and training burdens. Advocates for whole language are excluded (d=1.0) as their approach is actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (effective reading instruction for struggling learners) is still live, and its efficacy is well-supported for its target population. The risk of mandatrophy is low, but there is a risk of mission creep where the 'universal applicability' claim could lead to over-prescription and unnecessary extraction from populations who might thrive with less intensive methods, or where the costs outweigh the marginal benefits for typical learners. The classification as Tangled Rope reflects the genuine coordination function (effective pedagogy) intertwined with asymmetric extraction (training costs, curriculum sales).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_literacy_vs_phonics_variant,
    'Is ''Structured Literacy'' a distinct reading of the literacy acquisition kernel, or merely a more comprehensive variant of the ''Phonics Reading''?',
    'Analysis of core axiomatic differences and instructional scope: if Structured Literacy''s emphasis on phonological awareness, fluency, vocabulary, and comprehension (beyond just phonics) constitutes a distinct foundational claim, it''s a separate reading. If its core distinguishing feature is still primarily phonics, it''s a variant.',
    'If a variant, its distinctiveness and influence on other readings might be overstated, potentially collapsing its unique ''extractiveness'' profile into that of the broader phonics movement. If distinct, its unique contributions and costs are properly recognized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_literacy_vs_phonics_variant, conceptual, 'Whether Structured Literacy is a distinct pedagogical paradigm or a subset of phonics-first approaches.').

omega_variable(
    universal_applicability_efficacy,
    'Is the universal applicability of structured literacy, beyond students with dyslexia, empirically justified, or does it impose unnecessary costs on typical learners?',
    'Large-scale, longitudinal comparative studies evaluating the efficacy and cost-effectiveness of structured literacy versus other methods for diverse student populations, including those without identified reading difficulties.',
    'If not universally justified, the ''payer'' burden on general education teachers and school districts would be reclassified as higher extraction for a subset of students, and the constraint''s scope might need to be narrowed to specific populations. If justified, the current cost structure is more defensible as a universal coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_applicability_efficacy, empirical, 'Empirical justification for universal application of structured literacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1930, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(lite_tr_t1960, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t1930, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1930, 0.3).
narrative_ontology:measurement(lite_be_t1960, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1930, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1930, 0.2).
narrative_ontology:measurement(lite_su_t1960, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', which encompasses different pedagogical philosophies on how reading is acquired. Each reading represents a distinct constraint with its own structural properties and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
