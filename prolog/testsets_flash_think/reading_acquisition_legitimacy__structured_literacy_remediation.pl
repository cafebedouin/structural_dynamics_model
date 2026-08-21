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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Structured Literacy Remediation for Vulnerable Learners
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint asserts that reading instruction must prioritize the most
 *   vulnerable learners and adhere to explicit, cumulative, diagnostic
 *   principles derived from structured literacy. It represents a specific
 *   reading of the broader 'reading_acquisition_legitimacy' kernel,
 *   emphasizing preventative, intervention-grade instruction for all. The
 *   claimed type is 'rope' by its proponents, who see it as a necessary
 *   coordination mechanism for effective literacy. However, the authored
 *   metrics reflect the substantial costs and resistance involved in its
 *   implementation, suggesting a higher effective extraction for many
 *   stakeholders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.7).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation for Vulnerable Learners").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '42c74ba8-7dcc-4205-9a3c-9c154ac0fff6').
narrative_ontology:cs_kernel_codification('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', formalized).
narrative_ontology:cs_authority_grounding('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', expertise).
narrative_ontology:cs_interpretation_layer_present('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6').
narrative_ontology:cs_reading_relation('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', reading_acquisition_legitimacy__balanced_literacy_integration, forecloses).
narrative_ontology:cs_axiom('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', foundational, instruction_prioritizes_vulnerable_learners).
narrative_ontology:cs_axiom_status(instruction_prioritizes_vulnerable_learners, holdable).
narrative_ontology:cs_axiom_grounding('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', instruction_prioritizes_vulnerable_learners, deontological).
narrative_ontology:cs_axiom('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', foundational, reading_is_explicitly_teachable_skill).
narrative_ontology:cs_axiom_status(reading_is_explicitly_teachable_skill, holdable).
narrative_ontology:cs_axiom_grounding('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', reading_is_explicitly_teachable_skill, empirically_contingent).
narrative_ontology:cs_reference_frame('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', evidence_based_literacy_science).
narrative_ontology:cs_drift_state('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', contemporary_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('42c74ba8-7dcc-4205-9a3c-9c154ac0fff6', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_publishers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_publishers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_limited_budgets).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, science_of_reading_efficacy).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, early_intervention_criticality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the primary beneficiaries, as the instruction is explicitly designed to address their needs and prevent reading failure. Their 'exit' from struggling is dependent on effective instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners, beneficiary,
    powerless, immediate, trapped, local).

% Researchers, parent groups, and professional organizations who champion structured literacy, pushing for policy adoption and implementation. They set the agenda for pedagogical reform.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Companies that produce and sell curriculum, materials, and training programs aligned with structured literacy principles. They benefit financially from widespread adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Educators who must retrain, adapt their practice, and potentially discard years of experience with other pedagogical approaches. They bear the cost of professional development and change management.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods, payer,
    moderate, biographical, constrained, local).

% Companies whose existing curriculum and materials are based on whole language or balanced literacy approaches. They face declining market share and pressure to redevelop products, incurring significant costs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_publishers, payer,
    powerful, biographical, constrained, global).

% Local educational authorities that must allocate significant funds for new curriculum, diagnostic tools, and extensive teacher training, often without commensurate increases in state or federal funding.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_limited_budgets, payer,
    institutional, biographical, constrained, regional).

% Legislators and state education department officials who mandate the adoption of structured literacy, often responding to advocacy groups and research, but also balancing budget constraints and political pressures.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, education_policymakers, agenda_setter,
    institutional, generational, mobile, national).

% Researchers whose work on reading acquisition and brain function provides the scientific foundation for structured literacy. They observe and contribute to the evidence base, but do not directly implement or enforce policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure all learners, especially those most at risk, acquire foundational reading skills through a consistent, evidence-based, and effective instructional approach, preventing long-term literacy deficits and standardizing pedagogical practice.
% TRANSFER_FUNCTION: Transfers resources (funding, teacher training, curriculum development) and pedagogical authority from less structured or unproven methods to explicit, diagnostic, and cumulative structured literacy approaches, and ultimately transfers literacy skills to vulnerable learners.
% ABSENT_VOICES: Educators and researchers who advocate for whole language or balanced literacy as sufficient, or who prioritize broader literary engagement over explicit skill instruction, are often marginalized in policy discussions driven by structured literacy mandates, arguing for different pedagogical values.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, many vulnerable learners would continue to struggle, and educational systems would revert to less effective, fragmented, or ideologically driven instructional practices, leading to persistent literacy gaps and long-term societal costs. The entire ecosystem of literacy instruction would reorganize.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among disadvantaged students and those with dyslexia, and a perceived lack of evidence-based, systematic instruction in many schools leading to inconsistent outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Longitudinal studies on reading outcomes, cognitive science research on reading acquisition, and advocacy groups for dyslexic learners consistently corroborate the problem of reading failure and the need for effective instruction, from outside the direct beneficiaries of structured literacy.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.70) is high because implementing structured literacy requires significant investment in new curricula, diagnostic tools, and extensive teacher training, displacing existing, often cheaper, methods. Suppression (0.65) is driven by the need to overcome entrenched pedagogical beliefs, institutional inertia, and budgetary limitations. Resistance (0.70) is high from educators trained in other methods and publishers of non-structured literacy materials. Theater ratio is low (0.15) as the approach is highly functional and outcome-oriented. Accessibility collapse (0.40) is moderate; while alternatives exist, this reading argues they are insufficient, collapsing their legitimacy for vulnerable learners.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable learners and structured literacy advocates, this constraint is a vital coordination mechanism ensuring equitable access to effective reading instruction. From the perspective of teachers needing retraining or school districts facing budget constraints, it represents a significant, enforced cost and a loss of pedagogical autonomy. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable learners are clear beneficiaries (d=0.0), as the constraint directly serves their needs. Structured literacy advocates and publishers also benefit (d near 0.1-0.2) from the adoption and market expansion. Teachers trained in other methods, whole language publishers, and school districts with limited budgets are targets (d near 0.8-0.9) due to the costs of retraining, market displacement, and resource reallocation. Education policymakers and cognitive scientists act as agenda-setters and observers, respectively, with more symmetric or analytical directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a distinct reading of the ''reading_acquisition_legitimacy'' kernel, or is it merely an elaboration of the ''phonics_decoding_primacy'' reading?',
    'Analysis of core tenets: if ''structured_literacy_remediation'' introduces fundamentally new axioms (e.g., multisensory, diagnostic, cumulative principles beyond just phonics) that are not present in ''phonics_decoding_primacy'', it is distinct.',
    'If distinct, its unique contribution to literacy policy is affirmed. If an elaboration, its independent classification might be subsumed under the broader phonics-first approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Distinguishing this reading from its closest sibling.').

omega_variable(
    cost_benefit_all_learners,
    'Is the ''intervention-grade instruction preventatively for all students'' approach cost-effective and beneficial for *all* learners, including those without identified vulnerabilities?',
    'Longitudinal studies comparing outcomes and resource allocation for diverse student populations under universal structured literacy vs. differentiated instruction models.',
    'If not universally beneficial or cost-effective, the constraint''s extractiveness for non-vulnerable learners and school systems might be higher than currently assessed, potentially shifting its classification for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_all_learners, empirical, 'Assessing the universal applicability and cost-effectiveness of intervention-grade instruction.').

omega_variable(
    teacher_resistance_mechanism,
    'Is the resistance from teachers trained in other methods primarily structural (lack of resources, time for training) or internalized (deeply held pedagogical beliefs, identity fusion with prior methods)?',
    'Qualitative studies and surveys exploring teacher attitudes and perceived barriers, coupled with analysis of implementation success rates in contexts with high vs. low structural support.',
    'If resistance is largely internalized, the effective suppression required to enforce the constraint is higher and more persistent, even with structural support, indicating a deeper challenge to pedagogical change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_resistance_mechanism, empirical, 'Structural vs. internalized resistance from educators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.15).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.15).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.15).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
