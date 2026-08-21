% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__epistemological_boundary_reading, []).

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
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis: Epistemological Boundary of Knowable Computation
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint story models the Church-Turing Thesis as an
 *   epistemological boundary, defining what counts as 'formally knowable
 *   computation' within mathematics and computer science. It asserts that
 *   functions we can prove computable are precisely those that are
 *   Turing-computable, irrespective of whether non-Turing-computable
 *   processes might exist physically. This reading establishes a
 *   methodological exclusion, guiding formal proof and research, but also
 *   marginalizing alternative computational paradigms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.6).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis: Epistemological Boundary of Knowable Computation").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b').
narrative_ontology:cs_kernel_codification('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', formalized).
narrative_ontology:cs_authority_grounding('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', expertise).
narrative_ontology:cs_interpretation_layer_present('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b').
narrative_ontology:cs_reading_relation('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', foundational, computability_is_formal_proof_theoretic).
narrative_ontology:cs_axiom_status(computability_is_formal_proof_theoretic, holdable).
narrative_ontology:cs_axiom_grounding('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', computability_is_formal_proof_theoretic, conventional).
narrative_ontology:cs_axiom('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', foundational, turing_equivalence_defines_knowable_computation).
narrative_ontology:cs_axiom_status(turing_equivalence_defines_knowable_computation, holdable).
narrative_ontology:cs_axiom_grounding('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', turing_equivalence_defines_knowable_computation, conventional).
narrative_ontology:cs_reference_frame('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', formal_proof_theoretic_framework).
narrative_ontology:cs_drift_state('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', contemporary_hypercomputation_debates, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('1099d7a1-1ea5-4999-8e4d-fa6dfbe7905b', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_scientists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, proponents_of_hypercomputation).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, formally defined boundary for 'computable' functions, which aligns with their emphasis on explicit constructions and proofs. The thesis provides a stable framework for their work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    institutional, generational, constrained, global).

% Rely on the thesis as the foundational principle for algorithm design, complexity theory, and the limits of what can be automated. It provides a stable, universally accepted definition of computation for their practical and theoretical work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_scientists, beneficiary,
    institutional, generational, constrained, global).

% Their research into models of computation beyond Turing machines (e.g., oracle machines, infinite time Turing machines) is often marginalized or excluded from the mainstream definition of 'computability' as defined by this reading of the thesis. They bear the cost of being outside the established boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proponents_of_hypercomputation, payer,
    moderate, biographical, constrained, global).

% While their methods are valid in other areas of mathematics, their non-constructive proofs of existence for functions might be deemed outside the scope of 'formally knowable computation' if an explicit algorithm cannot be derived, thus bearing an epistemic cost.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematicians, payer,
    moderate, biographical, constrained, global).

% Analyze the philosophical implications of the thesis, its various interpretations, and its role in defining the limits of knowledge and formal systems. They are not directly subject to its enforcement but critically examine its boundaries.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_mathematics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, rigorous boundary for what constitutes 'formally knowable computation' within mathematics and computer science, ensuring consistency in proofs and theoretical frameworks.
% TRANSFER_FUNCTION: Transfers epistemic authority and research focus towards computational models that are provably Turing-equivalent, effectively excluding or marginalizing claims about computability that fall outside this boundary.
% ABSENT_VOICES: Researchers exploring computational models beyond Turing equivalence, whose work is often deemed outside the scope of 'computability' as defined by the thesis. They would argue for a broader, more inclusive definition of computation.
% DISAPPEARANCE_RATIONALE: If the Church-Turing Thesis as an epistemological boundary vanished, the fields of computability theory and theoretical computer science would lose their central organizing principle. There would be no universally accepted definition of 'computable function' for formal systems, leading to fragmentation, ambiguity, and a breakdown in shared understanding of what constitutes a valid computational proof or algorithm.
% FOUNDING_PROBLEM: To provide a rigorous, universally accepted definition of 'effective computability' for formal systems, resolving ambiguities and ensuring consistency in mathematical logic and early computer science, particularly in response to Hilbert's Entscheidungsproblem.
% FOUNDING_PROBLEM_CORROBORATION: The continued reliance on Turing machines and equivalent models as the standard in computer science curricula and research, and the ongoing philosophical debates about the thesis's scope and interpretation, corroborate the persistence of the underlying problem of defining computability, even if the thesis's interpretation is debated by various academic communities.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).
:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a crucial coordination function (a shared definition of computability) but also involves asymmetric extraction. Its extractiveness (0.35) is moderate, reflecting the cost borne by researchers whose work falls outside the defined boundary. Suppression (0.6) is significant, as the thesis is actively enforced through peer review, curriculum, and funding priorities, which can marginalize non-Turing-equivalent computational models. Theater ratio (0.1) is low, as the thesis remains a genuinely functional and foundational concept, not primarily performative. Accessibility collapse (0.8) is high because, within the formal framework, alternatives to Turing computability are largely considered outside the scope of 'computable'. Resistance (0.4) is moderate, stemming from ongoing philosophical debates and niche research into hypercomputation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of constructive mathematicians and computer scientists, the thesis is a foundational Rope, providing essential clarity and a stable framework. From the perspective of proponents of hypercomputation or non-constructive mathematicians, it operates more like a Snare, actively excluding their work from the mainstream definition of 'computability' and limiting the scope of what is considered valid research in the field.
 *
 * DIRECTIONALITY LOGIC:
 *   Constructive mathematicians and computer scientists are beneficiaries, as the thesis provides a stable, widely accepted framework for their work, reducing ambiguity. Proponents of hypercomputation and non-constructive mathematicians are victims, as their approaches are either excluded from or deemed less central to the definition of 'formally knowable computation'. The thesis's enforcement mechanisms (peer review, curriculum) direct resources and legitimacy towards Turing-equivalent models.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this constraint as a pure Rope (which would ignore the exclusionary costs) or a pure Snare (which would ignore its genuine coordination function in establishing a shared epistemic boundary). The ongoing debate about hypercomputation and the philosophical implications of the thesis indicate that its mandate is still live, though its interpretation and scope are contested, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''epistemological boundary'' reading of the Church-Turing Thesis, distinct from its mathematical definition or physical claim readings?',
    'Analysis of philosophical texts and research papers that explicitly frame the thesis in terms of formal knowability and proof-theoretic limits, rather than as a mere definition or an empirical statement about physics.',
    'If the distinction is not robust, this reading might collapse into one of the sibling readings, altering its beneficiary/victim structure and potentially its classification (e.g., a purely definitional reading would have lower extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing the epistemological reading from other interpretations of the Church-Turing Thesis.').

omega_variable(
    scope_of_formally_knowable,
    'What precisely constitutes ''formally knowable computation'' beyond Turing equivalence, and how would its inclusion alter the thesis''s boundary?',
    'Development of new formal systems or logical frameworks that rigorously define and prove the computability of functions beyond Turing machines, gaining widespread acceptance within the mathematical community.',
    'If such systems gain traction, the ''victim'' set of this constraint would shrink, and its extractiveness and suppression would decrease, potentially shifting its classification towards a Rope or even a Piton if the boundary becomes porous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_formally_knowable, empirical, 'Ambiguity regarding the precise limits of ''formally knowable'' computation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(chur_tr_t1975, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.25).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(chur_be_t1975, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(chur_su_t1975, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Church-Turing Thesis kernel. Each reading has a unique structural profile and classification, linked here to reflect their shared conceptual origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
