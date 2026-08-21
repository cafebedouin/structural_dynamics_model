% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__mathematical_definition_reading, []).

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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis (Mathematical Definition Reading)
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as a mathematical
 *   definition, stipulating what is meant by 'effective computability'. It is
 *   a convention, widely accepted for its utility in formal systems, rather
 *   than an empirically testable claim about the physical world. Its primary
 *   function is to coordinate terminology and provide a stable conceptual
 *   foundation for theoretical work in mathematics, logic, and computer
 *   science.
 *
 * KEY AGENTS:
 *   - Mathematicians: Primary beneficiaries, use the definition for rigor.
 *   - Computer Scientists: Primary beneficiaries, use the definition for theoretical foundations.
 *   - Logicians: Primary beneficiaries, use the definition for formal systems.
 *   - Philosophers of Computation: Analytical observers, analyze implications.
 *   - General Public: Excluded, not directly involved in the technical definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.1).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis (Mathematical Definition Reading)").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '65be1b70-5325-476d-ae7c-fcc0824ad6ac').
narrative_ontology:cs_kernel_codification('65be1b70-5325-476d-ae7c-fcc0824ad6ac', formalized).
narrative_ontology:cs_authority_grounding('65be1b70-5325-476d-ae7c-fcc0824ad6ac', expertise).
narrative_ontology:cs_reading_relation('65be1b70-5325-476d-ae7c-fcc0824ad6ac', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('65be1b70-5325-476d-ae7c-fcc0824ad6ac', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('65be1b70-5325-476d-ae7c-fcc0824ad6ac', foundational, computability_is_formally_definable).
narrative_ontology:cs_axiom_status(computability_is_formally_definable, holdable).
narrative_ontology:cs_axiom_grounding('65be1b70-5325-476d-ae7c-fcc0824ad6ac', computability_is_formally_definable, conventional).
narrative_ontology:cs_reference_frame('65be1b70-5325-476d-ae7c-fcc0824ad6ac', formal_mathematical_rigor).
narrative_ontology:cs_drift_state('65be1b70-5325-476d-ae7c-fcc0824ad6ac', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('65be1b70-5325-476d-ae7c-fcc0824ad6ac', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, logicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, universally accepted definition of 'effective computability' that underpins formal proofs and theoretical work in computability theory and related fields. Deviation from this definition would lead to conceptual ambiguity.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematicians, beneficiary,
    organized, generational, mobile, global).

% Utilize the definition as a foundational concept for understanding the limits and capabilities of algorithms and computational models. It provides a stable bedrock for theoretical computer science.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_scientists, beneficiary,
    organized, generational, mobile, global).

% Rely on the precise definition for constructing and analyzing formal systems, particularly in areas like proof theory and model theory where the concept of computability is central.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, logicians, beneficiary,
    organized, generational, mobile, global).

% Analyze the implications and interpretations of the Church-Turing Thesis, including its status as a definition versus an empirical claim. They observe its use and impact without directly benefiting or being harmed by its operation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, global).

% Not directly involved in the formal definition or its application. The technical nature of the constraint means they are largely unaware of its existence or implications, and thus excluded from the discourse.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, general_public, excluded,
    powerless, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the meaning of 'effective computability' across mathematics, logic, and computer science, enabling clear communication and rigorous formal reasoning about what can and cannot be computed by an algorithm.
% TRANSFER_FUNCTION: Transfers conceptual clarity, rigor, and a common theoretical foundation to the fields that rely on the definition, facilitating shared understanding and progress.
% ABSENT_VOICES: Those who might propose alternative, non-Turing-equivalent definitions of 'computability' for different conceptual purposes. While such proposals are possible, they would define a different concept, not challenge the utility of this specific definition for its intended domain.
% DISAPPEARANCE_RATIONALE: If this definition vanished overnight, the fields of computability theory, theoretical computer science, and mathematical logic would lose a fundamental, universally accepted bedrock. Formal proofs and discussions about 'computable functions' would become ambiguous, leading to widespread conceptual confusion and hindering research.
% FOUNDING_PROBLEM: The need for a precise, formal, and universally accepted definition of 'computable function' to enable rigorous mathematical and logical study of computation, moving beyond intuitive notions.
% FOUNDING_PROBLEM_CORROBORATION: The widespread adoption and continued use of the Church-Turing Thesis as a definition in textbooks, research papers, and university curricula across mathematics, computer science, and logic, attested by educators and researchers globally.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).
:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits very low extractiveness (0.05) because it primarily serves a coordination function, providing conceptual clarity without imposing significant costs or extracting rents. Suppression is low (0.1) as adherence is driven by utility and consensus rather than coercion; deviation would lead to conceptual isolation rather than active enforcement. Theater ratio is negligible (0.05) as its function is purely definitional and widely accepted. Accessibility collapse is high (0.85) because, within its domain, this definition is the standard, making alternative definitions for the same concept largely inaccessible or irrelevant. Resistance is low (0.05) due to its widespread acceptance and utility.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians, computer scientists, and logicians are clear beneficiaries (low directionality) as the definition provides essential conceptual tools for their work. There are no identifiable victims, as definitions, by their nature, cannot be 'violated' or extract from parties in a coercive sense. Philosophers of computation act as observers, analyzing its status and implications. The general public is excluded due to the highly technical nature of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_as_definition_vs_claim,
    'Is the Church-Turing Thesis fundamentally a mathematical definition, an empirical claim about the physical world, or an epistemological boundary?',
    'Philosophical analysis of its role in scientific practice, the nature of its justification, and the implications of its potential falsification (if treated as empirical).',
    'If resolved as an empirical claim (physical_claim_reading), its extractiveness and suppression might be re-evaluated based on physical evidence and the costs of maintaining a potentially false belief. If resolved as an epistemological boundary, its implications for the limits of knowledge would become central. As a definition, its status remains stable and non-extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_as_definition_vs_claim, conceptual, 'Ambiguity regarding the fundamental nature of the Church-Turing Thesis.').

omega_variable(
    scope_of_computability_concept,
    'Does the definition of ''effective computability'' fully capture all intuitive notions of computation, or are there aspects of ''computation'' that fall outside this formal definition?',
    'Development of new computational paradigms (e.g., hypercomputation, quantum computation) and philosophical arguments about whether these extend or merely redefine ''computability''.',
    'If new paradigms are widely accepted as ''computation'' in a sense not captured by Turing-equivalence, the scope and utility of this definition might narrow, potentially leading to the emergence of new, linked definitions. This would not invalidate the current definition but contextualize its domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_computability_concept, conceptual, 'Whether the formal definition fully encompasses the intuitive concept of computation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.05).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.1).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Church-Turing Thesis kernel. Its status as a mathematical definition influences, but does not foreclose, the empirical and epistemological interpretations, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
