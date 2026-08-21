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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis (Mathematical Definition Reading)
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as a purely
 *   mathematical definition, stipulating what 'effective computability' means
 *   within formal systems. It is a convention, not an empirically testable
 *   claim about the physical world. This reading is one of several
 *   interpretations of the thesis, focusing on its role in providing
 *   definitional clarity for theoretical computer science and mathematics.
 *   The low extractiveness reflects its status as a widely accepted,
 *   non-coercive definitional tool.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis (Mathematical Definition Reading)").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '53db6328-0a14-4a4b-a801-6e77047353d4').
narrative_ontology:cs_kernel_codification('53db6328-0a14-4a4b-a801-6e77047353d4', formalized).
narrative_ontology:cs_authority_grounding('53db6328-0a14-4a4b-a801-6e77047353d4', expertise).
narrative_ontology:cs_interpretation_layer_present('53db6328-0a14-4a4b-a801-6e77047353d4').
narrative_ontology:cs_reading_relation('53db6328-0a14-4a4b-a801-6e77047353d4', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('53db6328-0a14-4a4b-a801-6e77047353d4', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('53db6328-0a14-4a4b-a801-6e77047353d4', foundational, computability_is_formally_definable).
narrative_ontology:cs_axiom_status(computability_is_formally_definable, holdable).
narrative_ontology:cs_axiom_grounding('53db6328-0a14-4a4b-a801-6e77047353d4', computability_is_formally_definable, conventional).
narrative_ontology:cs_axiom('53db6328-0a14-4a4b-a801-6e77047353d4', foundational, turing_machine_is_universal_model).
narrative_ontology:cs_axiom_status(turing_machine_is_universal_model, holdable).
narrative_ontology:cs_axiom_grounding('53db6328-0a14-4a4b-a801-6e77047353d4', turing_machine_is_universal_model, conventional).
narrative_ontology:cs_reference_frame('53db6328-0a14-4a4b-a801-6e77047353d4', formal_mathematical_consensus).
narrative_ontology:cs_drift_state('53db6328-0a14-4a4b-a801-6e77047353d4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('53db6328-0a14-4a4b-a801-6e77047353d4', '').
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

% Benefit from a clear, universally accepted definition of computability that underpins their theoretical work. They use the thesis as a foundational convention for discussing algorithms and functions.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematicians, beneficiary,
    institutional, generational, mobile, universal).

% Utilize the thesis as the bedrock for algorithm design and complexity theory, providing a stable conceptual framework for what can and cannot be computed by machines. Their work relies on this definitional clarity.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_scientists, beneficiary,
    institutional, generational, mobile, universal).

% Are the primary architects and custodians of the formal systems that define computability. They benefit from the consensus around the thesis as a successful formalization of an intuitive concept.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, logicians, beneficiary,
    institutional, generational, mobile, universal).

% Analyze the implications and interpretations of the Church-Turing Thesis, including its status as a definition versus an empirical claim. They are not bound by its definitional force but study its role in other disciplines.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally accepted, precise mathematical definition for the intuitive concept of 'effective computability,' allowing mathematicians and computer scientists to communicate unambiguously about algorithms and functions.
% TRANSFER_FUNCTION: No direct transfer of resources. It transfers conceptual clarity and definitional stability to the fields of mathematics and computer science, reducing ambiguity and facilitating shared understanding.
% ABSENT_VOICES: No voices are structurally absent from this reading, as it functions as a convention. Any 'objection' would be a proposal for a different definition, not a challenge to its current status as a definition.
% DISAPPEARANCE_RATIONALE: If the Church-Turing Thesis as a mathematical definition vanished, the foundational language of computability theory would collapse into ambiguity. Every discussion of 'computable function' would require re-establishing a definition, severely hindering research and communication in mathematics and computer science.
% FOUNDING_PROBLEM: The need for a rigorous, formal definition of 'effective calculability' or 'computability' to replace vague intuitive notions in mathematics and logic during the early 20th century.
% FOUNDING_PROBLEM_CORROBORATION: The problem of defining computability rigorously is considered solved by the mathematical community, and the definition remains foundational. Logicians and computer scientists universally corroborate its continued relevance and utility as a definitional bedrock.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.02, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is very low (0.02) because a definition, by its nature, does not extract resources or impose costs beyond the initial cognitive effort of understanding it. Suppression is minimal (0.05) as adherence is driven by utility and consensus, not coercion. Theater ratio is negligible (0.01) as its function is purely definitional and conceptual. Accessibility collapse is high (0.95) because once the definition is accepted, alternatives for 'effective computability' are largely foreclosed within the mathematical discourse. Resistance is near zero (0.01) because its utility is widely recognized.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap for this reading, as its status as a definition is largely agreed upon within the mathematical and computational communities. Divergence arises when other readings (e.g., as a physical claim) are considered, but within this specific definitional frame, consensus is high.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians, computer scientists, and logicians are all beneficiaries (d near 0.0) as they gain conceptual clarity and a stable foundation for their work. There are no victims, as a definition cannot be violated or extract from an agent. Philosophers of computation act as observers, analyzing its status without being directly governed by it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_as_definition_vs_empirical_claim,
    'Is the Church-Turing Thesis fundamentally a mathematical definition, an empirical claim about the physical world, or an epistemological boundary?',
    'Philosophical analysis of its role in scientific practice, and the implications of hypothetical ''hypercomputation'' for its physical interpretation. This reading asserts it as a definition.',
    'If reclassified as an empirical claim, its extractiveness and suppression could rise if physical systems are found to violate it, leading to a re-evaluation of its ''mountain'' status. If reclassified as an epistemological boundary, its implications for knowledge would shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_as_definition_vs_empirical_claim, conceptual, 'Ambiguity regarding the fundamental nature of the Church-Turing Thesis.').

omega_variable(
    scope_of_effective_computability,
    'Does the definition of ''effective computability'' fully capture all intuitive notions of what can be computed by an algorithm, or are there edge cases or alternative formalisms that challenge its universality?',
    'Ongoing research in theoretical computer science and logic exploring alternative models of computation and their equivalence to Turing machines.',
    'If a widely accepted, non-Turing-equivalent model of ''effective computability'' were developed, the definitional consensus would be challenged, potentially increasing resistance and reducing accessibility collapse for this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_effective_computability, empirical, 'Uncertainty about the absolute universality and completeness of the Church-Turing definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.01).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1960, 0.01).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1980, 0.01).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.01).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1980, 0.02).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2024, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.05).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Church-Turing Thesis. Its status as a mathematical definition provides a stable conceptual foundation that influences, but does not foreclose, other interpretations of the thesis as an empirical claim or an epistemological boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
