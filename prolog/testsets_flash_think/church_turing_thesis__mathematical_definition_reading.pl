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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Church-Turing Thesis: Mathematical Definition Reading
 *   domain: philosophy_of_mathematics/computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as a mathematical
 *   definition, stipulating what is meant by 'effective computability' within
 *   formal systems. It is a convention adopted by mathematicians and computer
 *   scientists to provide a rigorous foundation for computability theory. As
 *   a definition, it is not empirically testable but serves to coordinate
 *   conceptual understanding. This is one reading of the broader
 *   'Church-Turing Thesis' kernel, which also has empirical and
 *   epistemological interpretations.
 *
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
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis: Mathematical Definition Reading").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/computation/foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '04a787b4-f559-43d9-b12f-5eb0783cf928').
narrative_ontology:cs_kernel_codification('04a787b4-f559-43d9-b12f-5eb0783cf928', formalized).
narrative_ontology:cs_authority_grounding('04a787b4-f559-43d9-b12f-5eb0783cf928', expertise).
narrative_ontology:cs_reading_relation('04a787b4-f559-43d9-b12f-5eb0783cf928', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('04a787b4-f559-43d9-b12f-5eb0783cf928', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('04a787b4-f559-43d9-b12f-5eb0783cf928', foundational, effective_computability_is_turing_computability).
narrative_ontology:cs_axiom_status(effective_computability_is_turing_computability, holdable).
narrative_ontology:cs_axiom_grounding('04a787b4-f559-43d9-b12f-5eb0783cf928', effective_computability_is_turing_computability, conventional).
narrative_ontology:cs_reference_frame('04a787b4-f559-43d9-b12f-5eb0783cf928', foundational_mathematical_definition).
narrative_ontology:cs_drift_state('04a787b4-f559-43d9-b12f-5eb0783cf928', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('04a787b4-f559-43d9-b12f-5eb0783cf928', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computability_theorists).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formal_systems_consistency).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, rigorous_mathematical_definition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, universally accepted definition that provides a stable foundation for theoretical work in logic and computability theory, allowing for rigorous proofs and consistent terminology.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematicians, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from a precise definition that underpins the theoretical limits and capabilities of algorithms, programming languages, and computational models, providing a common reference point for research.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_scientists, beneficiary,
    moderate, biographical, mobile, global).

% Are the primary custodians and users of the definition, ensuring its consistent application, teaching, and interpretation within the field. They benefit from the conceptual clarity it provides to their specialized area of research.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computability_theorists, agenda_setter,
    powerful, generational, mobile, global).

% Analyze the implications and interpretations of the thesis, including its status as a definition versus an empirical claim or an epistemological boundary. They observe its use and impact within the broader academic discourse.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a precise and universally accepted formal definition for the intuitive notion of 'effective computability', enabling clear communication and rigorous proof in mathematics and computer science.
% TRANSFER_FUNCTION: Transfers conceptual clarity, a shared foundational framework, and a basis for rigorous discourse to the fields of mathematics and computer science, from the collective agreement and intellectual labor of the academic community.
% ABSENT_VOICES: None, as the definition is widely accepted within the relevant academic communities. Any 'dissent' typically concerns its *interpretation* (e.g., as an empirical claim or epistemological boundary) rather than its utility as a definition.
% DISAPPEARANCE_RATIONALE: If this foundational definition vanished overnight, the fields of computability theory, theoretical computer science, and parts of mathematical logic would lose their common ground. This would lead to ambiguity, inconsistent results, and a breakdown in communication regarding what constitutes a 'computable' function, necessitating a rapid re-establishment of a similar convention.
% FOUNDING_PROBLEM: The pre-existence of an intuitive but imprecise notion of 'effective computability' that lacked a rigorous formal counterpart, hindering the development of a formal theory of computation and consistent mathematical proofs.
% FOUNDING_PROBLEM_CORROBORATION: The continued and widespread use of the Church-Turing Thesis as a definition in textbooks, research papers, and curricula across mathematics and computer science corroborates its ongoing utility and the live status of the problem it solves. No significant academic faction disputes its definitional role within its mathematical context.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because a definition primarily serves to clarify and coordinate, incurring minimal 'cost' beyond the intellectual effort of adoption. Suppression is low (0.1) as adherence is driven by intellectual utility and consensus rather than coercion. Theater ratio is very low (0.05) because its function as a foundational definition is direct and highly effective. Accessibility collapse is high (0.9) because, once accepted, alternative definitions for 'effective computability' within the same formal context are largely superseded. Resistance is very low (0.05) due to its widespread acceptance and utility.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians, computer scientists, and computability theorists are all beneficiaries. They gain conceptual clarity and a stable foundation for their work without bearing significant costs. The constraint subsidizes their research by providing a common, unambiguous starting point. Philosophers of computation act as observers, analyzing its implications without being directly subject to its definitional force.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_as_definition_vs_empirical_claim,
    'Is the thesis *purely* a mathematical definition, or does its widespread acceptance implicitly carry an empirical claim about physical reality (as in the ''physical_claim_reading'')?',
    'Further philosophical analysis distinguishing definitional from empirical statements, coupled with empirical tests of physical computability limits (e.g., discovery of hypercomputation in physics).',
    'If it were found to implicitly carry a strong empirical claim that is later falsified, its classification might shift from a Rope to a Piton (if maintained theatrically) or even a Snare (if used to suppress research into alternative computational models).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_as_definition_vs_empirical_claim, conceptual, 'Ambiguity regarding the thesis''s status as a definition versus an empirical claim about the physical world.').

omega_variable(
    status_as_definition_vs_epistemological_boundary,
    'Is the thesis *purely* a mathematical definition, or does its widespread acceptance implicitly carry an epistemological claim about the limits of formal knowledge (as in the ''epistemological_boundary_reading'')?',
    'Further philosophical analysis distinguishing definitional from epistemological statements, and exploration of formal systems that might challenge the ''knowable'' boundary within mathematics.',
    'If it were found to implicitly carry a strong epistemological claim, its classification might shift towards a Mountain (if an irreducible limit of knowledge) or a Tangled Rope (if used to enforce a particular epistemic framework within academia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_as_definition_vs_epistemological_boundary, conceptual, 'Ambiguity regarding the thesis''s status as a definition versus an epistemological claim about the limits of formal knowledge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1966, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1966, 0.05).
narrative_ontology:measurement(chur_tr_t1996, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1996, 0.05).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.05).
narrative_ontology:measurement(chur_be_t1966, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1966, 0.05).
narrative_ontology:measurement(chur_be_t1996, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1996, 0.05).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2026, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.1).
narrative_ontology:measurement(chur_su_t1966, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1966, 0.1).
narrative_ontology:measurement(chur_su_t1996, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1996, 0.1).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2026, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Church-Turing Thesis kernel. Each reading has a different structural function and ε value, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
