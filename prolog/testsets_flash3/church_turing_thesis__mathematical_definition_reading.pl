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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as a mathematical
 *   definition, stipulating what 'effective computability' means. It is a
 *   convention, not an empirically testable claim. This reading emphasizes
 *   its role in providing conceptual clarity and a stable foundation for
 *   theoretical computer science and mathematical logic. It is one reading of
 *   the broader 'Church-Turing Thesis' kernel, which also has physical and
 *   epistemological interpretations.
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
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae').
narrative_ontology:cs_kernel_codification('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', formalized).
narrative_ontology:cs_authority_grounding('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', expertise).
narrative_ontology:cs_interpretation_layer_present('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae').
narrative_ontology:cs_reading_relation('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', foundational, computability_is_formally_defined).
narrative_ontology:cs_axiom_status(computability_is_formally_defined, holdable).
narrative_ontology:cs_axiom_grounding('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', computability_is_formally_defined, conventional).
narrative_ontology:cs_axiom('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', foundational, definition_is_not_empirical_claim).
narrative_ontology:cs_axiom_status(definition_is_not_empirical_claim, holdable).
narrative_ontology:cs_axiom_grounding('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', definition_is_not_empirical_claim, deontological).
narrative_ontology:cs_reference_frame('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', formal_mathematical_definition).
narrative_ontology:cs_drift_state('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('58fbbf4c-f8a8-48c4-a44f-1f9630b9fcae', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, logicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_clarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, universally accepted definition of 'computable function' that allows for rigorous proofs and consistent terminology across sub-disciplines. They use the thesis as a foundational axiom in their work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematicians, beneficiary,
    institutional, generational, mobile, global).

% Utilize the thesis as the bedrock for theoretical computer science, defining the limits of what algorithms can achieve. It provides a stable framework for designing and analyzing computational models.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_scientists, beneficiary,
    institutional, generational, mobile, global).

% Benefit from the thesis as a precise formalization of intuitive notions of computability, enabling the study of decidability and undecidability in formal systems. It clarifies the scope of formal methods.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, logicians, beneficiary,
    institutional, generational, mobile, global).

% The abstract good of unambiguous and consistent mathematical language, which is directly served by the thesis acting as a definition. This is a non-agent entity that benefits conceptually.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_clarity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(church_turing_thesis__mathematical_definition_reading, mathematical_clarity).

% Analyze the nature and implications of the thesis, including its status as a definition versus an empirical claim. They observe its use and contest its interpretation without directly benefiting or paying from its operation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accepted, precise mathematical definition for the intuitive concept of 'effective computability,' coordinating terminology and foundational assumptions across mathematics, logic, and computer science.
% TRANSFER_FUNCTION: Transfers conceptual clarity and definitional stability to the fields of mathematics and computer science, from the collective agreement of its practitioners.
% ABSENT_VOICES: No voices are truly absent from this reading, as its status as a definition is largely accepted within the mathematical community. Disagreements arise in other readings (e.g., its physical implications), but not its definitional role.
% DISAPPEARANCE_RATIONALE: If the Church-Turing Thesis as a mathematical definition vanished, the foundational language of computability theory would collapse, leading to widespread ambiguity and inconsistency in proofs and theoretical models. The entire field would need to re-establish a common understanding of 'computable function'.
% FOUNDING_PROBLEM: The pre-1930s lack of a precise, formal definition for 'effectively computable function' in mathematics, leading to ambiguity in foundational arguments and proofs.
% FOUNDING_PROBLEM_CORROBORATION: Mathematicians and computer scientists universally attest that the problem of defining computability was live and that the thesis successfully resolved it, providing a stable foundation for their fields. This is corroborated by the consistent use of the definition in textbooks and research for decades.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.02) because a definition, by its nature, does not extract from those who adopt it; it provides a common language. Suppression is minimal (0.05) as adherence is driven by the utility of a shared definition, not coercion. Theater ratio is negligible (0.01) as its function is purely definitional and conceptual. Accessibility collapse is high (0.95) because once the definition is accepted, alternatives for 'effective computability' are largely foreclosed within the mathematical framework. Resistance is very low (0.01) because its definitional status is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of all direct stakeholders (mathematicians, computer scientists, logicians), this constraint is a pure coordination mechanism (a Rope) that provides immense benefit through clarity. There is no significant perspectival gap for this specific reading, as its definitional status is largely uncontested within its domain of application.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians, computer scientists, and logicians are all beneficiaries, as they gain conceptual clarity and a stable foundation for their work. Mathematical clarity itself is a non-agent beneficiary. There are no victims, as a definition cannot extract or harm. Philosophers of computation act as observers, analyzing its implications without being directly subject to its definitional force.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_as_definition_vs_claim,
    'Is the Church-Turing Thesis fundamentally a mathematical definition, an empirical claim about physics, or an epistemological boundary?',
    'Philosophical consensus on the primary epistemic status, or a formal proof demonstrating its equivalence to a physical law (unlikely).',
    'If reclassified as primarily an empirical claim, its extractiveness and suppression might increase (e.g., if it suppresses research into hypercomputation); if an epistemological boundary, its scope and implications for knowledge would shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_as_definition_vs_claim, conceptual, 'Ambiguity regarding the fundamental epistemic status of the Church-Turing Thesis.').

omega_variable(
    scope_of_computability_intuition,
    'Does the mathematical definition fully capture the intuitive notion of ''effective computability'' for all relevant contexts, or are there aspects of intuition that remain outside its formalization?',
    'Further philosophical analysis and potential development of alternative formalisms that capture different facets of ''computability'' intuition.',
    'If the definition is found to be incomplete, its coordination function might be weakened, leading to new definitional efforts and potential fragmentation in the field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_computability_intuition, conceptual, 'Whether the formal definition fully aligns with the intuitive concept it aims to capture.').


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
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1960, 0.02).
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
% This constraint is one reading of the Church-Turing Thesis kernel. Its definitional status influences, and is influenced by, the physical and epistemological interpretations of the thesis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
