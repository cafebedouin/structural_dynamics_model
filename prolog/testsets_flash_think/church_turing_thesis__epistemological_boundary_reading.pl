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
 *   domain: philosophy_of_computation
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as an epistemological
 *   boundary, defining what counts as 'formally knowable computation'. It
 *   asserts that functions we can prove computable are precisely those that
 *   are Turing-computable, irrespective of whether hypothetical physical
 *   processes might exceed this limit. This reading establishes a
 *   methodological standard within formal mathematics and computer science,
 *   coordinating research efforts but also implicitly excluding certain
 *   non-constructive or hypercomputational claims from the domain of
 *   'computability'.
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
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis: Epistemological Boundary of Knowable Computation").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '9eac89fe-ede9-491a-89ef-f4882bef46f0').
narrative_ontology:cs_kernel_codification('9eac89fe-ede9-491a-89ef-f4882bef46f0', formalized).
narrative_ontology:cs_authority_grounding('9eac89fe-ede9-491a-89ef-f4882bef46f0', expertise).
narrative_ontology:cs_interpretation_layer_present('9eac89fe-ede9-491a-89ef-f4882bef46f0').
narrative_ontology:cs_reading_relation('9eac89fe-ede9-491a-89ef-f4882bef46f0', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eac89fe-ede9-491a-89ef-f4882bef46f0', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('9eac89fe-ede9-491a-89ef-f4882bef46f0', foundational, computability_is_provability).
narrative_ontology:cs_axiom_status(computability_is_provability, holdable).
narrative_ontology:cs_axiom_grounding('9eac89fe-ede9-491a-89ef-f4882bef46f0', computability_is_provability, conventional).
narrative_ontology:cs_axiom('9eac89fe-ede9-491a-89ef-f4882bef46f0', foundational, formal_systems_are_turing_equivalent).
narrative_ontology:cs_axiom_status(formal_systems_are_turing_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('9eac89fe-ede9-491a-89ef-f4882bef46f0', formal_systems_are_turing_equivalent, empirically_contingent).
narrative_ontology:cs_reference_frame('9eac89fe-ede9-491a-89ef-f4882bef46f0', formal_proof_constructivism).
narrative_ontology:cs_drift_state('9eac89fe-ede9-491a-89ef-f4882bef46f0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9eac89fe-ede9-491a-89ef-f4882bef46f0', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_scientists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematicians).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, theorists_of_hypercomputation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, formal boundary for 'computable' that aligns with their emphasis on constructible proofs. The thesis provides a stable foundation for their work, defining the scope of what can be effectively calculated and proven.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    institutional, generational, mobile, universal).

% Rely on the thesis as a foundational principle for algorithm design, complexity theory, and the limits of what computers can achieve. It provides a stable, universally accepted framework for their theoretical and practical work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_scientists, beneficiary,
    institutional, generational, mobile, universal).

% Their work, which may involve existence proofs without explicit construction, is implicitly excluded from the 'knowable computation' defined by the thesis. While their methods are valid in other mathematical contexts, they are constrained in discussions of effective computability.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematicians, payer,
    powerful, generational, constrained, universal).

% Their theoretical models of computation beyond Turing equivalence are directly challenged or excluded by this reading of the thesis, which defines the boundary of what is formally 'knowable' as Turing-computable. They face an uphill battle for recognition within mainstream computability theory.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, theorists_of_hypercomputation, payer,
    moderate, biographical, constrained, universal).

% As the primary developers and enforcers of formal systems and proof standards, logicians play a key role in maintaining the epistemological boundary set by the Church-Turing Thesis. They define what counts as a valid formal proof of computability.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, logicians, agenda_setter,
    institutional, generational, mobile, universal).

% Analyze the philosophical implications, scope, and limitations of the Church-Turing Thesis, including its various interpretations and the debates surrounding them. They do not directly enforce or benefit from the constraint's operation but critically examine its foundations.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_mathematics, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, rigorous standard for what constitutes a 'computable function' within formal systems, enabling consistent proof techniques and a common language for theoretical computer science and constructive mathematics.
% TRANSFER_FUNCTION: Transfers epistemic authority and research focus towards Turing-computable models and constructive proofs, implicitly marginalizing non-constructive mathematical claims about computability and theoretical models of hypercomputation.
% ABSENT_VOICES: Researchers exploring forms of computation beyond Turing equivalence, or those who prioritize broader mathematical existence proofs over constructive ones, are structurally marginalized in discussions about 'knowable computability' within the formal framework established by the thesis.
% DISAPPEARANCE_RATIONALE: If this epistemological boundary vanished, the very definition of 'computable' in formal contexts would become ambiguous and fragmented. There would be no universally accepted standard for what constitutes a 'computable function' or a valid proof of computability, leading to a breakdown in shared understanding and research paradigms in theoretical computer science and constructive mathematics.
% FOUNDING_PROBLEM: The need for a precise, universally accepted definition of 'effective calculability' to formalize the limits of what can be computed by mechanical means, providing a rigorous foundation for logic and the nascent field of computer science.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing utility and foundational role of the thesis in computer science, logic, and constructive mathematics, attested by its pervasive presence in textbooks, research curricula, and the practical success of algorithms and computational models. This corroboration comes from the broad scientific and mathematical community, not just those who directly benefit from its exclusionary aspects.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The `extractiveness` is moderate (0.35) because while the thesis provides a useful framework, it also imposes a significant limitation on what is considered 'computable' in a formal sense, thereby extracting from alternative approaches. `Suppression` is moderate (0.60) as adherence to the thesis is actively enforced through peer review, curriculum design, and the acceptance criteria for formal proofs in relevant fields. `Theater_ratio` is low (0.10) because the thesis is a genuinely functional, foundational concept, not primarily performative. `Accessibility_collapse` is high (0.75) as it significantly narrows the scope of what is considered a valid computational model within formal contexts. `Resistance` is moderate (0.40) due to ongoing philosophical debates and theoretical work on hypercomputation, though the core thesis remains widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries like computer scientists, the thesis is a foundational 'rope' that enables robust theory and practical application. From the perspective of victims like hypercomputation theorists, it functions as a 'snare' that limits the scope of legitimate inquiry into computational models. The engine's computation of per-seat types will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Constructive mathematicians and computer scientists are beneficiaries (low d) as the thesis provides a stable, rigorous foundation for their work. Non-constructive mathematicians and theorists of hypercomputation are targets (high d) as their approaches are either implicitly excluded or directly challenged by this epistemological boundary. Logicians act as agenda-setters, defining and enforcing the formal standards that uphold this reading of the thesis.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this constraint as a pure 'rope' (which would ignore the exclusionary aspect) or a pure 'snare' (which would ignore its genuine coordination function in establishing a shared formal standard). The thesis genuinely coordinates by providing a common framework for computability, but it also extracts by defining what falls outside this 'knowable' boundary, requiring active enforcement to maintain this exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemological_vs_mathematical_definition,
    'Is the Church-Turing Thesis primarily an epistemological boundary for ''knowable computation'', or a purely conventional mathematical definition of ''effective computability''?',
    'Analysis of the philosophical arguments and the practical implications of accepting or rejecting the thesis in different contexts. If its force derives solely from definitional fiat, it leans towards the mathematical definition reading.',
    'If primarily a mathematical definition, the ''extraction'' from non-constructive claims would be reclassified as a definitional consequence rather than an active exclusion, potentially shifting the constraint type towards a ''rope'' or even ''mountain'' (by convention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemological_vs_mathematical_definition, conceptual, 'Distinguishing the epistemological boundary from a conventional mathematical definition.').

omega_variable(
    epistemological_vs_physical_claim,
    'Does this epistemological boundary implicitly or explicitly rely on the (contested) physical claim that no physical process can compute beyond Turing limits?',
    'Careful philosophical analysis of the arguments for the epistemological reading. If it''s found to implicitly assume the physical claim, then its validity becomes contingent on empirical physics, not just formal provability.',
    'If the epistemological boundary is found to be implicitly grounded in the physical claim, its stability and ''naturalness'' would be significantly reduced, and its classification might shift towards a ''tangled_rope'' with higher ''resistance'' if the physical claim is challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemological_vs_physical_claim, conceptual, 'Relationship between the epistemological boundary and the physical claim of the thesis.').

omega_variable(
    naturalness_of_the_boundary,
    'Is the boundary of ''formally knowable computation'' as defined by the Church-Turing Thesis a natural, inevitable limit, or a constructed methodological choice?',
    'Historical and philosophical analysis of alternative formalizations of computability and their eventual equivalence to Turing machines. If all ''reasonable'' formalisms converge, it supports naturalness; if alternatives are merely suppressed, it supports construction.',
    'If the boundary is a constructed choice, the ''extractiveness'' and ''suppression'' metrics would be seen as more deliberate and less inherent, reinforcing the ''tangled_rope'' classification. If it''s a natural limit, it would lean towards a ''mountain'' or ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_the_boundary, empirical, 'Whether the epistemological boundary is natural or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(chur_tr_t2010, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.25).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1970, 0.33).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(chur_be_t2010, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.45).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(chur_su_t2010, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, computational_complexity_theory).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, algorithm_design_principles).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Church-Turing Thesis kernel. Each reading has a different ε value, stakeholder structure, and classification, reflecting different interpretations of the thesis's nature and scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
