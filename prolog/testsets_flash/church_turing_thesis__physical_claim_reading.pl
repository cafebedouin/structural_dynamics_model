% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__physical_claim_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis (Physical Claim Reading)
 *   domain: philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis (CTT) as an empirical
 *   claim about the physical universe: that no physical process can compute
 *   functions beyond Turing-machine computability. This reading is distinct
 *   from the CTT as a mathematical definition or an epistemological boundary.
 *   It acts as a foundational assumption in mainstream computer science and
 *   physics, implicitly suppressing research into 'hypercomputation' or
 *   physical systems that might exceed Turing limits. The constraint is
 *   claimed as a Tangled Rope because it provides a coordination function (a
 *   shared understanding of computational limits) but also extracts from and
 *   suppresses alternative research directions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.65).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis (Physical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '247f7e81-cc4e-4057-b68e-fc980800b6de').
narrative_ontology:cs_kernel_codification('247f7e81-cc4e-4057-b68e-fc980800b6de', formalized).
narrative_ontology:cs_authority_grounding('247f7e81-cc4e-4057-b68e-fc980800b6de', expertise).
narrative_ontology:cs_interpretation_layer_present('247f7e81-cc4e-4057-b68e-fc980800b6de').
narrative_ontology:cs_reading_relation('247f7e81-cc4e-4057-b68e-fc980800b6de', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('247f7e81-cc4e-4057-b68e-fc980800b6de', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('247f7e81-cc4e-4057-b68e-fc980800b6de', foundational, physical_processes_are_turing_equivalent).
narrative_ontology:cs_axiom_status(physical_processes_are_turing_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('247f7e81-cc4e-4057-b68e-fc980800b6de', physical_processes_are_turing_equivalent, empirically_contingent).
narrative_ontology:cs_reference_frame('247f7e81-cc4e-4057-b68e-fc980800b6de', turing_computability_as_physical_limit).
narrative_ontology:cs_drift_state('247f7e81-cc4e-4057-b68e-fc980800b6de', contemporary_quantum_computing_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('247f7e81-cc4e-4057-b68e-fc980800b6de', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computer_science).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_physics_paradigm).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a stable theoretical foundation that limits the scope of 'computable' problems, simplifying curriculum and research directions. It would face significant disruption if hypercomputation were physically realized.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computer_science, beneficiary,
    institutional, generational, constrained, global).

% The physical claim reading aligns with the classical understanding of physical limits, providing a coherent framework. It would require fundamental revision if physical hypercomputation were demonstrated.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, classical_physics_paradigm, beneficiary,
    institutional, generational, constrained, universal).

% Their research into models of computation beyond Turing machines (e.g., using general relativity, quantum gravity, or exotic physics) is often marginalized or dismissed as 'not physics' or 'not computation' due to the dominance of this reading. They bear the cost of skepticism and funding difficulty.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% While demonstrating 'quantum supremacy' (solving problems intractable for classical computers) is a major goal, if such a demonstration were to exceed Turing-machine computability, this reading of the CTT would act as a conceptual barrier, forcing a re-evaluation of what 'computation' means in physics.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_theorists, payer,
    powerful, biographical, constrained, global).

% Analyze the implications of the CTT for the nature of physical reality and the limits of knowledge. They are not directly impacted by the constraint's enforcement but critically evaluate its empirical status and philosophical consequences.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_computation, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared empirical assumption about the limits of physical computation, allowing researchers to focus on problems within that boundary and avoid speculative avenues that are presumed impossible.
% TRANSFER_FUNCTION: Transfers legitimacy and resources away from research programs exploring 'hypercomputation' or physical processes that might exceed Turing limits, towards those operating within the established framework.
% ABSENT_VOICES: Future researchers who might discover or engineer physical hypercomputation would object to the premature closure of possibilities. Their 'voice' is currently hypothetical, but the constraint actively suppresses the conditions for its emergence.
% DISAPPEARANCE_RATIONALE: If the physical claim reading of the CTT vanished (e.g., due to a definitive experimental demonstration of hypercomputation), the fields of computer science and physics would undergo a profound rearrangement, opening new research frontiers and requiring a re-evaluation of fundamental assumptions about information and reality.
% FOUNDING_PROBLEM: To establish the empirical limits of what physical machines can compute, providing a foundational understanding for the nascent fields of computer science and theoretical physics.
% FOUNDING_PROBLEM_CORROBORATION: The problem is considered live by most computer scientists and physicists, as no physical process has definitively been shown to exceed Turing computability. However, hypercomputation researchers and some philosophers contest its 'settled' status, pointing to theoretical models and the incompleteness of current physical theories. The corroboration is thus strong from within the mainstream, but contested by a minority.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while the claim provides a useful boundary, it also limits the scope of inquiry and potentially marginalizes non-conforming research. Suppression is higher (0.65) as the scientific community actively enforces this boundary through peer review, funding decisions, and conceptual frameworks, making it difficult for 'hypercomputation' research to gain traction. Theater ratio is low (0.1) because the claim is genuinely believed to be true by many, and the enforcement is largely a sincere defense of a core scientific principle, not mere performance. Accessibility collapse is 0.7, reflecting that while theoretical alternatives exist, the practical and institutional barriers to pursuing them are significant. Resistance is 0.4, indicating ongoing but minority challenges from hypercomputation researchers and philosophers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream computer science, the CTT as a physical claim is a foundational truth that coordinates research and prevents wasted effort on impossible problems. From the perspective of hypercomputation researchers, it is a suppressive dogma that closes off legitimate avenues of inquiry and extracts career costs. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream computer science and classical physics are beneficiaries (d near 0.0-0.2) as the constraint provides a stable, coherent framework for their work. Hypercomputation researchers and quantum supremacy theorists are payers/targets (d near 0.8-1.0) as their work directly challenges or is constrained by this reading of the CTT. Philosophers of computation are observers (d near 0.5) as they analyze the constraint without being directly subject to its enforcement or benefiting from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to define the limits of physical computation) is still considered live by its beneficiaries. However, the 'tangled rope' classification suggests that the coordination function is intertwined with an extractive/suppressive element, where the 'solution' to the founding problem (fragmented understanding of computation) now also serves to maintain a particular paradigm against challengers. If hypercomputation were proven, the constraint would become a Snare or Piton, as its coordination function would collapse, leaving only the suppressive inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_status_of_physical_claim,
    'Is the physical claim reading of the Church-Turing Thesis an empirically settled fact, or an open scientific question?',
    'Experimental demonstration of a physical process that computes a non-Turing computable function, or a robust theoretical argument from a widely accepted physical theory that such a process is impossible.',
    'If settled as fact, the constraint''s suppressive aspect might be reclassified as a ''mountain'' (natural law). If proven false, the constraint would collapse, opening new research paradigms and reclassifying as a ''scaffold'' (temporary support that failed) or ''piton'' (inertial dogma).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_of_physical_claim, empirical, 'The empirical status of the CTT as a physical claim.').

omega_variable(
    distinction_from_mathematical_claim,
    'Is the ''physical claim'' reading truly distinct from the ''mathematical definition'' reading, or does the empirical claim merely follow from the mathematical one?',
    'Philosophical analysis clarifying the modal status of the CTT: whether it is necessarily true by definition, or contingently true as an empirical observation. This would involve examining the relationship between mathematical models and physical reality.',
    'If the physical claim is found to be merely a consequence of the mathematical definition, its ''extractiveness'' and ''suppression'' would be re-evaluated downwards, as it would be less a ''claim'' and more a ''convention'' applied to physics, potentially shifting its classification towards a ''rope'' or even ''mountain'' (if the convention is seen as universally beneficial).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distinction_from_mathematical_claim, conceptual, 'Conceptual distinction between physical and mathematical readings of CTT.').

omega_variable(
    resource_allocation_bias,
    'To what extent does the acceptance of the physical claim reading bias funding and institutional support away from ''hypercomputation'' research?',
    'Quantitative analysis of grant applications, publication acceptance rates, and academic hiring trends in fields related to non-Turing computation, compared to mainstream computer science and physics.',
    'If a significant bias is demonstrated, the ''suppression'' metric would be further validated, and the ''extractiveness'' could be seen as more direct, strengthening the ''snare'' or ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_bias, empirical, 'Impact of CTT physical claim on research resource allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement_basis(chur_tr_t1936, observed).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__physical_claim_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement_basis(chur_tr_t1960, observed).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__physical_claim_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement_basis(chur_tr_t1980, observed).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__physical_claim_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement_basis(chur_tr_t2000, observed).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__physical_claim_reading, theater_ratio, 2024, 0.1).
narrative_ontology:measurement_basis(chur_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.4).
narrative_ontology:measurement_basis(chur_be_t1936, observed).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__physical_claim_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement_basis(chur_be_t1960, observed).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__physical_claim_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement_basis(chur_be_t1980, observed).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__physical_claim_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(chur_be_t2000, observed).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__physical_claim_reading, base_extractiveness, 2024, 0.55).
narrative_ontology:measurement_basis(chur_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement_basis(chur_su_t1936, observed).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__physical_claim_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement_basis(chur_su_t1960, observed).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__physical_claim_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement_basis(chur_su_t1980, observed).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__physical_claim_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement_basis(chur_su_t2000, observed).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__physical_claim_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement_basis(chur_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, quantum_computation_limits).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, computational_complexity_theory).

% DUAL FORMULATION NOTE:
% The Church-Turing Thesis is a kernel with multiple readings. This file represents the 'physical claim' reading, which asserts an empirical limit on physical computation. It influences and is influenced by the mathematical and epistemological readings, as well as downstream constraints on quantum computation and complexity theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
