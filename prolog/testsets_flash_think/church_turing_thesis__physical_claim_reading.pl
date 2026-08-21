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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis (Physical Claim Reading)
 *   domain: philosophy_of_computation
 *
 * SUMMARY:
 *   This constraint represents the 'physical claim' reading of the
 *   Church-Turing Thesis, asserting that no physical process can compute
 *   functions beyond Turing-machine computability. It is presented as an
 *   empirical statement about the universe's fundamental limits. While
 *   claimed as a Mountain (a natural law), its operational effects include
 *   moderate extractiveness and suppression, as it channels research and
 *   resources away from alternative computational paradigms, creating
 *   identifiable beneficiaries and victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.6).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.7).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, mountain).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis (Physical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation").

domain_priors:emerges_naturally(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'c5ae8430-759e-49b9-8263-4a89e50bb0c9').
narrative_ontology:cs_kernel_codification('c5ae8430-759e-49b9-8263-4a89e50bb0c9', formalized).
narrative_ontology:cs_authority_grounding('c5ae8430-759e-49b9-8263-4a89e50bb0c9', expertise).
narrative_ontology:cs_interpretation_layer_present('c5ae8430-759e-49b9-8263-4a89e50bb0c9').
narrative_ontology:cs_reading_relation('c5ae8430-759e-49b9-8263-4a89e50bb0c9', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5ae8430-759e-49b9-8263-4a89e50bb0c9', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('c5ae8430-759e-49b9-8263-4a89e50bb0c9', foundational, physical_computability_is_turing_equivalent).
narrative_ontology:cs_axiom_status(physical_computability_is_turing_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('c5ae8430-759e-49b9-8263-4a89e50bb0c9', physical_computability_is_turing_equivalent, empirically_contingent).
narrative_ontology:cs_reference_frame('c5ae8430-759e-49b9-8263-4a89e50bb0c9', turing_machine_universality_principle).
narrative_ontology:cs_drift_state('c5ae8430-759e-49b9-8263-4a89e50bb0c9', contemporary_quantum_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('c5ae8430-759e-49b9-8263-4a89e50bb0c9', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, computational_complexity_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, widely accepted boundary for what is physically computable, which simplifies theoretical frameworks and guides research funding. Their careers and established theories are built on this foundation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computer_scientists, beneficiary,
    institutional, generational, analytical, global).

% Their entire field is predicated on the assumption that physical computation is bounded by Turing computability. This thesis provides the bedrock for defining complexity classes and limits.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computational_complexity_theorists, beneficiary,
    institutional, generational, analytical, global).

% Bear the cost of the constraint by facing skepticism, difficulty in securing funding, and marginalization for exploring computational models that theoretically exceed Turing limits. Their work is often dismissed as 'unphysical'.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    powerless, biographical, constrained, global).

% While quantum computers are generally believed to be Turing-equivalent, some interpretations or future developments might challenge the physical Church-Turing Thesis. They face the burden of proving their claims do not violate this established boundary, or of re-evaluating the boundary itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_advocates, payer,
    moderate, biographical, constrained, global).

% Analyze the conceptual and empirical implications of the thesis, its various readings, and its relationship to physics and mathematics. They do not directly benefit or pay but provide critical meta-analysis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, universal).

narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding and boundary for what constitutes 'physical computability' within the scientific community, guiding research and theoretical development in computer science and physics.
% TRANSFER_FUNCTION: Transfers legitimacy, research funding, and academic prestige towards research within the Turing-computable paradigm, and away from speculative 'hypercomputation' models.
% ABSENT_VOICES: Researchers in fringe areas of theoretical physics or mathematics who explore exotic computational models (e.g., based on general relativity, quantum gravity, or non-linear quantum mechanics) that might exceed Turing limits. Their work is often outside mainstream computer science discourse.
% DISAPPEARANCE_RATIONALE: If the physical Church-Turing Thesis were definitively disproven (e.g., by a physically realizable hypercomputer), it would necessitate a fundamental re-evaluation of the laws of physics, the limits of computation, and the foundations of computer science and artificial intelligence. The entire scientific landscape would reorganize.
% FOUNDING_PROBLEM: To define the fundamental limits of what is effectively computable by any physical process in the universe.
% FOUNDING_PROBLEM_CORROBORATION: The vast majority of the scientific community, particularly computer scientists and physicists, implicitly or explicitly corroborates the thesis through their research and theoretical frameworks. However, a minority of researchers (e.g., hypercomputation advocates) actively contest its empirical status, citing theoretical possibilities or interpretations of quantum mechanics. This contestation is acknowledged within philosophy of computation.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, ExtMetricName, E),
    domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(church_turing_thesis__physical_claim_reading),
    narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) arises from the opportunity cost for researchers pursuing non-Turing computation, whose work is often unfunded or marginalized. Suppression (0.70) is high because the scientific consensus actively discourages and dismisses research into hypercomputation, effectively 'suppressing' alternative lines of inquiry. Theater ratio is low (0.10) as the thesis is a serious scientific claim, not primarily performative. Accessibility collapse (0.60) reflects the conceptual closure around what is considered 'physically computable,' making alternatives difficult to pursue. Resistance (0.50) comes from the persistent, albeit minority, efforts of hypercomputation researchers and some quantum computing theorists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream computer science, the thesis is a foundational truth that provides a stable framework for research. From the perspective of hypercomputation researchers, it acts as a gatekeeping mechanism that stifles innovation and exploration of potentially valid physical phenomena. The engine's computation of per-seat types will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream computer scientists and computational complexity theorists are beneficiaries (low d) as their fields are built upon and validated by this thesis. Hypercomputation researchers and quantum supremacy advocates (if their claims exceed CT) are targets (high d) as their work is constrained and often dismissed by the prevailing consensus. The constraint subsidizes the established paradigm by limiting competition from alternative models.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a claimed Mountain, despite moderate extractiveness and suppression, highlights the tension between its presentation as a fundamental limit and its operational effects. If the empirical claim were definitively disproven, its persistence would shift it towards a Piton or Snare, maintained by institutional inertia or the beneficiaries' resistance to paradigm shift, rather than genuine natural law. This prevents mislabeling a potentially disproven empirical claim as an unchangeable truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    is_ctt_a_physical_law,
    'Is the Church-Turing Thesis, as a physical claim, a true and fundamental law of the universe, or an empirically falsifiable hypothesis?',
    'Discovery of a physically realizable hypercomputer, or a definitive theoretical proof that no such device is possible within known physics.',
    'If proven false, the constraint would shift from a claimed Mountain to a Snare (if maintained by inertia) or a Scaffold (if it opens new physics). If definitively proven true, its extractiveness would be re-evaluated as a necessary cost of a true natural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(is_ctt_a_physical_law, empirical, 'Empirical status of the physical Church-Turing Thesis.').

omega_variable(
    suppression_of_hypercomputation_research,
    'Is the suppression of hypercomputation research justified by scientific rigor and lack of evidence, or by institutional inertia and resistance to paradigm shifts?',
    'Analysis of funding patterns, publication biases, and the reception of novel theoretical models in physics and computer science over time.',
    'If primarily due to inertia, the suppression metric might be higher than warranted by pure scientific merit, indicating a more extractive constraint. If purely scientific, the suppression is a necessary part of maintaining scientific standards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_hypercomputation_research, conceptual, 'Justification for suppressing hypercomputation research.').

omega_variable(
    kernel_reading_ambiguity,
    'Given the Church-Turing Thesis kernel, is the ''physical claim'' reading the most appropriate framing, or do the ''mathematical definition'' or ''epistemological boundary'' readings offer a more accurate structural account?',
    'Philosophical analysis of the thesis''s historical development, its role in different scientific disciplines, and the implications of each reading for scientific practice and funding.',
    'Adopting a different reading would fundamentally alter the constraint''s claimed type, beneficiaries, and victims. For example, the ''mathematical definition'' reading would likely be a Rope or Mountain with no victims, while the ''epistemological boundary'' reading might be a Mountain with different implications for research.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the primary reading of the Church-Turing Thesis kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__physical_claim_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__physical_claim_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__physical_claim_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(chur_tr_t2010, church_turing_thesis__physical_claim_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__physical_claim_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.5).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__physical_claim_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__physical_claim_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__physical_claim_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(chur_be_t2010, church_turing_thesis__physical_claim_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__physical_claim_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.6).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__physical_claim_reading, suppression_requirement, 1950, 0.63).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__physical_claim_reading, suppression_requirement, 1970, 0.66).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__physical_claim_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(chur_su_t2010, church_turing_thesis__physical_claim_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__physical_claim_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, ai_safety_alignment_research).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, quantum_computing_limits).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three distinct readings of the Church-Turing Thesis kernel. Each reading has a different structural claim and metric profile, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
