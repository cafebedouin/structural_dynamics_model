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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint story instantiates the 'physical claim' reading of the
 *   Church-Turing Thesis, which asserts that no physical process can compute
 *   functions beyond Turing-machine computability. While presented as a
 *   fundamental truth about the universe, its acceptance has significant
 *   implications for research funding and the legitimacy of certain lines of
 *   inquiry, particularly in hypercomputation and some interpretations of
 *   quantum supremacy. The claimed type is 'mountain' because it asserts a
 *   natural, irreducible physical limit, but the metrics reflect its
 *   operational impact as a potentially suppressive force on alternative
 *   research paradigms.
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
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

domain_priors:emerges_naturally(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '32e70d38-926f-4ea3-b6d2-c213c7aaa42c').
narrative_ontology:cs_kernel_codification('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', formalized).
narrative_ontology:cs_authority_grounding('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', expertise).
narrative_ontology:cs_interpretation_layer_present('32e70d38-926f-4ea3-b6d2-c213c7aaa42c').
narrative_ontology:cs_reading_relation('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', foundational, physical_computability_is_turing_computability).
narrative_ontology:cs_axiom_status(physical_computability_is_turing_computability, holdable).
narrative_ontology:cs_axiom_grounding('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', physical_computability_is_turing_computability, empirically_contingent).
narrative_ontology:cs_reference_frame('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', turing_machine_as_universal_physical_model).
narrative_ontology:cs_drift_state('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', contemporary_quantum_computing_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('32e70d38-926f-4ea3-b6d2-c213c7aaa42c', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computational_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_machine_universality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their research and theoretical frameworks are validated by the thesis, which provides a stable foundation for their work. Shifting away would require a paradigm change.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computational_theorists, beneficiary,
    institutional, generational, constrained, global).

% Their work on models of computation beyond Turing machines is often marginalized, faces skepticism, and struggles for funding due to the prevailing belief in the physical Church-Turing Thesis. They bear the cost of intellectual suppression.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Researchers claiming quantum computers can achieve 'quantum supremacy' (solving problems intractable for classical computers) often find their work interpreted within the bounds of the Church-Turing Thesis, or face the burden of proving they exceed it in a way that challenges the physical claim.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, payer,
    moderate, biographical, constrained, global).

% They analyze the implications, scope, and validity of the Church-Turing Thesis as a physical claim, often engaging in meta-level debates without directly participating in the computational research itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, universal).

% They allocate research grants based on prevailing scientific consensus. The physical Church-Turing Thesis influences their decisions, making it harder for hypercomputation research to secure funding, while supporting mainstream computational physics.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, funding_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared foundational understanding of the limits of physical computation, guiding research directions and theoretical development in computer science and physics.
% TRANSFER_FUNCTION: Transfers legitimacy, academic prestige, and research funding towards computational models and theories consistent with Turing-machine computability, and away from those proposing hypercomputation.
% ABSENT_VOICES: The 'voices' of hypothetical physical hypercomputers or processes that could exceed Turing computability are absent from mainstream scientific discourse, as their existence is denied by the thesis itself. Their potential implications for physics and computation are not fully explored.
% DISAPPEARANCE_RATIONALE: If the physical Church-Turing Thesis were definitively disproven (e.g., by the discovery of a physical hypercomputer), it would fundamentally alter the understanding of physics, computation, and information. Entire fields of research would open, technological possibilities would expand, and the philosophical foundations of computability would be rewritten.
% FOUNDING_PROBLEM: To define the fundamental limits of what is physically computable by any machine or process in the universe, providing a bedrock principle for the nascent fields of computer science and theoretical physics.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing theoretical and experimental search for physical processes that might exceed Turing computability (e.g., in quantum gravity, black hole physics, or exotic quantum systems), as well as philosophical debates, corroborates that the question of physical computability limits remains a live problem, even if the thesis is widely accepted.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The `extractiveness` (0.60) and `suppression` (0.70) metrics reflect the intellectual and institutional costs borne by researchers whose work challenges or falls outside the bounds of the physical Church-Turing Thesis. While not enforced by direct coercion, the consensus around the thesis can lead to difficulty in securing funding, publishing, and gaining academic recognition for 'non-Turing' computational models. `Theater_ratio` is low (0.10) because the thesis is a serious scientific claim, not primarily performative. `Accessibility_collapse` (0.60) is moderate; alternatives are not entirely collapsed, but significantly constrained. `Resistance` (0.50) is moderate, reflecting ongoing, albeit often marginalized, research into hypercomputation and related fields.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream computational theorists, the thesis is a foundational 'mountain' that correctly describes the limits of reality. From the perspective of hypercomputation researchers, the same thesis acts as a 'snare' or 'tangled rope,' suppressing legitimate lines of inquiry and extracting career capital. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream computational theorists benefit from the stability and foundational validation the thesis provides, positioning them as beneficiaries. Hypercomputation researchers and certain quantum supremacy claimants bear the costs of marginalization and funding difficulties, making them payers. Funding agencies act as agenda-setters, directing resources based on the thesis's perceived validity. Philosophers of computation primarily observe and analyze, without direct benefit or cost from its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_status_of_thesis,
    'Is the Church-Turing Thesis, as a physical claim, truly an irreducible feature of the universe, or is its empirical status still open to falsification by future discoveries?',
    'Discovery of a physical process or device that demonstrably computes a non-Turing-computable function, or a theoretical breakthrough in physics that proves such processes are impossible.',
    'If falsified, the constraint would shift from a ''mountain'' (natural law) to a ''snare'' (suppressing valid research) or ''scaffold'' (if it opens new physics), with significant reclassification for all stakeholders. If definitively proven, its ''mountain'' status would be solidified, but its suppressive effects would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_of_thesis, empirical, 'The empirical testability and potential falsifiability of the physical Church-Turing Thesis.').

omega_variable(
    suppression_justification,
    'Is the suppression of hypercomputation research a justified consequence of a robust scientific principle, or an artifact of institutional inertia and a lack of open-mindedness to alternative paradigms?',
    'A shift in funding patterns and academic acceptance for hypercomputation research, even without definitive empirical falsification of the thesis, would suggest the latter. Continued marginalization despite compelling theoretical arguments would suggest the former.',
    'If the suppression is primarily due to inertia, the constraint''s ''snare''-like qualities are amplified, and its legitimacy as a ''mountain'' is undermined. If justified, the suppression is a necessary cost of maintaining scientific rigor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_justification, conceptual, 'The ethical and epistemic justification for the suppression of hypercomputation research.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.4).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__physical_claim_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__physical_claim_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__physical_claim_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(chur_be_t2010, church_turing_thesis__physical_claim_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__physical_claim_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__physical_claim_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__physical_claim_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__physical_claim_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(chur_su_t2010, church_turing_thesis__physical_claim_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__physical_claim_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Church-Turing Thesis kernel. Each reading has a different structural interpretation and set of implications, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
