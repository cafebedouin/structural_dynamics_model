% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Church-Turing Thesis as Physical Law
 *   domain: foundations_of_computer_science/philosophy_of_physics
 *
 * SUMMARY:
 *   The physical claim reading of the Church-Turing thesis asserts that no
 *   physical process in the universe can compute functions beyond what a
 *   Turing machine can compute. This is an empirical claim about the laws of
 *   physics, not a mathematical definition or an epistemological boundary. If
 *   true, it is a Mountain — a natural law that would persist regardless of
 *   human institutions. However, the thesis functions socially as a
 *   constraint that channels research funding, career advancement, and
 *   publication acceptance away from hypercomputation and non-standard models
 *   of computation. Mainstream theoretical computer science, the quantum
 *   computing establishment, and funding agencies benefit from the thesis's
 *   dominance (it stabilizes the theoretical foundation of their field and
 *   justifies resource allocation). Hypercomputation researchers and
 *   non-standard computation theorists bear the cost: their work is
 *   marginalized, unfunded, and treated as pseudoscience. The constraint's
 *   extractiveness and suppression have risen gradually as the field
 *   professionalized and quantum computing raised the stakes of what counts
 *   as 'physical computation.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.45).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, mountain).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis as Physical Law").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "foundations_of_computer_science/philosophy_of_physics").

domain_priors:emerges_naturally(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '5776974a-f7ff-4aef-88aa-3df641acfcb7').
narrative_ontology:cs_kernel_codification('5776974a-f7ff-4aef-88aa-3df641acfcb7', formalized).
narrative_ontology:cs_authority_grounding('5776974a-f7ff-4aef-88aa-3df641acfcb7', expertise).
narrative_ontology:cs_interpretation_layer_present('5776974a-f7ff-4aef-88aa-3df641acfcb7').
narrative_ontology:cs_reading_relation('5776974a-f7ff-4aef-88aa-3df641acfcb7', church_turing_thesis__mathematical_definition_reading, forecloses).
narrative_ontology:cs_reading_relation('5776974a-f7ff-4aef-88aa-3df641acfcb7', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('5776974a-f7ff-4aef-88aa-3df641acfcb7', foundational, physical_processes_bounded_by_turing_computability).
narrative_ontology:cs_axiom_status(physical_processes_bounded_by_turing_computability, holdable).
narrative_ontology:cs_axiom_grounding('5776974a-f7ff-4aef-88aa-3df641acfcb7', physical_processes_bounded_by_turing_computability, empirically_contingent).
narrative_ontology:cs_reference_frame('5776974a-f7ff-4aef-88aa-3df641acfcb7', classical_ct_physical_law).
narrative_ontology:cs_drift_state('5776974a-f7ff-4aef-88aa-3df641acfcb7', contemporary_quantum_computing_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('5776974a-f7ff-4aef-88aa-3df641acfcb7', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_theoretical_cs).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, quantum_computing_establishment).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, funding_agencies).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, non_standard_computation_theorists).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_machine_universality).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, physical_computability_equals_turing_computability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research hypercomputation models (infinite time Turing machines, analog computation, relativistic computation). Their work is systematically excluded from top venues, denied funding, and treated as fringe. Exit means abandoning their research program and switching to mainstream topics — a career-costly move. They bear the full suppression cost of the thesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Develop computational models beyond Turing machines (e.g., Blum-Shub-Smale machines, oracle machines, infinite-state systems). They face the same structural exclusion as hypercomputation researchers but with slightly more mathematical legitimacy. Their exit options are similarly constrained by career investment.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, non_standard_computation_theorists, payer,
    moderate, biographical, constrained, global).

% Define the field's foundations, control peer review and curricula, and set the research agenda. The thesis provides the stable theoretical bedrock that makes the discipline coherent and fundable. They benefit from the thesis's dominance (stable paradigm, clear boundaries) and have high exit options (could pivot to other foundations if thesis fell).
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_theoretical_cs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, mainstream_theoretical_cs, beneficiary).

% Build quantum computers and develop quantum algorithms. The thesis validates their enterprise as the 'physical realization of Turing computation' — the ultimate frontier. They benefit from the thesis's authority (funding, legitimacy) but would adapt quickly if the thesis were falsified (quantum computing would still have practical value).
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_computing_establishment, beneficiary,
    institutional, generational, mobile, global).

% Allocate research funding based on peer review and strategic priorities. The thesis provides a clear criterion: fund Turing-computable research; reject hypercomputation. They benefit from a stable evaluation framework. If the thesis fell, they would face a more complex funding landscape but have the bureaucratic capacity to adapt.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, funding_agencies, agenda_setter,
    institutional, biographical, arbitrage, national).

% Investigate the physical limits of computation as an empirical question. They use the thesis as a working hypothesis but are not institutionally bound to it. If evidence of hypercomputation appeared in physics (e.g., closed timelike curves, infinite energy densities), they would follow the evidence. Their seat is analytical: they see the full structure without bearing its costs or collecting its benefits.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physicists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, universally accepted definition of 'computation' that allows theoretical computer science, programming language design, and complexity theory to build on a shared foundation without re-litigating what counts as an algorithm.
% TRANSFER_FUNCTION: Channels research funding, publication slots, academic positions, and epistemic legitimacy from non-Turing research programs to mainstream Turing-machine-based research. The transfer is not monetary but career-structural: hypercomputation researchers pay with marginalization; mainstream researchers collect with stable career paths.
% ABSENT_VOICES: Researchers in institutions or regions where hypercomputation is taken seriously (e.g., some Eastern European and Latin American theory groups) are structurally excluded from the dominant Western CS publication and funding ecosystem. Their objections — that the thesis is unproven and empirically contestable — are not heard in the venues that set the field's agenda.
% DISAPPEARANCE_RATIONALE: If the thesis vanished overnight, theoretical CS would lose its foundational definition of computation. Hypercomputation research would flood mainstream venues, funding priorities would shift, quantum computing's theoretical framing would need revision, and the entire curriculum from undergraduate to graduate level would require restructuring. The field would reorganize around a pluralistic computability landscape.
% FOUNDING_PROBLEM: In the 1930s, multiple formalizations of 'effective computability' (Turing machines, lambda calculus, general recursive functions) were shown equivalent. The founding problem was: does this equivalence capture the intuitive notion of what a human calculator can do? The thesis answered 'yes' and provided a single foundation for the nascent field of computer science.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream CS (textbooks, curricula, ACM/IEEE guidelines) attests the problem is solved (dead). Hypercomputation researchers (Copeland, Siegelmann, Ord, Beggs & Tucker) and some philosophers of physics (Pitowsky, Welch) attest the problem remains open (live) — the equivalence of formalisms does not prove physical adequacy. No independent arbiter has settled the dispute; the contestation is structural.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

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
 *   The thesis claims Mountain status (emerges_naturally: true) but declares beneficiaries and victims, triggering False Summit Mountain evaluation. Extractiveness (0.35) reflects the research-funding and career-path channeling, not direct resource extraction. Suppression (0.45) reflects the structural illegitimacy of non-Turing research programs — papers are rejected, grants denied, careers stalled — without active policing. Theater ratio is low (0.15) because the community genuinely believes the thesis; the constraint is not performative. Accessibility collapse (0.75) is high but not absolute: hypercomputation research persists at margins, and quantum supremacy claims periodically test the boundary. Resistance (0.55) is moderate: a persistent minority research program exists but lacks institutional footholds.
 *
 * PERSPECTIVAL GAP:
 *   From the mainstream CS seat, the thesis is a Mountain — a discovered truth that organizes the field. From the hypercomputation researcher seat, it functions as a Snare — a claim enforced through institutional gatekeeping that suppresses alternatives without empirical finality. The engine computes this divergence from the structural data: same constraint, different seats, different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream theoretical CS and funding agencies are structural beneficiaries (d near 0): they control the theoretical framework, peer review, and resource allocation that the thesis stabilizes. Quantum computing establishment is a secondary beneficiary (d ~0.2): the thesis validates their research as the 'frontier' without threatening the foundation. Hypercomputation researchers are full targets (d near 1): they bear the full cost of marginalization with constrained exit (cannot switch fields without career loss). Non-standard computation theorists are similarly targeted. Physicists sit near analytical (d ~0.5): they use the thesis as a working hypothesis but would follow evidence if a physical hypercomputer were demonstrated.
 *
 * MANDATROPHY ANALYSIS:
 *   The thesis was founded to answer 'what is effectively computable?' — a live problem in the 1930s. That founding problem is now contested: mainstream CS says it's solved (dead); hypercomputation researchers say it's open (live). The thesis persists not because the founding problem remains live, but because an entire institutional edifice (curricula, funding, publication norms, quantum computing roadmaps) has been built on it. Mandatrophy is unresolved: the constraint's mandate (define computability) has arguably been achieved, but the constraint remains as the field's constitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the Church-Turing thesis a genuine natural law (Mountain) or a constructed constraint that benefits identifiable institutional actors (False Summit)?',
    'Empirical discovery of a physical hypercomputer would confirm natural law status (Mountain). Absent such discovery, sociological analysis of whether the thesis''s dominance is maintained by evidence or by institutional inertia and benefit-capture would resolve.',
    'If natural law, the constraint is a genuine Mountain and the beneficiaries are incidental. If constructed, FSM reclassifies to tangled_rope (coordination + extraction) or snare (pure extraction), and the victim structure becomes the primary classification signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, empirical, 'Whether the thesis''s Mountain status is genuine or a false summit maintained by beneficiary capture.').

omega_variable(
    quantum_supremacy_falsification_threshold,
    'What quantum computational result would constitute a falsification of the physical Church-Turing thesis?',
    'Consensus among physicists and computer scientists on whether a demonstrated quantum advantage exceeds Turing-computable functions (e.g., solves the halting problem) or merely provides speedup within Turing-computable bounds.',
    'If quantum supremacy is accepted as within Turing bounds, the thesis survives unchallenged. If a result is accepted as hypercomputation, the thesis is falsified and the constraint dissolves (or transforms into a historical artifact).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_supremacy_falsification_threshold, conceptual, 'Whether quantum computing advances threaten the thesis''s empirical claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__physical_claim_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__physical_claim_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__physical_claim_reading, theater_ratio, 90, 0.15).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__physical_claim_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__physical_claim_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__physical_claim_reading, base_extractiveness, 90, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__physical_claim_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__physical_claim_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(chur_su_t90, church_turing_thesis__physical_claim_reading, suppression_requirement, 90, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.02).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% Part of the church_turing_thesis constraint family. This reading (physical_claim) treats the thesis as an empirical hypothesis about physics. The mathematical_definition_reading treats it as a stipulative definition. The epistemological_boundary_reading treats it as a limit of formal knowledge. They share the same kernel but instantiate different constraints with different ε, beneficiaries, and victims. This reading's ε is moderate (empirically contestable); the mathematical reading's ε is near zero (true by convention); the epistemological reading's ε is low (formal boundary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, institutional, 0.1).
constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
