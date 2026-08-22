% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe as Irreducible Competence Anchor
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability domains (aviation, nuclear, surgery, maritime) maintain
 *   a doctrine that only real catastrophic events or near-misses provide the
 *   'irreducible exercise' that keeps operator competence from atrophying.
 *   This reading of the competence_exercise_requirement kernel treats
 *   simulation as fundamentally insufficient — a map that cannot replace the
 *   territory. The constraint coordinates readiness (genuine function) while
 *   extracting risk onto frontline operators and the public (asymmetric
 *   extraction), and requires active enforcement through hour requirements,
 *   certification gates, and cultural gatekeeping that dismiss
 *   simulation-first pathways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.55).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe as Irreducible Competence Anchor").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'd8320d41-69aa-4854-a79c-11021354d4d6').
narrative_ontology:cs_kernel_codification('d8320d41-69aa-4854-a79c-11021354d4d6', implicit).
narrative_ontology:cs_authority_grounding('d8320d41-69aa-4854-a79c-11021354d4d6', practice).
narrative_ontology:cs_interpretation_layer_present('d8320d41-69aa-4854-a79c-11021354d4d6').
narrative_ontology:cs_reading_relation('d8320d41-69aa-4854-a79c-11021354d4d6', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('d8320d41-69aa-4854-a79c-11021354d4d6', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('d8320d41-69aa-4854-a79c-11021354d4d6', foundational, only_real_jeopardy_forges_competence).
narrative_ontology:cs_axiom_status(only_real_jeopardy_forges_competence, holdable).
narrative_ontology:cs_axiom_grounding('d8320d41-69aa-4854-a79c-11021354d4d6', only_real_jeopardy_forges_competence, empirically_contingent).
narrative_ontology:cs_axiom('d8320d41-69aa-4854-a79c-11021354d4d6', secondary, simulation_cannot_replicate_neurophysiological_load).
narrative_ontology:cs_axiom_status(simulation_cannot_replicate_neurophysiological_load, holdable).
narrative_ontology:cs_axiom_grounding('d8320d41-69aa-4854-a79c-11021354d4d6', simulation_cannot_replicate_neurophysiological_load, empirically_contingent).
narrative_ontology:cs_reference_frame('d8320d41-69aa-4854-a79c-11021354d4d6', apprenticeship_through_crisis).
narrative_ontology:cs_drift_state('d8320d41-69aa-4854-a79c-11021354d4d6', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d8320d41-69aa-4854-a79c-11021354d4d6', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, legacy_operator_leadership).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_practitioners).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_trust_in_safety_systems).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, irreducible_experience_doctrine).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, muscle_memory_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate minimum hour/experience requirements and certification standards that privilege actual operational time over simulation. Benefit from the doctrine that real events are the only true test, which justifies their regulatory scope and inspection regimes. Can shift standards by rulemaking but face political cost if they reduce experience requirements after a catastrophe.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_authorities, beneficiary).

% Senior pilots/operators who earned their qualifications through the old 'blood and sweat' pathway. The catastrophe-necessary framing validates their career capital and gatekeeping authority. Resist simulation-first pathways that would devalue their lived experience and lower barriers for new entrants.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, legacy_operator_leadership, beneficiary,
    powerful, biographical, mobile, regional).

% Bear the risk of being the ones on duty when the 'irreducible exercise' arrives. Pay with their lives and licenses when competence decay is revealed by catastrophe. Training budgets and schedule pressure are justified by the claim that only real operations count; simulation time is treated as secondary. Exit requires leaving the profession or moving to a less regulated domain.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators, payer,
    organized, biographical, constrained, national).

% Enter a system where the only path to full competence is surviving the events that kill people. Pay in extended low-autonomy apprenticeship, higher accident exposure during the 'experience accumulation' phase, and career stagnation behind seniority gates. Cannot exit the constraint without abandoning the career; simulation-heavy training programs are dismissed as 'not real' by the culture.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_practitioners, payer,
    powerless, biographical, trapped, national).

% Pays the ultimate cost when the 'necessary catastrophe' occurs — lives lost, trust shattered, political legitimacy damaged. The constraint treats public harm as the calibration signal for operator competence. No exit: the public cannot opt out of depending on high-reliability systems (aviation, nuclear, medical).
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, public_trust_in_safety_systems, payer,
    powerless, generational, trapped, national).

% Develop high-fidelity simulators, synthetic environments, and debriefing tools that could substitute for some real-world exposure. Their evidence that simulation transfers to performance is systematically discounted by the 'muscle memory' doctrine. Excluded from the competence definition; their products are treated as supplementary, never sufficient.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_technology_vendors, excluded,
    moderate, biographical, mobile, global).

% Study the transfer gap between simulation and operational reality, the decay curves of competence without exercise, and the organizational dynamics that treat catastrophe as a learning opportunity rather than a systemic failure. See the full structure: the coordination function (maintaining readiness) and the extraction function (risk externalized to juniors and public).
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizational_learning_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that operators in high-reliability domains maintain the deep, embodied competence needed to handle novel, high-stakes situations — the kind that procedures and simulations cannot fully anticipate. The 'real event' anchor prevents procedural drift and simulator complacency.
% TRANSFER_FUNCTION: Transfers catastrophic risk from the institution (which would pay for more simulation, more supervision, more redundant design) to frontline operators and the public. The institution avoids the cost of continuous high-intensity training by accepting that periodic catastrophes will 'refresh' the competence pool. The gains (reduced training budget, sustained operational tempo, seniority prestige) accrue to regulatory authorities and legacy leadership.
% ABSENT_VOICES: The victims of the 'necessary catastrophes' — passengers, patients, communities downwind — are structurally absent from the competence definition. Their harm is the metric by which the system calibrates itself. Simulation vendors and human-factors researchers who demonstrate transfer are excluded from the authoritative standard-setting process.
% DISAPPEARANCE_RATIONALE: If the doctrine that only real catastrophe maintains competence vanished, training regimes would shift decisively toward high-fidelity simulation, synthetic scenario generation, and non-jeopardy operational audits. Regulatory hour requirements would be replaced by demonstrated proficiency metrics. The 'experience gate' for senior roles would collapse. The safety economics would invert: institutions would pay for continuous high-intensity preparation instead of externalizing risk to the public.
% FOUNDING_PROBLEM: Early aviation and nuclear operations discovered that pilots and operators who trained only on procedures and low-fidelity simulators failed catastrophically when confronted with novel, high-stress situations. The 'muscle memory' forged in actual crisis proved irreplaceable. The founding problem: how to maintain readiness for the unprecedented without suffering the unprecedented.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early HRO literature (Weick, Rochlin, La Porte) and accident investigations (Three Mile Island, early aviation hull losses). However, the claim that this problem REMAINS unsolvable except by catastrophe is contested: modern simulation fidelity, line-oriented flight training (LOFT), and synthetic training environments demonstrate substantial transfer. The corroboration for 'still live' comes from regulatory bodies and senior operator groups; the corroboration for 'substantially solved' comes from simulation validation studies and military/adopted synthetic training programs — sources outside the beneficiary set.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the constraint externalizes the cost of competence maintenance onto those who suffer the catastrophes and the novices who must survive the gauntlet. Suppression (0.55) is moderate: alternatives (high-fidelity simulation, synthetic training) exist and are used, but are structurally prevented from substituting for the 'real thing' by regulation and culture. Theater ratio (0.42) is elevated: the 'safety culture' rhetoric and simulation investment are real but increasingly performative relative to the doctrine's core claim. The rising extractiveness and theater over the interval reflect the growing gap between simulation capability and regulatory recognition.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/legacy seat, the constraint is a necessary coordination mechanism — the only thing that prevents 'simulator pilots' from taking command. From the novice/public seat, it is a risk-transfer machine that treats their bodies as the calibration instrument. The engine computes this divergence from the structural power/exit asymmetry. The analytical observer sees both: a genuine coordination problem (maintaining readiness for the unprecedented) solved by an extractive structure (catastrophe as the only accepted test).
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities and legacy leadership are structural beneficiaries (d near 0.1-0.2): they collect regulatory authority, career validation, and reduced training costs. Frontline operators, novices, and the public are targets (d near 0.7-0.9): they bear the risk, the apprenticeship burden, and the harm. Simulation vendors are excluded — their exclusion IS the enforcement mechanism. The organized power of frontline operators (unions, professional bodies) gives them some voice but exit remains constrained by domain specificity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining readiness for novel crises) remains live, but the claim that ONLY catastrophe solves it is the mandatrophic element. The coordination function (readiness maintenance) has been partially captured by the extraction function (risk externalization, seniority rent). The constraint persists not because alternatives don't exist, but because the beneficiaries control the definition of 'competence' and the cost of changing the definition is borne by others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_ceiling,
    'What is the true ceiling of simulation-to-operational transfer for novel, high-stakes scenarios? Can simulation ever fully substitute for the neurophysiological and cognitive load of real jeopardy?',
    'Longitudinal studies of operators trained primarily in synthetic environments vs. traditional pathways, measuring performance on novel emergencies. Military synthetic training programs (e.g., USAF Pilot Training Next, Navy simulation-heavy pipelines) provide natural experiments.',
    'If transfer ceiling is high, the ''irreducible exercise'' claim is falsified and the constraint''s coordination function is separable from its extraction function — reclassification toward rope or scaffold. If ceiling is low, the claim holds and the tangled_rope structure is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_ceiling, empirical, 'Whether the core empirical premise of this reading (simulation insufficiency) is true.').

omega_variable(
    catastrophe_learning_efficiency,
    'Do catastrophes actually produce net competence gain at the system level, or do they primarily destroy the competent and leave survivors with trauma-degraded performance?',
    'Post-accident organizational competence trajectories: compare units/organizations that suffered catastrophes vs. matched controls on subsequent safety performance, controlling for institutional reforms.',
    'If catastrophes degrade system competence more than they refresh it, the coordination function is illusory — the constraint is a snare masquerading as a tangled rope. If they produce net gain, the coordination function is real but the extraction distribution remains contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_learning_efficiency, empirical, 'Whether the constraint''s stated coordination function (competence maintenance via catastrophe) actually works.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the competence_exercise_requirement kernel best framed as ''maintaining readiness for the unprecedented'' (this reading''s frame) or ''validating competence claims for institutional legitimacy'' (a legitimacy-maintenance frame)?',
    'Trace the institutional history: when simulation capability improved, did the ''experience hour'' requirements decrease proportionally, or were they maintained/increased? If maintained, the kernel serves legitimacy more than readiness.',
    'If the legitimacy-maintenance frame better predicts institutional behavior, this reading''s claimed coordination function is a cover story. The constraint would reclassify toward snare. The conceptual framing determines which structural elements are visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s declared purpose matches its institutional function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.3).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.35).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.38).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 32, 0.4).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.08).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).

% DUAL FORMULATION NOTE:
% This is the catastrophe_as_necessary_anchor reading of the competence_exercise_requirement kernel. It decomposes the kernel into three structurally distinct constraints with different ε values and beneficiary/victim structures. The simulation_as_adequate_exercise reading has low ε (rope/scaffold); the hybrid_dependency reading has moderate ε (tangled_rope with different victim distribution); this reading has high ε because it treats public harm as the calibration signal. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, organized, 0.65).
constraint_indexing:directionality_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
