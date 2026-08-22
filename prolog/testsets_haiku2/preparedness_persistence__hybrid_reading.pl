% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Preparedness: Competent Engineering + Ritualized Drills
 *   domain: institutional/disaster_governance
 *
 * SUMMARY:
 *   Disaster preparedness operates as a stratified system: critical
 *   infrastructure inspection (engineering audits, equipment certification)
 *   maintains genuine technical competence and receives sustained funding and
 *   expert staffing. Procedural preparedness (evacuation drills,
 *   communication rehearsals) receives recurring administrative cycles but
 *   limited operational depth, creating a system where form persists while
 *   competence fragments. This hybrid reading treats preparedness as neither
 *   uniformly competent (competence_reading) nor uniformly hollow
 *   (husk_reading), but as a constraint that coordinates legitimate
 *   institutional necessity (real engineering oversight) with extractive
 *   institutional legitimacy (visible drills sustaining the appearance of
 *   total readiness). The constraint's persistence depends on active
 *   suppression of the transparency that would expose the stratification:
 *   administrators have incentive to maintain the fiction of uniform
 *   preparation; frontline personnel learn not to voice the gap; vulnerable
 *   populations are excluded from the design that determines which systems
 *   they can actually rely on.
 *
 * KEY AGENTS:
 *   - institutional_administrators: institutional power, arbitrage exit — set policy and allocate resources differentially across preparedness sectors
 *   - engineering_inspection_corps: organized power, constrained exit — maintain genuine competence in critical systems; benefit from budget protection and professional vindication
 *   - frontline_emergency_personnel: moderate power, trapped exit — must execute both competent and ritualized procedures; excluded from design; bear operational risk when gaps appear
 *   - vulnerable_populations: powerless, immediate horizon, trapped exit — depend on preparedness working in practice; experience actual competence gaps most acutely
 *   - risk_control_advocates: organized power, constrained exit — benefit from visible preparedness narratives and cycle of drills even when operationally shallow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.67).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Preparedness: Competent Engineering + Ritualized Drills").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/disaster_governance").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'c3a43d44-03ec-49a9-a412-ce30f25ab43b').
narrative_ontology:cs_kernel_codification('c3a43d44-03ec-49a9-a412-ce30f25ab43b', fixed_text).
narrative_ontology:cs_authority_grounding('c3a43d44-03ec-49a9-a412-ce30f25ab43b', extraction).
narrative_ontology:cs_interpretation_layer_present('c3a43d44-03ec-49a9-a412-ce30f25ab43b').
narrative_ontology:cs_reading_relation('c3a43d44-03ec-49a9-a412-ce30f25ab43b', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3a43d44-03ec-49a9-a412-ce30f25ab43b', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('c3a43d44-03ec-49a9-a412-ce30f25ab43b', foundational, preparedness_function_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_function_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('c3a43d44-03ec-49a9-a412-ce30f25ab43b', preparedness_function_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('c3a43d44-03ec-49a9-a412-ce30f25ab43b', foundational, institutional_legitimacy_decoupled_from_uniform_competence).
narrative_ontology:cs_axiom_status(institutional_legitimacy_decoupled_from_uniform_competence, holdable).
narrative_ontology:cs_axiom_grounding('c3a43d44-03ec-49a9-a412-ce30f25ab43b', institutional_legitimacy_decoupled_from_uniform_competence, deontological).
narrative_ontology:cs_reference_frame('c3a43d44-03ec-49a9-a412-ce30f25ab43b', uniform_preparedness_commitment).
narrative_ontology:cs_drift_state('c3a43d44-03ec-49a9-a412-ce30f25ab43b', contemporary_resource_constraint_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3a43d44-03ec-49a9-a412-ce30f25ab43b', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, risk_control_advocates).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_emergency_personnel).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspection_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce preparedness protocols across departments. They allocate budgets differentially: engineering inspection receives sustained funding and technical staffing; evacuation drills receive recurring administrative overhead but limited operational investment. They justify the split as pragmatic triage — critical infrastructure inspection is non-negotiable; mass-notification rehearsal achieves legitimacy through visible iteration rather than technical depth. They benefit from the constraint's appearance of systematic readiness without the cost of universal competence maintenance.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, institutional_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Maintains real competence through regular training, field testing, and equipment certification. Their work is recognized as technically necessary and receives budget protection. They operate under a constraint that validates and funds their practice as essential; the constraint's persistence strengthens their institutional position and professional claim to irreplaceability.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspection_corps, beneficiary,
    organized, biographical, constrained, national).

% Required to execute evacuation procedures that may not have been realistically rehearsed or may have degraded since the last drill. They bear the operational risk when actual events unfold and discover that competence was not maintained in their sector. Their exclusion from preparedness design means their feedback on unrealistic or atrophied procedures rarely feeds back into protocol revision.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_emergency_personnel, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, frontline_emergency_personnel, excluded).

% Depend on emergency protocols working in practice. They have no role in designing which systems remain competent and which are ritualized. In an actual event, they experience the gap: evacuations that lack current muscle memory, communication systems that weren't operationally tested, personnel who follow memorized scripts rather than practiced response. Their exposure to the actual performance gap is highest and their recourse is lowest.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, vulnerable_populations, payer,
    powerless, immediate, trapped, national).

% Promote preparedness narratives and visible safety culture. They benefit from the constraint's persistence: each maintained engineering inspection and conducted drill vindicates their advocacy, even when the drills are theatrical. The constraint allows them to claim victory (preparations are happening) without exposing the competence gaps that would demand deeper institutional change.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, risk_control_advocates, beneficiary,
    organized, generational, constrained, national).

% Oversight bodies and auditors see the constraint from outside: they track whether resources actually improve outcomes or merely generate the appearance of preparation. They can measure the stratification (which systems get real investment, which get symbolic iteration) but often lack enforcement power to mandate equalizing competence across all preparedness sectors.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, budget_constraint_actors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified institutional preparedness posture that satisfies regulatory and public-confidence requirements while distributing constrained technical resources to highest-criticality systems (engineering infrastructure). The constraint coordinates a set of mandatory practices — drills, inspections, plan revisions — that create a coherent disaster-management narrative.
% TRANSFER_FUNCTION: Moves institutional legitimacy (the appearance of systematic readiness) and risk-mitigation credit from areas of real technical competence (engineering inspection) to areas of ritualized procedure (evacuation drills). In reverse, it extracts from vulnerable populations and frontline personnel the assumption that visible preparation equals operational readiness, when the actual readiness is stratified.
% ABSENT_VOICES: Disaster survivors from past events who could attest to competence gaps; operational personnel in lower-resourced preparedness sectors who know the procedures have decayed; vulnerability researchers who would document which populations face the largest gap between drill realism and actual response capacity.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if administrators were forced to abandon the fiction of uniform preparedness and instead transparently maintain competence only in funded sectors while suspending mandatory-but-unresourced procedures — institutional legitimacy would fracture, public confidence would drop, but resource allocation would realign to match actual capacity. Some populations would lose the symbolic assurance of preparation (harmful in itself, but honest); funding would likely concentrate further on visible engineering assets, or alternatively shift to the neglected sectors if the competence gap became publicly undeniable.
% FOUNDING_PROBLEM: Early in disaster management history, periodic catastrophic events exposed institutional unpreparedness and failures of communication and coordination. Preparedness programs were built to maintain readiness across all critical systems through regular drills, inspections, and plan refinement.
% FOUNDING_PROBLEM_CORROBORATION: Administrators attest the founding problem is still live: disasters remain possible and continuous preparation is necessary. Operational personnel and disaster researchers attest that the founding problem has PARTIALLY shifted: engineering readiness is maintained, but procedural readiness (especially for mass-casualty or complex multi-sector scenarios) has degraded into symbolic iteration. No corroborating source outside the benefiting parties attests that the current constraint solves the original problem — corroboration, if present, comes from those who benefit from the appearance of solution.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58 at interval end) reflects that the constraint extracts institutional legitimacy and public confidence from the assumption of uniform readiness, when actual readiness is stratified — this is a net transfer from those who depend on competence to those who manage appearances. Suppression is high (0.67) because the constraint's persistence depends on keeping the stratification invisible: administrators suppress transparency about which systems are truly competent; personnel suppress their own knowledge of the gaps (identity-lock: speaking the gap risks appearing incompetent or demoralizing); the public is conditioned to interpret visible procedure (drills) as evidence of readiness. Theater ratio rises to 0.61–0.65 in the middle interval (points 20–30) when the constraint's extractive function peaks — this is the period of maximum drill iteration with minimal operational learning, maximum appearance-maintenance. The slight decline at point 40 reflects a minor shift after visible failures (post-event accountability pressure), which briefly increases calls for genuine competence-building before the constraint re-stabilizes around the stratified equilibrium. All metrics are authored on a single shared time grid (0, 5, 10, 20, 30, 40) enabling temporal alignment.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional administrator's seat, preparedness is a pragmatic coordination problem: maximize readiness where it matters most (critical infrastructure) while maintaining institutional legitimacy through visible procedures where budget is constrained. From the frontline personnel and vulnerable population seats, the same arrangement is experienced as extractive: they depend on competence-as-advertised, but receive competence-as-funded, which is stratified and hidden. The engine should compute substantially different classifications at these seats from the same structural data — the administrator seat seeing coordination (beneficiary, d near 0.0) and the payer seats seeing extraction (targets, d near 1.0). This divergence is the measurement the hybrid reading makes visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional administrators are beneficiaries (d ≈ 0.15): they collect institutional legitimacy and risk-mitigation credit; their exit is arbitrage-level (can move to other institutions or shift strategies). Engineering personnel are partial beneficiaries (d ≈ 0.25): they receive budget protection and professional vindication from the constraint; their competence work is real and valued, though it also becomes a vehicle for the constraint's broader extraction. Frontline emergency personnel are targets (d ≈ 0.75): they bear operational risk and are trapped by employment; their exclusion from design and suppression of their feedback about realistic competence gaps are structural features of the constraint. Vulnerable populations are full targets (d ≈ 0.95): powerless, immediate time horizon, trapped spatially, with zero voice in design; they experience the competence gap without recourse. Risk-control advocates are beneficiaries (d ≈ 0.20): they collect advocacy success and institutional legitimacy from the visible constraint; their time horizon is long and their exit options constrained by institutional position. Directionality overrides are not needed: the structural derivation (beneficiary/victim + power + exit) produces appropriate d values without manual correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids both false naturalism and false hollowness. It is NOT a mountain: preparedness is a human commitment (founded, maintained, contestable), not a natural law; extractiveness and suppression are non-negligible. It is NOT pure piton (husk): engineering inspection maintains genuine competence and serves a real function; the constraint is not entirely theatrical. The tangled_rope classification captures the structure: there IS a genuine coordination function (maintaining institutional preparedness readiness in critical systems), AND there IS asymmetric extraction (institutional legitimacy and risk-mitigation credit flowing to administrators and advocates; risk and competence-gap exposure flowing to personnel and vulnerable populations). The active enforcement (suppression) is the mechanism that keeps the stratification invisible — without suppression of transparency, the gap would become visible and the constraint would either transform (all systems brought to uniform competence, dissolving the asymmetry) or dissolve (preparedness narratives abandoned, institutions lose legitimacy cover). Mandatrophy does not resolve here: the founding problem (maintain readiness across critical systems) is still live, but the constraint's method (stratified competence + uniform appearance) is decoupled from the problem's solution (actual operational readiness where it matters). The constraint persists because decoupling is more institutionally convenient than solving the underlying problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engineering_competence_maintenance_mechanism,
    'Are engineering inspection systems genuinely competent because funding and expertise are maintained, or because the high-consequence nature of infrastructure failure creates non-negotiable accountability pressure that transcends the resource allocation game?',
    'Comparative analysis of infrastructure inspection performance in jurisdictions with different budget levels; post-failure audits of whether competence gap or execution failure caused the failure.',
    'If funding is the driver, removing budget would degrade engineering competence toward the husk pattern. If accountability is the driver, competence would persist even under budget constraint. The distinction matters for understanding whether the stratification is imposed (resource-driven) or structural (consequence-driven).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_competence_maintenance_mechanism, empirical, 'What sustains competence in the engineering subsystem.').

omega_variable(
    ritual_drill_atrophy_vs_learning,
    'Are evacuation drills and communication rehearsals atrophying into ritual (husk pattern) or are they generating distributed competence that only appears shallow because it is not measured at the individual-muscle-memory level?',
    'Controlled comparison of actual evacuation performance between populations that have received recent operationally-deep drills (smaller-scale, high-iteration, feedback-integrated) versus populations that have received standard-format drills (large-scale, annual, symbolic). Post-event performance data.',
    'If drills genuinely atrophy, theater_ratio should rise over time and actual evacuation performance should degrade. If drills maintain distributed competence, theater_ratio should stabilize and actual performance should remain steady. The interval measurements show theater_ratio rising to 0.61–0.65, which suggests atrophy, but the evidence is not dispositive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_drill_atrophy_vs_learning, empirical, 'Whether procedural drills maintain competence or degrade into theater.').

omega_variable(
    stratification_as_rational_triage_vs_institutional_capture,
    'Is the stratification of preparedness (high competence in engineering, lower in procedures) a rational triage decision reflecting genuine constraint (limited budget, highest-consequence systems prioritized), or is it institutional capture (administrators extract legitimacy by maintaining visible procedure while starving non-capture-resistant sectors)?',
    'Budget allocation analysis over time; interviews with administrators and oversight bodies about prioritization rationale; comparison with peer institutions that maintain more uniform preparedness levels despite similar budget constraints.',
    'If rational triage, the constraint is a mountain (unavoidable given scarcity); if capture, it is snare-flavored (extractive, alternatives suppressed). The hybrid reading treats it as tangled_rope, implying both coordination (real triage) and extraction (legitimacy capture). Evidence of selective budget cuts in non-symbolic sectors while maintaining symbolic procedure would support the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_as_rational_triage_vs_institutional_capture, conceptual, 'Whether stratification reflects rational resource allocation or institutional legitimacy extraction.').

omega_variable(
    kernel_committer_ambiguity_competence_vs_husk_vs_hybrid,
    'Which reading of the preparedness_persistence kernel is structurally true: do drills and inspections maintain live competence (competence_reading), generate hollow ritual (husk_reading), or operate in stratified fashion with competence in some sectors and ritual in others (hybrid_reading)?',
    'Longitudinal competence testing: periodic independent assessment of actual evacuation capability, communication system functionality, coordination response timing across multiple preparedness sectors. The engine computes this reading against empirical performance; sibling readings would compute differently from the same data.',
    'If competence_reading is correct, base_extractiveness should be lower (~0.25–0.35) and theater_ratio should be lower (~0.20–0.35); classification should shift toward rope. If husk_reading is correct, theater_ratio should be higher (~0.75+) and accessibility_collapse should be higher (~0.75+); classification should shift toward piton. This reading''s metrics (extractiveness 0.58, theater_ratio 0.61) are consistent with stratification — some subsystems competent, others ritual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_ambiguity_competence_vs_husk_vs_hybrid, empirical, 'Kernel ambiguity: which reading correctly describes preparedness persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__hybrid_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__hybrid_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__hybrid_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__hybrid_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__hybrid_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__hybrid_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__hybrid_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__hybrid_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__hybrid_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence__hybrid_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel is instantiated by three distinct readings: competence_reading (preparedness maintains live competence through practice), husk_reading (preparedness is hollow ritual), and hybrid_reading (preparedness is stratified with genuine competence in engineering inspection and ritual in procedural drills). Each reading authoring yields a different constraint with different ε, different beneficiary/victim structure, and different type classification. The three constraints are linked via network.affects_constraints as a constraint family. The disagreement is empirically located: whether drills and inspections maintain competence, degrade into hollow iteration, or operate differently across subsystems. Each reading's metrics and stakeholder structure are authored independently from that reading's frame; the engine computes each seat's perspective per-reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
