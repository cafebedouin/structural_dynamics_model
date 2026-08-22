% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Simulation Refresh Requirement for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety regulators mandate continuous simulation-based competence
 *   validation for critical-risk personnel (pilots, nuclear operators, hazmat
 *   handlers, etc.), claiming that competence degrades through disuse and
 *   periodic testing is insufficient. Operational personnel and training
 *   administrators bear the cost of continuous cycles. The constraint
 *   simultaneously provides genuine safety coordination (establishes shared
 *   standards for competence validation) and extracts time and administrative
 *   burden while expanding regulatory surveillance. The foundational claim of
 *   this reading is that competence retention is PROCESS-DEPENDENT —
 *   continuous exercise is structurally necessary because skill decay is
 *   continuous. The sibling reading 'simulation_as_proxy' treats simulation
 *   as ONE adequate form of exercise (role-playing catastrophe). The sibling
 *   reading 'real_catastrophe_only' treats only real incidents as sufficient
 *   exercise. This reading's empirical claim differs from its siblings: that
 *   the continuous rhythm itself—not just the existence of exercise—is what
 *   retention requires.
 *
 * KEY AGENTS:
 *   - safety_regulators: Institutional agenda-setter. Sets the validation standard, enforces via audit and licensing. Collects regulatory authority from the rule. Benefits from being seen as preventing catastrophes.
 *   - operational_personnel: Moderate-power payers. Must participate in recurring cycles. Identity-locked to the profession; cannot exit without career cost. Face continuous competence re-examination.
 *   - training_administrators: Moderate-power payers. Bear operational and scheduling costs. Their function is justified by continuous-refresh requirement.
 *   - institutional_risk_managers: Institutional beneficiaries. Adopt the standard as their own risk mitigation. Compliance defends them against liability.
 *   - real_incident_data_holders: Analytical observers. Possess empirical records of what actually causes catastrophes. Could resolve the reading contest if consulted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.61).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.54).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.61).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Simulation Refresh Requirement for Competence Retention").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '7780a566-82af-40a0-a185-27081039d99e').
narrative_ontology:cs_kernel_codification('7780a566-82af-40a0-a185-27081039d99e', fixed_text).
narrative_ontology:cs_authority_grounding('7780a566-82af-40a0-a185-27081039d99e', extraction).
narrative_ontology:cs_interpretation_layer_present('7780a566-82af-40a0-a185-27081039d99e').
narrative_ontology:cs_reading_relation('7780a566-82af-40a0-a185-27081039d99e', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('7780a566-82af-40a0-a185-27081039d99e', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('7780a566-82af-40a0-a185-27081039d99e', foundational, competence_is_process_dependent).
narrative_ontology:cs_axiom_status(competence_is_process_dependent, holdable).
narrative_ontology:cs_axiom_grounding('7780a566-82af-40a0-a185-27081039d99e', competence_is_process_dependent, empirically_contingent).
narrative_ontology:cs_axiom('7780a566-82af-40a0-a185-27081039d99e', foundational, continuous_timing_is_structurally_necessary).
narrative_ontology:cs_axiom_status(continuous_timing_is_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('7780a566-82af-40a0-a185-27081039d99e', continuous_timing_is_structurally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('7780a566-82af-40a0-a185-27081039d99e', continuous_skill_maintenance_required).
narrative_ontology:cs_drift_state('7780a566-82af-40a0-a185-27081039d99e', contemporary_post_regulatory_capture, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7780a566-82af-40a0-a185-27081039d99e', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, institutional_risk_managers).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, operational_personnel).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, training_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operational_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate continuous simulation-based competence validation for critical-risk personnel (pilots, nuclear operators, hazmat handlers). Justify the requirement as insurance against catastrophic failure due to skill atrophy. Enforce via audit, licensing sanctions, and incident investigation. Collect regulatory authority and institutional credibility from the rule.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt continuous simulation protocols as their own risk-mitigation policy. Compliance with the mandate defends them against liability if an incident occurs—they can demonstrate 'best practice' adherence. The requirement outsources the competence judgment to regulators, reducing organizational liability exposure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, institutional_risk_managers, beneficiary,
    institutional, generational, mobile, national).

% Must participate in recurring simulation cycles, often at time cost (time away from operational duties, off-shift training, travel to simulation centers). The cycles keep them operationally current and reduce their own catastrophic-failure risk. But the continuous cycle itself is a disciplinary machinery: it validates competence through perpetual re-examination rather than through demonstrated safe performance.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operational_personnel, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, operational_personnel, beneficiary).

% Bear the cost of scheduling, operating, and maintaining simulation infrastructure. Must ensure compliance across dispersed personnel. Face regulatory sanctions for non-compliance. The continuous requirement justifies their budget and staffing; removing it would eliminate their function entirely.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_administrators, payer,
    moderate, biographical, constrained, national).

% Possess empirical records of accident causation: what percentage of failures are competence-degradation, what are system or communication failures, what is the predictive validity of simulation performance for actual catastrophe avoidance. Their analysis could establish or refute whether continuous cycles are necessary, sufficient, or both.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, real_incident_data_holders, observer,
    analytical, civilizational, analytical, global).

% Advocates for competence validation through demonstrated safe performance, peer evaluation, or less-frequent high-fidelity scenarios rather than continuous low-stakes drills. Are structurally excluded from the regulatory definition of 'valid exercise' and have no formal standing to propose alternatives.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, alternate_validation_communities, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared standards for what constitutes adequate competence refresh in high-risk domains: when, how often, and in what fidelity must personnel exercise skills to remain operationally sound. Coordinates organizational training infrastructure, regulatory expectations, and professional identity around common validation rhythm.
% TRANSFER_FUNCTION: Transfers time and administrative burden (cost of continuous drills, travel, off-shift training) from operational personnel and training departments to simulation infrastructure providers and regulatory oversight. Transfers competence judgment from demonstrated performance to periodic examination results. Transfers professional authority from local peer evaluation to centralized regulatory standards.
% ABSENT_VOICES: Practitioners and methodologists advocating performance-based validation (demonstrated safe conduct, peer review, distributed informal learning) are not seated in regulatory standard-setting. Real-incident investigators whose data would establish whether continuous simulation actually prevents catastrophes are not consulted in mandate design. Personnel who have left operational fields (retired pilots, former nuclear operators) whose perspective on whether continuous validation was necessary to competence are not heard.
% DISAPPEARANCE_RATIONALE: If the continuous-refresh requirement vanished, regulatory frameworks would shift to alternative validation approaches (periodic high-stakes testing, performance-based evidence over intervals, extended observation windows). Organizations would rationalize training infrastructure. Personnel would face less continuous surveillance and periodic re-examination. The regulatory authority structure would need to reground competence judgment in demonstrated operational performance rather than exercise frequency. Professional identity would reorient from continuous-validation-as-proof-of-competence to performance-in-operation-as-proof-of-competence.
% FOUNDING_PROBLEM: Competence in critical-risk domains degrades through disuse; operators who do not continuously exercise their skills face elevated failure risk when actual emergencies occur. Periodic testing is insufficient to maintain skill at operational levels because decay is ongoing, not episodic.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science and human factors literature attests that skill decay accelerates over time and distributed practice outperforms massed practice; aviation regulators cite accident data correlating simulator training frequency with safety performance. However, systematic accident-database analysis by NTSB, ICAO, and academic safety researchers indicates that operator competence decay accounts for <15% of accidents in well-structured operational domains; system failures, communication breakdowns, and organizational errors dominate. Regulators interpret this incomplete causation as evidence FOR (competence is one of multiple failure modes, so continuous drill prevents at least that mode) while independent safety scientists interpret it as evidence competence decay is over-weighted as a regulatory target relative to system design and organizational factors. The founding problem is attested by some evidence (cognitive decay is real) but contested in scope (how much of actual catastrophe does it explain).
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.61, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.61 at interval end (2026) because the constraint genuinely coordinates validation standards (reducing it below 0.4) BUT also extracts time and administrative burden from personnel and administrators in a way decoupled from demonstrated necessity (raising it above 0.5). The measurement series shows rise from 0.25 (1980) to 0.61 (2026) as simulation infrastructure expanded and regulatory requirements tightened, indicating the constraint evolved from loosely-enforced guidance toward institutional machinery. Theater is 0.48 because while the coordination function is real, a significant share of continuous-cycle activity is defensive (demonstrating compliance for audit and liability purposes) rather than knowledge-based. Suppression is 0.54: the constraint operates by excluding alternative validation approaches (performance-based, distributed, peer-review) from the regulatory definition of 'valid exercise,' and by making non-compliance a licensing and career threat (moderate active enforcement, not extreme). Accessibility_collapse and resistance show leveled divergence: at structural level, regulators and risk managers have found alternatives (litigation liability, performance-based regulation) and resistance is moderate (0.58 at tn); at individual level, operational personnel face near-total collapse of alternatives (0.68) and highest resistance (0.82) because they cannot easily exit the profession. Suppression at organizational level (0.60) reflects institutional capture: risk managers adopt the constraint as 'best practice' to defend liability, making institutional exit costlier than compliance. The divergence across levels is the evidence of tangled_rope structure: genuine coordination at system level, extraction at personnel level.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (operational_personnel, training_administrators) and the agenda-setter (safety_regulators) should compute to different types. From the regulator's institutional seat, the constraint is genuine coordination: establishing shared standards for competence validation, reducing certification fragmentation, defending against catastrophic failures due to skill decay. From operational_personnel's identity-locked seat, the constraint is extractive: continuous cycle demonstrates institutional control over competence-checking, validates the system's authority to judge competence, and generates disciplinary cycles without clear correlation to improved safety. From training_administrators' seat, it is beneficial: the requirement justifies their budget and function, but the burden falls as well (they must operate the machinery). The engine computes these divergences from power (institutional vs. moderate), exit_options (arbitrage vs. identity_locked vs. constrained), and role (agenda_setter vs. payer vs. beneficiary). The authored claim/metric gap reflects this: the constraint is CLAIMED as necessary coordination but MEASURED as substantially extractive. The claim is what regulators assert; the metrics describe what the constraint actually does.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety_regulators: d ≈ 0.1 (full beneficiary). They set the rule, collect authority from its operation, face no exit cost. Institutional_risk_managers: d ≈ 0.2 (beneficiary with modest cost). They adopt compliance as their liability defense, but must update practices and infrastructure. Operational_personnel: d ≈ 0.8 (target). They bear time cost, continuous re-examination, and identity-lock prevents exit. Training_administrators: d ≈ 0.75 (target). They bear operational cost, but the requirement justifies their existence (slight beneficiary offset). Real_incident_data_holders: d = 0.5 (analytical symmetric). They have no stake in the outcome; their role is observation. Alternate_validation_communities: d ≈ 0.85 (excluded target). They would benefit from the constraint's removal but are trapped outside the regulatory definition of 'valid exercise.'
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatropic (mandate-dead but function persisting). The founding problem—competence decay causes accidents—is contested but live. Regulators attest it is live and critical; safety scientists (reading empirical accident causation data) attest it is live but secondary; operational personnel attest the system works but is overbuilt. The disappearance_verdict is world_rearranges: if the continuous-refresh requirement vanished, regulatory frameworks would shift, organizations would rationalize training, and personnel would face less surveillance. The constraint's mandate (competence retention through continuous exercise) is actively contested and its validity is under challenge (hence founding_problem_status = contested). No mandatrophy signal here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_causation_in_accidents,
    'What fraction of actual catastrophic failures in critical-risk domains are caused by individual competence decay vs. system/organizational/communication failures?',
    'Systematic retrospective analysis of accident databases (NTSB, ICAO, NRC, OSHA incident records) classifying accident root causes and the role of operator competence decay in each. Requires clear distinction between competence gaps identifiable post-incident and competence gaps present at time of incident.',
    'If competence decay causes <10% of failures, continuous simulation is addressing a secondary failure mode; the constraint''s extraction and suppression would be largely directed at a minor risk factor. If >40%, continuous simulation is addressing a primary risk driver. This resolves the kernel contest between ''necessary but not sufficient'' and ''insufficient substitute for system design.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_decay_causation_in_accidents, empirical, 'What role competence decay plays in actual accident causation.').

omega_variable(
    simulation_fidelity_vs_performance_correlation,
    'Does simulation performance (passing scores on high-fidelity sims) predict actual catastrophe avoidance better than demonstrated safe operational performance or interval-based refresher testing?',
    'Prospective cohort study: track personnel with varying simulation frequency and quality; compare accident rates (actual incidents where the operator''s decision was tested) against simulation scores. Compare accident-rate variation explained by continuous-sim metrics vs. by operational-performance metrics.',
    'If simulation correlation is weak or non-monotonic (beyond a threshold, more drills don''t predict fewer failures), the constraint''s empirical foundation shifts from ''continuous is necessary'' to ''continuous provides ritual confirmation.'' If strong, the constraint''s necessity claim is strengthened. This is the core of the reading disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_vs_performance_correlation, empirical, 'Whether continuous simulation predicts actual safety.').

omega_variable(
    reading_identity_competence_lock,
    'To what extent is operational personnel''s identity fused with the continuous validation apparatus? Is identity_locked exit in this reading produced by professional commitment (''this is my competence domain'') or by the constraint''s enforcement machinery (''I am competent only if this system says so'')?',
    'Qualitative interviews with personnel who have left operational roles (retirement, job change, departure from field); analysis of stated reasons for exit and whether competence validation anxiety was a barrier or accelerant. Post-exit trajectory of competence confidence and skill maintenance.',
    'If identity fusion is primarily organizational (the constraint produces internalized competence-checking), suppression is substantially internalized and persists after constraint removal. If fusion is primary (the person''s self-concept as an operator is constituted by continuous validation), exit_options should be marked identity_locked rather than constrained, raising d and effective extraction on this population. If weak, the constraint is more easily removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_competence_lock, empirical, 'Whether identity fusion with validation is structural or produced by the constraint.').

omega_variable(
    reading_boundary_continuous_vs_periodic_necessity,
    'This reading claims continuous (distributed over time) is necessary; sibling ''real_catastrophe_only'' claims only real-world emergence is sufficient; sibling ''simulation_as_proxy'' claims one-time or infrequent simulation is adequate. Are these genuinely foreclosing, or do they describe a continuum where the key disagreement is the minimum interval?',
    'Operationalize ''competence retention'' as measurable performance decrement over time, with attention to: (a) rate of decay by domain (some skills decay faster than others); (b) threshold intervention points (at what decay level does risk rise dangerously); (c) relearning cost (can competence be restored with minimal retraining if it decays). This transforms the reading contest from philosophical into empirical.',
    'If the readings are actually describing different intervention thresholds on a continuum (this reading: refresh when decay reaches X%; ''as_proxy'': refresh when decay reaches Y% or annually; ''catastrophe_only'': refresh via incident), the kernel is NOT genuinely contested — it is a calibration disagreement. If they describe genuinely incommensurable standards (continuous vs. never vs. only-real), the readings foreclose each other. This affects whether the constraint is naturally unstable (three coexisting readings) or logically structured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_continuous_vs_periodic_necessity, conceptual, 'Whether the reading contest is philosophical or empirical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1980, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(comp_tr_t1990, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(comp_tr_t2000, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(comp_tr_t2018, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(comp_tr_t2026, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(comp_be_t1980, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(comp_be_t1990, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(comp_be_t2000, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(comp_be_t2018, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(comp_be_t2026, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2026, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1980, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(comp_su_t1990, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(comp_su_t2000, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(comp_su_t2018, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(comp_su_t2026, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2026, 0.54).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2026
narrative_ontology:measurement(comp_grid_01, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(class), 1980, 0.28).
narrative_ontology:measurement(comp_grid_02, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(class), 2026, 0.7).
narrative_ontology:measurement(comp_grid_03, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(individual), 1980, 0.15).
narrative_ontology:measurement(comp_grid_04, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(individual), 2026, 0.68).
narrative_ontology:measurement(comp_grid_05, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(organizational), 1980, 0.42).
narrative_ontology:measurement(comp_grid_06, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(organizational), 2026, 0.72).
narrative_ontology:measurement(comp_grid_07, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(structural), 1980, 0.35).
narrative_ontology:measurement(comp_grid_08, competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse(structural), 2026, 0.58).
narrative_ontology:measurement(comp_grid_09, competence_exercise_validity__continuous_refresh_hybrid, resistance(class), 1980, 0.58).
narrative_ontology:measurement(comp_grid_10, competence_exercise_validity__continuous_refresh_hybrid, resistance(class), 2026, 0.75).
narrative_ontology:measurement(comp_grid_11, competence_exercise_validity__continuous_refresh_hybrid, resistance(individual), 1980, 0.68).
narrative_ontology:measurement(comp_grid_12, competence_exercise_validity__continuous_refresh_hybrid, resistance(individual), 2026, 0.82).
narrative_ontology:measurement(comp_grid_13, competence_exercise_validity__continuous_refresh_hybrid, resistance(organizational), 1980, 0.72).
narrative_ontology:measurement(comp_grid_14, competence_exercise_validity__continuous_refresh_hybrid, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(comp_grid_15, competence_exercise_validity__continuous_refresh_hybrid, resistance(structural), 1980, 0.65).
narrative_ontology:measurement(comp_grid_16, competence_exercise_validity__continuous_refresh_hybrid, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(comp_grid_17, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(class), 1980, 0.18).
narrative_ontology:measurement(comp_grid_18, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(class), 2026, 0.58).
narrative_ontology:measurement(comp_grid_19, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(individual), 1980, 0.1).
narrative_ontology:measurement(comp_grid_20, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(individual), 2026, 0.55).
narrative_ontology:measurement(comp_grid_21, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(organizational), 1980, 0.3).
narrative_ontology:measurement(comp_grid_22, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(organizational), 2026, 0.62).
narrative_ontology:measurement(comp_grid_23, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(structural), 1980, 0.22).
narrative_ontology:measurement(comp_grid_24, competence_exercise_validity__continuous_refresh_hybrid, stakes_inflation(structural), 2026, 0.45).
narrative_ontology:measurement(comp_grid_25, competence_exercise_validity__continuous_refresh_hybrid, suppression(class), 1980, 0.08).
narrative_ontology:measurement(comp_grid_26, competence_exercise_validity__continuous_refresh_hybrid, suppression(class), 2026, 0.48).
narrative_ontology:measurement(comp_grid_27, competence_exercise_validity__continuous_refresh_hybrid, suppression(individual), 1980, 0.05).
narrative_ontology:measurement(comp_grid_28, competence_exercise_validity__continuous_refresh_hybrid, suppression(individual), 2026, 0.52).
narrative_ontology:measurement(comp_grid_29, competence_exercise_validity__continuous_refresh_hybrid, suppression(organizational), 1980, 0.18).
narrative_ontology:measurement(comp_grid_30, competence_exercise_validity__continuous_refresh_hybrid, suppression(organizational), 2026, 0.6).
narrative_ontology:measurement(comp_grid_31, competence_exercise_validity__continuous_refresh_hybrid, suppression(structural), 1980, 0.12).
narrative_ontology:measurement(comp_grid_32, competence_exercise_validity__continuous_refresh_hybrid, suppression(structural), 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.18).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% The competence_exercise_validity kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what constitutes valid competence exercise. This constraint (continuous_refresh_hybrid) claims competence is process-dependent and continuous exercise is necessary. The simulation_as_proxy reading claims simulation is adequate substitute for catastrophe and frequency is secondary. The real_catastrophe_only reading claims only actual emergence exercises competence. The three readings coexist as live institutional positions held by different professional communities (regulators, training industry, post-incident investigators). All three share the referent (standing arrangement: simulation-based competence validation) but author different ε values reflecting their different readings of the arrangement's extractiveness and its necessity. Link the sibling stories with bidirectional affects_constraints entries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__continuous_refresh_hybrid, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
