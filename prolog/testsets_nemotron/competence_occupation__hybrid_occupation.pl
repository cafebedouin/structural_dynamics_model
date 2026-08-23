% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation Regime (Simulation + Refresher + Procedural Reinforcement + Line Audits)
 *   domain: organizational/safety/training
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare, rail)
 *   mandate a four-mechanism competence occupation regime: simulator
 *   sessions, classroom refreshers, procedural reinforcement drills, and line
 *   audits. The regime is presented as a coordination solution — ensuring all
 *   operators maintain readiness for rare catastrophic scenarios. But no
 *   consensus exists on optimal configuration (frequency, fidelity, scenario
 *   mix, audit standards). Each mechanism has advocates, vendors, and audit
 *   requirements. The regime expands over time (more hours, more mechanisms,
 *   higher fidelity) without evidence that marginal additions improve
 *   outcomes. Operators experience it as compliance burden; training
 *   departments and vendors capture the expansion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.38).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.45).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation Regime (Simulation + Refresher + Procedural Reinforcement + Line Audits)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "organizational/safety/training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '980b2f3a-2045-478c-b7d9-7b4750bef2b1').
narrative_ontology:cs_kernel_codification('980b2f3a-2045-478c-b7d9-7b4750bef2b1', distributed).
narrative_ontology:cs_authority_grounding('980b2f3a-2045-478c-b7d9-7b4750bef2b1', practice).
narrative_ontology:cs_interpretation_layer_present('980b2f3a-2045-478c-b7d9-7b4750bef2b1').
narrative_ontology:cs_reading_relation('980b2f3a-2045-478c-b7d9-7b4750bef2b1', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('980b2f3a-2045-478c-b7d9-7b4750bef2b1', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('980b2f3a-2045-478c-b7d9-7b4750bef2b1', foundational, no_single_mechanism_suffices_for_competence_occupation).
narrative_ontology:cs_axiom_status(no_single_mechanism_suffices_for_competence_occupation, holdable).
narrative_ontology:cs_axiom_grounding('980b2f3a-2045-478c-b7d9-7b4750bef2b1', no_single_mechanism_suffices_for_competence_occupation, empirically_contingent).
narrative_ontology:cs_axiom('980b2f3a-2045-478c-b7d9-7b4750bef2b1', foundational, competence_occupation_requires_continuous_multi_mechanism_exercise).
narrative_ontology:cs_axiom_status(competence_occupation_requires_continuous_multi_mechanism_exercise, holdable).
narrative_ontology:cs_axiom_grounding('980b2f3a-2045-478c-b7d9-7b4750bef2b1', competence_occupation_requires_continuous_multi_mechanism_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('980b2f3a-2045-478c-b7d9-7b4750bef2b1', structured_recurrent_training_floor).
narrative_ontology:cs_drift_state('980b2f3a-2045-478c-b7d9-7b4750bef2b1', contemporary_regulatory_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('980b2f3a-2045-478c-b7d9-7b4750bef2b1', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_auditors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_consultants).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_units).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, small_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, operational_units).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, competence_requires_active_maintenance).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, no_single_mechanism_suffices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, administer, and enforce the multi-mechanism regime. Control curriculum, scheduling, and compliance reporting. Capture budget allocation and institutional prestige from running the system. Can redirect resources between mechanisms but face pressure to expand rather than optimize.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_departments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, training_departments, beneficiary).

% Sell simulation platforms, scenario libraries, and refresher modules. Revenue scales with mandated hours and mechanism count. Compete on fidelity and regulatory acceptance, not on demonstrated competence outcomes. Exit is easy — they serve multiple industries and regulators.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Enforce compliance with the multi-mechanism standard. Their authority and resource justification depend on the regime's complexity — more mechanisms mean more audit surface. Professional identity is bound to the regulatory framework; exit means leaving the profession.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_auditors, beneficiary,
    institutional, generational, identity_locked, national).

% Advise on mechanism selection, configuration, and compliance. Sell optimization studies that rarely conclude any mechanism should be cut. Market position depends on the regime's perpetual complexity. Exit is easy — consulting skills transfer across domains.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_consultants, beneficiary,
    organized, biographical, mobile, global).

% Bear the time, cognitive load, and schedule disruption of all four mechanisms. Experience them as disjointed compliance exercises rather than integrated skill maintenance. Have limited voice in mechanism design; exit means leaving the occupation or accepting non-compliance risk.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Lose operational capacity to training hours and simulator downtime. Gain marginal readiness assurance but cannot measure which mechanism contributes what. Caught between regulatory compliance and mission readiness. Exit is constrained — they must operate under the same regulator.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_units, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, operational_units, beneficiary).

% Lack dedicated training departments, simulation access, and audit infrastructure. Must purchase all mechanisms commercially at full cost. Compliance burden threatens viability. No collective bargaining power; exit means ceasing operations.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, small_operators, payer,
    powerless, biographical, trapped, local).

% Studies skill decay, transfer of training, and mechanism interactions. Finds no consensus on optimal configuration; each study adds parameters. Their work sustains the research problem but does not resolve it. Exit is analytical — they observe from outside the compliance regime.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, research_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that personnel in high-reliability roles maintain competence across rare but catastrophic failure modes through structured, multi-modal practice when real incidents are too rare to provide sufficient exercise.
% TRANSFER_FUNCTION: Moves operator time, organizational capacity, and budget from operational readiness to compliance with a four-mechanism regime whose marginal contribution of each mechanism is unknown and unmeasured.
% ABSENT_VOICES: Frontline operators (especially from small operators) who experience the regime as disjointed compliance burden; early-career personnel who have never seen the regime questioned; families and communities that bear the opportunity cost of training hours.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism mandate vanished, training departments would shrink, simulation vendors would lose mandated revenue, regulators would lose audit surface, and operators would revert to ad-hoc or single-mechanism practice. Competence outcomes would become visibly unequal — some units would maintain rigorous practice, others would decay. The regime's coordination function (ensuring a floor) would disappear, revealing which units were only compliant.
% FOUNDING_PROBLEM: After several high-profile accidents where competent crews failed to recognize or manage rare failure modes, regulators mandated structured recurrent training. The founding problem was: how to guarantee competence for events that occur once per career or less, without relying on luck of incident exposure.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards (NTSB, AAIB, etc.) attest the founding problem remains live — rare-event competence gaps still appear in investigations. Training departments and regulators attest the problem is solved by the current regime. Independent researchers (e.g., Salas, Cannon-Bowers, Flin) attest the problem is real but the current regime's configuration is not evidence-based — corroboration from outside the benefiting parties supports 'contested'.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).
:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects real resource transfer from operations to training compliance, but the coordination function is genuine — rare-event competence genuinely requires structured practice. Suppression (0.45) is moderate: alternatives (e.g., competency-based progression, adaptive training) are discouraged but not banned; the regime persists through regulatory mandate and vendor lock-in, not pure coercion. Theater ratio (0.52) is rising: a growing share of training hours serves audit compliance rather than skill maintenance. Accessibility collapse (0.58) is partial: units can innovate at margins but cannot exit the four-mechanism frame. Resistance (0.42) is real but channeled into 'optimization' studies that preserve the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats, the regime is a necessary coordination infrastructure — they see the accidents it prevents (or claims to). From the payer seats, it's an expanding compliance burden with diminishing returns — they see the hours lost, the scenarios that don't transfer, the audits that check boxes. The research community sees a genuine unsolved problem (optimal configuration) that the regime treats as settled. The engine will compute different effective extraction per seat from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Training departments, simulation vendors, regulators, and consultants are structural beneficiaries — they collect budget, revenue, authority, and fees from the regime's expansion. Their exit options range from arbitrage (training depts control internal allocation) to identity-locked (regulators' professional identity is the regime). Frontline operators, operational units, and especially small operators are payers — they bear time, cognitive load, and cost with constrained or trapped exit. Operational units have secondary beneficiary role (readiness assurance) but it's marginal and unmeasured.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare-event competence maintenance) remains live, but the current regime has outgrown its solution mandate. It now extracts resources to sustain its own complexity (more mechanisms, higher fidelity, more audits) without evidence of marginal benefit. This is mandatrophy: the mandate has hypertrophied beyond the problem it was built to solve. The regime is not a snare (coordination function is real) nor a pure rope (asymmetric extraction is real) — it is a tangled rope where coordination and extraction are fused and the optimal boundary is perpetually deferred to 'more research'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_marginal_contribution,
    'What is the marginal competence contribution of each mechanism (simulation, refresher, procedural reinforcement, line audit) when the others are present?',
    'Controlled field experiments or natural experiments from regulatory variation (e.g., jurisdictions that mandate 3 vs 4 mechanisms, or varying simulator fidelity requirements). Requires competence outcome measures beyond compliance.',
    'If any mechanism shows near-zero marginal contribution, the regime contains pure extraction masquerading as coordination. If all show significant contribution, the regime''s complexity is justified but the optimal configuration remains unknown.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_marginal_contribution, empirical, 'Whether the four-mechanism bundle contains mechanisms that extract without contributing to competence.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (ensuring a competence floor) end and the extractive expansion (more hours, higher fidelity, more mechanisms) begin?',
    'Longitudinal analysis of competence outcomes vs. training hours/mechanisms across operators. Identify the inflection point where marginal hours stop improving outcomes.',
    'If a clear inflection exists, the regime above it is extractive; if no inflection is detectable, the regime may be efficiently scaled but unmeasured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'The boundary between necessary coordination and extractive expansion in the regime.').

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement between hybrid_occupation, simulation_sufficiency, and real_incident_necessity about (a) the empirical sufficiency of each mechanism, (b) the definition of ''competence occupation'', or (c) the institutional interests each reading serves?',
    'Trace the citation networks and funding sources of each reading''s advocates. Map which empirical claims each reading treats as settled vs. contested.',
    'If (a), the disagreement is resolvable by evidence. If (b), it''s a conceptual fragmentation of the kernel. If (c), the readings are interest-positions masquerading as empirical claims — the kernel is a contested terrain, not a scientific question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of the disagreement between the three competence_occupation readings.').

omega_variable(
    small_operator_viability_threshold,
    'At what compliance cost does the regime force small operators out of the market, and does this concentration increase or decrease systemic risk?',
    'Market structure analysis over time: correlate training cost burden with operator exit/entry and incident rates by operator size.',
    'If the regime drives consolidation and consolidation increases systemic risk (less diversity, more correlated failures), the regime''s coordination function produces a countervailing extraction at system level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_operator_viability_threshold, empirical, 'Whether the regime''s extraction from small operators creates a systemic risk that undermines its own coordination purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__hybrid_occupation, theater_ratio, 4, 0.38).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__hybrid_occupation, theater_ratio, 8, 0.44).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.48).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__hybrid_occupation, theater_ratio, 16, 0.5).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t4, competence_occupation__hybrid_occupation, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(comp_be_t8, competence_occupation__hybrid_occupation, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(comp_be_t16, competence_occupation__hybrid_occupation, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t4, competence_occupation__hybrid_occupation, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(comp_su_t8, competence_occupation__hybrid_occupation, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(comp_su_t16, competence_occupation__hybrid_occupation, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, resource_allocation).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.15).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, simulation_fidelity_escalation).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, regulatory_audit_scope_creep).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, operator_fatigue_management).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_occupation reading of the competence_occupation kernel. It coexists with simulation_sufficiency and real_incident_necessity readings. The three readings form a constraint family linked by network.affects_constraints. The hybrid reading sets the regulatory floor that influences the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, institutional, 0.15).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, organized, 0.35).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, moderate, 0.7).
constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
