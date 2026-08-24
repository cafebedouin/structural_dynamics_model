% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Bridge: Minor Failures Sufficient for Simulator Validation
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear, aviation, chemical, healthcare)
 *   face a competence maintenance problem: catastrophic events are rare but
 *   unacceptable, yet the skills to prevent them must stay sharp. This
 *   reading of the competence_retention_exercise kernel asserts that
 *   near-miss incidents and minor failures provide sufficient real-world
 *   feedback to validate and update simulator training, making catastrophic
 *   events unnecessary for competence maintenance. The claim structures
 *   investment in near-miss reporting systems, simulator procurement, and
 *   safety consulting — a coordination mechanism that also extracts resources
 *   from frontline operators and operating organizations. The measurement
 *   series shows rising extractiveness and theater_ratio as near-miss systems
 *   mature from voluntary learning tools into mandatory compliance regimes
 *   with growing bureaucratic overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.42).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.35).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Bridge: Minor Failures Sufficient for Simulator Validation").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '2c263276-cb82-4e58-86ac-0c747ba026e0').
narrative_ontology:cs_kernel_codification('2c263276-cb82-4e58-86ac-0c747ba026e0', distributed).
narrative_ontology:cs_authority_grounding('2c263276-cb82-4e58-86ac-0c747ba026e0', practice).
narrative_ontology:cs_interpretation_layer_present('2c263276-cb82-4e58-86ac-0c747ba026e0').
narrative_ontology:cs_reading_relation('2c263276-cb82-4e58-86ac-0c747ba026e0', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('2c263276-cb82-4e58-86ac-0c747ba026e0', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_axiom('2c263276-cb82-4e58-86ac-0c747ba026e0', foundational, near_miss_feedback_suffices_for_simulator_validation).
narrative_ontology:cs_axiom_status(near_miss_feedback_suffices_for_simulator_validation, holdable).
narrative_ontology:cs_axiom_grounding('2c263276-cb82-4e58-86ac-0c747ba026e0', near_miss_feedback_suffices_for_simulator_validation, empirically_contingent).
narrative_ontology:cs_axiom('2c263276-cb82-4e58-86ac-0c747ba026e0', foundational, catastrophe_not_required_for_competence_maintenance).
narrative_ontology:cs_axiom_status(catastrophe_not_required_for_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('2c263276-cb82-4e58-86ac-0c747ba026e0', catastrophe_not_required_for_competence_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('2c263276-cb82-4e58-86ac-0c747ba026e0', secondary, hybrid_simulator_near_miss_system_necessary).
narrative_ontology:cs_axiom_status(hybrid_simulator_near_miss_system_necessary, holdable).
narrative_ontology:cs_axiom_grounding('2c263276-cb82-4e58-86ac-0c747ba026e0', hybrid_simulator_near_miss_system_necessary, instrumental).
narrative_ontology:cs_reference_frame('2c263276-cb82-4e58-86ac-0c747ba026e0', operational_learning_without_catastrophe).
narrative_ontology:cs_drift_state('2c263276-cb82-4e58-86ac-0c747ba026e0', post_fukushima_deepwater_horizon, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c263276-cb82-4e58-86ac-0c747ba026e0', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_consultants).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, hro_management).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, operating_organizations).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, learning_from_failure_without_catastrophe).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, simulator_fidelity_through_operational_data).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces near-miss reporting requirements, procures simulators, commissions investigations. Benefits from demonstrated safety culture, regulatory compliance, and reduced catastrophe risk. Can move between organizations or sectors; their professional identity is tied to safety leadership.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, hro_management, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, hro_management, beneficiary).

% Experience near-misses directly; must file reports, participate in root cause analyses, attend simulator updates. Reporting burden competes with operational duties. Professional identity fuses with safety culture — exiting the reporting system feels like abandoning professional responsibility. Career progression depends on compliance.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Fund reporting infrastructure, simulator time, investigation teams, consultant fees. Bear opportunity cost of operator time diverted to reporting. Regulatory and insurance pressure constrains exit — cannot simply abandon near-miss systems without losing license or coverage. Shareholder pressure limits investment appetite.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, operating_organizations, payer,
    powerful, biographical, constrained, global).

% Sell high-fidelity simulators and update packages validated against near-miss data. Revenue depends on the bridge claim — if near-misses validate simulators, demand for frequent updates grows. Can pivot to other training markets if this constraint weakens.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Provide near-miss investigation methodologies, root cause analysis frameworks, and simulator scenario design services. Their expertise market exists because the bridge claim creates demand for translation of operational events into simulator updates. Portable skills across high-reliability domains.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_consultants, beneficiary,
    organized, biographical, mobile, global).

% Mandate near-miss reporting systems, audit compliance, evaluate simulator adequacy. Do not directly pay for or profit from the constraint. Their legitimacy depends on the bridge claim being true — if near-misses don't prevent catastrophes, regulatory mandate loses justification.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulators, observer,
    institutional, generational, analytical, national).

% Public populations around nuclear plants, under flight paths, downstream of chemical facilities, patients in hospitals. Bear catastrophic consequences if the bridge fails. No voice in near-miss system design, simulator validation criteria, or reporting requirements. Exit is impossible — they cannot avoid the risk.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, potential_victims, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides real-world operational data to anchor simulator fidelity without requiring catastrophic events, solving the competence maintenance problem in domains where catastrophes are rare but unacceptable.
% TRANSFER_FUNCTION: Moves organizational resources (operator time, investigation capital, simulator update budgets) from daily operations into the near-miss reporting and simulator validation pipeline; moves learning from frontline experience into simulator scenario libraries and training curricula.
% ABSENT_VOICES: Frontline operators experiencing reporting fatigue and fear of blame; potential victims (public, patients) who would suffer if near-miss bridge fails; critics arguing near-misses lack catastrophic regime dynamics (e.g., Perrow normal accident theorists, some resilience engineering voices); small organizations that cannot afford full near-miss infrastructure.
% DISAPPEARANCE_RATIONALE: If the near-miss bridge concept disappeared, organizations would face a binary choice: rely purely on simulation (simulation_as_sufficient) with no real-world validation anchor, or accept that competence maintenance requires catastrophic events (catastrophe_as_necessary). Simulator vendors would lose validation data stream; regulators would lose compliance metric; operators would lose reporting burden but gain unvalidated simulators.
% FOUNDING_PROBLEM: How to maintain genuine catastrophe-avoidance competence in high-reliability domains where catastrophic events are (thankfully) rare, but the skills to prevent them must remain sharp and validated against reality.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear industry INPO reports and IAEA safety guides document the competence maintenance problem as ongoing; aviation ASRS and LOSA data show continued investment in near-miss systems; chemical industry CCPS guidelines treat near-miss investigation as essential layer; healthcare WHO surgical safety checklist evolution shows live adaptation. Sources outside simulator vendor/consultant beneficiary set include regulatory bodies, professional societies, and independent safety researchers.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real resource transfer: frontline operator time for reporting/investigation, organizational capital for reporting infrastructure and simulator updates, consultant/vendor fees. The coordination function is genuine — near-miss data does improve simulator fidelity for routine and some off-normal scenarios. Suppression (0.35) captures the transition from voluntary reporting cultures to mandatory systems where non-reporting carries career risk. Theater_ratio (0.38) reflects the growing gap between 'learning from near-misses' rhetoric and compliance-driven reporting that fills databases without improving simulator catastrophic scenario validity. Accessibility_collapse (0.42) and resistance (0.48) are moderate: alternatives (pure simulation, catastrophe waiting) exist but are institutionally disfavored; resistance comes from reporting fatigue and skepticism about near-miss relevance to catastrophes.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats, the constraint is a rope: genuine coordination solving a real learning problem with net benefit. From the frontline_operator payer seat, it is a tangled_rope at best — coordination function exists but extraction (time, career risk, identity_lock to safety culture) is asymmetric and enforcement active. From the potential_victim excluded seat, it is a snare if the bridge claim is false — suppression of catastrophic learning behind a coordination facade. The engine computes this divergence from the structural data; the claimed_type (rope) reflects the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   HRO management (agenda_setter, institutional, generational, arbitrage, global) sets reporting requirements and buys simulators — structural beneficiary (d ~ 0.15). Simulator vendors and safety consultants (beneficiary, powerful/organized, biographical, mobile/arbitrage, global) capture revenue streams — strong beneficiaries (d ~ 0.1). Frontline operators (payer, moderate/powerless, biographical, identity_locked/constrained, local/regional) bear reporting burden and investigation time with limited exit — targets (d ~ 0.7-0.85). Operating organizations (payer, powerful, biographical, constrained, national/global) fund the infrastructure — moderate targets (d ~ 0.55). Regulators (observer, institutional, generational, analytical, national) monitor but don't directly pay or collect. Potential victims (excluded, powerless, generational, trapped, global) would bear catastrophe costs if the bridge fails — excluded from the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining catastrophe-avoidance competence without catastrophes) remains live. The constraint has not atrophied into piton — active enforcement and investment continue. However, rising theater_ratio and extractiveness suggest mandatrophy risk: if near-miss data proves insufficient for catastrophic regimes, the arrangement persists as ritual compliance (piton) or captures resources without delivering the claimed bridge (snare). The mandatrophy_resolved flag is false — the mandate's function is still contested, not settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_validity_for_catastrophe,
    'Do near-miss incidents genuinely capture the cognitive, organizational, and systemic dynamics of catastrophic failures, or do they represent a different regime of failure?',
    'Comparative analysis of near-miss causal pathways vs. catastrophe causal pathways across multiple high-reliability domains (nuclear, aviation, chemical, healthcare); experimental studies of simulator transfer from near-miss-trained vs. catastrophe-trained operators.',
    'If near-misses miss catastrophic regime dynamics, the bridge claim fails and catastrophe_as_necessary gains support; if they capture sufficient dynamics, near_miss_as_bridge is empirically validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_validity_for_catastrophe, empirical, 'Whether near-miss data transfers to catastrophic scenario competence').

omega_variable(
    reporting_burden_as_suppression,
    'Does the investigative and reporting burden of near-miss systems create de facto suppression of reporting, especially for minor but revealing incidents?',
    'Longitudinal analysis of reporting rates vs. investigation depth requirements; anonymous surveys of frontline operators on reporting decisions; comparison of voluntary vs. mandatory reporting systems.',
    'If reporting burden suppresses the very data the bridge needs, the constraint becomes self-undermining — higher theater_ratio, higher effective suppression, potential reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_burden_as_suppression, empirical, 'Whether the coordination mechanism suppresses its own data source').

omega_variable(
    kernel_reading_ambiguity,
    'Is the competence_retention_exercise kernel genuinely contested across three structurally distinct readings, or do simulation_as_sufficient and near_miss_as_bridge occupy compatible positions on a continuum?',
    'Trace institutional positions (INPO, IAEA, FAA, CCPS, WHO surgical safety) to see if they endorse one reading exclusively or blend simulator fidelity with near-miss integration; examine whether any organization has formally foreclosed a reading.',
    'If readings blend in practice, the kernel may not have three discrete constraints; if they are genuinely foreclosed against each other, each reading instantiates a distinct constraint with its own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three declared readings are structurally discrete or continuous in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 8, 0.22).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 16, 0.3).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.35).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 32, 0.37).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.08).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This constraint is one member of the competence_retention_exercise kernel family. The three readings decompose the colloquial 'competence maintenance' label into structurally distinct claims with different ε values, beneficiary/victim structures, and coordination/extraction profiles. simulation_as_sufficient claims lower extractiveness (simulator investment only) but higher accessibility_collapse (no real-world anchor); catastrophe_as_necessary claims near-zero extractiveness (no reporting infrastructure) but infinite suppression (catastrophes cannot be engineered); near_miss_as_bridge occupies the middle with active extraction from operators and organizations to fund the bridge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, moderate, 0.75).
constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
