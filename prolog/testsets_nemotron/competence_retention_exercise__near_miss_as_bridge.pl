% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Feedback Loop for Simulator Validation
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear power, commercial aviation,
 *   chemical processing) maintain operator competence through a hybrid
 *   system: high-fidelity simulators for routine skill preservation and
 *   procedural rehearsal, plus a rigorous near-miss reporting and
 *   investigation system that feeds real-world anomalies back into scenario
 *   development. This reading asserts that near-miss data provides sufficient
 *   empirical grounding to validate and update simulators, making
 *   catastrophic events unnecessary for competence maintenance. The
 *   constraint is the institutionalized feedback loop: near-miss reporting →
 *   root cause analysis → simulator scenario integration → operator rehearsal
 *   → improved detection/response.
 *
 * KEY AGENTS:
 *   - frontline_operators: Primary beneficiaries and participants — experience near-misses, report them, train in updated simulators (organized/constrained)
 *   - safety_managers: Agenda-setters — design reporting systems, prioritize investigations, commission simulator updates (institutional/biographical)
 *   - regulatory_bodies: Beneficiaries and overseers — use near-miss data for oversight, mandate reporting standards (institutional/generational)
 *   - simulation_vendors: Beneficiaries — receive scenario development contracts, sell fidelity upgrades (organized/biographical)
 *   - investigation_teams: Beneficiaries — conduct analyses, build organizational knowledge (organized/biographical)
 *   - executive_leadership: Payers — fund the reporting infrastructure and simulator time (powerful/biographical)
 *   - public_community: Excluded beneficiaries — protected by the system but not in the reporting loop (powerless/civilizational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.28).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.15).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Feedback Loop for Simulator Validation").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e').
narrative_ontology:cs_kernel_codification('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', implicit).
narrative_ontology:cs_authority_grounding('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', practice).
narrative_ontology:cs_interpretation_layer_present('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e').
narrative_ontology:cs_reading_relation('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_axiom('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', foundational, near_miss_data_suffices_for_simulator_validation).
narrative_ontology:cs_axiom_status(near_miss_data_suffices_for_simulator_validation, holdable).
narrative_ontology:cs_axiom_grounding('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', near_miss_data_suffices_for_simulator_validation, empirically_contingent).
narrative_ontology:cs_axiom('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', foundational, hybrid_system_superior_to_pure_simulation_or_catastrophe_waiting).
narrative_ontology:cs_axiom_status(hybrid_system_superior_to_pure_simulation_or_catastrophe_waiting, holdable).
narrative_ontology:cs_axiom_grounding('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', hybrid_system_superior_to_pure_simulation_or_catastrophe_waiting, instrumental).
narrative_ontology:cs_reference_frame('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', post_tmi_bhopal_proactive_safety_paradigm).
narrative_ontology:cs_drift_state('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', contemporary_digital_reporting_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0ddbf2da-a7b2-45bb-9c4e-3b2d76fad93e', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_managers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulation_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, investigation_teams).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, executive_leadership).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, near_miss_sufficiency_thesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, hybrid_competence_maintenance_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the hazardous systems daily; experience near-misses directly; file reports; train in simulators updated from those reports. Their competence is the system's output. Exit means leaving the profession or transferring to a less hazardous role — constrained by specialized licenses and career investment.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_operators, beneficiary,
    organized, biographical, constrained, global).

% Design and administer the near-miss reporting system; triage and prioritize investigations; commission simulator scenario updates based on findings; report to regulators and leadership. They set the agenda for what gets investigated and integrated. Mobile exit: safety management skills transfer across HRO sectors.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_managers, agenda_setter,
    institutional, biographical, mobile, global).

% Mandate reporting standards; use aggregated near-miss data for sector-wide oversight and rulemaking; audit simulator fidelity. They benefit from the data stream without operating the reporting system. Analytical exit: their role is defined by statute, not organizational membership.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulatory_bodies, beneficiary,
    institutional, generational, analytical, national).

% Develop and sell simulator hardware/software; bid on scenario development contracts driven by near-miss findings; market fidelity upgrades. They capture revenue from the coordination function. Arbitrage exit: they can pivot to adjacent simulation markets (defense, medical, entertainment) if HRO demand shifts.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Conduct root cause analyses of near-miss incidents; translate findings into simulator scenario specifications; build organizational knowledge bases. They gain professional standing and institutional memory. Constrained exit: deep domain knowledge ties them to specific hazard domains.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, investigation_teams, beneficiary,
    organized, biographical, constrained, global).

% Allocate budget for reporting infrastructure, investigation staff, simulator time, and operator training hours. They bear the direct financial costs and the opportunity cost of operator time away from production. Mobile exit: executives rotate across industries; the constraint is one of many portfolio items.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, executive_leadership, payer,
    powerful, biographical, mobile, global).

% Live near hazardous facilities; bear catastrophic risk if competence fails; have no formal role in near-miss reporting, investigation prioritization, or simulator design. Trapped exit: geographic relocation is the only exit, and it is costly and incomplete (risk is distributed).
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, public_community, excluded,
    powerless, civilizational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the rare-event competence problem: how to maintain operator readiness for catastrophic scenarios that occur too rarely (or never) to learn from directly. The constraint coordinates three elements: (1) simulator infrastructure for routine skill rehearsal, (2) near-miss reporting for real-world anomaly detection, (3) investigation-to-scenario pipeline for empirical grounding. Without this coordination, each organization would face the problem alone with higher cost and lower coverage.
% TRANSFER_FUNCTION: Moves near-miss incident data (low-cost, high-frequency) from frontline operators through investigation teams into simulator scenario libraries, converting it into operator competence (high-value, low-frequency readiness). The transfer is mediated by safety managers who prioritize and regulators who set standards. Executive leadership funds the pipeline; simulation vendors build the scenarios.
% ABSENT_VOICES: The public communities surrounding hazardous facilities are structurally excluded — they bear the tail risk of competence failure but have no seat in reporting prioritization, scenario selection, or resource allocation. Would-be whistleblowers in organizations with weak reporting cultures are also absent — fear of retaliation keeps them silent. These absences are not accidental: the reporting system's legitimacy depends on operator trust, which requires confidentiality protections that also shield the system from external scrutiny.
% DISAPPEARANCE_RATIONALE: If the near-miss feedback loop vanished overnight, simulator scenarios would freeze at their last update; operator competence for novel failure modes would decay on a 3-5 year horizon; organizations would revert to either catastrophe-driven learning (waiting for the next disaster) or simulation-only training (drifting from reality). The HRO sector would lose its primary empirical grounding mechanism. Regulatory oversight would lose its leading indicator data stream.
% FOUNDING_PROBLEM: Post-TMI (1979) and Bhopal (1984), the nuclear and chemical industries faced a crisis: catastrophic events were too rare for routine learning but too consequential to ignore. The founding problem was how to generate organizational learning about catastrophic failure modes without waiting for catastrophes to occur. The near-miss reporting system was built as the answer: treat minor failures as free lessons.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by multiple independent sources: (1) INPO and WANO founding charters cite the TMI lesson explicitly; (2) ICAO Annex 13 and subsequent SMS mandates reference the need for proactive hazard identification; (3) independent scholars (Weick, Sutcliffe, Reason, Perrow) document the shift from reactive to proactive safety management across HRO sectors; (4) regulatory bodies (NRC, FAA, EPA) have embedded near-miss reporting in rulemaking. No single beneficiary group controls this corroboration — it spans regulators, professional associations, and academic researchers outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.28) because the constraint primarily coordinates — it solves a genuine collective action problem: how to maintain rare-event competence without rare events. The cost (reporting burden, simulator time, investigation effort) is distributed and justified by the coordination benefit (averted catastrophes). Suppression is low (0.15) because participation is largely voluntary within professional norms; exits exist (operators can leave the industry, organizations can adopt different safety models). Theater ratio is moderate (0.35) because some reporting becomes performative — filed to satisfy metrics rather than drive learning — and some simulator updates are cosmetic. Accessibility collapse is moderate (0.42): alternative competence models exist (catastrophe-driven, simulation-only) but are structurally disadvantaged by cost or risk. Resistance is moderate-high (0.55) from production pressures that compete for operator time and from vendors selling simulation-as-sufficient packages.
 *
 * PERSPECTIVAL GAP:
 *   From the operator's seat, the constraint feels like a rope — a professional norm that makes their work safer and more competent. From executive leadership, it can feel like a snare during budget cycles — a cost center with no visible return until a catastrophe is averted. From the simulation vendor seat, it looks like a rope with a captive market. The engine computes these divergences from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline operators and safety managers are near the beneficiary end (d ~ 0.2-0.3): they gain competence and organizational resilience. Executive leadership is near symmetric (d ~ 0.5): they pay the costs but capture the risk-reduction benefit. Regulatory bodies are beneficiaries (d ~ 0.2): they gain oversight data without operating the system. Simulation vendors are beneficiaries (d ~ 0.15): they capture revenue from the coordination function. The public is excluded — they bear catastrophic risk if the system fails but have no voice in its design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining rare-event competence without catastrophes) remains live. The constraint has not atrophied — near-miss programs have expanded in scope and sophistication. However, theater ratio drift upward (0.25→0.35) signals creeping performativity: reporting for metrics, simulator updates for marketing. If the coordination function hollows out while the structure persists, mandatrophy risk rises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the near-miss-as-bridge reading a distinct constraint from simulation_as_sufficient and catastrophe_as_necessary, or are these three measurement perspectives on a single competence-retention constraint?',
    'Test ε-invariance: if extractiveness, suppression, and beneficiary structure shift when evaluating the same operational arrangements through different reading lenses, they are distinct constraints requiring separate stories.',
    'If ε-invariance fails, the three readings collapse into one constraint with contested classification; if it holds, they form a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel''s three readings instantiate structurally distinct constraints per the ε-invariance principle').

omega_variable(
    near_miss_sufficiency_threshold,
    'What volume, diversity, and severity of near-miss data are necessary and sufficient to maintain simulator fidelity without catastrophic events?',
    'Longitudinal analysis of HROs that have avoided catastrophes for decades: correlate near-miss reporting rates, investigation depth, and simulator update cycles with performance on rare-event scenarios.',
    'If a threshold exists below which competence degrades, the reading''s sufficiency claim is bounded; if no threshold is detectable, the claim holds as an open-ended coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(near_miss_sufficiency_threshold, empirical, 'Whether near-miss feedback has a quantifiable sufficiency boundary for competence maintenance').

omega_variable(
    simulator_fidelity_decay_without_catastrophe,
    'Does simulator training fidelity decay over decades without catastrophic validation events, even with robust near-miss integration?',
    'Compare simulator scenario libraries and operator performance in organizations with long catastrophe-free periods versus those with recent catastrophic experience, controlling for near-miss program maturity.',
    'If fidelity decays, catastrophe_as_necessary gains structural support; if fidelity holds, near_miss_as_bridge is empirically vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulator_fidelity_decay_without_catastrophe, empirical, 'Long-term simulator validity in the absence of catastrophic grounding events').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1980, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(comp_tr_t1990, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(comp_tr_t2000, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(comp_tr_t2010, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(comp_tr_t2020, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 2020, 0.35).

% Extraction over time
narrative_ontology:measurement(comp_be_t1980, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(comp_be_t1990, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(comp_be_t2000, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(comp_be_t2010, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(comp_be_t2020, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 2020, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1980, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(comp_su_t1990, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(comp_su_t2000, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(comp_su_t2010, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(comp_su_t2020, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 2020, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, information_standard).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.02).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'competence retention exercise' kernel into the hybrid position: near-miss data bridges simulator training and real-world validity. The sibling 'simulation_as_sufficient' claims the bridge is unnecessary (simulators are self-validating); 'catastrophe_as_necessary' claims the bridge is insufficient (only catastrophes provide true validation). All three are distinct constraints with different ε values and beneficiary structures, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
