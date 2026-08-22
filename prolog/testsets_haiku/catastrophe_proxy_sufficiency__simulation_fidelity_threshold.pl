% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Catastrophe-Proxy Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Organizations managing catastrophic-risk systems (nuclear reactors,
 *   aircraft, critical medical infrastructure) cannot train personnel on real
 *   catastrophes. Simulation emerged as the practical alternative. The
 *   constraint states: competence retention is valid IF AND ONLY IF
 *   simulation crosses a fidelity threshold where stress/uncertainty
 *   intensity matches real catastrophe conditions. This reading frames the
 *   threshold as technology-dependent sufficiency requiring continuous
 *   investment. The threshold is not a natural fact but a constructed
 *   benchmark that vendors help define and that creates binary certification
 *   gates. Competence either meets the threshold or does not; intermediate or
 *   hybrid competence is excluded from the validity set.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors: institutional agenda-setters, define threshold benchmarks, accrue licensing revenue
 *   - safety_compliance_authorities: institutional beneficiaries, adopt fidelity threshold as auditable standard, enforce compliance
 *   - operational_personnel: moderate-power beneficiaries/payers, train via simulation, must pass threshold certification
 *   - organizations_with_aging_infrastructure: powerful payers, fund continuous platform upgrades despite incompleteness of simulation coverage
 *   - field_practitioners_tacit_knowledge: excluded, their decades of competence unvalidated in simulation metrics
 *   - next_generation_operators: powerless/identity-locked, cannot enter profession without threshold certification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.62).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.71).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Catastrophe-Proxy Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'de17aec7-e066-4aa5-9471-123b176ce5fd').
narrative_ontology:cs_kernel_codification('de17aec7-e066-4aa5-9471-123b176ce5fd', distributed).
narrative_ontology:cs_authority_grounding('de17aec7-e066-4aa5-9471-123b176ce5fd', extraction).
narrative_ontology:cs_interpretation_layer_present('de17aec7-e066-4aa5-9471-123b176ce5fd').
narrative_ontology:cs_reading_relation('de17aec7-e066-4aa5-9471-123b176ce5fd', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('de17aec7-e066-4aa5-9471-123b176ce5fd', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_reading_relation('de17aec7-e066-4aa5-9471-123b176ce5fd', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('de17aec7-e066-4aa5-9471-123b176ce5fd', foundational, simulation_sufficiency_technology_dependent).
narrative_ontology:cs_axiom_status(simulation_sufficiency_technology_dependent, holdable).
narrative_ontology:cs_axiom_grounding('de17aec7-e066-4aa5-9471-123b176ce5fd', simulation_sufficiency_technology_dependent, instrumental).
narrative_ontology:cs_axiom('de17aec7-e066-4aa5-9471-123b176ce5fd', foundational, fidelity_threshold_binary_gate).
narrative_ontology:cs_axiom_status(fidelity_threshold_binary_gate, holdable).
narrative_ontology:cs_axiom_grounding('de17aec7-e066-4aa5-9471-123b176ce5fd', fidelity_threshold_binary_gate, conventional).
narrative_ontology:cs_reference_frame('de17aec7-e066-4aa5-9471-123b176ce5fd', catastrophe_simulation_substitutability_with_technology_investment).
narrative_ontology:cs_drift_state('de17aec7-e066-4aa5-9471-123b176ce5fd', contemporary_vendor_platform_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('de17aec7-e066-4aa5-9471-123b176ce5fd', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_compliance_authorities).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, organizations_with_aging_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel_critical_systems).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel_critical_systems).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, next_generation_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and markets high-fidelity simulation platforms that recreate catastrophic scenarios (reactor meltdowns, aircraft structural failures, medical trauma). Sets the standard by which simulation 'sufficiency' is measured. Controls the certification pathway and defines what fidelity threshold qualifies competence maintenance. Accrues revenue from licensing, upgrades, and training infrastructure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Regulates safety by mandating competence maintenance for personnel managing catastrophic-risk systems (nuclear, aviation, critical infrastructure). Adopts simulation fidelity threshold as the compliance standard because it is measurable, auditable, and avoids the ethical and practical barrier to deliberately creating real catastrophes for training. Certifies vendors' platforms and enforces periodic retraining.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_compliance_authorities, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_compliance_authorities, agenda_setter).

% Must maintain competence through simulation because real catastrophes are rare (by design) and ethically cannot be used for training. Benefit from structured, repeatable, risk-free practice. Pay through mandatory training time, licensing fees, and organizational compliance infrastructure. Their competence certification depends on passing simulations that cross the fidelity threshold.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel_critical_systems, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel_critical_systems, payer).

% Operate decades-old nuclear facilities, aircraft, or critical infrastructure with aging sensor and control systems that are difficult or expensive to retrofit into modern high-fidelity simulations. Must fund continuous platform upgrades and pay licensing fees to maintain certified competence, even when their actual hardware cannot fully be represented in the simulation environment. Cannot exit without shutting down operations.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, organizations_with_aging_infrastructure, payer,
    powerful, biographical, constrained, global).

% Experienced operators who maintain competence through decades of lived experience, heuristic learning, and intuitive pattern recognition developed under real but non-catastrophic operating conditions. Are systematically excluded from competence validation because their tacit knowledge cannot be directly measured in simulation metrics. Their expertise is treated as insufficient even when their actual safety record is superior.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, field_practitioners_tacit_knowledge, excluded,
    moderate, biographical, trapped, local).

% Cannot enter the profession without certification via simulation-fidelity threshold. Must accept the constraint's definition of competence as prerequisite to employment. Their professional identity is constituted through the certified pathway; rejecting simulation sufficiency means rejecting entry to the career itself. No alternative competence-validation pathway exists.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, next_generation_operators, payer,
    powerless, biographical, identity_locked, global).

% Empirically study whether simulation fidelity actually correlates with catastrophe-response competence and whether the threshold is calibrated correctly. Publish findings that sometimes contradict the sufficiency claim. Their research informs the broader epistemic challenge but does not control certification standards.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_inadequacy_researchers, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates competence maintenance across distributed organizations managing catastrophic-risk systems. Replaces ethically impossible real-catastrophe training with reproducible, scalable, technology-enabled simulation. Ensures personnel readiness without deliberately creating harm.
% TRANSFER_FUNCTION: Moves licensing revenue and training fees from organizations and personnel to simulation vendors. Moves compliance authority from individual operator judgment to technology-vendor definitions of sufficiency. Moves professional credentialing from mixed tacit-and-explicit knowledge to technology-mediated assessment.
% ABSENT_VOICES: Field practitioners with decades of tacit experience are structurally excluded from defining sufficiency — their lived competence cannot be quantified in simulation metrics and is thus treated as invalid. Researchers whose findings contradict the fidelity-threshold claim are present but not decision-making seats; they document the constraint but do not control it.
% DISAPPEARANCE_RATIONALE: If the simulation-fidelity threshold vanished and reverted to tacit-knowledge competence assessment, simulation vendors would lose licensing revenue streams, compliance authorities would lose auditable metrics, and organizations could train personnel through mixed mechanisms (mentorship, on-the-job learning, lower-cost simulators). The personnel competence ecosystem would reorganize around distributed validation rather than centralized technology standards.
% FOUNDING_PROBLEM: Real catastrophes are rare and ethically cannot be used for training; competence must be maintained between the decades-long intervals between actual events. Simulation emerged as the only practical substitute that allows personnel to practice responses to extreme scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and vendors attest the founding problem is live and necessitates the fidelity threshold. Researchers and practitioners document that competence has been maintained in systems with older, lower-fidelity simulators; that correlation between simulation fidelity and actual catastrophe response is not yet established; and that the threshold may be calibrated to vendor capabilities rather than operational necessity. Outside-beneficiary corroboration: accident investigation boards find that experienced personnel trained on lower-fidelity systems have sometimes responded more effectively than newly certified personnel, suggesting sufficiency may not be monotonic in simulation fidelity.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at endpoint) because the constraint creates a binary sufficiency gate controlled by technology vendors and compliance authorities, whose definitions of 'crossing the threshold' can be tightened over time without technical justification. Suppression is high (0.71) because alternative competence pathways (tacit knowledge, lower-cost simulation, mentorship) are actively suppressed through regulatory enforcement and professional gatekeeping. Theater is moderate-high (0.48) and rising: certification exercises become increasingly focused on passing threshold metrics rather than building real catastrophe-response capability; the elaborate simulation infrastructure performs the 'we are prepared' narrative more than it ensures preparedness. The measurement series models the constraint tightening over 40 years as vendors upgrade platforms and authorities increase fidelity requirements. The inflection point at t=30 reflects saturation: further threshold increases produce diminishing gains in actual competence but rising compliance costs, revealing the theater mechanism.
 *
 * PERSPECTIVAL GAP:
 *   Vendors and authorities compute the constraint as genuine coordination: simulation technology made catastrophe training possible and competence maintenance scalable. Operators and aging-infrastructure organizations compute it as enforced extraction: they pay for continuous upgrades to simulators that incompletely represent their actual systems, and professional gatekeeping excludes experienced tacit-knowledge practitioners. Field practitioners and researchers see a false sufficiency: simulation that looks impressive to auditors does not predict actual catastrophe response. The engine computes per-seat types from directionality (beneficiary vs. payer vs. excluded), modeling these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors are near-pure beneficiaries (d near 0.0): they set thresholds, define sufficiency, and collect recurring licensing revenue with minimal cost to themselves. Safety authorities are beneficiaries (d near 0.1-0.2): they gain auditable metrics and delegated risk. Operators and personnel are mixed: genuine benefit from organized training, but trapped in recurring upgrade costs and binary certification gates (d near 0.5-0.7). Field practitioners are targets of suppression (d near 1.0): their competence is invalidated, their exit options are eliminated, they bear the cost of regime change without benefit. Next-generation operators are identity-locked targets (d near 0.95): professional identity is constituted through the certified pathway; they cannot exit without rejecting the career itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence maintenance without real catastrophes) is live. The fidelity-threshold solution solves it partially but has accumulated secondary functions: regulatory gatekeeping, vendor rent extraction, exclusion of tacit knowledge. The constraint's persistence is not threatened by the founding problem's resolution; instead, the threshold mechanism persists by continuously raising the bar (theater ratio rising) without proportional increases in actual competence. Mandatrophy is present but partial: the constraint has not completely lost its founding function (simulation does maintain competence in most scenarios), but the ratio of genuine coordination to enforcement/extraction has degraded over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_versus_capability,
    'Is the fidelity threshold calibrated to actual disaster-response capability, or is it calibrated to what current technology vendors can economically deliver?',
    'Prospective validation study: deploy personnel trained on lower-fidelity simulators into high-consequence incidents (or controlled high-fidelity incidents) and measure actual catastrophe-response performance against threshold-certified personnel. Compare error rates, decision quality, and outcome metrics.',
    'If lower-fidelity training produces equivalent or superior outcomes, the threshold is vendor-calibrated and represents extractive gatekeeping rather than genuine sufficiency condition. Classification would shift toward snare. If threshold-certified personnel demonstrably outperform, the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_versus_capability, empirical, 'Whether the fidelity threshold represents actual competence requirements or vendor economic optimization').

omega_variable(
    tacit_knowledge_suppression_mechanism,
    'Is tacit knowledge from experienced operators structurally excluded because it genuinely cannot transfer to new personnel, or because it cannot be measured in simulation metrics and thus conflicts with the threshold''s binary gate?',
    'Ethnographic/comparative study of organizations that mix tacit-knowledge mentorship with simulation (where permitted). Measure whether competence and safety outcomes improve with hybrid training versus simulation-only pathways.',
    'If hybrid pathways produce better outcomes and are suppressed for metric convenience rather than safety necessity, the constraint is identified as false sufficiency. The suppression is revealed as driven by the need to maintain vendor control and auditable metrics, not by genuine competence requirements. Would support reclassification to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_suppression_mechanism, empirical, 'Whether exclusion of tacit knowledge is safety-necessary or metric-convenience-driven').

omega_variable(
    simulation_aging_infrastructure_incompleteness,
    'For organizations with decades-old nuclear reactors, aircraft, or critical infrastructure whose sensors and control systems cannot be fully represented in modern high-fidelity simulation: does the incomplete simulation still cross the fidelity threshold, and if so, is the threshold still meaningful?',
    'Gap analysis: identify the structural/operational elements of aging systems that cannot be represented in current simulators, assess the safety criticality of those gaps, and determine whether threshold sufficiency can hold despite known incompleteness.',
    'If the threshold is crossed despite known gaps in simulation coverage, the threshold is revealed as a regulatory theater mechanism — it certifies sufficiency that is not actually achieved. If compliance authorities acknowledge the gaps but enforce the threshold anyway, the constraint becomes pure extraction (paying for the appearance of certification, not actual competence). Would support snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_aging_infrastructure_incompleteness, empirical, 'Whether the fidelity threshold can claim sufficiency given known incompleteness in aging-infrastructure simulation coverage').

omega_variable(
    kernel_reading_disagreement_locus,
    'Do the disagreements between this reading and its siblings locate in empirical disputes about simulation effectiveness, or in fundamental commitments about whether real catastrophes are irreducibly necessary for competence?',
    'Analyze the logical structure of each reading''s core claim. Catastrophe_necessity_reading asserts simulation cannot substitute (deontological: real catastrophes are necessary). This reading asserts simulation can substitute if the technology reaches a threshold (instrumental: efficacy depends on means). Hybrid_degradation_reading asserts tacit knowledge decays regardless (empirical: generational competence loss). Simulation_as_proxy_catastrophe_reading asserts current simulation suffices (empirical: existing technology is adequate).',
    'If the disagreement is empirical (can we measure whether simulation works), the readings coexist and can be resolved by evidence. If foundational (is real catastrophe necessary on principle), the readings foreclose each other. The mismatch consumer (six_questions.founding_problem_status x disappearance_verdict) will detect unresolved mandatrophy when simulation sufficiency status is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether sibling readings are empirically resolvable or logically exclusive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 5, 0.49).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.18).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-reading decomposition of the catastrophe_proxy_sufficiency kernel. Each reading offers a structurally distinct answer to the question of whether simulation can substitute for real catastrophic events in competence maintenance. This reading (simulation_fidelity_threshold) asserts technology-dependent sufficiency calibrated to a measurable threshold. It influences all siblings because: (1) it sets the operational benchmark that catastrophe_necessity_reading must argue against, (2) it competes directly with simulation_as_proxy_catastrophe_reading on the question of current adequacy, and (3) it creates the framework that hybrid_degradation_reading must contextualize as a generational timescale phenomenon. The four readings form an epistemic cluster where each reading's credibility depends partly on the others' structural coherence; they do not converge but rather map the space of live positions on catastrophe substitutability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
