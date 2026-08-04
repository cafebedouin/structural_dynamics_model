% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident Necessity Doctrine for Competence Occupation
 *   domain: organizational/safety_science/high_reliability
 *
 * SUMMARY:
 *   The competence_occupation kernel concerns how high-reliability
 *   organizations maintain valid operator competence for rare catastrophic
 *   events. The real_incident_necessity reading asserts that only authentic
 *   catastrophic exposure provides the necessary conditions to occupy
 *   (maintain) the competence kernel—creating an unresolvable paradox, since
 *   catastrophes are precisely what these organizations exist to prevent. The
 *   constraint functions as a doctrinal limit: it is presented as a natural
 *   feature of human cognition under stress, yet it sustains an industry of
 *   realism vendors and a research paradigm centered on catastrophic stress.
 *   This story claims Mountain (natural limit) while authoring metrics that
 *   reflect substantial extraction, suppression, and theater—inviting FSM
 *   evaluation.
 *
 * KEY AGENTS:
 *   - safety_science_academy: agenda_setter (institutional/arbitrage/global) — promulgates the doctrine that authentic stress is irreducible
 *   - realism_training_vendors: beneficiary (organized/mobile/global) — captures revenue from the insufficiency of ordinary training
 *   - hro_operators: payer (institutional/constrained/national) — bears the cost of compliance with an impossible standard
 *   - frontline_practitioners: payer (moderate/identity_locked/local) — carries the burden of untested competence
 *   - simulation_advocates: excluded (organized/constrained/global) — argues for simulation sufficiency, marginalized
 *   - regulatory_bodies: observer (institutional/analytical/national) — caught between doctrinal adherence and operational feasibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.62).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.71).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Necessity Doctrine for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety_science/high_reliability").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '3ab78af7-579b-4525-a2bd-7e45bb4eff4e').
narrative_ontology:cs_kernel_codification('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', distributed).
narrative_ontology:cs_authority_grounding('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', expertise).
narrative_ontology:cs_interpretation_layer_present('3ab78af7-579b-4525-a2bd-7e45bb4eff4e').
narrative_ontology:cs_reading_relation('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', foundational, only_authentic_stress_constitutes_competence).
narrative_ontology:cs_axiom_status(only_authentic_stress_constitutes_competence, holdable).
narrative_ontology:cs_axiom_grounding('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', only_authentic_stress_constitutes_competence, empirically_contingent).
narrative_ontology:cs_axiom('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', foundational, catastrophic_fidelity_irreducible).
narrative_ontology:cs_axiom_status(catastrophic_fidelity_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', catastrophic_fidelity_irreducible, empirically_contingent).
narrative_ontology:cs_reference_frame('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', authentic_stress_exposure_competence).
narrative_ontology:cs_drift_state('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3ab78af7-579b-4525-a2bd-7e45bb4eff4e', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, realism_training_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, safety_science_academy).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, hro_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and sells high-fidelity simulation equipment, stress-exposure curricula, and authentic-experience training programs to high-reliability organizations. Revenue depends on the doctrine that ordinary simulation and rehearsal are insufficient for genuine competence validation.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, realism_training_vendors, beneficiary,
    organized, biographical, mobile, global).

% Conducts research on human performance under extreme stress, often concluding that authentic high-stakes exposure is irreplaceable. Sets training doctrine through peer-reviewed consensus and curriculum standards. Receives research funding and institutional prestige from the realism paradigm.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_science_academy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, safety_science_academy, beneficiary).

% Operates nuclear plants, air traffic systems, hospitals, and other high-consequence environments. Must demonstrate regulatory compliance for competence maintenance while facing the structural impossibility of providing authentic catastrophic experience. Pays for ever-more-elaborate training infrastructure.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_operators, payer,
    institutional, generational, constrained, national).

% Pilots, surgeons, control-room operators, and others whose competence is formally questioned because they have not faced actual catastrophe. Must participate in increasingly realistic training that cannot replicate the existential conditions asserted as necessary. Carries the psychological burden of untested status.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Researchers and practitioners who argue that structured simulation, deliberate practice, and procedural rehearsal can fully maintain competence without catastrophic exposure. Marginalized in safety-critical discourse as unrealistic or dangerous. Largely excluded from standard-setting bodies.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_advocates, excluded,
    organized, biographical, constrained, global).

% Licenses and inspects high-reliability organizations, often adopting safety-science doctrine on competence validation. Observes the tension between requiring demonstrable competence and prohibiting the catastrophic conditions that would demonstrate it.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, realism_training_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standard for what counts as genuine competence validation in environments where routine operations give no feedback on crisis performance.
% TRANSFER_FUNCTION: Moves epistemic authority and organizational resources from operators and simulation advocates to realism-training vendors and safety-science institutions, while transferring the burden of proof to frontline practitioners.
% ABSENT_VOICES: Simulation-sufficiency researchers; operators in domains with zero catastrophic history; cost-constrained public-sector HROs that cannot afford ultra-high-fidelity training.
% DISAPPEARANCE_RATIONALE: Organizations would shift competence validation toward engineering-grade simulation and structured rehearsal; the realism-training market would contract; frontline practitioners would be validated through demonstrated procedural fluency rather than catastrophe exposure.
% FOUNDING_PROBLEM: Genuine operator competence for rare, high-consequence events atrophies without high-stakes feedback, and normal operations provide no opportunity to test or refresh crisis skills.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigators and operational researchers outside the realism-training industry attest that rare-event skill degradation is a recurrent finding; however, they do not uniformly corroborate that only real catastrophes can address it.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored to reflect the permanent resource drain toward realistic training and the devaluation of non-catastrophic competence. Suppression (0.71) reflects the epistemic suppression of simulation-sufficiency arguments in safety-critical discourse—framed as recklessness. Theater_ratio (0.58) captures the performative construction of ever-more-elaborate simulations that cannot achieve the asserted authenticity. Accessibility_collapse (0.88) is high because once the only real catastrophes frame is accepted, simulation alternatives are structurally dismissed as inadequate. Resistance (0.42) is moderate because a substantive simulation-research community continues to contest the doctrine. The metrics and claim are independent: the claim is Mountain (the doctrine's own self-presentation), the metrics describe its extractive operation.
 *
 * PERSPECTIVAL GAP:
 *   The safety_science_academy and realism_training_vendors experience this constraint as a genuine discovery about human performance limits—a Mountain they are helping organizations navigate. Frontline_practitioners and hro_operators experience it as a no-win standard that either demands unacceptable events or perpetual inadequacy. The engine computes this divergence from the structural data: beneficiaries with mobile or arbitrage exit versus victims with constrained or identity_locked exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the training-vendor sector and the research academy that collects funding and prestige from the realism paradigm. Victim declarations map to the organizations and practitioners who must pay for compliance with a standard they cannot safely meet. The directionality derivation will produce low d for the academy and vendors (subsidized by the constraint) and high d for operators and practitioners (targeted by it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—rare-event skill atrophy—is genuinely live, corroborated by accident investigations. This prevents classifying the constraint as a pure Snare (the problem is not fabricated). However, the mandate that ONLY real catastrophes suffice may have outlived its validity given advances in simulation and deliberate-practice research. The classification as claimed Mountain with authored extraction metrics captures this tension: a problem-grounded constraint that has become a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_limit_vs_constructed_doctrine,
    'Is the requirement for authentic catastrophic exposure a genuine cognitive limit of human performance under stress, or a constructed epistemic standard that benefits the realism-training industry?',
    'Meta-analysis of transfer studies comparing high-fidelity simulation to real-incident performance; regulatory natural experiments where jurisdictions adopted simulation-only standards.',
    'If genuine limit, the Mountain claim strengthens and extraction metrics require re-evaluation as necessary coordination cost; if constructed, FSM reclassification to tangled_rope or snare is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_constructed_doctrine, empirical, 'Whether the real-incident necessity is a natural law of cognition or a constructed scarcity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of simulation-sufficiency voices structural (career penalties, funding exclusion) or internalized (safety culture genuinely views dissent as recklessness)?',
    'Anonymous surveys of safety professionals on simulation views; analysis of peer-review outcomes and funding allocation by doctrinal position.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s self-maintenance is ideological; if purely structural, reform may follow institutional incentive changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of simulation alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.38).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.45).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.52).
narrative_ontology:measurement(comp_tr_t32, competence_occupation__real_incident_necessity, theater_ratio, 32, 0.56).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(comp_be_t32, competence_occupation__real_incident_necessity, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(comp_su_t8, competence_occupation__real_incident_necessity, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(comp_su_t16, competence_occupation__real_incident_necessity, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(comp_su_t24, competence_occupation__real_incident_necessity, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(comp_su_t32, competence_occupation__real_incident_necessity, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel decomposes into three structurally distinct constraints because the natural-language phrase competence maintenance conflates incompatible epistemic standards. real_incident_necessity asserts a Mountain-like natural limit on learning; simulation_sufficiency asserts a Rope-like coordination through engineered practice; hybrid_occupation asserts a Tangled Rope of contested multi-mechanism coordination. Each reading carries a different epsilon, beneficiary structure, and victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
