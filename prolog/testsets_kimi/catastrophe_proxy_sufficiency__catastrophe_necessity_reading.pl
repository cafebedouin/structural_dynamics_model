% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Genuine Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the catastrophe-necessity reading of the
 *   catastrophe_proxy_sufficiency kernel: the claim that only actual
 *   catastrophic events generate the irreducible stress and uncertainty
 *   required to maintain genuine operational competence, rendering simulation
 *   categorically insufficient. It is claimed as a Mountain â a
 *   psychophysiological and organizational limit akin to a natural law within
 *   high-reliability organization (HRO) theory. The narrative 'victim' is
 *   operational safety margins, which decay during catastrophe-free periods;
 *   however, this is an abstract systemic outcome rather than an agent, so
 *   the constraint carries no beneficiary or victim declarations in the
 *   structural layer. The reading competes with simulation-sufficiency,
 *   hybrid-degradation, and fidelity-threshold readings of the same kernel.
 *
 * KEY AGENTS:
 *   - high_reliability_operators: Analytical observer seat â experienced personnel who attest that only real catastrophe produces requisite stress inoculation and tacit competence
 *   - simulation_technologists: Excluded voice â commercial and research advocates of high-fidelity simulation who dispute the irreducibility claim and are structurally absent from catastrophe-centric doctrine
 *   - safety_regulators: Observer seat â institutional actors who must set training standards without direct access to catastrophe experience and who mediate between competing empirical claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.05).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity for Genuine Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '16863daa-19f9-4ce5-8337-97b244293db5').
narrative_ontology:cs_kernel_codification('16863daa-19f9-4ce5-8337-97b244293db5', distributed).
narrative_ontology:cs_authority_grounding('16863daa-19f9-4ce5-8337-97b244293db5', expertise).
narrative_ontology:cs_interpretation_layer_present('16863daa-19f9-4ce5-8337-97b244293db5').
narrative_ontology:cs_reading_relation('16863daa-19f9-4ce5-8337-97b244293db5', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('16863daa-19f9-4ce5-8337-97b244293db5', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('16863daa-19f9-4ce5-8337-97b244293db5', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('16863daa-19f9-4ce5-8337-97b244293db5', foundational, actual_catastrophe_irreducible_for_competence).
narrative_ontology:cs_axiom_status(actual_catastrophe_irreducible_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('16863daa-19f9-4ce5-8337-97b244293db5', actual_catastrophe_irreducible_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('16863daa-19f9-4ce5-8337-97b244293db5', foundational, simulation_categorically_insufficient_for_genuine_competence).
narrative_ontology:cs_axiom_status(simulation_categorically_insufficient_for_genuine_competence, holdable).
narrative_ontology:cs_axiom_grounding('16863daa-19f9-4ce5-8337-97b244293db5', simulation_categorically_insufficient_for_genuine_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('16863daa-19f9-4ce5-8337-97b244293db5', catastrophe_dependent_competence).
narrative_ontology:cs_drift_state('16863daa-19f9-4ce5-8337-97b244293db5', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('16863daa-19f9-4ce5-8337-97b244293db5', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Not applicable as an inter-agent arrangement; the constraint describes a claimed psychophysiological and organizational limit rather than coordinating action among parties. Safety coordination occurs through separate training and protocol constraints.
% TRANSFER_FUNCTION: No inter-agent transfer; the constraint describes a unidirectional decay function in which operational competence and safety margins erode in the absence of irreducible catastrophic stress.
% ABSENT_VOICES: Simulation technology vendors and training-platform providers, who would argue that advanced simulation can replicate stress responses; junior safety practitioners trained exclusively via simulation who have no catastrophe experience against which to evaluate the claim; researchers advocating fidelity-threshold sufficiency.
% DISAPPEARANCE_RATIONALE: If actual catastrophe were not necessary for competence, high-reliability organizations would restructure training budgets, regulatory frameworks, and career pipelines around continuous high-fidelity simulation. The institutional status of catastrophe-experienced operators would decline, and safety regimes would shift from episodic, event-dependent learning to perpetual simulated exposure.
% FOUNDING_PROBLEM: Maintenance of genuine operational competence in safety-critical domains during long catastrophe-free intervals.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigators and safety researchers outside the simulation industry attest that prolonged calm periods correlate with normalization of deviance and skill atrophy; empirical literature on expertise decay under low-arousal conditions supports the problem's persistence.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the constraint describes a claimed natural limit rather than an extracting arrangement. Suppression is negligible (0.05) because the limit is not enforced by any party but is asserted as empirical fact about human stress physiology. Accessibility collapse is high (0.92) because, if the claim is true, no alternative training modality can substitute for actual catastrophe â the alternatives collapse by physical limit, not by enforcement. Resistance is low (0.08) because the claim itself is not actively resisted outside commercial simulation interests; the primary contest is empirical, not political. Theater ratio is minimal (0.05) because there is no enforcement performance to maintain.
 *
 * PERSPECTIVAL GAP:
 *   Since the constraint is a claimed Mountain with no agent parties, there is no payer-beneficiary divergence. The relevant perspectival gap is epistemic: operators with catastrophe experience report qualitatively different stress responses than simulation-trained practitioners, creating an observer-axis split that empirical measurement has not fully resolved. The engine computes no per-seat classification divergence because there are no seated agents; the divergence exists in the analytical layer between experience-based and technology-based epistemic communities.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation is triggered because the constraint declares no agent beneficiaries or victims. The abstract cost (competence decay) falls on operational safety margins â a systemic property, not a seated agent â and the putative benefit (competence retention via catastrophe exposure) is an unagented natural outcome. The constraint is therefore directionally flat across all power atoms, reverting to the canonical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The primary mandatrophy risk for this constraint is false-summit Mountain classification: if the irreducibility claim is not a genuine psychophysiological limit but rather a professional mythology that protects the status of catastrophe-experienced operators and suppresses simulation investment, the constraint would reclassify as Snare or Tangled Rope. The R5 genealogy interview shows the founding problem â competence decay in calm periods â is corroborated as live by accident investigators outside the benefiting parties, but the specific solution (catastrophe necessity) is contested by simulation research. Because founding_problem_status is live and disappearance_verdict is world_rearranges, there is no dead-problem mismatch flag; the constraint avoids mandatrophy mislabeling as a zombie piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducibility_empirical_or_contingent,
    'Is the insufficiency of simulation for maintaining genuine competence a fixed psychophysiological limit, or a contingent limitation of current technology and pedagogy?',
    'Longitudinal studies comparing catastrophe-experienced and high-fidelity-simulation-only operators across identical scenarios, measuring stress biomarkers, decision latency, and error rates under unexpected novel failures.',
    'If the limit is contingent on current technology, the constraint reclassifies from Mountain to Scaffold or Tangled Rope as simulation fidelity improves; if fixed, it remains a Mountain regardless of technological advance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreducibility_empirical_or_contingent, empirical, 'Whether simulation insufficiency is a natural limit or a contingent gap').

omega_variable(
    competence_measurement_circularity,
    'Can genuine competence be measured independently of exposure to actual catastrophe, or is the metric itself defined post-hoc by catastrophe performance?',
    'Development and validation of competency metrics that predict catastrophe performance without requiring catastrophe exposure for calibration.',
    'If competence is only verifiable by catastrophe outcomes, the constraint risks unfalsifiability and false-summit detection becomes harder; if independently measurable, the hypothesis becomes testable and the constraint can be evaluated against evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_measurement_circularity, conceptual, 'Risk of circular definition in competence assessment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is the catastrophe-necessity reading of the catastrophe_proxy_sufficiency kernel, holding that actual catastrophic events are irreducibly necessary for competence. Sibling readings decompose the kernel into simulation-sufficiency, hybrid-degradation, and fidelity-threshold claims. The kernel conflates empirical claims about stress psychophysiology, technological claims about simulation fidelity, and institutional claims about training efficacy; the decomposition separates these into structurally distinct, testable constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
