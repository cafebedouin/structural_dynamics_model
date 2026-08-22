% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority — Proportionality-Balancing Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the public health
 *   mandate authority kernel: legitimacy is not fixed to a categorical
 *   collective-benefit obligation (public_health_primary) nor categorically
 *   denied (bodily_autonomy_primary), but is a function of a sliding scale —
 *   threat severity, availability of less-restrictive alternatives, magnitude
 *   of coercion, and duration of imposition. Because ε is a fixed intrinsic
 *   property per DP-001, this story is authored at a mid-severity operating
 *   point representative of a moderately severe respiratory pathogen with
 *   meaningful but not catastrophic case-fatality and real (if imperfect)
 *   less-restrictive alternatives (masking, testing, ventilation) — not the
 *   Ebola-level extreme and not the mild-seasonal-virus extreme. The
 *   measurement series traces a single hypothetical outbreak arc (rising
 *   threat, mandate imposition, capacity crisis, de-escalation, and a
 *   late-interval uptick representing a second wave) to show how the SAME
 *   governing doctrine produces DIFFERENT extraction values as the threat
 *   parameter moves — this is the doctrine's whole claimed function, not
 *   drift or Goodhart substitution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.42).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.5).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority — Proportionality-Balancing Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '84deb0a8-e465-4d10-9166-e280621b5e8a').
narrative_ontology:cs_kernel_codification('84deb0a8-e465-4d10-9166-e280621b5e8a', distributed).
narrative_ontology:cs_authority_grounding('84deb0a8-e465-4d10-9166-e280621b5e8a', practice).
narrative_ontology:cs_interpretation_layer_present('84deb0a8-e465-4d10-9166-e280621b5e8a').
narrative_ontology:cs_reading_relation('84deb0a8-e465-4d10-9166-e280621b5e8a', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_reading_relation('84deb0a8-e465-4d10-9166-e280621b5e8a', public_health_mandate_authority__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('84deb0a8-e465-4d10-9166-e280621b5e8a', foundational, legitimacy_is_a_function_of_threat_calibrated_variables).
narrative_ontology:cs_axiom_status(legitimacy_is_a_function_of_threat_calibrated_variables, holdable).
narrative_ontology:cs_axiom_grounding('84deb0a8-e465-4d10-9166-e280621b5e8a', legitimacy_is_a_function_of_threat_calibrated_variables, instrumental).
narrative_ontology:cs_axiom('84deb0a8-e465-4d10-9166-e280621b5e8a', secondary, no_mandate_is_categorically_permanent_or_categorically_forbidden).
narrative_ontology:cs_axiom_status(no_mandate_is_categorically_permanent_or_categorically_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('84deb0a8-e465-4d10-9166-e280621b5e8a', no_mandate_is_categorically_permanent_or_categorically_forbidden, conventional).
narrative_ontology:cs_reference_frame('84deb0a8-e465-4d10-9166-e280621b5e8a', jacobson_era_rational_basis_deference).
narrative_ontology:cs_drift_state('84deb0a8-e465-4d10-9166-e280621b5e8a', post_covid19_heightened_scrutiny_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('84deb0a8-e465-4d10-9166-e280621b5e8a', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_capacity_planners).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals_low_threat_context).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, medically_contraindicated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, workers_facing_employment_conditioned_compliance).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, proportionality_as_constitutional_limiting_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate scope, duration, and exemption criteria based on a declared threat assessment (transmissibility, case fatality, healthcare capacity strain). Recalibrates mandates as threat data changes and is the seat that must justify the sliding-scale weighting to courts and legislatures. Bears reputational and legal cost if the scale is miscalibrated in either direction.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Cannot generate independent immunity and depend on population-level transmission reduction (herd protection) for basic safety in shared spaces, workplaces, and healthcare settings. Have no exit from exposure risk other than isolation, which is its own severe cost. Benefit directly and disproportionately when the mandate is proportionate to a genuine high-severity threat; gain nothing when mandates persist against a low-severity pathogen, since transmission risk to them was never really the binding constraint at that threat level.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, immediate, trapped, local).

% Manage hospital bed, ICU, and staffing capacity against surge risk. Benefit from mandates that flatten transmission curves during genuine capacity crises; the coordination function of the mandate is real and measurable in occupancy data during high-severity intervals. Have no reason to support the mandate once capacity risk subsides, and often say so publicly, which is a check against theatrical persistence.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_capacity_planners, beneficiary,
    organized, biographical, constrained, regional).

% Bear employment, travel, and social-access restrictions tied to compliance. Under this reading, whether they are legitimately a victim group depends entirely on the proportionality assessment at the time: the same restriction that is proportionate against an Ebola-level pathogen becomes disproportionate coercion against a mild seasonal virus. Their exit is constrained by employer or state conditioning of participation on compliance, not by any structural inability to exit — the coercion magnitude is what the sliding scale is meant to bound.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals_low_threat_context, payer,
    moderate, biographical, constrained, national).

% Cannot safely comply due to documented medical conditions, yet mandate exemption processes are frequently narrow, slow, or contested, leaving them exposed to the same access and employment consequences as voluntary non-compliers. Under a well-calibrated proportionality reading, this group should never bear the coercion; when they do, it signals a scale-implementation failure rather than a scale-design failure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, medically_contraindicated_individuals, payer,
    powerless, immediate, trapped, local).

% Face job loss or suspension for non-compliance regardless of personal risk assessment, because employers implement mandates categorically rather than proportionately to the worker's actual transmission risk (e.g., remote workers held to office-worker rules). The magnitude-of-coercion axis of the sliding scale is meant to catch this over-application but frequently does not, because employer-level implementation lags the underlying threat reassessment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, workers_facing_employment_conditioned_compliance, payer,
    moderate, biographical, constrained, national).

% Adjudicate whether a specific mandate's severity, alternatives, coercion magnitude, and duration are proportionate to the declared threat. Their rulings are the mechanism by which the sliding scale is operationalized and checked; a court finding of disproportionality forces recalibration or rescission.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% Benefit financially from mandate-driven demand but are not parties to the proportionality assessment itself — courts and agencies do not weigh manufacturer interest as a legitimate input to the scale, even though sustained mandates affect their revenue. Their financial stake is real but structurally excluded from the legitimacy calculus, which is part of what keeps the reading analytically clean from capture.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_or_intervention_manufacturers, excluded,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Calibrating mandate severity, duration, and coercion magnitude to the actual severity of the threat solves the problem of mandates persisting past their justification (or being imposed disproportionately relative to a mild threat) — it is the mechanism that is supposed to prevent both under-response to genuine emergencies and over-coercion once risk subsides.
% TRANSFER_FUNCTION: When correctly calibrated, the arrangement transfers reduced transmission risk to vulnerable populations and preserved healthcare capacity to the commons, financed by bounded, time-limited restrictions on individual liberty. When miscalibrated, it transfers employment, mobility, and bodily-decision costs from institutions and healthy populations onto individuals whose actual risk contribution does not justify the coercion magnitude imposed.
% ABSENT_VOICES: Individuals who are medically contraindicated but caught in categorical rather than individualized enforcement are rarely represented in the policy-setting process; exemption boards are frequently under-resourced relative to the volume of claims. Vaccine or intervention manufacturers are excluded by design, which is appropriate, but means the scale's threat-severity inputs are sometimes shaped by parties with a financial interest in threat framing without being visible as an interested party in the record.
% DISAPPEARANCE_RATIONALE: If the proportionality-balancing framework disappeared, public health agencies would default to either the public_health_primary reading (mandates justified categorically by collective benefit, regardless of threat magnitude) or the bodily_autonomy_primary reading (no mandate is ever justified). Courts, healthcare capacity planners, and civil liberties litigants would experience this very differently: capacity planners might see mandates become harder to graduate down once imposed (no scale-based sunset pressure); rights litigants would lose the doctrinal hook currently used to challenge disproportionate impositions. Whether the world 'rearranges' or 'stays the same' depends on which sibling reading fills the vacuum.
% FOUNDING_PROBLEM: Courts and legislatures needed a doctrinal test to distinguish legitimate emergency public health measures from either pretextual overreach or reflexive rights-absolutism, across widely varying pathogen severities and social contexts, without having to relitigate first principles for every outbreak.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars and public health ethicists outside both the enforcing agencies and the litigant advocacy groups (e.g., academic bioethics commentary analyzing Jacobson v. Massachusetts's proportionality lineage) attest that the underlying problem — calibrating coercive public health power against a moving threat baseline — recurs across pathogens and jurisdictions and has not been resolved by any single mandate episode; it remains a live doctrinal problem rather than a historical relic.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, contested).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).
:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and suppression (0.5) sit at moderate values reflecting a mandate whose severity was, on balance, proportionate to a real but not extreme threat, with genuine but imperfect alternatives available. Theater ratio rises through the middle of the interval (peaking at 0.38 around t=20) capturing the well-documented lag between threat de-escalation and mandate rescission — agencies are slower to remove restrictions than to impose them, which is exactly the dynamic the duration axis of the scale is meant to police and exactly where this reading is most vulnerable to sliding toward tangled_rope or snare if courts fail to force recalibration. Accessibility collapse (0.4) is moderate: alternatives to compliance existed (medical exemptions, remote work, testing regimes) but were unevenly available depending on employer and jurisdiction. Resistance (0.58) is substantial and legitimate under this reading — proportionality challenges in court are the doctrine's own error-correction mechanism, not illegitimate obstruction.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and healthcare capacity planners are coded as beneficiaries because the coordination function (transmission reduction, capacity preservation) is real and measurable during the high-threat portion of the interval. Unvaccinated individuals in a low-threat context, medically contraindicated individuals, and workers subject to categorical (non-individualized) employer enforcement are coded as payers because the same restriction, applied past the point of proportional justification or without adequate exemption pathways, becomes extraction rather than coordination. Crucially, under this reading the victim boundary is NOT fixed — it moves with the threat parameter, which is the structural delta this reading is specifically built to capture relative to its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading is precisely a mandatrophy-detection doctrine: it exists to distinguish a mandate whose founding problem (threat containment) is still live from one whose founding problem has resolved but which persists by inertia or institutional risk-aversion (theater). The rising theater_ratio at t=16-20 combined with a declining base_extractiveness is exactly the signature the doctrine is designed to catch and correct via the duration axis — a mandate that should have sunset but has not yet been rescinded. The late uptick at t=24 shows the doctrine correctly re-engaging (extraction rises again) when a genuine second wave restores threat severity, distinguishing this from simple regulatory ratchet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_severity_calibration_authority,
    'Who has legitimate authority to set the threat-severity input to the sliding scale, and is that determination itself subject to capture by agencies with institutional incentive to overstate threat (justifying mandate persistence) or understate it (avoiding political cost of imposition)?',
    'Compare agency threat classifications against independent epidemiological modeling and post-hoc case-fatality/transmissibility data across multiple mandate episodes; look for systematic directional bias.',
    'If threat-severity determinations are systematically biased in one direction, the proportionality reading''s apparent moderation is illusory — the scale is real in doctrine but captured in practice, which would push the computed classification toward tangled_rope or snare regardless of the doctrine''s clean design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_severity_calibration_authority, empirical, 'Whether the threat-severity input to the sliding scale is itself capturable.').

omega_variable(
    reading_boundary_stability,
    'At what threat-severity threshold does this proportionality reading functionally converge with public_health_primary (at high severity) or with bodily_autonomy_primary (at near-zero severity), and is that convergence a feature (the scale working as designed) or a sign that the three readings are not truly distinct constraints at the extremes?',
    'Model the sliding-scale output as threat severity approaches the extremes and compare against the fixed ε values authored for the two categorical sibling readings; check whether the proportionality reading''s ε curve asymptotically approaches the siblings'' fixed values.',
    'If convergence is smooth and asymptotic, this supports treating the three readings as genuinely distinct constraints per DP-001 (ε-invariance) rather than one constraint viewed through three lenses. If convergence is discontinuous or the readings diverge sharply near the boundary, the decomposition itself may need review.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_stability, conceptual, 'Whether the proportionality reading is a genuinely distinct constraint or a continuum bridging its two categorical siblings.').

omega_variable(
    exemption_process_gap,
    'Is the gap between doctrinal design (proportionality should protect medically contraindicated individuals via exemption) and implementation reality (exemption processes are frequently narrow or slow) a temporary administrative failure or a structural feature of how the sliding scale gets operationalized under time pressure?',
    'Track exemption approval rates, processing times, and appeal outcomes across multiple mandate episodes of varying threat severity and duration.',
    'If the gap is structural rather than transitional, medically contraindicated individuals are being extracted from by a doctrine that formally protects them — a false-summit-adjacent pattern where the doctrine''s own legitimacy claim (proportionate, exemption-respecting) masks persistent harm to a specific subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_process_gap, empirical, 'Whether exemption-process failure is transitional or structural to the doctrine''s operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t4, public_health_mandate_authority__proportionality_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__proportionality_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__proportionality_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__proportionality_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(publ_be_t4, public_health_mandate_authority__proportionality_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__proportionality_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__proportionality_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(publ_su_t4, public_health_mandate_authority__proportionality_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__proportionality_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__proportionality_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is the middle reading of a three-story kernel decomposition (public_health_mandate_authority). public_health_primary authors a fixed, higher ε reflecting categorical collective-benefit justification with a narrower victim exemption; bodily_autonomy_primary authors a fixed, near-maximal ε reflecting categorical rejection of any non-consensual mandate regardless of threat. This reading's ε (0.42, averaged across a moderate-severity operating point) is deliberately lower than public_health_primary's and far lower than bodily_autonomy_primary's, and is authored to VARY across the measurement interval as threat severity moves — the structural delta the kernel context specifically calls for. All three stories share the underlying doctrinal subject (Jacobson-line mandate authority) but are structurally distinct constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
