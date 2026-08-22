% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Proportionality-Balanced Public Health Mandate Authority
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   During a declared public health emergency, a jurisdiction imposes
 *   escalating and then de-escalating mandate measures (vaccination, masking,
 *   or movement restriction conditioned on employment or access) calibrated
 *   against a stated severity threshold. As the pathogen's measured severity
 *   declines (case fatality rate drops, treatments become available, or
 *   population immunity rises), the mandate is narrowed and eventually
 *   sunset. The proportionality framework is the mechanism that governs when
 *   and how much coercion is authorized at each severity level — it is not
 *   itself a fixed rule but a standing test applied and re-applied by
 *   agencies and courts across the interval.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter, sets and revises mandate scope against severity data
 *   - immunocompromised_populations: primary structural beneficiary during high-severity phases
 *   - unvaccinated_individuals_low_threat_context: bears coercive cost that becomes disproportionate as severity declines
 *   - immunocompromised_individuals_denied_accommodation: becomes a victim of the same balancing test that otherwise protects the class
 *   - courts_reviewing_mandate_proportionality: analytical/adjudicative seat testing the proportionality claim after the fact
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
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Proportionality-Balanced Public Health Mandate Authority").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, 'f5a2f211-a3a9-416a-90f7-a63df2516f7e').
narrative_ontology:cs_kernel_codification('f5a2f211-a3a9-416a-90f7-a63df2516f7e', distributed).
narrative_ontology:cs_authority_grounding('f5a2f211-a3a9-416a-90f7-a63df2516f7e', lineage).
narrative_ontology:cs_interpretation_layer_present('f5a2f211-a3a9-416a-90f7-a63df2516f7e').
narrative_ontology:cs_reading_relation('f5a2f211-a3a9-416a-90f7-a63df2516f7e', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_reading_relation('f5a2f211-a3a9-416a-90f7-a63df2516f7e', public_health_mandate_authority__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('f5a2f211-a3a9-416a-90f7-a63df2516f7e', foundational, legitimacy_is_a_function_of_proportional_fit_not_categorical_rule).
narrative_ontology:cs_axiom_status(legitimacy_is_a_function_of_proportional_fit_not_categorical_rule, holdable).
narrative_ontology:cs_axiom_grounding('f5a2f211-a3a9-416a-90f7-a63df2516f7e', legitimacy_is_a_function_of_proportional_fit_not_categorical_rule, conventional).
narrative_ontology:cs_axiom('f5a2f211-a3a9-416a-90f7-a63df2516f7e', secondary, victim_and_beneficiary_status_are_dynamic_not_fixed_by_vaccination_status).
narrative_ontology:cs_axiom_status(victim_and_beneficiary_status_are_dynamic_not_fixed_by_vaccination_status, holdable).
narrative_ontology:cs_axiom_grounding('f5a2f211-a3a9-416a-90f7-a63df2516f7e', victim_and_beneficiary_status_are_dynamic_not_fixed_by_vaccination_status, empirically_contingent).
narrative_ontology:cs_reference_frame('f5a2f211-a3a9-416a-90f7-a63df2516f7e', jacobson_rational_basis_deference).
narrative_ontology:cs_drift_state('f5a2f211-a3a9-416a-90f7-a63df2516f7e', post_pandemic_heightened_scrutiny_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f5a2f211-a3a9-416a-90f7-a63df2516f7e', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals_low_threat_context).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals_denied_accommodation).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, workers_facing_mandate_conditioned_employment).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, state_police_power_bounded_by_necessity).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, least_restrictive_means_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and periodically re-evaluates mandate scope, threshold triggers, and sunset conditions based on epidemiological severity data. Justifies the mandate's intensity as proportionate to the measured threat and revises it as case rates, variant severity, or hospital capacity change. Bears reputational and legal exposure if the proportionality assessment is later found unjustified.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, biographical, analytical, national).

% Cannot generate protective immunity themselves and depend on population-level transmission reduction for safety. Benefit from mandates during high-severity periods but have no independent way to compel continued protection once political will to maintain restrictions fades; their situation is the primary justificatory weight on the 'severity of threat' axis.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Face employment conditions, access restrictions, or other coercive pressure to comply with a mandate whose proportionality becomes contestable once threat severity declines (e.g., a mild seasonal variant). Their exit options are constrained by job dependency or access dependency, not eliminated outright, distinguishing this reading from a categorical-ban framing.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals_low_threat_context, payer,
    moderate, immediate, constrained, regional).

% In some proportionality assessments, mandates are relaxed or narrowed to reduce coercive burden on the compliant majority even though this population's exposure risk has not declined at the same rate. If the sliding scale trades their protection for reduced imposition on others, they become victims of the same balancing test that in other configurations protects them.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals_denied_accommodation, payer,
    powerless, immediate, trapped, local).

% Employment continuity is conditioned on mandate compliance. Whether this coercion is proportionate depends on the magnitude-of-coercion axis: termination is a severe cost regardless of how mild the underlying threat is, so this population's harm can be disproportionate even under a moderate-severity mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, workers_facing_mandate_conditioned_employment, payer,
    moderate, biographical, constrained, regional).

% Hospital and ICU capacity is protected when transmission mandates hold during surge periods. Not an agent itself but the structural interest the severity-of-threat axis is calibrated against; capacity data is the primary empirical input to the sliding scale.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity, beneficiary,
    institutional, immediate, analytical, regional).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity).

% Adjudicate whether a specific mandate's severity, alternatives, coercion magnitude, and duration were proportionate to the threat at the time it was imposed. Their rulings retroactively validate or invalidate the sliding-scale test itself as applied, and can compel narrowing, sunset, or complete withdrawal of a mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts_reviewing_mandate_proportionality, observer,
    institutional, generational, analytical, national).

% Argue the proportionality test is manipulable — that agencies can always characterize a threat as severe enough to justify current policy and can extend duration indefinitely by redefining benchmarks. They are heard in litigation and public comment but do not set the initial threshold or trigger the review cycle.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, revisable framework for calibrating collective transmission-control measures against actual epidemiological severity, avoiding both under-response to genuine threats and indefinite over-restriction once threat recedes.
% TRANSFER_FUNCTION: Moves burden of compliance (bodily autonomy, employment conditionality, movement restriction) from the population as a whole onto whichever subgroup the current proportionality assessment designates as bearing the coercive cost, in exchange for reduced transmission risk accruing to medically vulnerable populations and strained healthcare infrastructure.
% ABSENT_VOICES: Civil liberties advocates participate in litigation and comment but do not control the initial severity-assessment trigger or the review cadence; immunocompromised individuals denied accommodation under a relaxed mandate configuration have no independent voice once the balancing test shifts against them — their harm is absorbed into an aggregate proportionality judgment that does not track their individual risk.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished, the kernel dispute would not resolve — it would revert to whichever categorical reading (public_health_primary or bodily_autonomy_primary) currently holds more political power, likely producing either indefinite mandates or a permanent ban on mandate authority. Proponents say the sliding-scale mechanism itself, not just its current output, is what prevents both extremes; opponents say the framework's flexibility is precisely what allows extraction disguised as calibration.
% FOUNDING_PROBLEM: Public health emergencies vary enormously in severity and duration, and a single fixed rule (either 'mandates are always permissible' or 'mandates are never permissible') fails to track this variance — producing either unjustified coercion during mild threats or unjustified inaction during severe ones.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars outside both public-health and civil-liberties advocacy communities (e.g., administrative law academics writing on Jacobson v. Massachusetts and its progeny) attest the proportionality problem is live and structurally unresolved by either categorical reading. Civil liberties advocates counter that the test's open-endedness is itself the mechanism by which agencies avoid ever being forced to concede a mandate is disproportionate — corroboration exists but is contested on whether the framework solves the problem or merely defers it.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, contested).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored as a declining-then-rising curve (0.68 at T0 falling to 0.35 at T24, rising slightly to 0.42 at T36) because the proportionality reading's defining feature is that ε is NOT fixed — it tracks the severity/coercion ratio across the interval, per the expected structural delta. Early in the interval the threat is characterized as severe and coercion is high relative to alternatives (high ε); mid-interval, alternatives (treatments, natural/vaccine immunity) expand and coercion narrows (declining ε); late-interval a moderate uptick models either a new variant or a court finding the narrowed mandate still overshoots the reduced threat. Suppression tracks a similar but not identical curve, since suppression is the raw enforcement mechanism (mandate conditions on employment/access) which lags threat reassessment by design — agencies are slower to relax enforcement machinery than to revise official severity language, which is why suppression_requirement stays higher longer than extractiveness through the early-to-mid interval and stabilizes at 0.5 rather than tracking all the way down with ε.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting public health agencies experience this constraint as a functioning coordination mechanism they administer in good faith against evolving data (rope-like from that seat). The workers and unvaccinated individuals bearing employment-conditioned coercion during a period the sliding scale has not yet caught up to declining severity experience the identical structure as extraction backed by active enforcement (tangled-rope-to-snare-like from that seat). The engine's per-seat computation is expected to diverge sharply between the agenda_setter seat and the payer seats precisely because the proportionality test's central claim — that legitimacy tracks a genuine sliding scale — is contestable in real time from inside the interval, not just in retrospect.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality here is intentionally dynamic rather than fixed to a single population, which is the structural delta this reading is meant to capture. Immunocompromised populations are the beneficiary class when the mandate is proportionate to a genuine high-severity threat (low d, subsidized). The same class becomes a payer/victim when a proportionality assessment relaxes the mandate to reduce coercive burden on the compliant majority without a corresponding decline in their personal exposure risk — this is why immunocompromised_individuals_denied_accommodation appears as a separate stakeholder from immunocompromised_populations: they are structurally the same demographic experiencing opposite directionality depending on where the sliding-scale assessment currently sits. Unvaccinated individuals and mandate-conditioned workers sit on the target end whenever coercion magnitude is high relative to threat severity, and move toward the beneficiary end (avoided the mandate's cost) as thresholds relax.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality framework is explicitly the mechanism proposed to PREVENT mandatrophy — mandates that outlive their founding threat. Because it carries a built-in requirement to re-assess against current severity, alternatives, coercion magnitude, and duration (rather than a one-time authorization), in principle it should self-correct as threat recedes, which the declining extractiveness curve models. But the T36 uptick and the theater_ratio's late-interval plateau around 0.28-0.30 signal the risk the omega variables document: agencies retain review authority and can characterize a stabilized-but-nonzero threat as 'still severe enough' indefinitely, at which point the proportionality test stops functioning as a genuine constraint on mandate duration and starts functioning as a procedural justification for continuing a mandate whose founding severity has in fact passed — the dead-problem/live-authority mismatch the R5 fields are built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_indeterminacy,
    'Is the proportionality reading the correct lens for this kernel, or does its apparent moderation between the public_health_primary and bodily_autonomy_primary readings actually function to legitimate whichever categorical position currently holds institutional power by dressing it in the language of case-by-case balancing?',
    'Track whether proportionality assessments, across many jurisdictions and pathogens, actually produce narrowing/sunset outcomes proportional to measured severity decline, versus whether they systematically ratchet toward whichever direction institutional incentives favor (agencies toward continuation, courts toward restriction) regardless of the stated severity data.',
    'If assessments track severity data with reasonable fidelity, the proportionality reading is a genuine third framework distinct from both categorical siblings. If assessments systematically diverge from stated severity in a consistent direction, the proportionality reading is better understood as one of the categorical readings wearing a balancing-test costume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_indeterminacy, conceptual, 'Whether the sliding-scale test is a genuine independent framework or a legitimation device for a pre-committed categorical position.').

omega_variable(
    dynamic_victim_boundary_measurement,
    'At what point in the severity/coercion trajectory does the victim-designated population actually flip from unvaccinated/mandate-burdened individuals to immunocompromised/vulnerable individuals denied continued protection?',
    'Would require a jurisdiction-specific empirical threshold model correlating case fatality rate, hospital capacity utilization, and mandate coercion intensity with which population bears greater marginal harm at each point — likely reconstructable retrospectively from epidemiological and labor outcome data but not knowable prospectively at the time decisions are made.',
    'If the flip point can be identified with reasonable confidence in real time, the proportionality framework''s core promise (tracking genuine relative harm) is empirically achievable. If the flip point is only identifiable retrospectively, the framework authorizes real-time decisions under an appearance of precision it cannot actually deliver, which would support the civil-liberties critique that the test is manipulable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_victim_boundary_measurement, empirical, 'Whether the dynamic victim boundary the sliding scale implies is measurable prospectively or only retrospectively.').

omega_variable(
    kernel_framing_undetermination_cs,
    'Should this reading''s cs_structure be framed around the mandate-issuing agency (the obvious institutional kernel) or around the proportionality doctrine itself as a free-standing interpretive commitment that courts and agencies both claim to be bound by, independent of any specific mandate episode?',
    'Examine whether courts treat the proportionality test as a fixed doctrinal kernel they are interpreting (fixed_text-like, grounded in constitutional case law) versus treating each mandate as a fresh administrative determination reviewed under a loosely analogous standard (implicit, practice-grounded).',
    'Framing around the doctrine (fixed_text/lineage, courts as interpreters) yields a more stable, less agency-controlled kernel than framing around the issuing agency (formalized/extraction), which would treat the test as agency-administered and more exposed to the mandatrophy risk described above. This story adopts the doctrine framing; the agency framing is the alternative that would change interpretation_layer_present''s grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_undetermination_cs, conceptual, 'Whether the CS kernel is the proportionality doctrine (courts as lineage-interpreters) or the issuing agency''s mandate authority (agency as extraction-interpreter).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(publ_tr_t0, observed).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(publ_tr_t6, observed).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(publ_tr_t12, observed).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(publ_tr_t18, observed).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(publ_tr_t24, observed).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(publ_tr_t30, observed).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(publ_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(publ_be_t0, observed).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(publ_be_t6, observed).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(publ_be_t12, observed).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement_basis(publ_be_t18, observed).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement_basis(publ_be_t24, observed).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(publ_be_t30, observed).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.42).
narrative_ontology:measurement_basis(publ_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(publ_su_t0, observed).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement_basis(publ_su_t6, observed).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(publ_su_t12, observed).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(publ_su_t18, observed).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(publ_su_t24, observed).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement_basis(publ_su_t30, observed).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.5).
narrative_ontology:measurement_basis(publ_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'public health mandate legitimacy' claim, per the ε-invariance principle. public_health_primary treats collective protection as categorically dispositive (low/near-zero contestable ε from its own lights, since coercion is justified whenever collective benefit exists). bodily_autonomy_primary treats non-consensual intervention as categorically impermissible (high ε by construction, since any mandate is a rights violation regardless of severity). This proportionality_reading is structurally distinct from both: ε is NOT fixed but a function of the severity/alternatives/coercion/duration variables, authored here as a temporal trajectory across the interval rather than a single scalar judgment. The three stories share a kernel (public_health_mandate_authority) but are NOT the same constraint — each has its own stakeholders, its own victim/beneficiary boundary, and its own claimed_type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
