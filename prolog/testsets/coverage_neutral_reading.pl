% ============================================================================
% CONSTRAINT STORY: coverage_neutral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coverage_neutral_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: coverage_neutral_reading
 *   human_readable: OEWS Coverage Gap as Correctable Transitional Measurement Artifact
 *   domain: administrative_law/labor_economics/immigration_policy
 *
 * SUMMARY:
 *   The H-2A Adverse Effect Wage Rate is calculated using OEWS wage survey
 *   data that structurally excludes direct farm employers (as opposed to
 *   labor contractors and other intermediaries) from its sampling frame.
 *   Advocates for wage reform cite this exclusion as a mechanism that
 *   systematically depresses the computed prevailing wage, and some analyses
 *   attach a large aggregate transfer estimate (~$24bn) to the practice. This
 *   story takes the position that the exclusion is real but that its
 *   wage-direction effect is not established, and that the Department of
 *   Labor's IFR-committed phase-in represents an ordinary, correctable
 *   administrative transition rather than a designed extraction channel.
 *   Under this reading, coordination (a workable, litigation-avoiding wage
 *   benchmark) is real and largely undiluted by extraction — most of what
 *   critics attribute to the coverage gap should, on this reading, be
 *   reattributed to separable discretionary parameters.
 *
 * KEY AGENTS:
 *   - h2a_program_administrators: agenda_setter (institutional/analytical) — owns the OEWS methodology and the phase-in commitment
 *   - agricultural_employers_using_h2a: beneficiary (organized/constrained) — pays AEWR wages set from the current survey base
 *   - h2a_farmworkers: observer/payer (powerless/trapped) — receives the wage floor whose direction of bias is disputed
 *   - domestic_farmworkers: observer (powerless/constrained) — wage anchor effects, contested magnitude and direction
 *   - regulatory_economists: observer (analytical/analytical) — adjudicates between this reading and its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coverage_neutral_reading, 0.12).
domain_priors:suppression_score(coverage_neutral_reading, 0.08).
domain_priors:theater_ratio(coverage_neutral_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coverage_neutral_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(coverage_neutral_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(coverage_neutral_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coverage_neutral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(coverage_neutral_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coverage_neutral_reading, rope).
narrative_ontology:human_readable(coverage_neutral_reading, "OEWS Coverage Gap as Correctable Transitional Measurement Artifact").
narrative_ontology:topic_domain(coverage_neutral_reading, "administrative_law/labor_economics/immigration_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coverage_neutral_reading, '739c0b15-45e0-41dd-8885-88071253def5').
narrative_ontology:cs_kernel_codification('739c0b15-45e0-41dd-8885-88071253def5', formalized).
narrative_ontology:cs_authority_grounding('739c0b15-45e0-41dd-8885-88071253def5', expertise).
narrative_ontology:cs_interpretation_layer_present('739c0b15-45e0-41dd-8885-88071253def5').
narrative_ontology:cs_reading_relation('739c0b15-45e0-41dd-8885-88071253def5', adverse_effect_guarantee_kernel__instrument_dependent_reading, coexists_with).
narrative_ontology:cs_reading_relation('739c0b15-45e0-41dd-8885-88071253def5', adverse_effect_guarantee_kernel__textualist_severability_reading, influences).
narrative_ontology:cs_reading_relation('739c0b15-45e0-41dd-8885-88071253def5', adverse_effect_guarantee_kernel__capture_reading, coexists_with).
narrative_ontology:cs_reading_relation('739c0b15-45e0-41dd-8885-88071253def5', adverse_effect_guarantee_kernel__channel_conversion_reading, influences).
narrative_ontology:cs_axiom('739c0b15-45e0-41dd-8885-88071253def5', foundational, coverage_gap_direction_unestablished).
narrative_ontology:cs_axiom_status(coverage_gap_direction_unestablished, holdable).
narrative_ontology:cs_axiom_grounding('739c0b15-45e0-41dd-8885-88071253def5', coverage_gap_direction_unestablished, empirically_contingent).
narrative_ontology:cs_axiom('739c0b15-45e0-41dd-8885-88071253def5', foundational, agency_phase_in_commitment_presumptively_credible).
narrative_ontology:cs_axiom_status(agency_phase_in_commitment_presumptively_credible, holdable).
narrative_ontology:cs_axiom_grounding('739c0b15-45e0-41dd-8885-88071253def5', agency_phase_in_commitment_presumptively_credible, conventional).
narrative_ontology:cs_reference_frame('739c0b15-45e0-41dd-8885-88071253def5', administrable_prevailing_wage_benchmark_standard).
narrative_ontology:cs_drift_state('739c0b15-45e0-41dd-8885-88071253def5', post_ifr_phase_in_announcement, gap(stable, minor, true)).
narrative_ontology:cs_created_at('739c0b15-45e0-41dd-8885-88071253def5', '').
narrative_ontology:cs_kernel_id(coverage_neutral_reading, adverse_effect_guarantee_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coverage_neutral_reading, h2a_program_administrators).
narrative_ontology:constraint_beneficiary(coverage_neutral_reading, agricultural_employers_using_h2a).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coverage_neutral_reading, h2a_farmworkers).
narrative_ontology:constraint_vindicates(coverage_neutral_reading, adverse_effect_wage_rate_methodology_integrity).
narrative_ontology:constraint_vindicates(coverage_neutral_reading, phase_in_schedule_reliability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Adverse Effect Wage Rate methodology and the OEWS-derived prevailing wage survey. Committed under the current IFR to a phase-in schedule that will bring direct farm employers into OEWS sampling over a defined transition period, treating the current exclusion as a known, bounded, and already-scheduled-for-correction data gap rather than a permanent structural feature.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, h2a_program_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Currently pay AEWR wages calculated from an OEWS survey base that excludes direct farm employers, which under this reading is a temporary measurement artifact rather than a designed wage suppression channel. They anticipate the phase-in will normalize sampling and expect wage levels to adjust predictably once direct farm employer wage data enters the survey base.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, agricultural_employers_using_h2a, beneficiary,
    organized, biographical, constrained, national).

% Receive the AEWR as their wage floor. Under this reading, whatever wage effect exists from the coverage gap is not attributable to the exclusion itself in a directionally predictable way — the survey composition change could raise or lower the computed wage depending on how direct farm employer pay compares to the currently-sampled base, so no clear extraction from this population via the coverage mechanism is asserted here.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, h2a_farmworkers, observer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(coverage_neutral_reading, h2a_farmworkers, payer).

% Compete in the same regional farm labor markets and are affected by the AEWR as a wage anchor. Under this reading their situation is not asserted to be worsened by the coverage gap specifically, since the gap's wage-direction effect is treated as empirically ambiguous pending the phase-in.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, domestic_farmworkers, observer,
    powerless, biographical, constrained, national).

% Evaluate whether the OEWS exclusion biases the wage floor and whether the IFR phase-in schedule is a credible, funded, monitored commitment. Their assessment determines whether this reading (ordinary correctable gap) or a sibling reading (structural extraction) is the operative account.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, regulatory_economists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The AEWR/OEWS system solves a genuine coordination problem: setting a single, administratively tractable prevailing-wage benchmark across a fragmented, geographically dispersed agricultural labor market so that H-2A employers and enforcement agencies share one reference point instead of litigating wage adequacy case by case.
% TRANSFER_FUNCTION: Under this reading, no directional transfer is asserted to flow from the coverage gap itself — the gap is measurement incompleteness, not a mechanism moving value from one party to another. Any transfer in the system is attributed to the discretionary parameters (entry-level wage tier selection, housing cost deduction methodology) which this reading holds are separable from the coverage question.
% ABSENT_VOICES: Direct farm employers themselves are not sampled in OEWS and so cannot speak to whether their inclusion would raise or lower the computed wage; their absence is exactly the gap this reading treats as scheduled for closure rather than as evidence of a suppressed voice with a known directional interest.
% DISAPPEARANCE_RATIONALE: If the OEWS exclusion were corrected overnight (direct farm employers fully sampled), this reading predicts a modest, empirically uncertain adjustment to the AEWR rather than a large one-directional wage jump — because the excluded population's wage distribution relative to the currently-sampled base is not known to be systematically lower. Sibling readings dispute this and predict a large downward-biased-wage-floor correction, which is exactly the disagreement this reading exists to name.
% FOUNDING_PROBLEM: Agricultural guest-worker wage-setting needed an objective, defensible, and administrable prevailing-wage benchmark that would not require case-by-case wage litigation for every H-2A petition, while credibly protecting domestic farmworker wages from erosion by an influx of guest labor.
% FOUNDING_PROBLEM_CORROBORATION: The Department of Labor's own rulemaking record (the IFR itself) attests the coverage gap is recognized and scheduled for phase-in, which this reading treats as corroboration from the administering agency that the problem is being actively managed. However, this corroboration comes from the same agency administering the program rather than from an independent party, and farmworker advocacy organizations and some academic economists dispute that the phase-in timeline is either funded or enforceable — no corroboration from outside the administering and employer-beneficiary parties currently affirms the phase-in will close the gap on schedule.
narrative_ontology:disappearance_verdict(coverage_neutral_reading, contested).
narrative_ontology:founding_problem_status(coverage_neutral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coverage_neutral_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(coverage_neutral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coverage_neutral_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coverage_neutral_reading_tests).
:- end_tests(coverage_neutral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because this reading's central claim is that the coverage gap does not have an established directional bias — without a demonstrated direction, there is no clear extraction to measure at the coverage-mechanism level. Suppression is low (0.08) because nothing in this reading's account requires active coercion to hold the gap in place; it persists because collecting direct-farm-employer wage data is administratively harder, not because alternatives are being suppressed. Accessibility collapse is moderate (0.35): the phase-in path exists and is not blocked, but implementing it requires sustained agency follow-through that has not yet occurred, so alternatives to the current partial-coverage state are only partially open. Resistance is low (0.2): the main friction is analytical disagreement among economists and advocates, not organized resistance to a coercive structure, because on this reading there isn't one.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading's structural derivation, agricultural employers using H-2A are the nearest thing to a beneficiary (low d) because they currently pay a wage computed from the existing survey base and would face an uncertain adjustment if the base changed — but the reading declines to assert that this constitutes extraction from farmworkers, since the wage-direction effect of full coverage is not established. Farmworkers are named as payers of the wage floor as it currently exists (they receive whatever the floor is) but not as victims of a directional suppression mechanism, because this reading's core claim is exactly that no such mechanism is demonstrated. This is a materially different directionality picture than the sibling readings, which assign farmworkers a clear victim role.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is precisely the check against overclaiming mandatrophy: it insists that identifying an administrative gap (excluded sampling frame) is not sufficient to establish an extraction mechanism, and that the IFR's phase-in commitment should be evaluated as an ordinary regulatory correction unless and until evidence shows the commitment is hollow or the gap's directional bias is empirically confirmed. If the phase-in slips repeatedly or direct farm employer wages are shown to systematically exceed the current survey base, this reading's premises would be falsified and a sibling reading (capture_reading or channel_conversion_reading) would become the better-supported account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coverage_gap_directional_bias,
    'Does full OEWS inclusion of direct farm employers raise, lower, or leave roughly unchanged the computed AEWR, relative to the current survey base?',
    'A pilot survey extension sampling direct farm employer wages in a representative set of regions, compared against the current OEWS-derived AEWR for the same regions and job categories.',
    'If direct farm employer wages are systematically lower than the current sampled base, this reading is falsified and the structural extraction readings (capture_reading, instrument_dependent_reading) gain support, with the $24bn transfer estimate becoming attributable to the coverage gap itself. If wages are comparable or higher, this reading is corroborated and the transfer estimate must be reattributed to the discretionary parameters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coverage_gap_directional_bias, empirical, 'Whether the OEWS exclusion has an established downward wage-direction effect.').

omega_variable(
    phase_in_schedule_credibility,
    'Is the IFR''s phase-in schedule for including direct farm employers in OEWS sampling actually funded, monitored, and enforced, or is it an announced-but-unimplemented commitment?',
    'Track agency budget allocations, published sampling frame updates, and independent audits (e.g., GAO review) of whether the phase-in milestones are met on the announced timeline.',
    'If the phase-in repeatedly slips or lacks funding, this reading''s central premise (ordinary correctable transition) collapses and the constraint should be reclassified toward channel_conversion_reading or capture_reading, which treat the gap as a durable extraction feature rather than a transitional artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_in_schedule_credibility, empirical, 'Whether the phase-in commitment is a credible, tracked regulatory transition or a symbolic gesture.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the adverse_effect_guarantee_kernel better framed as the OEWS methodology itself (the obvious framing, which this reading and instrument_dependent_reading engage) or as the higher-order legitimacy claim that the entire AEWR system protects domestic wages adequately (the framing capture_reading and channel_conversion_reading implicitly contest)?',
    'Compare classification outcomes under both framings: does treating the kernel as ''the survey methodology'' versus ''the wage-protection guarantee as a whole'' change which parameters are held fixed and which are treated as the site of contest?',
    'Under the narrower framing (this reading''s choice), the coverage gap and the discretionary parameters are analytically separable, supporting this reading''s rope/mountain-hybrid classification. Under the broader framing, the entire AEWR system''s legitimacy is at stake, which supports the sibling readings'' more extractive classifications since any parameter choice becomes evidence for or against the overall guarantee.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is the survey methodology narrowly or the wage-protection guarantee broadly, and how that choice pre-shapes the classification this reading reaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coverage_neutral_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cove_tr_t0, coverage_neutral_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cove_tr_t4, coverage_neutral_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(cove_tr_t8, coverage_neutral_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(cove_tr_t12, coverage_neutral_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(cove_tr_t16, coverage_neutral_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(cove_tr_t20, coverage_neutral_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cove_tr_t24, coverage_neutral_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(cove_be_t0, coverage_neutral_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cove_be_t4, coverage_neutral_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(cove_be_t8, coverage_neutral_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement(cove_be_t12, coverage_neutral_reading, base_extractiveness, 12, 0.12).
narrative_ontology:measurement(cove_be_t16, coverage_neutral_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(cove_be_t20, coverage_neutral_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(cove_be_t24, coverage_neutral_reading, base_extractiveness, 24, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(coverage_neutral_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coverage_neutral_reading, resource_allocation).
narrative_ontology:affects_constraint(coverage_neutral_reading, instrument_dependent_reading).
narrative_ontology:affects_constraint(coverage_neutral_reading, textualist_severability_reading).
narrative_ontology:affects_constraint(coverage_neutral_reading, capture_reading).
narrative_ontology:affects_constraint(coverage_neutral_reading, channel_conversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of five linked readings of the adverse_effect_guarantee_kernel (the Department of Labor's AEWR/OEWS wage-protection commitment for H-2A agricultural labor). coverage_neutral_reading holds the coverage gap is directionally unestablished and the phase-in is a credible ordinary transition — the least extractive reading of the five, functioning as a rope/mountain hybrid with the discretionary parameters (entry-tier wage selection, housing deduction) doing analytical work this reading treats as severable. instrument_dependent_reading finds directional bias contingent on instrument choice; textualist_severability_reading treats coverage and parameters as legally severable with different remedies; capture_reading treats the exclusion as employer-lobbying-driven by design; channel_conversion_reading holds that closing the coverage gap alone will not eliminate wage suppression because it will migrate to the discretionary parameters. Each reading carries its own epsilon and stakeholder structure; they are linked here rather than merged because the underlying kernel — what the AEWR/OEWS commitment actually guarantees — is genuinely contested across these five structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
