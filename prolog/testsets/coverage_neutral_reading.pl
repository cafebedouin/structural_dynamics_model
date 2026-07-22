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
 *   human_readable: OEWS Coverage-Neutral Reading of Adverse Effect Wage Measurement
 *   domain: political_economy_of_labor_migration/administrative_law/agricultural_policy
 *
 * SUMMARY:
 *   The Adverse Effect Wage Rate for H-2A agricultural guest workers is
 *   increasingly computed from Occupational Employment and Wage Statistics
 *   (OEWS) data, which does not sample farm establishments directly. Advocacy
 *   groups and some researchers treat this coverage gap as evidence that the
 *   wage floor is systematically depressed relative to what a farm-inclusive
 *   survey would produce. This story takes the coverage-neutral reading: the
 *   gap is real and undisputed, but the inferential leap from 'gap exists' to
 *   'floor is biased downward' is not established by the gap alone. OEWS
 *   offers genuine advantages — higher sampling frequency, finer geographic
 *   and occupational resolution — that the instrument it replaced lacked, and
 *   the empirical direction of any coverage-driven bias remains open. This
 *   reading is one of four readings of the adverse_effect_measurability
 *   kernel; the instrument_capture_reading asserts the downward-bias
 *   inference as established, the bureaucratic_drift_reading treats the
 *   instrument choice as inertial administrative habit rather than a live
 *   methodological tradeoff, and the hold_up_efficiency_reading treats the
 *   gap as a deliberate lever employers exploit in rulemaking capture. This
 *   story does not adopt any of those framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coverage_neutral_reading, 0.18).
domain_priors:suppression_score(coverage_neutral_reading, 0.12).
domain_priors:theater_ratio(coverage_neutral_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coverage_neutral_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(coverage_neutral_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(coverage_neutral_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coverage_neutral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(coverage_neutral_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coverage_neutral_reading, rope).
narrative_ontology:human_readable(coverage_neutral_reading, "OEWS Coverage-Neutral Reading of Adverse Effect Wage Measurement").
narrative_ontology:topic_domain(coverage_neutral_reading, "political_economy_of_labor_migration/administrative_law/agricultural_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coverage_neutral_reading, '4bb318ab-871e-4ac9-88c2-eefb3df698f9').
narrative_ontology:cs_kernel_codification('4bb318ab-871e-4ac9-88c2-eefb3df698f9', distributed).
narrative_ontology:cs_authority_grounding('4bb318ab-871e-4ac9-88c2-eefb3df698f9', expertise).
narrative_ontology:cs_interpretation_layer_present('4bb318ab-871e-4ac9-88c2-eefb3df698f9').
narrative_ontology:cs_reading_relation('4bb318ab-871e-4ac9-88c2-eefb3df698f9', adverse_effect_measurability__instrument_capture_reading, influences).
narrative_ontology:cs_reading_relation('4bb318ab-871e-4ac9-88c2-eefb3df698f9', adverse_effect_measurability__bureaucratic_drift_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bb318ab-871e-4ac9-88c2-eefb3df698f9', adverse_effect_measurability__hold_up_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('4bb318ab-871e-4ac9-88c2-eefb3df698f9', foundational, coverage_gap_bias_direction_unresolved).
narrative_ontology:cs_axiom_status(coverage_gap_bias_direction_unresolved, holdable).
narrative_ontology:cs_axiom_grounding('4bb318ab-871e-4ac9-88c2-eefb3df698f9', coverage_gap_bias_direction_unresolved, empirically_contingent).
narrative_ontology:cs_axiom('4bb318ab-871e-4ac9-88c2-eefb3df698f9', secondary, measurement_quality_gains_are_genuine_coordination).
narrative_ontology:cs_axiom_status(measurement_quality_gains_are_genuine_coordination, holdable).
narrative_ontology:cs_axiom_grounding('4bb318ab-871e-4ac9-88c2-eefb3df698f9', measurement_quality_gains_are_genuine_coordination, instrumental).
narrative_ontology:cs_reference_frame('4bb318ab-871e-4ac9-88c2-eefb3df698f9', methodological_tradeoff_neutrality).
narrative_ontology:cs_drift_state('4bb318ab-871e-4ac9-88c2-eefb3df698f9', post_2015_oews_transition_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4bb318ab-871e-4ac9-88c2-eefb3df698f9', '').
narrative_ontology:cs_kernel_id(coverage_neutral_reading, adverse_effect_measurability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coverage_neutral_reading, wage_survey_statisticians).
narrative_ontology:constraint_beneficiary(coverage_neutral_reading, h2a_employers).
narrative_ontology:constraint_beneficiary(coverage_neutral_reading, labor_department_rulemakers).
narrative_ontology:constraint_vindicates(coverage_neutral_reading, measurement_neutrality_of_establishment_surveys).
narrative_ontology:constraint_vindicates(coverage_neutral_reading, coverage_gap_does_not_entail_directional_bias).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the Occupational Employment and Wage Statistics survey, sampling non-farm establishments by industry and geography at a resolution the agricultural survey it replaced could not match. They know the farm exclusion is real and documented in the methodology notes, and they maintain that its directional effect on the wage floor is unresolved rather than settled downward bias.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, wage_survey_statisticians, agenda_setter,
    institutional, generational, analytical, national).

% Pay the Adverse Effect Wage Rate computed substantially from OEWS data for many occupations. They benefit if OEWS's higher-frequency, higher-granularity data produces a wage floor that tracks local labor markets more accurately than the coarser instrument it replaced, regardless of whether that floor is higher or lower than a farm-inclusive alternative would produce.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, h2a_employers, beneficiary,
    organized, biographical, constrained, national).

% Are paid the wage rate the survey produces but are not employed in the establishments the survey samples, since OEWS excludes farm establishments by design. They are not part of the data-generating process for the wage floor that governs their own pay, though this reading holds that exclusion from the sample does not by itself establish that the resulting number is biased against them.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, h2a_farmworkers, excluded,
    powerless, immediate, trapped, national).

% Select OEWS as the instrument for computing wage floors under a rulemaking process, weighing its documented gaps against its granularity and frequency advantages over the alternative (farm-specific or reduced-frequency instruments). They can defend the choice on methodological grounds independent of any claim about who the choice benefits.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, labor_department_rulemakers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(coverage_neutral_reading, labor_department_rulemakers, agenda_setter).

% Argue in comment letters and litigation that the farm exclusion depresses the measured wage floor because non-farm occupational wages used as proxies are unrepresentative of farm labor markets. This reading does not adopt their inferential leap from 'gap exists' to 'floor is biased downward' as established, though it acknowledges the gap itself is real and the advocacy position is a legitimate contesting claim.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, farmworker_advocacy_organizations, excluded,
    organized, biographical, constrained, national).

% Study whether OEWS-derived wage floors converge with, exceed, or fall below floors that would be computed from farm-inclusive data where such data exists (e.g., in occupations with partial overlap). Their empirical findings to date are mixed and do not establish a uniform directional bias, which is the evidentiary basis this reading rests on.
narrative_ontology:constraint_stakeholder(coverage_neutral_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, high-frequency, geographically and occupationally granular wage survey that multiple regulatory programs (including H-2A wage-setting) can reference instead of maintaining separate, lower-resolution, agriculture-specific instruments.
% TRANSFER_FUNCTION: The survey itself transfers nothing directly; it sets a reference number that then determines wage transfers from employers to workers under the AEWR rule. Whether that reference number transfers value away from farmworkers relative to some counterfactual instrument is exactly the open question this reading refuses to presume answered.
% ABSENT_VOICES: Farmworkers whose wages are set by reference to a survey that does not sample their workplaces are not participants in the survey's design; farmworker advocacy organizations raise this on their behalf but their inferential claim (gap implies downward bias) is contested rather than corroborated by independent economic analysis.
% DISAPPEARANCE_RATIONALE: If OEWS were dropped as the wage-floor instrument, rulemakers would need to revert to a lower-frequency, lower-granularity alternative or construct a new farm-inclusive survey; employers would face a different (not necessarily lower) wage floor and advocacy groups would treat this as either vindication or continued injustice depending on which direction the new number moved. The world clearly rearranges administratively; whether it rearranges in farmworkers' favor is exactly what remains contested and is not resolved by removing the instrument.
% FOUNDING_PROBLEM: Agricultural labor markets needed a wage-setting mechanism to prevent H-2A guest-worker admission from depressing wages for domestic workers in similar occupations ('adverse effect'), and the original agriculture-specific survey used to compute this rate was low-frequency and coarse-grained.
% FOUNDING_PROBLEM_CORROBORATION: Labor department rulemakers and wage survey statisticians attest the switch to OEWS solved a real measurement-quality problem (frequency and granularity). Farmworker advocacy organizations and some labor economists attest the founding problem — protecting farmworker wages from erosion — is not being solved and may be undermined by the farm exclusion; independent academic studies on the direction of bias are inconclusive, which is the corroboration this reading relies on for its central claim of genuine uncertainty.
narrative_ontology:disappearance_verdict(coverage_neutral_reading, contested).
narrative_ontology:founding_problem_status(coverage_neutral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coverage_neutral_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(coverage_neutral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coverage_neutral_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18) because this reading holds that no directional extraction has been established — the coordination function (a higher-resolution wage survey usable across regulatory programs) is real and the alleged extraction (downward-biased wage floor harming farmworkers) is an unresolved empirical claim, not a structural fact of the instrument's operation. Suppression is low (0.12): no party is coerced into accepting OEWS as the instrument in a way that forecloses the advocacy groups' ability to contest it in rulemaking comments or litigation — the debate is open and ongoing, which is itself evidence against high suppression. Theater ratio is low and rises only slightly over the interval, reflecting that the survey performs substantive statistical function rather than performative compliance. Accessibility collapse is moderate (0.35): farmworkers cannot easily generate an alternative, farm-inclusive wage statistic themselves, but the political and administrative channel (rulemaking comment, litigation) for contesting the instrument choice remains genuinely open, which caps collapse well below mountain-level.
 *
 * PERSPECTIVAL GAP:
 *   Statisticians and rulemakers experience the instrument as a genuine methodological upgrade; farmworker advocates experience the same instrument as an unaccountable black box that determines their members' pay without sampling their members' workplaces. Both experiences are structurally real and do not require choosing a winner — the gap in experience is exactly why this kernel needs multiple readings rather than one story trying to average across them.
 *
 * DIRECTIONALITY LOGIC:
 *   Wage survey statisticians and labor department rulemakers are coded beneficiaries because they get a survey instrument with real methodological advantages (frequency, granularity) that lowers their own operational costs, independent of wage-floor direction. H-2A employers benefit from a more responsive, locally-accurate wage floor whether or not it happens to run higher or lower than a hypothetical farm-inclusive alternative — the benefit here is measurement quality, not necessarily a lower number. Farmworkers and their advocacy organizations are coded excluded, not victim: this reading declines to assert they bear a proven cost, because the direction of the coverage-driven effect on the AEWR is exactly the open question. Coding them as victims would smuggle in the instrument_capture_reading's contested inferential step as if it were settled structural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting domestic farmworker wages from erosion by guest-worker admission) is still live in the sense that H-2A admissions continue and the AEWR mechanism still operates against that stated purpose. Whether the current instrument still serves that purpose, versus having drifted into serving employer and statistical-agency convenience, is contested precisely because the coverage gap's directional effect is unresolved. This reading resists premature mandatrophy declaration in either direction: declaring the AEWR mechanism dead-but-persisting (pure extraction) would require establishing the downward bias claim, which this reading holds open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coverage_gap_bias_direction,
    'Does the exclusion of farm establishments from OEWS sampling produce a wage floor that is systematically lower, systematically higher, or not systematically different from what a farm-inclusive survey would produce for the same occupations and areas?',
    'Comparative studies using occupations with partial farm/non-farm overlap, or construction of a pilot farm-inclusive supplemental survey compared against OEWS estimates for the same geography and occupation codes over multiple cycles.',
    'If a systematic downward bias is established, this reading''s central premise collapses and the instrument_capture_reading''s inferential step is vindicated; if bias is neutral or upward, the instrument_capture_reading''s core claim fails even though the coverage gap remains a real technical limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_gap_bias_direction, empirical, 'Whether the farm-exclusion coverage gap produces a directionally biased wage floor.').

omega_variable(
    instrument_choice_as_capture_signal,
    'Is the Department of Labor''s continued reliance on OEWS despite the known coverage gap evidence of regulatory capture by employer interests, or a defensible tradeoff favoring a demonstrably superior general-purpose instrument over a discontinued, lower-quality agriculture-specific one?',
    'Review of rulemaking record for explicit consideration of alternative instruments, cost-benefit analysis of reinstating a farm-specific survey, and comparison of rulemaking outcomes across administrations with different asserted policy priorities.',
    'If the rulemaking record shows the coverage gap was raised and dismissed without substantive analysis, that supports a capture or drift reading; if it shows genuine cost-benefit deliberation reaching this outcome under multiple administrations, that supports the coverage-neutral reading''s framing of a defensible methodological tradeoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrument_choice_as_capture_signal, conceptual, 'Whether the persistence of the coverage gap despite advocacy objections reflects capture, drift, or genuine tradeoff.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the adverse_effect_measurability kernel better understood as a single contested technical question (what does the survey measure) or as four structurally distinct claims that only share a label because they all discuss the same statistical instrument?',
    'Track whether empirical resolution of the bias-direction question (this reading''s central omega) actually resolves the bureaucratic_drift_reading and hold_up_efficiency_reading''s claims, or whether those readings'' claims are orthogonal to bias direction and would survive resolution of it.',
    'If the sibling readings'' claims are genuinely orthogonal to bias direction, the four readings are less like contested interpretations of one kernel and more like four separate constraints wearing one label — which would argue for further decomposition rather than a four-reading kernel structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the four sibling readings are genuinely contesting the same kernel or are separable constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coverage_neutral_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cove_tr_t0, coverage_neutral_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cove_tr_t4, coverage_neutral_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(cove_tr_t8, coverage_neutral_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(cove_tr_t12, coverage_neutral_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(cove_tr_t16, coverage_neutral_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(cove_tr_t20, coverage_neutral_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(cove_be_t0, coverage_neutral_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cove_be_t4, coverage_neutral_reading, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(cove_be_t8, coverage_neutral_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(cove_be_t12, coverage_neutral_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(cove_be_t16, coverage_neutral_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(cove_be_t20, coverage_neutral_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(coverage_neutral_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(coverage_neutral_reading, instrument_capture_reading).
narrative_ontology:affects_constraint(coverage_neutral_reading, bureaucratic_drift_reading).
narrative_ontology:affects_constraint(coverage_neutral_reading, hold_up_efficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the adverse_effect_measurability kernel (OEWS coverage of farm establishments in AEWR wage-setting). coverage_neutral_reading asserts no established directional bias and treats OEWS's granularity/frequency advantages as genuine coordination gains. instrument_capture_reading asserts the coverage gap produces an established downward bias serving employer interests. bureaucratic_drift_reading treats the instrument choice as administrative inertia rather than active interest-capture. hold_up_efficiency_reading treats the gap as a deliberately exploited lever in an employer/rulemaker hold-up dynamic. Each reading has its own epsilon and stakeholder structure per the epsilon-invariance principle; they are linked here rather than merged into one story with a bias-direction parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
