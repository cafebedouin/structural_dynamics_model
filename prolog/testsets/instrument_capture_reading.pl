% ============================================================================
% CONSTRAINT STORY: instrument_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_instrument_capture_reading, []).

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
 *   constraint_id: instrument_capture_reading
 *   human_readable: Adverse Effect Wage Rate Instrument Capture via Survey Substitution
 *   domain: political_economy_of_labor_migration/administrative_law/agricultural_policy
 *
 * SUMMARY:
 *   The H-2A program's Adverse Effect Wage Rate is a statutory wage floor
 *   whose entire operation depends on the fidelity of the instrument used to
 *   measure prevailing farm wages. This story instantiates the reading in
 *   which the discontinuation of farm-specific occupational wage survey data,
 *   and its replacement with a survey instrument that structurally cannot
 *   capture the same occupational granularity, functions as a substantive de
 *   facto repeal of the statutory protection dressed as a technical
 *   methodology update. Under this reading, DOL's continued certification of
 *   H-2A labor conditions as satisfying the adverse-effect standard is a
 *   performance of compliance layered over an instrument that cannot actually
 *   perform the measurement the statute requires. This is one reading among
 *   several of the broader adverse_effect_measurability kernel: sibling
 *   readings (bureaucratic_drift_reading, hold_up_efficiency_reading,
 *   coverage_neutral_reading) characterize the same instrument change as
 *   unintentional administrative drift, an efficient hold-up resolution
 *   mechanism, or a coverage-neutral technical improvement, respectively.
 *   Those are different constraints with different ε profiles, not different
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - department_of_labor: agenda_setter (institutional/analytical) — selects and administers the wage survey instrument and issues certifications
 *   - large_agricultural_employers: primary beneficiary (organized/arbitrage) — pays the AEWR wage floor and benefits from its suppression
 *   - h2a_visa_workers: primary target (powerless/trapped) — bears the wage suppression with no exit from employer-tied visa status
 *   - domestic_farmworkers: secondary target (powerless/constrained) — bears eroded local wage bargaining position
 *   - congressional_research_service and government_accountability_office: analytical observers — document the methodological discontinuity without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(instrument_capture_reading, 0.78).
domain_priors:suppression_score(instrument_capture_reading, 0.71).
domain_priors:theater_ratio(instrument_capture_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(instrument_capture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(instrument_capture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(instrument_capture_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(instrument_capture_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(instrument_capture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(instrument_capture_reading, tangled_rope).
narrative_ontology:human_readable(instrument_capture_reading, "Adverse Effect Wage Rate Instrument Capture via Survey Substitution").
narrative_ontology:topic_domain(instrument_capture_reading, "political_economy_of_labor_migration/administrative_law/agricultural_policy").

domain_priors:requires_active_enforcement(instrument_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(instrument_capture_reading, '568e37ce-5f1f-4f82-a625-10e1914464a3').
narrative_ontology:cs_kernel_codification('568e37ce-5f1f-4f82-a625-10e1914464a3', distributed).
narrative_ontology:cs_authority_grounding('568e37ce-5f1f-4f82-a625-10e1914464a3', extraction).
narrative_ontology:cs_interpretation_layer_present('568e37ce-5f1f-4f82-a625-10e1914464a3').
narrative_ontology:cs_reading_relation('568e37ce-5f1f-4f82-a625-10e1914464a3', adverse_effect_measurability__bureaucratic_drift_reading, coexists_with).
narrative_ontology:cs_reading_relation('568e37ce-5f1f-4f82-a625-10e1914464a3', adverse_effect_measurability__hold_up_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('568e37ce-5f1f-4f82-a625-10e1914464a3', adverse_effect_measurability__coverage_neutral_reading, forecloses).
narrative_ontology:cs_axiom('568e37ce-5f1f-4f82-a625-10e1914464a3', foundational, measurement_substitution_is_substantive_policy).
narrative_ontology:cs_axiom_status(measurement_substitution_is_substantive_policy, holdable).
narrative_ontology:cs_axiom_grounding('568e37ce-5f1f-4f82-a625-10e1914464a3', measurement_substitution_is_substantive_policy, empirically_contingent).
narrative_ontology:cs_axiom('568e37ce-5f1f-4f82-a625-10e1914464a3', secondary, certification_without_valid_instrument_is_procedural_default).
narrative_ontology:cs_axiom_status(certification_without_valid_instrument_is_procedural_default, holdable).
narrative_ontology:cs_axiom_grounding('568e37ce-5f1f-4f82-a625-10e1914464a3', certification_without_valid_instrument_is_procedural_default, conventional).
narrative_ontology:cs_reference_frame('568e37ce-5f1f-4f82-a625-10e1914464a3', farm_specific_wage_survey_baseline).
narrative_ontology:cs_drift_state('568e37ce-5f1f-4f82-a625-10e1914464a3', post_oews_substitution_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('568e37ce-5f1f-4f82-a625-10e1914464a3', '').
narrative_ontology:cs_kernel_id(instrument_capture_reading, adverse_effect_measurability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(instrument_capture_reading, large_agricultural_employers).
narrative_ontology:constraint_beneficiary(instrument_capture_reading, farm_labor_contractors).
narrative_ontology:constraint_victim(instrument_capture_reading, h2a_visa_workers).
narrative_ontology:constraint_victim(instrument_capture_reading, domestic_farmworkers).
narrative_ontology:constraint_vindicates(instrument_capture_reading, data_methodology_modernization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Adverse Effect Wage Rate (AEWR) rule under the H-2A program, which is statutorily required to prevent employers from using foreign labor to depress wages for domestic and migrant farmworkers. DOL selects and can change the survey instrument used to calculate the AEWR by region and occupation. It discontinued the Farm Labor Survey occupational breakdowns and moved toward Occupational Employment and Wage Statistics data that does not capture the same farm-specific occupational categories, then certified labor conditions as if the statutory adverse-effect test remained satisfied. It faces no vote requirement to make this change, only notice-and-comment rulemaking, which it can frame as a technical methodology update rather than a substantive policy shift.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, department_of_labor, agenda_setter,
    institutional, generational, analytical, national).

% Petition for and lobby in favor of survey changes that lower or freeze the calculated wage floor, since the AEWR sets their minimum required wage offer to both H-2A workers and comparably employed domestic workers. They benefit directly from any methodology that fails to capture farm-specific wage pressure, and can shift H-2A recruitment across regions to exploit measurement gaps.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, large_agricultural_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Recruit and supply H-2A labor to growers, and benefit from a lower or stagnant wage floor because their margin is often the spread between what employers pay and what they retain for housing, transport, and fees. They participate in comment periods urging DOL toward survey instruments most favorable to depressed wage calculations.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, farm_labor_contractors, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(instrument_capture_reading, farm_labor_contractors, agenda_setter).

% Are tied to a single certified employer under the H-2A visa; cannot change jobs without losing legal status, and cannot organize collectively without deportation risk. They receive the AEWR as their wage floor, which the survey substitution silently depresses. They have no seat in the rulemaking process, no domestic political voice, and no practical exit from the wage rate the instrument sets for them.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, h2a_visa_workers, payer,
    powerless, immediate, trapped, national).

% The AEWR is meant to prevent H-2A hiring from undercutting wages available to domestic workers in the same occupations and region. When the wage floor is measured downward by instrument substitution, domestic workers' bargaining position erodes even though they are U.S. citizens or lawful residents with more exit options than H-2A workers, because local labor markets are anchored to the certified rate.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, domestic_farmworkers, payer,
    powerless, biographical, constrained, national).

% Produces reports analyzing whether DOL's wage-setting methodology satisfies the statutory adverse-effect standard. CRS has flagged methodological discontinuities between the Farm Labor Survey and its OEWS-based successor, documenting the resulting divergence without possessing enforcement authority to compel a change.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, congressional_research_service, observer,
    institutional, generational, analytical, national).

% Audits DOL's administration of the H-2A program and has previously examined whether wage-setting data is fit for the statutory purpose. GAO can recommend corrective action to Congress but cannot itself force DOL to revert or fix the instrument.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, government_accountability_office, observer,
    institutional, generational, analytical, national).

% File comments and litigation challenging the survey substitution as an unlawful substantive rule change adopted without the procedural rigor a wage-floor repeal would require, but have limited standing leverage against DOL's characterization of the change as a methodology update, and no direct seat in the survey design process itself.
narrative_ontology:constraint_stakeholder(instrument_capture_reading, farmworker_advocacy_organizations, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(instrument_capture_reading, large_agricultural_employers).
narrative_ontology:fixing_cost_class(instrument_capture_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally, the AEWR and its supporting wage survey coordinate a shared, objective measurement of prevailing agricultural wages so that H-2A employers cannot underpay foreign labor relative to domestic labor market conditions, solving a genuine collective problem of wage suppression through cross-border labor substitution.
% TRANSFER_FUNCTION: The survey substitution moves wage floor value from H-2A workers and domestic farmworkers (who receive a lower or stagnant statutory minimum wage than the discontinued instrument would have produced) to large agricultural employers and labor contractors (who pay less in aggregate wage costs while certification proceeds as if the statutory test is met).
% ABSENT_VOICES: H-2A workers themselves have essentially no voice in the rulemaking process that sets their wage floor; they are non-citizens tied to a single employer, cannot lobby, comment meaningfully without employer retaliation risk, or vote. Farmworker advocacy organizations attempt to represent this absence but are structurally outside the technical data-methodology conversation DOL frames the change as being about.
% DISAPPEARANCE_RATIONALE: If the instrument substitution were reversed and farm-specific wage measurement restored, calculated AEWRs would rise in numerous regions and occupations, employer labor costs would increase, some employers would shift toward mechanization or reduced H-2A reliance, and the statutory adverse-effect protection would function again as a live wage floor rather than a certification formality.
% FOUNDING_PROBLEM: Congress created the AEWR requirement to prevent the H-2A guest-worker program from becoming a mechanism for employers to import labor at wages below what would prevail absent foreign labor substitution, thereby protecting domestic farmworker wages and working conditions.
% FOUNDING_PROBLEM_CORROBORATION: CRS and GAO reports, produced independently of both DOL and the employer associations that benefit from the instrument change, document that the substituted survey does not capture farm-specific occupational wage data at the granularity the statute presumes; this corroboration comes from institutions outside the beneficiary set. DOL itself attests the founding problem remains addressed by its current methodology, but that attestation comes from the agency administering the very instrument in question.
narrative_ontology:disappearance_verdict(instrument_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(instrument_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(instrument_capture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(instrument_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(instrument_capture_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(instrument_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(instrument_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(instrument_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) and rising because the survey substitution's downward bias on the wage floor compounds each certification cycle: each year the discontinued instrument is not restored, the accumulated wage suppression widens relative to what farm-specific measurement would have produced. Theater ratio is authored substantially high and rising (0.30 to 0.62) because DOL's continued issuance of certifications performs statutory compliance ('the adverse-effect test is satisfied') while the underlying instrument cannot actually evaluate the test the statute specifies — the theater is the certification process proceeding as though nothing structural changed. Suppression is authored as a raw structural property, not scaled by scope: H-2A workers' employer-tied visa status is the suppression mechanism, independent of how large the geographic scope of any given certification is. Accessibility collapse is moderate (0.58) because alternative wage benchmarks (state minimum wage, collective bargaining) exist in principle but are foreclosed in practice by the trapped exit status of H-2A workers and the anchoring effect of the certified rate on the broader regional labor market.
 *
 * PERSPECTIVAL GAP:
 *   From DOL's seat, this looks like routine data-source modernization within its statistical discretion — an internal administrative choice about survey methodology, not a policy reversal. From the H-2A worker's seat, and from CRS/GAO's analytical seat, the same act operates as a substantive removal of a wage protection that was never subjected to the political process (notice-and-comment rulemaking, absent legislative override) that a formal repeal would require. The engine's computed divergence between the agenda_setter seat and the payer seat is exactly the structure this reading is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Large agricultural employers and farm labor contractors are declared beneficiaries because they pay the wage floor and gain directly (lower labor cost) when the instrument understates prevailing wages; their arbitrage/mobile exit options place them near the low-d end. H-2A workers are declared victims with trapped exit (employer-tied visa, deportation risk on job change) placing them at the high-d end — the engine should treat them as full targets. Domestic farmworkers are victims with only constrained exit; their citizenship gives more formal mobility than H-2A workers but their local labor market is anchored to the same certified rate, so they still absorb suppressed wage effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing wage suppression via foreign labor substitution) remains statutorily live — Congress has not repealed the AEWR requirement — but under this reading DOL's practical administration of the instrument has diverged from the statute's function while continuing to issue certifications as though the mandate is fulfilled. This is the tangled_rope signature: genuine coordination function (a shared wage benchmark preventing race-to-the-bottom hiring) persists in name, but the actual operation now channels asymmetric extraction to employers at H-2A and domestic worker expense, requiring DOL's continued active certification (enforcement) to hold. Treating this as pure bureaucratic drift (a sibling reading) would understate the beneficiary-directed character of the change; treating it as pure snare would understate that a real coordination problem (cross-border wage arbitrage) is still nominally being addressed, however poorly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structural_effect_of_survey_change,
    'Did DOL''s survey substitution reflect a deliberate policy choice to depress the wage floor (instrument capture), or an unintentional consequence of unrelated statistical modernization pressures affecting the broader federal survey infrastructure (bureaucratic drift)?',
    'Internal agency deliberation records, rulemaking docket comments showing whether employer associations specifically lobbied for the OEWS substitution and whether DOL considered farm-specific alternatives and rejected them for cost or capacity reasons versus genuinely lacking alternatives.',
    'If deliberate capture is established, this reading''s tangled_rope/near-snare classification is reinforced; if genuine drift with no identifiable beneficiary intent, the bureaucratic_drift_reading sibling becomes the more accurate characterization and this story''s ε would not transfer to that sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_structural_effect_of_survey_change, empirical, 'Whether the instrument change was purposive capture or unintentional administrative drift.').

omega_variable(
    statutory_test_evaluability,
    'Can the statutory adverse-effect standard be meaningfully evaluated at all using an occupational survey instrument that does not capture farm-specific wage categories, or does the substitution render the test structurally unanswerable rather than merely biased?',
    'Technical comparison of OEWS occupational granularity against Farm Labor Survey categories for the specific H-2A occupational classifications at issue, conducted by an independent statistical body (e.g., a National Academies panel).',
    'If the test is rendered genuinely unanswerable (not merely biased downward), the constraint''s character shifts from extractive-but-measurable toward a deeper certification-theater problem where no valid finding is possible under the new instrument, strengthening the theater_ratio interpretation over a pure extraction interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_test_evaluability, empirical, 'Whether the substitute instrument can evaluate the statutory standard at all, versus merely biasing it.').

omega_variable(
    kernel_framing_selection_basis,
    'What guided the selection of the instrument_capture_reading over the coverage_neutral_reading as the primary framing for this story, given that both are defensible readings of the same underlying survey substitution event?',
    'The CRS/GAO documentation of methodological discontinuity (cited in founding_problem_corroboration) and the rising theater_ratio/base_extractiveness trend across the interval were treated as evidence favoring the capture reading over a neutral-coverage reading; a coverage_neutral_reading would require evidence that OEWS substitution preserves equivalent wage-floor accuracy, which the CRS/GAO record does not support.',
    'Under the coverage_neutral_reading, the same survey substitution would carry near-zero extraction and no victim set (mountain-adjacent classification); under this reading, the same event carries substantial extraction with a clear victim set (tangled_rope classification). The two readings produce materially different ε values for the same underlying administrative act, which is why they are authored as separate constraint stories rather than one story with a variable observable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection_basis, conceptual, 'Documents the framing choice between this reading and the coverage_neutral_reading sibling, per the ε-invariance decomposition rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(instrument_capture_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, instrument_capture_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(inst_tr_t4, instrument_capture_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(inst_tr_t8, instrument_capture_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(inst_tr_t12, instrument_capture_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(inst_tr_t16, instrument_capture_reading, theater_ratio, 16, 0.55).
narrative_ontology:measurement(inst_tr_t20, instrument_capture_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(inst_tr_t24, instrument_capture_reading, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, instrument_capture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(inst_be_t4, instrument_capture_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(inst_be_t8, instrument_capture_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(inst_be_t12, instrument_capture_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(inst_be_t16, instrument_capture_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(inst_be_t20, instrument_capture_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(inst_be_t24, instrument_capture_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, instrument_capture_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(inst_su_t4, instrument_capture_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(inst_su_t8, instrument_capture_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(inst_su_t12, instrument_capture_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(inst_su_t16, instrument_capture_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(inst_su_t20, instrument_capture_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(inst_su_t24, instrument_capture_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(instrument_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(instrument_capture_reading, 0.1).
narrative_ontology:affects_constraint(instrument_capture_reading, bureaucratic_drift_reading).
narrative_ontology:affects_constraint(instrument_capture_reading, hold_up_efficiency_reading).
narrative_ontology:affects_constraint(instrument_capture_reading, coverage_neutral_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the adverse_effect_measurability kernel, all describing the same underlying DOL survey-instrument substitution but instantiating structurally distinct constraints with different ε values, beneficiary/victim sets, and classifications. instrument_capture_reading (this story) treats the change as substantive extraction dressed as methodology; bureaucratic_drift_reading treats it as unintentional administrative attrition with no identifiable capturing beneficiary; hold_up_efficiency_reading treats it as an efficient resolution of an employer-worker hold-up problem; coverage_neutral_reading treats it as a genuinely equivalent technical upgrade with negligible wage-floor impact. Each sibling should carry its own ε and its own stakeholder set; none should average across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
