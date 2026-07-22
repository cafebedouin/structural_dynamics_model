% ============================================================================
% CONSTRAINT STORY: instrument_dependent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_instrument_dependent_reading, []).

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
 *   constraint_id: instrument_dependent_reading
 *   human_readable: Adverse Effect Wage Rate Guarantee, Instrument-Dependent Reading
 *   domain: administrative_law/labor_economics/immigration_policy
 *
 * SUMMARY:
 *   This story instantiates the instrument-dependent reading of the
 *   adverse-effect guarantee kernel: the claim that the statutory wage
 *   protection for H-2A guest-workers is only as real as the wage survey
 *   instrument used to measure it, and that discontinuing the survey
 *   functionally suspends the protection regardless of what the statute still
 *   says. Under this reading, the guarantee has not been repealed, litigated
 *   away, or converted into a different regulatory channel — it has simply
 *   become unenforceable because the comparison test at its core requires a
 *   comparison population that no longer has a live data series. The result
 *   reads as a piton: enforcement machinery that persists in name (the
 *   statute, the methodology regulations, the agency's nominal authority)
 *   while the actual protective function has atrophied through instrument
 *   neglect rather than active repeal. No party designed this outcome by
 *   intent captured in this reading; the employer association benefits by
 *   default, not by engineered capture — that distinction is what separates
 *   this reading from the sibling capture_reading.
 *
 * KEY AGENTS:
 *   - h2a_guestworkers: primary target (powerless/trapped) — bears wage suppression that cannot be measured or proven
 *   - domestic_farmworkers: secondary target (powerless/constrained) — bears the same evidentiary foreclosure from the other side of the labor market
 *   - h2a_employers_association: default beneficiary (organized/arbitrage) — retains wage-setting discretion at zero cost by inertia, not design
 *   - department_of_labor: agenda_setter (institutional/constrained) — could restore the instrument but faces no concentrated pressure to do so
 *   - future_wage_litigants: excluded class (powerless/trapped) — foreclosed from ever bringing the claim the statute was built to allow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(instrument_dependent_reading, 0.62).
domain_priors:suppression_score(instrument_dependent_reading, 0.58).
domain_priors:theater_ratio(instrument_dependent_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(instrument_dependent_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(instrument_dependent_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(instrument_dependent_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(instrument_dependent_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(instrument_dependent_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(instrument_dependent_reading, piton).
narrative_ontology:human_readable(instrument_dependent_reading, "Adverse Effect Wage Rate Guarantee, Instrument-Dependent Reading").
narrative_ontology:topic_domain(instrument_dependent_reading, "administrative_law/labor_economics/immigration_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(instrument_dependent_reading, 'ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0').
narrative_ontology:cs_kernel_codification('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', fixed_text).
narrative_ontology:cs_authority_grounding('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', extraction).
narrative_ontology:cs_interpretation_layer_present('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0').
narrative_ontology:cs_reading_relation('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', adverse_effect_guarantee_kernel__textualist_severability_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', adverse_effect_guarantee_kernel__coverage_neutral_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', adverse_effect_guarantee_kernel__capture_reading, influences).
narrative_ontology:cs_reading_relation('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', adverse_effect_guarantee_kernel__channel_conversion_reading, influences).
narrative_ontology:cs_axiom('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', foundational, guarantee_is_coextensive_with_measurement_instrument).
narrative_ontology:cs_axiom_status(guarantee_is_coextensive_with_measurement_instrument, holdable).
narrative_ontology:cs_axiom_grounding('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', guarantee_is_coextensive_with_measurement_instrument, conventional).
narrative_ontology:cs_axiom('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', foundational, unprovability_functions_as_de_facto_suspension).
narrative_ontology:cs_axiom_status(unprovability_functions_as_de_facto_suspension, holdable).
narrative_ontology:cs_axiom_grounding('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', unprovability_functions_as_de_facto_suspension, instrumental).
narrative_ontology:cs_reference_frame('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', statutory_wage_floor_with_live_comparison_instrument).
narrative_ontology:cs_drift_state('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', post_survey_discontinuation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('ce30e0ff-f06c-4a79-81ce-f160e9a7e6a0', '').
narrative_ontology:cs_kernel_id(instrument_dependent_reading, adverse_effect_guarantee_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(instrument_dependent_reading, h2a_employers_association).
narrative_ontology:constraint_victim(instrument_dependent_reading, h2a_guestworkers).
narrative_ontology:constraint_victim(instrument_dependent_reading, domestic_farmworkers).
narrative_ontology:constraint_victim(instrument_dependent_reading, future_wage_litigants).
narrative_ontology:constraint_vindicates(instrument_dependent_reading, statutory_text_remains_formally_intact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are statutorily promised a wage floor calibrated to prevent their presence from depressing wages for comparable domestic work. The floor is set by reference to a wage survey instrument. When the instrument is discontinued or degraded, there is no current comparison population against which adverse effect can be measured, so no worker can demonstrate the depression the statute exists to prevent. Visa sponsorship ties them to a single employer; departing mid-season means losing status and wages owed.
narrative_ontology:constraint_stakeholder(instrument_dependent_reading, h2a_guestworkers, payer,
    powerless, biographical, trapped, national).

% Compete for the same agricultural jobs at wages the guarantee was designed to protect from downward pressure. Without a functioning measurement instrument, any wage depression they experience from an expanded H-2A workforce is structurally unprovable in litigation or rulemaking challenge, because the comparison test requires exactly the data series that has been discontinued.
narrative_ontology:constraint_stakeholder(instrument_dependent_reading, domestic_farmworkers, payer,
    powerless, biographical, constrained, national).

% Any party, worker or employer, who might in the future need to prove or disprove adverse effect in either direction is foreclosed by the absence of the measurement series itself. They are excluded from the current controversy because the harm to them is prospective and diffuse — a standing problem before it is ever a merits problem.
narrative_ontology:constraint_stakeholder(instrument_dependent_reading, future_wage_litigants, excluded,
    powerless, civilizational, trapped, national).

% Benefits from wage rates that no longer rise with a functioning comparison instrument, and from the practical impossibility of adverse-effect challenges to current wage-setting methodology. Did not necessarily lobby to kill the specific survey; benefits by default from its discontinuation and from every day the agency does not replace it, at essentially zero cost to itself.
narrative_ontology:constraint_stakeholder(instrument_dependent_reading, h2a_employers_association, beneficiary,
    organized, generational, arbitrage, national).

% Administers the wage-setting methodology and could commission, restore, or replace the discontinued survey instrument. Faces budget constraints, competing statutory priorities, and no concentrated political pressure to fix the instrument, because the group most harmed (comparison-population workers who cannot prove harm) cannot generate the evidentiary record that would force action.
narrative_ontology:constraint_stakeholder(instrument_dependent_reading, department_of_labor, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate challenges to wage methodology under the APA and INA. Under this reading, courts confronting a claim that the guarantee has been suspended in substance face the specific problem that a comparison test without a comparison population is not just weak evidence but structurally unenforceable — there is no denominator to test against.
narrative_ontology:constraint_stakeholder(instrument_dependent_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(instrument_dependent_reading, h2a_employers_association).
narrative_ontology:fixing_cost_class(instrument_dependent_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The wage survey instrument exists to solve a genuine coordination problem: how to set a guest-worker wage floor that tracks actual local labor-market conditions rather than an arbitrary or employer-set number, so that importing foreign labor does not become a mechanism for suppressing domestic wages.
% TRANSFER_FUNCTION: When the instrument is discontinued, the arrangement stops moving information (comparison wage data) into the wage-setting process, and by that omission it moves real income from workers whose wages would have risen with a live comparison series to employers who retain wage-setting discretion by default.
% ABSENT_VOICES: Domestic farmworkers and H-2A workers most affected by frozen or stale wage floors have no seat in the technical decision to discontinue or not replace the survey instrument; that decision is made inside agency budget and methodology processes where labor voice is thin and employer trade associations are well-represented.
% DISAPPEARANCE_RATIONALE: Employers dispute that anything has disappeared, since the statutory guarantee remains on the books unchanged; worker advocates and some courts would say the substantive protection has already disappeared in all but name, because the measurement precondition for its enforcement no longer exists. The world does not visibly rearrange because the text persists — but the protection it names is not operative.
% FOUNDING_PROBLEM: Congress and the Department of Labor built the adverse-effect wage guarantee to prevent temporary foreign agricultural labor programs from becoming a channel for undercutting wages that domestic workers would otherwise receive, following documented wage suppression under earlier guest-worker programs.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists and Government Accountability Office reports, sitting outside both the employer association and the worker advocacy groups, have documented gaps and discontinuities in the underlying wage survey data series and their effect on wage-setting outcomes; the employer association attests the founding problem no longer applies to current, integrated labor markets, while worker advocates attest it is as live as ever and simply unmeasured.
narrative_ontology:disappearance_verdict(instrument_dependent_reading, contested).
narrative_ontology:founding_problem_status(instrument_dependent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(instrument_dependent_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(instrument_dependent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(instrument_dependent_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(instrument_dependent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(instrument_dependent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(instrument_dependent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate-rising (0.28 to 0.62) rather than starting high, because the harm accrues gradually as the discontinued instrument ages and wage floors drift further from actual market conditions each season the survey is not restored. Theater ratio rises sharply (0.20 to 0.71) because an increasing share of the agency's compliance activity — annual wage-rate announcements, methodology memos, statutory citations — becomes performative continuation of a process whose evidentiary foundation no longer exists. Suppression (0.58) is moderate: there is no active coercive enforcement against workers who might challenge the methodology, but the structural unavailability of the comparison population functions as a suppression mechanism in its own right, foreclosing the claim before it can be litigated. Resistance is low (0.35) precisely because the harm is diffuse and hard to organize around — you cannot build a movement around a statistic that does not exist.
 *
 * PERSPECTIVAL GAP:
 *   From the department_of_labor's agenda_setter seat, this looks like a continuing, lawful administration of an unchanged statute — nothing has been suspended, only a data source has lapsed pending budget reauthorization. From the h2a_guestworkers and domestic_farmworkers payer seats, the same arrangement looks like the practical death of a promised protection, indistinguishable in effect from repeal, because the mechanism that would let them prove harm has been quietly disabled. The engine's per-seat computation should register this asymmetry: the agenda_setter's low measured extraction sits against payer seats whose trapped/constrained exit options and high accessibility_collapse push their effective extraction well upward.
 *
 * DIRECTIONALITY LOGIC:
 *   H-2A guestworkers and domestic farmworkers are declared as victims because the discontinued instrument removes their only path to demonstrating the specific harm (wage depression) the statute was built to prevent — this pushes their derived directionality toward the full-target end. The employer association is declared beneficiary because it retains wage-setting discretion at no cost and no risk of successful legal challenge — even though, under this reading, it did not engineer the instrument's discontinuation, only benefits from it, which is why gain_flow names it directly rather than declaring the gains diffuse: the story establishes a concentrated recipient of the resulting discretion, even absent capture-style intent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is sharp: the founding problem (guest-worker programs undercutting domestic wages) is authored as contested-status rather than flatly dead, because independent GAO and economist corroboration suggests the underlying wage dynamics the statute targeted may still be live, even though the specific measurement apparatus meant to detect them is not. This is exactly the case the framework is built to catch: a statute that has NOT been repealed, whose founding problem may still exist, but whose enforceability has been hollowed out by instrument neglect — labeling this either 'the protection is fine, the text says so' or 'the protection was always fake' would both be wrong. It is a piton: atrophied through inertia, not dismantled through design, and the fix (restoring or replacing the survey instrument) is authored as cheap relative to the harm it prevents, which is precisely why its non-restoration is diagnostic rather than merely unfortunate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrument_identity_vs_severability,
    'Is the adverse-effect guarantee structurally identical to its measurement instrument (such that discontinuing the instrument suspends the guarantee), or is the guarantee a legal obligation severable from any particular measurement method (the textualist_severability_reading)?',
    'Judicial resolution of an APA challenge arguing the agency has a nondiscretionary duty to maintain SOME functioning comparison methodology, even if not the specific discontinued survey; alternatively, agency rulemaking adopting a substitute instrument would moot the question empirically.',
    'If the instrument-dependent reading prevails, the guarantee is currently unenforceable in substance despite intact statutory text — supporting the piton classification here. If the severability reading prevails, the same facts support a tangled_rope or scaffold classification under the sibling story, since the agency would then have an affirmative, currently-breached duty rather than a lapsed-by-default protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrument_identity_vs_severability, conceptual, 'Whether the guarantee and its measurement instrument are structurally fused or severable — the central committer disagreement across kernel readings.').

omega_variable(
    default_versus_designed_capture,
    'Did the employer association or its allies actively lobby for or cause the survey''s discontinuation, or did it lapse through ordinary budget attrition and agency neglect, with the employer association merely benefiting afterward?',
    'Discovery of agency budget deliberation records, appropriations riders, and any employer-association lobbying disclosures contemporaneous with the discontinuation decision.',
    'If designed, this constraint should be re-read as the capture_reading sibling (tangled_rope or snare, with active enforcement of the discontinuation itself); if default, the piton classification under low-intent framing holds, since a piton characteristically has no party who engineered the atrophy, only one who benefits from not fixing it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(default_versus_designed_capture, empirical, 'Whether beneficiary status arose by design (capture) or by default (piton) — determines which sibling reading actually describes the facts on the ground.').

omega_variable(
    diffuse_versus_concentrated_gain,
    'Does the discretion freed up by the discontinued instrument accrue concentratedly to the h2a_employers_association, or is it dissipated across a wider, less organized set of agricultural employers with no single capturing seat?',
    'Wage and employment data disaggregated by employer size and association membership, to see whether wage-floor stagnation benefits align with organized employer membership or are spread evenly across the sector.',
    'This governs whether gain_flow should remain named at the association or be revised to ''diffuse'' — a diffuse finding would push the classification further toward pure piton (no concentrated beneficiary at all), while a concentrated finding keeps some snare-adjacent pressure alive even under the low-intent frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_versus_concentrated_gain, empirical, 'Whether the freed wage-setting discretion is captured by an organized beneficiary or genuinely diffuse across the employer class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(instrument_dependent_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, instrument_dependent_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inst_tr_t4, instrument_dependent_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(inst_tr_t8, instrument_dependent_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(inst_tr_t12, instrument_dependent_reading, theater_ratio, 12, 0.53).
narrative_ontology:measurement(inst_tr_t16, instrument_dependent_reading, theater_ratio, 16, 0.61).
narrative_ontology:measurement(inst_tr_t20, instrument_dependent_reading, theater_ratio, 20, 0.67).
narrative_ontology:measurement(inst_tr_t24, instrument_dependent_reading, theater_ratio, 24, 0.71).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, instrument_dependent_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(inst_be_t4, instrument_dependent_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(inst_be_t8, instrument_dependent_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(inst_be_t12, instrument_dependent_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(inst_be_t16, instrument_dependent_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(inst_be_t20, instrument_dependent_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(inst_be_t24, instrument_dependent_reading, base_extractiveness, 24, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(instrument_dependent_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(instrument_dependent_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(instrument_dependent_reading, textualist_severability_reading).
narrative_ontology:affects_constraint(instrument_dependent_reading, coverage_neutral_reading).
narrative_ontology:affects_constraint(instrument_dependent_reading, capture_reading).
narrative_ontology:affects_constraint(instrument_dependent_reading, channel_conversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the adverse_effect_guarantee_kernel, each instantiating a structurally distinct constraint from the same statutory text and the same discontinued measurement instrument. The instrument_dependent_reading treats the guarantee as coextensive with its measurement apparatus and classifies as piton under low-intent framing. textualist_severability_reading treats the legal obligation as surviving the instrument's loss (likely tangled_rope, with an unmet agency duty). coverage_neutral_reading treats enforceability gaps as orthogonal to the guarantee's continued legal scope. capture_reading treats the same facts as designed regulatory capture (likely snare or tangled_rope with active enforcement of the gap itself). channel_conversion_reading treats the protective function as having migrated elsewhere rather than disappeared (likely rope or scaffold). Each carries its own epsilon and stakeholder set; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
