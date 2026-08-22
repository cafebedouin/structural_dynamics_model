% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC Section 469 Material Participation Gate - Strict Gatekeeper Reading
 *   domain: economic/legal/regulatory
 *
 * SUMMARY:
 *   IRC Section 469 suspends passive activity losses unless the taxpayer
 *   materially participates; the strict gatekeeper reading instantiates the
 *   gate as requiring verifiable, substantial personal labor proved to a high
 *   documentation standard. Under this reading the qualifying population
 *   narrows sharply, compliance friction is high, and passive losses rarely
 *   reach ordinary income. The arrangement has a genuine coordination core -
 *   preventing shelter-driven base erosion that harms every other taxpayer -
 *   while imposing asymmetric costs: the documentation bar over-deters
 *   genuine participants with imperfect records and feeds a substantial
 *   advisory-fee economy keyed to the bar's height. Claim and metrics are
 *   authored independently: the claimed type reflects my structural judgment
 *   (coordination plus asymmetric extraction, actively enforced); the metrics
 *   describe observed operation. This file is one reading of the
 *   irc_469_material_participation_kernel; the strategic_shelter_reading is a
 *   separate constraint linked in network.affects_constraints, and committer
 *   structure is routed to omega variables per Rules 1-2.
 *
 * KEY AGENTS:
 *   - - internal_revenue_service: Agenda-setter (institutional/constrained) - administers the tests, sets examination posture, bears enforcement cost
 *   - - federal_treasury: Primary beneficiary (institutional/constrained) - receives preserved revenue; receipts scale with bar strictness
 *   - - general_taxpayer_base: Diffuse beneficiary (moderate/mobile) - protected from base erosion; benefit invisible and unorganized
 *   - - tax_advisory_industry: Incidental beneficiary (organized/mobile) - fee stream scales with documentation-bar height
 *   - - small_real_estate_investors: Payer (moderate/constrained) - real labor, informal records, suspended losses
 *   - - borderline_documentation_taxpayers: Payer (powerless/trapped) - genuine participation failing the evidentiary bar, mid-hold with recognition risk on exit
 *   - - shelter_promotion_syndicators: Excluded (powerful/arbitrage) - the enforcement object; pivots to adjacent vehicles
 *   - - tax_court_judiciary: Analytical observer (analytical/analytical) - adjudicates where the bar actually sits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.47).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC Section 469 Material Participation Gate - Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "economic/legal/regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '12b0fb9c-9f5a-474a-bfbf-b479e97eb179').
narrative_ontology:cs_kernel_codification('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', formalized).
narrative_ontology:cs_authority_grounding('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', lineage).
narrative_ontology:cs_interpretation_layer_present('12b0fb9c-9f5a-474a-bfbf-b479e97eb179').
narrative_ontology:cs_reading_relation('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', irc_469_material_participation_kernel__strategic_shelter_reading, forecloses).
narrative_ontology:cs_axiom('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', foundational, verification_constitutes_participation).
narrative_ontology:cs_axiom_status(verification_constitutes_participation, holdable).
narrative_ontology:cs_axiom_grounding('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', verification_constitutes_participation, instrumental).
narrative_ontology:cs_axiom('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', secondary, elective_aggregation_cannot_substitute_for_labor).
narrative_ontology:cs_axiom_status(elective_aggregation_cannot_substitute_for_labor, holdable).
narrative_ontology:cs_axiom_grounding('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', elective_aggregation_cannot_substitute_for_labor, conventional).
narrative_ontology:cs_reference_frame('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', verified_labor_substantiation_baseline).
narrative_ontology:cs_drift_state('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', contemporary_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('12b0fb9c-9f5a-474a-bfbf-b479e97eb179', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, general_taxpayer_base).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, federal_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, borderline_documentation_taxpayers).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, tra86_horizontal_equity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the participation tests: publishes the hour-count thresholds and facts-and-circumstances standards, examines returns, disallows loss deductions that lack substantiation, and defends the documentation bar in litigation. It operates inside a statute it did not write and inside appropriation cycles that alternately expand and contract its examination capacity.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, internal_revenue_service, agenda_setter,
    institutional, generational, constrained, national).

% Receives the revenue preserved whenever a loss deduction is denied; its receipts move directly with the strictness of the substantiation standard. It comments on regulations and estimates revenue effects but does not run examinations.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, federal_treasury, beneficiary,
    institutional, generational, constrained, national).

% The mass of households and businesses reporting income without passive-loss offsets. They file nothing extra under the gate and gain indirectly whenever shelter-driven base erosion is prevented; the benefit arrives as marginally lower rates or deficits rather than as anything visible. Any individual can simply never enter a passive activity, so exposure is voluntary.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, general_taxpayer_base, beneficiary,
    moderate, generational, mobile, national).

% CPAs, tax attorneys, and preparers who sell hour-tracking systems, contemporaneous log templates, grouping-election memoranda, and audit defense. Their fee stream scales with the height of the documentation bar, and they simultaneously file comment letters and litigate test cases that shape how the bar is applied.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_industry, beneficiary,
    organized, biographical, mobile, national).

% Professionals and savers holding one to a few rental properties who perform real management labor - repairs, tenant turnover, bookkeeping - often in irregular bursts. Whether their losses reach their wages turns on whether their records survive an examination; many keep adequate informal books but not contemporaneous logs built to the examination standard. Selling out means realizing accumulated losses against little or no current income.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_real_estate_investors, payer,
    moderate, biographical, constrained, national).

% Taxpayers mid-hold whose participation is genuine but whose records fall short: reconstructed calendars, missing mileage logs, contractor invoices without time entries. They carry suspended loss carryforwards year after year, pay continuing professional fees to defend the position, and face recognition consequences if they unwind the holding that generated the losses.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, borderline_documentation_taxpayers, payer,
    powerless, biographical, trapped, national).

% Sponsors and marketers of pooled passive-investment products sold on tax-loss generation. The 1986 Act removed their core product; they now operate at the margins of the regime - cost-segregation studies, short-term-rental exceptions, oil-and-gas offerings - and lobby for permissive application of the participation standards they can no longer sell around.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, shelter_promotion_syndicators, excluded,
    powerful, immediate, arbitrage, national).

% Decides the disputes the gate generates: whether hours were worked, whether logs are credible, whether activities properly group. Applies the standard case by case; its published opinions are the main public record of where the bar actually sits.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_judiciary, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, federal_treasury).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem in the tax base: without a shared definition of who may claim trade-or-business loss treatment, passive-loss shelters let a subset convert ordinary-rate income into paper losses and shift the burden onto everyone else. The gate coordinates all participants on a single, examinable definition of qualifying engagement.
% TRANSFER_FUNCTION: Moves deductible-loss capacity and the tax revenue it represents: under the strict reading it withholds loss-offset capacity from taxpayers who cannot verify substantial labor, transferring preserved revenue to the general fund and compliance fees to the advisory industry.
% ABSENT_VOICES: Small investors with genuine but undocumented labor bear the bar's sharpest costs yet have no seat in rulemaking; their experience surfaces only as litigated cases years after positions are taken. Shelter-market participants are deliberately outside the conversation since 1986, and the diffuse taxpayer base that nominally benefits is represented only by proxy.
% DISAPPEARANCE_RATIONALE: If the gate vanished overnight, pooled loss-shelter structures would reconstitute within a few filing seasons - as they did before 1986 - shifting tens of billions in annual liability off passive-investment income onto wage earners, collapsing the compliance-advisory market built on the current bar, and forcing the IRS to rebuild an anti-abuse apparatus from scratch.
% FOUNDING_PROBLEM: Pre-1986 tax shelters: wealthy investors bought tax-motivated ventures - equipment leasing, real estate, oil and gas - engineered to produce paper losses deducted against salaries, eroding the tax base and horizontal equity at scale.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: TRA86 legislative history and Treasury Blueprints document the founding problem; academic tax-policy literature quantifies pre-1986 shelter volume; and contemporary congressional testimony on cost-segregation abuse and short-term-rental loss harvesting attests the underlying incentive persists wherever loss capacity meets ordinary income.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.47): the primary flow is revenue preservation benefiting the broad base, but real burdens concentrate narrowly - denied deductions, suspended carryforwards, and advisory fees paid by the very taxpayers seeking the status. Suppression (0.58) is a raw structural property, unscaled by power or scope: the gate actively suppresses shelter strategies (its purpose) and conditions access to loss treatment on substantiation many cannot produce, backed by examination and accuracy penalties. Theater (0.28) reflects documentation theater - logs constructed for audit defense rather than lived work patterns - alongside a substantively functional labor requirement. Accessibility collapse is low-moderate (0.42): alternatives persist (genuine qualification, grouping, real-estate-professional election, accepting passive treatment, choosing non-passive assets). Resistance (0.52) is continuous litigation, aggressive elections, and recurring legislative proposals to relax the rules. All three temporal series share one six-point grid (1986-2026). The suppression_requirement series is authored because this story genuinely tracks enforcement-capacity change: requirements ratcheted with the 1992 final regulations, decayed through the post-1998-reform contraction in examination capacity (trough at 2008), and partially recovered since. Extraction rises gently throughout as the advisory-rent layer thickens on top of a stable bar.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the IRS and Treasury positions the gate is legitimate administration: a rule applied evenly, defending a base everyone shares. From the small-investor and borderline-taxpayer seats the identical structure operates as enforced extraction: their real labor is discounted for want of paper, while the advisory industry monetizes the gap between labor and proof. The advisory seat occupies a third position - it benefits from the bar's height and simultaneously shapes its application through comment letters and test cases. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and the general taxpayer base sit near the full-beneficiary end: they receive protection and preserved revenue and bear no filing burden; the base's mobile exit (never entering a passive activity) places it nearest the subsidy end. The advisory industry is a beneficiary with a twist - its take scales with the friction itself - but it is still structurally subsidized by the arrangement. Small real estate investors and borderline documentation taxpayers sit near the full-target end; the latter's trapped exit (mid-hold, suspended carryforwards, recognition consequences on unwind) pushes them toward maximal effective extraction despite modest nominal stakes. Shelter syndicators are excluded rather than coordinated: suppressing their product is the enforcement object itself, so their position is defined by the gate's coercive edge, not by payment into it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Calling the gate pure extraction ignores the genuine collective-action function: pre-1986 shelters were a real, large-scale raid on the base, and the founding problem is corroborated as live by sources outside the benefiting parties. Calling it pure coordination ignores the asymmetric incidence: the documentation bar over-deters genuine participants, and a measurable share of compliance spending purchases defensive redundancy and advisory rent rather than substantiation value. Tangled rope holds both truths. On obsolescence: the founding problem is live, so no resolved mandatrophy is declared; the monitored symptom is the slow theater-ratio climb as documentation practice drifts toward audit-defense performance - watched, but treated as a symptom rather than the test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the irc_469_material_participation_kernel (reading: strict_gatekeeper_reading). What would the sibling strategic_shelter_reading change structurally, and where exactly does the disagreement bind?',
    'Adoption of the sibling reading - by legislation, regulatory reinterpretation, or sustained judicial acceptance of asserted hour-counts and grouping elections - would widen the qualifying population, collapse the documentation bar, and shift the burden incidence measured here.',
    'Under the sibling reading the victim set contracts toward non-sheltering taxpayers, the advisory-rent layer thins, and the arrangement''s classification moves away from the mixed coordination/extraction profile computed for this reading; the two stories must never be scored on a blended epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel reading this story instantiates and what the sibling reading would structurally change.').

omega_variable(
    substantiation_bar_calibration,
    'Does the documentation bar track what fraud prevention actually requires, or is it set above the anti-abuse necessity level, over-deterring genuine participants?',
    'Compare detection and deterrence outcomes across varying substantiation thresholds - safe-harbor experiments, state-level analogues, and measured false-negative rates (genuine participants denied) against audited-fraud incidence.',
    'If the bar exceeds necessity, the excess segment is extraction riding on the coordination function and effective extraction for payer seats rises; if calibrated, the burden is closer to the inherent cost of the anti-abuse coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantiation_bar_calibration, empirical, 'Whether the evidentiary bar is calibrated to abuse prevention or over-deterrent.').

omega_variable(
    advisory_rent_capture,
    'What share of taxpayer compliance spending buys genuine substantiation value versus defensive redundancy and advisory rent keyed to the bar''s height?',
    'Benchmark preparation and defense fees against the expected value of substantiation produced; survey practitioner billing composition across bar-height jurisdictions and eras.',
    'A high rent share raises effective extraction on payer seats and supports reading the advisory industry as a structural maintainer of bar height rather than a neutral interpreter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_rent_capture, empirical, 'How much of the compliance economy is rent versus service.').

omega_variable(
    practitioner_identity_fusion,
    'Is specialist resistance to simplifying the participation tests driven by client economics or by professional identity - technical mastery of the passive-loss regime as status?',
    'Position analysis of professional bodies on simplification proposals against member revenue exposure; if opposition persists where members would gain, identity rather than economics is operative.',
    'If identity-driven, deregulatory proposals face resistance beyond economic interest, entrenching the current bar independently of its calibration and slowing any drift toward the sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_identity_fusion, conceptual, 'Cognitive-capture ambiguity: economic versus identity-based maintenance of the documentation bar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1986, 0.14).
narrative_ontology:measurement_basis(irc__tr_t1986, observed).
narrative_ontology:measurement(irc__tr_t1992, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1992, 0.19).
narrative_ontology:measurement_basis(irc__tr_t1992, observed).
narrative_ontology:measurement(irc__tr_t2000, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement_basis(irc__tr_t2000, observed).
narrative_ontology:measurement(irc__tr_t2008, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(irc__tr_t2008, observed).
narrative_ontology:measurement(irc__tr_t2017, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2017, 0.27).
narrative_ontology:measurement_basis(irc__tr_t2017, observed).
narrative_ontology:measurement(irc__tr_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(irc__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1986, 0.32).
narrative_ontology:measurement_basis(irc__be_t1986, observed).
narrative_ontology:measurement(irc__be_t1992, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement_basis(irc__be_t1992, observed).
narrative_ontology:measurement(irc__be_t2000, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(irc__be_t2000, observed).
narrative_ontology:measurement(irc__be_t2008, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2008, 0.44).
narrative_ontology:measurement_basis(irc__be_t2008, observed).
narrative_ontology:measurement(irc__be_t2017, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2017, 0.46).
narrative_ontology:measurement_basis(irc__be_t2017, observed).
narrative_ontology:measurement(irc__be_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2026, 0.47).
narrative_ontology:measurement_basis(irc__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement_basis(irc__su_t1986, observed).
narrative_ontology:measurement(irc__su_t1992, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1992, 0.57).
narrative_ontology:measurement_basis(irc__su_t1992, observed).
narrative_ontology:measurement(irc__su_t2000, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(irc__su_t2000, observed).
narrative_ontology:measurement(irc__su_t2008, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2008, 0.51).
narrative_ontology:measurement_basis(irc__su_t2008, observed).
narrative_ontology:measurement(irc__su_t2017, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2017, 0.55).
narrative_ontology:measurement_basis(irc__su_t2017, observed).
narrative_ontology:measurement(irc__su_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(irc__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'material participation' (epsilon-invariance principle). The single statutory term covers two structurally distinct arrangements: this story instantiates the strict_gatekeeper_reading (narrow qualifying population, high documentation friction, passive losses rarely reaching ordinary income), while the sibling story instantiates the strategic_shelter_reading (permissive threshold reachable through aggressive hour-counting and grouping elections). Their epsilon values differ materially because the incidence of burden and protection differs; they are separate constraints sharing one kernel, linked here. The strict reading functions upstream in policy discourse: its anti-abuse rationale is cited as the reason permissive application must be resisted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
