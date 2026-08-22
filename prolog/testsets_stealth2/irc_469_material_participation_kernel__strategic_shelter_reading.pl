% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC 469 Material Participation Threshold (Strategic Shelter Reading)
 *   domain: economic/legal_regulatory
 *
 * SUMMARY:
 *   Section 469 of the Internal Revenue Code (1986) suspends losses from
 *   passive activities unless the owner materially participates; material
 *   participation is therefore the gate deciding whether a loss stream may
 *   offset wages and business income. This file instantiates ONE reading of
 *   that gate, the strategic-shelter reading, under which the threshold is
 *   permissive by design: countable hour tests plus grouping elections under
 *   Treas. Reg. 1.469-4 let an advised taxpayer order affairs into
 *   qualification, and the practitioner community treats hour logs and
 *   elections as engineering deliverables rather than descriptions of fact.
 *   The standing arrangement under contest, the epsilon referent, is that
 *   permissive-threshold operation as it actually runs: broad qualifying
 *   population, low compliance friction, systematic passive-loss deduction
 *   against active income. Epsilon is authored through THIS reading's lights,
 *   which legitimize the transfer as lawful planning and price its costs as
 *   policy tradeoffs, so it sits low-moderate; the sibling file
 *   (strict_gatekeeper_reading) authors the same referent through a
 *   verifiable-substantial-labor lens and lands far higher. That divergence
 *   over one shared referent is the indexical datum the kernel exists to
 *   take. Claim and metrics are independent: the structural data below is
 *   authored as it stands, not reconciled to the reading's self-image.
 *
 * KEY AGENTS:
 *   - high_income_real_estate_investors: Primary beneficiary (powerful/arbitrage) — collects the deduction flow that preserves compounding wealth
 *   - tax_structuring_advisors: Beneficiary and de facto administrator (organized/mobile) — sells and runs the qualification machinery for fees
 *   - real_estate_syndication_sponsors: Secondary beneficiary (organized/arbitrage) — monetizes the deduction pitch in deal marketing
 *   - us_federal_treasury: Primary target (institutional/trapped) — bears foregone revenue with no exit from its funding obligation
 *   - compliant_wage_earners: Secondary target (powerless/trapped) — bears the shifted burden with no equivalent loss streams
 *   - irs_exam_and_appeals: Enforcer seat (institutional/constrained) — administers a gate it cannot tighten and frequently loses litigating
 *   - unsophisticated_small_landlords: Excluded seat (powerless/trapped) — suffers the fairness gap without a voice in the practice community
 *   - tax_court_and_appeals_judiciary: Analytical observer (institutional/analytical) — adjudicates contests without settling the standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.31).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.42).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation Threshold (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "economic/legal_regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'c861d48a-eb72-4769-9258-15d963675e1b').
narrative_ontology:cs_kernel_codification('c861d48a-eb72-4769-9258-15d963675e1b', formalized).
narrative_ontology:cs_authority_grounding('c861d48a-eb72-4769-9258-15d963675e1b', lineage).
narrative_ontology:cs_interpretation_layer_present('c861d48a-eb72-4769-9258-15d963675e1b').
narrative_ontology:cs_reading_relation('c861d48a-eb72-4769-9258-15d963675e1b', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('c861d48a-eb72-4769-9258-15d963675e1b', foundational, statutory_thresholds_exhaust_the_inquiry).
narrative_ontology:cs_axiom_status(statutory_thresholds_exhaust_the_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('c861d48a-eb72-4769-9258-15d963675e1b', statutory_thresholds_exhaust_the_inquiry, conventional).
narrative_ontology:cs_axiom('c861d48a-eb72-4769-9258-15d963675e1b', foundational, brightline_administrability_beats_substantive_inquiry).
narrative_ontology:cs_axiom_status(brightline_administrability_beats_substantive_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('c861d48a-eb72-4769-9258-15d963675e1b', brightline_administrability_beats_substantive_inquiry, instrumental).
narrative_ontology:cs_reference_frame('c861d48a-eb72-4769-9258-15d963675e1b', permissive_brightline_threshold_framework).
narrative_ontology:cs_drift_state('c861d48a-eb72-4769-9258-15d963675e1b', contemporary_str_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c861d48a-eb72-4769-9258-15d963675e1b', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_structuring_advisors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndication_sponsors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, us_federal_treasury).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_wage_earners).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, brightline_administrability_doctrine).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, affair_ordering_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own rental and business activity portfolios that generate losses while earning large salary or operating income elsewhere. Engage advisors to construct participation records and grouping elections that bring them inside the qualifying population, converting suspended losses into current deductions against active income. The preserved tax is compounding wealth; exit means restructuring holdings, shifting activity mixes, or accepting suspension.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors, beneficiary,
    powerful, generational, arbitrage, national).

% CPAs, enrolled agents, and tax attorneys who design participation strategies, prepare hour logs and contemporaneous-looking records, file grouping elections, and defend the positions in examination. Fee income scales with shelter size. They run the qualification machinery day to day and effectively define what the threshold means in practice; exit is dropping the practice line or moving to adjacent advisory work.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_structuring_advisors, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, tax_structuring_advisors, agenda_setter).

% Package and market investment deals whose offering materials lean on tax-shield features, structuring investor touchpoints such as scheduled meetings, office hours, and tracked decision participation so that nominally passive subscribers can claim qualifying involvement. Deal flow depends on the deduction pitch staying credible.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndication_sponsors, beneficiary,
    organized, biographical, arbitrage, national).

% Collects less revenue than a verified-labor gate would yield, bearing the gap as higher borrowing or shifted burden onto other taxpayers. Cannot exit its obligation to fund the government. Its remedy channels, regulation and legislation, run through a political process in which the beneficiary seats are durably and concentratedly represented.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, us_federal_treasury, payer,
    institutional, generational, trapped, national).

% Pay tax on salaries with no equivalent loss streams to deduct and no advisor budget to manufacture qualification. Bear the shifted share of federal funding. Cannot opt out of taxation; their remedy is diffuse electoral pressure that almost never targets this provision specifically, though a coalition with other diffuse payers is theoretically available.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_wage_earners, payer,
    powerless, biographical, trapped, national).

% Audits returns asserting material participation, tests hour logs against calendar metadata, bank records, and third-party evidence, and litigates credibility contests in Tax Court. Loses frequently on witness-credibility grounds. Examination capacity and competing priorities cap how many positions it can challenge, and it cannot rewrite the tests it administers; its realistic lever is selective litigation that nudges documentation norms at the margin.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_exam_and_appeals, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicates case-by-case contests between the readings without settling the standard. Some decisions demand contemporaneous written records and reject after-the-fact reconstructions; others credit testimony and rough logs. Precedent accumulates loosely, leaving both readings live across circuits and years.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_court_and_appeals_judiciary, observer,
    institutional, generational, analytical, national).

% Own one or two rentals, lack advisor budgets, and keep rough records. Their losses suspend year after year while better-advised peers deduct identical underlying economics. They would object to the fairness gap between advised and unadvised taxpayers but have no seat in the practice community where the operative reading is made and maintained.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, unsophisticated_small_landlords, excluded,
    powerless, biographical, trapped, national).

% Wrote section 469 in 1986 and retain power to redefine participation, harden substantiation, or restrict grouping. Face concentrated, well-resourced industry opposition against diffuse revenue benefits, so periodic tightening proposals stall in committee.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congress_taxwriting_committees, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws an administrable line between owners who genuinely operate income-producing activities and owners who merely hold them, so that losses from the former may offset other income while losses from the latter are suspended. Countable hour tests and grouping rules replace an open-ended inquiry into engagement with bright lines a taxpayer can know in advance.
% TRANSFER_FUNCTION: Moves deductible-loss capacity, realized as deferred tax, from the federal revenue base to taxpayers who engineer qualification, and moves fee income from those taxpayers to advisory and syndication firms. The mirror image is a shifted tax burden onto taxpayers without access to engineered qualification.
% ABSENT_VOICES: Unsophisticated small landlords facing permanent suspension of identical economics, the diffuse general taxpaying public bearing the shifted burden, and strict-gatekeeper proponents inside Treasury and academia would all object to the permissive-threshold operation. None sits inside the practitioner community where the reading is operative; their objections surface only episodically in legislative hearings, GAO reports, and law-review commentary.
% DISAPPEARANCE_RATIONALE: If the permissive-threshold operation vanished overnight, millions of dollars of planned deductions would evaporate, syndication products priced on tax shields would reprice downward, advisory practices built on qualification engineering would contract sharply, and Treasury receipts would rise. The real estate investment market would reorganize around whatever gate replaced the engineered one.
% FOUNDING_PROBLEM: The pre-1986 wave of paper-loss tax shelters: marketed write-off schemes in which investors with no operational role deducted losses that stripped large volumes of income from taxation.
% FOUNDING_PROBLEM_CORROBORATION: GAO and Joint Committee on Taxation revenue analyses, produced outside the beneficiary set, document the continuing revenue cost of the rules' current operation. Academic tax scholarship attests that the original paper-shelter population was largely eliminated by the 1990s. Treasury anti-abuse notices on short-term-rental structures attest the boundary problem recurring in mutated form. Industry comment letters and advisor publications attest the opposite, that the gate remains necessary as written. The dispute between these external and internal attestations is itself why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).
:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.31 at interval end and reading-indexed: through the strategic-shelter reading's lights the revenue transfer is lawful planning, and the reading acknowledges only its priced costs, the revenue gap, advisor skim, and the advised-versus-unadvised fairness gap, which grow slowly as the playbook industrializes (0.15 to 0.31 across the interval). Suppression is 0.42 and is a raw structural property, unscaled by power or scope: the arrangement persists not by coercing participants but by keeping the strict-gatekeeper alternative from consolidating, through enforcement starvation, burden-of-proof realities, and political protection of the beneficiary seats. Theater is 0.55: a majority of qualifying activity is record manufacture for audit defense, logs reconstructed around known test lines, elections filed as formalities, while genuine management of real properties coexists alongside it. Accessibility collapse is 0.50: among advised taxpayers the playbook collapses the alternative of honest non-qualification almost completely, but fee and knowledge barriers keep the collapse partial for the unadvised. Resistance is 0.55: persistent IRS litigation, Treasury anti-abuse notices on short-term-rental structures, and recurring reform proposals that consistently stall. All three temporal series run on one shared seven-point grid so every metric is authored at every examined time point; trajectories are monotonic, rising extraction and theater against falling applied suppressive force as enforcement capacity attrited and the arrangement normalized.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the investor and advisor seats the arrangement is functioning coordination: clear rules, satisfied on their face, delivering what the statute offers. From the Treasury and compliant-wage-earner seats the same structure operates as enforced extraction with trapped exits. The IRS seat occupies a distinctive intermediate position: it expends scarce resources litigating credibility contests it often loses, administering a gate the political system will not let it tighten, which is closer to a target position than its agenda-setter role suggests. The judiciary seat oscillates case by case between the readings. Coalition potential for the powerless seats exists in theory, wage earners plus unadvised landlords as a diffuse fairness coalition, but it almost never forms around this provision because the harm is diffuse and the beneficiary opposition is concentrated. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map directly onto structural relationships: investors, advisors, and sponsors sit near the beneficiary end of directionality, amplified for investors by arbitrage-grade exit (they restructure freely within the rules) and damped for advisors whose position is fee-mediated. Victims map to the target end: the Treasury is trapped at full institutional commitment and wage earners are trapped by citizenship and law, so both sit near the full-target end despite very different power levels, which is precisely the asymmetry the effective-extraction computation should register. No directionality overrides are authored: the derivation chain from roles, power atoms, and exit options produces the right relationships for every seat, including the IRS, whose target-leaning position emerges from its constrained exit and enforcement-burden situation rather than from a manual correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, pre-1986 paper-loss shelters marketed to non-participating investors, is substantively dead in its original form; section 469 succeeded at killing it. The arrangement nonetheless persists and has been repurposed as a wealth-preservation channel for participants who engineer qualification, which is classic mandate-outlived-function territory. The tangled-rope classification prevents mislabeling in both directions: calling the arrangement a pure snare ignores the residual genuine gating, zero-participation paper losses remain blocked and the active/passive boundary still does real coordinative work, while calling it a pure rope, the reading's own self-image, ignores the named victims and the capture-shaped receipt flow. The founding-problem status is authored contested rather than dead because the boundary problem demonstrably recurs in mutated forms, short-term-rental structures chief among them, and the parties genuinely dispute whether today's engineered qualification is the old disease or legitimate planning. The mismatch consumer reads the status-by-verdict pair against the computed piton and theater path; the elevated theater ratio here is consistent with partial atrophy of the original gating function without full inertial takeover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (strategic_shelter_reading) of the irc_469_material_participation_kernel; what would the sibling strict_gatekeeper_reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification of the sibling file over the same referent: the strict reading narrows the qualifying population, raises the documentation bar, converts the advised-qualification flow into suspended losses, and authors high epsilon through its own lights. The disagreement lives in the participation-content and evidentiary-bar elements, not in the statutory text both readings accept.',
    'If the strict reading were adopted as operative law, this file''s beneficiary set loses its deduction flow, the victim set shrinks toward the Treasury''s enforcement-cost position alone, and the arrangement''s classification migrates toward a defended coordination gate; the two files must never be merged into one epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one indexed reading of a contested kernel, with the disagreement located in participation-content and evidentiary-bar elements.').

omega_variable(
    hour_log_verifiability,
    'Do the hour logs and participation records that qualify taxpayers under this reading describe real labor, or are they predominantly constructed artifacts assembled around the known test lines?',
    'Randomized examination studies comparing self-prepared logs against independent evidence trails: calendar and email metadata, third-party vendor records, property-management system data.',
    'If logs are largely constructed, the arrangement''s coordination function is thinner than the reading claims, the theater ratio understates performance, and the residual gating benefit shrinks toward zero, pushing the structural truth toward pure extraction riding a statutory shell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hour_log_verifiability, empirical, 'Whether measured participation reflects real management labor or manufactured records.').

omega_variable(
    revenue_cost_counterfactual,
    'How large is the actual revenue transfer from engineered qualification, measured against a verified-labor counterfactual rather than against current-law baselines?',
    'Joint Committee on Taxation or Congressional Budget Office scoring of a strict-substantiation counterfactual, with distributional tables separating advised from unadvised taxpayers.',
    'A large counterfactual gap would establish that the transfer component dominates the coordination benefit even on sympathetic accounting, pressuring the reading-indexed epsilon upward and strengthening extraction-weighted classifications at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_cost_counterfactual, empirical, 'Magnitude of the fisc-to-investor transfer under a verified-labor counterfactual.').

omega_variable(
    grouping_election_indeterminacy,
    'Do grouping elections under Treas. Reg. 1.469-4 aggregate genuinely integrated economic units, or do they manufacture scale that lets small activities clear thresholds no single activity could?',
    'Examination-level review of elected groupings against the regulation''s integration factors, plus comparison of deduction outcomes for identical portfolios with and without elections.',
    'If manufacturing dominates, the threshold is gamed at the aggregation layer rather than the hour layer, meaning hardening documentation standards alone would not close the arrangement and the coordination function is weaker than the bright-line framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grouping_election_indeterminacy, conceptual, 'Whether the appropriate-economic-unit standard disciplines grouping or licenses threshold manufacturing.').

omega_variable(
    str_exception_recurrence,
    'Does the short-term-rental exception, average stay of seven days or less combined with significant participation, mark a genuine recurrence of the founding shelter problem in mutated form, or a bounded edge case?',
    'Treasury enforcement data on short-term-rental positions, transaction-volume studies of STR structures marketed on tax shields, and revenue attribution from affected returns.',
    'Recurrence would shift the founding-problem status from contested toward live, strengthen the case that the arrangement still performs its original gating function, and cut against mandatrophy resolutions that would treat the constraint as purely vestigial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(str_exception_recurrence, empirical, 'Whether the founding shelter problem recurs through the short-term-rental exception.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(irc__tr_t0, observed).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(irc__tr_t6, observed).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(irc__tr_t12, observed).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement_basis(irc__tr_t18, observed).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(irc__tr_t24, observed).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(irc__tr_t30, observed).
narrative_ontology:measurement(irc__tr_t36, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 36, 0.55).
narrative_ontology:measurement_basis(irc__tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(irc__be_t0, observed).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 6, 0.19).
narrative_ontology:measurement_basis(irc__be_t6, observed).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement_basis(irc__be_t12, observed).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement_basis(irc__be_t18, observed).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement_basis(irc__be_t24, observed).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement_basis(irc__be_t30, observed).
narrative_ontology:measurement(irc__be_t36, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 36, 0.31).
narrative_ontology:measurement_basis(irc__be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(irc__su_t0, observed).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(irc__su_t6, observed).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(irc__su_t12, observed).
narrative_ontology:measurement(irc__su_t18, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement_basis(irc__su_t18, observed).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(irc__su_t24, observed).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(irc__su_t30, observed).
narrative_ontology:measurement(irc__su_t36, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 36, 0.42).
narrative_ontology:measurement_basis(irc__su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'section 469 material participation' decomposes into two structurally distinct constraints sharing one statutory referent: this file (strategic_shelter_reading, permissive threshold, low reading-indexed epsilon, broad qualifying population) and irc_469_material_participation_kernel__strict_gatekeeper_reading (verifiable substantial labor, high epsilon, narrow qualifying population). Per the epsilon-invariance principle these are two constraints, not one constraint viewed from two angles: different epsilon, different failure modes, different beneficiary and victim salience. The readings coexist across factions of an ongoing interpretive dispute; neither logically eliminates the other within the legal system as a whole, though each excludes the other for any single taxpayer-year. The upstream common ground is the enacted text and regulations themselves; each reading instantiates a different operating constraint downstream of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
