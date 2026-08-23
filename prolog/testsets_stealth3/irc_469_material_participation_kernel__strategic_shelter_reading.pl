% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
 *   human_readable: IRC Section 469 Material Participation Threshold — Strategic Shelter Reading
 *   domain: economic/legal/regulatory_interpretation
 *
 * SUMMARY:
 *   This story instantiates the strategic_shelter_reading of the IRC Section
 *   469 material participation kernel: the claim that the participation
 *   threshold is a permissive, plannable boundary rather than a verification
 *   gate. The standing arrangement under contest — and the fixed epsilon
 *   referent — is the threshold as it actually operates under this reading:
 *   seven quantitative tests (500-hour, 100-hour-more-than-anyone,
 *   significant-participation, and kin) plus the Section 1.469-4 grouping
 *   elections, administered against sharply shrunken examination capacity,
 *   and serviced by an advisory industry that sells qualification as a
 *   product. Under this operation, depreciation-driven paper losses on
 *   leveraged real estate convert into current deductions against
 *   professional and business income for a broad, advice-accessible
 *   population, while the documentation burden lands undifferentiated on
 *   every filer claiming the losses. Per the epsilon-invariance discipline,
 *   the sibling reading — the verifiable-substantial-labor gate — is a
 *   separate constraint in a separate file; this story authors one stable
 *   epsilon for one reading and links the family through network edges. KEY
 *   AGENTS (by structural relationship): -
 *   high_net_worth_real_estate_investors: Primary beneficiary
 *   (powerful/arbitrage) — collects the deduction channel -
 *   tax_advisory_industry: Secondary beneficiary and de facto administrator
 *   (organized/mobile) — designs and defends the qualification packages -
 *   real_estate_syndicators: Beneficiary (powerful/arbitrage) — monetizes the
 *   deduction in capital raising - treasury_irs_administration: Agenda setter
 *   (institutional/constrained) — administers the kernel with decayed
 *   examination capacity - congressional_tax_committees: Agenda setter
 *   (institutional/constrained) — holds the pen that could redefine the
 *   kernel - compliant_small_landlords: Primary payer (moderate/constrained)
 *   — bears suspension and documentation costs without the planning offsets -
 *   ordinary_income_taxpayers: Diffuse payer (powerless/trapped) — finances
 *   the revenue gap - future_taxpayers: Excluded voice (powerless/trapped) —
 *   inherits the deferred fiscal cost - gao_tax_analysts: Analytical observer
 *   (institutional/analytical) — measures the gap between determination and
 *   activity
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.42).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC Section 469 Material Participation Threshold — Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "economic/legal/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '9311f6f6-7ecd-4705-8a9b-b1298ffa8454').
narrative_ontology:cs_kernel_codification('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', formalized).
narrative_ontology:cs_authority_grounding('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', practice).
narrative_ontology:cs_interpretation_layer_present('9311f6f6-7ecd-4705-8a9b-b1298ffa8454').
narrative_ontology:cs_reading_relation('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', foundational, participation_threshold_is_planning_objective).
narrative_ontology:cs_axiom_status(participation_threshold_is_planning_objective, holdable).
narrative_ontology:cs_axiom_grounding('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', participation_threshold_is_planning_objective, conventional).
narrative_ontology:cs_axiom('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', secondary, structured_engagement_records_suffice_for_loss_character).
narrative_ontology:cs_axiom_status(structured_engagement_records_suffice_for_loss_character, holdable).
narrative_ontology:cs_axiom_grounding('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', structured_engagement_records_suffice_for_loss_character, instrumental).
narrative_ontology:cs_reference_frame('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', permissive_planning_threshold_framework).
narrative_ontology:cs_drift_state('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', contemporary_enforcement_decay_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9311f6f6-7ecd-4705-8a9b-b1298ffa8454', '2026-08-11T14:22:07Z').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndicators).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_small_landlords).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, ordinary_income_taxpayers).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, formal_engagement_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own leveraged rental portfolios and business interests that throw off large annual paper losses, chiefly depreciation. Retain advisory firms to satisfy one of the participation tests each year through hour logs and grouping elections so those losses offset salaries, K-1 income, and operating profits currently. Can move holdings between entity types, pursue real estate professional status where household labor supports it, or absorb a suspended loss when convenient. Planning costs run a small fraction of the deduction value secured.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_real_estate_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% CPA firms and specialty practices design the qualification package: time-tracking templates, contemporaneous-log protocols, grouping-election memoranda, and audit-defense files. Fee income scales with assets under structuring. Through comment letters, continuing-education curricula, and published methodologies they shape what examiners accept as adequate records, effectively co-writing the operating standard. Their expertise ports easily to adjacent compliance niches if this one closes.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, agenda_setter).

% Sponsor funds and exchange programs marketed partly on current loss deductions flowing into investor returns. Structure offerings so sponsor-side participation satisfies the tests at the fund level where the rules permit, and advertise after-tax yield advantages in placement materials. Benefit arrives through faster capital formation and promoted-interest economics; they can relocate offerings across jurisdictions and vehicle types.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndicators, beneficiary,
    powerful, generational, arbitrage, national).

% Writes the regulations, issues audit-technique guidance on passive-activity records, examines returns, and decides which hour-counting and grouping positions survive contact with audit. Examination capacity for the Schedule E and partnership populations has fallen sharply across three decades, shifting the operative standard toward what practitioners can defend cheaply. It cannot decline to administer the regime; every return claiming the losses passes through its processing.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, treasury_irs_administration, agenda_setter,
    institutional, generational, constrained, national).

% Hold the pen that could redefine participation, cap grouping elections, or mandate third-party verification of hours. Repeated tightening proposals have stalled against concentrated industry opposition and a diffuse, unorganized constituency for the status quo. Bound simultaneously to donor industries that rely on the deduction channel and to deficit politics that indict it.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congressional_tax_committees, agenda_setter,
    institutional, generational, constrained, national).

% Own one to a handful of units, keep honest books, and frequently cannot clear the hour bars because a day job crowds out property work — or cannot afford the advisory engineering that clears them. Losses suspend year after year while better-advised peers deduct comparable ones. Selling out to escape triggers depreciation recapture and transaction costs, so holding and absorbing the suspension is usually the least-bad path.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_small_landlords, payer,
    moderate, biographical, constrained, national).

% Wages and salaries fund the general revenues that absorb the deduction channel. They hold no positional play — no grouping election, no entity menu, no advisor retainer — and cannot opt out of financing the difference. Each bears a thin slice individually; collectively the slices are the largest single funding stream behind the arrangement.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, ordinary_income_taxpayers, payer,
    powerless, biographical, trapped, national).

% Will service the debt issued against today's foregone revenue and inherit whichever settlement between the competing readings eventually hardens into law. Present in no comment process, no hearing, and no negotiation; their interests enter only through advocates speaking on their behalf.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Audit the regime from outside the collection apparatus: measure passive-loss volumes, weigh compliance burdens, and report on whether participation determinations track genuine activity. Findings feed hearings and reform proposals but carry no enforcement pen of their own.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, gao_tax_analysts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_real_estate_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sorts taxpayers whose losses arise from trades or businesses they personally operate (currently usable against any income) from those whose losses are passive investment outcomes (banked until offsetting passive income or disposition), solving once and centrally a classification problem that would otherwise be relitigated return by return; the grouping provisions additionally coordinate multi-activity taxpayers into single defensible units.
% TRANSFER_FUNCTION: Moves current deduction capacity — the present cash value of loss write-offs — from the shared revenue base to taxpayers positioned to satisfy the participation thresholds through structured engagement records, and moves documentation costs onto every filer claiming the losses regardless of advice access.
% ABSENT_VOICES: Future taxpayers are structurally absent from every venue where the reading's operating standard hardens; unsophisticated filers are nominally represented but practically absent, since the comment-letter process, advisory literature, and examination-pattern development are dominated by sophisticated repeat players and their counsel.
% DISAPPEARANCE_RATIONALE: Overnight removal of the participation threshold would strip the passive-loss regime of its active/passive sort: either all losses would flow currently, delivering a fiscal shock and repricing leveraged real estate immediately, or all would suspend, freezing disposition and financing across the sector. Either branch reorganizes the after-tax structure of real estate investment at once; no steady state survives the deletion.
% FOUNDING_PROBLEM: The 1986 Tax Reform Act confronted an industry of tax shelters: paper losses from leveraged real estate and equipment leasing wiping out professionals' wage income. Material participation was built to distinguish genuine business operators from passive investors so that losses would follow economic exposure rather than paper structure.
% FOUNDING_PROBLEM_CORROBORATION: GAO reports, Joint Committee on Taxation analyses, and Treasury testimony — all outside the benefiting parties — attest that passive losses continue to shelter salary and business income at material scale, supporting a live-problem reading. Industry trade groups dispute the magnitude and characterize the residual as ordinary planning. The status is therefore contested with corroboration available on both sides, but the attesting sources outside the beneficiary set lean toward the problem persisting in attenuated form.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68: the arrangement transfers substantial deduction value from the shared revenue base to a narrow, advice-accessible class, bounded by the requirement that losses be real economic losses and by at-risk limits — substantial, not maximal. Suppression is 0.42: the coercive apparatus (substantiation demands, penalty regimes, audit risk) is real but its active force has decayed with examination capacity; alternatives are not broadly suppressed. Theater_ratio is 0.58: under this reading a majority of qualifying activity is documentation manufacture — logs assembled or retrofitted for audit defense, grouping elections filed to cross bars — while a genuine residue of sorting and simplification function remains. Accessibility_collapse is 0.40: alternatives persist (paying the tax, real estate professional status, corporate ownership, dealer treatment), so understanding the regime does not close the option space the way a natural limit would. Resistance is 0.45: recurring legislative proposals, IRS initiatives, and academic critique meet organized, effective industry defense. The claim and the metrics are independent authored facts: claimed_type tangled_rope states my structural belief that the arrangement retains a genuine coordination function (the active/passive sort) while carrying asymmetric extraction through the same structure under active enforcement; the metrics describe its observed operation without tuning toward any predicted engine verdict. The measurement series runs on one shared grid (1994, 2000, 2006, 2012, 2018, 2024) with all three tracked metrics authored at every point; the suppression series deliberately traces enforcement-capacity decay, which is the dynamic this story tracks alongside extraction accumulation and Goodhart drift in the theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the structural data explains why. From the investor and advisor positions the arrangement is planning infrastructure they built and can navigate at will — the seven tests are a menu, grouping is a tool, and audit is a priced risk. From the compliant small landlord's position the identical statutory text operates as a wall: losses suspend, documentation demands arrive undifferentiated, and the engineering that would open the door is unaffordable. Ordinary wage earners experience only the financing side. Same rule, opposite lived constraints — differentiated not by the statute's wording but by advisor access, entity menus, and exit options. The engine computes this per-seat divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations carry the directional signal. High-net-worth investors, the advisory industry, and syndicators sit near the beneficiary pole (d approaching 0): the arrangement subsidizes them, and their arbitrage-grade exits damp effective extraction further. Compliant small landlords and ordinary income taxpayers sit near the target pole (d approaching 1): they bear the transfer and the burden with constrained or absent exits, and the small landlords' identity-free but capital-trapped position keeps them from the beneficiary end despite nominal eligibility for the same tests. The administrative seats — Treasury/IRS and the congressional committees — fall near symmetric by fallback: they neither collect the gains nor bear the transfer directly, though both carry enforcement-burden and credibility costs that tilt them slightly toward target. GAO analysts are analytical and directionally neutral. I author no directionality_overrides: the derivation from beneficiary/victim data plus exit options produces the right relationships, and power-atom-keyed overrides cannot distinguish the three institutional seats from one another, so structural data rather than overrides carries the per-agent signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 1986 shelter crisis — is attenuated but not dead: paper losses still shelter labor income, but at lower intensity and through planning rather than raw promotional shelters. Calling the whole regime a snare would erase the genuine sorting function that still binds unsophisticated filers and still channels real business losses correctly; calling it a rope would erase the systematic extraction channel this reading opens for the advice-accessible class. The tangled_rope claim preserves both facts and keeps the mandatrophy question open rather than resolved: the mandate has degraded unevenly, functioning for one population and hollowed for another. The mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): the world does depend on the arrangement, but the parties dispute whether the dependence still serves the founding purpose — exactly the configuration in which capture-or-zombie flags deserve investigation rather than declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the material participation threshold a permissive planning objective satisfiable through structured hour-counting and grouping elections (this reading), or a verification gate demanding documented, substantial personal labor (sibling strict_gatekeeper_reading)?',
    'Authoritative resolution paths: a sustained IRS examination posture against engineered logs, statutory amendment mandating third-party verification of hours, or appellate precedent settling the evidentiary status of grouping elections.',
    'Adopting the sibling reading collapses the qualifying population, raises compliance friction, and narrows the systematic deduction channel to a residual — moving the arrangement''s computed classification toward enforced extraction of the remaining gaming population; entrenching this reading broadens the beneficiary class further and stabilizes the current profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the irc_469 material participation kernel governs the threshold''s operation.').

omega_variable(
    hour_log_authenticity,
    'Do contemporaneous participation logs submitted under this reading record genuine labor, or are they substantially reconstructed compliance artifacts assembled for audit defense?',
    'Examination sampling correlating logged hours against independent activity evidence — property-management records, vendor communications, travel and access data — across a stratified filer sample.',
    'High reconstruction rates would push theater_ratio above its authored 0.58, undercut the reading''s coordination claim, and date the drift toward pure extraction earlier than the measurement series shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hour_log_authenticity, empirical, 'Authenticity of the hour-counting evidence base beneath the reading''s operation.').

omega_variable(
    grouping_election_substance,
    'Do grouping elections aggregate activities with genuine functional integration, or manufacture the scale needed to cross the 500-hour bar?',
    'Systematic review of election filings against the integration criteria of Treas. Reg. Sec. 1.469-4, plus outcome comparison of grouped versus ungrouped qualification rates.',
    'If elections predominantly manufacture scale, the grouping mechanism is a threshold-gaming instrument rather than coordination-faithful simplification, and the reading''s coordination-function claim narrows to the ungrouped population only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grouping_election_substance, empirical, 'Whether the grouping-election channel aggregates real integration or engineers test-crossing scale.').

omega_variable(
    enforcement_capacity_trajectory,
    'Will examination capacity for passive-activity populations continue declining, stabilize, or reverse under budget and legislative trajectories?',
    'Appropriations history and projections, staffing plans for the relevant examination divisions, and legislative action on enforcement funding.',
    'Continued decline pushes the arrangement toward unchecked operation of this reading — a suppression floor with a widening beneficiary class; reversal re-tightens the gate and validates the tangled-rope reading of the current metric profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Trajectory of the enforcement capacity that disciplines the reading''s operation.').

omega_variable(
    deduction_legitimacy_valuation,
    'Is converting passive losses into current deductions through engineered participation records legitimate planning within the rules'' intent, or abusive avoidance of their substance?',
    'Not resolvable by data alone — settles through political and legal valuation: enacted amendments, penalty posture, and professional-standard pronouncements.',
    'A legitimacy verdict selects the policy endpoint: codifying the permissive reading stabilizes the arrangement as accepted coordination, while adopting the strict reading contracts the beneficiary class sharply and re-prices the whole channel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deduction_legitimacy_valuation, preference, 'Value dispute over whether the reading''s operation is legitimate planning or abuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1994, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement_basis(irc__tr_t1994, observed).
narrative_ontology:measurement(irc__tr_t2000, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement_basis(irc__tr_t2000, observed).
narrative_ontology:measurement(irc__tr_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2006, 0.41).
narrative_ontology:measurement_basis(irc__tr_t2006, observed).
narrative_ontology:measurement(irc__tr_t2012, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2012, 0.47).
narrative_ontology:measurement_basis(irc__tr_t2012, observed).
narrative_ontology:measurement(irc__tr_t2018, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2018, 0.53).
narrative_ontology:measurement_basis(irc__tr_t2018, observed).
narrative_ontology:measurement(irc__tr_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(irc__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t1994, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1994, 0.48).
narrative_ontology:measurement_basis(irc__be_t1994, observed).
narrative_ontology:measurement(irc__be_t2000, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(irc__be_t2000, observed).
narrative_ontology:measurement(irc__be_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2006, 0.57).
narrative_ontology:measurement_basis(irc__be_t2006, observed).
narrative_ontology:measurement(irc__be_t2012, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2012, 0.61).
narrative_ontology:measurement_basis(irc__be_t2012, observed).
narrative_ontology:measurement(irc__be_t2018, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement_basis(irc__be_t2018, observed).
narrative_ontology:measurement(irc__be_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(irc__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1994, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1994, 0.58).
narrative_ontology:measurement_basis(irc__su_t1994, observed).
narrative_ontology:measurement(irc__su_t2000, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(irc__su_t2000, observed).
narrative_ontology:measurement(irc__su_t2006, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2006, 0.5).
narrative_ontology:measurement_basis(irc__su_t2006, observed).
narrative_ontology:measurement(irc__su_t2012, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2012, 0.47).
narrative_ontology:measurement_basis(irc__su_t2012, observed).
narrative_ontology:measurement(irc__su_t2018, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2018, 0.44).
narrative_ontology:measurement_basis(irc__su_t2018, observed).
narrative_ontology:measurement(irc__su_t2024, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(irc__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_real_estate_professional_exception).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the material participation test.' The natural-language concept covers two structurally distinct arrangements with different epsilon values: (1) this file, the strategic_shelter_reading — the threshold as operated under permissive, planning-driven qualification, epsilon 0.68, broad qualifying population, low compliance friction for the advice-accessible class; and (2) the strict_gatekeeper_reading — the threshold as a verifiable-labor gate with a high documentation bar, narrow qualifying population, materially different beneficiary/victim structure and classification. Measuring the arrangement one way yields clearly lower extractiveness; measuring it the other yields clearly higher — per the epsilon-invariance principle these are two constraints, not one with a measurement parameter. The upstream statutory baseline (textually closer to the strict reading) is routinely cited by enforcement constituencies as evidence for the gate reading, while the regulatory permissiveness is cited by practitioners as evidence for this reading; both edges run through network.affects_constraints. The real estate professional exception is linked as the adjacent escape hatch whose availability modulates this reading's qualifying population.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
