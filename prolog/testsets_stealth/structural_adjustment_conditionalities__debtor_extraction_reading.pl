% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities — Debtor-Extraction Reading
 *   domain: economic/political_international
 *
 * SUMMARY:
 *   Since the early 1980s, international lending to fiscally stressed states
 *   has been extended subject to binding policy conditions: expenditure cuts,
 *   subsidy removal, user fees in health and education, privatization of
 *   state enterprises, trade and capital-account liberalization, and currency
 *   devaluation — monitored through performance reviews with tranche
 *   disbursement withheld on noncompliance. This story authors that standing
 *   arrangement as the debtor-extraction reading sees it: a regime whose
 *   operative effect is a sustained transfer of fiscal resources, public
 *   assets, and policy discretion out of adjusting polities toward external
 *   creditors and purchasers, carried by financial coercion rather than
 *   participant assent. Per the epsilon-referent rule, extractiveness is
 *   assessed on this existing arrangement by this reading's own lights —
 *   never on any endorsed alternative. Claim and metrics are independent
 *   authored facts: the claimed type is snare; the metrics describe the
 *   arrangement's operation as this reading measures it. This file is the
 *   extraction member of a three-story family decomposing the colloquial
 *   label; see network.dual_formulation_note. KEY AGENTS (by structural
 *   relationship): - imf_executive_board: agenda-setter
 *   (institutional/arbitrage) — approves programs, sets performance criteria,
 *   withholds tranches - g7_finance_ministries: beneficiary with
 *   agenda-setting weight (institutional/arbitrage) — voting shares, informal
 *   direction - transnational_creditor_banks: primary beneficiary
 *   (institutional/arbitrage) — debt-service receipts, socialized rescues -
 *   privatized_asset_purchasers: beneficiary (powerful/mobile) —
 *   distressed-asset acquisitions - creditor_country_exporters: beneficiary
 *   (organized/mobile) — opened markets, devalued competition -
 *   domestic_technocratic_elites: dual-positioned beneficiary/payer
 *   (moderate/identity_locked) — program careers, patronage -
 *   adjusting_government_finance_ministries: payer with local administration
 *   duties (moderate/constrained) - urban_service_dependent_poor: primary
 *   payer (powerless/trapped) — user fees, subsidy loss -
 *   retrenched_public_workers: payer (organized/trapped) — wage-bill cuts,
 *   downsizing - rural_smallholder_farmers: payer (powerless/trapped) —
 *   input-subsidy removal, crop conversion - jubilee_debt_campaigners:
 *   excluded voice (organized/constrained) — outside negotiation rooms -
 *   independent_evaluation_office: analytical observer
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - imf_executive_board: agenda-setter (institutional/arbitrage) — approves programs, sets performance criteria, withholds tranches
 *   - g7_finance_ministries: beneficiary with agenda-setting weight (institutional/arbitrage) — voting shares, informal direction
 *   - transnational_creditor_banks: primary beneficiary (institutional/arbitrage) — debt-service receipts, socialized rescues
 *   - privatized_asset_purchasers: beneficiary (powerful/mobile) — distressed-asset acquisitions
 *   - creditor_country_exporters: beneficiary (organized/mobile) — opened markets, devalued competition
 *   - domestic_technocratic_elites: dual-positioned beneficiary/payer (moderate/identity_locked) — program careers, patronage
 *   - adjusting_government_finance_ministries: payer with local administration duties (moderate/constrained)
 *   - urban_service_dependent_poor: primary payer (powerless/trapped) — user fees, subsidy loss
 *   - retrenched_public_workers: payer (organized/trapped) — wage-bill cuts, downsizing
 *   - rural_smallholder_farmers: payer (powerless/trapped) — input-subsidy removal, crop conversion
 *   - jubilee_debt_campaigners: excluded voice (organized/constrained) — outside negotiation rooms
 *   - independent_evaluation_office: analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.7).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities — Debtor-Extraction Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "economic/political_international").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '34353272-8e63-491b-aa91-6c46f71dab4c').
narrative_ontology:cs_kernel_codification('34353272-8e63-491b-aa91-6c46f71dab4c', formalized).
narrative_ontology:cs_authority_grounding('34353272-8e63-491b-aa91-6c46f71dab4c', extraction).
narrative_ontology:cs_interpretation_layer_present('34353272-8e63-491b-aa91-6c46f71dab4c').
narrative_ontology:cs_reading_relation('34353272-8e63-491b-aa91-6c46f71dab4c', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('34353272-8e63-491b-aa91-6c46f71dab4c', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('34353272-8e63-491b-aa91-6c46f71dab4c', foundational, population_welfare_precedes_debt_service).
narrative_ontology:cs_axiom_status(population_welfare_precedes_debt_service, holdable).
narrative_ontology:cs_axiom_grounding('34353272-8e63-491b-aa91-6c46f71dab4c', population_welfare_precedes_debt_service, deontological).
narrative_ontology:cs_axiom('34353272-8e63-491b-aa91-6c46f71dab4c', foundational, nonconsensual_conditionality_lacks_legitimate_authority).
narrative_ontology:cs_axiom_status(nonconsensual_conditionality_lacks_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('34353272-8e63-491b-aa91-6c46f71dab4c', nonconsensual_conditionality_lacks_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('34353272-8e63-491b-aa91-6c46f71dab4c', sovereign_social_contract_autonomy).
narrative_ontology:cs_drift_state('34353272-8e63-491b-aa91-6c46f71dab4c', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('34353272-8e63-491b-aa91-6c46f71dab4c', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, privatized_asset_purchasers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_country_exporters).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, g7_finance_ministries).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_technocratic_elites).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, urban_service_dependent_poor).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, retrenched_public_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, rural_smallholder_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_technocratic_elites).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, adjusting_government_finance_ministries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approves lending programs and their attached policy conditions, reviews compliance at scheduled checkpoints, and releases or withholds subsequent tranches accordingly. Members answer to national capitals and rotate through the seat; leaving the arrangement means returning home, not exiting its effects.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_executive_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold controlling voting shares and set the informal direction the board follows. Their treasuries receive debt-service flows and their banking systems hold the underlying exposure; they can reshape terms through quota and leadership decisions without ever borrowing under the rules themselves.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, g7_finance_ministries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, g7_finance_ministries, agenda_setter).

% Hold sovereign loans and bonds issued under program frameworks and receive scheduled debt service. When payment capacity breaks, official rescue lending restores their repayment streams. Exposure can be sold into secondary markets, so their position in any single debtor is liquid even while the framework persists.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    institutional, biographical, arbitrage, global).

% Acquire state utilities, mines, and telecommunications operators offered for sale under program-mandated divestiture, frequently at prices set by distressed sellers. Profits are repatriated under liberalized capital-account rules; operations can be relocated or wound down across jurisdictions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, privatized_asset_purchasers, beneficiary,
    powerful, biographical, mobile, global).

% Gain newly opened import markets and compete against devalued local production. Sales channels are diversified across many economies, so exposure to any single adjusting country is limited and redirectable.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_country_exporters, beneficiary,
    organized, biographical, mobile, continental).

% Staff the central banks and finance ministries that draft program letters, and move afterward into consultancies, multilateral posts, and corporate boards. Careers and training are built inside the framework's methods; dissenting from it would forfeit the professional standing the framework confers. They also absorb some electoral and reputational backlash for policies they implement.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_technocratic_elites, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_technocratic_elites, payer).

% Sign the agreements, legislate the cuts, and administer implementation — closing clinics, raising fees, selling enterprises — while bearing street protests and election losses. Renouncing the framework invites suspension of disbursement and loss of market access; staying offers continued budget support and discretionary patronage within the envelope.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, adjusting_government_finance_ministries, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, adjusting_government_finance_ministries, agenda_setter).

% Pay newly introduced user fees for clinic visits and schooling, absorb price increases from subsidy removal, and lose informal-sector income when demand contracts. There is no jurisdiction to move to that lies outside the framework's reach; coping means skipped meals, doses, and school terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, urban_service_dependent_poor, payer,
    powerless, immediate, trapped, national).

% Face wage freezes, hiring ceilings, and layoffs under civil-service downsizing targets. Unions that resist encounter strike restrictions and emergency decrees; jobs lost in downsized industries do not return in the same regions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, retrenched_public_workers, payer,
    organized, biographical, trapped, national).

% Lose fertilizer and seed subsidies, shift from food crops to export crops under marketing liberalization, and face land consolidation toward larger export producers. Mobility is limited by land ties and credit dependence on the same channels the programs restructure.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, rural_smallholder_farmers, payer,
    powerless, biographical, trapped, regional).

% Organize outside the negotiation architecture — churches, NGOs, student movements — arguing for repudiation or unconditional relief. Formal channels are limited to petition and protest; they are not seated in program negotiations, and their proposals enter the record only as public pressure.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, jubilee_debt_campaigners, excluded,
    organized, generational, constrained, global).

% Audits program design and outcomes, publishes findings including unfavorable ones, and recommends revisions. Its seat is observational: it collects testimony from all sides and holds no vote on program terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, independent_evaluation_office, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single table where dispersed creditors and a distressed sovereign settle one repayment framework, and supplies liquidity timed to policy milestones so that competing claims do not trigger a scramble for exit.
% TRANSFER_FUNCTION: Moves budgetary resources, public assets, and policy discretion from adjusting-country populations and states toward external creditors, asset purchasers, and exporting producers — via debt service, divestiture proceeds, user fees, and mandated policy changes.
% ABSENT_VOICES: Debtor-country legislatures and the users of cut services are absent from program negotiation rooms; terms are settled between finance ministries and Fund staff before parliamentary review, and repudiation advocates (Jubilee-style campaigns, default theorists) hold no seat. Their objections survive only as street pressure and election results.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force immediate renegotiation of every outstanding program: some states would default and restructure on their own timelines, creditor balance sheets would reprice, privatization pipelines would halt, and domestic budgets would reopen around restored subsidies and services — a wholesale rearrangement of crisis-finance arrangements.
% FOUNDING_PROBLEM: Recurrent balance-of-payments crises in which a sovereign cannot meet external obligations, compounded by the collective-action problem of dispersed creditors each racing to be repaid first.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the fund's own independent evaluation office has documented recurring design failures and protracted program cycles; academic development economics and UNCTAD analyses attest both the reality of the original liquidity problem and the dispute over whether the arrangement still addresses it. No attesting source sits wholly outside the Bretton Woods orbit, which is itself signal.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The end-state scores describe the arrangement as the debtor-extraction reading measures it at interval close. Extractiveness is high (0.82): debt service absorbs large shares of export earnings and budgets, privatization proceeds flow outward, and user fees convert previously public provision into household expenditure. Suppression (0.70) is a raw structural property — unscaled by power or scope — resting on tranche withholding, capital-market lockout threats, and restrictions on public-sector collective action. Theater (0.48) reflects the growth of consultative and ownership language whose policy output tracks prior program templates. Accessibility collapse is moderate (0.50): exit routes exist — unilateral default, alternative creditors — but each carries heavy, publicly advertised penalty, so alternatives persist yet are priced to deter. Resistance (0.62) is sustained: repeated austerity riots, electoral turnover of program governments, and outright defaults. The temporal series run on one shared grid (1980-2020, six points, every tracked metric authored at every point) and trace a wave rather than a monotonic ramp: intensification through the 1980s-1990s, a concession trough in the early 2000s following the global-justice protest cycle and internal evaluation criticism, then re-tightening after 2008 and through pandemic-era lending. The oscillation is driven partly by external crisis timing and partly by protest-concession response; milestone-based disbursement also gives the arrangement an intermittent-reinforcement texture, since relief arrives only between compliance episodes. Coalition potential among the powerless victim seats is real but systematically frustrated: creditor sequencing (negotiating with treasuries one at a time), information asymmetry, and the domestic elite seat's stake in continuation have repeatedly broken debtor-cartel attempts, which is why powerless seats remain individually trapped despite aggregate numbers. Coordination type is resource_allocation: the genuine coordination problem solved is allocating scarce liquidity and sequencing competing repayment claims during crises; the type default floor applies, no override.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the same documents read as orderly crisis management: liquidity supplied, discipline attached, sustainability restored. From the payer seats the identical clauses read as externally drafted budgets: services priced out of reach, employment cut, assets sold under duress. Institutionally, the board and G7 ministries hold arbitrage-grade exit (rotation, quota politics, salable exposure), debtor treasuries are constrained, and debtor populations are trapped — the same nominal event lands as four different lived arrangements. The engine computes per-seat classifications from the structural data — power, exit, and declared position — and the divergence between those computations is the finding; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (creditor banks, asset purchasers, exporters, G7 ministries, domestic technocratic elites) sit near the beneficiary end of d, with exit quality reinforcing the placement — banks and ministries hold arbitrage-grade exit, purchasers are mobile. Declared victims (service-dependent poor, retrenched workers, smallholders) sit near the full-target end, with trapped exit pinning them there. The two dual-positioned seats split: adjusting finance ministries carry payer costs but administer locally and retain discretionary rents, tempering their d below the trapped-poor seats; domestic technocratic elites collect program-linked rewards while bearing reputational and electoral spillovers, and their identity-locked exit keeps them structurally committed to continuation. The excluded campaigner seat sits outside the derivation — its exclusion is maintained by the negotiation architecture itself. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the intended placements.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating creditors and financing balance-of-payments gaps — remains disputed rather than dead: crises recur, so the arrangement is never obviously vestigial, which is precisely what makes the snare reading load-bearing. Classifying from the payer side prevents coordination-washing (a genuine creditor-coordination function coexisting with asymmetric transfer does not certify the whole as rope), while the contested founding-problem status blocks the opposite error of declaring the mandate resolved and the arrangement inert. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no dead-mandate/zombie flag fires, correctly — the arrangement is vigorously maintained, not theatrically preserved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_structural_adjustment,
    'This story instantiates one reading (debtor_extraction) of the kernel structural_adjustment_conditionalities; what structural facts would the sibling readings (creditor_coordination, hybrid_selectivity) change, and where exactly do the readings disagree?',
    'Compile all three sibling stories and compare computed per-seat classifications and epsilon values; locate the disagreement in function attribution (the arrangement''s primary purpose) and victim identification rather than in any observable.',
    'If the creditor-coordination sibling computes negligible excess extraction from the same structural data, the snare verdict here is reading-indexed, not topic-indexed; if all three converge on high extraction, the kernel itself is extractive regardless of framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structural_adjustment, conceptual, 'Committer-frame uncertainty: one of three rival readings of the conditionality kernel.').

omega_variable(
    no_program_counterfactual_baseline,
    'What would debtor-country trajectories have been absent conditionality-linked lending — deeper collapse, or comparable distress without the imposed policy changes?',
    'Matched comparisons of adjusting versus non-adjusting crisis countries with similar initial conditions; natural experiments where programs were interrupted (arrears episodes, post-default paths such as Argentina after 2001).',
    'If counterfactual outcomes were worse, part of the measured harm is crisis cost rather than regime-imposed transfer, lowering effective epsilon; if comparable or better, the full measure attaches to the arrangement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_program_counterfactual_baseline, empirical, 'Counterfactual baseline for attributing harm to the arrangement rather than to the underlying crises.').

omega_variable(
    suppression_structural_vs_internalized_tina,
    'Is the measured suppression primarily structural (tranche withholding, capital-market lockout threats, strike restrictions) or internalized (technocratic conviction that no alternative exists)?',
    'Post-exit suppression trajectory: states that left the framework (post-default episodes) — did policy space remain narrowed by belief and career structures, or reopen with restored market access?',
    'If internalized, suppression travels beyond enforcement reach and effective suppression exceeds the structural measure; the identity-locked technocrat seat''s directionality rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized_tina, empirical, 'Structural versus internalized suppression mechanism in the enforcing and administered seats.').

omega_variable(
    domestic_elite_capture_share,
    'What share of the transferred value accrues to domestic technocratic and political elites versus external creditors and asset purchasers?',
    'Follow-the-money studies of privatization proceeds, offshore wealth accumulation by program-era officials, and consultancy and board appointment flows.',
    'A large domestic share complicates a purely external-transfer account and strengthens the dual-role reading of the domestic elite seat; a small share supports the external-creditor concentration recorded in gain_flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_elite_capture_share, empirical, 'Distribution of captured value between domestic intermediary seats and external creditor seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sap_debtor_extraction_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(sap_debtor_extraction_tr_t1988, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1988, 0.26).
narrative_ontology:measurement(sap_debtor_extraction_tr_t1996, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1996, 0.36).
narrative_ontology:measurement(sap_debtor_extraction_tr_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2004, 0.46).
narrative_ontology:measurement(sap_debtor_extraction_tr_t2012, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2012, 0.43).
narrative_ontology:measurement(sap_debtor_extraction_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.48).

% Extraction over time
narrative_ontology:measurement(sap_debtor_extraction_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(sap_debtor_extraction_be_t1988, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1988, 0.71).
narrative_ontology:measurement(sap_debtor_extraction_be_t1996, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1996, 0.8).
narrative_ontology:measurement(sap_debtor_extraction_be_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2004, 0.73).
narrative_ontology:measurement(sap_debtor_extraction_be_t2012, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2012, 0.79).
narrative_ontology:measurement(sap_debtor_extraction_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sap_debtor_extraction_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(sap_debtor_extraction_su_t1988, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1988, 0.67).
narrative_ontology:measurement(sap_debtor_extraction_su_t1996, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1996, 0.76).
narrative_ontology:measurement(sap_debtor_extraction_su_t2004, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2004, 0.64).
narrative_ontology:measurement(sap_debtor_extraction_su_t2012, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2012, 0.73).
narrative_ontology:measurement(sap_debtor_extraction_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: 'structural adjustment conditionalities' is a colloquial label covering at least three structurally distinct claims — pure coordination, pure extraction, selective discipline. Each gets its own story, its own epsilon, its own beneficiaries and victims. Citation pressure historically runs from the official coordination framing to the critique that formed against it. This file is the extraction member; its epsilon (0.82) is authored from the extraction reading's lights over the fixed referent of the standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
