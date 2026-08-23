% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   Under this reading, the structural-adjustment lending system — standby
 *   and extended arrangements, structural adjustment loans, and their
 *   successor facilities, each disbursing in tranches against signed policy
 *   benchmarks — operates as an enforced transfer machine. Finance ministries
 *   sign letters of intent committing to wage-bill cuts, subsidy removal, tax
 *   changes, trade liberalization, and privatization; disbursement follows
 *   completion of prior actions, not need; other official lenders and donors
 *   align on the fund's judgment (cross-conditionality), and private markets
 *   price sovereign paper off program status, so falling out of a program
 *   closes refinancing everywhere at once. The payment hierarchy services
 *   external claims first; the adjustment is absorbed domestically — clinics
 *   that begin charging, subsidies that vanish, payrolls that shrink, farms
 *   switched to export crops whose prices then fall. This file authors the
 *   debtor_extraction_reading of the kernel
 *   structural_adjustment_conditionalities as a clean, epsilon-invariant
 *   constraint: the referent is the standing conditional-lending arrangement
 *   itself, assessed by this reading's own lights (epsilon approximately
 *   0.86). The kernel decomposes into three readings — this one,
 *   creditor_coordination_reading (the same term sheets read as necessary
 *   creditor coordination; its own file, its own lower epsilon), and
 *   hybrid_selectivity_reading (discipline applied selectively by
 *   geopolitical weight; its own file) — linked through
 *   network.affects_constraints. The coordination reading is the upstream
 *   establishment claim whose approval statistics and restored-market-access
 *   narratives this reading contests from below; no epsilon is averaged
 *   across readings. KEY AGENTS (by structural relationship): -
 *   imf_wb_program_authorities: Agenda-setting enforcer
 *   (institutional/arbitrage) — designs prior actions, gates disbursement,
 *   staff revolve into private finance - transnational_creditor_banks:
 *   Primary beneficiary (powerful/mobile) — receives protected debt service;
 *   1980s–90s rescues routed public money to its balance sheets -
 *   foreign_portfolio_investors: Secondary beneficiary (powerful/arbitrage) —
 *   buys distressed paper and privatized assets behind program credibility -
 *   northern_export_industries: Ancillary beneficiary (organized/mobile) —
 *   gains opened markets and deregulated investment regimes -
 *   creditor_government_treasuries: Political guarantor and beneficiary
 *   (institutional/arbitrage) — steers quotas and policy, carries tail risk
 *   of failed rescues - debtor_governments: Dual-positioned intermediary
 *   (moderate/constrained) — signs and implements terms, surrenders budget
 *   discretion, retains office and deal-flow access -
 *   debtor_domestic_populations: Primary target (powerless/trapped) — absorbs
 *   fee introduction, subsidy removal, devaluation, service collapse -
 *   public_sector_workers: Organized target (organized/constrained) — bears
 *   retrenchment and wage-bill caps - smallholder_farmers: Diffuse target
 *   (powerless/trapped) — bears input-subsidy removal, crop conversion, price
 *   collapse - civil_society_debt_movements: Excluded opposition
 *   (organized/constrained) — demands audits and cancellation from outside
 *   the room - heterodox_development_economists: Analytical observer
 *   (analytical/analytical) — documents the outcome record and term-sheet
 *   mechanics
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.86).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.78).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities — Debtor-Extraction Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "economic/political/international").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '46f73f0c-ea22-4bfc-9215-d4a4d27b98d1').
narrative_ontology:cs_kernel_codification('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', formalized).
narrative_ontology:cs_authority_grounding('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', extraction).
narrative_ontology:cs_interpretation_layer_present('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1').
narrative_ontology:cs_reading_relation('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', foundational, conditionality_stream_constitutes_coercive_transfer).
narrative_ontology:cs_axiom_status(conditionality_stream_constitutes_coercive_transfer, holdable).
narrative_ontology:cs_axiom_grounding('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', conditionality_stream_constitutes_coercive_transfer, empirically_contingent).
narrative_ontology:cs_axiom('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', foundational, external_budget_veto_is_illegitimate_per_se).
narrative_ontology:cs_axiom_status(external_budget_veto_is_illegitimate_per_se, holdable).
narrative_ontology:cs_axiom_grounding('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', external_budget_veto_is_illegitimate_per_se, deontological).
narrative_ontology:cs_reference_frame('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', creditor_controlled_policy_extraction).
narrative_ontology:cs_drift_state('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', multipolar_credit_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46f73f0c-ea22-4bfc-9215-d4a4d27b98d1', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_portfolio_investors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, northern_export_industries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_domestic_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, smallholder_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_government_treasuries).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the policy benchmarks attached to each loan, negotiate letters of intent with finance ministries, and release money in tranches only as prior actions are completed. Their institutions charge interest and fees on outstanding credit, and their staffing, budgets, and standing in the credit system depend on a steady pipeline of programs. Careers continue afterward in private banks, sovereign advisory firms, and fund management.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_wb_program_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold syndicated and bilateral loans to developing governments. When repayment stalled in the 1980s and 1990s, rescue packages routed public money through debtor treasuries to keep interest flowing to them; write-downs came late and partial (Brady exchanges, HIPC). They can shift exposure between regions and instruments, and their claims are the first serviced under any program's payment hierarchy.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    powerful, biographical, mobile, global).

% Buy sovereign bonds and privatization offerings priced off program credibility: an approved program narrows spreads, and distressed state enterprises arrive at discounts. Positions can be exited in seconds; the horizon of a trade is shorter than any parliament's term.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_portfolio_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Gain tariff cuts, removed import controls, relaxed profit-repatriation rules, and privatized ports, utilities, and mines opened to foreign acquisition. Cash crops and minerals flow toward their markets at prices set after subsidy removal depresses southern purchasing power. They operate from jurisdictions that help set the terms and can relocate sourcing.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, northern_export_industries, beneficiary,
    organized, biographical, mobile, global).

% Steer quota shares and policy at the fund and bank, use programs as diplomatic leverage over debtor alignments, and implicitly guarantee their own banks' emerging-market exposure. They carry the tail risk when rescues fail and periodically absorb bilateral write-offs for strategically aligned debtors.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_government_treasuries, beneficiary,
    institutional, generational, arbitrage, global).

% Sign the letters of intent and implement the benchmarks: cutting wage bills, raising utility tariffs, selling state firms. They surrender discretion over budgets they were elected to steer and absorb the street-level backlash. At the same time, officeholders gain access to privatization deal flows, advisory fees, and the external validation that sustains them in office; leaving the system means losing the market access that finances the state.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_governments, beneficiary).

% Live under the spending rules the programs impose: school and clinic fees where services were free, withdrawn food and fuel subsidies, devalued currencies that raise import prices while wages stay frozen. The rules reach water, transport, and medicine; getting out from under them would mean emigration, which is open mainly to the credentialed few.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_domestic_populations, payer,
    powerless, generational, trapped, national).

% Face retrenchment targets, wage-bill ceilings, and frozen nominal salaries as standard program content. Unions strike and demonstrate — and do — but stoppages meet emergency-cost arguments and, in several episodes, security responses; employment outside the shrinking state means informal work.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    organized, biographical, constrained, national).

% Lost fertilizer and seed subsidies under agricultural benchmark reforms and were steered from food crops toward export crops whose world prices collapsed in the late 1980s. Land, credit, and buyers are concentrated; stepping out of the export chain means losing the only credit access available to them.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, smallholder_farmers, payer,
    powerless, generational, trapped, regional).

% Organize jubilee campaigns, parliamentary petitions, and parallel summits demanding debt audits and unconditional finance. They stand outside the negotiating room: term sheets are drafted between missions and finance ministries before publics see them, and movement proposals enter the process, if at all, as post hoc consultation inputs.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_debt_movements, excluded,
    organized, generational, constrained, global).

% Compile the outcome record — growth shortfalls, repeated re-programming, deteriorating social indicators — and trace the term-sheet mechanics: who signs, what is pledged, which tranches move when. They publish outside the program authorities' review chain and advise movements and some debtor ministries.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, heterodox_development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of dispersed official and private creditors who otherwise face incentives to defect — each preferring that some other lender grant forbearance while it collects. The program system creates a single negotiating table, a seniority hierarchy (preferred-creditor status), and shared policy benchmarks, so rescheduling, new money, and market re-entry occur as bloc decisions rather than through a competitive market for leniency. It also synchronizes bilateral donors around program markers.
% TRANSFER_FUNCTION: Moves debt-service payments, privatized public assets, export receipts, and policy discretion from debtor-state treasuries and domestic populations to external creditor banks, bondholders, acquiring multinationals, and the program authorities' institutional budgets; moves labor income and consumption (via wage caps, subsidy removal, devaluation) from domestic households toward the tradable-export sector and the servicing of external claims.
% ABSENT_VOICES: Debtor-country parliaments (programs are negotiated and signed by executives, frequently without legislative ratification), the domestic populations who absorb user fees and subsidy removal (consultation arrives post hoc through template exercises), representatives of the care economy (service cuts land disproportionately on unpaid household labor, mostly women's), and future generations. Civil-society debt movements organize outside the room; their proposals — debt audits, unconditional grants — never reach the term-sheet stage.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, creditor balance sheets would mark down sovereign claims immediately; frontier-market spreads would reprice violently; debtor treasuries would regain discretionary fiscal space and several would restructure on their own timelines; the program authorities' budgets, staffing, and standing in the credit system would shrink; and the export-access and investment regimes the benchmarks opened would be renegotiated bilaterally. Arrangements across the credit system depend on it — the world rearranges.
% FOUNDING_PROBLEM: The 1982 sovereign-debt overhang: petrodollar recycling had left Latin American and African states owing commercial banks sums exceeding bank capital, threatening simultaneous default and banking collapse. The arrangement was built to keep those debts serviced — rolling over principal and extracting domestic adjustment — until private confidence returned.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: UNCTAD Trade and Development Reports document that the original overhang was resolved by the mid-1990s while programming continued undiminished; the fund's own Independent Evaluation Office found prolonged engagement in low-income countries without restored market access; the Meltzer Commission's congressional testimony reached the same structural conclusion. No corroborating source outside the arrangement's operating coalition attests that the 1982 overhang remains the operative problem the arrangement solves.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.86, the 2024 endpoint of the shared-grid series) because the referent — the standing conditional-lending arrangement assessed by this reading's lights — exhibits service-first payment hierarchies, privatization outflows, and adjustment compressed onto domestic consumption across four decades of net-negative transfer episodes. Suppression (0.78) is authored as a raw structural property, unscaled by power or scope: enforcement is disbursement gating, cross-conditionality, and market-access punishment, not participant preference. Theater ratio (0.58) reflects a growing share of activity devoted to legitimation performance — participatory poverty-reduction templates, social-impact annexes, results frameworks — wrapped around an unchanged payment hierarchy whose enforcement core remains functional. Accessibility_collapse (0.58) is moderate-high: alternatives existed but were priced and punished — default carried years of exclusion (Argentina post-2001), alternative lenders arrived late and thin, and donor coordination closed grant channels to non-programmers. Resistance (0.62) is recurrent and real: unrest across program cities in the 1980s–90s, electoral turnover of implementing parties, jubilee mobilization, outright defaults, and eventually non-Paris-Creditor lending. All three tracked series run on one shared ten-point grid (1980–2024) so no metric is sampled against another's end-state. The dynamics are wavelike, not monotonic: enforcement intensity tracks debtor crises (1982, 1994–98, 2010–15, 2020–23) and relaxes when debtor-side leverage rises (HIPC/MDRI relief, the 2000s commodity boom, Chinese lending). The oscillation is functional to the transfer, not noise — the harshest benchmark sets attach at moments of maximum desperation, when the signing seat's bargaining power is lowest — so crisis-timed enforcement is itself part of the mechanism this reading measures. Receipt surface: gain_flow names transnational_creditor_banks because the recurring, demonstrable receipt of the arrangement's stream is protected debt service; fixing_cost is prohibitive because the seats able to fix the arrangement (the fund's board, the treasuries steering it) would be writing down claims they themselves hold or implicitly guarantee, and collapsing the institutional funding model their own budgets and doctrine rest on — the benefit of fixing accrues almost entirely to seats outside that coalition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute different types from identical term sheets. From the trapped domestic seats the arrangement arrives as gates, fees, and vanished services with no exit that does not mean emigration; from the creditor seats the same structure is predictable servicing, a single negotiating table, and rules they helped write — a coordination good; from the program-authority seat it is professional mandate fulfilled. The engine computes this divergence from the structural data (power, exit, declared position); the authored snare claim stays independent of it. Coalition check: the victim seats are numerous but their coalition formation is blocked — the seat that negotiates (the treasury) is not the seat that pays (households), repression costs fall on strike-prone workers, and information about term sheets reaches publics late. Cross-border coalition power is real but bounded: the jubilee movement won partial, conditional relief (HIPC) without dismantling the arrangement, demonstrating latent coalition capacity insufficient to dissolve the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (creditor banks, portfolio investors, export industries) derive low directionalities — the arrangement subsidizes them; declared victims (domestic populations, public workers, smallholder farmers) derive high directionalities — it extracts from them. Exit atoms modulate within those poles: trapped populations, farmers, and constrained workers sit near the full-target end; arbitrage-capable portfolio investors sit nearest the beneficiary end; mobile creditor banks sit close behind them. Debtor governments are genuinely dual-positioned (payer with a secondary beneficiary position: office retention, deal-flow access, external validation), placing them intermediate rather than at either pole. No directionality_overrides are authored: overrides key on power atoms, and within this story's atoms the seats diverge in relationship (the two institutional seats sit on opposite sides of the ledger), so an atom-level override would misassign one of them; the derivation from declared roles plus exit atoms captures the structure, and the government-seat nuance is carried by its secondary_role and the elite_capture omega. Scope note: the arrangement's global spatial scope modestly amplifies effective extraction on target seats through verification difficulty, per the engine's scope scaling — authored here as the scope atom only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the 1982 overhang serviced so bank capital and sovereign refinancing survived — was resolved by the Brady exchanges and HIPC/MDRI; the arrangement persisted and recruited new borrower cohorts (frontier-market issuers, pandemic-era emergency financing). Mandatrophy is therefore resolved: the mandate outlived its origin, and the R5 interview records founding_problem_status dead against disappearance_verdict world_rearranges — the mismatch flag this reading expects, since a dead founding problem plus a rearranging world is the signature of an arrangement maintained for its current recipients rather than its stated purpose. The snare claim guards against the inverse error: reading the arrangement as pure coordination because coordination surfaces (a single table, seniority rules, synchronized donors) are visible on it. The primary_function_attribution omega keeps the snare claim falsifiable rather than dogmatic: if separability evidence accumulates — coordination persisting wherever the riders are dropped — this reading must downgrade epsilon toward the coordination floor and concede the tangled_rope territory the hybrid sibling occupies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel structural_adjustment_conditionalities — the debtor_extraction_reading. Which structural facts would change if a sibling reading were adopted instead?',
    'Not resolvable by data alone; resolution is a framing choice by the corpus consumer. The signal to watch is which seat the analyst takes as the unit of account: creditor balance sheets (creditor_coordination_reading), the population of debtor states sorted by geopolitical weight (hybrid_selectivity_reading), or domestic populations living under program terms (this reading).',
    'Adopting creditor_coordination_reading drops epsilon toward the coordination-cost floor and reshapes the computed type toward rope; adopting hybrid_selectivity_reading partitions the victim set by geopolitical alignment and yields a mixed-type corpus. This file''s epsilon (0.86), victim set, and snare claim are valid only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the conditionalities kernel; disagreement located in primary-function attribution and in which seat counts as the paying seat.').

omega_variable(
    primary_function_attribution,
    'Is the observed pattern better explained as transfer wearing a coordination cover (this reading) or as coordination producing incidental severe harm (the sibling''s claim)?',
    'Search for coordination achievable without the transfer: episodes where equivalent creditor coordination operated without policy riders, or where riders were dropped at no coordination cost (speed-of-disbursement windows in acute crises, surcharge waivers, benchmark deletions under review). If coordination persists when the riders vanish, the riders are separable and this reading''s epsilon overstates.',
    'Demonstrated separability would force epsilon down toward the enforcement-mechanism floor and push the computed type toward tangled_rope; demonstrated inseparability consolidates the snare computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_function_attribution, empirical, 'Whether the transfer component is structurally separable from the coordination component.').

omega_variable(
    counterfactual_default_baseline,
    'What is the honest counterfactual for program countries: would autonomous default and unconstrained policy have left populations better or worse off than the program path?',
    'Matched comparison of program participants against abstainers and defaulters conditioned on pre-crisis fundamentals (Argentina''s post-2001 recovery, states that delayed program entry), plus within-country event studies around program entry, interruption, and exit.',
    'Sets the baseline for net-harm attribution: if abstainers fared systematically worse, part of the measured burden is the price of retained market access and epsilon falls; if abstainers fared equally or better, the full value stands against the arrangement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_default_baseline, empirical, 'Counterfactual trajectory against which program-path harm is measured.').

omega_variable(
    elite_capture_within_debtors,
    'How much of the stream taken out of debtor states lands on domestic ruling coalitions rather than passing through to external creditors — and does that change who the paying seat is?',
    'Trace privatization proceeds, deal-fee flows, and offshore asset accumulation of program-era officeholders through leaked ledgers, procurement audits, and beneficial-ownership registries.',
    'High domestic capture splits the victim set (populations versus rival elites) and lowers the effective position of debtor_governments below the payer-derived value; low capture consolidates the external-creditor receipt story and the named gain_flow seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_within_debtors, empirical, 'Domestic elite share of the extracted stream; refines the dual-positioned government seat.').

omega_variable(
    suppression_internalization_via_technocracy,
    'Is the enforcement that holds the arrangement together purely structural (disbursement gates, market-access punishment) or partly internalized — debtor technocracies formed inside the prescription set who apply austerity to their own societies without external pressure?',
    'Post-relief and post-exit policy trajectories: do governments that exit programs, or receive unconditional grants, maintain the same fiscal stances voluntarily? Compare finance-ministry training-network pipelines against policy persistence after enforcement lapses.',
    'An internalized component means enforcement outlives enforcement capacity: removing the gates would not immediately restore policy autonomy, and the structural suppression measure understates the arrangement''s binding force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_via_technocracy, empirical, 'Structural versus internalized share of the arrangement''s enforcement mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(stru_tr_t1985, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(stru_tr_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(stru_tr_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(stru_tr_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.56).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(stru_be_t1985, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1990, 0.76).
narrative_ontology:measurement(stru_be_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1995, 0.82).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(stru_be_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(stru_be_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2024, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(stru_su_t1985, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(stru_su_t1995, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(stru_su_t2005, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(stru_su_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel structural_adjustment_conditionalities per the epsilon-invariance principle: the colloquial label 'IMF/World Bank conditionality' covers three structurally distinct claims with different epsilon values, beneficiary structures, and failure modes. This file authors the debtor_extraction_reading (epsilon 0.86, victims = domestic populations, claimed snare). The sibling files author creditor_coordination_reading (low epsilon, coordination function primary, claimed rope/tangled_rope) and hybrid_selectivity_reading (mid epsilon, victim set partitioned by geopolitical alignment, claimed tangled_rope). Upstream/downstream: the coordination reading is the establishment claim whose evidentiary base (program approvals, restored-access narratives) the extraction reading cites and contests; edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
