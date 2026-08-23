% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities — Creditor Coordination Reading
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This file instantiates the creditor_coordination_reading of the
 *   structural_adjustment_conditionalities kernel: the arrangement under
 *   which multilateral crisis lending is disbursed against negotiated policy
 *   commitments — fiscal targets, subsidy and tariff reform, state-enterprise
 *   restructuring, monetary restraint — read here as a coordination device
 *   that aligns dispersed creditors on a single adjustment standard, restores
 *   market confidence through credible commitment, and protects the revolving
 *   credit pool all member states share. On this reading the arrangement's
 *   costs are real but bounded: compliance burden, transition hardship
 *   concentrated in previously protected sectors, and the ceding of discrete
 *   policy levers for the program's duration. KEY AGENTS (by structural
 *   relationship): - imf_executive_board: Agenda setter
 *   (institutional/arbitrage) — administers programs, controls tranche
 *   release - program_country_finance_ministries: Dual-positioned signatory
 *   (powerful/constrained) — receives liquidity and credibility, bears
 *   implementation politics - future_taxpayers_of_program_countries:
 *   Intergenerational beneficiary (powerless/trapped) — inherits the adjusted
 *   fiscal trajectory - international_bondholders: Creditor beneficiary
 *   (organized/arbitrage) — repayment certainty priced into spreads -
 *   multilateral_creditor_institutions: Institutional beneficiary
 *   (institutional/arbitrage) — pool protection and senior recovery -
 *   state_enterprise_workers: Primary payer (organized/constrained) —
 *   restructuring and wage-bill consolidation - urban_subsidy_consumers:
 *   Primary payer (powerless/trapped) — subsidy-withdrawal incidence -
 *   protected_domestic_industries: Secondary payer (moderate/constrained) —
 *   liberalization exposure - program_country_legislatures: Excluded voice
 *   (moderate/trapped) — ratification duty without a negotiation seat -
 *   debt_relief_advocacy_networks: Excluded challenger (organized/mobile) —
 *   outside formal governance - independent_program_evaluators: Analytical
 *   observer (institutional/analytical) — outcome evidence. This story is one
 *   member of a three-reading constraint family; the sibling files
 *   instantiate the same kernel differently and are linked via
 *   network.affects_constraints — see the dual formulation note and the
 *   kernel-reading omega. Per the epsilon-invariance rule this file authors
 *   only this reading's structure: one stable epsilon over the standing
 *   arrangement, assessed by this reading's own lights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.38).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities — Creditor Coordination Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '5c5448e5-7c01-47e8-8cf6-f9765b4de634').
narrative_ontology:cs_kernel_codification('5c5448e5-7c01-47e8-8cf6-f9765b4de634', formalized).
narrative_ontology:cs_authority_grounding('5c5448e5-7c01-47e8-8cf6-f9765b4de634', expertise).
narrative_ontology:cs_interpretation_layer_present('5c5448e5-7c01-47e8-8cf6-f9765b4de634').
narrative_ontology:cs_reading_relation('5c5448e5-7c01-47e8-8cf6-f9765b4de634', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c5448e5-7c01-47e8-8cf6-f9765b4de634', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('5c5448e5-7c01-47e8-8cf6-f9765b4de634', foundational, conditionality_restores_market_confidence).
narrative_ontology:cs_axiom_status(conditionality_restores_market_confidence, holdable).
narrative_ontology:cs_axiom_grounding('5c5448e5-7c01-47e8-8cf6-f9765b4de634', conditionality_restores_market_confidence, instrumental).
narrative_ontology:cs_axiom('5c5448e5-7c01-47e8-8cf6-f9765b4de634', secondary, creditor_pool_requires_common_enforcement_standard).
narrative_ontology:cs_axiom_status(creditor_pool_requires_common_enforcement_standard, holdable).
narrative_ontology:cs_axiom_grounding('5c5448e5-7c01-47e8-8cf6-f9765b4de634', creditor_pool_requires_common_enforcement_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('5c5448e5-7c01-47e8-8cf6-f9765b4de634', bretton_woods_sound_finance_mandate).
narrative_ontology:cs_drift_state('5c5448e5-7c01-47e8-8cf6-f9765b4de634', contemporary_evaluation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5c5448e5-7c01-47e8-8cf6-f9765b4de634', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_program_countries).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, multilateral_creditor_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, state_enterprise_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, urban_subsidy_consumers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, protected_domestic_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_finance_ministries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_finance_ministries).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, credible_commitment_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, washington_consensus_reform_agenda).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approves lending arrangements, sets performance criteria and structural benchmarks, decides waivers and tranche releases, and periodically rewrites the conditionality guidelines its staff apply. Weighted voting gives advanced economies decisive shares. Collects charges on outstanding credit and recycles them to reserves; its exposure is managed at the portfolio level, so it can adjust terms and lending volumes across cycles rather than exiting any single country position.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_executive_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Negotiates and signs the program, implements fiscal targets and structural measures domestically, and issues the letters of intent that anchor reviews. Receives liquidity and an external credibility endorsement that lowers borrowing spreads, while absorbing the political cost of subsidy withdrawal, wage restraint, and privatization. Walking out mid-program forfeits remaining disbursements and typically triggers a spread spike, so the exit door narrows sharply once the signature is on.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_finance_ministries, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_finance_ministries, payer).

% Inherit the debt trajectory that today's adjustment shapes. If stabilization holds, they service a smaller stock and face wider policy space; if it fails, they inherit the arrears anyway. They have no vote in current negotiations, no representative at the table, and no way to decline obligations contracted on their behalf.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_program_countries, beneficiary,
    powerless, generational, trapped, national).

% Hold sovereign paper issued by emerging and frontier markets. A monitored program reduces perceived default risk and compresses spreads on the country's curve; policy commitments protect the coupon stream they price. They can sell positions, hedge, or reprice continuously, and they condition new lending on program presence.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders, beneficiary,
    organized, biographical, arbitrage, global).

% Recycle member contributions as loans carrying service charges. Conditionality protects the revolving pool from serial arrears and preserves preferred-creditor status in practice, letting them keep lending into distressed countries that private markets have closed. They co-finance with the Fund and calibrate exposure across regions and decades.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, multilateral_creditor_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Face restructuring, privatization, or closure of subsidized state enterprises and public-sector wage-bill consolidation. Jobs and protected status disappear on a schedule set in program reviews. Unions can strike and protest, and sometimes win sequencing concessions, but cannot replace the budget lines the fiscal targets remove; re-employment depends on the very sectors the same reforms reshape.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, state_enterprise_workers, payer,
    organized, biographical, constrained, national).

% Bear fuel, food, and transport price increases when subsidies are withdrawn under fiscal targets, spending large budget shares on the affected goods. There is no alternative supplier of basics at pre-adjustment prices. Street protest — the austerity-riot pattern — is their principal lever, and it occasionally forces compensation measures or timing delays.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, urban_subsidy_consumers, payer,
    powerless, immediate, trapped, national).

% Lose tariff walls, import licensing, and directed-credit channels under liberalization conditions and must compete against imports without prior barriers. Some firms upgrade into export markets; others contract or fold. Lobbying for renewed protection runs directly against the program commitments their own government signed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, protected_domestic_industries, payer,
    moderate, biographical, constrained, national).

% Constitutionally responsible for budgets and ratification, yet typically learn the contents of program letters of intent after executive-to-executive negotiation concludes. They can slow implementing legislation and force debate, but delaying past review dates puts disbursements at risk, and they hold no seat where the terms are written.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_legislatures, excluded,
    moderate, biographical, trapped, national).

% Campaign for debt cancellation and against policy conditions from outside formal program governance. They won consultative channels in poverty-strategy processes after 1999 and can move public opinion and donor-country politics, but they hold no vote on program terms and their proposals enter, if at all, as annexes to documents already shaped.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debt_relief_advocacy_networks, excluded,
    organized, generational, mobile, global).

% Run ex-post assessments of program outcomes — growth during programs, return to voluntary market access, social indicators, design quality — and publish findings the Board commissions. They enforce nothing and collect nothing; their evidence base is the main outside check on the claims each side of the program debate makes about what the mechanism delivers.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, independent_program_evaluators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of official crisis lending: dispersed creditors cannot individually commit to disciplined treatment of an over-indebted sovereign, markets cannot distinguish committed adjustment from improvised muddling-through, and the pooled credit facility needs protection from serial arrears. Conditionality supplies one negotiated, monitored standard that all parties can verify against.
% TRANSFER_FUNCTION: Moves policy decisions (fiscal targets, subsidy and tariff schedules, state-enterprise disposition, monetary stance) from program-country polities to creditor-negotiated adjustment paths for the program's duration; moves liquidity and an external credibility endorsement from the creditor pool to the signing government; moves debt service and charge payments from the program country to creditor seats.
% ABSENT_VOICES: Program-country legislatures hold ratification responsibility but negotiate nothing; wage earners and subsidy consumers bear the incidence without a seat; future taxpayers — the reading's central beneficiaries — have no possible seat at all. Civil-society challengers gained only consultative channels late, in poverty-strategy processes. Unanimity in program documents reflects who was in the room: executives, Fund staff, and creditor representatives.
% DISAPPEARANCE_RATIONALE: Creditor coordination would re-emerge in cruder forms — bilateral conditionality, covenants, collateral demands, faster market shutout — but the existing architecture of jointly monitored, catalytically financed adjustment would dissolve overnight: pending reviews would lapse, disbursements would stop, spreads would reprice, and restructuring negotiations would lose their anchor.
% FOUNDING_PROBLEM: Recurrent sovereign debt crises in which countries over-borrowed, creditors overlent, adjustment was delayed until it became disorderly, and each creditor sought preferential treatment — the free-rider problem the Bretton Woods designers answered with a supervised, temporary, condition-attached credit facility.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by: the IMF's own Independent Evaluation Office program assessments; the documented design debates behind the Articles of Agreement in scholarly histories of Bretton Woods; recurring formal requests for arrangements by crisis governments acting against their short-term electoral interest; and market-pricing studies attributing spread movements to program presence. No attestation comes from the payer seats, which consistently describe the founding problem as imposed-upon rather than served — that dissent is itself signal and is routed to the sibling readings.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored from this reading's own descriptive account of the standing arrangement, independently of the rope claim. Extractiveness 0.25: the reading concedes genuine costs — compliance overhead, transition pain, ceded policy levers — but holds them dominated by the coordination dividend; the series shows early blunt-conditionality designs (0.32-0.34 in the 1980s) improving as streamlining and poverty-focus reforms land (0.25-0.28 after 2000). Suppression 0.38: enforcement is contractual (tranche withholding, benchmark waivers) rather than coercive closure; alternatives — default, bilateral lenders, unilateral adjustment — remain accessible at high friction, and the suppression_requirement series traces the 1980s-90s enforcement ratchet (peak 0.48 in 1995) releasing after the 2002 streamlining guidelines before a mild austerity-era uptick. Theater ratio 0.25 and drifting up slowly: review missions and structural benchmarks accumulate pro-forma content as programs multiply, but core appraisal and disbursement functions remain operative. Accessibility collapse 0.30: understanding the mechanism does not eliminate alternatives, it prices them. Resistance 0.45: austerity protests, non-completion, and renegotiation are recurrent and real, which the reading attributes to transition-cost incidence rather than rejection of coordination as such. All three tracked series share one ten-point grid (1980-2025, five-year steps) so no metric row is sampled against another's gaps. Receipt surface: gains divide across creditor seats (coupons, charges, senior recovery) and the intergenerational stability dividend — no single named seat captures them, hence gain_flow 'diffuse' as an affirmative checked claim made after reviewing every seat; for the agenda-setting Board, dismantling conditionality would forfeit the coordination function and reopen moral hazard, so fixing_cost is 'prohibitive'. The claim (rope) and these metrics are independent authored facts; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat should compute differently. From the Board and creditor seats the arrangement is a prudential standard they fund and administer; from the worker and subsidy-consumer seats it arrives as removed subsidies and restructured employers decided elsewhere; from the excluded legislature seat it arrives as a letter of intent ratified after the fact. The finance ministry seat straddles: it signs, receives, implements, and absorbs the politics. The engine derives these divergent experiences from the declared roles, power levels, and exit options; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for future taxpayers (they receive the adjusted trajectory without bearing current costs), for bondholders (repayment protection), and for multilateral creditors (pool continuity and senior recovery). Victim declarations drive high directionality for enterprise workers, subsidy consumers, and protected industries, whose exit is constrained or trapped. The Board sits near the administrative middle: it collects charges but recycles them to the pool it stewards. One override is declared: the structural derivation would read program_country_finance_ministries as near-full beneficiaries from their beneficiary role, but their actual position mixes liquidity receipt with cost-bearing, ceded policy autonomy, and electoral exposure — d is overridden to 0.55 (just target-side of symmetric) for the powerful atom, which in this story only that seat occupies.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — disorderly sovereign adjustment and creditor free-riding — recurs across the interval (1980s Latin American reschedulings, 1990s Asian crisis programs, post-2010 euro-area arrangements, the 2020s debt-distress cluster), so the arrangement's mandate is live rather than outlived. The classification guards both error directions: reading the arrangement as pure extraction would erase the genuine collective-action solution this reading documents; reading it as costless coordination would erase the incidence evidence the payer seats supply. The theater series is the early-warning channel: if programs persist and thicken procedural content after the underlying crisis cycle abates, the mandatrophy question reopens with founding_problem_status dead against disappearance_verdict world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_creditor_coordination,
    'This constraint instantiates the creditor_coordination_reading of the structural_adjustment_conditionalities kernel. What would the sibling readings (debtor_extraction_reading, hybrid_selectivity_reading) change structurally, and where exactly is the disagreement located?',
    'Compare the three reading-files'' epsilon values, victim sets, and computed types against the same program-outcome record. The disagreement is located in three structural elements: (a) whether the coordination function is genuine or cover, (b) whether costs fall on inefficient sector rents or on ordinary households, (c) whether enforcement is uniform or geopolitically selective.',
    'If the extraction reading''s premises dominate, this reading''s victim set expands from sectoral payers to general populations and its type shifts from rope toward snare; if the hybrid reading dominates, uniformity of enforcement becomes the load-bearing variable and epsilon rises with measured waiver asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_creditor_coordination, conceptual, 'Committer structure: one of three readings of the structural-adjustment kernel; records what siblings would change and where the dispute sits.').

omega_variable(
    conditionality_efficacy_record,
    'Does conditionality actually deliver the fiscal sustainability and market re-access it is credited with, relative to feasible alternatives such as unconditional liquidity with ex-post restructuring or state-contingent instruments?',
    'Program-outcome meta-analysis: growth during programs, probability of return to voluntary market access within a fixed horizon, repeat-program rates, compared across conditionality intensities and against comparable non-program crisis cases.',
    'A sustained null result dissolves the instrumental foundation of this reading and shifts weight to the extraction sibling; robust positive effects stabilize the rope classification and the low epsilon authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_efficacy_record, empirical, 'Empirical basis of the reading''s means-ends claim.').

omega_variable(
    adjustment_incidence_attribution,
    'Do the costs borne by the listed payer groups represent rents of inefficient protected sectors, as this reading asserts, or ordinary household income with no efficiency rationale?',
    'Incidence studies of subsidy removal, tariff reduction, and wage-bill consolidation that distinguish rent recipients from subsistence households within each payer group.',
    'If incidence falls mainly on subsistence households, the victim set expands, epsilon rises, and the reading migrates toward the hybrid or extraction sibling despite unchanged formal design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_incidence_attribution, empirical, 'Whether the inefficient-sector attribution of costs survives incidence analysis.').

omega_variable(
    enforcement_uniformity_question,
    'Is enforcement applied uniformly across debtors, or do waiver frequency, program completion, and financing volume vary with the geopolitical alignment of the program country?',
    'Code structural-benchmark waivers, program completions, and financing-per-program against alliance and strategic-variable datasets across the interval.',
    'Measured selectivity breaks the uniform-standard premise this reading rests on and hands the hybrid_selectivity_reading its central evidence, raising effective extraction for weakly aligned debtors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_uniformity_question, empirical, 'Uniformity premise versus the selective-waiver record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(stru_tr_t1980, observed).
narrative_ontology:measurement(stru_tr_t1985, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1985, 0.17).
narrative_ontology:measurement_basis(stru_tr_t1985, observed).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.19).
narrative_ontology:measurement_basis(stru_tr_t1990, observed).
narrative_ontology:measurement(stru_tr_t1995, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement_basis(stru_tr_t1995, observed).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement_basis(stru_tr_t2000, observed).
narrative_ontology:measurement(stru_tr_t2005, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement_basis(stru_tr_t2005, observed).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement_basis(stru_tr_t2010, observed).
narrative_ontology:measurement(stru_tr_t2015, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement_basis(stru_tr_t2015, observed).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement_basis(stru_tr_t2020, observed).
narrative_ontology:measurement(stru_tr_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(stru_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement_basis(stru_be_t1980, observed).
narrative_ontology:measurement(stru_be_t1985, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1985, 0.34).
narrative_ontology:measurement_basis(stru_be_t1985, observed).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement_basis(stru_be_t1990, observed).
narrative_ontology:measurement(stru_be_t1995, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement_basis(stru_be_t1995, observed).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.27).
narrative_ontology:measurement_basis(stru_be_t2000, observed).
narrative_ontology:measurement(stru_be_t2005, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement_basis(stru_be_t2005, observed).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement_basis(stru_be_t2010, observed).
narrative_ontology:measurement(stru_be_t2015, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement_basis(stru_be_t2015, observed).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.27).
narrative_ontology:measurement_basis(stru_be_t2020, observed).
narrative_ontology:measurement(stru_be_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2025, 0.25).
narrative_ontology:measurement_basis(stru_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement_basis(stru_su_t1980, observed).
narrative_ontology:measurement(stru_su_t1985, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement_basis(stru_su_t1985, observed).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.44).
narrative_ontology:measurement_basis(stru_su_t1990, observed).
narrative_ontology:measurement(stru_su_t1995, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement_basis(stru_su_t1995, observed).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement_basis(stru_su_t2000, observed).
narrative_ontology:measurement(stru_su_t2005, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(stru_su_t2005, observed).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement_basis(stru_su_t2010, observed).
narrative_ontology:measurement(stru_su_t2015, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement_basis(stru_su_t2015, observed).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement_basis(stru_su_t2020, observed).
narrative_ontology:measurement(stru_su_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(stru_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% One colloquial label ('structural adjustment conditionalities') covers three structurally distinct claims with different epsilon values, victim sets, and failure modes; per the epsilon-invariance principle they are authored as a three-story family linked by affects_constraints. The creditor_coordination file is the institutional self-description (upstream: it supplies the official account the other two contest); the extraction reading is the maximal counter-claim; the hybrid reading mediates on enforcement-pattern evidence. Cross-file comparison against the same program-outcome record is the resolution path named in the kernel-reading omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__creditor_coordination_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
