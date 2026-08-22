% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionality Regime (Debtor-Extraction Reading)
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   The structural adjustment conditionality regime, as this reading
 *   instantiates it: when a debtor state loses market access, official rescue
 *   finance arrives only inside a program whose conditions — fiscal
 *   austerity, food and fuel subsidy removal, user fees for health and
 *   education, civil-service wage-bill ceilings and retrenchment,
 *   privatization of state enterprises, and trade and capital-account
 *   liberalization — are enforced by tranche-by-tranche review and by the
 *   gating of all other official and private finance on the program's good
 *   standing. This reading holds that the package transfers the costs of
 *   adjustment onto domestic populations while keeping external creditor
 *   claims current, and that its privatization and liberalization clauses
 *   transfer public assets and policy control to external capital. The
 *   epsilon authored here (0.84) is the standing conditional-lending
 *   arrangement as this reading assesses it — the arrangement under contest,
 *   not any reformed arrangement this reading would endorse. The claimed type
 *   and the metrics are independent authored facts: the snare claim and the
 *   descriptive metric values below are both authored from this reading's
 *   seat, and the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - transnational_creditor_banks: primary beneficiary (institutional/arbitrage) — receive the debt-service flow the program financing schedule protects
 *   - imf_world_bank_program_officers: agenda-setter (institutional/arbitrage) — design, negotiate, and enforce conditionality; bear none of its domestic costs
 *   - foreign_portfolio_investors: secondary beneficiary (powerful/arbitrage) — collect restructured yield under liberalized capital accounts
 *   - multinational_extractive_firms: secondary beneficiary (powerful/arbitrage) — acquire privatized assets at post-crisis valuations
 *   - northern_export_manufacturers: tertiary beneficiary (powerful/mobile) — sell into markets opened by liberalization conditionality
 *   - debtor_state_finance_ministries: domestic implementing agent (powerful/constrained) — sign and implement under credit-cutoff threat; dual position as administrator and bearer of the backlash
 *   - debtor_state_domestic_populations: primary target (organized/trapped) — bear service cuts, price shocks, and wage restraint; resist but cannot exit the jurisdiction
 *   - public_sector_workers: target (organized/trapped) — face retrenchment and wage ceilings; unions strike but the employer implements external terms
 *   - smallholder_farmers: target (powerless/trapped) — lose subsidies and price supports; no exit from land or price system
 *   - urban_poor_service_users: target (powerless/trapped) — absorb user fees and subsidy-removal price shocks with no asset buffer
 *   - domestic_legislatures: excluded (moderate/constrained) — constitutionally responsible for budgets but locked out of program negotiation
 *   - civil_society_watchdogs: analytical observer (analytical/analytical) — document benefit incidence and social outcomes from outside the negotiating room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.84).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.84).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionality Regime (Debtor-Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, 'a9ca5068-fa38-42d0-8b65-abd189a438f5').
narrative_ontology:cs_kernel_codification('a9ca5068-fa38-42d0-8b65-abd189a438f5', formalized).
narrative_ontology:cs_authority_grounding('a9ca5068-fa38-42d0-8b65-abd189a438f5', extraction).
narrative_ontology:cs_interpretation_layer_present('a9ca5068-fa38-42d0-8b65-abd189a438f5').
narrative_ontology:cs_reading_relation('a9ca5068-fa38-42d0-8b65-abd189a438f5', structural_adjustment_conditionalities__creditor_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('a9ca5068-fa38-42d0-8b65-abd189a438f5', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('a9ca5068-fa38-42d0-8b65-abd189a438f5', foundational, conditionalities_are_coercive_extraction).
narrative_ontology:cs_axiom_status(conditionalities_are_coercive_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a9ca5068-fa38-42d0-8b65-abd189a438f5', conditionalities_are_coercive_extraction, empirically_contingent).
narrative_ontology:cs_axiom('a9ca5068-fa38-42d0-8b65-abd189a438f5', foundational, imposed_fiscal_policy_violates_sovereignty).
narrative_ontology:cs_axiom_status(imposed_fiscal_policy_violates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a9ca5068-fa38-42d0-8b65-abd189a438f5', imposed_fiscal_policy_violates_sovereignty, deontological).
narrative_ontology:cs_reference_frame('a9ca5068-fa38-42d0-8b65-abd189a438f5', washington_consensus_conditionality_package).
narrative_ontology:cs_drift_state('a9ca5068-fa38-42d0-8b65-abd189a438f5', post_fiscal_multiplier_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a9ca5068-fa38-42d0-8b65-abd189a438f5', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_portfolio_investors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, multinational_extractive_firms).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, northern_export_manufacturers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_domestic_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, smallholder_farmers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_service_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministries).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, market_confidence_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, fiscal_consolidation_growth_thesis).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__debtor_extraction_reading, washington_consensus_policy_package).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold syndicated loan and sovereign bond exposure to crisis-state governments. When a debtor can no longer service its debt, official rescue finance arrives inside a program whose first call on proceeds is keeping these creditors current; arrears to them are treated as the event to be prevented. Once exposure is rolled into restructured or officially backstopped paper they can sell or rotate to new markets; their balance sheets are the flow the program's financing schedule protects.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Design program conditionality, negotiate letters of intent with finance ministries, and release tranches on quarterly review; their certification gates access to all other official and private finance. Institutional budgets, staff careers, and the mandate's relevance ride on program volume. They bear none of the domestic costs of the packages they draft and rotate out of country assignments on fixed tours.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_world_bank_program_officers, agenda_setter,
    institutional, generational, arbitrage, global).

% Buy distressed sovereign paper at post-crisis prices and collect restructured yield premia once a program restores service; capital-account liberalization written into the same programs lets them move funds in and out freely. They bear no domestic adjustment costs and can exit any single debtor's market at will.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, foreign_portfolio_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Acquire privatized utilities, mines, ports, and concessions divested under program conditionality, often at post-crisis valuations, and repatriate profits under the liberalized investment codes the same programs install. Their exposure to any one jurisdiction is a portfolio decision.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, multinational_extractive_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Gain access to Southern markets as program conditionality dismantles import-substitution protections, export taxes, and state trading monopolies; they sell into markets their own governments' lending discipline opened. Exit from any single market is a commercial decision.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, northern_export_manufacturers, beneficiary,
    powerful, biographical, mobile, continental).

% Sign the letters of intent and implement austerity, subsidy removal, and privatization domestically under the standing threat that deviation closes credit access and triggers arrears. They lose control over the content of fiscal policy, absorb the electoral and street-level backlash of implementation, and cannot exit the arrangement without losing the financing that keeps the state solvent.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministries, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministries, payer).

% Bear the adjustment directly: cuts to health and education budgets, removal of food and fuel subsidies, user fees, wage restraint, and the unemployment that follows retrenchment and privatization. They resist through riots, strikes, and electoral turnover — the IMF riots of the 1980s-90s, the Jubilee campaigns, the electoral left turns of the 2000s — but cannot leave the fiscal jurisdiction whose budget is being adjusted; emigration is partial, costly, and reaches only a fraction of the population.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_domestic_populations, payer,
    organized, generational, trapped, national).

% Face civil-service hiring freezes, retrenchment targets, and wage-bill ceilings written into program conditionality. Unions can strike and have done so, but their employer is the state implementing externally negotiated terms, and the alternative to compliance is the arrears and market closure that deepen the crisis.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers, payer,
    organized, biographical, trapped, national).

% Lose input subsidies, marketing-board price supports, and agricultural credit as conditionality dismantles state marketing institutions; many are pushed from food crops to export crops priced at volatile world prices. They have no exit from the land, the price system, or the jurisdiction.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, smallholder_farmers, payer,
    powerless, generational, trapped, national).

% Pay user fees for clinics and schools introduced under cost-recovery conditionality; when fees bind, households withdraw children from school or forgo care. Their consumption basket absorbs the full price shock of subsidy removal, and they have no asset buffer or mobility to escape it.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_service_users, payer,
    powerless, immediate, trapped, national).

% Hold constitutional authority over budgets and ratification but are locked out of the room where program terms are drafted; letters of intent pass between finance ministries and fund staff, and legislatures are presented with implemented austerity or face the arrears that follow refusal. Their objections surface only after terms are set.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_legislatures, excluded,
    moderate, biographical, constrained, national).

% Track program documents, benefit-incidence studies, and social outcomes; publish critiques from academic, NGO, and UN-affiliated seats; and supply the outside-the-negotiating-room record of who bore what. They hold no seat in program negotiation and no lever over tranche release.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_watchdogs, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem among dispersed creditors: without a unified framework each creditor has an incentive to hold out for full repayment while free-riding on others' concessions, and no single lender can enforce adjustment on the debtor. The conditionality framework coordinates creditor claims into one negotiating position, converts sovereign fiscal policy into a monitorable package with quarterly review points, and provides a common signal — the program's good standing — on which all other lenders and investors key their decisions.
% TRANSFER_FUNCTION: Moves debt-service payments, privatization proceeds, and export earnings from debtor-state treasuries and their populations to external creditors and asset purchasers, and moves effective control over fiscal, trade, and ownership policy from domestic institutions to the creditor institutions that certify the program.
% ABSENT_VOICES: The populations bearing the adjustment — patients, students, retrenched workers, subsidized farmers — were never in the negotiating room; nor were domestic legislatures, which hold constitutional budget authority but received programs negotiated executive-to-executive between finance ministries and fund staff. Labor unions and professional associations typically learned the program's content when implementation began. Their absence is structural: participation was limited to the two seats whose signatures closed the financing gap.
% DISAPPEARANCE_RATIONALE: If the conditionality regime vanished overnight, creditor coordination would collapse into holdout litigation and competitive disorder; debtor states would restructure on their own timelines as Argentina did after 2001; capital flows to the South would reprice around country risk rather than program certification; and the program-review infrastructure, its staffing, and the gating of other official finance on the fund's seal would all dissolve.
% FOUNDING_PROBLEM: The 1982 sovereign debt crisis: commercial banks had recycled petrodollars into sovereign lending at volumes that threatened bank solvency if major debtors defaulted, and no sovereign bankruptcy mechanism existed to coordinate an orderly write-down.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the IMF's own Independent Evaluation Office has documented program design failures across eras; UNCTAD's Trade and Development Reports and the academic debt-relief literature record the Brady Plan securitization (1989) and HIPC/MDRI write-downs (1996-2005) as resolving the founding solvency emergency; and the fund's own 1999 rebranding to poverty-reduction-and-growth programming tacitly conceded the original stabilization framework's social failure. No source outside the creditor institutions maintains that the 1982 bank-solvency emergency remains the regime's operative problem.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.84) because this reading assesses the standing arrangement as routing the adjustment burden to domestic populations while official rescue finance keeps external claims current: the financing schedule's first call is creditor service, and the social-spending cuts that fall on populations are program conditions rather than incidental outcomes. Suppression (0.84) is a raw, unscaled structural property reflecting the enforcement architecture — deviation closes market access and official flows, the program's certification gates all other lending, and arrears spiral punishes exit — while extractiveness is the value the engine scales by directionality and scope. Accessibility_collapse sits at 0.60 rather than mountain-grade because the alternative set is coercively narrowed but not eliminated: default and alternative creditors exist, as Argentina after 2001 and the 2000s diversification of Southern credit demonstrated. Theater (0.55) reflects a genuine creditor-coordination core wrapped in a growing performative layer — the poverty-reduction, ownership, and resilience language accreted since 1999 as the original stabilization rationale aged. Resistance (0.65) is historically documented — the IMF riots of the 1980s-90s, Jubilee 2000, the electoral turnover and defaults of the 2000s — and partly registers coalition formation among nominally powerless seats: smallholder and urban-poor seats act through riots, unions, and ballots even though individually each lacks exit, which raises resistance without much lowering their effective burden. The measurement series run on one shared grid (1982, 1989, 1996, 2002, 2008, 2015, 2020, 2026) with all three metrics authored at every point. The series rise with one interruption: extraction and suppression dip after the 2001 Argentina default and the HIPC write-downs (2002-2008), then re-accumulate as the 2010s-2020s restructuring wave reimposed austerity; theater rises monotonically across the whole interval — the coordination story grows more performative as its original rationale ages, the Goodhart signature. The 2026 points are projected: the current austerity wave was ongoing at generation time.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently from the same structural data. From the creditor and program-officer seats the arrangement is prudent risk management: contracts honored, moral hazard contained, dispersed claims coordinated. From the trapped population seats the same structure is externally authored austerity: terms drafted in Washington, signed by finance ministers under credit-cutoff threat, implemented on people who were never in the room. The finance-ministry seat sits between — administering terms it did not set while absorbing the backlash of implementing them. The engine computes this divergence per seat from power, exit options, and declared position; this commentary does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The four declared beneficiary seats (creditor banks, portfolio investors, extractive firms, northern exporters) derive d near the beneficiary end: each collects from the arrangement, and each holds arbitrage-grade or mobile exit, which damps effective burden toward subsidy. The four declared victim seats (domestic populations, public-sector workers, smallholder farmers, urban poor) derive d near the target end, and their trapped exit amplifies effective extraction — they cannot leave the fiscal jurisdiction being adjusted. The finance ministries are dual-positioned administrators who also bear political cost, sitting mid-to-high. The program officers' seat sits nearer the beneficiary end than symmetric — institutional budgets, careers, and mandate relevance ride on program volume, and they bear none of the domestic costs — but the schema's directionality-override surface is keyed to power atom rather than agent: an override at the institutional atom would also move the creditor banks, whose derived d is already correct. No overrides are authored; the officers' position is left to derivation from their undeclared status and documented here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 1982 bank-solvency emergency — was resolved by the Brady securitization (1989) and the HIPC/MDRI write-downs (1996-2005), yet the arrangement not only persisted but expanded from balance-of-payments stabilization into comprehensive structural transformation, rebranding as poverty-reduction-and-growth programming in 1999 rather than sunsetting. The R5 interview records founding_problem_status dead against disappearance_verdict world_rearranges — the mismatch that flags a mandate outliving its function and persisting by institutional momentum and rebranded justification. The snare claim guards against the mirror error as well: reading the regime's genuine creditor-coordination surface as its function would misclassify a coercive architecture as a coordination mechanism, while reading that surface as absent would miss the reason dispersed creditors defer to the framework at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading — the debtor_extraction_reading — of the kernel structural_adjustment_conditionalities; how would the sibling readings restructure the classification over the same referent?',
    'Generate the sibling stories (creditor_coordination_reading, hybrid_selectivity_reading) over the same standing arrangement and compare computed types. The disagreement is located in the regime''s operative function (creditor-coordination service versus extraction instrument) and in whether crisis-signed letters of intent carry consent.',
    'The creditor_coordination_reading would author low epsilon over the same referent and likely compute a coordination type; the hybrid_selectivity_reading would key enforcement intensity to geopolitical leverage and likely compute a hybrid type. This file''s snare verdict is a property of this reading, not of the colloquial label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the structural adjustment conditionalities kernel.').

omega_variable(
    consent_validity_ambiguity,
    'Does the signed letter of intent constitute voluntary consent to the program''s terms, or acquiescence under duress — crisis conditions, closed markets, and the standing threat of credit cutoff?',
    'Counterfactual reconstruction of the option set facing debtor negotiators at signature: available financing alternatives, market access, and the documented consequences of refusal across program archives.',
    'If consent is substantive, suppression falls sharply and the arrangement moves toward a coordination-hybrid classification; if duress, the snare reading stands and the sovereignty axiom holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_validity_ambiguity, conceptual, 'Whether crisis-context signatures carry contract-valid consent — the location of the sibling disagreement on consent.').

omega_variable(
    benefit_incidence_concentration,
    'Are the regime''s gains concentrated on the named creditor seats (bank balance sheets, bondholders, privatization acquirers) or diffuse across market participants as a whole?',
    'Benefit-incidence accounting of debt-service flows, rescue-lending on-lending, and privatization transfer prices across program episodes.',
    'Concentrated gains confirm the named receipt seat and the extraction reading; diffuse gains would flip the receipt verdict and open inertial-decay analysis of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_incidence_concentration, empirical, 'Concentration versus diffusion of the regime''s captured gains.').

omega_variable(
    counterfactual_default_trajectory,
    'Would debtor states outside the regime have fared worse — validating the coordination function — or comparably, undermining it?',
    'Comparative outcome analysis of defaulters (for example Argentina after 2001) against continuous program countries, controlling for initial conditions and commodity cycles.',
    'Comparable defaulter outcomes collapse the fiscal-sustainability justification and harden the snare reading; worse defaulter outcomes would credit part of the arrangement as genuine coordination and soften epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_default_trajectory, empirical, 'Whether the coordination justification survives the defaulter counterfactual.').

omega_variable(
    enforcement_uniformity,
    'Is the regime''s coercive force applied uniformly across debtor states, or modulated by geopolitical alignment — waived or softened for strategically useful debtors?',
    'Cross-country comparison of enforcement intensity (program interruptions, tranche delays, waiver frequency) against alignment indicators across program episodes.',
    'Systematic modulation would support the selectivity-weighted sibling''s structure and push this reading toward a geopolitically keyed variant; uniform enforcement would confirm this reading''s uniform-extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_uniformity, empirical, 'Whether enforcement tracks debtor need or debtor alignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sad_debtor_extraction_tr_t1982, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t1982, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t1989, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1989, 0.3).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t1989, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t1996, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 1996, 0.38).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t1996, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t2002, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2002, 0.42).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t2002, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t2008, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2008, 0.45).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t2008, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t2015, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2020, 0.52).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t2020, observed).
narrative_ontology:measurement(sad_debtor_extraction_tr_t2026, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 2026, 0.55).
narrative_ontology:measurement_basis(sad_debtor_extraction_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(sad_debtor_extraction_be_t1982, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1982, 0.72).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t1982, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t1989, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1989, 0.76).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t1989, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t1996, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 1996, 0.81).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t1996, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t2002, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2002, 0.77).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t2002, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t2008, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t2008, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t2015, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t2020, observed).
narrative_ontology:measurement(sad_debtor_extraction_be_t2026, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 2026, 0.84).
narrative_ontology:measurement_basis(sad_debtor_extraction_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(sad_debtor_extraction_su_t1982, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t1982, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t1989, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1989, 0.7).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t1989, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t1996, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 1996, 0.76).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t1996, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t2002, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2002, 0.72).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t2002, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t2008, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2008, 0.74).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t2008, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t2015, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t2015, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t2020, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t2020, observed).
narrative_ontology:measurement(sad_debtor_extraction_su_t2026, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 2026, 0.84).
narrative_ontology:measurement_basis(sad_debtor_extraction_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'structural adjustment conditionalities' decomposes into three structurally distinct constraint stories over the shared kernel structural_adjustment_conditionalities. This file instantiates the debtor_extraction_reading, authoring epsilon 0.84 over the standing arrangement assessed as an extraction architecture. The creditor_coordination_reading sibling would author low epsilon over the same referent (genuine creditor coordination); the hybrid_selectivity_reading sibling would author intermediate epsilon with enforcement keyed to geopolitical leverage. The epsilon values differ because the readings assess the same referent by different lights; each file is epsilon-invariant within its reading. Linked via affects_constraints per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
