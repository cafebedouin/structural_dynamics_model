% ============================================================================
% CONSTRAINT STORY: future_claims_present_resources_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_future_claims_present_resources_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: future_claims_present_resources_flat_control
 *   human_readable: Present Conversion of Future Productive Capacity into Purchasing Power (Money/Credit/Insurance/Tax/Bankruptcy Complex)
 *   domain: constitutional_political_economy/monetary_theory/corporate_property_law
 *
 * SUMMARY:
 *   This story treats as a single flat constraint the persisting kernel
 *   commitment underlying money, credit, insurance, taxation, and bankruptcy
 *   law: the shared question of how a proposed future acquires present
 *   purchasing power. Rather than decomposing into separate readings of that
 *   kernel, this authoring treats the entire institutional complex —
 *   sovereign monetary authority, commercial credit issuance, private
 *   insurance, state taxation, and bankruptcy priority-of-claims — as one
 *   constraint whose coordination function (intertemporal resource
 *   allocation) and extractive structure (asymmetric distribution of who
 *   bears the downside when projected futures fail to materialize) are
 *   authored together. The contestation over WHICH institutional answer
 *   should be authoritative, and on what terms, is left to land as
 *   perspectival disagreement across stakeholder seats and as open omegas,
 *   rather than being split into separate reading-stories.
 *
 * KEY AGENTS:
 *   - central_bank_and_treasury_authorities: sets terms of conversion, institutional/arbitrage — administers the answer
 *   - commercial_credit_issuers: originates specific conversion instruments, organized/mobile — collects the price of conversion
 *   - sovereign_debt_holders: purchases claims on future tax revenue, powerful/arbitrage — benefits with exit leverage
 *   - insured_capital_owners: purchases certainty about the future, powerful/mobile — benefits from bounding uncertainty
 *   - unbanked_and_thin_file_households: excluded from favorable conversion terms, powerless/trapped — pays through exclusion
 *   - future_taxpayers: bears the ultimate collateral, powerless/civilizational/trapped — pays without present voice
 *   - unsecured_creditors_in_bankruptcy: absorbs failed future-capacity bets, moderate/constrained — pays through subordination
 *   - currency_holders_subject_to_debasement: absorbs monetization costs, powerless/constrained — pays through inflation
 *   - unrepresented_future_generations: excluded from all present negotiation, powerless/trapped — the deepest absent voice
 *   - constitutional_drafters_and_courts: allocates legitimacy among competing answers, institutional/analytical — observes and rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(future_claims_present_resources_flat_control, 0.61).
domain_priors:suppression_score(future_claims_present_resources_flat_control, 0.58).
domain_priors:theater_ratio(future_claims_present_resources_flat_control, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(future_claims_present_resources_flat_control, extractiveness, 0.61).
narrative_ontology:constraint_metric(future_claims_present_resources_flat_control, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(future_claims_present_resources_flat_control, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(future_claims_present_resources_flat_control, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(future_claims_present_resources_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(future_claims_present_resources_flat_control, tangled_rope).
narrative_ontology:human_readable(future_claims_present_resources_flat_control, "Present Conversion of Future Productive Capacity into Purchasing Power (Money/Credit/Insurance/Tax/Bankruptcy Complex)").
narrative_ontology:topic_domain(future_claims_present_resources_flat_control, "constitutional_political_economy/monetary_theory/corporate_property_law").

domain_priors:requires_active_enforcement(future_claims_present_resources_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(future_claims_present_resources_flat_control, future_claims_present_resources).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(future_claims_present_resources_flat_control, central_bank_and_treasury_authorities).
narrative_ontology:constraint_beneficiary(future_claims_present_resources_flat_control, commercial_credit_issuers).
narrative_ontology:constraint_beneficiary(future_claims_present_resources_flat_control, sovereign_debt_holders).
narrative_ontology:constraint_beneficiary(future_claims_present_resources_flat_control, insured_capital_owners).
narrative_ontology:constraint_victim(future_claims_present_resources_flat_control, unbanked_and_thin_file_households).
narrative_ontology:constraint_victim(future_claims_present_resources_flat_control, future_taxpayers).
narrative_ontology:constraint_victim(future_claims_present_resources_flat_control, unsecured_creditors_in_bankruptcy).
narrative_ontology:constraint_victim(future_claims_present_resources_flat_control, currency_holders_subject_to_debasement).
narrative_ontology:constraint_vindicates(future_claims_present_resources_flat_control, sovereign_monetary_authority_doctrine).
narrative_ontology:constraint_vindicates(future_claims_present_resources_flat_control, limited_liability_capital_formation_doctrine).
narrative_ontology:constraint_vindicates(future_claims_present_resources_flat_control, priority_of_claims_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms under which promises about future output become spendable money today: issues currency, sets policy rates, backstops the banking system, and ultimately decides whose claims on the future get monetized first (its own sovereign debt) and whose get rationed. Its authority to answer the Boss Fight question is precisely what constitutional ratification (C2) is meant to legitimate or contest.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, central_bank_and_treasury_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Banks and lenders originate the specific instruments (loans, credit lines, securitized debt) that convert a borrower's projected future income into present spending power, charging interest as the price of that conversion and holding first-mover advantage in deciding whose future gets discounted favorably.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, commercial_credit_issuers, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(future_claims_present_resources_flat_control, commercial_credit_issuers, beneficiary).

% Purchase claims on the state's future tax revenue at a price reflecting confidence in the state's capacity to make good on that revenue; can exit into other sovereigns' debt or hard assets if confidence wavers, giving them leverage over how aggressively the state can answer the Boss Fight question in its own favor.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, sovereign_debt_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Businesses and individuals who can pay a premium today to convert an uncertain future loss into a present, bounded, tradeable liability held by an insurer — effectively purchasing certainty about the future at a price only the well-capitalized can consistently afford.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, insured_capital_owners, beneficiary,
    powerful, biographical, mobile, national).

% Lack the documented credit history or collateral that the money/credit system requires to certify a claim on their future income; must pay cash premiums, use predatory alternative lenders, or forgo consumption entirely, bearing the cost of a system that only recognizes certain kinds of future capacity as monetizable.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, unbanked_and_thin_file_households, payer,
    powerless, biographical, trapped, local).

% Are not present in any negotiation over today's deficit spending, sovereign borrowing, or bailout guarantees, yet are the ultimate collateral behind every claim the state issues against 'the future' — they inherit the tax obligations, inflation exposure, or austerity that results when today's conversion of future capacity into present resources is priced generously to today's claimants.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, future_taxpayers, payer,
    powerless, civilizational, trapped, national).

% Extended goods, services, or short-term credit on the assumption of the debtor's continuing future capacity; when that capacity fails to materialize, bankruptcy's priority-of-claims rule places them behind secured lenders and administrative costs, converting their expected future payment into a fractional, delayed, or total loss determined by a legal ordering they had no hand in setting.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, unsecured_creditors_in_bankruptcy, payer,
    moderate, immediate, constrained, national).

% Hold cash and cash-denominated savings that lose real value whenever monetary authorities expand the money supply to monetize future claims (deficit financing, crisis liquidity) faster than real output grows; cannot meaningfully exit the national currency without cost, sophistication, or capital most do not have.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, currency_holders_subject_to_debasement, payer,
    powerless, biographical, constrained, national).

% Would object to today's terms of converting projected future productive capacity into present resources if they could — every unit of sovereign debt, every unfunded liability, every inflationary financing choice discounts their eventual capacity without their consent — but they have no seat in any legislature, court, or central bank meeting that sets these terms.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, unrepresented_future_generations, excluded,
    powerless, civilizational, trapped, national).

% Determine, through ratification and judicial interpretation (C2), which institutional answer to the Boss Fight question receives constitutional legitimacy — whether sovereign money-issuance, private credit contract, insurance pooling, taxation, or bankruptcy priority is treated as the authoritative mechanism, and under what limits. Their choices allocate legitimacy but do not themselves collect or pay.
narrative_ontology:constraint_stakeholder(future_claims_present_resources_flat_control, constitutional_drafters_and_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(future_claims_present_resources_flat_control, diffuse).
narrative_ontology:fixing_cost_class(future_claims_present_resources_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The complex solves a genuine and unavoidable problem: productive capacity that does not yet exist (next year's harvest, next decade's output, an uninvented technology, an unrealized loss) cannot be physically transacted today, yet present decisions — investment, consumption, risk-pooling, disaster recovery — require resources now. Money, credit, insurance, taxation, and bankruptcy are five distinct institutional technologies that each answer 'how does a claim on the not-yet-existing become a claim on the actually-existing,' allowing present resource allocation to track expected future capacity rather than only present capacity.
% TRANSFER_FUNCTION: Moves present real resources (goods, labor, capital) to whoever holds a legitimated claim on the future, financed by future resources extracted from whoever bears the eventual repayment, tax, premium, or priority-of-claims cost — i.e., from future taxpayers, future debtors' labor, future insured events, and subordinated creditors — to present borrowers, the state, insurers, and senior claimants.
% ABSENT_VOICES: Future taxpayers and future generations bear the largest aggregate transfer and have no seat in any present negotiation; unbanked households are present but structurally screened out of the credit-issuance conversation that would let them monetize their own future capacity on comparable terms to the banked.
% DISAPPEARANCE_RATIONALE: If no institution were permitted to convert projected future capacity into present purchasing power — no money creation, no credit, no insurance, no deficit-financed taxation, no bankruptcy reorganization — virtually all investment, infrastructure, disaster recovery, and intertemporal consumption smoothing would collapse to whatever could be financed from currently-realized resources alone; the modern economy is structurally unrecognizable without some answer to the Boss Fight question.
% FOUNDING_PROBLEM: Economic activity requires resources to be committed before the output they are meant to produce exists — a farmer needs seed before harvest, a state needs an army before the tax base that will fund it recovers, a business needs capital before revenue. Absent an institutional answer, only accumulated present wealth could finance the future, freezing growth and concentrating power in whoever already holds resources.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians and comparative-law scholars (outside the central banks, credit issuers, and insurers who administer these institutions) attest that the intertemporal financing problem remains structurally unsolved by any alternative mechanism — every documented economy above subsistence scale has developed some version of money, credit, or insurance to bridge it. The dispute is not whether the founding problem is live but whether the current allocation of who bears the downside is defensible; that second question is contested and is not resolved by the founding-problem corroboration itself.
narrative_ontology:disappearance_verdict(future_claims_present_resources_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(future_claims_present_resources_flat_control, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(future_claims_present_resources_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-08',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(future_claims_present_resources_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(future_claims_present_resources_flat_control, 0.61, 'claude-sonnet-5', 'c2_monetary_architecture_2026_20260808_170220', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(future_claims_present_resources_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(future_claims_present_resources_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(future_claims_present_resources_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.61 because the complex genuinely solves an intertemporal coordination problem (the founding_problem is live and corroborated outside the beneficiary set) but the terms on which that coordination occurs are set overwhelmingly by the parties who benefit from favorable conversion rates — central banks, credit issuers, sovereign creditors, and insurers — while the costs of mispriced or failed conversions (inflation, subordination, exclusion, future tax burden) fall on parties absent from or structurally disadvantaged within the negotiation. Suppression (0.58) reflects the real but partial coercive backbone: legal tender laws, bankruptcy court authority, and regulatory licensing of credit issuance are enforced, but genuine alternatives (barter, informal credit, mutual aid, cryptocurrency at the margins) are not fully suppressed, only marginalized. Theater ratio (0.33) captures a moderate but rising share of activity that performs prudential soundness (capital adequacy theater, ratings theater, deficit 'concern' theater) without proportionally changing who bears the ultimate risk. Accessibility collapse (0.52) and resistance (0.55) are mid-range: once understood, the system's basic logic is hard for an individual to escape (you cannot simply opt out of currency or bankruptcy priority), but organized resistance (debtor unions, MMT-adjacent policy movements, insurance reform coalitions) is real and ongoing, unlike a true mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank's or credit issuer's seat, this looks like rope or scaffold: a necessary, well-managed intertemporal bridge that anyone can in principle access on improving terms as institutions mature (financial inclusion narratives support this). From the unbanked household's or future taxpayer's seat, the same structure looks like a tangled rope shading toward snare: a genuine coordination function exists, but the specific terms of access and the specific allocation of downside risk are set by parties who do not bear the downside, producing asymmetric extraction dressed as prudent macro-management. The engine computing divergent seat classifications from identical structural data is exactly the intended behavior here — this flat authoring does not resolve which seat's view is 'correct'; it lets the structural asymmetry produce the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows cleanly from institutional position within the conversion mechanism. Central banks, credit issuers, sovereign debt holders, and insured capital owners sit near the beneficiary end: they either administer the terms of conversion or can purchase favorable conversion terms and exit into alternative assets or jurisdictions if terms sour. Unbanked households, future taxpayers, unsecured creditors, and currency holders sit near the target end: they either cannot access the conversion mechanism on fair terms, or they are the residual bearers of cost when conversions are mispriced, with little to no exit (currency holders cannot easily de-dollarize their savings; future taxpayers cannot renegotiate obligations incurred before their political existence). Future generations are the extreme case: zero present voice, maximal future exposure, fully trapped by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intertemporal resource bridging) remains genuinely live — modern economies cannot function without some institutional answer to the Boss Fight question, so this is not a pure mandatrophy case where the function has vanished while the mandate persists. What has drifted is the allocation of who administers the answer and who bears its failure costs: the coordination function persists while the distributional terms have shifted toward concentrating benefit among institutional agenda-setters and dispersing cost among powerless and future-tense payers. This is precisely the tangled_rope signature — genuine coordination plus real asymmetric extraction, both true simultaneously — rather than either a pure rope (which would require the terms to be genuinely available to all on comparable footing) or a pure snare (which would require the coordination story to be mere cover with no real function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_authority_vs_constructed_privilege,
    'Is sovereign monetary authority''s privileged position in answering the Boss Fight question (issuing the reference currency, backstopping the banking system, monetizing its own debt first) a structurally necessary feature of any workable intertemporal coordination system, or a constructed privilege that a differently-designed constitutional settlement could allocate more broadly or more competitively?',
    'Comparative constitutional and monetary history: examine historical periods and jurisdictions with plural currency issuance, free banking, or currency board arrangements to assess whether sovereign monetary monopoly is functionally necessary or one design choice among several viable alternatives.',
    'If structurally necessary, the extraction embedded in sovereign monetary privilege is closer to an unavoidable coordination cost; if constructed, the current concentration of conversion authority in central banks and treasuries is a contestable design choice that could be redistributed, materially changing the extraction assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_authority_vs_constructed_privilege, conceptual, 'Whether sovereign monetary primacy in the conversion mechanism is necessary or constructed.').

omega_variable(
    future_generations_representation_gap,
    'Can any present constitutional or institutional mechanism adequately represent the interests of future taxpayers and future generations in setting the terms of present-future resource conversion, or is their exclusion structurally irreducible?',
    'Examine whether fiscal rules, generational accounting mandates, or independent fiscal councils with binding authority have in practice altered outcomes for future cohorts, versus remaining advisory theater.',
    'If representable, part of the measured extraction against future generations could be mitigated through institutional reform without abandoning the conversion mechanism; if structurally irreducible, the extraction from unrepresented future parties is a permanent feature of any present-future conversion system, however designed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation_gap, conceptual, 'Whether future-generation exclusion from conversion-term-setting is fixable or inherent.').

omega_variable(
    credit_access_screening_necessity,
    'Is the exclusion of unbanked and thin-file households from favorable credit terms a necessary consequence of legitimate risk assessment (their future income genuinely is harder to verify and predict), or a constructed barrier reflecting biased or lazy underwriting criteria that could be corrected without increasing systemic risk?',
    'Empirical study of alternative underwriting models (cash-flow-based, community-vouched, algorithmic-alternative-data) tested against traditional credit-history-based underwriting for actual default rates among currently-excluded populations.',
    'If necessary, unbanked exclusion is a coordination cost of accurate risk-pricing; if constructed, it is closer to pure extraction dressed as prudent risk management, and the tangled_rope classification would weight more heavily toward the extraction pole for this specific victim group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_access_screening_necessity, empirical, 'Whether credit-access exclusion of unbanked households reflects genuine risk or constructed bias.').

omega_variable(
    bankruptcy_priority_fairness,
    'Is the priority-of-claims ordering in bankruptcy law (secured creditors and administrative costs ahead of unsecured creditors) a fair allocation of risk that unsecured creditors implicitly price into their terms, or a systematically underpriced risk that unsecured creditors bear without adequate compensation because they lack bargaining power to demand security interests?',
    'Empirical analysis of whether unsecured trade credit and short-term supplier financing terms actually incorporate a risk premium sufficient to compensate for typical bankruptcy recovery rates, across firm size and creditor sophistication.',
    'If adequately priced, the priority ordering is a known and compensated feature of the credit system; if systematically underpriced, unsecured creditors — often smaller suppliers and employees — bear uncompensated extraction embedded in the bankruptcy priority structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bankruptcy_priority_fairness, empirical, 'Whether unsecured creditor subordination in bankruptcy is adequately priced or systematically extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(future_claims_present_resources_flat_control, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(futu_tr_t0, future_claims_present_resources_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(futu_tr_t10, future_claims_present_resources_flat_control, theater_ratio, 10, 0.22).
narrative_ontology:measurement(futu_tr_t20, future_claims_present_resources_flat_control, theater_ratio, 20, 0.25).
narrative_ontology:measurement(futu_tr_t30, future_claims_present_resources_flat_control, theater_ratio, 30, 0.28).
narrative_ontology:measurement(futu_tr_t40, future_claims_present_resources_flat_control, theater_ratio, 40, 0.31).
narrative_ontology:measurement(futu_tr_t50, future_claims_present_resources_flat_control, theater_ratio, 50, 0.33).

% Extraction over time
narrative_ontology:measurement(futu_be_t0, future_claims_present_resources_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(futu_be_t10, future_claims_present_resources_flat_control, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(futu_be_t20, future_claims_present_resources_flat_control, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(futu_be_t30, future_claims_present_resources_flat_control, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(futu_be_t40, future_claims_present_resources_flat_control, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(futu_be_t50, future_claims_present_resources_flat_control, base_extractiveness, 50, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(futu_su_t0, future_claims_present_resources_flat_control, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(futu_su_t10, future_claims_present_resources_flat_control, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(futu_su_t20, future_claims_present_resources_flat_control, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(futu_su_t30, future_claims_present_resources_flat_control, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(futu_su_t40, future_claims_present_resources_flat_control, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(futu_su_t50, future_claims_present_resources_flat_control, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(future_claims_present_resources_flat_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(future_claims_present_resources_flat_control, 0.15).

% DUAL FORMULATION NOTE:
% This story authors the Boss Fight kernel commitment as a single flat constraint spanning money, credit, insurance, taxation, and bankruptcy, per the construction-perturbation control instruction. It deliberately does not decompose into per-institution reading-stories (a money-issuance reading, a credit reading, an insurance reading, a taxation reading, a bankruptcy reading) even though such a decomposition would likely be warranted under the ε-invariance principle if each institution's extraction and beneficiary structure were measured separately — sovereign money creation and consumer credit access plausibly have different ε values. This flat-construction file exists specifically to test how the framework's perspectival and omega machinery absorbs contestation when decomposition is withheld by design, not because the underlying kernel is analytically indivisible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
