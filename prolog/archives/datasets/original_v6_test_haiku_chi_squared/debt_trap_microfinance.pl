% ============================================================================
% CONSTRAINT STORY: debt_trap_microfinance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_trap_microfinance, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: debt_trap_microfinance
 *   human_readable: Microfinance Debt Trap
 *   domain: economic/financial_inclusion
 *
 * SUMMARY:
 *   Microfinance emerged as a pro-poor innovation with genuine coordination
 *   properties: connecting borrowers excluded from traditional banking with
 *   lenders seeking to diversify and serve unmet demand. However, the
 *   constraint has evolved into a structural debt trap extracting from the
 *   most vulnerable populations. The mechanism combines high interest rates
 *   (partial compensation for lending risk), social collateral enforcement
 *   (leveraging village peer pressure and reputation), limited exit options
 *   (borrowers cannot easily access alternative lenders), and income
 *   volatility (borrowers lack income-smoothing mechanisms to weather
 *   repayment). The constraint exhibits snare characteristics: effective
 *   extraction χ ≈ 0.80 from the borrower perspective, driven by suppression
 *   (limited alternatives, social enforcement) and base extractiveness
 *   (interest rates and compounding). Paradoxically, microfinance
 *   institutions frame the constraint as pure coordination (Rope perspective)
 *   — they genuinely believe they are solving market failures. Regulators
 *   occupy a Tangled Rope position, balancing financial inclusion policy with
 *   borrower protection mandates, but institutional capture by the MFI sector
 *   has skewed regulations to favor lender profitability. The constraint's
 *   theater ratio (0.55) reflects that microfinance marketing emphasizes
 *   poverty alleviation (theater) while profit extraction and portfolio
 *   growth dominate operational strategy (function). The trajectory shows
 *   increasing extractiveness (0.35 → 0.62) over two decades as MFI
 *   competition has pushed rates and volume upward, and theater ratio rising
 *   (0.30 → 0.55) as the gap between poverty-alleviation narrative and
 *   debt-trap reality widens.
 *
 * KEY AGENTS:
 *   - Vulnerable Borrowers: Primary victims (powerless/trapped) — lack alternative credit sources; captured by social collateral and limited exit options
 *   - Microfinance Institutions: Primary beneficiaries (institutional/arbitrage) — capture spreads between cost of capital and interest rates; exit through securitization or acquisition
 *   - Low-Income Households: Secondary victims (moderate/constrained) — debt service crowds out investment in education and business; household economic fragility increases
 *   - Village Lending Networks: Tertiary actors (moderate/mobile) — traditional RoSCAs displaced by formal MFIs; persist as social theater while economic function atrophies
 *   - Microfinance Regulator: Institutional mediator (organized/constrained) — responsible for both financial inclusion and borrower protection; increasingly captured by lender influence
 *   - Loan Investors / International Donors: Secondary beneficiaries (institutional/arbitrage) — capital providers profiting from MFI portfolio returns; maintain pro-poor narrative despite extractive outcomes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structural snare: extractive mechanism disguised as pro-poor innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_trap_microfinance, 0.62).
domain_priors:suppression_score(debt_trap_microfinance, 0.68).
domain_priors:theater_ratio(debt_trap_microfinance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_trap_microfinance, extractiveness, 0.62).
narrative_ontology:constraint_metric(debt_trap_microfinance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(debt_trap_microfinance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_trap_microfinance, snare).
narrative_ontology:human_readable(debt_trap_microfinance, "Microfinance Debt Trap").
narrative_ontology:topic_domain(debt_trap_microfinance, "economic/financial_inclusion").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_trap_microfinance, microfinance_institutions).
narrative_ontology:constraint_beneficiary(debt_trap_microfinance, loan_investors).
narrative_ontology:constraint_victim(debt_trap_microfinance, vulnerable_borrowers).
narrative_ontology:constraint_victim(debt_trap_microfinance, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE BORROWER (SNARE) — Trapped by lack of alternative credit sources, restricted exit options (repay or default with social consequences), and compounding interest. Coerced by social collateral enforcement and village peer pressure. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.80. High effective extraction.
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MICROFINANCE INSTITUTION (ROPE) — Institutional beneficiary with arbitrage options (exit through IPO, acquisition, or portfolio sale). Frames the constraint as pure coordination: matching borrowers with capital, solving market failure in underserved regions. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.07. Net beneficiary; negative effective extraction from their viewpoint.
constraint_indexing:constraint_classification(debt_trap_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LOW-INCOME HOUSEHOLD ECONOMY (SNARE) — Constrained by limited income volatility absorption, seasonal cash flow stress, and household emergency costs. Debt service crowds out investment in children's education or business expansion. d≈0.85, f(d)≈1.20, σ=0.9 → χ≈0.64. High extraction with moderate exit cost.
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MICROFINANCE REGULATOR (TANGLED ROPE) — Constrained by political pressure to support financial inclusion and lender profitability, but also responsible for borrower protection. Coordination function: rate-setting, disclosure rules, consumer protections. Active enforcement required. Asymmetric extraction: lenders benefit more than borrowers from regulatory design. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.42. Mixed coordination-extraction with institutional capture signals.
constraint_indexing:constraint_classification(debt_trap_microfinance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VILLAGE LENDING NETWORK (PITON) — Traditional rotating savings and credit associations (RoSCAs) have been displaced by formal MFIs but persist performatively. Theater_ratio≈0.65: RoSCA lending is now theatrical social practice wrapped around the reality of MFI debt service. The coordination mechanism has atrophied; the social ritual remains. d≈0.45, f(d)≈0.52, σ=0.9 → χ≈0.24. Degraded constraint maintained by social inertia.
constraint_indexing:constraint_classification(debt_trap_microfinance, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a global/civilizational view, the debt trap exhibits structural snare properties: high base extraction (0.62), severe suppression (0.68 — limited exit options, social enforcement), and high effective extraction despite moderate theater ratio. The constraint persists despite international criticism because extraction mechanisms (interest rates, social collateral, poverty-driven demand) are robust. No natural law frame can explain this — it is a designed institutional arrangement. d≈0.70, f(d)≈1.15, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_trap_microfinance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_trap_microfinance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_trap_microfinance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_trap_microfinance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_trap_microfinance, TR),
    TR >= 0.70.

:- end_tests(debt_trap_microfinance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Microfinance interest rates typically range 25-40% annually, significantly above cost of capital (~5-10% for institutional lenders). Base extraction reflects the interest spread plus compounding effects. The trajectory (0.35 → 0.62 over 20 years) shows increasing extraction as competition for borrower volume has driven interest rates upward and as securitization of portfolios has created profit pressure. Suppression (0.68): High. Borrowers face severe barriers to exit: (1) Limited alternatives — in many regions, MFIs are the only formal credit source; informal moneylenders charge even higher rates. (2) Social collateral enforcement — village peer pressure and reputation loss make default costly beyond financial penalties. (3) Information asymmetries — borrowers often don't understand compounding or refinancing costs until trapped. (4) Lock-in through repeat borrowing — initial small loans create dependency patterns. Theater ratio (0.55): Moderate. Microfinance marketing emphasizes poverty alleviation and women's empowerment (theater). Actual operations focus on portfolio growth, risk-adjusted returns, and borrower volume (function). The theater has increased over time as international criticism has mounted, requiring lenders to produce social impact reports despite unchanged extraction mechanics.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the beneficiary (MFI institutional view) and the victim (borrower powerless view). The MFI sees Rope: they are solving a genuine market failure (credit rationing of the poor) with an innovative financial instrument. The borrower sees Snare: they are trapped by lack of alternatives, compounding debt, and social coercion. The regulator sees Tangled Rope: they must coordinate financial inclusion while extracting from the poorest households, caught between lender profitability pressure and borrower welfare mandates. The analytical observer sees pure Snare: from a global/civilizational view, the constraint exhibits all hallmarks of pure extraction — high base extractiveness, severe suppression, and robust enforcement mechanisms — regardless of intent. The village lending network perspective (Piton) reveals institutional displacement: traditional RoSCAs provided similar coordination with lower theater and extraction; formal MFIs have replaced them while maintaining social ritual but degrading the actual coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable borrowers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit options. MFI institutions: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; can exit through securitization or portfolio sale. Low-income households: Victim + constrained → d≈0.85, f(d)≈1.20. High extraction but can relocate geographically or seek informal alternatives (both costly). Microfinance regulator: Victim (of political pressure) + constrained → d≈0.52, f(d)≈0.68. Regulatory capture creates a mixed position: regulator benefits from financial inclusion metrics but victims to lender lobbying. Village lending networks: Moderate + mobile → d≈0.45, f(d)≈0.52. Piton classification comes from high theater (0.65), not from extraction mechanics. Analytical observer: d≈0.70, f(d)≈1.15. Observer-relative, capturing full structural snare from global/civilizational perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves as pure Snare at ε=0.62 (χ≈0.80 from borrower perspective) because all three snare gates are met: (1) High base extractiveness (0.62 > 0.46). (2) High suppression (0.68 > 0.60). (3) High effective extraction (χ≈0.80 > 0.66 from primary victim perspective). The ambiguity that mandatrophy must resolve is: 'Is this a failed Rope (market failure becoming extractive) or a designed Snare (extractive mechanism disguised as pro-poor)?' The analytical evidence points to DESIGNED SNARE because (a) extraction increases over time despite competition, suggesting structural rather than temporary advantage; (b) suppression mechanisms are actively enforced (social collateral, portfolio lock-in, regulatory capture); (c) lenders profit-optimize despite poverty outcomes; (d) MFI entry has not solved credit rationing for the poorest (the constraint persists). The Rope perspective from the MFI is their genuine cognitive frame, but it does not reflect the structural mechanism. Mandatrophy is resolved by accepting both perspectives as empirically true from their structural positions while classifying the constraint according to its actual extractive mechanism from the primary victim's structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_outcome_gap,
    'Is the debt trap an unintended consequence of well-designed lending, or a structurally engineered outcome that benefits lenders despite pro-poor intent?',
    'Comparative analysis of MFI portfolio composition (stress-tested vs actual), borrower outcome tracking (income change vs debt service), and lender cost-benefit structures; disclosure of internal pricing models and default-rate projections',
    'If unintended: constraint is Tangled Rope (lenders genuinely trying to solve market failure but misunderstanding borrower capacity). If engineered: constraint is pure Snare (lenders profit-optimize knowing default will occur).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_outcome_gap, empirical, 'Whether debt trap is unintended outcome or profit-engineered feature').

omega_variable(
    social_collateral_binding_force,
    'How much of the debt trap''s suppression comes from formal legal enforcement vs informal village peer enforcement (social collateral)?',
    'Comparative default analysis in regions with weak vs strong village social institutions; borrower exit surveys (what actually prevents exit); litigation data and informal dispute resolution records',
    'If primarily legal enforcement: regulator can intervene (Tangled Rope perspective strengthens). If primarily social enforcement: regulator action is limited; suppression is structural to community ties (Snare perspective deepens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_collateral_binding_force, empirical, 'Relative strength of legal vs social enforcement of debt obligation').

omega_variable(
    alternative_lender_competitiveness,
    'Would entry of competitive formal lenders with lower rates and better borrower protections eliminate the debt trap, or do structural barriers (geographic reach, credit-testing speed) prevent meaningful competition?',
    'Natural experiments in regions where new lenders entered; borrower switching rates and rate/term improvements; detailed analysis of why poor borrowers cannot easily switch lenders despite higher-cost incumbents',
    'If competition eliminates trap: constraint is Rope (market failure being solved). If competition doesn''t materialize or borrowers can''t switch: constraint is Snare (competitive isolation enforced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_lender_competitiveness, empirical, 'Whether competition from alternative lenders can break the debt trap').

omega_variable(
    income_volatility_mitigation_feasibility,
    'Are income-smoothing products (savings accounts, insurance, seasonal borrowing adjustments) technically feasible and would they prevent debt traps if deployed?',
    'Pilot data on MFI savings products, insurance penetration, and income stability outcomes; cost analysis of weatherproofing borrower income vs cost of debt service',
    'If feasible and scalable: constraint could shift from Snare toward Rope/Tangled Rope if lenders deploy these. If not feasible or unprofitable to deploy: extractive structure is irreducible (Snare perspective confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_volatility_mitigation_feasibility, empirical, 'Whether income-smoothing products can prevent debt traps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_trap_microfinance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mf_debt_tr_t0, debt_trap_microfinance, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mf_debt_tr_t10, debt_trap_microfinance, theater_ratio, 10, 0.42).
narrative_ontology:measurement(mf_debt_tr_t20, debt_trap_microfinance, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(mf_debt_be_t0, debt_trap_microfinance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mf_debt_be_t10, debt_trap_microfinance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mf_debt_be_t20, debt_trap_microfinance, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_trap_microfinance, resource_allocation).
narrative_ontology:affects_constraint(debt_trap_microfinance, credit_access_exclusion).
narrative_ontology:affects_constraint(debt_trap_microfinance, poverty_intergenerational_transmission).

% DUAL FORMULATION NOTE:
% The microfinance debt trap decomposes into two structurally distinct constraints: (1) Microfinance as market coordination (addressing credit access gaps) — ε≈0.08, Rope from beneficiary perspective, but overlaid with (2) Extractive debt service burden on vulnerable households — ε≈0.62, Snare from victim perspective. These are not two views of the same constraint; they are two distinct structural mechanisms in causal sequence. Market coordination creates the institutional space within which extractive mechanisms operate. The story focuses on the second mechanism (ε=0.62); the first is a separate constraint story that should link to this one as an upstream cause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_trap_microfinance, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
