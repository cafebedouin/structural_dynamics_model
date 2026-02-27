% ============================================================================
% CONSTRAINT STORY: debt_trap_microfinance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: economic/development_finance
 *
 * SUMMARY:
 *   Microfinance emerged in the 1970s-1980s as a development innovation to
 *   provide capital access to the poorest populations excluded from formal
 *   banking. The original theory was that providing small loans at market
 *   interest rates to low-income borrowers would enable income-generating
 *   activities and reduce poverty. However, structural analysis reveals a
 *   debt-trap mechanism: high interest rates (20-40% annually), mandatory
 *   insurance and hidden fees, social collateral enforcement through group
 *   liability, and coercive renewal cycles create extraction that exceeds
 *   borrower income gains. The constraint exhibits characteristics of pure
 *   extraction (Snare) from the borrower perspective but is perceived as
 *   legitimate risk management and coordination from the lender perspective.
 *   The theater ratio (0.55) reflects that impact measurement emphasizes
 *   outreach and repayment rates while suppressing default data, debt-burden
 *   analysis, and income outcomes. The extractiveness value has increased
 *   over the 20-year interval as competition for borrowers intensified,
 *   driving higher interest rates and more aggressive collateral enforcement.
 *   The constraint is contingent on institutional design choices (not a law
 *   of nature) and is beginning to be undermined by digital lending
 *   alternatives, creating scaffold-type reform pathways with genuine sunset
 *   potential.
 *
 * KEY AGENTS:
 *   - Low-income borrowers: Primary victim (powerless/trapped) — faces repayment enforced through social collateral and asset seizure; extraction exceeds income gains; no alternative access to capital
 *   - Borrower households: Primary victim (powerless/trapped) — multi-generational debt trap; children lose school access; household assets pledged as collateral; regional immobility
 *   - Microfinance institution: Primary beneficiary (institutional/arbitrage) — captures interest spread (15-25 percentage points above cost of funds), fees, and asset seizures; high arbitrage options to exit or reposition
 *   - Loan officers: Secondary beneficiary (moderate/arbitrage) — commission-based compensation incentivizes aggressive lending and coercive renewal; income depends on portfolio volume, not borrower outcomes
 *   - Investors in MFI funds: Secondary beneficiary (institutional/arbitrage) — expecting 8-12% returns; have no direct relationship with borrowers; exit through secondary market sales
 *   - Microfinance reform coalition (CGAP, Accion, SKS, local regulators): Organized actors (organized/constrained) — recognize debt trap; building alternative models (cash-flow lending, portfolio caps, digital lenders); constrained by legacy MFI incumbent resistance and network effects
 *   - Development policy community: Institutional observer (institutional/arbitrage) — maintains original microfinance narrative despite contradictory evidence; reputational investment in theory prevents acknowledgment of debt-trap mechanism
 *   - Analytical observer: Universal perspective (analytical/analytical) — sees the structural extraction mechanism and its contingency on institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_trap_microfinance, 0.58).
domain_priors:suppression_score(debt_trap_microfinance, 0.68).
domain_priors:theater_ratio(debt_trap_microfinance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_trap_microfinance, extractiveness, 0.58).
narrative_ontology:constraint_metric(debt_trap_microfinance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(debt_trap_microfinance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_trap_microfinance, snare).
narrative_ontology:human_readable(debt_trap_microfinance, "Microfinance Debt Trap").
narrative_ontology:topic_domain(debt_trap_microfinance, "economic/development_finance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_trap_microfinance, microfinance_institutions).
narrative_ontology:constraint_beneficiary(debt_trap_microfinance, loan_officers).
narrative_ontology:constraint_beneficiary(debt_trap_microfinance, investors).
narrative_ontology:constraint_victim(debt_trap_microfinance, low_income_borrowers).
narrative_ontology:constraint_victim(debt_trap_microfinance, borrower_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED BORROWER (SNARE) — Low-income borrower with no exit: repayment enforced through social collateral (group guarantees), asset seizure, or coercive renewal cycles. High interest rates (20-40% annual), mandatory insurance, hidden fees, and group liability create maximal extraction. Borrower experiences pure extraction with no coordination benefit — the loan structure actively prevents income-generating activity by consuming cash flow.
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BORROWER HOUSEHOLD (SNARE) — Intergenerational trap. Debt obligations consume household income for years; children lose school access due to labor requirements; household assets are pledged as collateral. No exit from the region without defaulting and facing social sanction. Accumulating debt cycles create multi-generational poverty lock-in.
constraint_indexing:constraint_classification(debt_trap_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MICROFINANCE INSTITUTION (ROPE) — Views itself as solving a coordination problem: providing access to capital in markets without formal banking. Experiences the constraint as legitimate risk management through interest rates, collateral, and repayment discipline. Net beneficiary — extraction flows toward the institution through interest, fees, and asset seizure. High arbitrage options: can exit individual markets, refinance portfolios, or rebrand if reputation damage emerges.
constraint_indexing:constraint_classification(debt_trap_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MICROFINANCE REFORM COALITION (SCAFFOLD) — Organized actors (CGAP, Accion, SKS reforms) recognize the debt trap and are building alternative models: group lending with dynamic incentives, cash-flow-based lending instead of asset-based collateral, financial literacy coupled with loan access, and portfolio caps to prevent over-indebtedness. These reforms represent temporary support structures (Scaffold) with sunset logic — as digital finance and formalized banking reach underserved populations, the microfinance debt-trap mechanism loses dominance. Estimated sunset: 15-25 years as mobile money and regulated digital lenders expand market access.
constraint_indexing:constraint_classification(debt_trap_microfinance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPMENT NARRATIVE (PITON) — The original microfinance narrative ('financial inclusion empowers the poor') persists in policy despite contradictory evidence. Studies show microfinance has minimal impact on borrower income and substantial default rates in high-extraction contexts. The narrative is maintained through performative metrics (% reaching poorest quintile, repayment rates) that mask debt outcomes. Theater ratio is high because reporting emphasizes outreach numbers while suppressing debt-burden and default data. The constraint persists through institutional inertia and reputational investment in the original theory, not because the mechanism works.
constraint_indexing:constraint_classification(debt_trap_microfinance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the debt-trap mechanism is a pure extraction architecture: high interest rates, social collateral enforcement, coercive renewal, hidden fees, and default-triggered asset seizure operate to transfer wealth from borrowers to institutions without creating corresponding productive capacity or income growth for borrowers. Randomized trials show minimal or negative income effects for borrowers in high-extraction environments. The constraint is not a natural law (it is contingent on institutional design choices) and not primarily a coordination mechanism (borrowers gain access but at a cost exceeding the benefit). This is snare classification across all substantive perspectives.
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
 *   Extractiveness (0.58): High. The measurement reflects that borrowers in high-extraction microfinance environments experience net negative income effects (studies show minimal to negative income growth post-borrowing). Interest rates of 20-40% annually, combined with mandatory insurance (2-5% of loan), processing fees, and renewal incentives, consume 25-35% of borrower income without creating corresponding income-generating capacity. Over-indebtedness is common — borrowers borrow repeatedly from multiple lenders to service prior debt, creating debt accumulation. RCT evidence from India, Mexico, and Philippines shows zero to negative income impact in the poorest quintile. Suppression (0.68): High. Multiple mechanisms prevent borrower exit: (1) Social collateral — group members enforce repayment through social sanction, not because they benefit but because group members are jointly liable. Default triggers loss of social standing and exclusion from future credit. (2) Asset seizure — collateral (land, productive assets, savings) is seized on default, creating downside risk far exceeding any upside from successful borrowing. (3) Information asymmetry — borrowers often do not understand the full cost of borrowing (hidden fees, mandatory insurance, renewal coercion). (4) Lack of alternatives — formal banking typically requires documented income and collateral that poor borrowers lack. Theater ratio (0.55): Moderate-high. MFI reporting emphasizes outreach metrics ('reached X million poor borrowers') and repayment rates (often >95% because of coercive enforcement) while suppressing default data, debt-burden measures, and income outcome analysis. The narrative of 'empowering the poor' is maintained despite evidence that borrower welfare outcomes are minimal or negative. The performative element has increased over the interval as institutional pressure for scale and profitability intensified.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The lender (institutional/arbitrage) sees Rope — solving the coordination problem of connecting savers with borrowers in underserved markets. The borrower (powerless/trapped) sees Snare — repayment enforced through social collateral and asset seizure, with no path to exit. The reform coalition (organized/constrained) sees Scaffold — alternative models exist and are scaling, creating a sunset to the traditional debt-trap model. The development narrative (institutional/arbitrage) sees Piton — the original theory of empowerment persists through performative metrics despite contradictory evidence. The analytical observer (analytical/analytical) sees pure Snare — high interest rates, social collateral coercion, and negative borrower outcomes constitute an extraction mechanism without genuine coordination benefit. The gap arises because different agents experience different extraction flows: the lender captures positive flows (interest spread, fees); the borrower experiences negative flows (debt burden, asset seizure, social sanction); the reform coalition experiences constrained opportunity to redirect flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Borrower directionality (d): High (0.85-0.95). Borrowers are trapped by the constraint (no genuine exit alternatives), are victims of extraction (debt burden exceeds income gains), and hold no power to reform the mechanism (powerless relative to lender). The sigmoid f(d) produces high effective extractiveness chi experienced by this agent. Lender directionality (d): Low (0.10-0.20). Lenders are beneficiaries (interest spread, fees, asset seizures), have arbitrage options (exit markets, refinance, reposition), and are institutional actors with power to set terms. Low d → negative f(d) → negative or minimal experienced chi for the lender. Reform coalition directionality (d): Moderate (0.45-0.55). Organized actors have some power and some exit options (can work in reformed sectors) but are constrained by incumbent resistance and network effects. They bear some cost (fighting institutional inertia) but also have agency to change systems. The perspectival gap is fundamental: what the lender experiences as coordination and legitimate risk management, the borrower experiences as pure extraction with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED. The mandatrophy analysis confirms that microfinance debt trap is not a hidden coordination mechanism masquerading as extraction. The structural evidence is clear: (1) Beneficiary and victim are distinct — lenders benefit consistently, borrowers bear consistent costs. (2) Extraction exceeds coordination benefit for borrowers — RCT evidence shows zero to negative income effects; borrower welfare is not improved. (3) Suppression is high and structural — social collateral, asset seizure, information asymmetry, and lack of alternatives create effective trapping. (4) Exit is not available — borrowers cannot walk away without losing assets and social standing. (5) The mechanism is not natural law — institutional design choices (interest rates, collateral policy, renewal coercion) could be different, and alternative models demonstrate viable reform pathways. The mandatrophy risk would arise if we claimed Tangled Rope (mixed coordination and extraction) — the microfinance industry narrative does make this claim: 'we provide access to capital that borrowers couldn't otherwise obtain.' But access without positive income outcomes is not coordination; it is predatory lending. The debt-trap classification as Snare is robust across all the evidence dimensions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    borrower_income_impact_causation,
    'Does microfinance debt trap reduce borrower income growth, or do low-income borrowers self-select into microfinance precisely because they lack income-growth opportunities?',
    'Randomized controlled trials with treatment/control groups in same regions; longitudinal tracking of borrower income with counterfactual matched controls; analysis of borrower selection criteria',
    'If causally negative: debt trap is extraction mechanism (Snare confirmed). If selection effect dominates: apparent trap is rational response to limited alternatives (shifts toward Rope classification). If both: extractiveness varies by borrower initial income level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(borrower_income_impact_causation, empirical, 'Causal impact of microfinance debt on borrower income').

omega_variable(
    social_collateral_enforcement_mechanism,
    'Is social collateral (group liability) a coordination mechanism that reduces lender risk (Rope interpretation) or a coercive enforcement tool that extracts from borrowers who would otherwise default (Snare interpretation)?',
    'Comparative analysis: default rates in individual vs group lending in same markets; measurement of peer pressure intensity and social sanction severity; borrower interviews on repayment motivation; analysis of repayment using coercion vs. genuinely improved cash flow',
    'If coordination: suppression ≤ 0.40, classification shifts toward Rope or Tangled Rope. If coercive: suppression ≥ 0.65, Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_collateral_enforcement_mechanism, empirical, 'Whether social collateral functions as coordination or coercion').

omega_variable(
    alternative_lender_availability,
    'In microfinance markets, what proportion of borrowers have genuine alternatives (formal bank credit, informal lending, family capital) vs. are trapped with microfinance as the only available option?',
    'Market structure analysis: banking penetration, informal lending rates, family capital availability by region; borrower survey on alternative access attempts and rejection rates',
    'High trapped proportion (>60%): trap is structural (Snare). Low trapped proportion (<30%): borrowers choose microfinance despite costs (Rope). Mixed: extractiveness varies by subpopulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_lender_availability, empirical, 'Proportion of borrowers with genuine exit alternatives').

omega_variable(
    institutional_model_variation,
    'Does the debt-trap mechanism persist across all microfinance models (community banks, digital lenders, group-based, individual) or only specific institutional forms?',
    'Comparative extractiveness measurement across model types; analysis of interest rates, collateral requirements, renewal coercion, and default outcomes by lender type; borrower outcome tracking by institution',
    'If universal: snare is structural feature of lending-to-poor mechanism itself (high ε). If model-dependent: snare is institutional design choice (lower ε, implies reform is viable). If reformable: scaffold perspective gains structural weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_model_variation, empirical, 'Whether debt trap is universal or model-specific').

omega_variable(
    reform_sustainability_timeline,
    'Are alternative microfinance models (cash-flow-based lending, portfolio caps, digital low-cost lenders) sustainable at scale and scaling fast enough to exit from the debt-trap era before a new generation of borrowers are trapped?',
    'Tracking adoption rates of reformed microfinance models; measurement of cost structure and default rates as digital lenders scale; projection of traditional high-extraction MFI market share decline; analysis of borrower migration from extractive to reformed lenders',
    'If sustainable and scaling fast (>5% annual market shift): scaffold perspective is structural, sunset timeline is real (~20 years). If stalling or fragile: scaffold is aspirational, traditional debt trap persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sustainability_timeline, empirical, 'Viability and scale of microfinance reform models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_trap_microfinance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mf_debt_tr_t0, debt_trap_microfinance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mf_debt_tr_t10, debt_trap_microfinance, theater_ratio, 10, 0.48).
narrative_ontology:measurement(mf_debt_tr_t20, debt_trap_microfinance, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(mf_debt_be_t0, debt_trap_microfinance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mf_debt_be_t10, debt_trap_microfinance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mf_debt_be_t20, debt_trap_microfinance, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_trap_microfinance, resource_allocation).
narrative_ontology:affects_constraint(debt_trap_microfinance, rural_informal_lending_substitution).
narrative_ontology:affects_constraint(debt_trap_microfinance, poverty_trap_asset_seizure).

% DUAL FORMULATION NOTE:
% The microfinance debt trap represents one constraint in a family of extractive lending mechanisms operating on the global poor. It is upstream of alternative informal lending structures and downstream of broader poverty-trap mechanisms that create the demand for microfinance in the first place. The three constraints are linked: poverty creates demand for microfinance → microfinance creates debt trap → debt trap forces alternative informal lending or asset loss. Each story has distinct extractiveness: microfinance debt trap (ε=0.58), informal lending substitution effects (ε=0.45), poverty trap dynamics (ε=0.42).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_trap_microfinance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
