% ============================================================================
% CONSTRAINT STORY: collateral_sufficiency_floor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collateral_sufficiency_floor, []).

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
 *   constraint_id: collateral_sufficiency_floor
 *   human_readable: Collateral Sufficiency Floor in Credit Markets
 *   domain: economic/financial/credit
 *
 * SUMMARY:
 *   The collateral sufficiency floor in credit markets creates a structural
 *   tension between the legitimate lender need for repayment assurance and
 *   the systematic extraction of surplus from borrowers who hold real asset
 *   value but cannot meet institutional valuation standards. This constraint
 *   exhibits the full range of DR classification types from different
 *   structural positions. The same mechanism — requiring borrowers to pledge
 *   assets exceeding the loan amount — appears as an immutable law of lending
 *   (mountain), a coordination mechanism protecting systemic stability
 *   (rope), a legitimate risk-adjustment mechanism with embedded extraction
 *   (tangled rope), a regulatory enforcement apparatus (tangled rope from
 *   authority perspective), or pure coercion against powerless agents
 *   (snare), depending on the observer's position in the credit hierarchy.
 *   The constraint's extractiveness (0.58) reflects that collateral
 *   sufficiency floors do coordinate genuine risk management while
 *   simultaneously enabling systematic extraction through asymmetric
 *   valuation, collateral capture, and exclusion of agents with illiquid but
 *   real asset holdings. The theater ratio (0.48) indicates that collateral
 *   valuation models, while maintaining some predictive function,
 *   increasingly operate as ritualistic legitimation for lending decisions
 *   already made on other grounds — traditional haircut schedules poorly
 *   capture modern asset classes like human capital, ecosystem services, and
 *   digital rights.
 *
 * KEY AGENTS:
 *   - Trapped Borrowers: Primary victim (powerless/trapped) — cannot meet collateral requirements despite real asset value; face predatory lending alternatives or credit exclusion
 *   - Marginalized Borrowers: Secondary victim (moderate/constrained) — can theoretically meet floors but face asymmetric valuation and high friction costs; partially dependent on credit for economic mobility
 *   - Commercial Lenders: Primary beneficiary (institutional/arbitrage) — use collateral floors to price risk and access capital; have full exit options and leverage in negotiations
 *   - Collateral Pricing Authorities: Secondary beneficiary (institutional/arbitrage) — set floors to manage systemic risk; have regulatory leverage and exit options
 *   - Regulatory Authority: Mixed (organized/constrained) — enforces collateral sufficiency for systemic stability while using it as tool for sectoral control; constrained by political pressure and international coordination requirements
 *   - Collateral Valuation Apparatus: Institutional infrastructure (institutional/arbitrage) — maintains predictive models that are increasingly ceremonial; persists through regulatory inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collateral_sufficiency_floor, 0.58).
domain_priors:suppression_score(collateral_sufficiency_floor, 0.62).
domain_priors:theater_ratio(collateral_sufficiency_floor, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collateral_sufficiency_floor, extractiveness, 0.58).
narrative_ontology:constraint_metric(collateral_sufficiency_floor, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(collateral_sufficiency_floor, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collateral_sufficiency_floor, tangled_rope).
narrative_ontology:human_readable(collateral_sufficiency_floor, "Collateral Sufficiency Floor in Credit Markets").
narrative_ontology:topic_domain(collateral_sufficiency_floor, "economic/financial/credit").

domain_priors:requires_active_enforcement(collateral_sufficiency_floor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collateral_sufficiency_floor, lenders).
narrative_ontology:constraint_beneficiary(collateral_sufficiency_floor, collateral_pricing_authorities).
narrative_ontology:constraint_victim(collateral_sufficiency_floor, borrowers_with_illiquid_assets).
narrative_ontology:constraint_victim(collateral_sufficiency_floor, economically_marginalized_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED BORROWER (SNARE) — Cannot meet collateral floor requirements despite having real asset value. Faced with binary choice: accept predatory terms, use alternative high-cost lending (payday loans, pawn shops), or exit credit market entirely. No negotiation leverage. Bears full cost of collateral floor enforcement while lender captures rate premium for assumed risk.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED BORROWER (TANGLED ROPE) — Can theoretically satisfy collateral floor by liquidating secondary assets or accepting higher rates, but faces significant friction costs. The constraint coordinates genuine risk management (lender needs assurance of repayment ability) while extracting through asymmetric valuation of collateral. Constrained by high switching costs and limited alternative sources.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL LENDER (ROPE) — The collateral floor is experienced as coordination mechanism: protecting against default risk while enabling credit extension. The lender has exit options (different collateral standards, different borrower segments, collateral swaps in secondary markets). Experiences the constraint as enabling rather than restricting. Pure beneficiary with agency.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Maintains collateral sufficiency standards for genuine systemic stability (coordination function: preventing cascade failures, maintaining credit flow). But also uses collateral floors to channel lending toward preferred sectors and away from high-risk populations, creating asymmetric extraction. Active enforcement required; sunset absent but periodic regulatory review creates tactical constraint.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COLLATERAL VALUATION APPARATUS (PITON) — The specific models and procedures for setting collateral sufficiency floors (Basel III, LTV ratios, haircut schedules) have become largely performative. Originally designed to reflect true default risk, these models now function primarily as ritualistic legitimation for extraction decisions already made on other grounds. Theater ratio high because the apparatus is maintained through regulatory inertia despite degraded predictive power — modern correlations (human capital, ecosystem assets, digital rights) are poorly captured by traditional collateral models.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, collateral requirements are seen as immutable features of credit markets themselves: lenders must have assurance of recovery, and this assurance inherently requires seizeable assets. This perspective naturalizes collateral floors as logical necessities. However, the structural data contradicts the mountain classification — alternative credit models (relationship banking, group guarantees, reputation systems) exist and function without traditional collateral sufficiency floors, revealing that this 'natural law' is actually a contingent institutional choice.
constraint_indexing:constraint_classification(collateral_sufficiency_floor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collateral_sufficiency_floor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collateral_sufficiency_floor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collateral_sufficiency_floor, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collateral_sufficiency_floor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collateral_sufficiency_floor, TR),
    TR >= 0.70.

:- end_tests(collateral_sufficiency_floor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Collateral sufficiency floors do perform genuine risk management — lenders cannot credibly extend credit without assurance of recovery value. However, the extractiveness is elevated above pure risk pricing because: (1) collateral valuations are systematically conservative for certain asset classes (illiquid assets, ecosystem services, human capital), (2) lenders capture collateral through foreclosure at recovery values well below market prices, (3) the constraint excludes entire populations from credit access, and (4) regulatory authorities use floors to channel credit toward preferred sectors. The 0.58 value reflects this mixed coordination-extraction profile. Suppression (0.62): High. Multiple mechanisms suppress alternatives: (1) regulatory standardization of collateral models prevents experimentation with alternative sufficiency mechanisms, (2) capital requirements tied to collateral floors create institutional path dependency, (3) reputational barriers to lenders who deviate from standard collateral practices, and (4) lack of competitive pressure (relationship banking and group guarantee models are legally/functionally marginalized). Theater ratio (0.48): Moderate. The collateral valuation apparatus maintains some genuine predictive power — collateral does correlate with default risk, and lenders benefit from models that improve accuracy. However, the theater has increased over the measurement interval (0.35 → 0.48) because: (1) traditional asset class models (real estate LTV, inventory turnover) are increasingly applied to novel asset types (digital rights, human capital, environmental assets) where they have weak predictive power, (2) regulatory compliance audits measure adherence to standardized procedures rather than actual risk performance, and (3) enforcement of collateral standards has become decoupled from realized default outcomes in some market segments.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival divergence between powerless and institutional agents. The trapped borrower sees pure extraction (snare) because the collateral floor is an absolute barrier — there is no negotiation, no coordination benefit, only the choice between acceptance and exclusion. The lender sees pure coordination (rope) because the collateral floor enables credit extension that wouldn't otherwise occur — from their perspective, the constraint solves the problem of asymmetric information. The regulatory authority sees both: coordination (systemic stability requires collateral assurance) and extraction (floors can be weaponized to favor incumbent sectors). The trapped and lender perspectives are both structurally accurate — they are measuring different extraction flows. The trapped borrower's 'extraction' is the lender's 'risk adjustment.' The gap reveals that collateral sufficiency floors are not neutral mechanisms but transfers of surplus that appear as coordination to beneficiaries and coercion to targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is derived from the agent's structural position relative to the constraint. Trapped borrowers with no exit options occupy d ≈ 0.95 (full target); the sigmoid f(d) produces high experienced extractiveness. Institutional lenders with arbitrage options occupy d ≈ 0.10 (full beneficiary); f(d) produces negative effective extraction. Marginalized borrowers with constrained but available exits occupy d ≈ 0.60 (mixed); f(d) produces moderate extraction. The regulatory authority is structurally mixed: they are a beneficiary (collateral floors stabilize credit systems under their jurisdiction) but also constrained by international coordination requirements and political pressure; d ≈ 0.45 yields tangled rope classification. The collateral valuation apparatus is institutional with arbitrage options (can adjust haircuts, can service multiple jurisdictions); d ≈ 0.15 yields rope classification from its perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The apparent contradiction between 'collateral floors enable credit (Rope/coordination benefit)' and 'collateral floors exclude borrowers (Snare/pure extraction)' is resolved by recognizing that these are legitimate readings from different structural positions. The beneficiary's rope is the victim's snare. Both classifications are correct — they measure different extraction flows. The constraint's extractiveness (0.58) reflects that the coordination function is real but asymmetrically distributed: coordination benefits accrue to lenders and solvent borrowers with sufficient collateral; extraction costs fall on borrowers with real assets but inadequate collateral and excluded populations. The tangled rope classification from the moderate and organizational perspectives captures this hybrid nature. The mandatrophy dissolves when we recognize that 'pure coordination' is a beneficiary's perspective, not an objective property of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collateral_measurement_arbitrage,
    'Are collateral valuation disparities (price gaps between bank appraisal and market value) the result of genuine information asymmetries or systematic extraction through undervaluation?',
    'Comparative analysis of collateral valuations across institutional types (community banks, credit unions, fintech lenders) and asset classes; temporal tracking of haircut changes relative to actual default rates',
    'If genuine information asymmetry: collateral floors are legitimate risk pricing. If systematic undervaluation: collateral floors are extraction mechanism masked as risk management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collateral_measurement_arbitrage, empirical, 'Whether collateral valuation disparities reflect information asymmetries or systematic extraction').

omega_variable(
    alternative_credit_sufficiency,
    'Do alternative credit models without traditional collateral sufficiency floors (group guarantee lending, relationship banking, microfinance reputation systems) achieve comparable default rates and systemic stability?',
    'Cross-model comparison of default rates, loan performance, portfolio volatility; long-term stability analysis of credit systems using alternative sufficiency mechanisms',
    'If comparable or superior: collateral sufficiency floors are contingent institutional choice, not natural necessity. Tangled rope and snare classifications hold. If inferior: floors are legitimate safeguard; mountain classification gains credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credit_sufficiency, empirical, 'Whether alternative credit models achieve comparable stability without traditional collateral floors').

omega_variable(
    regulatory_capture_in_floor_setting,
    'Are collateral sufficiency floors set to reflect actual systemic risk or to protect incumbent lenders from competition and favor particular asset classes?',
    'Historical analysis of floor adjustments relative to actual default rates, sector profitability, and regulatory capture indicators (lobbying intensity, revolving door employment); comparison across jurisdictions with different regulatory models',
    'If floors reflect true risk: tangled rope with legitimate coordination function. If floors protect incumbents: snare with extraction masked as stability; regulatory authority moves from constrained to arbitrage exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_floor_setting, empirical, 'Whether collateral floors reflect actual risk or incumbent protection').

omega_variable(
    theater_ratio_temporal_drift,
    'Is the theater ratio (0.48) stable or increasing? Does the collateral valuation apparatus maintain predictive power or is it becoming increasingly ritualistic?',
    'Tracking collateral model prediction errors over time; analysis of whether regulatory updates to valuation models improve or degrade empirical performance; measurement of enforcement consistency (do lenders actually apply stated collateral rules uniformly?)',
    'If theater increasing and approaching piton threshold (≥0.70): collateral floors are degrading into pure legitimation apparatus. If theater stable: original coordination function remains functionally intact despite extraction overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_temporal_drift, empirical, 'Whether collateral valuation apparatus theater ratio is increasing over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collateral_sufficiency_floor, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csf_tr_t0, collateral_sufficiency_floor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(csf_tr_t7, collateral_sufficiency_floor, theater_ratio, 7, 0.42).
narrative_ontology:measurement(csf_tr_t15, collateral_sufficiency_floor, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(csf_be_t0, collateral_sufficiency_floor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(csf_be_t7, collateral_sufficiency_floor, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(csf_be_t15, collateral_sufficiency_floor, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collateral_sufficiency_floor, resource_allocation).
narrative_ontology:affects_constraint(collateral_sufficiency_floor, loan_rate_asymmetry).
narrative_ontology:affects_constraint(collateral_sufficiency_floor, collateral_foreclosure_bias).
narrative_ontology:affects_constraint(collateral_sufficiency_floor, credit_market_exclusion).

% DUAL FORMULATION NOTE:
% Collateral sufficiency floor is a parent constraint affecting loan pricing asymmetries and foreclosure mechanisms. Upstream constraints (lender capital requirements, regulatory framework for asset classification) determine what collateral standards are enforced; downstream constraints (specific collateral valuation models, haircut schedules) implement the floor. All three stories in this family should be linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collateral_sufficiency_floor, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
