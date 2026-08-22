% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__financialization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Financialized Price Formation in Housing Markets
 *   domain: political_economy/housing/financial_systems
 *
 * SUMMARY:
 *   Housing-market price formation has progressively decoupled from
 *   shelter-provision costs and wage-income fundamentals. This reading frames
 *   that decoupling as driven by financial-sector credit expansion, leverage
 *   feedback loops, and the reframing of housing from shelter to financial
 *   asset. The financial sector originates mortgages, sets lending standards,
 *   and profits from origination fees, servicing spreads, and asset-price
 *   appreciation. Primary residence households enter markets at maximum
 *   leverage to participate; non-owner-occupant investors leverage credit to
 *   acquire inventory. The constraint's persistence depends on continued
 *   credit expansion to sustain prices that would otherwise collapse toward
 *   income-relative affordability. Rising prices are treated as evidence of
 *   market health (not extraction), and price declines are treated as
 *   systemic risk (triggering regulatory intervention to suppress them). The
 *   founding problem — supply scarcity and allocation inefficiency — is long
 *   solved, but the apparatus persists as a leverage-amplification engine
 *   extracting from future-income borrowers to present-lenders.
 *
 * KEY AGENTS:
 *   - financial_sector: originator, standard-setter, arbiter of leverage availability — institutional power, arbitrage exit
 *   - real_estate_investors: leverage-enabled asset accumulators — powerful, mobile exit
 *   - primary_residence_households: identity-locked shelter-need, maximum leverage to participate — powerless, trapped by housing necessity
 *   - non_landowner_wage_earners: completely priced out, rent-bearing, constrained exit by geography and income
 *   - regulatory_authorities: mandated financial stability oversight, structurally captured by leverage-stability equivalence belief
 *   - political coalitions: homeowner-majority opposition to any policy reducing prices — constrained by electoral dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Financialized Price Formation in Housing Markets").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing/financial_systems").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '34470fb6-1feb-45e5-b5f2-29be1830e20a').
narrative_ontology:cs_kernel_codification('34470fb6-1feb-45e5-b5f2-29be1830e20a', fixed_text).
narrative_ontology:cs_authority_grounding('34470fb6-1feb-45e5-b5f2-29be1830e20a', extraction).
narrative_ontology:cs_interpretation_layer_present('34470fb6-1feb-45e5-b5f2-29be1830e20a').
narrative_ontology:cs_reading_relation('34470fb6-1feb-45e5-b5f2-29be1830e20a', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('34470fb6-1feb-45e5-b5f2-29be1830e20a', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('34470fb6-1feb-45e5-b5f2-29be1830e20a', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_axiom('34470fb6-1feb-45e5-b5f2-29be1830e20a', foundational, credit_availability_determines_price_ceiling).
narrative_ontology:cs_axiom_status(credit_availability_determines_price_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('34470fb6-1feb-45e5-b5f2-29be1830e20a', credit_availability_determines_price_ceiling, empirically_contingent).
narrative_ontology:cs_axiom('34470fb6-1feb-45e5-b5f2-29be1830e20a', foundational, housing_as_financial_asset_legitimizes_leverage).
narrative_ontology:cs_axiom_status(housing_as_financial_asset_legitimizes_leverage, holdable).
narrative_ontology:cs_axiom_grounding('34470fb6-1feb-45e5-b5f2-29be1830e20a', housing_as_financial_asset_legitimizes_leverage, conventional).
narrative_ontology:cs_reference_frame('34470fb6-1feb-45e5-b5f2-29be1830e20a', credit_enabled_price_discovery).
narrative_ontology:cs_drift_state('34470fb6-1feb-45e5-b5f2-29be1830e20a', post_2008_zero_interest_rate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('34470fb6-1feb-45e5-b5f2-29be1830e20a', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, real_estate_investors).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, primary_residence_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, non_landowner_wage_earners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, primary_residence_households).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, construction_sector).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, construction_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originates mortgages, securitizes debt instruments, and profits from origination fees, servicing spreads, and asset-price appreciation. Sets lending standards that determine who can borrow and at what leverage. Controls the volume of credit extended into housing markets and thus the amount of money chasing assets. Benefits directly from rising prices (AUM expansion, securitization volume) and from volatility (refinancing, mark-to-market opportunities). Can exit to other asset classes or geographies instantly.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, biographical, arbitrage, global).

% Purchase housing for appreciation and rental income, leveraging credit expansion to increase holdings. Benefit from rising asset prices and from credit availability that enables purchase at leverage. Can hold through downturns or exit to other markets. Compete directly with owner-occupants for inventory but have superior financing access and time-horizon flexibility.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, real_estate_investors, beneficiary,
    powerful, biographical, arbitrage, regional).

% Need housing for shelter and family stability. Enter markets denominated in credit-expanded prices that decouple from their incomes. Forced to borrow at maximum leverage to participate; debt service consumes 40-50% of income for many. Carry crash risk: if credit contracts or employment falters, negative equity traps them. Their need for shelter (identity-locked exit) means they cannot simply leave the market. Incidental benefit from living in their home, but the extraction mechanism is the leverage required to enter.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, primary_residence_households, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, primary_residence_households, beneficiary).

% Cannot afford ownership at credit-expanded prices; pay rising rents as landlords leverage credit to acquire inventory and extract rental income. Exit is constrained by geography (jobs are location-specific) and income (cannot save down-payment while paying rising rents). Bear the full crash risk of housing-as-asset without any ownership claim: when speculation contracts, rents may fall, but they also lose housing security if landlords default or liquidate.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, non_landowner_wage_earners, payer,
    powerless, biographical, constrained, national).

% Set lending standards, capital requirements, and macroprudential policy that shape credit availability. Officially charged with financial stability and prudential oversight. Observe the constraint structure but are partially captured by financial-sector expertise dependency and by the belief that price stability equals financial stability (housing-price declines are treated as systemic risk, so policymakers suppress them even when they reflect unwinding of leverage). Their official mandate conflicts with the extraction structure.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Builds housing in response to credit-driven demand signals. Benefits from high sales volumes and prices in strong credit environments. During expansions they enjoy capital inflows and high margins; during contractions they face demand collapse and margin compression. Trapped in the cycle: if they don't build during expansions they lose market share, but building into a credit-driven bubble means absorbing inventory losses when it contracts. Their supply-side position is intermediate: they profit from expansion but cannot steer credit availability.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, construction_sector, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, construction_sector, payer).

% Maintain political support by opposing policies that would lower housing prices (which homeowners perceive as personal wealth destruction). Captured by this constraint in the sense that any politician who proposes credit contraction or leverage reduction faces organized homeowner opposition and media framing as anti-growth. They have authority to change the arrangement but constrained exit from the political economy that sustains them.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, political_coalitions, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, political_coalitions, agenda_setter).

% Structural inability to access credit (poor credit history, insufficient income documentation, immigration status, or racial redlining legacy). Completely priced out and cannot participate in the coordination function (ownership) or benefit from leverage. If they could organize politically they would advocate for alternatives (public housing, strict affordability controls, or credit democratization), but are not in the conversation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, households_excluded_from_credit, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates housing and capital across time: credit extension enables current consumption of future income, allowing households to occupy housing before fully accumulating its cost. Matches savers' desire to lend with borrowers' desire to consume. Solves the inter-temporal coordination problem of matching consumption timing to income timing.
% TRANSFER_FUNCTION: Moves purchasing power from future-income earners (households) to present-lenders (financial sector and investors) via interest payments, origination fees, and asset-price capture. Also moves shelter consumption from non-leveraged households to leveraged households and investors. The volume and leverage of the transfer is set by the financial sector's lending decisions, not by the underlying cost of shelter provision.
% ABSENT_VOICES: Households excluded from credit markets, wage earners unable to participate in ownership, future generations bearing the debt service burden, construction workers whose wages are squeezed as developer margins compress in contraction phases. These parties would demand alternatives (strict lending limits, public housing, or land-value taxation) but are not at the table where credit policy is set. Regulatory authorities theoretically speak for financial stability but are structurally captured by the same credit-expansion logic they are meant to police.
% DISAPPEARANCE_RATIONALE: If credit-driven price formation disappeared overnight, housing prices would fall toward incomes and rents, wiping out leveraged investor positions and unsecured gains. Financial institutions would face massive losses on mortgage portfolios. The construction sector would experience demand collapse. Homeowner political coalitions would mobilize against any policy that induced this outcome. The entire allocation of who lives where would reorganize around affordability relative to wages rather than debt-service capacity. The macroeconomic effects would be severe.
% FOUNDING_PROBLEM: Housing markets in the mid-20th century faced periodic supply constraints and allocation inefficiency; credit expansion was adopted as a mechanism to smooth demand, enable broader home ownership, and stimulate construction and employment. The founding problem was legitimate: how to allocate scarce housing and enable income-smoothing via borrowing.
% FOUNDING_PROBLEM_CORROBORATION: Supply-side economists, construction economists, and housing-policy researchers outside the financial sector attest the founding problem (supply scarcity, allocation difficulty) is substantially solved in most developed markets — excess capacity exists, prices are decoupled from replacement costs, and credit availability (not scarcity) is now the binding constraint on prices. Financial-sector analysts continue to cite lending-enabled demand as growth stimulus, but this is defended as macroeconomic stimulus, not as a solution to the founding housing-allocation problem. Central banks' post-2008 adoption of asset-price stabilization as a financial-stability goal (preventing price declines) is corroborating evidence that the arrangement has shifted from supply-smoothing mechanism to leverage-amplification engine.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__financialization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__financialization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) and rising over 44 years, from 0.32 at baseline. This rise reflects the increasing decoupling of price from shelter-provision cost and wage-income capacity. The constraint extracts from households forced to enter at maximum leverage (debt-service burden) and from non-landowners unable to participate. Suppression is high (0.71) because the constraint's persistence requires active enforcement: regulatory suppression of price-correction mechanisms (market discipline, foreclosure, debt forgiveness), political suppression of affordability-focused alternatives (public housing, leverage limits, land-value taxation), and cultural suppression of the idea that housing should be primarily shelter rather than financial asset. Theater is moderate-high (0.42), reflecting that 42% of the enforcement infrastructure is dedicated to legitimation rather than functional operation: macroprudential framing as 'systemic risk management,' housing-as-ownership as 'personal wealth-building,' and credit expansion as 'growth stimulus' rather than leverage amplification. The coercion grid shows systematic amplification at all four levels from 1980 to 2024: accessibility collapse (how far alternatives are closed), stakes inflation (how high the cost of noncompliance), and suppression (active pressure) all rose, while resistance (ability to mount effective opposition) fell. At the individual level, a household in 1980 had more neighborhood options, lower debt-to-income requirements, and more cultural permission to rent; by 2024 the opposite holds on all three.
 *
 * PERSPECTIVAL GAP:
 *   From the financial sector's position, the constraint is genuine coordination: they provide essential lending services, assume origination risk, and deserve compensation via fees and appreciation. From the household payer position, the same structure is asymmetric extraction: the financial sector sets leverage availability and thus price ceilings, collects fees and spreads that have no relationship to marginal provision cost, and can exit into other asset classes if housing crashes. From the regulatory authority position, the constraint is a necessary stability mechanism — price declines are treated as failures of the system. From the non-landowner wage-earner position (excluded from access), the constraint is pure extraction they bear through rising rents without any ownership benefit. The engine computes these divergences from the structural data: the financial sector's arbitrage exit vs. households' identity-locked exit produces different d values; the different power atoms (institutional vs. powerless) produce different directionality amplifications; the political capture of regulatory authorities (constrained by leverage-stability ideology) produces different seat readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector: d ≈ 0.05 (full beneficiary). Originates mortgages, sets standards, profits from leverage volume and asset-price increases. Can exit to other asset classes instantly. No identity lock, maximum power, arbitrage-grade exit options. Effective extraction into this agent is deeply damped (or inverted into subsidy). Real estate investors: d ≈ 0.15 (near beneficiary). Leverage-enabled buyers with appreciation-harvesting strategies. Powerful, mobile across geographies, exit via liquidation anytime. Extract from the rising-price differential; the constraint subsidizes them. Primary residence households: d ≈ 0.92 (nearly full target). Identity-locked shelter need drives them to maximum leverage. Constrained exit (geographic and income dependence). Power atom powerless. Debt service consumes 40-50% of income. The constraint extracts via interest payments, origination fees, and crash-risk bearing. Non-landowner wage earners: d ≈ 0.85 (high target). Priced out of ownership, rent-bearing, constrained exit by location-specificity and income ceiling. Share crash risk without ownership claim. Extraction via rising rents. Regulatory authorities: d ≈ 0.45 (near symmetric, toward target). Institutionally powerful and analytic-horizon-lengthy, but structurally captured by leverage-stability equivalence. Their mandate (financial stability) and the extraction structure (leverage amplification) are coupled: price declines destabilize the system, so they are incentivized to suppress correction mechanisms. This is not a full-target position but not symmetric either. The capture is structural, not intentional; they maintain the constraint because they define system health as leverage sustainability.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding mandate (solve supply-scarce allocation via credit smoothing) is accomplished and obsolete. Housing supply exceeds demand in most developed markets; zoning and land scarcity are now the binding constraints, not credit availability. The apparatus persists not because it solves an ongoing problem but because financial institutions profit from leverage volume and price inflation, because political coalitions treat price decline as personal wealth loss, and because regulatory authorities have adopted asset-price stability as a financial-stability proxy. The dying mandate is obscured by constant rhetoric celebrating homeownership and mortgage availability — theater. The lived mandate is leverage amplification and extraction. The 2008 financial crisis temporarily exposed the mandatrophy (housing prices collapsed, leverage unraveled, the profit stream dried up), but post-2008 regulatory policy (quantitative easing, macroprudential floor-setting, institutional rescue packages) explicitly reconstructed the apparatus rather than allowing correction. This is mandatrophy denial and institutional inertia: the ruling apparatus accepts that the original problem is gone, but maintains the machinery because losing it would trigger political and financial destabilization. The measurement series (extractiveness rising from 0.32 to 0.78 over 44 years, theater rising from 0.12 to 0.42) captures this progression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_expansion_necessity,
    'Is rising credit availability structurally necessary to maintain price stability at current levels, or is it a historical contingency that policymakers have mistaken for necessity?',
    'Counterfactual: what would price trajectories be under constant credit availability? Empirical: examine periods of credit contraction (2008-2012 Eurozone, 2020 China) to observe price behavior without stimulus replacement. Theoretical: compare replacement-cost pricing in competitive supply regimes vs. credit-driven pricing in constrained supply.',
    'If credit expansion is contingent (not necessary), the constraint is an artifact of past policy choices that can be reversed; if necessary, the constraint is a structural feature of financialized housing and reversal would require systemic economic restructuring. The type classification depends on this: if contingent, more Snare-like (policy choice imposing extraction); if necessary, more complex (coordination problem shifted to leverage adequacy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credit_expansion_necessity, empirical, 'Whether credit expansion is structurally necessary for price stability or a historical policy artifact.').

omega_variable(
    beneficiary_vs_coordinator_collapse,
    'Do financial institutions benefit from credit expansion primarily as originators and servicers (paid for a real function) or primarily as speculators and leverage-profit-takers (extraction)?',
    'Forensic comparison: origination fees + servicing spreads vs. proprietary trading profits and asset-appreciation gains in financial-institution portfolios. Separate the value they create (liquidity matching, credit underwriting) from the value they extract (leverage-amplification gains). Examine financial-sector business model evolution: did originate-to-distribute models isolate lenders from default risk, removing skin-in-the-game and incentivizing leverage over prudence?',
    'If primarily coordinators being compensated, the Tangled Rope read is correct: coordination function + asymmetric extraction of compensation. If primarily speculators, the constraint is closer to Snare with a coordination cover story. The measurement hinges on counterfactual: what would financial-sector profits be under a non-leverage-amplifying lending regime?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_vs_coordinator_collapse, empirical, 'Whether financial institutions extract primarily by performing coordination functions or by leveraging imbalances.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression (0.71) primarily structural (legal, financial, zoning barriers to alternatives) or primarily internalized (households'' identity fusion, cultural acceptance of leverage, psychological defense of their own choices)?',
    'Post-exit trajectory: if households that sell at negative equity or leave homeownership rapidly adopt alternative housing narratives (renting is fine, housing-as-asset is toxic), suppression is predominantly structural. If they persist in defending homeownership and criticizing alternatives despite personal loss, suppression is internalized. Also: does access to credit-escape alternatives (migration to cheap countries, multigenerational housing, deliberate non-ownership) show adoption patterns consistent with suppression or preference?',
    'Structural suppression can be removed by regulatory action; internalized suppression persists after external barriers fall, requiring consciousness work. The classification changes: if internalized, directionality for identity-locked households should amplify further; if structural, removing barriers should shift directionality downward and type leftward (more Rope-like once extraction access is equal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Whether measured suppression is external barriers or internalized acceptance.').

omega_variable(
    kernel_reading_contest,
    'Is price formation better described by this financialization reading, or by one of the sibling readings (naturalist, georgist, institutional)?',
    'Ablation test: disable each mechanism (no credit expansion, no zoning constraint, no asset-class reframing) and observe which produces the observed price trajectory. Compare model fit: econometric models incorporating financialization effects vs. models omitting them. Examine time-series dynamics: periods of credit acceleration and deceleration should predict price changes if financialization reading is dominant.',
    'Each reading implies different policy prescriptions: naturalist→supply expansion; georgist→land-tax; institutional→zoning/platform reform; financialization→credit regulation. If the financialization reading is correct, credit-focused policies (lending standards, leverage caps, macroprudential tools) should move prices more effectively than supply-side policies alone. If incorrect, credit regulation will be ineffective and other readings'' prescriptions will dominate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether financialization reading correctly identifies the dominant price-formation mechanism relative to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(pric_tr_t1995, price_formation_kernel__financialization_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(pric_tr_t2005, price_formation_kernel__financialization_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(pric_tr_t2008, price_formation_kernel__financialization_reading, theater_ratio, 2008, 0.39).
narrative_ontology:measurement(pric_tr_t2015, price_formation_kernel__financialization_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__financialization_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(pric_be_t1995, price_formation_kernel__financialization_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(pric_be_t2005, price_formation_kernel__financialization_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(pric_be_t2008, price_formation_kernel__financialization_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(pric_be_t2015, price_formation_kernel__financialization_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__financialization_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(pric_su_t1995, price_formation_kernel__financialization_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(pric_su_t2005, price_formation_kernel__financialization_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(pric_su_t2008, price_formation_kernel__financialization_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(pric_su_t2015, price_formation_kernel__financialization_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__financialization_reading, suppression_requirement, 2024, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2024
narrative_ontology:measurement(pric_grid_01, price_formation_kernel__financialization_reading, accessibility_collapse(class), 1980, 0.38).
narrative_ontology:measurement(pric_grid_02, price_formation_kernel__financialization_reading, accessibility_collapse(class), 2024, 0.72).
narrative_ontology:measurement(pric_grid_03, price_formation_kernel__financialization_reading, accessibility_collapse(individual), 1980, 0.28).
narrative_ontology:measurement(pric_grid_04, price_formation_kernel__financialization_reading, accessibility_collapse(individual), 2024, 0.65).
narrative_ontology:measurement(pric_grid_05, price_formation_kernel__financialization_reading, accessibility_collapse(organizational), 1980, 0.42).
narrative_ontology:measurement(pric_grid_06, price_formation_kernel__financialization_reading, accessibility_collapse(organizational), 2024, 0.71).
narrative_ontology:measurement(pric_grid_07, price_formation_kernel__financialization_reading, accessibility_collapse(structural), 1980, 0.35).
narrative_ontology:measurement(pric_grid_08, price_formation_kernel__financialization_reading, accessibility_collapse(structural), 2024, 0.68).
narrative_ontology:measurement(pric_grid_09, price_formation_kernel__financialization_reading, resistance(class), 1980, 0.55).
narrative_ontology:measurement(pric_grid_10, price_formation_kernel__financialization_reading, resistance(class), 2024, 0.38).
narrative_ontology:measurement(pric_grid_11, price_formation_kernel__financialization_reading, resistance(individual), 1980, 0.42).
narrative_ontology:measurement(pric_grid_12, price_formation_kernel__financialization_reading, resistance(individual), 2024, 0.35).
narrative_ontology:measurement(pric_grid_13, price_formation_kernel__financialization_reading, resistance(organizational), 1980, 0.62).
narrative_ontology:measurement(pric_grid_14, price_formation_kernel__financialization_reading, resistance(organizational), 2024, 0.28).
narrative_ontology:measurement(pric_grid_15, price_formation_kernel__financialization_reading, resistance(structural), 1980, 0.68).
narrative_ontology:measurement(pric_grid_16, price_formation_kernel__financialization_reading, resistance(structural), 2024, 0.32).
narrative_ontology:measurement(pric_grid_17, price_formation_kernel__financialization_reading, stakes_inflation(class), 1980, 0.35).
narrative_ontology:measurement(pric_grid_18, price_formation_kernel__financialization_reading, stakes_inflation(class), 2024, 0.75).
narrative_ontology:measurement(pric_grid_19, price_formation_kernel__financialization_reading, stakes_inflation(individual), 1980, 0.25).
narrative_ontology:measurement(pric_grid_20, price_formation_kernel__financialization_reading, stakes_inflation(individual), 2024, 0.68).
narrative_ontology:measurement(pric_grid_21, price_formation_kernel__financialization_reading, stakes_inflation(organizational), 1980, 0.38).
narrative_ontology:measurement(pric_grid_22, price_formation_kernel__financialization_reading, stakes_inflation(organizational), 2024, 0.72).
narrative_ontology:measurement(pric_grid_23, price_formation_kernel__financialization_reading, stakes_inflation(structural), 1980, 0.32).
narrative_ontology:measurement(pric_grid_24, price_formation_kernel__financialization_reading, stakes_inflation(structural), 2024, 0.74).
narrative_ontology:measurement(pric_grid_25, price_formation_kernel__financialization_reading, suppression(class), 1980, 0.38).
narrative_ontology:measurement(pric_grid_26, price_formation_kernel__financialization_reading, suppression(class), 2024, 0.71).
narrative_ontology:measurement(pric_grid_27, price_formation_kernel__financialization_reading, suppression(individual), 1980, 0.42).
narrative_ontology:measurement(pric_grid_28, price_formation_kernel__financialization_reading, suppression(individual), 2024, 0.73).
narrative_ontology:measurement(pric_grid_29, price_formation_kernel__financialization_reading, suppression(organizational), 1980, 0.32).
narrative_ontology:measurement(pric_grid_30, price_formation_kernel__financialization_reading, suppression(organizational), 2024, 0.68).
narrative_ontology:measurement(pric_grid_31, price_formation_kernel__financialization_reading, suppression(structural), 1980, 0.28).
narrative_ontology:measurement(pric_grid_32, price_formation_kernel__financialization_reading, suppression(structural), 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, household_debt_trap_constraint).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, regulatory_capture_financial_sector).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the price_formation_kernel, coexisting with naturalist, georgist, and institutional readings. Each reading identifies a different mechanism determining housing prices. The financialization reading emphasizes credit expansion and leverage feedback, yielding Tangled Rope classification with high extractiveness. Decomposition rationale: the same kernel-level phenomenon (housing price determination) admits multiple ε values depending on framing — financialization reading assigns high ε (leverage is extraction), while naturalist reading assigns low ε (equilibrium process). These are not observables of one constraint but different constraints instantiating different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
