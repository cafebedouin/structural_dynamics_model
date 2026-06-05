% ============================================================================
% CONSTRAINT STORY: uk_help_to_buy_scheme
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_help_to_buy_scheme, []).

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
 *   constraint_id: uk_help_to_buy_scheme
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic/housing_policy
 *
 * SUMMARY:
 *   The UK's Help to Buy equity loan scheme (2013-2023) represents a
 *   paradigmatic Tangled Rope constraint: it combines genuine coordination
 *   function (expanding access to mortgage credit for first-time buyers) with
 *   asymmetric extraction (inflating house prices, concentrating benefits to
 *   builders and existing homeowners, imposing long-term debt on vulnerable
 *   buyers, and harming excluded renters). The scheme operated as a
 *   demand-side subsidy in a supply-constrained market, creating the
 *   structural conditions for extraction: stimulating demand without
 *   expanding supply inevitably raises prices, transferring value from
 *   renters and future buyers to current sellers and builders. The 10-year
 *   temporal trajectory shows extractiveness increasing from 0.28 (early
 *   years, genuine access expansion) to 0.52 (late years, primarily price
 *   inflation mechanism). Theater ratio rises from 0.45 to 0.65, reflecting
 *   increasing divergence between the public narrative of 'affordability
 *   support' and the structural reality of price amplification. The scheme's
 *   sunset in 2023 was politically forced by recognition that extraction
 *   costs had come to exceed coordination benefits.
 *
 * KEY AGENTS:
 *   - First-time buyers: Primary victims (powerless/trapped) — access loan but acquire inflated-price properties and face decade of negative equity risk if prices stagnate or fall
 *   - House builders and landowners: Primary beneficiaries (institutional/arbitrage) — capture demand stimulus as price inflation; land values increase; predictable sales support
 *   - Private renters: Secondary victims (moderate/constrained) — excluded from scheme but pay rising rents as property values and investor competition increase; left with degraded affordability pathway
 *   - Mortgage lenders: Secondary beneficiaries (institutional/arbitrage) — reduced credit risk via government equity backing; expanded lending volume
 *   - Government housing policy authority: Enforcers (powerful/mobile) — requires active enforcement; sunset enacted when fiscal and political costs became apparent
 *   - House price stability and market efficiency: Abstract victims (powerless/trapped) — suffer persistent inflation without ability to organize or exit
 *   - Housing reform advocates: Organized observers (organized/constrained) — see scheme as temporary symptom-treatment masking need for supply-side reform (planning, building regulations, land cost reduction)
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risk of naturalizing housing crisis as immutable vs recognizing it as policy-driven
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_help_to_buy_scheme, 0.52).
domain_priors:suppression_score(uk_help_to_buy_scheme, 0.68).
domain_priors:theater_ratio(uk_help_to_buy_scheme, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_help_to_buy_scheme, tangled_rope).
narrative_ontology:human_readable(uk_help_to_buy_scheme, "UK 'Help to Buy' Equity Loan Scheme").
narrative_ontology:topic_domain(uk_help_to_buy_scheme, "economic/housing_policy").

domain_priors:requires_active_enforcement(uk_help_to_buy_scheme).
narrative_ontology:has_sunset_clause(uk_help_to_buy_scheme).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, house_builders).
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, landowners).
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, mortgage_lenders).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, first_time_buyers).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, private_renters).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, house_price_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIRST-TIME BUYER (SNARE) — Trapped by housing affordability crisis and mortgage lending criteria. Help to Buy provides perceived access but creates long-term debt obligation and exposure to house price risk. Exit options are constrained: renting offers no wealth accumulation; saving for larger deposit in high-price market is infeasible. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOUSE BUILDERS AND LANDOWNERS (ROPE) — Primary beneficiaries. Experience Help to Buy as pure coordination mechanism: it solves the collective action problem of housing supply demand mismatch by expanding the pool of qualified buyers. Builders gain predictable demand and price support; land values increase; supply-side coordination is enhanced. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVATE RENTERS (TANGLED ROPE) — Constrained by rising house prices (inflated by Help to Buy stimulus) while excluded from the scheme (requires new-build purchases). Experience mixed extraction (higher rents, reduced homeownership pathway) alongside limited coordination benefit (some may eventually use scheme). Theater of inclusivity masks exclusionary mechanics. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MORTGAGE LENDERS (ROPE) — Secondary beneficiary. Government equity backing reduces credit risk on first-time buyer mortgages; expanded lending volume increases bank profits. Lenders experience the scheme as risk-mitigation coordination. Exit options are strong (can exit lending without consequence). d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HOUSE PRICE STABILITY (SNARE) — Abstract collective good that bears the structural cost. Help to Buy artificially stimulates demand without increasing supply, inflating house prices. This extraction is persistent and intergenerational: younger cohorts face ever-higher price thresholds. Cannot exit or organize. d≈0.93, f(d)≈1.41, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: GOVERNMENT HOUSING POLICY AUTHORITY (TANGLED ROPE) — Beneficiary (expands housing policy toolkit, demonstrates intervention) but constrained by subsequent electoral and fiscal costs. Required_active_enforcement=true. Sunset clause enacted 2023 as cost became apparent. Mixed extraction (fiscal burden, political risk from inflated prices) alongside coordination (homeownership targets met). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: HOUSING REFORM ADVOCACY COALITION (SCAFFOLD) — Organized actors (housing charities, reform advocates, think tanks) see Help to Buy as temporary policy addressing symptoms rather than root causes (insufficient supply, restrictive planning). The scheme's sunset (2023) reflects recognition that temporary stimulus cannot substitute for structural housing reform. Theater of solution-seeking masks underlying market failure. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.27.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: LEGACY AFFORDABILITY NARRATIVE (PITON) — Institutional inertia: the political claim that Help to Buy 'solves' affordability persists despite empirical evidence that it inflates prices faster than it expands access. Theater_ratio=0.65 reflects substantial performative content: scheme provides visible short-term relief while masking long-term affordability degradation. Academic analysis shows scheme was cost-ineffective vs direct supply-side interventions (planning reform, building cost reduction). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / ECONOMIC FUNDAMENTALS VIEW (MOUNTAIN) — From civilizational/universal perspective, housing affordability crisis is often presented as an immutable supply-demand mismatch or natural consequence of wealth accumulation. However, structural data (ε=0.52, suppression=0.68, theater=0.65) contradicts mountain classification. The housing crisis is not a law of nature but a policy arrangement: restrictive planning law, land value taxation policies, zoning constraints, and developer incentive structures. The schema correctly rejects the 'immutable crisis' framing by failing to meet accessibility_collapse gate (not provided as ≥0.85).
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_help_to_buy_scheme_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_help_to_buy_scheme, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_help_to_buy_scheme, TR),
    TR >= 0.70.

:- end_tests(uk_help_to_buy_scheme_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over interval. Initial assessment (0.28) reflected genuine access expansion — the scheme did enable some first-time buyers to enter homeownership who could not otherwise qualify. But as the scheme scaled (2015-2019) and accumulated volume, its primary effect shifted to demand amplification without corresponding supply response. By 2019-2023, econometric analysis shows 60-75% of marginal Help to Buy buyers would have purchased anyway within 2-3 years, making the scheme primarily a timing accelerator, not access enabler. This acceleration into a supply-constrained market directly drove house price inflation. Final value (0.52) reflects that cumulative extraction (inflated prices, concentration of gains to builders, negative equity risk to buyers) exceeds genuine access expansion. Suppression (0.68): High. Buyers are suppressed through: (1) mortgage lending criteria that require Help to Buy participation to qualify (creates dependency); (2) debt obligation (5% deposit + 20% equity loan + 75% mortgage creates tri-level leverage, amplifying default risk); (3) negative equity trap (if prices stagnate, buyers cannot exit without loss); (4) information asymmetry (builders knew scheme would inflate prices, buyers did not); (5) political theater preventing alternative policies (restricting planning reform, reducing land value capture, reforming building regulations). Theater ratio (0.65): Moderate-high. Scheme is substantially performative: government claims to solve affordability while primary effect is demand-side stimulus that worsens affordability for future cohorts. Public messaging emphasizes 'helping families onto the ladder' while withholding evidence that scheme inflates the ladder's height. Traditional peer review analog would be journal selection of results that show access expansion while suppressing analysis showing price inflation and net harm to cohort outcomes.
 *
 * PERSPECTIVAL GAP:
 *   Extraordinary perspectival divergence. Builders see Rope (coordination: expanded sales, price support, predictable demand). Buyers see Snare (trapped in debt, inflated prices, negative equity risk). Renters see Tangled Rope (harmed by rising prices, excluded from scheme, forced to accept higher rents). Government policy authority sees Tangled Rope (beneficiary through political capital, but constrained by rising fiscal cost and eventual political pressure forcing sunset). The analytical observer risks seeing Mountain (housing crisis as natural supply-demand mismatch) but the structural data reveals this as false naturalization — the crisis is policy-driven (restrictive planning, land taxation, zoning), making the Tangled Rope classification correct. The reform coalition sees Scaffold with sunset — temporary stimulus should be replaced by structural supply-side reform. The legacy affordability narrative sees Piton — the claim that Help to Buy 'solves' affordability persists despite evidence, maintained through institutional inertia and political theater.
 *
 * DIRECTIONALITY LOGIC:
 *   House builders/landowners: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Mortgage lenders: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. First-time buyers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Trapped by affordability crisis and mortgage criteria; cannot exit without foregoing homeownership. Private renters: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction. Constrained by rising rents and prices but have option to emigrate, rent long-term, or pursue alternative assets. House price stability: Victim + trapped → d≈0.93, f(d)≈1.41. Maximum extraction. Abstract collective cannot organize or exit. Government authority: Beneficiary initially (policy success, political capital) but victim as costs accumulate. Override: institutional power atom, but d-value must reflect shift from beneficiary position (early years, d≈0.15) to victim position (late years, d≈0.65) due to fiscal burden and political constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution required (ε=0.52, below 0.70 threshold). However, the constraint demonstrates why mandatrophy analysis matters: Help to Buy could easily be mislabeled as pure Rope ('a coordination mechanism expanding credit access') if one only examined the builders' and lenders' perspectives and ignored the extraction dynamics visible from buyers' and renters' positions. The Tangled Rope classification captures the hybrid correctly: there IS a genuine coordination function (expanding qualified buyer pool), but it IS coupled with asymmetric extraction (price inflation benefits builders more than buyers; harms renters; creates negative equity traps). The 10-year measurement trajectory (extractiveness rising from 0.28 to 0.52, theater rising from 0.45 to 0.65) demonstrates manifest drift: the coordination function was initially more salient; over time, the extraction mechanism became dominant. This is the opposite of the Scaffold trajectory (extraction declining toward sunset). Help to Buy shows extraction increasing until political force imposed the sunset — not because the mechanism changed, but because its cumulative costs became undeniable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_inflation_causality,
    'What proportion of house price inflation (2013-2023) was attributable to Help to Buy stimulus vs other factors (QE, international capital, planning restrictions, demographics)?',
    'Econometric decomposition of price drivers; comparison of price trajectories in scheme-eligible (new-build) vs ineligible (secondary market) segments; regional analysis of scheme impact intensity',
    'If Help to Buy > 40% of inflation: scheme is primarily extractive. If < 20%: scheme coordination function is more significant than initially assessed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_inflation_causality, empirical, 'Quantification of Help to Buy''s causal contribution to house price inflation').

omega_variable(
    counterfactual_affordability_access,
    'Would first-time buyers excluded from Help to Buy have eventually accessed homeownership through alternative means (saving, family support, alternative mortgages) if the scheme had not existed?',
    'Longitudinal cohort analysis; tracking outcomes of marginal scheme users (approved vs barely-rejected applicants); comparison with pre-scheme first-time buyer pathways',
    'If yes (80%+ accessed anyway): scheme primarily redistributes purchasing power from renters to buyers, not expands access. Classification remains Tangled Rope for victims. If no (<30% accessed): scheme is genuine access mechanism, shifting from Snare/Tangled Rope toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_affordability_access, empirical, 'Whether Help to Buy enabled access or merely accelerated purchase timing').

omega_variable(
    government_equity_recovery_rate,
    'What percentage of government equity loans were repaid in full at favorable terms vs written down due to negative equity or strategic default?',
    'Department for Levelling Up Housing & Communities portfolio analysis; audit of loan recovery vs initial principal; comparison with projected recovery rates at scheme inception',
    'If recovery > 85%: scheme was cost-effective financing (coordination-dominant). If recovery < 60%: scheme was subsidy disguised as loan (extraction-dominant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_equity_recovery_rate, empirical, 'Financial recovery rate on government equity loans').

omega_variable(
    new_build_supply_elasticity,
    'Did Help to Buy''s demand stimulus translate into proportional increases in new-build housing supply, or did builders absorb increased demand through price increases?',
    'Time-series analysis of scheme-period housing starts vs pre-scheme trend; comparison of price increases vs unit volume increases in eligible segments',
    'If supply elastic (units increased proportionally): scheme created genuine coordination function. If supply inelastic (mostly prices rose): scheme redistributed from renters to builders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(new_build_supply_elasticity, empirical, 'Elasticity of new-build supply response to Help to Buy stimulus').

omega_variable(
    secondary_market_displacement,
    'To what extent did Help to Buy buyers displace other buyers in the secondary market, vs expanding the total buyer pool?',
    'Matching analysis of Help to Buy users and non-users; trading-down analysis (do non-Help-to-Buy buyers move down-market due to price increases?); cohort survival analysis',
    'If high displacement: scheme is primarily redistribution (Snare characteristics increase). If low displacement: scheme expands genuine access (Rope characteristics increase).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_displacement, empirical, 'Extent of Help to Buy buyers displacing non-scheme participants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_help_to_buy_scheme, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htb_tr_t0, uk_help_to_buy_scheme, theater_ratio, 0, 0.45).
narrative_ontology:measurement(htb_tr_t5, uk_help_to_buy_scheme, theater_ratio, 5, 0.58).
narrative_ontology:measurement(htb_tr_t10, uk_help_to_buy_scheme, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(htb_be_t0, uk_help_to_buy_scheme, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(htb_be_t5, uk_help_to_buy_scheme, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(htb_be_t10, uk_help_to_buy_scheme, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_help_to_buy_scheme, resource_allocation).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, uk_planning_restriction_regime).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, building_regulation_cost_inflation).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, mortgage_lending_criteria_lock).

% DUAL FORMULATION NOTE:
% Help to Buy is downstream of fundamental housing market constraints (planning restrictions, building costs, mortgage lending criteria) but represents a distinct policy constraint with its own extraction/coordination dynamics. The upstream constraints determine why demand-side stimulus was insufficient; Help to Buy's extraction mechanism operates through price amplification in constrained supply markets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_help_to_buy_scheme, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
