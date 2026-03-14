% ============================================================================
% CONSTRAINT STORY: rural_property_speculation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rural_property_speculation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rural_property_speculation
 *   human_readable: Rural Property Speculation and Agricultural Land Consolidation
 *   domain: economic/land_use/agricultural_policy
 *
 * SUMMARY:
 *   Rural property speculation represents a structural extraction mechanism
 *   where external capital concentrates ownership of agricultural land by
 *   exploiting informational asymmetries, capital access differentials, and
 *   weak enforcement of land-use protections. Small farmers face rising
 *   property taxes, debt servicing obligations, and commodity price
 *   volatility that force asset liquidation at below-true-value prices.
 *   Investment funds and development corporations acquire this land at
 *   discount, holding it for appreciation as population pressure and
 *   urbanization increase regional land values. The constraint exhibits
 *   genuine coordination (market price discovery for land transactions)
 *   alongside asymmetric extraction (informed outsiders extracting surplus
 *   from uninformed locals). Rural communities experience this as a
 *   generational loss of agricultural capacity and social resilience, but
 *   lack the capital or political power to reverse consolidation. Land reform
 *   mechanisms exist but face capacity and political constraints. The theater
 *   ratio has declined over time as deregulation has reduced enforcement of
 *   land-use restrictions, making the extraction mechanism more direct and
 *   less dependent on performative regulation.
 *
 * KEY AGENTS:
 *   - Small Farmers: Primary victims (powerless/trapped) — forced to sell land due to debt, taxes, and lack of alternative income; cannot afford to compete with external capital; lose livelihood and identity
 *   - External Investors/Investment Funds: Primary beneficiaries (institutional/arbitrage) — acquire undervalued land, hold for appreciation, exit at profit; experience constraint as pure market coordination with no coercion felt
 *   - Rural Communities: Secondary victims (moderate/constrained) — face knowledge asymmetries about true land value, limited capital for competitive bidding, loss of community control; constrained by resource and political limitations
 *   - Financial Intermediaries: Secondary beneficiaries (institutional/arbitrage) — provide financing that enables investor acquisition; extract fees and interest; benefit from land price inflation
 *   - Land Development Corporations: Secondary beneficiaries (institutional/arbitrage) — convert agricultural land to urban/commercial use; capture value from zoning and development rights; benefit from regulatory capture
 *   - Agricultural Ministry/Regulators: Institutional actors (institutional/constrained) — formally maintain land-use protections but enforcement capacity has atrophied; theater ratio declined as regulations become performative
 *   - Land Reform Coalition: Organized actors (organized/constrained) — cooperatives, NGOs, agrarian movements building alternatives (community land trusts, cooperative purchase); facing suppression through capital scarcity and political opposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rural_property_speculation, 0.58).
domain_priors:suppression_score(rural_property_speculation, 0.52).
domain_priors:theater_ratio(rural_property_speculation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rural_property_speculation, extractiveness, 0.58).
narrative_ontology:constraint_metric(rural_property_speculation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rural_property_speculation, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rural_property_speculation, tangled_rope).
narrative_ontology:human_readable(rural_property_speculation, "Rural Property Speculation and Agricultural Land Consolidation").
narrative_ontology:topic_domain(rural_property_speculation, "economic/land_use/agricultural_policy").

domain_priors:requires_active_enforcement(rural_property_speculation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rural_property_speculation, external_investors).
narrative_ontology:constraint_beneficiary(rural_property_speculation, financial_intermediaries).
narrative_ontology:constraint_beneficiary(rural_property_speculation, land_development_corporations).
narrative_ontology:constraint_victim(rural_property_speculation, small_farmers).
narrative_ontology:constraint_victim(rural_property_speculation, rural_communities).
narrative_ontology:constraint_victim(rural_property_speculation, future_food_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL FARMER (SNARE) — Trapped by debt dependency, rising property taxes, and lack of alternative livelihoods. Cannot exit without losing land and identity. Experiences full extraction through forced sale at below-market rates when unable to service debt or taxes. No coordination benefit — the constraint serves only to transfer wealth upward.
constraint_indexing:constraint_classification(rural_property_speculation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL COMMUNITY (TANGLED ROPE) — Constrained by limited capital access, knowledge asymmetries about land value, and dependence on agricultural income. Experiences both coordination (market enables land transactions, price discovery) and asymmetric extraction (outsiders with superior information and capital extract surplus). Has some agency through collective action but faces high coordination costs.
constraint_indexing:constraint_classification(rural_property_speculation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INVESTMENT FUND (ROPE) — Primary beneficiary with maximal exit optionality. Experiences the constraint as pure coordination: acquire undervalued land, hold, exit at profit. Land as asset class merely requires market mechanism. No coercion needed from fund's perspective — mechanism is self-interested coordination among rational economic actors.
constraint_indexing:constraint_classification(rural_property_speculation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AGRICULTURAL MINISTRY (PITON) — Maintains formal land-use protections and agricultural support programs, but enforcement capacity has atrophied. Theater ratio (0.38) is moderate-low because regulatory framework still exists and is ostensibly enforced, but the constraint operates despite this — the ministry's primary function (protecting agricultural land) has degraded while bureaucratic apparatus persists. Policies exist; compliance is uncertain.
constraint_indexing:constraint_classification(rural_property_speculation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LAND REFORM COALITION (SCAFFOLD) — Organized agents (cooperatives, NGOs, agrarian movements) perceive the constraint as a temporary coordination failure with a potential sunset: community land trusts, purchase-and-grant schemes, and cooperative purchase agreements create alternative pathways that reduce extraction by replacing speculative acquisition with stewardship incentives. Suppression is high but declining as coalitional power grows.
constraint_indexing:constraint_classification(rural_property_speculation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET RATIONALITY (MOUNTAIN) — From a civilizational/universal perspective, land price appreciation driven by supply scarcity and capital flows is treated as a natural market equilibrium — an immutable outcome of rational agents allocating scarce resources. This perspective risks naturalizing what are actually political choices: agricultural land policy, capital gains taxation, foreign ownership restrictions, and speculation taxes are all contingent policy levers, not laws of nature. The mountain classification reveals false naturalization.
constraint_indexing:constraint_classification(rural_property_speculation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rural_property_speculation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rural_property_speculation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rural_property_speculation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rural_property_speculation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rural_property_speculation, TR),
    TR >= 0.70.

:- end_tests(rural_property_speculation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting that external capital systematically acquires undervalued assets from information-disadvantaged sellers, but the mechanism requires willing participation (land sales are legal transactions, not theft). The extraction is embedded in market asymmetries, not pure coercion. The trajectory shows rising extractiveness over the interval as regulatory frameworks have weakened and speculative capital has become more aggressive, increasing the price gap between what locals can afford and what investors offer. Suppression (0.52): Moderate-high. Barriers to farmer exit include: (1) debt dependency trapping landholdings as collateral, (2) limited alternative income sources in rural economies, (3) information asymmetries about true land value, (4) weak enforcement of land-use protections and anti-speculation measures, (5) cultural attachment to ancestral land making psychological exit costly. These are not total barriers — some farmers retain land and some organize to resist — but most face substantial suppression. Theater ratio (0.38): Moderate-low. Unlike many extractive constraints, rural property speculation relies less on performative legitimation and more on market rationality framing ('prices reflect scarcity and capital flows'). Regulatory theater has declined over the interval as land-use controls were relaxed, making extraction more naked. The theater that remains is ideological (naturalizing market outcomes as inevitable) rather than institutional (maintaining degraded bureaucratic forms).
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap reflects the fundamental difference between market efficiency (the investor's frame) and extraction asymmetry (the farmer's frame). From the investor's perspective, the constraint solves a real coordination problem: how to allocate scarce agricultural land to agents with capital and intent to develop it. Land is efficiently transferred from those unable to maximize its value to those who can. This is Rope — pure coordination, minimal coercion, mutual benefit (the farmer gets liquidity; the land gets development). From the small farmer's perspective, the same mechanism is extraction: they sell not because they chose to but because they were forced by debt, tax, and commodity price shocks. They received below-market value for their land. They lost livelihood and identity. This is Snare — pure extraction, high suppression, no compensation, no exit. Both perspectives are structurally accurate. The constraint IS a coordination mechanism (market transaction) AND an extraction mechanism (asymmetric surplus transfer). The gap reveals that Rope and Snare are not absolute categories but relational ones — the same institutional mechanism can be Rope for beneficiaries and Snare for victims. The perspectival divergence is diagnostic evidence that the constraint is actually Tangled Rope (mixed coordination and extraction), not pure of either kind.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values track the flow of extraction. External investors (d ≈ 0.10) experience negative effective extraction (they extract FROM the system, not toward them). Small farmers (d ≈ 0.93) experience positive extraction (the system extracts FROM them). The sigmoid function f(d) translates these structural positions into power multipliers. The farmer's trapped status and victim position produce high d → high f(d) ≈ 1.42, amplifying their experienced extractiveness. The investor's arbitrage status and beneficiary position produce low d → low f(d) ≈ -0.12, dampening their experienced extraction (they benefit). Scope modifier σ(regional) = 0.90 applies to effective extraction but not to suppression — rural property speculation's suppression (0.52) is structural and unscaled. The chi formula produces the perspectival divergence: the same ε (0.58) yields very different χ values depending on agent position. Beneficiary investors experience χ ≈ 0.58 × (-0.12) × 0.90 ≈ -0.063 (they perceive the constraint as beneficial Rope). Trapped farmers experience χ ≈ 0.58 × 1.42 × 0.90 ≈ 0.741 (they perceive the constraint as high-extraction Snare). The numerical gap (from negative to high-positive) explains the perspectival chasm.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how Tangled Rope is the correct classification when a genuine coordination function (market price discovery for land) is irreducibly coupled with asymmetric extraction (informed outsiders extracting surplus from uninformed locals). The false summit (Mountain via market rationality) claims that land price appreciation is a natural equilibrium with no extractive component. The structural data contradicts this: small farmers are victims, external investors are beneficiaries, and there is active enforcement (land sales law, debt servicing, tax collection). These three facts (beneficiary, victim, enforcement) are required by the Tangled Rope gate and absent from pure Rope. The constraint cannot be Rope-only because it produces clear victims who bear costs without equivalent benefit. It cannot be Snare-only because market transactions create genuine coordination value — the land is traded to agents with intent and capital to use it, and this allocation may produce socially valuable outcomes. It is Tangled Rope: both mechanisms operate, neither dominates, both must be recognized. The mandatrophy prevention emerges from the dual perspective structure: beneficiary sees Rope, victim sees Snare, analytical observer sees Tangled Rope, land reform coalition sees sunset (Scaffold). No single type captures the structure; the presheaf over all perspectives IS the accurate characterization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_vs_productive_intent,
    'How do we empirically distinguish speculative land acquisition (extraction) from investment in agricultural productivity (coordination)?',
    'Longitudinal tracking of land use: improvements, soil investment, working-capital deployment, employment generation on acquired parcels vs. holding for resale. Cross-referencing investor portfolio diversity (agricultural focus vs. diversified speculation).',
    'If distinction is reliable: extractiveness may be lower than 0.58 for investors with genuine productivity intent, requiring classification revision. If indistinguishable: the market mechanism conflates both intents, masking extraction as investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_vs_productive_intent, empirical, 'Distinguishing speculative from productive land acquisition').

omega_variable(
    debt_trap_mechanism,
    'Is farmer indebtedness driven by agricultural commodity price volatility (external shock) or by land-debt escalation engineered by speculation-driven price inflation?',
    'Time-series analysis of commodity prices vs. land price inflation; comparison of debt-to-income ratios in high-speculation regions vs. low-speculation control regions; structural examination of credit terms offered to farmers in speculative zones.',
    'If external shock dominates: suppression is structural but not engineered — classification shifts toward Rope (coordination failure). If debt-trap mechanism dominates: suppression is deliberately maintained — classification confirmed as Snare/Tangled Rope (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_trap_mechanism, empirical, 'Whether farmer debt is exogenous shock or engineered extraction').

omega_variable(
    capital_inflow_inevitability,
    'Is the concentration of land ownership in external investor hands an inevitable outcome of market efficiency or a preventable outcome of policy choices?',
    'Comparative institutional analysis: jurisdictions with active land-use restrictions, community purchase programs, and capital gains taxation show different ownership concentration patterns than deregulated markets. Historical counterfactuals where policy interventions altered trajectories.',
    'If inevitable: mountain classification for capital flows is legitimate — policy can only redirect, not prevent. If preventable: mountain classification is false naturalization — the constraint is a Tangled Rope with policy-determined suppression levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_inflow_inevitability, conceptual, 'Whether land concentration is inevitable or policy-contingent').

omega_variable(
    food_security_causality,
    'Does rural land consolidation and loss of small-farm productivity directly threaten food security, or is the linkage overstated?',
    'Longitudinal agricultural productivity analysis post-consolidation; yield per hectare comparison before/after ownership change; supply chain resilience assessment; food import dependency trends correlated with land concentration.',
    'If direct causality confirmed: victim status of ''future_food_security'' is justified, strengthening snare classification. If linkage is weak: victim designation is rhetorical, not structural, requiring reclassification toward pure Rope (market coordination without true victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(food_security_causality, empirical, 'Whether land consolidation directly threatens food security').

omega_variable(
    reform_sunset_realism,
    'Are land reform and community purchase mechanisms a genuine structural sunset for speculation-driven extraction, or aspirational policy theater without enforcement capacity?',
    'Longitudinal tracking of community land trust acquisition rates, purchase subsidy utilization, land return to smallholder management; comparison of policy ambition vs. actual capital deployed; analysis of whether reform mechanisms reverse ownership concentration or merely slow it.',
    'If sunset is real: scaffold classification confirmed — extraction mechanism is genuinely declining. If theater: scaffold is misclassified — should reclassify as Piton (degraded reform institutions maintaining appearance without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sunset_realism, empirical, 'Whether land reform mechanisms provide genuine sunset or theatrical policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rural_property_speculation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rurspec_tr_t0, rural_property_speculation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rurspec_tr_t5, rural_property_speculation, theater_ratio, 5, 0.45).
narrative_ontology:measurement(rurspec_tr_t10, rural_property_speculation, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(rurspec_be_t0, rural_property_speculation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rurspec_be_t5, rural_property_speculation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rurspec_be_t10, rural_property_speculation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rural_property_speculation, resource_allocation).
narrative_ontology:boltzmann_floor_override(rural_property_speculation, 0.12).
narrative_ontology:affects_constraint(rural_property_speculation, agricultural_commodity_price_volatility).
narrative_ontology:affects_constraint(rural_property_speculation, rural_credit_market_extraction).
narrative_ontology:affects_constraint(rural_property_speculation, land_tenure_insecurity).

% DUAL FORMULATION NOTE:
% Rural property speculation is downstream of commodity price shocks and rural credit extraction but represents a distinct constraint with its own ε. Upstream constraints (commodity volatility, credit market extraction) force farmer asset liquidation; the speculation constraint governs who captures the liquidated assets and at what terms. Constraint family: upstream shocks → forced farmer distress → downstream investor acquisition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rural_property_speculation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
