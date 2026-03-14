% ============================================================================
% CONSTRAINT STORY: property_value_preservation_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_property_value_preservation_regime, []).

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
 *   constraint_id: property_value_preservation_regime
 *   human_readable: Property Value Preservation Regime
 *   domain: economic/social/political
 *
 * SUMMARY:
 *   The property value preservation regime encompasses the cluster of
 *   policies, zoning restrictions, building quotas, and financial incentives
 *   that stabilize or increase residential property values. Ostensibly
 *   designed to preserve neighborhood character and protect homeowner equity,
 *   the regime functions as a mechanism for wealth concentration and
 *   exclusion. The constraint generates coordination benefits (stable
 *   neighborhoods, predictable neighborhoods, investment security) alongside
 *   severe asymmetric extraction (exclusion of new entrants, wealth transfer
 *   to existing owners). This is a canonical tangled rope: genuine
 *   coordination function exists (neighborhoods require some stability to
 *   provide schools, services, safety), but the coordination mechanism is
 *   layered with extractive mechanisms (artificial scarcity, financial
 *   institution capture, zoning gatekeeping). The regime exhibits drift over
 *   the measurement interval: extractiveness rises from 0.38 to 0.58 as
 *   down-payment requirements, credit barriers, and property costs increase
 *   relative to household income. Theater ratio rises from 0.42 to 0.58 as
 *   the regime's performative elements (design review boards, community input
 *   processes, 'livability' metrics) become more elaborate while supply
 *   response capability remains constrained.
 *
 * KEY AGENTS:
 *   - Existing Property Owners: Primary beneficiaries (institutional/arbitrage, moderate/constrained) — benefit from equity gains and neighborhood stability; captured by the regime but with real options for exit
 *   - First-Time Buyers: Primary victims (powerless/trapped) — excluded by down-payment barriers and competing demand; no realistic path to ownership within biographical horizon
 *   - Renters: Secondary victims (moderate/constrained) — benefit from neighborhood stability but bear suppression costs; can exit through relocation but at high cost
 *   - Financial Institutions: Secondary beneficiaries (institutional/arbitrage) — profit from stable collateral values and mortgage origination; no suppression burden
 *   - Local Government Zoning Apparatus: Institutional maintainer (institutional/arbitrage) — enforces the regime through planning procedures and building restrictions; sees its coordination function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(property_value_preservation_regime, 0.58).
domain_priors:suppression_score(property_value_preservation_regime, 0.65).
domain_priors:theater_ratio(property_value_preservation_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(property_value_preservation_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(property_value_preservation_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(property_value_preservation_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(property_value_preservation_regime, tangled_rope).
narrative_ontology:human_readable(property_value_preservation_regime, "Property Value Preservation Regime").
narrative_ontology:topic_domain(property_value_preservation_regime, "economic/social/political").

domain_priors:requires_active_enforcement(property_value_preservation_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(property_value_preservation_regime, existing_property_owners).
narrative_ontology:constraint_beneficiary(property_value_preservation_regime, real_estate_financial_institutions).
narrative_ontology:constraint_victim(property_value_preservation_regime, prospective_first_time_buyers).
narrative_ontology:constraint_victim(property_value_preservation_regime, renters_seeking_ownership).
narrative_ontology:constraint_victim(property_value_preservation_regime, lower_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED FIRST-TIME BUYER (SNARE) — Trapped by artificially elevated property values sustained through zoning restrictions, building quotas, and speculative dynamics. No realistic path to homeownership within biographical horizon. Suppression is structural: capital requirements, credit barriers, competing demand from institutional investors. Maximum experienced extraction — bear full cost of the regime with no exit option or coordination benefit.
constraint_indexing:constraint_classification(property_value_preservation_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RENTING HOUSEHOLD (TANGLED ROPE) — Constrained by down-payment requirements and competing bidders; also benefits from stable neighborhood conditions (schools, services) that value preservation maintains. Real estate markets provide housing coordination (matching supply to demand) alongside extraction (wealth transfer to existing owners through scarcity rent). Exit possible but costly — relocation, credit damage, opportunity cost. Moderate extraction.
constraint_indexing:constraint_classification(property_value_preservation_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROPERTY OWNER WITH INSTITUTIONAL CAPITAL (ROPE) — Benefits from value preservation mechanisms without bearing suppression costs. Can arbitrage across markets, exit freely through sale, or refinance. Experiences the regime as pure coordination: property value stability enables equity extraction, refinancing, and investment decisions. Negative experienced extraction — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(property_value_preservation_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL INSTITUTION (ROPE) — Banks and mortgage lenders benefit from stable property values (reduces default risk, collateral volatility). Value preservation mechanisms enable mortgage products and lending arbitrage. Can exit entirely through portfolio reallocation. Experiences the regime as enabling coordination with asymmetric benefit — they capture spread and risk reduction without bearing suppression costs.
constraint_indexing:constraint_classification(property_value_preservation_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXISTING HOMEOWNER (TANGLED ROPE) — Benefits from value preservation (equity gains, neighborhood stability) but also constrained by the regime. Cannot freely subdivide, renovate, or develop property (zoning restrictions). Values create intergenerational wealth but also create lock-in (selling means exit from community, schools, networks). Moderate extraction — extraction runs toward financial institutions more than toward this agent, but constraints are real.
constraint_indexing:constraint_classification(property_value_preservation_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ZONING ENFORCEMENT APPARATUS (PITON) — Local government zoning and building codes maintain the value preservation regime through performative enforcement. The original coordination function (preventing incompatible uses, protecting neighborhood character) has largely atrophied; the regime now primarily serves wealth concentration. Maintained through institutional inertia and political capture rather than genuine coordination need. Theater ratio high — elaborate planning procedures, design review boards, neighborhood opposition procedures perform the role of 'careful stewardship' while blocking supply response.
constraint_indexing:constraint_classification(property_value_preservation_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a broad view, property value preservation appears as an immutable consequence of scarcity, geography, and human preference. Values must rise in high-demand areas because land is finite. This perspective naturalizes the regime as inevitable. However, the structural data reveals this as a false summit: extraction through artificial supply restriction (zoning, building caps) is distinguishable from genuine scarcity-driven appreciation. Policy choices create the regime; they are not laws of nature.
constraint_indexing:constraint_classification(property_value_preservation_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(property_value_preservation_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(property_value_preservation_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(property_value_preservation_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(property_value_preservation_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(property_value_preservation_regime, TR),
    TR >= 0.70.

:- end_tests(property_value_preservation_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The regime transfers wealth from new entrants to existing owners through artificial scarcity. Down-payment requirements, competing institutional investor bidders, and credit barriers create suppression proportional to household income, making extraction severe for lower-income agents. The value has increased from 0.38 to 0.58 over 20 years as down-payment ratios rise (from median ~10% to ~15% of property value in many markets) while median household incomes stagnate relative to property prices. This trajectory indicates extraction is accumulating, not stabilizing. Suppression (0.65): High and structural. Barriers to property ownership include capital requirements (down payment), credit score minimums, competing institutional demand, zoning-limited supply, and longer approval timelines. These barriers affect powerless and moderate agents disproportionately. Suppression does not decrease as household wealth increases — it affects the bottom 60% of earners uniformly high. Theater ratio (0.58): Moderate-high. Zoning enforcement includes elaborate design review processes, neighborhood input procedures, environmental assessments, and 'livability' metrics — performative procedures that maintain the appearance of careful stewardship while functioning primarily to restrict supply. The theater has increased as planning bureaucracy has elaborated (from ~0.42 to ~0.58) while supply response has actually declined. Claimed type (Tangled Rope): Requires beneficiaries + victims + active enforcement. The regime coordinates genuine neighborhood public goods (schools, services, safety correlate with stable property ownership). But it accomplishes this coordination through artificial scarcity and wealth transfer rather than through direct public provisioning. The enforcement is active: zoning boards, building review committees, lending standards all actively maintain the regime.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap centers on whether the regime's coordination function (neighborhood stability, public goods) is genuinely integral or post-hoc rationalization. Existing homeowners sincerely experience the stability benefit and coordination value. Powerless renters and first-time buyers see only the extraction: they are excluded from the benefit (cannot own) while bearing the suppression cost (cannot afford). The analytical observer risks conflating genuine coordination (schools require stable tax base, neighborhoods require long-term resident investment) with false natural law (values must rise because land is finite). The gap is diagnostic: if zoning and building restrictions were removed, would neighborhood public goods degrade? If yes, the coordination function is real and the regime is justified as tangled rope with a misaligned extraction mechanism. If no, the coordination is a cover story and the regime is a snare with elaborate theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position: existing property owners with arbitrage options (can sell, refinance, relocate) experience low d (~0.15 derived from institutional + arbitrage + beneficiary). Financial institutions derive similar d (~0.10, full beneficiary with maximum exit). First-time buyers with trapped exit experience high d (~0.92, powerless + trapped + victim). Renting households with constrained exit experience moderate-high d (~0.68, moderate + constrained + victim). The sigmoid f(d) amplifies the difference: high d values produce χ multipliers > 1.0, low d values produce χ multipliers < 0.3. This explains why the powerless agent experiences extraction as maximum (snare) while the institutional beneficiary experiences it as zero or negative (rope). The regime's effective extractiveness χ scales differently for each agent based on their structural position, not based on variation in the regime itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK DETECTED: The regime risks classification as a snare disguised as tangled rope. The coordination function (neighborhood stability, public goods) is real but may not require the extraction mechanism that the regime employs. Alternative mechanisms (direct public provisioning of schools and services, community land trusts, reduced zoning restrictions with alternative stabilization) could provide the same coordination with lower or zero extraction. The extractiveness trajectory (rising from 0.38 to 0.58) suggests the regime is accumulating rent-seeking layers rather than maintaining stable coordination. Mandatrophy resolution: Establish whether removing the regime's supply restriction mechanisms would degrade neighborhood public goods. If neighborhood quality depends structurally on scarcity-driven exclusion, the tangled rope classification holds. If neighborhood quality depends on public investment and long-resident composition (achievable under higher-density scenarios), the regime is a snare with a coordination cover story. The measurement trajectory (rising extractiveness, rising theater) suggests the latter: the regime has shifted from tangled rope (mixed) toward snare (pure extraction) over the 20-year interval. This represents degradation of the coordination function — a drift toward extraction dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_artificial_scarcity,
    'What portion of property value appreciation reflects genuine land scarcity vs. artificial supply restriction through zoning and regulatory barriers?',
    'Comparative analysis: jurisdictions with vs. without restrictive zoning; price elasticity studies; counterfactual modeling of housing supply under reduced-restriction scenarios',
    'If artificial scarcity > 50%: extraction component is dominant, snare/tangled_rope classifications strengthened. If artificial < 30%: regime is closer to natural law, mountain classification gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_scarcity, empirical, 'Natural vs. artificial scarcity component in property value').

omega_variable(
    neighborhood_stability_value,
    'How much of owner benefit comes from value preservation vs. from genuine neighborhood public goods (schools, services, safety)?',
    'Hedonic pricing analysis isolating neighborhood amenities from scarcity rent; measurement of correlation between zoning stringency and amenity quality',
    'If public goods > 40%: coordination function is stronger, tangled_rope framing solidifies. If public goods < 15%: coordination is post-hoc rationalization for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neighborhood_stability_value, empirical, 'Share of owner benefit from public goods vs. scarcity rent').

omega_variable(
    financial_institution_dependency,
    'Does the property value preservation regime depend structurally on financial institution participation, or could alternative financing mechanisms (community land trusts, owner-financed transactions, cooperative models) sustain housing coordination without the regime?',
    'Analysis of successful alternative financing models; mortgage market dynamics without value preservation guarantees; pilot program outcomes',
    'If alternatives viable: financial institution perspective shifts from rope (pure beneficiary) to constrained (could operate differently but profit incentives enforce status quo). Directionality of regime toward this agent becomes more visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_institution_dependency, empirical, 'Structural dependency of regime on financial institution participation').

omega_variable(
    exit_cost_trajectory,
    'Is suppression (down-payment barriers, competition, credit requirements) increasing or decreasing over the measurement interval?',
    'Time-series analysis of median down-payment as % of property value; credit score requirements for approval; first-time buyer share of market',
    'If increasing: snare classification strengthened, regime becoming more extractive over time. If stable: tangled_rope framing holds, extraction not accumulating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_trajectory, empirical, 'Trajectory of suppression (exit barriers) over time').

omega_variable(
    regime_beneficiary_concentration,
    'Has wealth concentration among property owners increased relative to non-owners over the interval?',
    'Gini coefficient analysis of property owner vs. renter wealth; intergenerational wealth transfer patterns; correlation of zoning stringency with wealth inequality',
    'If concentration increased: extraction component of regime is accumulating, mandatrophy risk (disguised snare as tangled rope). If stable: regime is functioning as partial coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_beneficiary_concentration, empirical, 'Wealth concentration trajectory among property owners').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(property_value_preservation_regime, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pvpr_tr_t0, property_value_preservation_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pvpr_tr_t10, property_value_preservation_regime, theater_ratio, 10, 0.5).
narrative_ontology:measurement(pvpr_tr_t20, property_value_preservation_regime, theater_ratio, 20, 0.58).
narrative_ontology:measurement(pvpr_tr_t5, property_value_preservation_regime, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(pvpr_be_t0, property_value_preservation_regime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pvpr_be_t10, property_value_preservation_regime, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pvpr_be_t20, property_value_preservation_regime, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pvpr_be_t5, property_value_preservation_regime, base_extractiveness, 5, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(property_value_preservation_regime, resource_allocation).
narrative_ontology:boltzmann_floor_override(property_value_preservation_regime, 0.18).
narrative_ontology:affects_constraint(property_value_preservation_regime, financial_institution_capital_concentration).
narrative_ontology:affects_constraint(property_value_preservation_regime, intergenerational_wealth_inequality).
narrative_ontology:affects_constraint(property_value_preservation_regime, urban_development_zoning_capture).

% DUAL FORMULATION NOTE:
% The property value preservation regime decomposes into multiple structurally distinct constraints: (1) zoning coordination (genuine neighborhood public goods function, likely rope or tangled rope), (2) financial institution value capture (extraction, snare), (3) intergenerational wealth transfer (accumulation, snare). This constraint story focuses on the aggregate regime as experienced by first-time buyers and renters (snare/tangled rope perspectives dominant). Separate stories can decompose the zoning coordination function from the financial extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(property_value_preservation_regime, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
