% ============================================================================
% CONSTRAINT STORY: budgetary_commons_tragedy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_budgetary_commons_tragedy, []).

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
 *   constraint_id: budgetary_commons_tragedy
 *   human_readable: Budgetary Commons Tragedy: Overallocation and Deficit Accumulation
 *   domain: fiscal_policy/institutional_economics
 *
 * SUMMARY:
 *   The budgetary commons tragedy emerges when political institutions face
 *   conflicting incentives: current voters demand services and oppose taxes,
 *   while costs of deficit financing are displaced to future periods and
 *   diffused across large populations. The constraint operates as a hybrid
 *   coordination-extraction mechanism. Organized constituencies coordinate
 *   around preserving their budget allocations, creating a Rope from their
 *   perspective. Unorganized taxpayers face a mixed coordination-extraction
 *   dynamic: they benefit from current spending but bear costs through
 *   inflation and future austerity. Future generations are structurally
 *   excluded from current budget negotiation and bear extraction through
 *   reduced fiscal capacity. The analytical observer risks naturalizing this
 *   as immutable democratic incentive structure (Mountain), but the data
 *   reveals a tangled institutional arrangement where organized beneficiaries
 *   actively enforce deficit spending against fiscal norms that have degraded
 *   to theatrical performance. The constraint's extractiveness has
 *   accumulated over the interval (0.28 → 0.52) as debt service consumes
 *   increasing budget shares, and theater_ratio has risen (0.32 → 0.58) as
 *   balanced-budget rules are consistently suspended through creative
 *   accounting and procedural exceptions.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped/generational) — structurally excluded from budget negotiation; inherit reduced fiscal capacity and austerity obligations
 *   - Non-Organized Taxpayers: Secondary victim (moderate/constrained/biographical) — benefit from current spending but bear distributed costs through inflation, crowding-out, and future obligations; face collective action problem in opposing deficits
 *   - Politically Organized Constituencies: Primary beneficiary (organized/arbitrage/biographical) — agriculture, defense, public sector unions, entitlement recipients; coordinate to preserve allocations; have exit options through electoral switching
 *   - Legislative Budget Authority: Inter-institutional actor (institutional/constrained/immediate) — must coordinate conflicting demands; institutional reputation and autonomy degrade as debt accumulates; constrained by electoral cycles
 *   - Executive Fiscal Authority: Secondary beneficiary (institutional/arbitrage/immediate) — executes spending; benefits from avoiding politically costly cuts; can reorder priorities; aligned with current-period incentives
 *   - Fiscal Sustainability Norms: Institutional constraint (institutional/constrained/civilizational) — balanced-budget rules, debt ceilings exist but function theatrically; enforcement has deteriorated through decades of violation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(budgetary_commons_tragedy, 0.52).
domain_priors:suppression_score(budgetary_commons_tragedy, 0.65).
domain_priors:theater_ratio(budgetary_commons_tragedy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(budgetary_commons_tragedy, extractiveness, 0.52).
narrative_ontology:constraint_metric(budgetary_commons_tragedy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(budgetary_commons_tragedy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(budgetary_commons_tragedy, tangled_rope).
narrative_ontology:human_readable(budgetary_commons_tragedy, "Budgetary Commons Tragedy: Overallocation and Deficit Accumulation").
narrative_ontology:topic_domain(budgetary_commons_tragedy, "fiscal_policy/institutional_economics").

domain_priors:requires_active_enforcement(budgetary_commons_tragedy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(budgetary_commons_tragedy, current_budget_claimants).
narrative_ontology:constraint_beneficiary(budgetary_commons_tragedy, politically_organized_constituencies).
narrative_ontology:constraint_victim(budgetary_commons_tragedy, future_fiscal_capacity).
narrative_ontology:constraint_victim(budgetary_commons_tragedy, intergenerational_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Structurally unable to participate in current budget negotiations; inherit constrained fiscal capacity and servicing obligations. Bear extraction through reduced public investment, higher taxes, and austerity measures they cannot escape. No exit options; maximum experienced extraction.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-ORGANIZED TAXPAYERS (TANGLED ROPE) — Benefit from current public services (roads, education, defense) funded through deficit spending, but bear the cost through inflation, crowding-out of productive investment, and future tax obligations. Constrained exit — relocation costly but possible; participation in budget process is collective action problem. Mixed extraction and coordination.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICALLY ORGANIZED CONSTITUENCIES (ROPE) — Agricultural interests, defense contractors, public sector unions, entitlement recipient groups. Coordinate around preserving or expanding their budget allocations. Experience the constraint as pure coordination: securing funding commitments through organized pressure. Arbitrage options — can shift support to opposing party if promises not kept. Net beneficiaries during allocation cycles.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE BUDGET AUTHORITY (TANGLED ROPE) — Must coordinate across constituencies with conflicting demands (spend on X, spend on Y, cut taxes). Also bears extraction: institutional reputation suffers during debt crises, partisan gridlock degrades institutional capacity, and external constraints (bond markets, ratings agencies) limit future autonomy. Constrained exit — cannot easily delegate authority without losing legitimacy. Active enforcement required to sustain spending patterns; enforcement deteriorates as debt accumulates.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE FISCAL AUTHORITY (ROPE) — Coordinates tax collection and spending execution; has arbitrage options (reordering spending priorities, negotiating with bond markets, adjusting regulatory interpretation). Short-term incentive alignment with current political cycle. Experiences the constraint as coordination problem solved by deficit spending: avoids politically costly tax increases or spending cuts in election years. Net beneficiary relative to legislative authority.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FISCAL SUSTAINABILITY NORMS (PITON) — Balanced-budget rules, debt ceilings, fiscal targets exist in many jurisdictions but have become largely performative. Theater includes temporary suspensions, creative accounting, off-budget financing, and procedural exceptions. The norms persist through institutional inertia despite decades of consistent violation. Functional constraint has atrophied; theatrical enforcement remains. Theater ratio reflects the gap between stated fiscal rules and actual spending behavior.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, budget commons tragedy appears as immutable: any political system where voters benefit from spending but diffuse costs across future periods will structurally tend toward deficits. This perspective sees the constraint as arising from unavoidable voter incentive asymmetry and information gaps — a natural law of democratic political economy. However, the base properties contradict this: organized beneficiaries actively enforce the constraint; suppression is high but not unchangeable; theater ratio shows norms have degraded. The mountain classification reveals where analytical naturalization masks contingent institutional design.
constraint_indexing:constraint_classification(budgetary_commons_tragedy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(budgetary_commons_tragedy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(budgetary_commons_tragedy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(budgetary_commons_tragedy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(budgetary_commons_tragedy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(budgetary_commons_tragedy, TR),
    TR >= 0.70.

:- end_tests(budgetary_commons_tragedy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising trajectory. The constraint extracts from future fiscal capacity (forced austerity, reduced investment) and from unorganized taxpayers (inflation tax, reduced purchasing power). The extractiveness is not static — it accumulates as debt service grows and crowds out discretionary spending. Initial extractiveness was lower (0.28) when debt ratios were manageable; current trajectory toward 0.52+ reflects that the burden is becoming binding. Suppression (0.65): High. Barriers to exit include collective action problems (individual taxpayer cannot escape deficit consequences), information barriers (future costs are abstract and temporally distant), institutional design (citizens cannot directly control budget rules), and political equilibrium (organized beneficiaries prevent deficit reduction). Exit for some agents is possible (migration to lower-debt jurisdictions) but costly and asymmetric. Theater ratio (0.58): Moderate-high, rising. Fiscal rules (balanced-budget amendments, debt ceilings) exist but are consistently suspended, reinterpreted, or circumvented through off-budget financing. The performative content has increased as the gap between rules and behavior has widened. Claimed type (Tangled Rope): Requires beneficiaries, victims, and active enforcement. All three conditions are met: organized constituencies benefit, future generations and unorganized taxpayers bear costs, and political institutions actively enforce the deficit through legislative choices and suspension of fiscal rules.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the structural tension between current-period beneficiaries and future-period victims. Organized constituencies perceive Rope (pure coordination of budget claims) because they control the legislative process and see their preferences reflected in allocations. Legislative authority perceives Tangled Rope (mixed coordination and constraint) because they face genuine coordination problems across constituencies but also suffer reputational and autonomy damage. Unorganized taxpayers perceive Tangled Rope (mixed benefit from spending and extraction through inflation) but cannot articulate this politically. Future generations perceive Snare (pure extraction with no exit) because they cannot negotiate and inherit constrained fiscal space. Fiscal norms perceive themselves as Piton (their enforcement function has degraded to theater). The analytical observer risks Mountain (naturalizing as inescapable democratic incentive structure) but the structural data reveals this as false naturalization: the constraint is actively enforced by organized political actors, not inherent to democracy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from structural relationships. Organized constituencies have arbitrage options (electoral switching, regulatory influence) and benefit from budget allocations, deriving low d → negative χ (they experience rope, net beneficiary). Legislative authority is constrained by electoral cycles and faces reputational damage, deriving moderate d → moderate χ. Unorganized taxpayers are victims with constrained exit (high cost but possible relocation), deriving high d → higher χ. Future generations are trapped victims with zero exit options, deriving maximum d → maximum χ. The analytical observer at civilizational scope risks naturalizing the constraint (deriving d from the 'natural' incentive asymmetry), but this masks that the constraint is actively enforced by political institutions with choices. The engine's false summit detector should flag the mountain perspective as naturalization of a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The budgetary commons tragedy resolves mandatrophy through the tangled_rope classification, which captures the genuine coordination problem (citizens demand current spending and oppose current taxes) alongside extractive asymmetry (costs displaced to future periods). The snare perspective on future generations is structurally correct — they face pure extraction with no escape. The rope perspective on organized constituencies is also correct — they experience pure coordination and net benefit. The constraint is NOT a snare masquerading as rope (which would be mandatrophy violation) — it IS both snare and rope from different structural positions. The piton perspective on fiscal norms reveals why the constraint persists: the norms that could enforce sustainability have degraded to theater, their functional capacity atrophied through consistent violation. This is correct piton structure, not failed enforcement. The mountain perspective represents the deepest analytical risk: naturalizing the constraint as 'inherent to democracy' masks that it is actively enforced by political institutions through legislative choices, budget priorities, and suspension of fiscal rules. The institutional design could be restructured (mandatory savings accounts, revenue-binding rules, intergenerational trusts) to alter the constraint's structure, but such restructuring is politically difficult because it reduces current beneficiaries' extraction options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate,
    'What discount rate should apply to costs borne by future generations? Does economic growth justify current extraction of future fiscal capacity?',
    'Empirical measurement of long-term growth trajectories under high-debt scenarios; historical analysis of debt overhang effects; cross-national fiscal outcome correlations',
    'Low discount rate: current spending is extractive. High discount rate: deficit spending is legitimate coordination incentive. Different discount rates produce different mandatrophy resolutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'Ethical weighting of intergenerational costs vs current benefits').

omega_variable(
    monetary_financing_option_existence,
    'Does central bank monetary financing of deficits (quantitative easing, digital currency) fundamentally alter the constraint structure, converting snare-victim dynamics into purely distributional politics?',
    'Theoretical analysis of modern monetary theory; empirical measurement of inflation and wealth redistribution under different QE regimes; long-term fiscal sustainability under continuous monetary accommodation',
    'If financing always available: constraint becomes pure extraction (beneficiaries extract through inflation tax). If financing constrained: constraint remains snare with hard limits. Classification changes from tangled_rope to snare if this resolves affirmatively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_financing_option_existence, empirical, 'Whether unlimited monetary accommodation removes the hard constraint').

omega_variable(
    structural_revenue_insufficiency,
    'Are deficits structurally inevitable given that tax bases (labor, capital) have declined as share of income while benefit programs (aging populations) have expanded as share of demand, independent of political choice?',
    'Demographic projection analysis; historical tax base trends; long-term revenue/expenditure ratio modeling; cross-national comparison of revenue systems with stable deficits vs sustainable structures',
    'If structurally inevitable: deficits reflect demographic/economic change, not political extraction. Constraint reclassifies from snare/tangled_rope toward mountain (inescapable feature). If politically contingent: current classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_revenue_insufficiency, empirical, 'Whether deficits are structurally determined by demographics and tax base erosion').

omega_variable(
    organized_beneficiary_dominance,
    'To what extent does the budget commons tragedy represent extraction by organized minorities (defense, agriculture, public sector unions) versus diffuse democratic preference for current spending over future taxation?',
    'Comparison of budget allocations in high-organization vs low-organization districts; analysis of voting patterns on taxes vs spending; measurement of concentrated vs diffuse budget beneficiaries',
    'If dominated by organized minorities: constraint is snare on unorganized majority, reclassify powerless perspective as organized coalition. If democratic preference dominates: tangled_rope classification stands (genuine coordination failure, not extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_beneficiary_dominance, empirical, 'Whether deficits reflect organized minority extraction or democratic preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(budgetary_commons_tragedy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(budget_tr_t0, budgetary_commons_tragedy, theater_ratio, 0, 0.32).
narrative_ontology:measurement(budget_tr_t10, budgetary_commons_tragedy, theater_ratio, 10, 0.48).
narrative_ontology:measurement(budget_tr_t20, budgetary_commons_tragedy, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(budget_be_t0, budgetary_commons_tragedy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(budget_be_t10, budgetary_commons_tragedy, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(budget_be_t20, budgetary_commons_tragedy, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(budgetary_commons_tragedy, resource_allocation).
narrative_ontology:affects_constraint(budgetary_commons_tragedy, monetary_financing_inflation_tax).
narrative_ontology:affects_constraint(budgetary_commons_tragedy, demographic_entitlement_demand).
narrative_ontology:affects_constraint(budgetary_commons_tragedy, tax_base_erosion_structural).

% DUAL FORMULATION NOTE:
% The budgetary commons tragedy decomposes into three structurally distinct constraints with different ε values: (1) fiscal coordination failure (ε=0.35, coordination-driven tangled_rope) — political negotiation over current spending/taxing, (2) inflation tax extraction (ε=0.48, monetary-driven snare) — future generations bear cost through purchasing power loss, (3) structural revenue insufficiency (ε=0.42, demographic-driven tangled_rope) — aging and eroding tax bases create inescapable deficits independent of political choice. Each has different resolution mechanisms and different mandatrophy risks. The current story focuses on (1), the political coordination failure. Upstream constraints (2) and (3) create structural preconditions that make (1) more extractive and harder to resolve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(budgetary_commons_tragedy, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
