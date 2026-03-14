% ============================================================================
% CONSTRAINT STORY: global_supply_chain_restructuring_2020s
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_supply_chain_restructuring_2020s, []).

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
 *   constraint_id: global_supply_chain_restructuring_2020s
 *   human_readable: Global Supply Chain Restructuring of the 2020s
 *   domain: economic/political/infrastructure
 *
 * SUMMARY:
 *   The global supply chain restructuring of the 2020s represents a
 *   fundamental reorganization of manufacturing location and control driven
 *   by geopolitical fragmentation (US-China decoupling), pandemic
 *   vulnerabilities exposed in 2020-2021, and political pressure for
 *   nearshoring/onshoring in developed markets. The restructuring appears
 *   simultaneously as a coordination mechanism (enabling resilient, shorter
 *   supply chains), an extraction apparatus (concentrating control in
 *   consolidated manufacturers and developed-market governments), a temporary
 *   policy intervention (explicit sunset in re-industrialization subsidies),
 *   and degraded legacy institutions (WTO multilateral trade regime losing
 *   functional authority). Each structural position experiences the
 *   constraint differently: powerless suppliers in developing economies face
 *   snare-like dynamics; middle-income manufacturing hubs navigate tangled
 *   rope dynamics; consolidated manufacturers see coordination benefits;
 *   organized policy initiatives embed sunset logic; and the analytical
 *   observer must resist naturalizing what is clearly a political choice as
 *   physical law.
 *
 * KEY AGENTS:
 *   - Powerless Suppliers: Small manufacturers in Bangladesh, Vietnam, Cambodia (powerless/trapped) — targeted for delisting from supply chains, facing margin compression and investment mandates with no capital access
 *   - Middle-Income Manufacturing Hubs: Mexico, Vietnam, Indonesia, Morocco (moderate/constrained) — face pressure to absorb production through capex investment while managing debt and currency exposure; benefit from increased developed-market demand but constrained by existing infrastructure
 *   - Consolidated Manufacturers: Apple, Intel, Bosch, Samsung (institutional/arbitrage) — architect restructuring to reduce geopolitical exposure, lower transportation costs, and increase tariff arbitrage; high exit optionality
 *   - Developed-Market Governments: US, EU, Japan (institutional/arbitrage) — implement nearshoring subsidies and tariff incentives; experience restructuring as coordination enabling strategic autonomy
 *   - Regional Trade Blocs: ASEAN, MERCOSUR, USMCA (organized/constrained) — negotiate restructuring terms but constrained by asymmetric dependence on developed-market access
 *   - Re-Industrialization Policy Institutions: CHIPS Act, EU CMO, India PLI (organized/constrained) — coordinate temporary subsidy mechanisms with explicit sunset clauses; organized enough to deploy capital but constrained by sunset enforcement logistics
 *   - Supply Chain Coordination Bodies: SMIC Alliance, Global Semiconductor Alliance (institutional/identity_locked) — maintain fiction of neutrality while serving developed-market interests; identity-locked to 'multi-stakeholder governance' narrative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing geopolitical choices as immutable manufacturing law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_supply_chain_restructuring_2020s, 0.58).
domain_priors:suppression_score(global_supply_chain_restructuring_2020s, 0.62).
domain_priors:theater_ratio(global_supply_chain_restructuring_2020s, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_supply_chain_restructuring_2020s, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_supply_chain_restructuring_2020s, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_supply_chain_restructuring_2020s, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_supply_chain_restructuring_2020s, tangled_rope).
narrative_ontology:human_readable(global_supply_chain_restructuring_2020s, "Global Supply Chain Restructuring of the 2020s").
narrative_ontology:topic_domain(global_supply_chain_restructuring_2020s, "economic/political/infrastructure").

domain_priors:requires_active_enforcement(global_supply_chain_restructuring_2020s).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_supply_chain_restructuring_2020s, consolidated_manufacturers).
narrative_ontology:constraint_beneficiary(global_supply_chain_restructuring_2020s, developed_market_governments).
narrative_ontology:constraint_beneficiary(global_supply_chain_restructuring_2020s, logistics_monopolies).
narrative_ontology:constraint_victim(global_supply_chain_restructuring_2020s, small_suppliers).
narrative_ontology:constraint_victim(global_supply_chain_restructuring_2020s, developing_economies).
narrative_ontology:constraint_victim(global_supply_chain_restructuring_2020s, supply_chain_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Small manufacturers in developing economies face severe extraction through onshoring/nearshoring pressure. Trapped by economic dependency and lack of capital mobility. Cannot exit supply chains without economic collapse — retaliation through delisting. Maximum experienced extraction with no alternatives.
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Mexico, Vietnam, Indonesia face restructuring pressure: lose low-cost production to automation in developed markets, or transition to higher-value manufacturing requiring capital investment. Constrained by existing infrastructure lock-in and debt. Experience both coordination benefit (access to developed market contracts) and extraction (margin compression, forced reinvestment, technology transfer demands).
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Apple, Intel, Bosch experience restructuring as coordination mechanism enabling nearshoring strategy. Benefits from reduced geopolitical risk, lower transportation costs, and increased tariff arbitrage. High exit optionality — can shift production or suppress suppliers. Low experienced extraction; net beneficiary.
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% ASEAN, MERCOSUR, USMCA experience restructuring as both coordination (enabling regional value chains) and extraction (asymmetric tariff asymmetries, tech transfer requirements from dominant trading partners). Organized enough to negotiate but constrained by dependence on developed market access. Perspectival gap between small suppliers (snare) and bloc institutional actors (tangled rope).
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% CHIPS Act, EU Critical Raw Materials Act, India PLI scheme represent organized response to supply chain fragility with explicit sunset logic: temporary subsidies and tax breaks (2023-2035) to build domestic capacity and manufacturing resilience. Sunset mechanisms embedded in legislation. Theater ratio moderate (performative commitment announcements) but real structural goal (capacity building). Organized actors see this as temporary coordinate to solve a coordination problem, not permanent extraction.
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The post-1995 multilateral trade regime persists through institutional inertia despite functional decay. MFN status and tariff bindings are increasingly circumvented through non-tariff barriers, environmental/labor standards enforcement, and state-owned enterprise carve-outs. The regime maintains performance theater (USTR reviews, WTO dispute settlements) but has lost primary coordination function — state actors prioritize strategic autonomy over liberalization. Piton classification: high theater (procedural compliance), low functional extraction (because the regime is no longer the mechanism actually allocating supply chains).
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organizations like SMIC Alliance, Global Semiconductor Alliance coordinate industry logistics and standard-setting. Institutionally powerful but identity-locked: their legitimacy depends on maintaining the fiction of neutrality and multi-stakeholder governance while serving as vehicles for developed-market interests (standardization favoring incumbent manufacturers, regulatory capture through technical committee participation). Would dissolve if they acknowledged the asymmetry — maintained through performative inclusion of small suppliers and developing economies. This is institutional capture through identity fusion: the organization's self-concept as 'neutral convener' prevents acknowledging its structural role as extraction mechanism.
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(continental))).

% From extreme analytical distance (civilizational/global/analytical), supply chain restructuring might appear as an immutable feature of global manufacturing: specialization requires coordination, and coordination creates efficiency differentials that appear as extraction only if you measure from the less-efficient side. But the structural data contradicts this — the reorganization is driven by geopolitical choices (nearshoring), policy intervention (subsidies), and power asymmetries, not physical laws. The mountain perspective here is a false summit: naturalizing a contingent institutional arrangement.
constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_supply_chain_restructuring_2020s_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_supply_chain_restructuring_2020s, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_supply_chain_restructuring_2020s, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_supply_chain_restructuring_2020s, TR),
    TR >= 0.70.

:- end_tests(global_supply_chain_restructuring_2020s_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The restructuring involves significant wealth transfer from suppliers in developing economies to consolidated manufacturers and developed-market governments through margin compression, subsidy capture, and tariff advantages. The value reflects that some genuine coordination benefits exist (reduced geopolitical risk, shorter supply chains reduce costs), but these benefits concentrate in developed markets while costs concentrate in developing economies. The trajectory from 0.28 (2020) to 0.58 (2026) shows extraction accelerating as policy interventions (subsidies, tariffs) become binding. Suppression (0.62): High. Small suppliers face multiple barriers to exit: lack of capital for new market development, retaliation through delisting, contractual lock-in clauses, and asymmetric information about alternative buyers. Middle-income economies face lock-in through subsidy dependence and infrastructure investment. Developed-market governments face political constraints (constituency pressure for manufacturing jobs). Theater ratio (0.68): High. Significant performative content in restructuring narratives: reshoring/nearshoring announcements often exceed actual capex deployment; sustainability commitments accompany margin-squeeze strategies; 'resilience' framing masks geopolitical decoupling. Re-industrialization subsidies are partially performative (announcements of capacity targets often exceed funded timelines). WTO compliance theater persists despite functional decay of the regime.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless suppliers (snare) and consolidated manufacturers (rope) is extreme: same constraint, opposite classifications. For powerless suppliers, restructuring is coercive extraction with no exit; for consolidated manufacturers, it is a solution to a coordination problem they face. This gap derives entirely from exit options (trapped vs. arbitrage) and power asymmetry. Middle-income hubs occupy the bridge: they have constrained exit (can refuse restructuring but face economic cost), which produces tangled rope classification. The organized policy perspective (scaffold) bridges snare and rope: policy institutions see restructuring as temporary coordination with sunset logic, while powerless suppliers see it as permanent extraction. The piton perspective on WTO reveals that legacy institutions lose functional purpose as power reorganizes around new mechanisms (bilateral/bloc agreements, unilateral subsidy schemes, tariff coercion). The false mountain perspective occurs when the analytical observer treats restructuring as inevitable ('the logic of global manufacturing') rather than contingent on geopolitical choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) track the agent's structural position in the extraction flow. Powerless suppliers with trapped exit face maximum d (~0.95) and experience high f(d) ~ 1.42 chi. Consolidated manufacturers with arbitrage exit face minimum d (~0.05) and experience negative f(d) ~ -0.12 chi — the constraint subsidizes them. Middle-income hubs with constrained exit occupy the middle: d ~ 0.55-0.65 produces f(d) ~ 0.75-1.00 chi. Organized policy institutions with constrained exit but genuine coordination function derive d ~ 0.35-0.45 (lower because they are beneficiaries of subsidies despite constraints), producing f(d) ~ 0.35-0.55 chi. The regional trade blocs with organized power but constrained exit derive d ~ 0.50-0.60, producing moderate chi. The identity_locked institutional perspective on supply chain coordination bodies shows the mechanism: d derived from beneficiary status + arbitrage-level options would produce low extraction (~0.05), but the identity lock (must maintain fiction of neutrality) prevents the institution from exercising exit — functional d is higher (~0.25-0.35) despite structural options, because using arbitrage exit would expose the capture.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This constraint does NOT require resolution of type ambiguity because the six classifications represent genuine perspectival differences grounded in structural position, not ambiguity about inherent type. Snare (powerless suppliers) is structurally accurate from their position; rope (consolidated manufacturers) is structurally accurate from theirs. The mandatrophy appears only if we assume a single 'true' classification independent of perspective — if we do, we face the question 'is this coordination or extraction?' The framework's answer is: it is both, and the perspectival distribution reveals the asymmetry. The mandatrophy is resolved by recognizing that the constraint is a **presheaf over the observation site**: there is no single type, only a multi-perspective classification that reveals power asymmetry through the dispersion of types. A naive single-perspective analysis would classify as snare (if measuring from developing-economy victim perspective) or rope (if measuring from consolidated manufacturer perspective), missing the structured asymmetry that the full perspectival suite reveals. The presence of identity_locked institutional perspective adds diagnostic power: it identifies that supply chain coordination institutions maintain capture not through overt coercion but through cognitive capture ('neutrality' fiction). The false mountain perspective identifies the risk of naturalizing contingent political choice as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_capacity_constraint,
    'How much of supply chain restructuring is driven by genuine automation-enabled reshoring vs. strategic geopolitical decoupling using automation as cover narrative?',
    'Cost analysis: comparing automation ROI in developed vs. developing markets with control for labor cost differentials; correlation between automation capex announcements and geopolitical tension;  measurement of actual automation deployment vs. policy announcements.',
    'If automation-driven: restructuring is market-driven (lower extraction pressure, more legitimate coordination). If geopolitically-driven: restructuring is policy-enforced (higher extraction, clear snare characteristics for trapped suppliers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_capacity_constraint, empirical, 'Attribution of supply chain restructuring to automation capacity vs. geopolitical decoupling').

omega_variable(
    developing_economy_capacity_absorption,
    'Can middle-income manufacturing hubs absorb restructured production through capital investment, or does restructuring represent net job/wage loss despite policy incentives?',
    'Longitudinal employment data (2020-2026) comparing manufacturing job growth in nearshoring destinations vs. outshoring origins; wage/productivity trends; capital accumulation rates in recipient countries; comparison against counterfactual (no-restructuring scenario).',
    'If absorption is real: middle-income perspective (tangled rope) is justified — genuine coordination with asymmetric extraction. If absorption fails: becomes snare for developing economies — promises of reindustrialization not materialize, leaving deficit in capital and employment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_economy_capacity_absorption, empirical, 'Whether developing economies can absorb restructured production through capital investment').

omega_variable(
    extraction_mechanism_opacity,
    'Is the observed suppression structural (actual barriers to supplier mobility/bargaining power) or performative (suppliers believe barriers exist but they are largely institutional theater)?',
    'Case studies of successful supplier exit/switching; interviews with suppliers on perceived vs. actual barriers; comparison of contract terms between consolidated and independent procurement; measurement of actual costs to supplier exit vs. stated industry norms.',
    'If structural: suppression (0.62) is well-calibrated. If performative: true suppression is lower (~0.35), and the constraint is closer to rope than tangled rope — the extraction mechanism depends on cognitive capture rather than structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_opacity, empirical, 'Whether suppression is structural barriers or institutional theater about barriers').

omega_variable(
    subsidy_sustainability_timeline,
    'Are re-industrialization subsidies (CHIPS Act, PLI, EU CMO) genuinely temporary with sunset clauses, or do they create permanent structural dependencies that prevent sunset?',
    'Analysis of sunset clause language; historical precedent for subsidy sunsetting; measurement of industry capacity/profitability trajectory against subsidy decline schedule; political economy analysis of sunset enforcement likelihood.',
    'If genuinely sunset (2028-2035): scaffold classification is appropriate. If subsidies become permanent or renew automatically: restructures into tangled rope with high suppression — subsidies become extraction mechanism binding suppliers/countries to dependent status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidy_sustainability_timeline, preference, 'Whether re-industrialization subsidies are genuinely temporary or become permanent').

omega_variable(
    technological_autonomy_feasibility,
    'Can nearshoring regions achieve genuine technological autonomy in critical supply chains (semiconductors, rare earths, advanced batteries), or does restructuring create new dependencies (on IP licensing, upstream materials, equipment suppliers)?',
    'Mapping of full supply chain dependencies before and after restructuring; identifying choke points that remain external to nearshoring region; measurement of technological autonomy metrics (domestic IP generation, indigenous equipment capability, material sourcing control).',
    'If autonomy achieved: restructuring solves geopolitical fragility (reduced snare risk for dependent economies). If new dependencies created: restructuring is lateral extraction — replacing one set of suppliers with another equivalent set, creating illusion of autonomy while maintaining underlying asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_autonomy_feasibility, empirical, 'Whether nearshoring achieves genuine technological autonomy or creates new dependencies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_supply_chain_restructuring_2020s, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gscr_tr_t0, global_supply_chain_restructuring_2020s, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gscr_tr_t2, global_supply_chain_restructuring_2020s, theater_ratio, 2, 0.58).
narrative_ontology:measurement(gscr_tr_t4, global_supply_chain_restructuring_2020s, theater_ratio, 4, 0.68).
narrative_ontology:measurement(gscr_tr_t6, global_supply_chain_restructuring_2020s, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(gscr_be_t0, global_supply_chain_restructuring_2020s, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gscr_be_t2, global_supply_chain_restructuring_2020s, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(gscr_be_t4, global_supply_chain_restructuring_2020s, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(gscr_be_t6, global_supply_chain_restructuring_2020s, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_supply_chain_restructuring_2020s, resource_allocation).
narrative_ontology:affects_constraint(global_supply_chain_restructuring_2020s, geopolitical_decoupling).
narrative_ontology:affects_constraint(global_supply_chain_restructuring_2020s, semiconductor_supply_security).
narrative_ontology:affects_constraint(global_supply_chain_restructuring_2020s, rare_earth_dependency).
narrative_ontology:affects_constraint(global_supply_chain_restructuring_2020s, labor_cost_arbitrage).
narrative_ontology:affects_constraint(global_supply_chain_restructuring_2020s, subsidy_induced_overcapacity).

% DUAL FORMULATION NOTE:
% Global supply chain restructuring decomposes into multiple structurally distinct constraints: (1) geopolitical decoupling (strategic choice driving restructuring), (2) semiconductor supply security (specific supply chain vulnerability motivating nearshoring), (3) labor cost arbitrage dynamics (economic incentives underlying location decisions), (4) subsidy-induced overcapacity (policy-driven distortion creating extraction mechanism). Each has distinct epsilon values and time horizons. This story captures the overall constraint; related stories capture domain-specific sub-constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_supply_chain_restructuring_2020s, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
