% ============================================================================
% CONSTRAINT STORY: shock_propagation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shock_propagation_asymmetry, []).

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
 *   constraint_id: shock_propagation_asymmetry
 *   human_readable: The One-Way Crisis Valve
 *   domain: economic/logistical
 *
 * SUMMARY:
 *   Global supply chain integration creates a structural asymmetry where
 *   positive shocks (technological innovation, demand growth, currency
 *   strength) accrue to central economies and multinational firms, while
 *   negative shocks (commodity price collapse, supply disruptions, inflation
 *   contagion, environmental/climate costs) are systematically concentrated
 *   in peripheral economies through mechanisms of financial dependency, debt
 *   servicing, currency exposure, and export-price volatility. This
 *   constraint is a tangled rope: it provides genuine coordination benefits
 *   (market access, economies of scale, efficient production networks) while
 *   simultaneously functioning as an extraction mechanism that concentrates
 *   risk. The asymmetry is enforced through contracts, monetary policy, and
 *   institutional structures rather than through direct coercion, but the
 *   effect is binding. Peripheral economies are locked into the system by
 *   their debt obligations, capital scarcity, and lack of autonomous supply
 *   alternatives. The constraint's extractiveness has grown over 30 years as
 *   supply chain integration has deepened and as central economies have
 *   accumulated capacity to absorb or insulate themselves from shocks while
 *   peripheral economies have become increasingly specialized in volatile
 *   commodity exports. The theater ratio reflects that much of the policy
 *   apparatus (development programs, poverty reduction frameworks, structural
 *   adjustment) operates performatively — following conditionality
 *   requirements — while the real extraction mechanism (debt cycles, terms of
 *   trade, currency exposure) persists regardless of policy compliance.
 *
 * KEY AGENTS:
 *   - Central Economies (institutional/arbitrage): Primary beneficiaries — receive positive shocks, absorb capacity to redirect negative shocks through currency reserves, financial markets, and portfolio diversification. Experience the system as coordination (Rope).
 *   - Peripheral Economies (powerless/trapped): Primary victims — locked into commodity export and manufactured-goods import. Bear full shock concentration with no exit routes. Experience the system as pure extraction (Snare).
 *   - Resource-Dependent Regions (moderate/constrained): Secondary victims with temporary benefits — gain market access and development financing during growth phases, but face austerity and debt defaults during downturns. Exit is constrained by capital requirements for diversification.
 *   - Multinational Supply Networks (institutional/arbitrage): Beneficiaries through efficiency gains and risk portfolio management. Can shift supplier relationships and access diversified inputs across geographies.
 *   - Regional Development Coalitions (organized/constrained): Challengers building parallel structures (BRICS, ACFTA, regional trade blocs) designed to provide decoupling and shock insulation. Have agency and exit pathways but face capital constraints.
 *   - International Financial Institutions (institutional/arbitrage): Maintainers of the institutional structure through loan conditionality and debt enforcement. Theater-heavy — compliance rituals matter more than development outcomes.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the asymmetry as inevitable consequence of comparative advantage rather than recognizing it as a contingent institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shock_propagation_asymmetry, 0.58).
domain_priors:suppression_score(shock_propagation_asymmetry, 0.68).
domain_priors:theater_ratio(shock_propagation_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shock_propagation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shock_propagation_asymmetry, tangled_rope).
narrative_ontology:human_readable(shock_propagation_asymmetry, "The One-Way Crisis Valve").
narrative_ontology:topic_domain(shock_propagation_asymmetry, "economic/logistical").

domain_priors:requires_active_enforcement(shock_propagation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, central_economies).
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, multinational_supply_networks).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, peripheral_economies).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, resource_dependent_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL ECONOMY (SNARE) — Structurally locked into commodity export and demand for central manufactured goods. When supply chains shatter or commodity prices collapse, peripheral economies bear full shock absorption with no escape route. Cannot decouple from integrated markets without catastrophic domestic collapse. Maximum extraction experienced — shock funneling mechanism is the constraint's core function.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-DEPENDENT REGION (TANGLED ROPE) — Benefits from market access and commodity pricing during growth phases, enabling debt-financed development and infrastructure investment. Bears shock concentration during downturns through currency collapse, debt defaults, and austerity requirements. Constrained exit — can reduce supply chain dependence only through years of diversification, but faces capital constraints to do so. Genuine coordination function (access to global markets) + asymmetric extraction (shock concentration).
constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL MANUFACTURING HUB (ROPE) — Experiences the constraint as pure coordination: integrated supply networks enable economies of scale, just-in-time production, and demand-responsive manufacturing. During downturns, can shift supplier relationships or build redundancy. Net beneficiary — receives positive shocks and can absorb or redirect negative shocks through portfolio diversification and currency strength. The constraint solves their collective coordination problem.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL DEVELOPMENT COALITION (SCAFFOLD) — Organized agents (BRICS, regional trade blocs, development banks) are building parallel supply networks and commodity exchanges designed to decouple peripheral economies from central shock propagation. Examples: ASEAN infrastructure integration, African Continental Free Trade Area, South American regional development banks. These mechanisms explicitly target the asymmetry. Sunset logic: As regional alternatives mature (10-30 year horizon), peripheral economies gain mobile exit options, reducing the constraint's extractive force.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL FINANCIAL INSTITUTIONS (PITON) — The IMF, World Bank, and bilateral creditors maintain loan structures and structural adjustment programs that are substantially theatrical: they enforce compliance with market-liberalization policies that concentrate shock absorption in peripheral economies. The performative element is high (bureaucratic conditionality, development metrics that measure activity rather than welfare). The real function has degraded — capital flows and remittances often outpace aid, and the institutional apparatus persists through treaty obligation and bureaucratic inertia rather than genuine efficiency. Theater ratio reflects that much of the institutional mandate is now maintenance of relationships rather than effective development.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an economic theory perspective, the constraint may appear inevitable: comparative advantage and specialization are claims about optimality. Peripheral economies are 'specialized' in commodities; central economies are 'specialized' in manufacturing and financial services. Integration is efficient. Shocks naturally transmit through integrated markets. This perspective risks naturalizing what is actually a contingent institutional choice (shock distribution rules are set by contracts and central bank policies, not by physics). The mountain classification will trigger the false summit detector — the structural data shows this is not an immutable law.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shock_propagation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shock_propagation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shock_propagation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(shock_propagation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint concentrates negative shocks in peripheral economies while permitting central economies to absorb or avoid them. This is not pure expropriation — peripheral economies do gain access to markets and capital during growth phases. But the asymmetry is real and binding: peripheral debt-to-GDP ratios spike during commodity downturns while central economies maintain stable asset prices. The extractiveness value reflects the measured shock transmission asymmetry (peripheral shock velocity ~2x central economies) and the debt-financed development model that creates persistent vulnerability. Suppression (0.68): High. Peripheral economies have severely constrained exit options due to: capital scarcity (cannot build autonomous supply chains without massive investment), debt servicing (foreign exchange earned must service debt before diversification investment), currency exposure (import prices surge when currency collapses, forcing import contraction), and institutional lock-in (trade agreements, IMF programs, creditor relationships enforce integration). Theater ratio (0.55): Moderate. The policy apparatus (development frameworks, poverty reduction metrics, structural adjustment) is partially performative — institutions measure compliance with liberalization, not welfare outcomes. But the underlying extraction mechanism (debt cycles, terms-of-trade volatility, currency exposure) is real and structural, not purely theatrical. The theater reflects bureaucratic overhead and goal displacement, not the absence of genuine extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. Central manufacturing sees Rope — a pure coordination mechanism solving global efficiency problems. Peripheral economies see Snare — they are locked into absorbing shocks with no alternatives. Regional development coalitions see Scaffold — parallel structures (BRICS, ACFTA) are building exit paths that will sunset the central asymmetry within 20-30 years if successful. International institutions see their own process as Piton — structural adjustment and development conditionality persist through bureaucratic inertia and treaty obligation, not because they produce the promised welfare gains. The analytical observer risks seeing Mountain — naturalizing the asymmetry as inevitable comparative advantage — but the structural data (measurable shock asymmetry, binding debt constraints, institutional policy choices) reveals this as a false summit. The gap reflects that the same system is functionally a pure coordination mechanism for central actors and a pure extraction mechanism for peripheral actors, with moderate mixed outcomes for intermediate regions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position within the shock distribution mechanism. Central economies and multinational networks are beneficiaries with arbitrage options (low d → low/negative χ). They can rebalance portfolios, shift suppliers, and access capital markets to absorb shocks. Peripheral economies are victims with trapped exit options (high d → high f(d) → high χ). They cannot decouple from integrated markets without domestic economic collapse and lack the capital to build alternatives. Resource-dependent regions are moderate victims with constrained (not trapped) exit — they can diversify but at substantial cost and over multi-decade timescales (d ≈ 0.65 → f(d) ≈ 1.00). Regional development coalitions are organized agents with constrained exit — they are building alternatives but face capital and political coordination barriers (d ≈ 0.55 → f(d) ≈ 0.75). International financial institutions are beneficiaries through fee structures and institutional persistence, with arbitrage options in creditor relationships (d ≈ 0.15 → f(d) ≈ -0.01 → negative χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through structural decomposition. The question 'Is this Rope or Snare?' cannot be answered globally — the answer depends on the observer's position. The central economy genuinely experiences Rope: market integration solves real coordination problems (production efficiency, demand distribution, technological diffusion). The peripheral economy genuinely experiences Snare: the same integration funnels shocks with no countervailing benefit during downturns. The mandatrophy is not 'which type is correct?' but 'the constraint is simultaneously both because the extraction function and coordination function operate on different agents.' The solution is the tangled_rope classification: both beneficiaries and victims are required for the schema validation; both a coordination function and asymmetric extraction are present; active enforcement (debt servicing, currency management, trade agreement compliance) is required to maintain the structure. The scaffold perspective (regional decoupling) and the piton perspective (institutional degradation) together suggest that the constraint is under structural pressure — if regional alternatives mature or if central shock absorption capacity is exceeded, the asymmetry will degrade. The mandate resolves by recognizing that this is not a single constraint viewed from different angles, but a genuinely hybrid structure where coordination and extraction are coupled: peripheral economies cannot access markets without accepting shock concentration, and central economies cannot achieve their efficiency levels without offloading volatility to peripheries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shock_magnitude_threshold,
    'What magnitude and duration of negative shock distinguishes normal market adjustment from systemic funneling?',
    'Comparative analysis of shock transmission: peripheral vs central economies across 50+ major shocks (commodity crashes, financial crises, supply chain disruptions, 1980-2026). Measure velocity of wage adjustment, currency response, debt-to-GDP impacts, employment duration.',
    'If transmission is symmetric: constraint is pure market dynamics (Rope). If asymmetric by >2x: constraint is extraction mechanism (Snare/Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shock_magnitude_threshold, empirical, 'Threshold for distinguishing normal adjustment from asymmetric shock funneling').

omega_variable(
    regional_decoupling_feasibility,
    'Can peripheral economies build sufficient autonomous supply capacity and regional trade density to achieve materially meaningful decoupling from central shock propagation within 30 years?',
    'Historical precedent analysis (China 1980-2010, India 1991-2020, ASEAN regional integration). Measurement of regional trade as % of total, domestic manufacturing value-add growth, supply chain localization metrics. Correlation with shock insulation.',
    'If feasible: Regional Development Coalition''s scaffold perspective is realistic, sunset is material. If infeasible: decoupling is aspirational theater, and the constraint''s extractive structure is semi-permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_decoupling_feasibility, empirical, 'Whether autonomous regional supply chains can provide material decoupling').

omega_variable(
    central_shock_absorption_capacity,
    'What is the true shock absorption capacity of central economies when cumulative peripheral defaults threaten central bank balance sheets and financial system stability?',
    'Stress-test modeling: peripheral debt default cascades, currency collapse propagation to central economies, central bank intervention costs. Historical case studies (Latin American debt crisis 1982, Asian Financial Crisis 1997-98, 2008 financial crisis feedback loops).',
    'If central absorption is limited: apparent asymmetry is sustainable only until a critical mass of peripheral failures triggers contagion (transition point exists). If absorption is effectively unlimited: asymmetry can persist indefinitely through central institutional capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_shock_absorption_capacity, empirical, 'Central economies'' shock absorption capacity before systemic destabilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shock_propagation_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shock_tr_t0, shock_propagation_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shock_tr_t15, shock_propagation_asymmetry, theater_ratio, 15, 0.48).
narrative_ontology:measurement(shock_tr_t30, shock_propagation_asymmetry, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(shock_be_t0, shock_propagation_asymmetry, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(shock_be_t15, shock_propagation_asymmetry, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(shock_be_t30, shock_propagation_asymmetry, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shock_propagation_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, commodity_price_volatility).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, currency_exposure_debt_cycles).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, supply_chain_concentration).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, peripheral_fiscal_space_collapse).

% DUAL FORMULATION NOTE:
% The shock propagation asymmetry is upstream of specific peripheral economic crises (currency collapses, debt defaults, austerity spirals) but represents a distinct structural constraint operating at the system level. Downstream constraints have their own extractiveness values reflecting specific institutional mechanisms (currency pegging, debt contracts, trade agreements); the shock propagation asymmetry represents the ensemble effect of these mechanisms creating directional shock funneling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shock_propagation_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
