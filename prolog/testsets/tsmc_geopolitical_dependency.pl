% ============================================================================
% CONSTRAINT STORY: tsmc_geopolitical_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsmc_geopolitical_dependency, []).

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
 *   constraint_id: tsmc_geopolitical_dependency
 *   human_readable: TSMC Geopolitical Dependency and Strategic Extraction
 *   domain: geopolitical_economy/semiconductor_supply_chain
 *
 * SUMMARY:
 *   TSMC (Taiwan Semiconductor Manufacturing Company) represents a critical
 *   choke point in global semiconductor supply chains. As the world's largest
 *   and most advanced chip foundry, TSMC produces over 50% of global
 *   semiconductors and >90% of advanced chips below 10 nanometers. This
 *   concentration creates a geopolitical dependency constraint with multiple
 *   structural dimensions: (1) economic extraction through monopoly pricing
 *   on advanced nodes, (2) geopolitical leverage through Taiwan's military
 *   vulnerability, (3) forced coordination among allied states to maintain
 *   Taiwan's independence and TSMC's stability, and (4) institutional inertia
 *   in free trade frameworks that rationalize the dependency despite
 *   recognition of strategic risk. The constraint exhibits tangled rope
 *   structure: TSMC's role coordinates global semiconductor supply while
 *   simultaneously extracting geopolitical rent through the threat of
 *   disruption. Extractiveness has increased from 0.35 to 0.58 over the past
 *   decade as China's military capability has grown and geopolitical tensions
 *   have escalated, making the dependency more costly to all non-allied
 *   purchasers. Theater ratio remains relatively low (0.35) because the
 *   mechanisms are primarily economic/military rather than performative —
 *   actual fab capacity, actual geopolitical risk, actual pricing power —
 *   though increasing rhetoric about supply chain resilience adds a
 *   performative element.
 *
 * KEY AGENTS:
 *   - Global Semiconductor Purchasers (Powerless/Trapped): Computer companies, smartphone manufacturers, defense contractors worldwide who depend on TSMC for advanced chips and have no viable alternatives
 *   - Taiwan Government & TSMC (Institutional/Arbitrage): Primary beneficiary; captures economic rents through TSMC dominance and maintains geopolitical importance through strategic dependency
 *   - United States (Institutional/Arbitrage): Secondary beneficiary; uses TSMC dependency to maintain alliance leverage in Asia-Pacific; also victim of supply chain vulnerability
 *   - China (Powerful/Arbitrage): Implicit extractor; geopolitical threat against Taiwan increases TSMC's strategic value and the cost of maintaining its independence
 *   - Alternative Chip Manufacturers (Moderate/Constrained): Samsung, Intel, foundries in South Korea, Japan suppressed by TSMC's technology lead and capacity dominance
 *   - Taiwan's Military Security (Powerless/Trapped): Hostage situation where TSMC's economic value makes Taiwan a higher-value target in any conflict
 *   - Regional Diversification Coalition (Organized/Constrained): US, EU, Japan, South Korea building redundant fab capacity with sunset logic; currently constrained by technology lags and high costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsmc_geopolitical_dependency, 0.58).
domain_priors:suppression_score(tsmc_geopolitical_dependency, 0.68).
domain_priors:theater_ratio(tsmc_geopolitical_dependency, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsmc_geopolitical_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(tsmc_geopolitical_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tsmc_geopolitical_dependency, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsmc_geopolitical_dependency, tangled_rope).
narrative_ontology:human_readable(tsmc_geopolitical_dependency, "TSMC Geopolitical Dependency and Strategic Extraction").
narrative_ontology:topic_domain(tsmc_geopolitical_dependency, "geopolitical_economy/semiconductor_supply_chain").

domain_priors:requires_active_enforcement(tsmc_geopolitical_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsmc_geopolitical_dependency, united_states_strategic_position).
narrative_ontology:constraint_beneficiary(tsmc_geopolitical_dependency, taiwan_government_revenue).
narrative_ontology:constraint_beneficiary(tsmc_geopolitical_dependency, tsmc_shareholders).
narrative_ontology:constraint_victim(tsmc_geopolitical_dependency, semiconductor_purchasers_globally).
narrative_ontology:constraint_victim(tsmc_geopolitical_dependency, taiwan_military_security).
narrative_ontology:constraint_victim(tsmc_geopolitical_dependency, alternative_chip_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SEMICONDUCTOR PURCHASERS (SNARE) — Locked into dependency on TSMC's manufacturing capacity for advanced chips. No viable exit: alternative foundries (Samsung, Intel) lag in process technology and capacity. Suppression is structural: geopolitical instability over Taiwan creates artificial scarcity premium. Purchasers bear extraction costs through pricing power and allocation rationing during supply constraints.
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TAIWAN MILITARY SECURITY (SNARE) — TSMC's concentration in Taiwan creates geopolitical hostage logic. Taiwan cannot diversify this economic asset without sacrificing the economic rent that finances defense spending. China's incentive to seize TSMC in any conflict makes Taiwan a higher-value target and raises the military cost of defense. Suppression is maximum — no exit exists without surrendering strategic independence.
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC POSITION (TANGLED ROPE) — Benefits from TSMC dependency: Taiwan's vulnerability to Chinese pressure increases US leverage in Asia-Pacific alliance coordination; semicondutor supply control creates coordination incentive for allied states to maintain close US relationships. Active enforcement through CHIPS Act, export controls to China, and Taiwan security guarantees. Also pays extraction cost: vulnerable to supply disruptions; forced to invest in domestic capacity redundancy (CHIPS Act subsidies); trapped in Taiwan defense commitment. Mixed: genuine coordination function (allies need secure supply) with asymmetric extraction (US extracts geopolitical leverage).
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TSMC MANAGEMENT AND SHAREHOLDERS (ROPE) — Primary beneficiary. Captures monopoly pricing on advanced nodes (N3, N2). Taiwan government guarantees protect assets and markets. Experiences the constraint as coordination: managing geopolitical relationships is part of operational risk, but the constraint generates sustained premium returns. Exit is genuinely available (relocation to US/Japan) but arbitrage (remaining in Taiwan with government backing) exceeds exit value. No meaningful suppression experienced — constraint operates as coordination mechanism for this agent.
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE CHIP MANUFACTURERS (TANGLED ROPE) — Suppressed by TSMC's technology lead and fab capacity dominance. Cannot exit: invested in foundry business tied to TSMC benchmarks. Experience extraction through technology lag (5G nodes behind TSMC), customer loss, and pricing pressure. Also benefit from TSMC's geopolitical instability: customer diversification pressure drives some work to Samsung/Intel. Constrained exit: could withdraw from advanced nodes competition but face sunk R&D costs and loss of foundry revenue tier. Moderate extraction, genuine coordination need (alternatives must exist for supply resilience).
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL TRADE NORMS FRAMEWORK (PITON) — Free trade orthodoxy (WTO, comparative advantage, specialization) has been the framework justifying TSMC concentration. But the constraint reveals the framework as degraded: geopolitical considerations override trade logic; US is abandoning specialization through CHIPS Act subsidies; reshoring is being mandated despite comparative disadvantage. The trade norm persists rhetorically ('we support free markets') while being actively violated. Theater ratio high: extensive discourse about supply chain resilience and friendly-shoring is performative cover for coercive reshoring pressure. Piton classification reflects institutional inertia of free trade framing even as the actual mechanism is geopolitical control.
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: REGIONAL DIVERSIFICATION COALITION (SCAFFOLD) — Organized agents (US CHIPS Act, EU Chips Act, Japan subsidies, South Korea incentives) are building redundant foundry capacity. Sunset logic: as Intel Arizona, Samsung Texas, TSMC Arizona, and Samsung Europe mature (5-10 years), dependency on Taiwan's single-location fab concentration declines. Constraints on purchasers ease. Current suppression and extraction reflect the interim period before alternatives mature. Organized agents have genuine exit path and agency — structured investment in sunset.
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICS CONSTRAINT VIEW (MOUNTAIN) — From a civilizational/universal perspective, TSMC dependency appears structurally immutable: semiconductor fabs require massive capital (10-20B per fab), years to mature, and stable supply chains for exotic materials. Geographic concentration reflects physics constraints (water access, seismic stability, skilled labor concentration). This perspective risks naturalizing what is actually a contingent geopolitical choice. The 'immutability' frame obscures that alternative locations are physically viable but were abandoned for economic efficiency. The engine's false summit detector will identify this as naturalization of contingency.
constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsmc_geopolitical_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tsmc_geopolitical_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsmc_geopolitical_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tsmc_geopolitical_dependency, TR),
    TR >= 0.70.

:- end_tests(tsmc_geopolitical_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. TSMC captures monopoly-level pricing on advanced nodes (estimated 30-50% premium over competitive baseline). However, the extraction is not as severe as pure snare (which would be 0.70+) because genuine coordination functions exist — TSMC's massive R&D investment produces real technological leadership, and its role as supply-chain hub does coordinate global chip design. The increase from 0.35 to 0.58 reflects geopolitical escalation: as Taiwan military risk has increased, purchasers have less negotiating power and TSMC can extract more through supply rationing and pricing. Suppression (0.68): High. Multiple barriers to exit: no alternative fab has achieved process parity (N3/N2 level); building equivalent capacity takes 5-10 years and 15-20 billion dollars; geopolitical instability makes TSMC unreliable yet unavoidable; export controls from US prevent alternative sourcing from China. Suppression would be higher (0.80+) but for the credible regional diversification efforts, which offer a long-term escape path. Theater ratio (0.35): Moderate-low. The constraint is primarily structural (actual fab capacity, actual geopolitical risk) rather than performative. However, discourse around 'friendly-shoring' and supply chain resilience involves significant rhetoric divorced from actual progress — estimated 35% of public statements about diversification are theater, 65% reflect genuine investment. The theater has grown from 0.25 to 0.35 as governments have publicized CHIPS Act funding without yet delivering equivalent capacity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence: TSMC/Taiwan sees rope (coordination + legitimate business), global purchasers see snare (trapped in dependency), US sees tangled rope (coordination + extraction), alternatives see tangled rope (mixed benefit/suppression), Taiwan's military sees snare (hostage situation), trade norms see piton (framework degraded but persisting), diversification coalition sees scaffold (temporary with sunset), civilizational view risks mountain (naturalizing what is contingent). The gap is extreme because the constraint's structure genuinely IS different from each agent's position: beneficiaries experience coordination; trapped agents experience pure extraction; organized agents see a solvable intermediate problem with sunset. This perspectival divergence is the core diagnostic signature that the constraint is tangled rope, not pure rope (which would show less gap) or pure snare (which would show uniform victimhood).
 *
 * DIRECTIONALITY LOGIC:
 *   TSMC/Taiwan occupies the beneficiary + arbitrage position: they have technology leadership (d ≈ 0.10 from canonical fallback) and genuine exit options (could diversify to US but don't because Taiwan dominance is higher value). Applied sigmoid f(d) produces low or negative chi — they experience coordination, not extraction. Global purchasers occupy trapped + victim position: high-cost exit (building alternatives takes 10 years), no structural alternatives, geopolitical risk traps them (d ≈ 0.95). Applied sigmoid f(d) produces maximum chi — they experience high extraction. US occupies the mixed position: benefits from TSMC dependency (geopolitical leverage, d ≈ 0.30) but also constrained by it (supply vulnerability, forced Taiwan defense commitment, d ≈ 0.70). The average for US perspective is roughly 0.50, producing moderate extraction. Directionality overrides are not needed because the structural data (beneficiary/victim declarations + exit options per perspective) correctly capture the asymmetries. The scope modifier σ(S) amplifies chi for global scope (σ=1.2) because the constraint affects all semiconductor purchasers worldwide — verification difficulty is high, allowing more extraction to remain hidden.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR OF HYBRID CONSTRAINT: TSMC dependency resolves mandatrophy by exhibiting genuine tangled rope structure — not a false binary between 'coordination' and 'extraction' but a real hybrid where both functions are present. The coordination function: TSMC's advanced fabs do solve the global problem of how to manufacture cutting-edge chips. No actor could unilaterally coordinate semiconductor supply at N3/N2 levels without concentrated investment. The extraction function: TSMC's monopoly position extracts economic rent (30-50% pricing premium estimated) and geopolitical leverage (Taiwan's security becomes hostage to chip supply). The active enforcement dimension: US CHIPS Act, export controls to China, Taiwan security guarantees are all enforcement mechanisms that sustain TSMC's position. The false natural law risk is captured in the 'mountain' perspective — the constraint can appear as an immutable consequence of physics (fabs require massive capital, process R&D, stable conditions) and economics (comparative advantage). But the structural data reveals contingency: alternative locations are physically viable (US, Europe, Japan have land, water, talent); the concentration was a choice optimizing for efficiency in the 1990s-2010s when geopolitics seemed stable. The choice is now locked by sunk costs and technology lags, not by physics. The scaffold perspective reveals the sunset: as regional fabs mature, the dependency genuinely declines. This is not aspirational — it is structural diversification with enforceable timelines (2028-2032 for Intel/Samsung/TSMC US fabs to reach volume at advanced nodes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semiconductor_process_divergence,
    'Will regional diversification actually produce functionally equivalent advanced nodes, or will geographical fab dispersion create persistent technology gaps?',
    'Longitudinal tracking of process node specifications across regional fabs; comparison of yield rates, defect densities, and performance parity for identical designs manufactured in Taiwan vs US vs Europe vs Japan fabs',
    'If parity achieved: dependency genuinely sunsets; scaffold becomes functional rope. If gaps persist: regional fabs remain backup/commodity suppliers; TSMC dependency persists at reduced but non-zero level; scaffold extends or fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semiconductor_process_divergence, empirical, 'Whether regional diversification produces functionally equivalent advanced nodes').

omega_variable(
    china_military_timeline,
    'What is the probability and timeline for Chinese military action against Taiwan? Does geopolitical risk premium reflect actual military threat or primarily financial market risk-pricing?',
    'Military capability assessments from defense intelligence agencies; correlation analysis of TSMC supply volatility vs China military exercise timing; historical precedent from other contested territories',
    'If military action probability is <5% in 10-year horizon: geopolitical risk premium is partially rent-extraction rather than coordination cost; suppression metric overstates genuine constraint. If >20%: suppression metric understates true risk; extraction is worse than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_military_timeline, empirical, 'Probability and timeline for Chinese military action against Taiwan').

omega_variable(
    alternative_fab_subsidy_viability,
    'Can regional fab investments (US CHIPS Act, EU Chips Act) achieve cost parity with Taiwan production, or will they require permanent subsidies to remain operational?',
    'Financial analysis of US/EU fab operating costs vs Taiwan baseline; tracking of production volume ramp curves and unit cost trajectories; comparison of implicit costs (capital subsidy + operating support) across regions',
    'If parity achievable: diversification reduces extraction; scaffold sunset is real. If permanent subsidy required: regional fabs become welfare-dependent; extraction shifts from TSMC pricing to taxation for subsidies; total system extraction may not decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fab_subsidy_viability, empirical, 'Whether regional fabs can achieve cost parity with Taiwan production').

omega_variable(
    geopolitical_extraction_mechanism,
    'Is TSMC dependency primarily a structural result of comparative advantage (physics + economics) or primarily a geopolitical tool deliberately constructed for leverage?',
    'Historical analysis of semiconductor fab location decisions from 1980-2020; examination of US/Taiwan policy choices that could have diversified but didn''t; counterfactual analysis of what early fab distribution would have looked like without geopolitical consideration',
    'If structural/comparative-advantage driven: constraint is closer to mountain (inevitable given physics); extraction is less intentional. If deliberately constructed: constraint is closer to snare (intentional control); extraction mechanism is clearer and more culpable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_extraction_mechanism, conceptual, 'Whether TSMC dependency is structural or deliberately constructed for geopolitical leverage').

omega_variable(
    taiwan_revenue_dependence,
    'What percentage of Taiwan government revenue depends on TSMC taxes and export duties? Could Taiwan sustain defense spending without TSMC economic rents?',
    'Fiscal analysis of Taiwan government budget; decomposition of tax revenue sources; scenario modeling of Taiwan defense spending under reduced TSMC economic contribution',
    'If Taiwan defense budget critically dependent on TSMC rents: Taiwan faces bind where economic vulnerability and military vulnerability are coupled; escape from geopolitical dependency creates fiscal crisis. If Taiwan has fiscal independence: TSMC is asset, not necessity; Taiwan has more exit optionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taiwan_revenue_dependence, empirical, 'Degree to which Taiwan government revenue depends on TSMC economic rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsmc_geopolitical_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsmc_tr_t0, tsmc_geopolitical_dependency, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tsmc_tr_t5, tsmc_geopolitical_dependency, theater_ratio, 5, 0.32).
narrative_ontology:measurement(tsmc_tr_t10, tsmc_geopolitical_dependency, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(tsmc_be_t0, tsmc_geopolitical_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tsmc_be_t5, tsmc_geopolitical_dependency, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(tsmc_be_t10, tsmc_geopolitical_dependency, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsmc_geopolitical_dependency, resource_allocation).
narrative_ontology:affects_constraint(tsmc_geopolitical_dependency, us_china_semiconductor_competition).
narrative_ontology:affects_constraint(tsmc_geopolitical_dependency, taiwan_military_security_asymmetry).
narrative_ontology:affects_constraint(tsmc_geopolitical_dependency, advanced_chip_pricing_power).

% DUAL FORMULATION NOTE:
% TSMC geopolitical dependency decomposes into three structurally distinct constraints with different epsilon values: (1) semiconductor_fab_concentration (epsilon=0.35, rope/tangled_rope — pure technical coordination), (2) taiwan_geopolitical_hostage (epsilon=0.72, snare — military/political extraction), (3) advanced_node_pricing_power (epsilon=0.48, snare — economic extraction through monopoly). This story focuses on the hybrid constraint that encompasses all three. Upstream constraints include semiconductor physics limits (chiplets, 3D stacking); downstream constraints include defense supply chain dependencies. Network edges indicate that changes to any one would structurally affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsmc_geopolitical_dependency, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
