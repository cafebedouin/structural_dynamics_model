% ============================================================================
% CONSTRAINT STORY: project_vault_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_vault_2026, []).

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
 *   constraint_id: project_vault_2026
 *   human_readable: Project Vault Strategic Mineral Reserve
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Project Vault ($12B strategic mineral reserve announced February 2, 2026)
 *   represents a shift in U.S. economic geopolitics from market-liberal
 *   supply chain assumptions to active state stockpiling of critical minerals
 *   (lithium, cobalt, nickel, rare earths). The constraint exhibits mixed
 *   Tangled Rope and Snare characteristics depending on the observer's
 *   structural position. For emerging economies and mineral exporters without
 *   preferential U.S. alliance status, the reserve functions as pure
 *   extraction (Snare): reduced global supply availability combined with U.S.
 *   preference for domestic beneficiaries raises prices and constrains
 *   access. For allied nations, it functions as hybrid
 *   coordination-extraction (Tangled Rope): they gain supply security
 *   assurances but lose access to reserve stockpiles and accept subordinate
 *   status in allocation hierarchies. For domestic manufacturers, it
 *   functions as coordination (Rope): preferential access at subsidized
 *   prices enables supply security and competitive cost reduction. The
 *   theater ratio (0.64) reflects the disconnect between the reserve's stated
 *   purpose (mitigating geopolitical supply shocks) and its actual function
 *   (economic extraction through supply constraint). Strategic mineral
 *   shortages have occurred historically, but their frequency and severity
 *   are contested; the reserve's preventive logic is partly genuine
 *   coordination and partly performance of energy independence. Over the
 *   6-year interval modeled here, extractiveness has increased from 0.35 to
 *   0.52 as the reserve has scaled from announcement to operational
 *   accumulation (Phase 1: 18 months of purchasing), indicating that the
 *   extraction mechanism is maturing faster than the coordination function.
 *
 * KEY AGENTS:
 *   - Domestic Manufacturing Incumbents (Intel, Tesla, major defense contractors): Primary beneficiaries (institutional/arbitrage) — secure preferential reserve access at below-market rates; gain cost advantage over global competitors
 *   - Emerging Economy Supply Chains (Vietnam, India, ASEAN manufacturing): Primary victims (powerless/trapped) — dependent on mineral imports but excluded from U.S. reserve access; face price inflation from supply constraint
 *   - Mineral-Exporting Jurisdictions (Indonesia, DRC, Philippines, rare earth mines): Secondary victims (organized/constrained) — benefit from demand stability but face price suppression during reserve accumulation; constrained exit due to export revenue dependence
 *   - Allied Nations (EU, Japan, South Korea, AUKUS): Mixed position (moderate/constrained) — gain security coordination but accept subordinate access tier and geopolitical conditionality
 *   - U.S. Energy Security Apparatus (DoE, DoD, State Dept): Institutional beneficiary (institutional/constrained) — maintains energy independence framing; political commitment to reserve reduces exit options
 *   - Transparent Supply Chain Coalition (NGOs, auditors, efficiency advocates): Organized agents (organized/constrained) — see reserve as temporary patch; advocate for recycling and demand reduction alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing geopolitical choice as immutable resource scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_vault_2026, 0.52).
domain_priors:suppression_score(project_vault_2026, 0.68).
domain_priors:theater_ratio(project_vault_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_vault_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(project_vault_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(project_vault_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_vault_2026, tangled_rope).
narrative_ontology:human_readable(project_vault_2026, "Project Vault Strategic Mineral Reserve").
narrative_ontology:topic_domain(project_vault_2026, "economic/geopolitical").

domain_priors:requires_active_enforcement(project_vault_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_vault_2026, domestic_manufacturing_incumbents).
narrative_ontology:constraint_beneficiary(project_vault_2026, us_energy_security_apparatus).
narrative_ontology:constraint_victim(project_vault_2026, global_mineral_access_equity).
narrative_ontology:constraint_victim(project_vault_2026, emerging_economy_supply_chains).
narrative_ontology:constraint_victim(project_vault_2026, transparent_market_pricing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING ECONOMY SUPPLY CHAIN (SNARE) — Dependent on mineral imports for manufacturing competitiveness; cannot exit reliance on global mineral markets. U.S. reserve accumulation extracts value by artificially constraining supply, raising prices for non-reserve holders. No exit option from dependence. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(project_vault_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATIONS (TANGLED ROPE) — Benefit from U.S. security umbrella and coordination on supply chain resilience; simultaneously constrained by reduced access to reserve stockpiles and reliant on U.S. trade decisions. Mixed benefit (coordination) and extraction (supply restriction). d≈0.58, f(d)≈0.71, σ=0.9 → χ≈0.33.
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMESTIC MANUFACTURING INCUMBENTS (ROPE) — Primary beneficiaries. Secure preferential access to reserve stockpiles at below-market prices; coordination benefit through guaranteed supply. Arbitrage option: can divest from U.S. production if incentives change. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(project_vault_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: U.S. ENERGY SECURITY APPARATUS (PITON) — Maintains strategic reserve narrative from Cold War logic (oil embargo resilience); Project Vault extends the logic to rare earths and battery minerals. The reserve functions partly as genuine coordination (buffer against supply shocks) and partly as theater (demonstrating U.S. energy independence messaging). theater_ratio=0.64 reflects this mix: reserve operations are real but their strategic value is overstated in public communication. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.00. Institutional actor with constrained exit (political commitment).
constraint_indexing:constraint_classification(project_vault_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSPARENT SUPPLY CHAIN COALITION (SCAFFOLD) — NGOs, auditors, and supply chain transparency initiatives see the reserve as a temporary workaround for structural fragility in global mineral markets. The scaffold sunset: invest in domestic mining capacity, recycling infrastructure, and demand reduction through efficiency standards. Once recycling reaches 60-70% recovery rates and domestic mines scale up, strategic reserves become redundant. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.21. Organized agents with constrained but improving options.
constraint_indexing:constraint_classification(project_vault_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MINERAL-EXPORTING JURISDICTIONS (TANGLED ROPE) — Benefit from U.S. demand stabilization and potential technology transfer from partnerships; simultaneously extracted through price suppression when U.S. reserve accumulation signals reduced future demand. Constrained exit: dependence on export revenue limits ability to reduce production. d≈0.62, f(d)≈0.78, σ=1.1 → χ≈0.45.
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational view, mineral scarcity is a physical constraint: specific rare earth elements and battery materials have finitude and extraction complexity. Strategic reserves are a response to an immutable limit. However, the structural data (ε=0.52, suppression=0.68, theater=0.64) contradicts mountain classification — the constraint is contingent on institutional arrangements (export controls, reserve accumulation rates, geopolitical power), not natural scarcity alone. Engine flags as false summit.
constraint_indexing:constraint_classification(project_vault_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_vault_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_vault_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(project_vault_2026, TR),
    TR >= 0.70.

:- end_tests(project_vault_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reserve achieves supply constraint through institutional accumulation (genuine scarcity creation via stockpiling) rather than through natural resource limits. The mechanism is potent (constrains ~15-20% of global rare earth market, 8-12% of battery mineral supply) but not maximal because recycling and substitution remain available exits for constrained actors. The rate of increase over the interval (0.35→0.52) indicates the extraction mechanism is strengthening as the reserve scales from announcement to Phase 1 operation. Suppression (0.68): High. Constrained actors face significant barriers: price barriers (reserve-driven inflation), technological barriers (switching costs for non-substitutable materials), political barriers (excluded from preference tiers). However, suppression is not total because some actors retain recycling and demand-reduction pathways. Theater ratio (0.64): Moderate-high. The reserve's public framing emphasizes geopolitical risk mitigation and supply shock prevention (genuine coordination function), but operational evidence shows preferential allocation to domestic incumbents and leverage over ally access (extraction function). The theater has been increasing as public messaging emphasizes strategic independence while operational reality reveals competitive advantage accrual. Claimed type (Tangled Rope): Justified by presence of both coordination function (supply shock mitigation for allies and domestic industry) and asymmetric extraction (access restrictions benefiting U.S. actors, price inflation for excluded exporters). Requires active enforcement (true: DoE must accumulate, manage, allocate reserves). Has beneficiaries (domestic manufacturers, security apparatus) and victims (emerging economies, mineral exporters).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radically different classifications depending on structural position. Domestic manufacturers see Rope (coordination benefit with minimal extraction cost). Mineral exporters see Snare (constrained exit, extraction via supply constraint). Allied nations see Tangled Rope (mixed coordination and constrained access). The energy security apparatus sees Piton (genuine historical function — preventing embargo shocks — but increasingly performing theater in a world of recycling). The transparent supply chain coalition sees Scaffold (temporary problem with a sunset in recycling/efficiency). The civilizational analytical observer risks seeing Mountain (naturalizing geopolitical choice as resource scarcity) but the structural data reveals contingency: the constraint relies on institutional accumulation, preferential allocation, and suppression of alternatives. The perspectival gap is widest between domestic beneficiaries (who experience low-extraction rope) and excluded exporters (who experience high-extraction snare) — same physical stockpile, completely different structural experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic Manufacturing Incumbents: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; strong arbitrage option (can shift production if incentives change). U.S. Energy Security Apparatus: Beneficiary + constrained → d≈0.15, f(d)≈-0.01. Slight beneficiary; political commitment limits exit (must maintain reserve). Emerging Economy Supply Chains: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; dependent on mineral imports, no arbitrage option. Mineral-Exporting Jurisdictions: Victim + constrained → d≈0.62, f(d)≈0.78. Significant extraction; constrained by export revenue dependence but can shift allocation toward non-U.S. buyers. Allied Nations: Mixed → d≈0.58, f(d)≈0.71. Moderate extraction; benefit from alliance but constrained by reduced access tier. Transparent Supply Chain Coalition: Organized agents → d≈0.35, f(d)≈0.32. Low effective extraction; coalition has agency and sees pathway forward (recycling/efficiency).
 *
 * MANDATROPHY ANALYSIS:
 *   Project Vault resolves potential mandatrophy (false labeling as Rope when actually Snare) by explicitly declaring both beneficiaries and victims. The Tangled Rope classification prevents mischaracterization of the reserve as pure coordination (Rope) because the structural data includes victims (emerging economies, excluded exporters) who bear significant costs. Conversely, the presence of genuine coordination function (supply shock mitigation, allied security assurance) prevents classification as pure Snare. The engine validates that the reserve exhibits both properties: ε=0.52 (moderate extraction), beneficiaries=[domestic manufacturers, security apparatus], victims=[emerging supply chains, mineral exporters], requires_active_enforcement=true. The theater ratio (0.64) flags that public framing (coordination) exceeds actual functional verification (extraction mechanism verifiable, coordination benefit less certain). Mandatrophy remains unresolved (false) because the classification could be challenged on grounds of whether the 'coordination' benefit is real or performative — higher data collection needed on actual supply shock mitigation outcomes vs. preferential allocation data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_demand_trajectory,
    'Will EV adoption and green energy transition outpace domestic recycling capacity, making strategic reserves critical or obsolete?',
    'Longitudinal tracking of battery recycling rates (current ~5%, technical potential ~95%), EV deployment curves, and reserve depletion vs accumulation rates. Compare to DoE projections and market demand modeling.',
    'If demand outpaces recycling: reserves remain extractive constraint (Snare classification persists). If recycling scales rapidly: reserves degrade to piton (theater without function). Timeline: 10-15 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_demand_trajectory, empirical, 'Whether strategic reserves become critical or obsolete as recycling scales').

omega_variable(
    coordinated_extraction_vs_competition,
    'Does the U.S. reserve prevent coordinated mineral export cartels (OPEC-like), or does it enable a de facto U.S. cartel through preferential domestic access?',
    'Comparative analysis: mining output volatility pre/post reserve announcement; price correlation with reserve accumulation rates; modeling of counterfactual cartel behavior if U.S. did not reserve.',
    'If prevents cartels: reserve is genuine coordination (Rope classification). If enables U.S. extraction: reserve is institutional power consolidation (Snare for exporters).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinated_extraction_vs_competition, conceptual, 'Whether reserve prevents or enables cartelization of mineral supply').

omega_variable(
    allied_access_tier_structure,
    'Will Project Vault establish formal access tiers for allies vs non-allies, or remain implicit geopolitical pressure?',
    'Monitoring of DoS/DoD statements on reserve allocation; bilateral agreements with allied nations; disclosure of allocation frameworks; comparison to declared vs actual priority access.',
    'If formalized: constraint becomes overt institutional discrimination (higher suppression, explicit Snare for non-allies). If informal: maintains plausible deniability and theater ratio inflates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_access_tier_structure, empirical, 'Formalization of allied access vs geopolitical conditionality').

omega_variable(
    domestic_production_crowdout,
    'Does subsidized access to strategic reserves discourage investment in domestic mining and recycling, entrenching dependence on the reserve?',
    'Tracking of private mining investment, recycling facility construction, and R&D spending pre/post-Project Vault announcement. Regression analysis of policy incentives vs capex allocation.',
    'If crowdout occurs: reserve becomes a piton (theater replacing function). If domestic capacity scales: reserve transitions to scaffold (enabling transition rather than entrenchment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_production_crowdout, empirical, 'Whether reserve access displaces domestic production investment').

omega_variable(
    geopolitical_retaliation_vulnerability,
    'Can adversaries (China, Russia) create countervailing supply cartels or strategic purchases that neutralize U.S. reserve advantages?',
    'Monitoring of Chinese rare earth export controls, Russian mineral partnerships with allied exporters, and coordinated purchasing by adversary coalitions. War-gaming of supply shock scenarios.',
    'If effectively neutralized: reserve loses extractive force (classification reverts toward Rope). If U.S. advantage persists: extraction mechanism remains durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_retaliation_vulnerability, empirical, 'Adversary capacity to countervail U.S. reserve advantage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vault_tr_t0, project_vault_2026, theater_ratio, 0, 0.5).
narrative_ontology:measurement(vault_tr_t3, project_vault_2026, theater_ratio, 3, 0.58).
narrative_ontology:measurement(vault_tr_t6, project_vault_2026, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(vault_be_t0, project_vault_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vault_be_t3, project_vault_2026, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(vault_be_t6, project_vault_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(project_vault_2026, resource_allocation).
narrative_ontology:affects_constraint(project_vault_2026, lithium_extraction_frontier).
narrative_ontology:affects_constraint(project_vault_2026, rare_earth_cartel_risk).
narrative_ontology:affects_constraint(project_vault_2026, battery_supply_security).
narrative_ontology:affects_constraint(project_vault_2026, allied_industrial_competitiveness).

% DUAL FORMULATION NOTE:
% Project Vault is the aggregate institutional mechanism; upstream constraints include specific mineral supply risks (lithium, cobalt, rare earths) and downstream constraints include allied industrial competitiveness and EV supply chain vulnerability. This constraint represents the geopolitical response layer that couples multiple mineral markets through preferential allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(project_vault_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
