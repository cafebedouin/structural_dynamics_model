% ============================================================================
% CONSTRAINT STORY: global_oil_supply_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_oil_supply_stability, []).

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
 *   constraint_id: global_oil_supply_stability
 *   human_readable: Global Oil Supply Stability: Coordination and Extraction
 *   domain: geopolitical_economy/energy_infrastructure
 *
 * SUMMARY:
 *   Global oil supply stability is a structurally hybrid constraint that
 *   simultaneously solves a genuine coordination problem (organizing massive
 *   capital investment in energy infrastructure across political borders and
 *   long timescales) and extracts rents through the concentration of
 *   geological reserves, market power by integrated majors, and geopolitical
 *   leverage by oil-producing states. The constraint exhibits all six DR
 *   types from different structural positions: from snare for trapped
 *   oil-importing nations to rope for organized energy companies to scaffold
 *   for renewable transition coalitions. The extractiveness trajectory (0.42
 *   → 0.65 → 0.58) reflects a rise during the 2008-2015 period of resource
 *   nationalism and OPEC coordination strengthening, peak extraction around
 *   2010-2015 (shale revolutions in importing nations + renewable cost
 *   collapse + climate policy acceleration), followed by partial moderation
 *   as transition economics improve and alternative energy sources mature.
 *   The suppression trajectory rises from 0.55 to 0.68 as geopolitical
 *   competition (U.S.-China, Russia sanctions, Middle East tensions)
 *   intensifies, then moderates as renewable transition reduces import
 *   dependence pressure. Theater ratio remains moderate (0.35→0.52) because
 *   coordination remains partly genuine — supply forecasting and price
 *   discovery have real coordination function — but increasingly performative
 *   as market mechanisms (futures, derivatives, strategic reserves)
 *   substitute for institutional coordination narrative.
 *
 * KEY AGENTS:
 *   - Oil-Producing States (OPEC, Russia, Iraq, Iran, Saudi Arabia): Primary beneficiaries (institutional/arbitrage) — capture rents through cartelization and geopolitical leverage
 *   - Integrated Petroleum Majors (Shell, ExxonMobil, Saudi Aramco, TotalEnergies, ADNOC): Primary beneficiaries (powerful/mobile) — maintain downstream oligopoly and capture processing/distribution rents
 *   - Oil-Importing Nations (OECD Europe, Japan, Korea, India, developing world): Primary victims (powerless/trapped) — locked into oil-dependent infrastructure with no alternative in biographical horizon
 *   - Renewable Energy Coalition (IEA, IRENA, climate advocates, solar/wind developers): Organized beneficiaries of sunset (organized/constrained) — building alternative pathway with generational horizon
 *   - Energy-Dependent Poor and Vulnerable Populations: Powerless victims (powerless/trapped) — bear immediate cost of price spikes without exit options
 *   - Diversified Energy Companies: Organized coordinators (organized/mobile) — benefit from price stability across energy portfolio; have flexibility to arbitrage
 *   - International Energy Institutions (IEA, OPEC Secretariat, IAEA): Institutional theater-maintainers (institutional/arbitrage) — perform coordination narrative; actual coordination increasingly happens through bilateral deals and market mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable scarcity constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_oil_supply_stability, 0.58).
domain_priors:suppression_score(global_oil_supply_stability, 0.62).
domain_priors:theater_ratio(global_oil_supply_stability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_oil_supply_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_oil_supply_stability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_oil_supply_stability, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_oil_supply_stability, tangled_rope).
narrative_ontology:human_readable(global_oil_supply_stability, "Global Oil Supply Stability: Coordination and Extraction").
narrative_ontology:topic_domain(global_oil_supply_stability, "geopolitical_economy/energy_infrastructure").

domain_priors:requires_active_enforcement(global_oil_supply_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_oil_supply_stability, oil_producing_states).
narrative_ontology:constraint_beneficiary(global_oil_supply_stability, integrated_energy_majors).
narrative_ontology:constraint_beneficiary(global_oil_supply_stability, oil_exporting_companies).
narrative_ontology:constraint_victim(global_oil_supply_stability, oil_importing_nations).
narrative_ontology:constraint_victim(global_oil_supply_stability, renewable_transition_actors).
narrative_ontology:constraint_victim(global_oil_supply_stability, energy_dependent_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OIL-IMPORTING NATIONS (SNARE) — Structurally trapped. Energy infrastructure is already built around oil; switching costs are prohibitive (decades for renewable transition, trillions in capital). Global oil price shocks cause immediate economic damage with no credible exit path in biographical time. Maximum experienced extraction through price volatility, supply disruption risk, and forced alignment with producer preferences. Zero degrees of freedom.
constraint_indexing:constraint_classification(global_oil_supply_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY TRANSITION ECONOMIES (TANGLED ROPE) — Constrained but mobile across generational horizon. Genuine coordination function: global oil supply stabilizes energy infrastructure while transition alternatives mature. But also extraction: oil-dependent development pathway locks in infrastructure favoring fossil fuels; transition capital competes with oil subsidies and support for incumbent energy infrastructure. Significant agency but high cost to exit.
constraint_indexing:constraint_classification(global_oil_supply_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIVERSIFIED ENERGY COMPANIES (ROPE) — Organized actors with portfolio flexibility. Coordinate supply stability across oil, gas, renewables, nuclear. Experience oil supply constraint as pure coordination mechanism — stabilizing one commodity enables investment in others. Can arbitrage between markets; low switching cost. Net beneficiary from the coordination function.
constraint_indexing:constraint_classification(global_oil_supply_stability, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: OIL PRODUCING STATES (ROPE) — Institutional actors benefiting from supply coordination. OPEC production quotas and informal coordination among major producers (Russia, Saudi Arabia, Iraq) function as a collective action solution to the prisoner's dilemma of unlimited production. The constraint is coordination from their perspective: managing the supply curve to maintain prices above extraction costs. Arbitrage options available (switching between production levels, alliances, storage). Net beneficiaries.
constraint_indexing:constraint_classification(global_oil_supply_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RENEWABLE ENERGY COALITION (SCAFFOLD) — Organized actors (IEA, IRENA, climate policy advocates, renewable developers) see global oil supply stability as a temporary constraint with a sunset. The constraint is sunsetting as renewable costs fall below fossil fuels, battery technology scales, and carbon pricing internalizes externalities. Generational horizon — 30-50 years for full transition. Structured exit path: declining oil dependency, rising renewable penetration, phase-out of fossil fuel subsidies. Theater is moderate because transition infrastructure is being actively built.
constraint_indexing:constraint_classification(global_oil_supply_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL ENERGY INSTITUTIONS (PITON) — IEA, OPEC, IAEA maintain elaborate monitoring and scenario-building apparatus around oil supply. The institutional function (price reporting, supply forecasting, strategic reserve management) is largely performative at civilizational scale — it communicates stability but does not fundamentally alter production decisions or geopolitical risk. Theater ratio 0.48 reflects that coordination is still genuine (supply data genuinely matters for investment), but much institutional activity is maintaining a narrative of managed stability rather than controlling underlying variables. Piton classification: institution persists through inertia (legacy role) while actual coordination increasingly happens through market mechanisms and bilateral deals.
constraint_indexing:constraint_classification(global_oil_supply_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ENERGY-DEPENDENT POOR (SNARE) — Structurally trapped. Oil price spikes directly reduce purchasing power for fuel and transportation; no alternatives available (cannot switch to renewables without capital investment). Immediate time horizon — crisis occurs within months. Global scope because oil prices are globally indexed. Maximum extraction with zero perceived agency.
constraint_indexing:constraint_classification(global_oil_supply_stability, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: INTEGRATED PETROLEUM MAJORS (TANGLED ROPE) — Powerful institutional actors (Shell, ExxonMobil, Saudi Aramco, ADNOC). Genuine coordination function: global oil market infrastructure (pipelines, refineries, distribution networks) is standardized and scaled through incumbent majors. But also extraction: market structure allows supermajors to capture downstream rents; barriers to entry protect incumbent positions; regulatory capture enables favorable taxation and environmental terms. Mobile — can diversify into renewables — but extraction premium from oil infrastructure lock-in makes exit costly.
constraint_indexing:constraint_classification(global_oil_supply_stability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, oil supply stability reflects immutable geological and technological constraints: oil is a dense energy carrier requiring massive infrastructure; its extraction, refining, and distribution are capital-intensive and long-cycle; geopolitical concentration of reserves creates structural inequality in bargaining power. This perspective sees the constraint as an unavoidable feature of the Anthropocene energy transition. However, structural data contradicts the mountain classification — the engine will flag this as a false summit, revealing that natural-seeming constraints are actually the product of deliberate choice (infrastructure investment, subsidy allocation, regulatory capture).
constraint_indexing:constraint_classification(global_oil_supply_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_oil_supply_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_oil_supply_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_oil_supply_stability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_oil_supply_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_oil_supply_stability, TR),
    TR >= 0.70.

:- end_tests(global_oil_supply_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint genuinely coordinates supply, reducing catastrophic underinvestment and price crashes, which benefits all agents. But the coordination function is packaged with significant extraction: (1) OPEC/major producers capture geological scarcity rents (legitimate) plus cartel-enforced price premium (extraction); (2) supermajors maintain oligopolistic position in processing/distribution; (3) oil-dependent nations face externalized climate costs plus geopolitical vulnerability. The 0.58 value reflects that genuine coordination (justifying ~0.20-0.30 baseline) is layered with significant but not dominant extraction (adding ~0.30-0.35). Suppression (0.62): High. Multiple suppression mechanisms operate simultaneously: (1) infrastructure lock-in — oil infrastructure is capital-intensive and long-lived, creating high switching costs for importing nations; (2) geopolitical leverage — control of reserves and chokepoints enables coercion; (3) subsidy structures — many producing states subsidize domestic fuel, making alternative energy uncompetitive without policy intervention; (4) political capture — oil industry influences energy policy across importing nations. Suppression is not absolute (renewable transition is occurring), but credible alternative pathways require decades and active policy support. Theater ratio (0.48): Moderate. Supply coordination through IEA, OPEC, and national energy agencies has genuine information function — price discovery, inventory management, scenario planning have real coordination value. But increasingly, actual price discovery happens through futures markets and bilateral deals; institutional theater consists of producing supply forecasts and strategic narrative about 'stable markets.' The theater_ratio trajectory (rising from 0.35 to 0.52 at peak, falling to 0.45) reflects growth of institutional coordination narrative during 2010-2015 (IEA expanded, OPEC more vocal) followed by replacement of institutional narrative with market-based price discovery as renewable energy and shale production decentralized decision-making. Claimed type (tangled_rope): Justified by the presence of (1) genuine coordination function — stabilizing supply across long capital cycles; (2) active enforcement — OPEC production quotas, strategic reserve management, bilateral coordination among majors; (3) asymmetric distribution — benefits concentrate to producing states and majors, costs concentrate to importing nations and vulnerable populations.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Oil-producing states experience pure coordination (Rope) — they are solving their own prisoner's dilemma of avoiding destructive overproduction. Diversified energy companies experience coordination (Rope) — stable oil prices enable investment in renewables and other energy sources. But oil-importing nations without transition pathways experience pure extraction (Snare) — forced price-taker status with no agency. Renewable energy coalitions see a temporary problem with a sunset (Scaffold) — falling costs and accelerating transition mean the constraint's extraction mechanism is eroding. Energy-dependent poor experience maximum extraction (Snare) with zero perceived alternatives. Integrated majors experience mixed coordination and extraction (Tangled Rope) — they benefit from supply stability AND from oligopolistic position; their mobile exit option (to renewables) exists but is not costless. International institutions perform coordination narrative (Piton) — institutional apparatus persists despite declining real function as markets increasingly price supply. The analytical observer risks seeing natural law (Mountain) — geological scarcity appears immutable — but the structural data reveals this as false summit: the extraction mechanisms (cartelization, geopolitical leverage, subsidy structures, infrastructure lock-in) are socially constructed, not geological inevitabilities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position within the extraction flow. Oil-producing states as net beneficiaries with arbitrage options (mobile exit): d ≈ 0.10-0.15 (beneficiary location). Integrated majors as beneficiaries but with switching costs (mobile but constrained by asset base): d ≈ 0.25-0.35 (partial beneficiary). Oil-importing nations as victims with trapped exit: d ≈ 0.90+ (maximum victim location). Renewable transition actors as organized agents with exit pathway: d ≈ 0.55-0.65 (victim location but high agency). Energy-dependent poor as powerless victims: d ≈ 0.95+ (maximum victim location). The sigmoid f(d) transforms these position parameters into experienced extractiveness multipliers, with the formula χ = ε × f(d) × σ(S). For trapped importing nations: high d generates high f(d) ~1.35-1.42, which amplifies the base extractiveness (0.58) to experienced χ ≈ 0.78-0.85 despite moderate base rate. For beneficiary states with arbitrage: low d generates f(d) ~-0.05 to 0.20, which dampens or reverses the extraction signal, producing χ ≈ -0.03 to 0.12 (coordination signal). This explains the perspectival gap: same base extractiveness (0.58) produces snare experience (χ > 0.70) for victims and rope experience (χ < 0.35) for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint exhibits all six types legitimately. The resolution does not require choosing one type as 'correct' but understanding the presheaf: each agent's classification is their true experienced constraint structure given their structural position. Oil-producing states genuinely coordinate supply (Rope). Importing nations genuinely experience extraction without perceived exit (Snare). Renewable coalitions genuinely see a sunset pathway (Scaffold). Integrated majors genuinely face mixed coordination and extraction (Tangled Rope). Institutions genuinely perform partially degraded functions (Piton). The analytical observer genuinely risks naturalizing contingency as law (false summit Mountain). No mandatrophy exists — the six types are six truthful measurements of the same constraint from six different structural positions. The constraint's identity is the presheaf over these perspectives, not any single point in the space.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_legitimate_scarcity_rent,
    'Is the price premium captured by oil-producing states and majors a legitimate return to scarce geological assets, or extractive rent enforced through cartelization and geopolitical leverage?',
    'Comparative analysis: oil prices relative to extraction costs, refining margins, and transport costs; correlation between OPEC coordination strength and price spreads; counterfactual modeling of prices under perfect competition',
    'If mostly legitimate scarcity rent: constraint reclassifies toward Rope from more perspectives. If mostly extraction rent: constraint reclassifies toward Snare from more perspectives. Determines whether beneficiary capture is structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimate_scarcity_rent, empirical, 'Legitimate scarcity rent versus extractive cartel rent').

omega_variable(
    renewable_transition_timeline_credibility,
    'Is the scaffold sunset (30-50 year energy transition) a genuine structural path or aspirational narrative lacking credible financing and political commitment?',
    'Tracking of renewable capacity additions, battery deployment rates, and carbon pricing trajectory against Net Zero scenarios; monitoring of fossil fuel subsidy removal vs maintained support; assessment of political consensus stability across electoral cycles',
    'If genuinely credible: scaffold perspective confirmed. If aspirational without enforcement mechanisms: constraint persists beyond sunset and scaffold reclassifies to Rope or Tangled Rope. Affects directionality of transition economies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_transition_timeline_credibility, empirical, 'Whether renewable transition has credible timeline and financing').

omega_variable(
    geopolitical_leverage_contingency,
    'How much of the extraction mechanism depends on specific geopolitical vulnerabilities (Middle East centrality, chokepoint straits, OPEC cohesion) versus fundamental scarcity?',
    'Scenario analysis: oil supply stability under alternative geopolitical configurations (China-dominated Middle East, U.S. energy independence, OPEC dissolution, diversified supply sources). Historical correlation between geopolitical shocks and price spikes. Long-run elasticity of supply responses to price signals.',
    'If mostly geopolitical contingency: constraint is contingently remediable through alliance restructuring, diversification, or strategic decoupling. If mostly fundamental scarcity: constraint is durable. Affects classification of importing nations — trapped vs constrained distinction hinges on whether exit is geopolitically or physically blocked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_leverage_contingency, empirical, 'Extent to which extraction depends on geopolitical leverage versus fundamental scarcity').

omega_variable(
    infrastructure_lock_in_reversibility,
    'How quickly can oil-dependent energy infrastructure be transitioned or repurposed? What is the true switching cost for importing nations?',
    'Detailed lifecycle analysis of power generation fleets, vehicle fleets, heating infrastructure; capital replacement timelines; technical compatibility of renewable integration with existing grids; case study analysis of early-transition economies (Denmark, Costa Rica, Iceland)',
    'If high reversibility and low switching cost: trapped classification becomes constrained or mobile. If low reversibility and extremely high cost: trapped classification is correct and persists. Directly affects biographical horizon assessment for importing nations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_lock_in_reversibility, empirical, 'Reversibility of oil infrastructure lock-in and true switching costs').

omega_variable(
    supermajor_transition_credibility,
    'Can integrated petroleum majors genuinely transition to renewable-dominant business models, or do their organizational capabilities and asset bases make them structurally dependent on fossil fuels?',
    'Tracking of major capital allocation to renewables vs fossil; organizational capability in low-carbon technologies; empirical comparison of transition progress across majors; scenarios from energy modeling groups about future energy mix',
    'If credible transition: majors'' mobile exit option is genuine, tangled_rope classification holds. If constrained transition: mobile classification overstates agency, classification becomes rope or snare. Affects understanding of who captures long-term rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajor_transition_credibility, empirical, 'Credibility of petroleum major transition to renewables').

omega_variable(
    false_summit_natural_law_claim,
    'Is global oil supply stability a natural law of energy infrastructure (geological scarcity + capital requirements), or a socially constructed constraint naturalizing economic and geopolitical choices?',
    'Examining whether the constraint persists without beneficiary enforcement; counterfactual analysis of energy development pathways that might have been taken; historical sociology of oil infrastructure decision-making; assessment of which components are truly unavoidable versus which reflect deliberate design choices',
    'If natural law: mountain classification is justified. If socially constructed: false summit signature fires, engine reclassifies to tangled_rope or snare. Affects entire analytical framing of whether ''stability'' is achievable or whether instability is a feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Natural law versus socially constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_oil_supply_stability, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oilstab_tr_t0, global_oil_supply_stability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(oilstab_tr_t15, global_oil_supply_stability, theater_ratio, 15, 0.48).
narrative_ontology:measurement(oilstab_tr_t30, global_oil_supply_stability, theater_ratio, 30, 0.52).
narrative_ontology:measurement(oilstab_tr_t45, global_oil_supply_stability, theater_ratio, 45, 0.45).

% Extraction over time
narrative_ontology:measurement(oilstab_be_t0, global_oil_supply_stability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(oilstab_be_t15, global_oil_supply_stability, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(oilstab_be_t30, global_oil_supply_stability, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(oilstab_be_t45, global_oil_supply_stability, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(oilstab_su_t0, global_oil_supply_stability, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(oilstab_su_t15, global_oil_supply_stability, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(oilstab_su_t30, global_oil_supply_stability, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(oilstab_su_t45, global_oil_supply_stability, suppression_requirement, 45, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_oil_supply_stability, resource_allocation).
narrative_ontology:affects_constraint(global_oil_supply_stability, opec_production_coordination).
narrative_ontology:affects_constraint(global_oil_supply_stability, petrodollar_currency_regime).
narrative_ontology:affects_constraint(global_oil_supply_stability, renewable_energy_transition).
narrative_ontology:affects_constraint(global_oil_supply_stability, geopolitical_middle_east_stability).
narrative_ontology:affects_constraint(global_oil_supply_stability, climate_externality_pricing).

% DUAL FORMULATION NOTE:
% The global oil supply stability constraint decomposes into sub-constraints based on observable: (1) supply_price_coordination (ε ≈ 0.30, Rope) — the genuine coordination of investment and production across political borders; (2) cartel_rent_extraction (ε ≈ 0.65, Snare) — the enforced monopoly premium on oil pricing; (3) infrastructure_lock_in (ε ≈ 0.70, Mountain-candidate) — the apparent immutability of existing energy infrastructure. The aggregate story (this file) treats them as unified because they operate through the same mechanisms (OPEC coordination, supply uncertainty, geopolitical leverage) but the ε-invariance principle allows decomposition into separate stories if research focuses on one observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_oil_supply_stability, institutional, 0.15).
constraint_indexing:directionality_override(global_oil_supply_stability, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
