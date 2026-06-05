% ============================================================================
% CONSTRAINT STORY: arctic_resource_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_resource_competition, []).

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
 *   constraint_id: arctic_resource_competition
 *   human_readable: Arctic Resource Competition and Geopolitical Extraction
 *   domain: geopolitical/economic/environmental
 *
 * SUMMARY:
 *   Arctic resource competition has transformed from a geopolitically
 *   marginal constraint into a central site of great-power extraction,
 *   indigenous rights conflicts, and climate tipping-point dynamics. Climate
 *   change has lowered ice barriers to extraction, making petroleum, rare
 *   earth minerals, and shipping routes newly accessible. This creates a
 *   multi-layered constraint that couples genuine coordination problems
 *   (maritime safety, territorial clarity, environmental baseline-setting)
 *   with severe asymmetric extraction of resources, decision-making
 *   authority, and externalized climate harm. The constraint exhibits the
 *   full range of DR types across institutional and community perspectives:
 *   states experience rope (pure coordination benefits), extractive
 *   industries experience tangled rope (mixed coordination and market power),
 *   indigenous communities experience snare (trapped extraction with no
 *   exit), environmental organizations experience tangled rope (constrained
 *   advocacy with some institutional inclusion), developing coastal nations
 *   experience snare (trapped by climate harms with no voice), and organized
 *   transition movements experience scaffold (temporary constraint with real
 *   sunset through renewable energy substitution). The analytical observer
 *   risks seeing a mountain (inevitable thermodynamic opening of Arctic
 *   resources) — but structural data reveals this as a false summit
 *   naturalizing a distributional choice. The constraint's extractiveness has
 *   risen from 0.28 (pre-climate intensification) to 0.58 (current peak
 *   extraction phase) as climate change has both lowered ice barriers and
 *   raised suppression requirements (indigenous resistance, environmental
 *   opposition, geopolitical competition). The theater ratio's rise from 0.32
 *   to 0.58 reflects increasing performative inclusion (Arctic Council
 *   indigenous voices, environmental impact statements) alongside functional
 *   subordination (extraction proceeds despite stated consultation) — a
 *   classic marker of tangled rope theater escalation.
 *
 * KEY AGENTS:
 *   - Arctic State Governments (U.S., Canada, Russia, Nordic states): Primary beneficiaries (institutional/arbitrage) — capture exclusive territorial control, resource extraction rights, shipping route authority, military strategic advantage
 *   - Multinational Extractive Industries: Secondary beneficiaries (powerful/mobile) — gain access to high-margin petroleum, rare earth minerals, and lower-cost shipping routes; mobile exit options via alternative extraction frontiers
 *   - Indigenous Arctic Communities: Primary victims (powerless/trapped) — face extraction of land access, water quality, subsistence resources, cultural integrity; trapped by relocation barriers and legal subordination
 *   - Circumpolar Indigenous Organizations: Constrained advocates (organized/constrained) — benefit from transnational coordination but structurally subordinated in governance decision-making
 *   - Developing Coastal Nations: Secondary victims (powerless/trapped) — bear climate externalities (sea-level rise, weather instability, fisheries collapse) driven by Arctic methane release and cumulative extraction
 *   - Environmental and Climate Organizations: Organized advocates (organized/constrained) — constrained by state dominance but organized through transnational networks; pushing sunset through renewable transition
 *   - Global Transition Movement: Organized agents (organized/constrained) — represent real structural challenge to extraction through economic substitution and regulatory pressure; scaffold perspective with genuine sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_resource_competition, 0.58).
domain_priors:suppression_score(arctic_resource_competition, 0.62).
domain_priors:theater_ratio(arctic_resource_competition, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_resource_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(arctic_resource_competition, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(arctic_resource_competition, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_resource_competition, tangled_rope).
narrative_ontology:human_readable(arctic_resource_competition, "Arctic Resource Competition and Geopolitical Extraction").
narrative_ontology:topic_domain(arctic_resource_competition, "geopolitical/economic/environmental").

domain_priors:requires_active_enforcement(arctic_resource_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_resource_competition, advanced_industrial_states).
narrative_ontology:constraint_beneficiary(arctic_resource_competition, extractive_industries).
narrative_ontology:constraint_victim(arctic_resource_competition, indigenous_arctic_communities).
narrative_ontology:constraint_victim(arctic_resource_competition, arctic_ecosystem_integrity).
narrative_ontology:constraint_victim(arctic_resource_competition, global_climate_stability).
narrative_ontology:constraint_victim(arctic_resource_competition, developing_coastal_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS ARCTIC COMMUNITIES (SNARE) — Trapped within territories targeted for resource extraction. Face extraction of land access, water quality, and subsistence resources with minimal exit options (relocation is culturally destructive and economically nonviable). Suppression is both structural (legal claims subordinated to development rights) and internalized (historical dispossession normalized through treaties that prioritized settler sovereignty). No meaningful coordination benefit — the 'infrastructure development' serves external actors' supply chains, not community needs.
constraint_indexing:constraint_classification(arctic_resource_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIRCUMPOLAR INDIGENOUS ORGANIZATIONS (TANGLED ROPE) — Constrained by resource scarcity and state control of territorial governance, but benefit from transnational coordination mechanisms (Arctic Council, UNPFII, indigenous treaties). Extraction occurs through subordinated voice in decision-making and land-use allocation, but genuine coordination occurs in knowledge-sharing about sustainable practices and collective advocacy. The constraint functions as both exclusion from power and partial inclusion in legitimacy-granting institutions.
constraint_indexing:constraint_classification(arctic_resource_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: ARCTIC STATE GOVERNMENTS (ROPE) — Primary beneficiaries with arbitrage optionality. Benefit from exclusive territorial claims, resource extraction rights, and shipping route control. Experience the constraint as pure coordination: establishing territorial boundaries, maritime navigation standards, and extraction protocols solves genuine collective action problems (preventing accidents, establishing claim priority, managing shared fish stocks). Suppression is minimal from this perspective — the state possesses enforcement capacity.
constraint_indexing:constraint_classification(arctic_resource_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENVIRONMENTAL AND CLIMATE ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Constrained by state dominance of territorial governance and weak international enforcement mechanisms, but organized through transnational networks and scientific consensus. The constraint offers genuine coordination (UNFCCC Arctic protocols, environmental impact assessments) alongside extraction: climate change impacts are externalized to vulnerable populations and future generations while extraction benefits concentrate on resource-accessing states. Exit is constrained — advocacy organizations lack enforcement power, but have real political voice in framing legitimacy.
constraint_indexing:constraint_classification(arctic_resource_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPING COASTAL NATIONS AND FUTURE GENERATIONS (SNARE) — Trapped by global climate impacts (sea-level rise, weather instability, fish stock disruption) driven by Arctic methane release and cumulative fossil fuel extraction. No exit options and no coordination benefit — these agents bear the cost of acceleration through Arctic resource extraction while having no voice in decision-making. Suppression is structural (lack of voting power in territorial governance) and systemic (climate damages outpace adaptation capacity).
constraint_indexing:constraint_classification(arctic_resource_competition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTINATIONAL EXTRACTIVE INDUSTRIES (TANGLED ROPE) — Mobile with powerful market position and legal frameworks favorable to extraction. Benefit from Arctic resource access but depend on coordination with states and communities (infrastructure investment, regulatory predictability, social license). Extraction occurs through monopolistic control of technology and market access; coordination occurs through investment in local infrastructure and employment (however unequally distributed). Exit is highly mobile — can relocate extraction to alternative frontiers if Arctic becomes politically or environmentally costly.
constraint_indexing:constraint_classification(arctic_resource_competition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: GLOBAL TRANSITION MOVEMENT AND CLIMATE GOVERNANCE (SCAFFOLD) — Organized agents (renewable energy coalitions, carbon-reduction protocols, alternative shipping technologies) represent a structural challenge to Arctic resource extraction through technological substitution and regulatory pressure. The constraint has a real sunset clause: as renewable energy costs fall, carbon pricing rises, and Arctic shipping becomes politically unacceptable, the extraction incentives for Arctic resources degrade. Low effective extraction because exit mechanisms (renewable substitutes) are materially present and politically mobilized. Theater is moderate — coordination around green transition is both performative (carbon markets, COP pledges) and functionally real (renewable deployment, grid transformation).
constraint_indexing:constraint_classification(arctic_resource_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Arctic resource competition reflects immutable thermodynamic and geophysical constraints: climate change has lowered extraction costs through ice melt, making Arctic resources thermodynamically accessible for the first time. This perspective naturalizes the competition as an inevitable outcome of energy-economics physics. However, false summit alert: the structural data shows clear beneficiaries (states, extractive industries) and victims (indigenous communities, future generations, developing nations). The 'inevitability' framing is a beneficiary claim designed to preempt political resistance to what is fundamentally a distributional choice.
constraint_indexing:constraint_classification(arctic_resource_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_resource_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_resource_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_resource_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_resource_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arctic_resource_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Arctic resource competition combines genuine resource value (petroleum, rare earths, shipping routes) with genuine coordination problems (maritime safety, territorial clarity). The extraction component is substantial but not total — states and industries capture the surplus, but some coordination benefits are distributed (e.g., safety protocols, environmental impact monitoring). The rising trajectory (0.28 → 0.58) reflects climate-driven cost reduction making extraction more profitable and therefore more extractive. Suppression (0.62): High. Multiple suppression mechanisms operate simultaneously: structural (indigenous communities lack enforcement power in territorial governance, developing nations lack Arctic seat at decision table), legal (territorial sovereignty doctrine privileges state claims over indigenous authority), military (Arctic militarization raises barriers to indigenous assertion of alternative authority), and institutional (international frameworks like Arctic Council grant indigenous voice but not veto power). The rising suppression trajectory (0.38 → 0.62) reflects both intensifying indigenous/environmental resistance and intensifying state/industry enforcement capacity to suppress that resistance. Theater ratio (0.58): Moderate-high. Significant performative components: indigenous consultation without binding consent, environmental impact assessments conducted by extraction proponents, Arctic Council statements on sustainability while major extraction projects proceed. But not pure theater — some genuine coordination functions exist (icebreaker communication standards, baseline environmental monitoring, maritime navigation rules). Theater has risen as rhetorical inclusion (Arc Council indigenous participation, sustainability pledges) has increased while functional authority remains concentrated in states and industries.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the structural divergence that makes tangled rope diagnostically necessary. Arctic states and extractive industries see rope — coordination benefits dominate their experience because they control enforcement and capture surplus. Circumpolar indigenous organizations and environmental advocates see tangled rope — constrained by state dominance but organized enough to negotiate partial inclusion, genuine coordination in knowledge-sharing and advocacy, but extraction through subordinated voice and externalized harm. Indigenous communities trapped in extraction zones see snare — no coordination benefit, full extraction of land and subsistence access, insurmountable structural and legal barriers to exit. Developing coastal nations see snare — no voice in Arctic governance, bearing full climate externalities from extraction. The global transition movement sees scaffold — the constraint has a real sunset through economic substitution as renewable energy costs fall and carbon pricing rises, sunset is materially driven (not merely rhetorical), but not yet triggered because extraction remains politically favored and suppression of transition advocacy remains high. The analytical observer risks mountain — naturalizing Arctic resource extraction as inevitable thermodynamic consequence of climate opening. But the structural data reveals false summit: clear beneficiaries (states, industries) capturing surplus, clear victims (communities, future generations, developing nations) bearing costs, clear enforcement mechanisms (legal, military, institutional) suppressing alternative authority. This is not natural law — it is contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Arctic states, as primary beneficiaries with arbitrage optionality, have low d (0.05-0.15) — they are beneficiaries receiving net benefit from the constraint. Extractive industries, as powerful beneficiaries with mobile exit options, have low d (0.10-0.20) — they benefit but can exit to alternative extraction frontiers. Indigenous communities, as trapped victims with no exit options, have high d (0.90-0.95) — they bear maximum extraction with no ability to escape. Circumpolar indigenous organizations, as organized agents with constrained exit, have moderate-high d (0.65-0.75) — they are partially beneficial to ecosystem coordination but structurally subordinated in resource allocation. Developing coastal nations, as powerless victims with no Arctic agency, have high d (0.85-0.90) — they bear climate externalities with no voice in causing decisions. Environmental organizations, as organized agents with analytical exit options, have moderate d (0.60-0.70) — they experience extraction through suppression of advocacy but have real advocacy capacity and exit pathways (transition to alternative institutions). The engine computes chi from these d values via f(d) sigmoid and scope modifiers — high-d agents (trapped indigenous communities, powerless coastal nations) experience high chi despite moderate base extractiveness because f(d) amplifies their experience. Low-d agents (beneficiary states, arbitrage industries) experience low or negative chi because f(d) dampens their experience. This perspectival divergence in chi despite uniform base extractiveness is the core diagnostic signal for tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition: the constraint is simultaneously tangled rope (for most institutional and organized agents) and snare (for powerless agents trapped in extraction zones) and rope (for pure beneficiary states) and scaffold (for transition advocates). There is no single classification that captures the full structure. The claim that Arctic resource competition is 'inevitable coordination' (mountain or rope) fails to account for victims and false summit markers. The claim that it is 'pure extraction' (snare) fails to account for genuine coordination functions in maritime safety and territorial clarity. The tangled rope classification is correct because it captures both genuine coordination (infrastructure, navigation, environmental baseline-setting) and asymmetric extraction (resource surplus concentration, decision-making subordination, externalized climate harm) existing simultaneously. The rising extractiveness trajectory (0.28 → 0.58) confirms that the extraction component has intensified while coordination functions have remained relatively flat — classic tangled rope degradation pattern indicating the constraint is becoming more extractive and less coordinative over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_consent_governance_scope,
    'Do international legal frameworks recognize indigenous communities'' authority to withhold consent for extraction, or do they reserve that authority to Arctic states?',
    'Comparative analysis of UNDRIP implementation, Arctic Council protocols, and state-level consent frameworks; tracking cases where indigenous communities successfully blocked or rerouted projects',
    'If indigenous authority is recognized: the constraint reclassifies from snare (for indigenous agents) toward tangled rope — genuine coordination occurs. If states retain veto power: indigenous perspectives remain snare; governance is performatively inclusive but decisionally subordinated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_consent_governance_scope, conceptual, 'Whether indigenous communities possess enforceable consent authority in Arctic governance').

omega_variable(
    climate_cost_internalization_threshold,
    'At what carbon price level do Arctic extraction costs exceed renewable alternatives, triggering economic collapse of the extraction constraint?',
    'Lifecycle cost analysis of Arctic extraction vs. renewable energy systems; sensitivity analysis of carbon prices and extraction technology costs; economic modeling of alternative energy transitions',
    'If threshold is low ($40-80/ton CO2): scaffold perspective is robust and sunset is materially driven. If threshold is high (>$150/ton): scaffold is aspirational; extraction persists through political extraction (subsidies, externalities) rather than market logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_cost_internalization_threshold, empirical, 'Economic threshold where Arctic extraction becomes uncompetitive with renewables').

omega_variable(
    geopolitical_military_extraction_decoupling,
    'Can Arctic shipping and resource routes be governed independently of military strategic competition, or do they remain inseparable from great-power extraction dynamics?',
    'Analysis of Arctic Council enforcement capacity vs. military presence; historical tracking of how commercial vs. military interests shape policy; counterfactual analysis of demilitarization scenarios',
    'If decoupling is possible: extractiveness can decline through commercial governance. If inseparable: geopolitical suppression will persist regardless of economic factors — the constraint remains snare/tangled rope at state level despite renewable energy transitions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_military_extraction_decoupling, conceptual, 'Whether Arctic commercial governance can decouple from military competition').

omega_variable(
    methane_feedback_tipping_point,
    'Does Arctic methane release from permafrost thaw create an irreversible tipping point that accelerates climate change regardless of subsequent emission reductions?',
    'Paleoclimate data on methane dynamics; current permafrost monitoring and feedback models; IPCC assessments of tipping point probability and timeline',
    'If tipping point is likely/imminent: powerless agents (future generations, coastal nations) are already locked into severe harm — the classification cannot improve regardless of policy change. If reversible or far-distant: adaptation and transition policies retain meaningful efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methane_feedback_tipping_point, empirical, 'Whether Arctic methane feedback creates irreversible climate tipping point').

omega_variable(
    indigenous_knowledge_economic_value_capture,
    'Do international benefit-sharing mechanisms (Nagoya Protocol, UNDRIP resource rights) actually transfer economic value to indigenous communities, or do they remain extractive through asymmetric access to global markets?',
    'Tracking of benefit-sharing payments, intellectual property licensing, and market access for indigenous-derived products; comparison of indigenous income shares in bioeconomy vs. global profit distribution',
    'If value transfer is real: coordination function is genuine and becomes more visible in measurements. If token: the constraint remains extractive even under benefit-sharing framing — the theater ratio stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_knowledge_economic_value_capture, empirical, 'Whether benefit-sharing mechanisms genuinely transfer economic value to indigenous communities').

omega_variable(
    state_monopoly_on_legitimacy_foreclosure,
    'Can Arctic indigenous governance frameworks acquire state-equivalent enforcement authority (e.g., through co-management with binding indigenous veto), or does territorial sovereignty remain fundamentally monopolized by states?',
    'Empirical tracking of co-management experiments (e.g., Inuit Tapiriit Kanatami joint authority in Canada); analysis of whether indigenous veto power has blocked projects; comparison with decolonization models in other regions',
    'If indigenous authority can scale: the snare perspective for indigenous communities reclassifies toward tangled rope or rope as genuine power-sharing occurs. If sovereignty remains state-monopolized: indigenous perspectives remain snare regardless of rhetorical inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_on_legitimacy_foreclosure, conceptual, 'Whether indigenous governance can acquire state-equivalent enforcement authority in Arctic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_resource_competition, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arctic_theater_t0_functional_coordination, arctic_resource_competition, theater_ratio, 0, 0.32).
narrative_ontology:measurement(arctic_theater_t15_rhetorical_inclusion, arctic_resource_competition, theater_ratio, 15, 0.48).
narrative_ontology:measurement(arctic_theater_t30_sustainability_performance, arctic_resource_competition, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(arctic_extractiveness_t0_preclimate, arctic_resource_competition, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(arctic_extractiveness_t15_climatedriven, arctic_resource_competition, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(arctic_extractiveness_t30_peak_extraction, arctic_resource_competition, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arctic_suppression_t0_early, arctic_resource_competition, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(arctic_suppression_t15_organized_resistance, arctic_resource_competition, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(arctic_suppression_t30_intense_conflict, arctic_resource_competition, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_resource_competition, resource_allocation).
narrative_ontology:affects_constraint(arctic_resource_competition, indigenous_land_tenure_security).
narrative_ontology:affects_constraint(arctic_resource_competition, arctic_methane_emissions_feedback).
narrative_ontology:affects_constraint(arctic_resource_competition, polar_shipping_route_geopolitics).
narrative_ontology:affects_constraint(arctic_resource_competition, rare_earth_mineral_supply_chains).

% DUAL FORMULATION NOTE:
% Arctic resource competition decomposes into multiple structurally distinct constraints: (1) indigenous land tenure security (ε≈0.70, snare-dominant, indigenous perspective); (2) Arctic methane emissions feedback (ε≈0.05, mountain-dominant, thermodynamic constraint); (3) polar shipping route geopolitics (ε≈0.52, tangled rope, multistate coordination); (4) rare earth mineral supply chains (ε≈0.48, tangled rope, industrial coordination). The present story integrates these at the meta-level of the overall Arctic system. Each decomposed story has distinct metrics, victim/beneficiary structures, and temporal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_resource_competition, institutional, 0.08).
constraint_indexing:directionality_override(arctic_resource_competition, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
