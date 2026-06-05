% ============================================================================
% CONSTRAINT STORY: mongolian_resource_extraction_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mongolian_resource_extraction_sovereignty, []).

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
 *   constraint_id: mongolian_resource_extraction_sovereignty
 *   human_readable: Mongolian Resource Extraction Sovereignty Constraint
 *   domain: economic/geopolitical/environmental
 *
 * SUMMARY:
 *   Mongolia faces a structural constraint arising from the intersection of
 *   resource wealth, capital scarcity, geopolitical dependency, and unequal
 *   bargaining power with foreign mining corporations and Chinese demand. The
 *   constraint exhibits classical Tangled Rope characteristics: genuine
 *   coordination function (Mongolia needs foreign investment to develop its
 *   resources; corporations need reliable supply; China needs resource
 *   security) combined with asymmetric extraction (herding communities
 *   displaced, environmental costs externalized, long-term sovereignty
 *   degraded). The constraint is not pure extraction because Mongolia's
 *   central government genuinely benefits from resource rents and uses some
 *   proceeds for domestic development. However, it is not pure coordination
 *   because the distribution of costs and benefits is structurally unequal,
 *   with the most vulnerable populations bearing disproportionate extraction
 *   while foreign actors capture majority benefits. The theater ratio (0.55)
 *   reflects genuine governance infrastructure (mining law, environmental
 *   regulations, international standards compliance) that functions partially
 *   but is often honored in breach — enforcement capacity is weak, and
 *   powerful actors navigate around formal rules.
 *
 * KEY AGENTS:
 *   - Herding Communities: Primary victim (powerless/trapped/local) — lose grazing access, water quality, and long-term livelihood options with no compensation or exit pathway
 *   - Mongolian Central Government: Primary beneficiary and coordinating actor (institutional/constrained/national) — captures tax revenue and employment benefits but constrained by foreign capital dependence and Chinese demand dominance; simultaneously extracts from herding populations
 *   - Foreign Mining Corporations: Primary beneficiary (institutional/arbitrage/global) — profit from extraction under favorable concession terms; can relocate if terms change; genuine arbitrage options reduce experienced extraction
 *   - Chinese Industrial Demand: Secondary beneficiary (institutional/arbitrage/continental) — reliable resource supply reduces Chinese commodity costs; genuine arbitrage through alternative suppliers provides exit options
 *   - Civil Society and Environmentalists: Secondary victim (moderate/constrained/national) — organize resistance but face suppression; constrained by state capacity to sideline activism
 *   - Long-Term Mongolian Sovereignty: Abstract victim (powerless/trapped/regional/generational) — future resource autonomy and economic sovereignty degraded by present extraction dependency and environmental depletion
 *   - International Governance Framework: Institutional observer (institutional/arbitrage/global) — maintains performative legitimacy through treaty compliance but weak enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mongolian_resource_extraction_sovereignty, 0.58).
domain_priors:suppression_score(mongolian_resource_extraction_sovereignty, 0.62).
domain_priors:theater_ratio(mongolian_resource_extraction_sovereignty, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mongolian_resource_extraction_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(mongolian_resource_extraction_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(mongolian_resource_extraction_sovereignty, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mongolian_resource_extraction_sovereignty, tangled_rope).
narrative_ontology:human_readable(mongolian_resource_extraction_sovereignty, "Mongolian Resource Extraction Sovereignty Constraint").
narrative_ontology:topic_domain(mongolian_resource_extraction_sovereignty, "economic/geopolitical/environmental").

domain_priors:requires_active_enforcement(mongolian_resource_extraction_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mongolian_resource_extraction_sovereignty, foreign_mining_corporations).
narrative_ontology:constraint_beneficiary(mongolian_resource_extraction_sovereignty, chinese_industrial_demand).
narrative_ontology:constraint_beneficiary(mongolian_resource_extraction_sovereignty, mongolian_central_government).
narrative_ontology:constraint_victim(mongolian_resource_extraction_sovereignty, mongolian_herding_communities).
narrative_ontology:constraint_victim(mongolian_resource_extraction_sovereignty, local_environmental_systems).
narrative_ontology:constraint_victim(mongolian_resource_extraction_sovereignty, long_term_mongolian_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HERDING COMMUNITIES (SNARE) — Trapped by loss of grazing lands, water contamination, and lack of alternative livelihoods. Bears full extraction cost with no exit options. Cannot organize effectively against foreign corporate and state actors. Maximum experienced extraction.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CIVIL SOCIETY AND ENVIRONMENTALISTS (TANGLED ROPE) — Experiences both coordination (genuine environmental stewardship shared goals) and extraction (suppression of activism, regulatory capture, constrained exit through harassment). Organized but constrained by state capacity and corporate influence. Significant asymmetric extraction alongside some genuine coordination function.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOREIGN MINING CORPORATIONS (ROPE) — Experiences constraint as pure coordination: standardized royalty agreements, transparent regulatory frameworks, and rule of law enable profitable extraction. Genuine beneficiary with arbitrage options (can relocate operations or exit entirely). Sees the constraint as enabling their business model.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE INDUSTRIAL DEMAND (ROPE) — Experiences constraint as coordination of resource supply chains. Coordination function is genuine (Mongolia's mining is integrated into China's manufacturing supply chain). Net beneficiary with exit options (alternative suppliers exist, though less convenient). Arbitrage options drive low effective extraction from this perspective.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: MONGOLIAN CENTRAL GOVERNMENT (TANGLED ROPE) — Simultaneously coordinates domestic revenue generation (coordination function: tax revenue, employment, infrastructure investment) while extracting from herding communities through land alienation and environmental degradation. Benefits from resource rents but constrained by dependence on foreign capital and Chinese demand. Active enforcement of mining concessions against local resistance. Asymmetric extraction combined with coordination of fiscal functions.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LONG-TERM MONGOLIAN SOVEREIGNTY (SNARE) — Abstract institutional claim. Mongolia's future resource autonomy is trapped in extractive commodity dependence, constrained by Chinese demand dominance, foreign corporate control of extraction, and environmental degradation that reduces future options. No exit from structural dependency during biographical timescale. Maximum extraction from future agency.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 7: REGIONAL DIVERSIFICATION COALITION (SCAFFOLD) — Some Asian states (Vietnam, Laos) and international actors (World Bank, renewable energy investors) are building alternative supply chains and diversification pathways for Mongolia: renewable energy exports, agricultural product development, tourism. These represent genuine sunset mechanisms for the resource extraction constraint — Mongolia could transition to lower-extraction economic models. However, sunset is not yet active (still 10-20 years away). Coalition has mobile options but faces resistance from established extraction interests.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: INTERNATIONAL GOVERNANCE THEATER (PITON) — UN conventions on resource sovereignty, UNDRIP (UN Declaration on Rights of Indigenous Peoples), environmental covenants are largely performative. Mongolia has ratified these instruments but enforcement is absent due to capacity constraints and competing great-power interests. Theater ratio high (0.55+) — the institutional scaffolding appears robust but functions weakly. Maintains legitimacy through ritual adherence despite weak functional capacity.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / GEOPOLITICAL NATURAL LAW (MOUNTAIN) — From a civilizational/global perspective, resource-rich but capital-poor states face immutable constraints: they cannot fund extraction without foreign investment, they cannot build sovereign state capacity without resource revenues, and they cannot resist great-power commodity demand without alternative development models. This appears as natural law of geopolitical economy. However, this is a FALSE SUMMIT — the base properties reveal contingent institutional choices (regulatory capture, exclusionary concession models, extraction-first development strategy) naturalizing what are actually reversible structural arrangements. Vietnam and Indonesia show alternative models.
constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mongolian_resource_extraction_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mongolian_resource_extraction_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mongolian_resource_extraction_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mongolian_resource_extraction_sovereignty, TR),
    TR >= 0.70.

:- end_tests(mongolian_resource_extraction_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Foreign mining corporations capture approximately 55-65% of resource rents through favorable concession terms, tax incentives, and transfer pricing. Mongolian government captures 25-35% in royalties and taxes, with some proceeds invested domestically. Herding communities receive minimal compensation and face negative externalities. The base extractiveness reflects the unequal distribution where the most vulnerable populations bear costs while most benefits flow to foreign capital. This is higher than pure coordination (which would be ≤0.35) but not as severe as pure extraction (which would be ≥0.66), reflecting genuine mixed functions. Suppression (0.62): High. Significant barriers prevent alternative development paths: herding communities lack political organization capacity against state-backed corporations; environmental regulations lack enforcement; international legal frameworks (UNDRIP, environmental covenants) are honored symbolically but not functionally enforced; Chinese demand dominance constrains Mongolia's negotiating leverage; capital scarcity forces Mongolia to accept unfavorable terms. Theater ratio (0.55): Moderate-high. Mongolia has substantial formal governance infrastructure (mining law, environmental impact assessments, regulatory agencies, international treaty compliance) that appears robust but functions weakly in practice. Enforcement of environmental regulations against large corporations is inconsistent; community consultation processes are performative rather than substantive; government capacity constraints mean regulations are often unenforced. This represents meaningful but incomplete theater — some functional governance exists, but much is ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across nine perspectives. Foreign mining corporations see Rope (pure coordination enabling profitable resource access). Chinese demand sees Rope (integrated supply chain coordination). Mongolian government sees Tangled Rope (coordinates resource revenue with communities but extracts from herding populations). Civil society sees Tangled Rope (coordinates environmental stewardship principles but faces suppression). Herding communities see Snare (pure extraction with no exit). Long-term sovereignty sees Snare (trapped in extractive commodity dependency). International governance theater sees Piton (performative compliance with weak function). Regional diversification sees Scaffold (temporary extraction with renewable energy sunset). The analytical observer at civilizational scale risks seeing Mountain (natural law of geopolitical economy: poor states cannot resist great powers' resource demands) — but this is a false summit, as Vietnam and Indonesia demonstrate alternative models. The perspectival gap reveals that the constraint's type depends entirely on structural position: beneficiaries with exit see coordination; victims without exit see extraction; the institutional observer sees mixed functions; the analytical observer risks naturalizing contingent arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural relationships: Foreign mining corporations are beneficiaries with arbitrage options (can exit or relocate) → low d → low/negative χ. Herding communities are victims with no exit (trapped) → high d → high χ (maximum experienced extraction). Mongolian government is mixed: beneficiary of resource rents (low d component) but also victim of Chinese demand dominance and capital dependency (higher d component) → moderate d overall. The Chinese state is beneficiary with arbitrage options (alternative suppliers exist, though less convenient) → low d. Civil society is organized but constrained by state suppression → moderate d. The analytical observer computing d from beneficiary/victim status plus exit options produces the perspectival differentiation: those with arbitrage options experience low χ even if formally subject to the constraint; those with no exit experience high χ. The pipeline applies the sigmoid f(d) to these d values, producing different experienced extractiveness for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the constraint is simultaneously Rope for beneficiaries (genuine coordination function), Tangled Rope for intermediate agents (mixed extraction-coordination), and Snare for trapped victims. This is not contradiction but perspectival specificity. The same institutional arrangement can be seen as enabling coordination (from the beneficiary's viewpoint) while delivering extraction (from the victim's viewpoint) — both are accurate relative to structural position. The false summit risk appears in the analytical/civilizational perspective: the claim that resource extraction is 'natural law' for capital-poor states naturalizes what are actually contingent policy choices (extraction-first development, foreign-controlled concessions, lack of revenue diversification, minimal environmental protection). The constraint resolves the mandatrophy by showing that the institutional design contains genuine coordination functions (Mongolia needs investment capital; corporations need supply security; integration enables development) but the distribution of extracted value and externalized costs is neither natural nor immutable. The theater ratio (0.55) indicates substantial performative content — the governance infrastructure appears robust but enforcement is weak, suggesting room for institutional redesign to shift the constraint toward more genuine coordination and less asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''resource extraction sovereignty'' defined as formal state ownership, effective extraction control, or long-term resource security?',
    'Legal analysis of Mongolian mining law vs. effective control outcomes; comparison with alternative sovereignty frameworks (Norway sovereign wealth fund model, Botswana resource management)',
    'If sovereignty = ownership: Mongolia formally sovereign but extraction controlled by foreign agents (Snare). If sovereignty = control: Mongolia not sovereign (Snare from national scale). If sovereignty = security: Mongolia facing depletion risk with no alternative (Snare). Classification shifts based on definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Operational definition of resource extraction sovereignty').

omega_variable(
    chinese_demand_elasticity,
    'How elastic is Chinese industrial demand for Mongolian resources, and does China have viable alternative suppliers?',
    'Analysis of Chinese commodity import diversification strategies; assessment of alternative suppliers'' capacity; modeling of Chinese manufacturing decoupling',
    'If Chinese demand is inelastic and suppliers limited: Mongolia has bargaining power, constraint is more Tangled Rope (genuine coordination function). If Chinese demand is elastic and alternatives abundant: Mongolia has less leverage, constraint is more Snare (pure extraction). This determines the effective d values for Chinese institutional actor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_demand_elasticity, empirical, 'Chinese demand elasticity for Mongolian resources').

omega_variable(
    herding_community_collective_action,
    'Can herding communities overcome collective action barriers to form sustained political organization against mining extraction?',
    'Historical analysis of herding community mobilization capacity; assessment of community organization success rates in similar contexts (pastoral regions in Central Asia, East Africa); monitoring of current organizing efforts',
    'If capacity exists: Snare classification may shift toward Tangled Rope from community perspective (organized exit option exists). If capacity is structurally absent: Snare persists (trapped classification holds). This feeds directly into directionality computation for powerless/organized distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herding_community_collective_action, empirical, 'Whether herding communities can sustain collective political organization').

omega_variable(
    environmental_damage_reversibility,
    'Are the environmental externalities from mining (water table depletion, soil contamination, grazing land loss) reversible within a generational timescale?',
    'Hydrological and soil science assessment of mine site remediation capacity; comparison with post-mining landscape recovery in other regions; modeling of herding land restoration',
    'If reversible: extraction is temporary (Scaffold perspective more credible). If irreversible: extraction is permanent (Snare becomes generational/civilizational). This affects long-term sovereignty victim status and theater ratio measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(environmental_damage_reversibility, empirical, 'Reversibility of environmental mining externalities').

omega_variable(
    mongolian_government_agency_assessment,
    'Does the Mongolian central government genuinely pursue coordinated resource governance, or is it captured by foreign interests and acting as an extractive agent against domestic populations?',
    'Policy analysis of government mining contracts vs. public benefit outcomes; assessment of regulatory enforcement against corporate violations; tracking of government revenue allocation toward domestic development',
    'If genuine coordination: Tangled Rope classification holds (mixed extraction and coordination functions). If captured: government is co-beneficiary in pure extraction system (constraints shift to Snare from community perspective, with government as part of extraction apparatus). This determines whether government appears as separate perspective or merged with corporate beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mongolian_government_agency_assessment, empirical, 'Degree of Mongolian government regulatory capture').

omega_variable(
    renewable_energy_transition_timeline,
    'What is the realistic timeline for Mongolia to transition to renewable energy exports as an alternative revenue model?',
    'Technical assessment of solar/wind capacity in Mongolia; infrastructure investment requirements; regional market demand analysis; comparison with successful renewables transitions in similar economies',
    'If timeline < 15 years: Scaffold sunset is operational (extract high χ eventually transitions to low). If timeline > 25 years: Scaffold sunset is aspirational rather than structural (constraint may not resolve within generational timescale). This affects measurement trajectory and theater ratio interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_energy_transition_timeline, empirical, 'Timeline for renewable energy transition viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mongolian_resource_extraction_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mres_tr_t0, mongolian_resource_extraction_sovereignty, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mres_tr_t10, mongolian_resource_extraction_sovereignty, theater_ratio, 10, 0.53).
narrative_ontology:measurement(mres_tr_t20, mongolian_resource_extraction_sovereignty, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(mres_be_t0, mongolian_resource_extraction_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mres_be_t10, mongolian_resource_extraction_sovereignty, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mres_be_t20, mongolian_resource_extraction_sovereignty, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mongolian_resource_extraction_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(mongolian_resource_extraction_sovereignty, chinese_resource_security_dependency).
narrative_ontology:affects_constraint(mongolian_resource_extraction_sovereignty, central_asian_environmental_degradation).
narrative_ontology:affects_constraint(mongolian_resource_extraction_sovereignty, mongolian_pastoral_land_loss).

% DUAL FORMULATION NOTE:
% The resource extraction sovereignty constraint is upstream of specific environmental and pastoral impacts. Decomposition rationale: the sovereignty constraint (ε=0.58) involves aggregate extraction from multiple populations and externalities; specific downstream constraints have their own ε values reflecting particular impacts (grazing land loss, water contamination, biodiversity loss). The sovereignty constraint coordinates these impacts through unified extraction mechanism. Network links show how Mongolia's dependence affects regional resource security dynamics and environmental outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mongolian_resource_extraction_sovereignty, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
