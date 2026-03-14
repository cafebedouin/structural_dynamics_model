% ============================================================================
% CONSTRAINT STORY: canadian_technology_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canadian_technology_sovereignty, []).

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
 *   constraint_id: canadian_technology_sovereignty
 *   human_readable: Canadian Technology Sovereignty Constraint
 *   domain: political_economy/technology_policy
 *
 * SUMMARY:
 *   Canada faces a structural constraint between technology integration with
 *   North American and global supply chains and assertions of technology
 *   sovereignty. This constraint creates a coordination problem (ensuring
 *   supply chain resilience, reducing dependence on hostile state actors)
 *   layered with extraction mechanisms (tariffs, procurement mandates,
 *   localization requirements that impose costs on consumers and unconnected
 *   firms). The constraint exhibits characteristics of a tangled rope:
 *   genuine coordination functions coexist with asymmetric extraction.
 *   Government policy frames the constraint as security necessity; consumers
 *   experience it as cost imposition; established tech firms experience it as
 *   guaranteed market access; small startups experience it as regulatory
 *   friction; continental integration advocates experience it as temporary
 *   friction resolvable through alliance coordination. The extractiveness and
 *   theater ratio have increased over 14 years as the domestically-driven
 *   industrial policy deepens despite limited technological capability
 *   development and growing costs to consumers. The mandatrophy remains
 *   unresolved: is this a justified coordination mechanism (rope with
 *   coordination function) or an extraction mechanism (snare) masquerading as
 *   coordination?
 *
 * KEY AGENTS:
 *   - Canadian tech consumers: Primary victim (powerless/trapped) — trapped within tariffs and supply chain redirections, experiencing higher costs and slower adoption
 *   - Small tech startups: Primary victim (powerless/trapped) — trapped by localization requirements and domestic-first mandates lacking exemptions available to established firms
 *   - Established Canadian tech firms: Primary beneficiary (institutional/constrained) — receive guaranteed market access and supply chain preference; constrained from global scaling due to regulatory embeddedness
 *   - Canadian government industrial policy: Beneficiary (institutional/arbitrage) — designs and controls the constraint; benefits from perceived national security gains and domestic champion narrative
 *   - North American trade integration advocates: Organized alternative (organized/mobile) — see sovereignty constraints as temporary friction resolvable through continental security arrangements and integration
 *   - Cold War autonomy narrative: Institutional piton (institutional/arbitrage) — persists through performative maintenance despite reduced technological justification
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — identifies genuine coordination components (supply resilience) layered with extraction (protection of domestic champions from competition)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canadian_technology_sovereignty, 0.58).
domain_priors:suppression_score(canadian_technology_sovereignty, 0.65).
domain_priors:theater_ratio(canadian_technology_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canadian_technology_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(canadian_technology_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(canadian_technology_sovereignty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canadian_technology_sovereignty, tangled_rope).
narrative_ontology:human_readable(canadian_technology_sovereignty, "Canadian Technology Sovereignty Constraint").
narrative_ontology:topic_domain(canadian_technology_sovereignty, "political_economy/technology_policy").

domain_priors:requires_active_enforcement(canadian_technology_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canadian_technology_sovereignty, canadian_tech_champions).
narrative_ontology:constraint_beneficiary(canadian_technology_sovereignty, domestic_supply_chain_actors).
narrative_ontology:constraint_victim(canadian_technology_sovereignty, canadian_consumers).
narrative_ontology:constraint_victim(canadian_technology_sovereignty, small_tech_firms).
narrative_ontology:constraint_victim(canadian_technology_sovereignty, global_integration_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANADIAN TECH CONSUMER (SNARE) — Trapped within tariff and content barriers justified as sovereignty protection. Faces higher device costs, slower technology adoption, and reduced competitive pressure. No meaningful exit option: purchasing foreign devices faces tariffs, supply chain redirection creates delays and premiums. Suppression is structural through trade barriers and regulatory requirements. Pure extraction masked as national interest.
constraint_indexing:constraint_classification(canadian_technology_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL TECH STARTUP (SNARE) — Trapped by domestic-first procurement mandates and supply chain localization requirements. Cannot access cheaper global components without regulatory friction. High barriers to international scaling: data residency requirements, certification duplication, local hiring mandates. Extractive cost imposed on firms lacking lobbying access to secure exemptions. No exit: remaining small within domestic market is the only accessible path.
constraint_indexing:constraint_classification(canadian_technology_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED CANADIAN TECH FIRM (TANGLED ROPE) — Genuine coordination function: domestic supply chain preference creates guaranteed market access and strategic partnerships. Active enforcement through procurement mandates and supply chain subsidies ensures domestic competitors gain traction. Asymmetric extraction: smaller domestic firms bear localization costs that established players have leverage to absorb or pass through. Constrained exit: firm could relocate but loses domestic preference status and government subsidy linkages. Mixed coordination (assured customer base) and extraction (preference granted selectively to those meeting political criteria).
constraint_indexing:constraint_classification(canadian_technology_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CANADIAN GOVERNMENT INDUSTRIAL POLICY (ROPE) — Genuine coordination function: organizing domestic tech ecosystem, securing supply chains for critical infrastructure, reducing dependency on hostile state actors. Solves real collective action problem of tech resilience. Minimal coercive overhead from government's perspective: policy is voluntary alignment with industrial incentives. Arbitrage available to policy: can adjust localization requirements, shift subsidy allocation, negotiate bilateral deals. Net beneficiary: policy benefits from perceived national security gain and domestic tech champion growth. Extraction runs toward this agent.
constraint_indexing:constraint_classification(canadian_technology_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NORTH AMERICAN TRADE INTEGRATION MOVEMENT (SCAFFOLD) — Organized agents (USMCA signatories, continental tech alliances, supply chain harmonization initiatives) see sovereignty constraints as temporary friction to be resolved through deeper integration and mutual security arrangements. Low effective extraction because these actors can mobilize alternatives: continental agreements create parallel supply chains, standards harmonization reduces localization costs, security partnerships provide alternatives to domestic-only procurement. Sunset logic: as North American tech integration deepens and supply chain vulnerabilities are addressed through alliance coordination rather than protectionism, the sovereignty constraint's extraction function declines. Estimated sunset: 10-20 years as USMCA deepens and continental security frameworks mature.
constraint_indexing:constraint_classification(canadian_technology_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: COLD WAR STRATEGIC AUTONOMY NARRATIVE (PITON) — Historical institutional arrangement (domestic tech champions, supply chain autarky, technology independence) persists through performative maintenance despite reduced functional necessity. Theater ratio high: sovereignty rhetoric substitutes for actual technological capacity gaps. Real strategic autonomy questions (Can Canada independently design chips? Maintain semiconductor fabs?) remain unanswered; instead, policy focuses on tariffs, procurement mandates, and symbolic champion creation. Degraded: the institutional logic that justified full domestic alternatives has atrophied (global supply chains are too specialized), but the performative ritual persists. Maintained by inertia and identity fusion with 'independence' rather than structural necessity.
constraint_indexing:constraint_classification(canadian_technology_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the sovereignty constraint has genuine coordination components (reducing reliance on hostile adversaries, ensuring supply chain resilience) layered with asymmetric extraction (protecting domestic champions from competition, redirecting procurement benefits, raising consumer costs). Active enforcement is required because the coordination function is not self-sustaining — without tariffs and mandates, global efficiency would dissolve domestic supply chains. The constraint is neither pure coordination (rope) nor pure extraction (snare); it is a hybrid where the coordination rationale legitimizes extraction mechanisms.
constraint_indexing:constraint_classification(canadian_technology_sovereignty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canadian_technology_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canadian_technology_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canadian_technology_sovereignty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canadian_technology_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canadian_technology_sovereignty, TR),
    TR >= 0.70.

:- end_tests(canadian_technology_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts costs from consumers (tariffs, supply chain premiums) and small firms (regulatory friction), while concentrating benefits to established domestic firms and government policy apparatus. The extraction is asymmetric: firms with government relationships receive exemptions and subsidies; firms without such relationships face full burden. Over 14 years, extractiveness has increased from 0.35 to 0.58 as protectionist measures have deepened without corresponding technological capability gains. Suppression (0.65): Moderate-high. Tariff barriers, content requirements, data residency mandates, and domestic-first procurement create structural barriers to escaping the constraint. Smaller actors cannot negotiate exemptions. International firms cannot freely access Canadian market. Consumers face limited alternatives and tariff costs on alternatives. The suppression is primarily structural (regulatory barriers) rather than internalized (cognitive capture), though nationalist framing provides some internalization. Theater ratio (0.58): Moderate-high. Sovereignty rhetoric (independence, autonomy, security) substitutes for actual technological capability measurement. Canada's real capacity to independently design and manufacture advanced semiconductors, 5G infrastructure, and critical materials remains limited. The policy focuses on tariffs and procurement mandates rather than capability building. Industrial champions created through procurement preference have not achieved significant international competitiveness. Theater has increased as the gap between sovereignty narrative and actual technological independence has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gaps reveal the mandatrophy structure. The canonical classifier observes: (1) Beneficiaries (established firms, government) experience coordination function + market preference = rope; (2) Victims (consumers, small firms) experience barriers + cost imposition = snare; (3) Organized alternatives (continental integration advocates) experience temporary friction + sunset mechanism = scaffold; (4) Cold War narrative sees its own degradation = piton; (5) Analytical observer at civilizational scope sees both coordination (supply resilience) and extraction (champion protection) = tangled rope. No single type is 'correct' because the constraint genuinely embodies multiple functions from different structural positions. The question is not 'which type is right' but 'what is the actual proportion of genuine coordination vs extractive overhead?' If 70%+ of the suppression and extractiveness serves real supply chain resilience, classification shifts toward rope. If 70%+ serves protection of uncompetitive incumbents, classification shifts toward snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the constraint. Consumers as powerless/trapped agents experience maximum directionality toward extraction (d ≈ 0.95, f(d) ≈ 1.42) because they face tariffs and supply chain costs with no exit options. Small startups as powerless/trapped experience high directionality (d ≈ 0.90) because regulatory requirements impose friction without proportional benefits. Established firms as institutional/constrained experience lower directionality (d ≈ 0.35) because they benefit from procurement preference and supply chain coordination, though constrained by regulatory embeddedness. Government as institutional/arbitrage experiences negative directionality (d ≈ 0.15, f(d) ≈ -0.01) because the policy is designed from this institution's interests and it captures the coordination benefits. The chi formula applies: effective extraction is scaled by directionality function and scope modifier. At national scope (σ = 1.0), chi for powerless consumers is ε × f(d) × 1.0 = 0.58 × 1.42 × 1.0 ≈ 0.82, placing the constraint firmly in snare territory from that perspective. At national scope for institutional beneficiaries, chi is approximately 0.58 × (-0.01) × 1.0 ≈ -0.006, producing rope classification. The perspectival gap is mathematically significant.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: Canadian technology sovereignty exhibits the defining feature of mandatrophy — the classification is genuinely uncertain because the coordinate classification components (coordination function vs extraction mechanism) are empirically indeterminate. The constraint possesses: (A) Genuine coordination function: real supply chain vulnerabilities exist, isolated suppliers can be weaponized, resilience has value. (B) Genuine extraction mechanisms: consumers pay tariffs, small firms face regulatory friction, incumbent champions receive protection selectively based on government relationships rather than competitiveness. The mandatrophy cannot be resolved by examining the metrics alone because both the coordination function and the extraction function are present and partially serving each other. Deeper supply chain resilience (coordination benefit) would require tariff protection (extraction mechanism). Removing extraction mechanisms would undermine resilience gains. The constraint is a true tangled rope where the two components are structurally intertwined. Resolution would require either: (1) Empirical determination of whether Canadian technological capacity can be developed to justify the extraction costs, or (2) Institutional redesign to decouple coordination (supply chain resilience through alliance rather than isolation) from extraction (champion protection through procurement preference). Without one of these, the mandatrophy persists and the constraint remains properly classified as tangled rope with unresolved uncertainty about the true ratio of coordination to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_versus_integration_tradeoff,
    'Is the optimal strategy for Canadian technological resilience domestic autarky or deeper continental/allied integration with security guarantees?',
    'Comparative analysis of technology supply chain robustness across sovereign-first vs integrated models; modeling supply chain reconstruction timelines under different geopolitical scenarios; empirical comparison of resilience between Canada''s domestic-first approach vs allied nations using integration-first models',
    'If autarky is optimal: sovereignty constraints are justified coordination mechanisms (reclassify toward rope from more perspectives). If integration is optimal: sovereignty constraints are primarily extractive overhead (reclassify toward snare from more perspectives). Current mandatrophy hangs on this uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_versus_integration_tradeoff, empirical, 'Whether technology resilience requires domestic autarky or integrated supply chains').

omega_variable(
    canadian_technological_capability_gap,
    'Can Canada sustain truly independent design and manufacturing capacity for critical technologies (semiconductors, telecommunications, advanced materials), or is the sovereignty narrative masking dependence on imported capability?',
    'Technical audit: Canadian design capacity for advanced semiconductors, fabs, 5G/6G infrastructure. Historical cost analysis of achieving technological parity. Feasibility assessment of maintaining isolated supply chains vs leveraging continental/allied capabilities. Patent generation and technology transfer data.',
    'If Canada can achieve independent capability: sovereignty constraint is functional coordination mechanism (higher classification as rope). If Canada cannot: sovereignty constraint is performative cover for extraction (higher theater ratio, reclassify toward piton). This determines whether the constraint solves a real problem or creates theatrical solutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(canadian_technological_capability_gap, empirical, 'Whether Canada can achieve independent advanced technology capabilities').

omega_variable(
    geopolitical_threat_model_assumption,
    'What is the actual probability and timeline of the geopolitical scenario (supply chain cutoff by hostile actor) that justifies sovereignty protection costs?',
    'Scenario analysis: sustained semiconductor/telecommunications embargo by China, decoupling from US supply chains, NATO supply disruption. Cost-benefit analysis: probability × impact of threat vs extraction cost of current sovereignty measures. Comparison with actual historical precedent of supply disruptions.',
    'If threat probability is high and immediate: sovereignty constraint is justified protection (classification remains tangled_rope with strong coordination function). If threat is low or distant: sovereignty constraint is precautionary at high extraction cost (classification shifts toward snare). This determines whether suppression reflects genuine necessity or risk premium on extracted costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_threat_model_assumption, preference, 'Probability and timeline of geopolitical scenarios requiring supply chain autonomy').

omega_variable(
    champion_selection_capability,
    'Does the Canadian government have institutional capacity to identify and cultivate genuinely competitive technology champions, or does selective procurement create rent-seeking incumbents?',
    'Outcome tracking: which Canadian firms receiving sovereignty protection achieved international competitiveness vs which remained domestically protected. Patent quality and technology advancement comparison. Market share tracking in non-protected global markets. Comparison with government tech champion programs in peer nations (South Korea, Israel, Taiwan).',
    'If government selection succeeds: protection benefits are real (classification lowers toward rope, beneficiaries gain legitimate advantage). If government selection fails: protection creates zombie firms dependent on tariffs (classification shifts toward snare for consumers and small firms, piton for the protected firms). This determines whether the constraint solves a coordination problem or creates extractive dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(champion_selection_capability, empirical, 'Whether government can successfully identify and cultivate competitive technology champions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canadian_technology_sovereignty, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctech_tr_t0, canadian_technology_sovereignty, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ctech_tr_t7, canadian_technology_sovereignty, theater_ratio, 7, 0.52).
narrative_ontology:measurement(ctech_tr_t14, canadian_technology_sovereignty, theater_ratio, 14, 0.58).
narrative_ontology:measurement(ctech_tr_t10, canadian_technology_sovereignty, theater_ratio, 10, 0.56).

% Extraction over time
narrative_ontology:measurement(ctech_be_t0, canadian_technology_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ctech_be_t7, canadian_technology_sovereignty, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(ctech_be_t14, canadian_technology_sovereignty, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(ctech_be_t3, canadian_technology_sovereignty, base_extractiveness, 3, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canadian_technology_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(canadian_technology_sovereignty, semiconductor_supply_chain_vulnerability).
narrative_ontology:affects_constraint(canadian_technology_sovereignty, telecommunications_infrastructure_sovereignty).
narrative_ontology:affects_constraint(canadian_technology_sovereignty, critical_materials_supply_resilience).

% DUAL FORMULATION NOTE:
% Canadian technology sovereignty decomposes into three downstream constraints: (1) semiconductor supply chain vulnerability (ε ≈ 0.65, higher extractiveness, more directly about vulnerability), (2) telecommunications infrastructure sovereignty (ε ≈ 0.52, moderate extractiveness, more about Huawei/5G policy), (3) critical materials supply resilience (ε ≈ 0.48, lower extractiveness, more about mineral supply). Each has distinct beneficiaries/victims and perspectives. This story represents the umbrella constraint; the downstream stories represent specific domains where the general sovereignty mandate generates specific constraints with different metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canadian_technology_sovereignty, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
