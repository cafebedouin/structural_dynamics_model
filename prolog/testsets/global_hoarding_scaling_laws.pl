% ============================================================================
% CONSTRAINT STORY: global_hoarding_scaling_laws
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_hoarding_scaling_laws, []).

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
 *   constraint_id: global_hoarding_scaling_laws
 *   human_readable: The Planetary Siphon: Global Hoarding Scaling Laws
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Planetary Siphon represents the scaling of localized extraction
 *   mechanisms into a globally-integrated system for accumulating capital and
 *   resources. At the local level, a merchant who controls a river crossing
 *   extracts tolls; at regional scale, a trader with privileged access to
 *   distant markets captures spreads; at global scale, the same extraction
 *   principle scales through supply-chain integration, IP regimes, and
 *   capital mobility. The constraint systematically redirects value from
 *   resource peripheries to capital centers, from present consumption to
 *   future deprivation, and from ecological commons to private accumulation.
 *   The siphon is enforced through interlocking mechanisms: trade rules that
 *   prohibit domestic production, debt structures that force commodity
 *   export, technological monopolies that lock in dependencies, and
 *   regulatory capture that prevents jurisdictional escape. The theater
 *   component (0.58) reflects that the siphon is legitimized through language
 *   of development, efficiency, and comparative advantage — economic
 *   doctrines that frame extraction as mutual benefit. The suppression
 *   component (0.72) is structural: locked global supply chains make autarky
 *   economically infeasible for most populations; the alternative to
 *   participation is not autonomy but destitution.
 *
 * KEY AGENTS:
 *   - Capital Concentrators: Primary beneficiary (institutional/arbitrage) — multinational corporations, institutional investors, high-net-worth individuals; capture monopoly rents across supply chain
 *   - Global Periphery Populations: Primary victim (powerless/trapped) — populations in resource-extraction zones, manufacturing regions, and commodity-dependent economies; locked into export-dependent models with no exit to self-sufficiency
 *   - Global Middle-Income Consumer: Secondary victim (moderate/constrained) — appears to benefit from low-cost goods but is locked into extraction-subsidized consumption; cannot exit to alternative supply chains
 *   - Multinational Firm: Hybrid actor (powerful/mobile) — coordinates global logistics while extracting monopoly positions; primary mechanism of siphon enforcement
 *   - International Regulatory Framework: Institutional theater (institutional/constrained) — WTO, IMF, World Bank legitimize siphon through language of free trade and development; actual coordination is private
 *   - Future Generations: Distant victim (powerless/trapped) — bear the full cost of resource depletion and ecological degradation; cannot participate in negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_hoarding_scaling_laws, 0.68).
domain_priors:suppression_score(global_hoarding_scaling_laws, 0.72).
domain_priors:theater_ratio(global_hoarding_scaling_laws, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, extractiveness, 0.68).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_hoarding_scaling_laws, snare).
narrative_ontology:human_readable(global_hoarding_scaling_laws, "The Planetary Siphon: Global Hoarding Scaling Laws").
narrative_ontology:topic_domain(global_hoarding_scaling_laws, "economic/political").

domain_priors:requires_active_enforcement(global_hoarding_scaling_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_hoarding_scaling_laws, capital_concentrators).
narrative_ontology:constraint_beneficiary(global_hoarding_scaling_laws, resource_monopolists).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, global_periphery_populations).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, future_generations).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, ecological_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL PERIPHERY (SNARE) — Populations in resource-extraction zones and supply-chain terminus regions experience the siphon as total entrapment. No alternative to participation in extraction-dependent economies; no exit option from dependency on globally-controlled resource flows. Extraction is maximal and coercive.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL MIDDLE-INCOME CONSUMER (SNARE) — Appears to have choice via market mechanisms, but choice is constrained to participation in extraction-subsidized consumption. Price structures hide true resource costs; exit to lower-extraction alternatives is blocked by supply-chain monopolies. Extraction is high, suppression is structural (appears as freedom).
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MULTINATIONAL FIRM (TANGLED ROPE) — Coordinates global supply networks (genuine coordination function) while extracting monopoly rents and externalizing costs (asymmetric extraction). Active enforcement through IP regimes, regulatory capture, trade agreements. Can shift operations, arbitrage between jurisdictions. Benefits from siphon but also genuinely coordinates logistics.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL CONCENTRATOR (ROPE) — Primary beneficiary. Experiences siphon as pure coordination mechanism: aggregating global demand, matching it to global supply, capturing the spread. No extraction experienced by the beneficiary — the constraint operates FOR this agent. Arbitrage exit option is natural (can relocate capital instantly).
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL REGULATORY FRAMEWORK (PITON) — WTO, IMF, World Bank appear to coordinate global trade (theater of legitimacy), but their primary function has atrophied. Real coordination is handled by private supply chains and bilateral agreements. Regulatory framework persists through institutional inertia and theatrical compliance theater (treaty signings, committee meetings). Low functional verification — rules are written by the extracted-from on behalf of extractors.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risk naturalizing the siphon as an inevitable consequence of comparative advantage, scale economics, or human nature itself. The mountain framing ('extraction always happens when systems scale') obscures that this particular siphon is enforced by specific institutional arrangements, IP regimes, and political choices. The engine's false summit detector will flag this perspective as naturalization of contingent structure.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_hoarding_scaling_laws_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_hoarding_scaling_laws, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_hoarding_scaling_laws, TR),
    TR >= 0.70.

:- end_tests(global_hoarding_scaling_laws_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, rising over the measurement interval from 0.35 to 0.68. The siphon's extraction function has intensified as global integration deepened. Early phases (0-25) showed modest extraction partly offset by genuine coordination gains (transportation, market information). Later phases (25-50) show extraction decoupling from coordination as supply chains became locked-in and monopoly positions stabilized. The 0.68 value reflects that extraction is now the dominant dynamic. Suppression (0.72): High and structural. The siphon suppresses alternatives through: (1) trade rules that penalize domestic production, (2) infrastructure investment decisions that lock in global supply dependence, (3) technological IP regimes that prevent leapfrogging, (4) debt structures that force commodity export. Suppression is highest at the periphery (trapped), moderate in middle-income economies (constrained), and near-zero for capital centers (arbitrage exit always available). Theater ratio (0.58): Moderate-high and rising. The siphon is legitimized through efficiency rhetoric, development narratives, and comparative advantage doctrine. These are not false — global coordination does achieve genuine efficiency — but they obscure the extraction component. The theater has increased over time as alternative production models (regional manufacturing, circular economy, cooperative supply networks) have become technologically feasible but politically foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   The siphon exhibits maximum perspectival divergence across all six types. The beneficiary (capital concentrator) sees a coordination mechanism for aggregating global demand and matching it to distributed supply — a rope perspective. The multinational firm sees its own genuine coordination function (logistics, supply-chain integration) alongside its extractive position, producing tangled rope. The regulatory framework maintains theatrical legitimacy through institutional theater and performative rule-making (piton). The global periphery experiences total extraction with no exit path (snare). The middle-income consumer appears to benefit from low prices but cannot exit the extraction-subsidized system (snare). The analytical observer risks naturalizing the siphon as an inevitable consequence of scale and specialization (false mountain). The perspectival gap reveals that 'efficiency' and 'extraction' are not incompatible — the siphon is genuinely efficient at capturing and concentrating value, which is precisely why it is extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are computed from the structural position of each agent relative to extraction flow. Capital concentrators occupy the extraction sink (d ≈ 0.05, full beneficiary with arbitrage exit) — they experience negative effective extraction (χ < 0). Multinational firms are positioned downstream of both extraction and coordination (d ≈ 0.48-0.55, mixed beneficiary-victim with mobile exit) — they experience moderate positive χ. The global periphery is trapped at the extraction source (d ≈ 0.95, full victim with trapped exit) — they experience maximum χ approaching 1.42. The middle-income consumer faces constrained exit from extraction-subsidized consumption (d ≈ 0.75, victim with constrained exit) — they experience high χ ≈ 1.15. The international framework is a captive beneficiary (d ≈ 0.25, appears as beneficiary but actually constrained by private power, shifted upward by override) — it experiences low positive χ but masks real powerlessness. The analytical observer (d ≈ 0.73, observer perspective) experiences χ ≈ 1.15 and risks false summit classification if directionality is inverted toward the natural law fallacy.
 *
 * MANDATROPHY ANALYSIS:
 *   EXTRACTION VS COORDINATION RESOLUTION: The Planetary Siphon resolves the mandatrophy by demonstrating that the same global supply chain is simultaneously a genuine coordination mechanism (beneficiaries see rope, coordination reduces transaction costs) and a pure extraction system (victims see snare, value flows upward regardless of efficiency gains). The mandatrophy is resolved not by choosing one classification but by tracking the perspectival divergence: as the siphon matures, the ratio of extracted value to coordination benefit rises, shifting the aggregate classification from rope-weighted (early phases) toward snare-weighted (current phase). The constraint would truly resolve the mandatrophy if a middle path were available — if beneficiaries and victims could negotiate a Tangled Rope settlement where some extraction is retained but suppression is reduced. This is theoretically possible (supply-chain transparency, labor standards, environmental accounting, debt forgiveness) but is actively resisted by capital concentrators because reducing extraction also reduces benefit. The piton perspective reveals that institutional theater (international frameworks) maintains legitimacy when the material extraction would otherwise be transparent. The false summit risk shows that naturalizing the siphon as 'efficiency law' forecloses the possibility that alternative coordination mechanisms with lower extraction could achieve comparable efficiency at lower cost to periphery populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_externalization_threshold,
    'At what point do hidden externalities (labor, environmental, geopolitical) become visible enough to trigger circuit-breakers in global supply networks?',
    'Tracking of carbon accounting standards, supply-chain transparency mandates, and labor monitoring; measurement of when price signals begin incorporating true resource costs',
    'If threshold is crossed before system degradation: extractiveness may drop to 0.35-0.45 (Tangled Rope). If threshold is crossed after irreversible damage: constraint becomes a Snare locked in by path dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_externalization_threshold, empirical, 'Threshold for externalities to become visible in price signals').

omega_variable(
    alternative_coordination_feasibility,
    'Can regional or decentralized supply coordination mechanisms (cooperative networks, local manufacturing, circular economy loops) actually replicate the logistical efficiency of the global siphon at scale?',
    'Comparison of transaction costs, inventory requirements, and delivery times for local vs global supply chains across sectors; measurement of whether efficiency losses are worth externality gains',
    'If feasible: scaffold perspective validated — alternative pathways exist with sunset potential. If not feasible: global siphon may lock in as permanent coordination requirement (downgrade to Rope from Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Whether decentralized coordination can match global siphon efficiency').

omega_variable(
    capital_mobility_ceiling,
    'What level of regulatory friction or collective action actually prevents capital from shifting to extraction-permissive jurisdictions?',
    'Historical analysis of capital flight responses to labor standards, environmental regulation, and tax policy; measurement of actual vs threatened relocation rates',
    'If capital mobility remains high: institutional responses are capped at Piton level — performative theater. If capital mobility can be constrained: institutional reform path becomes feasible and siphon transitions to Tangled Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_ceiling, empirical, 'Actual constraints on capital exit in response to regulation').

omega_variable(
    siphon_inheritance_lock,
    'Are future generations locked into the siphon by infrastructure investments, institutional momentum, and technological path dependence, or can technological discontinuities enable exit?',
    'Analysis of technological disruption potential (energy transition, manufacturing localization, biotechnology), infrastructure replacement cycles, and institutional reform windows',
    'If locked: constraint becomes civilizational-scale in magnitude, potentially irreversible. If exit windows exist: scaffold perspective is correct — sunset is possible within generational timeframe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(siphon_inheritance_lock, conceptual, 'Whether path dependence locks in the siphon across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_hoarding_scaling_laws, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ghsl_tr_t0, global_hoarding_scaling_laws, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ghsl_tr_t25, global_hoarding_scaling_laws, theater_ratio, 25, 0.45).
narrative_ontology:measurement(ghsl_tr_t50, global_hoarding_scaling_laws, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(ghsl_be_t0, global_hoarding_scaling_laws, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ghsl_be_t25, global_hoarding_scaling_laws, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(ghsl_be_t50, global_hoarding_scaling_laws, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_hoarding_scaling_laws, resource_allocation).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, debt_trap_accumulation).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, resource_monopoly_pricing).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, jurisdictional_tax_arbitrage).
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, labor_arbitrage_wage_suppression).

% DUAL FORMULATION NOTE:
% The Planetary Siphon decomposes into multiple structural constraints: the resource pricing mechanism (commodity extraction), the supply-chain coordination mechanism (logistics), the capital accumulation mechanism (monopoly rents), and the political enforcement mechanism (regulatory capture). These are analyzed as a single siphon story because they are structurally coupled — each mechanism depends on the others. Resource monopolies require supply-chain coordination to distribute; capital accumulation requires regulatory enforcement to prevent alternatives; enforcement requires institutional theater to maintain legitimacy. Decomposition would isolate mechanisms that are only functional when integrated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_hoarding_scaling_laws, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
