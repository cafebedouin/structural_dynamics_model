% ============================================================================
% CONSTRAINT STORY: us_china_technology_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_technology_decoupling, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: us_china_technology_decoupling
 *   human_readable: US-China Technology Decoupling: Supply Chain Fragmentation and Strategic Extraction
 *   domain: geopolitical/economic/technology
 *
 * SUMMARY:
 *   US-China technology decoupling represents a deliberate fragmentation of
 *   integrated global supply chains, justified by national security
 *   rationales but operating as an asymmetric extraction mechanism favoring
 *   US and allied technological sectors. The constraint exhibits
 *   characteristics of Tangled Rope: genuine coordination functions (supply
 *   chain resilience, reducing single-point-of-failure vulnerability) exist
 *   alongside systematic extraction concentrated in technological
 *   superpowers. The constraint is maintained through active enforcement
 *   (export controls, IP restrictions, sanctions, CFIUS screening) rather
 *   than emerging from market incentives, and operates across geopolitical
 *   rather than organizational boundaries. The extractiveness has increased
 *   over the measurement interval (0.32 to 0.62) as export control regimes
 *   have tightened and rare earth supply constraints have deepened. The
 *   theater ratio (0.68) reflects that much decoupling rhetoric frames
 *   geopolitical extraction as inevitable technological competition or
 *   natural security necessity, obscuring the active policy choices that
 *   create and maintain the constraint.
 *
 * KEY AGENTS:
 *   - US Strategic Technology Sectors: Primary beneficiary (institutional/arbitrage) — captures protected markets, secures supply chains, commands price premiums
 *   - Allied Semiconductor Manufacturers: Organized beneficiary (organized/mobile) — benefit from market certainty and government subsidies; have exit capacity through geographic diversification
 *   - Chinese Technology Sectors: Constrained target (institutional/constrained) — lose advanced market access but mobilize domestic capacity; high capability but restricted exit options
 *   - Cost-Sensitive Manufacturers: Moderate victim (moderate/constrained) — face higher procurement costs and dual-sourcing complexity; benefit from supply chain redundancy
 *   - Developing World Technology Access: Powerless victim (powerless/trapped) — trapped in second-source supply chains, face restricted access and rising costs; no alternative vendors
 *   - Multilateral Trade Regime: Institutional actor (institutional/arbitrage) — maintains performative integration rhetoric while decoupling operates through exceptions; degraded effectiveness (Piton)
 *   - Analytical Observer: Structural analyst (analytical/analytical) — identifies mixed coordination and extraction; sees suppression as policy choice rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_technology_decoupling, 0.58).
domain_priors:suppression_score(us_china_technology_decoupling, 0.72).
domain_priors:theater_ratio(us_china_technology_decoupling, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_technology_decoupling, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_technology_decoupling, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_china_technology_decoupling, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_technology_decoupling, tangled_rope).
narrative_ontology:human_readable(us_china_technology_decoupling, "US-China Technology Decoupling: Supply Chain Fragmentation and Strategic Extraction").
narrative_ontology:topic_domain(us_china_technology_decoupling, "geopolitical/economic/technology").

domain_priors:requires_active_enforcement(us_china_technology_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_technology_decoupling, us_strategic_technology_sectors).
narrative_ontology:constraint_beneficiary(us_china_technology_decoupling, allied_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(us_china_technology_decoupling, domestic_rare_earth_producers).
narrative_ontology:constraint_victim(us_china_technology_decoupling, global_technology_supply_chains).
narrative_ontology:constraint_victim(us_china_technology_decoupling, cost_sensitive_manufacturers).
narrative_ontology:constraint_victim(us_china_technology_decoupling, developing_world_technology_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING WORLD TECHNOLOGY ACCESS (SNARE) — Trapped by supply chain fragmentation; cannot exit decoupling regime. Faces rising component costs, restricted access to advanced semiconductors, and forced dependence on second-source supply chains with reduced capability. No alternative vendors, no exit option. Maximum extraction — maximum suppression through geopolitical allocation of critical technology.
constraint_indexing:constraint_classification(us_china_technology_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COST-SENSITIVE MANUFACTURERS (TANGLED ROPE) — Constrained by dual-sourcing requirements and certification costs; also benefit from supply chain diversification reducing single-source risk. Genuine coordination function (redundancy) exists alongside asymmetric extraction (higher procurement costs, regulatory overhead). Moderate agency but significant cost burden.
constraint_indexing:constraint_classification(us_china_technology_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US STRATEGIC TECHNOLOGY SECTORS (ROPE) — Primary beneficiary with arbitrage exit options. Decoupling protects market share, secures supply chains, and enables price premiums in allied markets. Experiences constraint as coordination mechanism: protecting technological sovereignty enables long-term R&D investment and reduces strategic vulnerability. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(us_china_technology_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE TECHNOLOGY SECTORS (TANGLED ROPE) — Constrained by export controls, IP restrictions, and equipment bans; also mobilizing domestic supply chains and capturing export markets in restricted zones. Genuine coordination function (building domestic semiconductor capacity, rare earth consolidation) exists alongside asymmetric extraction (lost access to advanced markets, forced vertical integration costs). High structural capability but significant trade restrictions.
constraint_indexing:constraint_classification(us_china_technology_decoupling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ALLIED SEMICONDUCTOR COALITION (SCAFFOLD) — Organized agents (TSMC, Samsung, Intel, European fabs) see decoupling as temporary restructuring with sunset logic. Invested in geographically distributed capacity, subsidized fabs in allied territories, and phased exit from cutting-edge dependence on single regions. Benefits from coordination (market certainty, government subsidies) with planned sunset as mature redundancy. Theater ratio declining as actual duplication replaces performative supply chain resilience.
constraint_indexing:constraint_classification(us_china_technology_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: MULTILATERAL TRADE REGIME (PITON) — WTO, GATT principles, and free-trade rhetoric persist but are performative; decoupling operates through explicit exceptions and strategic carve-outs framed as national security. The institutions maintain theater of integration while enforcement follows geopolitical extraction logic. Degraded system maintained through inertia; primary function (reducing transaction costs) has atrophied while political rhetoric about open trade persists.
constraint_indexing:constraint_classification(us_china_technology_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Decoupling is genuinely mixed: contains real coordination functions (supply chain resilience, reducing single-point-of-failure vulnerability) alongside asymmetric extraction of benefits toward technological superpowers. The constraint persists through active enforcement (export controls, sanctions, IP restrictions) not through natural law. The suppression mechanism (geopolitical allocation of technology) is contingent and modifiable, making the extraction structural rather than inevitable.
constraint_indexing:constraint_classification(us_china_technology_decoupling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_technology_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_technology_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_technology_decoupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_technology_decoupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_technology_decoupling, TR),
    TR >= 0.70.

:- end_tests(us_china_technology_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The US and allied technology sectors gain protected markets, supply chain security, and price premiums. China loses access to advanced markets and faces forced vertical integration. Cost-sensitive manufacturers pay for dual-sourcing and certification. Developing nations lose access to cutting-edge components. The extraction is substantial but not maximal — genuine supply chain resilience benefits exist alongside the rent-seeking, and some coordination functions reduce rather than increase costs for certain actors. The trajectory shows extractiveness increasing over time (0.32 to 0.62) as export controls tighten and rare earth consolidation deepens. Suppression (0.72): High. Export controls, IP restrictions, sanctions, and geopolitical allocation of technology create substantial barriers to exit. These are hard constraints enforced by state power, not soft coordination problems. However, suppression is not absolute — sophisticated supply chain engineering, indigenous technology development, and third-country sourcing provide workarounds at high cost. Theater ratio (0.68): Moderate-high. Decoupling is justified through national security rhetoric, inevitability framing ('the technology competition is heating up'), and risk mitigation narratives that obscure active policy choices. Allied capacity buildouts involve genuine investment but also substantial performative signaling ('resilience' that primarily demonstrates political commitment rather than technical improvement). The theater has increased over time as rhetoric emphasizes predetermined technological competition rather than contingent policy decisions.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the beneficiary perspective (US sectors seeing coordination) and the victim perspective (developing nations seeing extraction). For the beneficiary, decoupling solves technological vulnerability — a genuine problem. For the victim, decoupling is pure cost with no offsetting benefit. The intermediate actors (China, cost-sensitive manufacturers, allied semiconductors) experience genuine hybridity: they are simultaneously constrained and agents. The piton perspective reveals that the constraint persists through theater even as its original function (preventing dependence) has shifted toward serving protectionist extraction. The analytical perspective shows that framing the constraint as 'inevitable technological competition' naturalizes what is actually a contingent set of policy choices, risking false-summit (mountain) classification if the narrative is not interrupted.
 *
 * DIRECTIONALITY LOGIC:
 *   The derived directionality values reflect power asymmetries embedded in technology allocation. US institutional/arbitrage agents derive d ≈ 0.05-0.15 (full beneficiaries with exit); allied institutional/mobile agents derive d ≈ 0.20-0.35 (strong beneficiaries with options); Chinese institutional/constrained agents derive d ≈ 0.60-0.75 (constrained targets); cost-sensitive moderate/constrained agents derive d ≈ 0.55-0.70 (moderate targets); developing powerless/trapped agents derive d ≈ 0.90-0.98 (maximum targets). The sigmoid f(d) transforms these into chi multipliers, with powerless/trapped agents experiencing the maximum extractiveness scaling while beneficiaries experience negative scaling (they gain while others bear cost). The scope modifier σ(S) amplifies effect at global scope (1.2) versus local scope (0.8), making the constraint's extractiveness more visible at civilizational time horizons.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: Extractiveness 0.58 places this at the tangled rope boundary (ε ≥ 0.30, χ overlaps Rope-Snare ranges depending on perspective). The resolution is that perspectives split: beneficiaries see Rope (coordination), victims see Snare (extraction), and the institutional observer sees Tangled Rope (both functions present). The claimed type (Tangled Rope) is justified because (1) beneficiaries exist and benefit from genuine supply chain resilience coordination, (2) victims exist and bear asymmetric extraction, and (3) active enforcement is required to maintain the constraint — it does not emerge from market incentives alone. The mandatrophy arises from the natural question: 'Is this really coordination pretending to be extraction, or really extraction wearing coordination's mask?' The answer is: both are true depending on structural position. For the US/allied sectors, it is coordination. For developing nations, it is extraction. For intermediate actors and the overall analytical frame, it is hybrid. The engine resolves this by requiring all three elements (beneficiaries, victims, active enforcement) to appear simultaneously, which prevents collapsing the constraint into pure Rope (which requires suppression < 0.60) or pure Snare (which requires beneficiaries to be absent or incidental). The measured high suppression (0.72) and presence of both beneficiaries and victims confirm Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_versus_extraction_balance,
    'What fraction of the supply chain restructuring is genuine risk mitigation vs. extractive rent-seeking by dominant players?',
    'Cost analysis of dual sourcing vs single-source efficiency; measurement of price premiums in restricted supply chains; comparison of decoupling costs to actual supply chain failures prevented',
    'If coordination > 60%: Tangled Rope classification stable across perspectives. If extraction > 60%: shifts toward Snare for most perspectives; chi increases significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_extraction_balance, empirical, 'Genuine supply chain resilience vs. extractive rent-seeking balance').

omega_variable(
    technological_substitutability,
    'Can Chinese and non-Chinese semiconductor supply chains produce functionally equivalent components for the majority of applications, or is decoupling creating permanent capability gaps?',
    'Technical benchmarking of Chinese-source semiconductors vs Western equivalents across application domains; measurement of performance deficits and timeline to parity',
    'If parity achievable within 10 years: Scaffold perspective validated; sunset is real. If permanent capability gaps emerge: Snare perspective validated; extraction becomes structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_substitutability, empirical, 'Whether semiconductor decoupling creates permanent or temporary capability gaps').

omega_variable(
    rare_earth_monopoly_stability,
    'Are non-Chinese rare earth capacity buildouts economically sustainable, or do they require permanent subsidies to compete with Chinese cost structures?',
    'Cost curve analysis of Western rare earth mining vs Chinese production; measurement of break-even points with and without government support; tracking of private investment in non-Chinese capacity',
    'If sustainable without subsidy: decoupling addresses real monopoly vulnerability. If permanent subsidy-dependent: extraction mechanism is subsidies to private firms, and the constraint masks industrial policy extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rare_earth_monopoly_stability, empirical, 'Economic sustainability of non-Chinese rare earth production').

omega_variable(
    third_country_alliance_coherence,
    'Can the US-led allied coalition maintain decoupling enforcement as individual allies face economic pressure to maintain China trade relationships?',
    'Tracking of tariff exceptions, IP enforcement cooperation, and export control compliance across allied nations; measurement of defection incentives and coalition cohesion over time',
    'If coalition remains coherent: Rope/Tangled Rope classification stable. If defections increase: constraint degrades toward Piton; effectiveness declines while theater persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_country_alliance_coherence, empirical, 'Stability of US-led allied coalition enforcing technology decoupling').

omega_variable(
    suppression_mechanism_visibility,
    'Is the suppression mechanism (export controls, sanctions, IP restrictions) openly acknowledged as geopolitical extraction, or does it remain framed as national security and risk mitigation?',
    'Discourse analysis of policy communications; measurement of alignment between stated rationales (risk mitigation) and actual policy effects (asymmetric extraction); tracking of public vs private acknowledgment by policymakers',
    'If openly acknowledged: suppression remains at measured levels, classification stable. If successfully framed as natural law/necessary protection: suppression metric understates effective coercion; reclassify as higher suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_visibility, conceptual, 'Whether suppression mechanism is openly acknowledged or naturalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_technology_decoupling, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uschi_tr_t0, us_china_technology_decoupling, theater_ratio, 0, 0.55).
narrative_ontology:measurement(uschi_tr_t2, us_china_technology_decoupling, theater_ratio, 2, 0.62).
narrative_ontology:measurement(uschi_tr_t4, us_china_technology_decoupling, theater_ratio, 4, 0.68).
narrative_ontology:measurement(uschi_tr_t6, us_china_technology_decoupling, theater_ratio, 6, 0.7).

% Extraction over time
narrative_ontology:measurement(uschi_be_t0, us_china_technology_decoupling, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(uschi_be_t2, us_china_technology_decoupling, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(uschi_be_t4, us_china_technology_decoupling, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(uschi_be_t6, us_china_technology_decoupling, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_technology_decoupling, global_infrastructure).
narrative_ontology:boltzmann_floor_override(us_china_technology_decoupling, 0.25).
narrative_ontology:affects_constraint(us_china_technology_decoupling, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(us_china_technology_decoupling, rare_earth_geopolitical_dependence).
narrative_ontology:affects_constraint(us_china_technology_decoupling, ip_regime_enforcement_asymmetry).

% DUAL FORMULATION NOTE:
% US-China technology decoupling operates as a container constraint affecting three downstream constraints: semiconductor supply concentration (how dual-sourcing rules propagate); rare earth dependence (how export controls concentrate supply); and IP enforcement asymmetry (how patent restrictions differentially impact domestic vs foreign R&D). Each downstream constraint has its own ε value and reflects specific mechanisms through which the decoupling regime operates. This story models the decoupling regime itself; the downstream stories model specific technology domains affected by it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_technology_decoupling, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
