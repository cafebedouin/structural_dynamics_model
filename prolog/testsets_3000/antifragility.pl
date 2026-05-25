% ============================================================================
% CONSTRAINT STORY: antifragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antifragility, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility is a structural principle describing systems that gain
 *   capability from volatility, stressors, and disorder. The constraint
 *   operates across biological, economic, and technological domains. Its core
 *   mechanism is simple: variation enables selection; selection concentrates
 *   viable adaptations; concentrated adaptations increase system capability.
 *   Antifragility is fundamentally different from robustness (resistance to
 *   disorder) or resilience (recovery after disorder). An antifragile system
 *   does not merely survive volatility — it becomes stronger through
 *   exposure. Evolution is the canonical example: genetic variation is the
 *   raw material; environmental stress selects viable phenotypes; the
 *   population becomes fitter. Markets exhibit antifragility: business
 *   failures eliminate unviable strategies; surviving firms innovate to
 *   escape competition; the economy becomes more adaptive. Decentralized
 *   networks gain robustness from redundancy: node failures force protocol
 *   redesign; redesigned protocols are more robust than the original. The
 *   constraint's extractiveness is moderate because the benefit distribution
 *   is context-dependent: evolutionary populations share antifragility gains
 *   equally; decentralized networks distribute gains to all nodes; small
 *   business owners bear concentrated stress but capture gains if they
 *   survive; precarious workers bear stress with zero antifragility benefit;
 *   institutional consultants extract rent by selling performative
 *   antifragility. The theater ratio has increased over the 10-unit interval
 *   as corporate 'resilience programs' have become widespread — the
 *   appearance of antifragility-building is increasingly decoupled from
 *   actual volatility exposure. The constraint emerges naturally (it is not
 *   enforced), but its distribution across agents is deeply unequal, creating
 *   extraction dynamics when institutional structures concentrate volatility
 *   exposure and unequally distribute gains.
 *
 * KEY AGENTS:
 *   - Adaptive Systems: Primary beneficiary (analytical/analytical) — gain capability from volatility through evolutionary/market/network selection mechanisms
 *   - Evolutionary Populations: Beneficiary (organized/constrained) — biological entities that benefit from environmental volatility through genetic diversity and phenotypic flexibility
 *   - Decentralized Networks: Beneficiary (institutional/arbitrage) — distributed systems that gain robustness from failure and redundancy
 *   - Small Business Owner: Mixed (moderate/constrained) — benefits from antifragility if business survives; bears concentrated stress during volatility; experiences asymmetric extraction from creditors and market discipline
 *   - Precarious Worker: Primary victim (powerless/trapped) — bears full volatility exposure with zero antifragility benefit; no exit option; experiences extraction without corresponding gain
 *   - Resilience-Building Coalition: Secondary agent (organized/constrained) — creates temporary scaffolds that accelerate antifragility development with explicit sunset clauses
 *   - Antifragility Industry: Institutional actor (institutional/arbitrage) — sells performative antifragility programs; benefits from theater expansion; sees constraint as source of revenue rather than real principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility, 0.28).
domain_priors:suppression_score(antifragility, 0.12).
domain_priors:theater_ratio(antifragility, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility, extractiveness, 0.28).
narrative_ontology:constraint_metric(antifragility, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(antifragility, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(antifragility, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(antifragility, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility, rope).
narrative_ontology:human_readable(antifragility, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility, "technological/economic/biological").

domain_priors:emerges_naturally(antifragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility, adaptive_systems).
narrative_ontology:constraint_beneficiary(antifragility, evolutionary_populations).
narrative_ontology:constraint_beneficiary(antifragility, decentralized_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATURAL LAW VIEW (MOUNTAIN) — Antifragility is a fundamental principle of complex adaptive systems across scales: biological evolution gains from environmental stress, market economies discover innovations through competition and failure, immune systems strengthen through pathogen exposure. The constraint emerges naturally from the mathematics of optimization under volatility and from the physics of information integration in far-from-equilibrium systems. No agent enforces it; it is an irreducible structural feature of how adaptive systems work. Zero degrees of freedom.
constraint_indexing:constraint_classification(antifragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: EVOLUTIONARY POPULATION (ROPE) — Populations that experience environmental volatility develop greater genetic diversity and phenotypic flexibility. Stressors eliminate weak adaptations and concentrate viable ones. The constraint is pure coordination: individual organisms cooperate (through mating, resource sharing, colony structures) to capture antifragility benefits. No asymmetric extraction — all members of the population benefit from the collective response to disorder. The suppression is minimal (natural selection is costless in information-theoretic terms). Theater ratio is low (actual adaptive mechanisms, not performative ones).
constraint_indexing:constraint_classification(antifragility, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DECENTRALIZED NETWORK (ROPE) — Internet protocols, peer-to-peer systems, and open-source ecosystems gain robustness through distributed redundancy and failure. No central authority enforces antifragility; the constraint emerges from the coordination of autonomous nodes. Each node benefits from the network's resilience (benefits are evenly distributed). Suppression is low (the protocol simply works). Theater ratio is low (redundancy and failure recovery are functional, not performative). Beneficiaries and victims are identical: all network participants.
constraint_indexing:constraint_classification(antifragility, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALL BUSINESS OWNER (TANGLED ROPE) — A business that survives market downturns becomes stronger (if it survives). But the path to antifragility is painful: operational stress, job losses, near-insolvency experiences, and survival-driven innovation. The owner benefits from antifragility if the business survives, but bears concentrated extraction during the disorder phase. Coordination function exists (the market selects for viable strategies), but extraction is asymmetric: the owner experiences the full stress of the volatility; external investors and creditors can exit. Active enforcement is present in the form of bankruptcy law, creditor pressure, and market discipline. Suppression is moderate (the owner cannot easily exit during downturns; constrained exit option).
constraint_indexing:constraint_classification(antifragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRECARIOUS WORKER (SNARE) — A worker exposed to high volatility in labor markets (gig economy, seasonal employment, contract work) gains no antifragility benefit. Each downturn causes immediate income loss. No coordination function protects workers; the suppression is high (limited social safety net, no union power, trapped in contract terms). The constraint extracts from workers: they bear full volatility risk while benefiting from none of the system's adaptability gains. No exit option — workers cannot leave the labor market, and individual volatility exposure cannot be hedged. High extraction, high suppression, minimal theater (the precarity is real, not performative).
constraint_indexing:constraint_classification(antifragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: RESILIENCE-BUILDING COALITION (SCAFFOLD) — Disaster-preparedness organizations, redundancy-building initiatives, and resilience investment programs create temporary coordination scaffolds that accelerate antifragility development. These have explicit sunset clauses: once redundancy is built, the scaffold is no longer needed. Early stage: high suppression (mandatory participation in preparedness drills, compliance costs, resource allocation). Late stage: suppression declines as redundancy becomes normal and the scaffolding is removed. Theater ratio is moderate (some performative compliance to safety standards, but mostly functional infrastructure). Beneficiaries: all parties who inherit the resilient system. Victims: those bearing upfront compliance costs during early phases.
constraint_indexing:constraint_classification(antifragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANTIFRAGILITY INDUSTRY (PITON) — Consulting firms, business books, and corporate 'resilience programs' sell antifragility as a service. The actual antifragility principle (gain from disorder) requires real exposure to volatility and failure. Corporate programs substitute performative 'stress-testing' and 'resilience training' for actual antifragility. Employees participate in resilience theater while the corporation avoids real volatility (hedging risk, protecting divisions from accountability). Theater ratio is very high (0.75+): the program appears functional but delivers minimal actual antifragility. The constraint persists through institutional inertia (HR departments maintain the programs) despite degraded function. No real beneficiary except the consulting vendor.
constraint_indexing:constraint_classification(antifragility, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(antifragility, TR),
    TR >= 0.70.

:- end_tests(antifragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint's base extractiveness is low because antifragility is a pure coordination mechanism in most contexts — evolutionary populations, decentralized networks, and market economies all benefit from the antifragility principle without asymmetric extraction. However, extractiveness increases in institutionalized contexts where volatility exposure is concentrated (small business owners, precarious workers, resilience programs with compliance costs) and where performative versions of antifragility substitute for real volatility exposure (corporate resilience consulting). The trajectory from 0.15 to 0.28 reflects increasing theater and institutional layering. Suppression (0.12): Low. Antifragility does not require high suppression — systems achieve antifragility through exposure and adaptation, not coercion. Some institutional versions (scaffold perspective with compliance mandates, snare perspective with labor market traps) exhibit higher suppression, but the core principle has minimal suppressive overhead. Theater ratio (0.35, increasing from 0.20): Moderate and rising. Biological and market antifragility exhibit low theater (actual adaptations working). Decentralized networks have low theater (redundancy and failure recovery are functional). However, institutional antifragility programs (consulting, corporate training, compliance scaffolds) are increasingly performative — they create the appearance of antifragility without requiring agents to actually face real volatility. The rise in theater ratio indicates that institutional mediation is substituting ritual for reality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a universal principle (antifragility) manifests as different constraint types depending on structural position. The analytical observer sees an immutable natural law (Mountain): antifragility is how adaptive systems work at all scales. The evolutionary population and decentralized network see pure coordination (Rope): benefits are evenly distributed, no extraction, minimal theater. The small business owner sees a mixed coordination-extraction hybrid (Tangled Rope): some genuine antifragility benefit if survival occurs, but concentrated stress during volatility and constrained exit options during downturns. The precarious worker sees pure extraction (Snare): full volatility exposure with zero antifragility gain; trapped exit option; high suppression from labor market structure. The resilience-building coalition sees a temporary scaffold (Scaffold): intentional coordination to build antifragility infrastructure with explicit sunset. The antifragility industry sees a degraded ritual (Piton): performative resilience programs that substitute theater for real volatility exposure; maintains institutional presence through consulting revenue, not through functional antifragility delivery. The perspectival gap reveals that antifragility is not inherently extractive — its extractiveness depends entirely on whether institutional structures concentrate volatility exposure and unequally distribute the gains from adaptation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural relationship to volatility exposure and antifragility gain distribution. Agents that voluntarily expose to volatility and capture adaptive gains (evolutionary populations, decentralized networks, market entrepreneurs) have low d (beneficiary position) — they choose disorder because it improves their position. Agents with institutional power to avoid volatility while capturing gains (institutional investors, corporate executives hedging risk) have lower d than their formal power level would suggest (arbitrage exit option reduces d). Agents forced to bear volatility with zero adaptive gain (precarious workers, trapped in labor market) have high d (victim position) — they experience the full extraction without the benefit. The small business owner's mixed position (moderate power, constrained exit) produces mid-range d reflecting both benefit and extraction. The scaffold and piton perspectives derive from enforcement levels and theater ratios rather than power-level directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Antifragility resolves the mandatrophy by showing how a pure coordination principle can be institutionally converted into extraction. The mountain perspective correctly identifies the core principle: antifragility is a natural law of adaptive systems. The rope perspectives (evolutionary population, decentralized network) show that when agents voluntarily expose to disorder and share gains equally, no extraction occurs. The tangled rope perspective shows how institutional structures (financing arrangements, market discipline) convert shared antifragility into asymmetric extraction — the business owner captures gains only if they survive, but bears full stress during volatility. The snare perspective shows how institutional structures (labor market segmentation, no social safety net, volatile gig contracts) can extract volatility exposure without distributing any antifragility gains. The scaffold perspective shows how temporary institutional coordination can accelerate antifragility with declining suppression over time. The piton perspective shows how institutional actors (consulting firms, corporate HR) can substitute performative antifragility (training programs, stress-testing rituals) for real volatility exposure, converting a natural principle into an extractive revenue stream. The mandatrophy is resolved by recognizing that antifragility is a mountain-level principle, but its distribution through institutional structures creates secondary constraints (small_business_leverage, precarious_worker_volatility_extraction, corporate_resilience_theater) that range from rope to snare to piton. The network links these secondary constraints back to the antifragility foundation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_vs_fragility_measurement,
    'What empirical metric distinguishes antifragile gain from fragile loss when both involve volatility exposure?',
    'Time-series analysis of system capability (fitness, revenue, adaptive capacity) conditional on volatility shock; comparison of pre- and post-shock trajectory slopes; longitudinal tracking across multiple stressor types',
    'If metric is capability gain: antifragility is real and measurable (Mountain confirmed). If metric is merely survival: concept collapses into robustness (not antifragility). If metric is option value: antifragility is a financial derivative property (Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(antifragility_vs_fragility_measurement, empirical, 'How to measure genuine antifragile gain versus noise or mere robustness').

omega_variable(
    domain_specificity_of_antifragility,
    'Is antifragility a universal principle or domain-specific? Does the same mechanism that builds evolutionary fitness also build economic resilience or technological robustness?',
    'Cross-domain comparative analysis: identify structural homologies and differences in antifragility mechanisms across biological, economic, and technological systems; examine whether optimized volatility exposure in one domain transfers to another',
    'If universal (same mechanism): single constraint story with multiple perspectives (current approach confirmed). If domain-specific: decompose into separate constraints (antifragility_biological, antifragility_economic, antifragility_technological) with different ε values and structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_of_antifragility, conceptual, 'Whether antifragility is a universal principle or multiple domain-specific phenomena').

omega_variable(
    extraction_via_volatility_asymmetry,
    'When different agents bear different volatility exposure (owner vs precarious worker), is the extraction mechanism a feature of antifragility or a separate constraint (labor market extraction)?',
    'Decompose the snare perspective (precarious worker) from the tangled rope perspective (small business owner) into separate constraint stories with independent ε values; verify whether removing the extraction mechanism would also remove the antifragility benefit',
    'If extraction is essential to antifragility: single story confirmed (current approach). If extraction is contingent: precarious_worker_exposure becomes a separate snare story downstream of antifragility (network link). Precarious worker cannot gain antifragility without changing structural position (exit option, power level).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_volatility_asymmetry, empirical, 'Whether volatility-based extraction is inherent to antifragility or a separate institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antifrag_tr_t0, antifragility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(antifrag_tr_t5, antifragility, theater_ratio, 5, 0.28).
narrative_ontology:measurement(antifrag_tr_t10, antifragility, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(antifrag_be_t0, antifragility, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(antifrag_be_t5, antifragility, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(antifrag_be_t10, antifragility, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility, resource_allocation).
narrative_ontology:affects_constraint(antifragility, market_selection_mechanism).
narrative_ontology:affects_constraint(antifragility, genetic_variation_pressure).
narrative_ontology:affects_constraint(antifragility, precarious_worker_volatility_extraction).
narrative_ontology:affects_constraint(antifragility, corporate_resilience_theater).

% DUAL FORMULATION NOTE:
% Antifragility is the upstream constraint: a universal principle of adaptive systems gaining from volatility. Downstream constraints inherit this principle but express it through institutional structures with different ε values: market_selection_mechanism (ε≈0.15, Rope) implements antifragility in competitive economies; precarious_worker_volatility_extraction (ε≈0.68, Snare) is an institutional extraction mechanism that forces volatility exposure without distributing antifragility gains; corporate_resilience_theater (ε≈0.25, Piton) is a degraded version where performative programs substitute for real antifragility. The network decomposition recognizes that 'antifragility' as a natural principle (Mountain, ε≈0.08) is distinct from how institutional actors distribute its costs and benefits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(antifragility, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
