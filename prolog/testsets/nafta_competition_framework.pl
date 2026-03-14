% ============================================================================
% CONSTRAINT STORY: nafta_competition_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_competition_framework, []).

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
 *   constraint_id: nafta_competition_framework
 *   human_readable: NAFTA Competition Framework: Trade Liberalization with Embedded Extraction
 *   domain: trade_policy/economic_regulation
 *
 * SUMMARY:
 *   NAFTA (1994-2020, superseded by USMCA 2020-present) was presented as a
 *   coordination mechanism to liberalize North American trade, enable
 *   integrated supply chains, and create mutual prosperity. Instead, it
 *   functioned as a hybrid coordination-extraction framework that genuinely
 *   coordinated multinational supply chains while simultaneously extracting
 *   from labor-intensive sectors and geographic communities dependent on
 *   traditional manufacturing. The constraint exhibits all three dimensions:
 *   it coordinates capital flows and corporate supply chains (rope for
 *   multinationals), embeds asymmetric extraction through rules of origin and
 *   geographic wage arbitrage (snare for manufacturing workers), maintains
 *   performative dispute resolution that protects capital against
 *   labor/environmental regulation (piton), enables some reorganization and
 *   benefits alongside extraction (tangled rope for moderate actors), and
 *   contains sunset logic through renegotiation cycles (scaffold for
 *   coalition actors). The theater ratio increased over the interval as NAFTA
 *   governance shifted from trade coordination to investor protection
 *   (Chapter 11 litigation), and extractiveness accumulated as the
 *   mechanism's distributional asymmetries became institutionalized in supply
 *   chain lock-in.
 *
 * KEY AGENTS:
 *   - Mexican agricultural workers: Primary victim (powerless/trapped) — smallholder corn farmers displaced by cheap US imports with no alternative income
 *   - US manufacturing workers: Primary victim (powerless/trapped) — factory relocations to Mexico eliminate jobs in communities with limited alternatives
 *   - Multinational corporations: Primary beneficiary (institutional/arbitrage) — access to integrated North American supply chains with tariff-free market access
 *   - US financial services sector: Primary beneficiary (institutional/arbitrage) — market liberalization in Mexico and Canada with implicit US dominance
 *   - Small domestic manufacturers: Secondary victim (moderate/constrained) — face integration barriers and supply chain lock-in but retain some agency
 *   - Canadian resource communities: Mixed victim/beneficiary (organized/constrained) — coordinate with continental markets but locked into export patterns and energy pricing constraints
 *   - Labor and environmental coalition: Organized advocates (organized/constrained) — perceive NAFTA as temporary, renegotiation as sunset mechanism, but retain limited structural power
 *   - Trade administration bodies: Institutional maintainer (organized/constrained) — enforce NAFTA rules while theater ratio increases through litigation-driven governance
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_competition_framework, 0.58).
domain_priors:suppression_score(nafta_competition_framework, 0.65).
domain_priors:theater_ratio(nafta_competition_framework, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_competition_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(nafta_competition_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nafta_competition_framework, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_competition_framework, tangled_rope).
narrative_ontology:human_readable(nafta_competition_framework, "NAFTA Competition Framework: Trade Liberalization with Embedded Extraction").
narrative_ontology:topic_domain(nafta_competition_framework, "trade_policy/economic_regulation").

domain_priors:requires_active_enforcement(nafta_competition_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_competition_framework, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_competition_framework, capital_intensive_exporters).
narrative_ontology:constraint_beneficiary(nafta_competition_framework, us_financial_services).
narrative_ontology:constraint_victim(nafta_competition_framework, small_domestic_manufacturers).
narrative_ontology:constraint_victim(nafta_competition_framework, labor_intensive_sectors).
narrative_ontology:constraint_victim(nafta_competition_framework, mexican_agricultural_workers).
narrative_ontology:constraint_victim(nafta_competition_framework, canadian_resource_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEXICAN AGRICULTURAL WORKERS (SNARE) — Trapped within NAFTA's agricultural liberalization framework. Smallholder corn farmers cannot exit; cheap US corn imports undercut domestic prices, eliminating viable cultivation. No alternative income sources in depressed rural communities. Maximum suppression through geographic immobility and lack of retraining access. Pure extraction: forced into migration, informal labor, or subsistence poverty.
constraint_indexing:constraint_classification(nafta_competition_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US MANUFACTURING WORKERS (SNARE) — Trapped by factory relocations to Mexico where labor costs are 5-10x lower. No meaningful exit: factory towns offer limited alternative employment; relocation entails family separation and housing loss. NAFTA rules of origin allow US firms to externalize labor costs while maintaining tariff-free access to US market. High suppression: retraining programs insufficient, union leverage collapsed post-NAFTA.
constraint_indexing:constraint_classification(nafta_competition_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL DOMESTIC MANUFACTURERS (TANGLED ROPE) — Constrained but not trapped. Face tariff elimination and rules of origin that advantage integrated supply chains only accessible to large firms. High barriers to exit: cannot relocate to Mexico (lack capital), cannot compete with integrated North American producers, cannot exit NAFTA-dependent supply chains without business failure. But some firms adapted through specialization or integration into larger networks. Moderate extraction: real constraints but some agency and some coordination benefit from market access.
constraint_indexing:constraint_classification(nafta_competition_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATIONS (ROPE) — Primary beneficiaries with arbitrage options. NAFTA enables supply chain optimization (sourcing components from lowest-cost location while maintaining tariff-free market access). Can coordinate procurement across borders, shift production based on labor costs, and access Mexican and Canadian markets without tariff barriers. Net beneficiary—this is their coordination mechanism. No extraction experienced; constraint enables their business model.
constraint_indexing:constraint_classification(nafta_competition_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: US FINANCIAL SERVICES (ROPE) — Benefits from NAFTA chapter 11 (financial services liberalization) and investor protections. US banks access Mexican and Canadian financial markets; no reciprocal constraint on US market access. Experiences NAFTA as pure coordination: rules clarify market access, enabling profitable expansion. Minimal suppression; maximum agency. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(nafta_competition_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADE ADMINISTRATION BODIES (PITON) — NAFTA panels and dispute resolution mechanisms maintain performative legitimacy while their actual function has degraded. Chapter 11 investor-state dispute settlement (ISDS) panels have become extraction mechanisms for corporations against governments (tobacco regulations, environmental policy), contradicting the trade liberalization mandate. Theater ratio high: dispute resolution appears neutral and rule-based but systematically advantages capital over labor and environment. Constrained: North American governments cannot easily exit NAFTA architecture without triggering investor lawsuits.
constraint_indexing:constraint_classification(nafta_competition_framework, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: CANADIAN RESOURCE COMMUNITIES (TANGLED ROPE) — Constrained by NAFTA's rules locking in resource export patterns (softwood lumber, minerals, energy). Some coordination benefit: access to continental markets at scale. But also extraction: energy prices locked below market rates during 1990s-2000s; rules prevent domestic value-added processing (log export clauses). Constrained exit: political economy of resource dependence makes diversification difficult; divestment from other sectors already occurred. Mixed: some genuine trade benefit, significant asymmetric cost.
constraint_indexing:constraint_classification(nafta_competition_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: LABOR & ENVIRONMENTAL COALITION (SCAFFOLD) — See NAFTA as a temporary institutional arrangement with a sunset built in through renegotiation pressures. Labor side agreements and environmental side agreements (negotiated 1993) were explicitly framed as transitional mechanisms to manage NAFTA's distributional costs. Coalition perceives renegotiation (which occurred 2020, USMCA) as evidence that sunset is real. However, USMCA retains core asymmetries, so sunset logic is partly aspirational. Constrained exit: coalition has political voice but limited structural power to force transformative change without renegotiation leverage.
constraint_indexing:constraint_classification(nafta_competition_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, comparative advantage and factor price equalization are immutable economic laws. NAFTA merely reveals the natural equilibrium: labor-intensive production migrates to lower-wage jurisdictions, and capital-intensive production concentrates in capital-rich jurisdictions. The distribution of winners and losers is natural, not institutional. However, this perspective naturalizes the political choice to embed this distribution in a legal framework (NAFTA's rules of origin, ISDS, energy lockups) that prevents democratic correction. Engine will flag this as false summit.
constraint_indexing:constraint_classification(nafta_competition_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_competition_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nafta_competition_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nafta_competition_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_competition_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nafta_competition_framework, TR),
    TR >= 0.70.

:- end_tests(nafta_competition_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. NAFTA creates genuine coordination benefits for multinational supply chains (estimated 0.25-0.35 of total effect) alongside asymmetric extraction through geographic wage arbitrage, rules of origin, and capital mobility. The extractiveness value reflects the net: significant extraction from labor and geographically concentrated sectors, but not total extraction (some coordination function persists, and some actors retain agency to adapt). Suppression (0.65): High. Multiple barriers prevent exit: rules of origin lock suppliers into North American networks; geographic immobility traps workers in depressed communities; energy lockups (Canada) prevent diversification; NAFTA dispute mechanisms penalize policy experimentation on labor/environmental grounds. However, suppression is not total—multinational firms have arbitrage options, and renegotiation is possible. Theater ratio (0.62): Moderate-high and increasing. NAFTA dispute resolution (especially Chapter 11) maintains appearance of neutral, rule-based governance while systematically extracting through litigation that protects capital against labor and environmental regulation. USMCA labor chapters appear responsive but show limited enforcement mechanisms. Theater increased over the 1994-2020 interval as ISDS litigation became the primary governance mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the multinational perspective (rope: NAFTA is coordination) and the manufacturing worker perspective (snare: NAFTA is displacement) is maximal—approximately 2-3 types and 1.5+ in chi magnitude. This gap reveals that NAFTA is fundamentally a framework that enabled some actors to coordinate while imposing extraction on others. The classification outputs serve as a diagnostic: if NAFTA were truly a coordination mechanism, all perspectives would cluster around rope. Instead, the full spread (snare, tangled rope, rope, piton, scaffold) indicates an asymmetric institutional framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction (d values) is determined by structural relationship to NAFTA's core mechanism: access to integrated North American supply chains with tariff-free market access. Multinationals have arbitrage options and benefit directly—low d. Workers trapped in geographic communities without alternative employment bear extraction—high d. Moderate actors face constraints but retain some adaptation capacity—moderate d. The accumulation of extractiveness over the 1994-2020 interval (0.35 → 0.52 → 0.58) reflects institutional embedding: early NAFTA was experienced as disruptive but reversible; mature NAFTA created lock-in through supply chain dependencies and geographic concentration of benefits, making extraction increasingly structural.
 *
 * MANDATROPHY ANALYSIS:
 *   NAFTA resolves the mandatrophy by showing that coordination and extraction are not mutually exclusive—they can coexist in the same institutional framework, distributed across different agent positions. NAFTA genuinely coordinates multinational supply chains (rope function) while simultaneously extracting from labor and geographically concentrated sectors (snare function). The constraint is not 'which type?' but 'how is this framework asymmetrically distributed?'. The analytics show that calling NAFTA 'trade liberalization' (suggesting rope/coordination) masks the asymmetric extraction embedded in rules of origin, ISDS, and geographic wage arbitrage. The mandatrophy resolves through perspectival precision: each perspective's classification is correct from that position, and the spread of types across perspectives reveals the true structure (asymmetric, hybrid, with sunset logic available through renegotiation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rules_of_origin_sufficiency,
    'Do NAFTA rules of origin genuinely coordinate North American production networks, or do they primarily extract through geographic wage arbitrage by allowing US firms to declare Mexican components as ''North American''?',
    'Historical analysis of actual component sourcing patterns; comparison of rules-of-origin declared origin vs actual origin; assessment of whether rules reduced shipping/logistics costs more than they enabled labor cost externalization',
    'If coordination-dominant: perspectives shift toward rope for more agents. If extraction-dominant: tangled_rope classification confirmed as snare masquerading as coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rules_of_origin_sufficiency, empirical, 'Whether rules of origin coordinate production or enable labor arbitrage').

omega_variable(
    chapter_11_isds_intentionality,
    'Was Chapter 11 (investor-state dispute settlement) designed as a constraint on labor/environmental regulation, or did extraction emerge through unanticipated interpretation and aggressive use by corporate litigants?',
    'Textual analysis of NAFTA drafting records and negotiation transcripts; comparison of negotiators'' stated intent vs realized ISDS precedent; assessment of whether early dispute patterns were foreseen',
    'If designed for extraction: snare classification strengthened. If emergent extraction through legal interpretation: tangled_rope classification confirmed; reveals how coordination mechanisms degrade into extraction mechanisms through strategic litigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chapter_11_isds_intentionality, conceptual, 'Whether ISDS extraction was intentional design or emergent behavior').

omega_variable(
    renegotiation_effectiveness_ceiling,
    'Can USMCA renegotiation create genuine substantive change to labor/environmental extraction, or is renegotiation itself a scaffold-theater that retains core asymmetries while appearing responsive?',
    'Longitudinal comparison of distributional outcomes pre-NAFTA, post-NAFTA, post-USMCA across Mexican wages, US manufacturing employment, environmental compliance; assessment of whether labor provisions in USMCA are enforceable or performative',
    'If substantive change: scaffold classification confirmed; extraction pressure genuinely declining. If theater: scaffold is piton; renegotiation is ritual that preserves extraction while maintaining legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renegotiation_effectiveness_ceiling, empirical, 'Whether USMCA renegotiation achieves substantive change').

omega_variable(
    factor_price_equalization_inevitability,
    'Is the geographic concentration of manufacturing job losses and wage stagnation in North America an inevitable consequence of comparative advantage, or is it a contingent outcome of NAFTA''s specific rule architecture (rules of origin, ISDS, energy lockups, weak labor enforcement)?',
    'Counterfactual institutional design: comparison with alternative free trade models (e.g., EU labor mobility + regulatory harmonization, or bilateral treaties with labor standards pre-embedded); assessment of whether same outcomes would obtain under different institutional rules',
    'If inevitable: mountain classification validated; constraint is natural law. If contingent: false summit revealed; constraint is institutional choice masquerading as economic necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(factor_price_equalization_inevitability, conceptual, 'Whether wage distribution outcomes are inevitable or contingent on institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_competition_framework, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_tr_t0, nafta_competition_framework, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nafta_tr_t7, nafta_competition_framework, theater_ratio, 7, 0.55).
narrative_ontology:measurement(nafta_tr_t14, nafta_competition_framework, theater_ratio, 14, 0.62).
narrative_ontology:measurement(nafta_tr_t21, nafta_competition_framework, theater_ratio, 21, 0.62).

% Extraction over time
narrative_ontology:measurement(nafta_be_t0, nafta_competition_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nafta_be_t7, nafta_competition_framework, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(nafta_be_t14, nafta_competition_framework, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(nafta_be_t21, nafta_competition_framework, base_extractiveness, 21, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_competition_framework, resource_allocation).
narrative_ontology:affects_constraint(nafta_competition_framework, supply_chain_lock_in).
narrative_ontology:affects_constraint(nafta_competition_framework, chapter_11_investor_state_power).
narrative_ontology:affects_constraint(nafta_competition_framework, mexican_agricultural_collapse).

% DUAL FORMULATION NOTE:
% NAFTA is upstream of specific sectoral extraction constraints: supply chain lock-in (medium extractiveness, institutional dependence), Chapter 11 investor rights (high extractiveness, government policy constraint), Mexican agricultural displacement (very high extractiveness, labor market collapse). Each downstream constraint has its own story with ε ≥ 0.50; NAFTA is the institutional architecture enabling all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_competition_framework, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
