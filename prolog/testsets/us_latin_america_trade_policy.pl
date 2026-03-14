% ============================================================================
% CONSTRAINT STORY: us_latin_america_trade_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_latin_america_trade_policy, []).

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
 *   constraint_id: us_latin_america_trade_policy
 *   human_readable: US-Latin America Trade Policy Asymmetry
 *   domain: international_trade/geopolitical
 *
 * SUMMARY:
 *   US-Latin America trade policy operates as a structural constraint on
 *   regional development trajectories through asymmetric tariff enforcement,
 *   conditional lending, and supply-chain integration that concentrates
 *   extraction in the US while distributing costs across Latin American
 *   societies. The constraint exhibits genuine coordination functions (supply
 *   chains, financial stability, security partnerships) alongside substantial
 *   extraction (profit repatriation, policy constraints, manufacturing
 *   displacement). Trade rules create mutual dependency while the US
 *   maintains structural advantages through capital abundance, market size,
 *   and enforcement capacity. The constraint's extractiveness has increased
 *   over the interval (0.42 → 0.58) as trade volume has grown, and theater
 *   ratio has risen (0.55 → 0.70) as performative renegotiations (USMCA,
 *   bilateral talks) substitute for fundamental restructuring. Regional
 *   alternative frameworks (ALBA, CELAC, post-NAFTA reformers) represent
 *   organized resistance with genuine but constrained exit potential.
 *
 * KEY AGENTS:
 *   - Latin American Manufacturing Workers: Primary victims (powerless/trapped) — face factory closures and unemployment when US-supported competitors enter markets; cannot exit regional dependency without dislocation
 *   - US Multinational Corporations: Primary beneficiaries (institutional/arbitrage) — source production in low-wage regions, export finished goods under tariff protection; can reallocate if terms worsen
 *   - Latin American Governments: Secondary beneficiaries and constrained actors (moderate/constrained) — depend on US foreign aid, capital inflows, and market access; constrained by IMF conditions and capital flight risk
 *   - Latin American Domestic Manufacturing: Secondary victim (moderate/constrained) — compete against subsidized US imports; face capital constraints and technology gaps
 *   - US Agricultural Producers: Primary beneficiaries (powerful/mobile) — export under preferential tariffs while competitors face barriers; highly mobile globally but embedded in regional supply chains
 *   - Regional Trade Blocs: Organized agents building alternatives (organized/constrained) — ALBA, CELAC, post-NAFTA reformers pursuing regional integration with sunset toward autonomy
 *   - Cold War Institutional Frameworks: Organizational actors (institutional/mobile) — NAFTA/CAFTA structures persist through inertia despite geopolitical shifts; performatively updated without functional restructuring
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_latin_america_trade_policy, 0.58).
domain_priors:suppression_score(us_latin_america_trade_policy, 0.65).
domain_priors:theater_ratio(us_latin_america_trade_policy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_latin_america_trade_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_latin_america_trade_policy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_latin_america_trade_policy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_latin_america_trade_policy, tangled_rope).
narrative_ontology:human_readable(us_latin_america_trade_policy, "US-Latin America Trade Policy Asymmetry").
narrative_ontology:topic_domain(us_latin_america_trade_policy, "international_trade/geopolitical").

domain_priors:requires_active_enforcement(us_latin_america_trade_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_latin_america_trade_policy, us_capital_intensive_sectors).
narrative_ontology:constraint_beneficiary(us_latin_america_trade_policy, us_agricultural_producers).
narrative_ontology:constraint_beneficiary(us_latin_america_trade_policy, us_multinational_corporations).
narrative_ontology:constraint_victim(us_latin_america_trade_policy, latin_american_domestic_manufacturing).
narrative_ontology:constraint_victim(us_latin_america_trade_policy, latin_american_agricultural_workers).
narrative_ontology:constraint_victim(us_latin_america_trade_policy, latin_american_small_medium_enterprises).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATIN AMERICAN MANUFACTURING WORKERS (SNARE) — Trapped by structural dependency on US market access; cannot exit without massive economic dislocation. Trade rules enforce US manufacturing advantage; tariff barriers protect US competitors. High suppression through limited alternative export markets and capital flight risk if FDI withdraws. Coordination function minimal — extraction dominates.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LATIN AMERICAN GOVERNMENTS (TANGLED ROPE) — Constrained by IMF structural adjustment requirements, currency reserves management, and capital flight risk if they deviate from US-preferred policies. Simultaneously benefit from US foreign aid, investment, and security partnerships. Active enforcement through conditional lending and market access threats. Asymmetric extraction coexists with genuine coordination on financial stability.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MULTINATIONAL CORPORATIONS (ROPE) — Primary beneficiaries with arbitrage options. Trade rules enable supply-chain optimization: source low-wage manufacturing in Latin America, export finished goods back under tariff protection. Can relocate investment if terms worsen. Experiences constraint as coordination mechanism enabling efficient production networks.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL TRADE BLOC ADVOCATES (SCAFFOLD) — Organized agents (ALBA, CELAC, post-NAFTA reformers) pursuing alternative regional integration with sunset trajectory toward reduced US dependence. Lower effective extraction because these agents have agency and visible exit pathways through intra-regional trade. Theater high (performative regional unity rhetoric) but functional coordination developing.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COLD WAR TRADE FRAMEWORKS (PITON) — Traditional NAFTA/CAFTA architecture persists through institutional inertia despite changed geopolitical context (China competition, regional climate/migration coordination requirements). Trade preference systems are largely performative maintenance of alliance structures. Theater ratio high — ongoing renegotiations and 'updates' substitute for fundamental restructuring. Functional coordination has atrophied.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: US AGRICULTURAL AND CAPITAL SECTORS (TANGLED ROPE) — Powerful agents with genuine mobile exit options (can reallocate globally) but deeply embedded in Latin American supply chains. Benefit from asymmetric terms but also coordinate on stabilizing access to inputs, labor, and markets. Extraction coexists with coordination — the constraint serves both functions simultaneously.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal scope, the constraint appears as an immutable structural feature: geographic proximity, capital differential, and historical empire-building produce inevitable asymmetry in trade terms. This perspective naturalizes institutional arrangements as laws of economic geography. However, the structural data contradicts the mountain classification — observable dependency mechanisms (tariff structure, IPR enforcement, capital controls) are policy choices, not natural limits. False summit.
constraint_indexing:constraint_classification(us_latin_america_trade_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_latin_america_trade_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_latin_america_trade_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_latin_america_trade_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_latin_america_trade_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_latin_america_trade_policy, TR),
    TR >= 0.70.

:- end_tests(us_latin_america_trade_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting substantial asymmetric terms alongside genuine coordination functions. The original trade dependency created by NAFTA/CAFTA involved tariff asymmetry and supply-chain integration that concentrated gains in capital-intensive sectors (automotive, agriculture) in the US while displacing Latin American manufacturing. Over the 9-year interval, extractiveness increased as trade volume grew and Chinese competition intensified Latin American pressure to integrate further into US-centered supply chains. The growth trajectory (0.42 → 0.58) reflects accumulating extraction rather than initial establishment — the framework was contingent, not inevitable. Suppression (0.65): Moderate-high. Active enforcement through tariff barriers, conditional lending (IMF), and capital account restrictions creates material barriers to exit. But suppression is not total — some regional governments have successfully diversified exports (commodity booms, intra-regional trade), and informal economy activity persists outside formal supply chains. Theater ratio (0.68): High and increasing. USMCA renegotiation, ongoing bilateral talks, and performative regional summit rhetoric (PROSUR, ALBA declarations) substitute for structural change. The negotiation theater maintains the framework's legitimacy while suppression mechanisms persist unchanged. Trade preference systems perform 'partnership' while enforcement remains asymmetric.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence: beneficiaries (US sectors) see coordination; victims (Latin American workers) see pure extraction; organized agents (regional blocs) see a temporary problem with exits; institutional holdovers see their own inertia; governments see constrained negotiation spaces; the analytical observer risks seeing natural law. The disagreement is not measurement error but genuine structural difference in position.
 *
 * DIRECTIONALITY LOGIC:
 *   US institutional power with arbitrage exit → low/negative d; Latin American powerless with trapped exit → high d; Latin American moderate governments with constrained exit → moderate d; organized regional blocs with constrained but directional exit → lower d than trapped counterparts. Suppression (0.65) is a structural property unscaled by context — material barriers to exit are real and substantial regardless of agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint requires mandatrophy analysis because tangled_rope classification claims BOTH coordination and asymmetric extraction coexist, and the tangled_rope type can obscure mislabeling. Mandatrophy resolution: The coordination function is genuine — supply chains do require coordination, financial stability is mutually beneficial, security partnerships have real value. The asymmetric extraction is also genuine — US sectors capture disproportionate gains, tariff structures enforce asymmetry, policy autonomy is constrained through conditional lending. Neither function overwhelms the other: removing coordination (pure snare) would be empirically false (supply chains genuinely work); removing extraction (pure rope) would be empirically false (gains are asymmetric and enforced). The tangled rope classification holds because both functions are essential to the constraint's operation. The mandatrophy is resolved by the perspectival gap: beneficiaries experience primarily coordination, victims experience primarily extraction, analytical observers risk naturalizing the arrangement as inevitable. All three readings are structurally sound from their positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_dependency_mechanism,
    'Is Latin American trade dependency a structural feature of geographic/capital differentials or a contingent policy outcome?',
    'Counterfactual analysis: regional trade volumes under alternative tariff regimes (full reciprocity, regional preferencing, non-aligned manufacturing). Historical comparison with periods of higher regional autonomy (1970s ISI era).',
    'If structural: constraint is mountain-like and exits are genuinely limited. If contingent: classification shifts toward snare (pure extraction via policy enforcement). Affects whether regional alternatives (ALBA, CELAC) represent real exit options or aspirational framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_dependency_mechanism, empirical, 'Structural dependency vs. policy-driven asymmetry').

omega_variable(
    us_enforcement_cost,
    'What fraction of the constraint''s suppression comes from active US enforcement (tariffs, sanctions, capital controls) vs. passive market dominance?',
    'Time-series analysis of trade volume correlation with enforcement intensity (tariff changes, FDI restrictions). Regional comparison with non-US trade partners (China, EU) to isolate US-specific suppression mechanisms.',
    'If enforcement-heavy: constraint is snare (requires coercion). If market-dominant: constraint is rope (coordination under asymmetric terms). Affects interpretation of alternative exit options and theater ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_enforcement_cost, empirical, 'Active enforcement vs. passive market dominance').

omega_variable(
    regional_exit_credibility,
    'Are ALBA, CELAC, and post-NAFTA regional alternatives functionally viable exits or performative regional theater?',
    'Trade volume tracking: within-region trade growth, diversification of exports beyond US markets, reduction in capital flight under regional frameworks. Institutional capacity: tariff coordination, dispute resolution, enforcement mechanisms in regional bodies.',
    'If viable: scaffold perspective confirmed, generational sunset is real, organized agents have genuine alternatives. If performative: regional alternatives are aspirational framing, suppression remains high, classification stays snare/tangled rope. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_exit_credibility, empirical, 'Viability of regional trade alternative exits').

omega_variable(
    latin_american_agency_framing,
    'Do Latin American governments perceive themselves as trapped, constrained, or rationally accepting asymmetric terms in exchange for stability and investment?',
    'Policy discourse analysis: rhetoric in IMF negotiations, trade agreements, regional forums. Revealed preference: government choices when US leverage is absent (commodity booms reducing capital dependency). Agent interviews and internal policy documents.',
    'If genuinely constrained with exit costs: tangled rope classification holds. If identity-locked (have internalized US economic framework): exits are structurally available but cognitively unavailable; classification becomes more snare-like. If rationally accepting: classification could be rope (coordination under asymmetric but mutually beneficial terms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_american_agency_framing, conceptual, 'Latin American government agency perception').

omega_variable(
    china_as_alternative_extraction,
    'Does Chinese trade expansion represent genuine alternative exit option or replacement extractor with different asymmetries?',
    'Comparative analysis: trade dependency metrics (export concentration, tariff barriers, supply chain integration) under US vs. Chinese partnership. Debt sustainability and policy autonomy under Chinese lending vs. IMF conditionality.',
    'If genuine alternative: regional agents have more agency, scaffold exit viable. If replacement extraction: agents perceive smaller exit set, snare characteristics persist. Affects assessment of whether constraint is inherent or contingent on US dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_as_alternative_extraction, empirical, 'China as alternative or replacement extractor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_latin_america_trade_policy, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uslat_tr_t0, us_latin_america_trade_policy, theater_ratio, 0, 0.55).
narrative_ontology:measurement(uslat_tr_t3, us_latin_america_trade_policy, theater_ratio, 3, 0.62).
narrative_ontology:measurement(uslat_tr_t6, us_latin_america_trade_policy, theater_ratio, 6, 0.68).
narrative_ontology:measurement(uslat_tr_t9, us_latin_america_trade_policy, theater_ratio, 9, 0.7).

% Extraction over time
narrative_ontology:measurement(uslat_be_t0, us_latin_america_trade_policy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uslat_be_t3, us_latin_america_trade_policy, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(uslat_be_t6, us_latin_america_trade_policy, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(uslat_be_t9, us_latin_america_trade_policy, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_latin_america_trade_policy, resource_allocation).
narrative_ontology:affects_constraint(us_latin_america_trade_policy, imf_structural_adjustment).
narrative_ontology:affects_constraint(us_latin_america_trade_policy, us_agricultural_subsidies).
narrative_ontology:affects_constraint(us_latin_america_trade_policy, chinese_trade_expansion_latam).

% DUAL FORMULATION NOTE:
% US-Latin America trade policy is downstream of global financial architecture (IMF conditionality) and upstream of specific sectoral constraints (agricultural commodity prices, manufacturing competitiveness). The three linked constraints form a constraint family: IMF structural adjustment enforces trade openness which enables US agricultural export advantage and simultaneously creates vulnerability to Chinese manufacturing competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_latin_america_trade_policy, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
