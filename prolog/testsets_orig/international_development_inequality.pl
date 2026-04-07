% ============================================================================
% CONSTRAINT STORY: international_development_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_development_inequality, []).

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
 *   constraint_id: international_development_inequality
 *   human_readable: International Development Inequality Constraint
 *   domain: political_economy/international_development
 *
 * SUMMARY:
 *   International development inequality represents a structural constraint
 *   where capital-rich nations, multinational corporations, and financial
 *   institutions extract economic value from low-income countries through
 *   mechanisms that superficially appear as coordination (trade, finance,
 *   aid) but operate asymmetrically. The constraint exhibits all six DR types
 *   depending on the observer's structural position. From the perspective of
 *   resource-dependent nations with no alternative export markets and
 *   debt-service obligations, the constraint appears as a snare:
 *   insurmountable barriers to exit and maximum extraction. From the
 *   perspective of capital-rich nations with reserve-currency privileges and
 *   investment portfolio diversification, the same constraint appears as
 *   rope: pure coordination enabling efficient capital allocation and trade.
 *   The constraint's theater_ratio (0.58) reflects that development aid,
 *   capacity building, and institutional reform programs are substantially
 *   performative — visible outputs (projects completed, institutions built,
 *   targets met) decouple from structural inequality reduction. The
 *   extractiveness has drifted upward (0.42 → 0.58) over the 40-year interval
 *   as conditionality regimes have intensified, commodity-price volatility
 *   has increased pressure on poor countries, and capital markets have
 *   enabled more sophisticated extraction through currency arbitrage and
 *   debt-trap lending.
 *
 * KEY AGENTS:
 *   - Low-income countries (resource-dependent): Primary victims (powerless/trapped) — face structural entrapment through debt obligations, export concentration, and institutional dependencies
 *   - Multinational corporations and financial institutions: Primary beneficiaries (institutional/arbitrage) — extract through terms-of-trade advantage, capital flight optionality, and conditionality enforcement
 *   - Capital-rich nations: Secondary beneficiaries (institutional/arbitrage) — benefit from reserve-currency seigniorage, investment returns, and market access
 *   - Emerging market middle class: Secondary victim (moderate/constrained) — face currency exposure and skill-export dependence; also benefit from supply-chain integration
 *   - Multilateral development banks: Dual-role institutional actor (institutional/constrained) — coordinate development finance but simultaneously enforce extraction through conditionality
 *   - Regional integration coalitions (ASEAN, MERCOSUR, AU): Organized agents (organized/constrained) — attempting to restructure inequality through collective bargaining and internal integration
 *   - Development aid apparatus: Institutional actors (institutional/arbitrage) — maintain theatrical performance of development support with low structural impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_development_inequality, 0.58).
domain_priors:suppression_score(international_development_inequality, 0.62).
domain_priors:theater_ratio(international_development_inequality, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_development_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_development_inequality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(international_development_inequality, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_development_inequality, tangled_rope).
narrative_ontology:human_readable(international_development_inequality, "International Development Inequality Constraint").
narrative_ontology:topic_domain(international_development_inequality, "political_economy/international_development").

domain_priors:requires_active_enforcement(international_development_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_development_inequality, capital_rich_nations).
narrative_ontology:constraint_beneficiary(international_development_inequality, multinational_corporations).
narrative_ontology:constraint_beneficiary(international_development_inequality, financial_institutions).
narrative_ontology:constraint_victim(international_development_inequality, low_income_countries).
narrative_ontology:constraint_victim(international_development_inequality, local_workers).
narrative_ontology:constraint_victim(international_development_inequality, extractive_resource_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-DEPENDENT NATION (SNARE) — Low-income countries with concentrated export economies (mineral, agricultural) face structural entrapment: debt obligations to multilateral institutions, terms-of-trade volatility, and colonial-era infrastructure dependencies create insurmountable barriers to exit. Maximum extraction; minimal coordination benefit. The constraint operates through capital account control and conditional lending.
constraint_indexing:constraint_classification(international_development_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING MARKET MIDDLE CLASS (TANGLED ROPE) — Constrained by currency exposure, skill-export dependence, and brain drain incentives, but also benefits from integration into global supply chains, access to capital, and professional networks. Mixed extraction and coordination: the constraint coordinates their productivity but asymmetrically extracts through exchange-rate exposure and remittance obligations.
constraint_indexing:constraint_classification(international_development_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-RICH NATION TREASURY (ROPE) — Benefits from comparative advantage, remittance inflows, market access for exports, and low-cost capital flows. Experiences the constraint as coordination: reserve-currency status enables surplus recycling that funds development projects. Net beneficiary with exit optionality through arbitrage (currency diversification, capital flight).
constraint_indexing:constraint_classification(international_development_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTILATERAL DEVELOPMENT BANK (TANGLED ROPE) — Genuinely coordinates development finance (reduces information asymmetry, enables knowledge transfer), but simultaneously operates as an extraction mechanism through conditionality regimes, structural adjustment requirements, and sovereignty constraint. Institutional actor constrained by governance rules and donor accountability, not by exit barriers — experiences the constraint as dual coordination and asymmetric extraction.
constraint_indexing:constraint_classification(international_development_inequality, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL INTEGRATION COALITIONS (SCAFFOLD) — ASEAN, MERCOSUR, African Union initiatives represent organized attempts to restructure the inequality constraint through reduced trade barriers within regions, joint infrastructure investment, and negotiated commodity agreements. These coalitions see the global inequality structure as a temporary coordination failure with a generational sunset: regional bloc power can gradually shift terms of trade and access. Extraction declines as coalition bargaining power grows.
constraint_indexing:constraint_classification(international_development_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPMENT AID THEATER (PITON) — Official development assistance (ODA), NGO programs, and capacity-building initiatives are substantially performative: aid flows average ~0.3% of donor GDP with visibility/domestic political value far exceeding impact; conditionality serves donor interests more than recipient development; measured outcomes (MDG/SDG attainment) decouple from structural inequality reduction. The theater persists through institutional inertia despite low functional change in core extractive dynamics — replaced by neither donor countries nor recipients because it serves symbolic functions for both.
constraint_indexing:constraint_classification(international_development_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scale, some inequality is inherent to initial capital accumulation and comparative advantage: countries with natural resources, geographic location, or early industrialization have structural advantages that compound. This perspective sees international development inequality as an immutable feature of how economies develop. However, this classification is vulnerable to false summit detection: historical contingency (colonial extraction, post-war institutional design, currency regimes) reveals the apparent 'law' as path-dependent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(international_development_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_development_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_development_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_development_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_development_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_development_inequality, TR),
    TR >= 0.70.

:- end_tests(international_development_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint operates through multiple channels: terms-of-trade deterioration for primary-commodity exporters, debt-service obligations that consume government revenue, capital flight conduits that drain resources, conditional lending that constrains fiscal autonomy, and profit repatriation mechanisms. The 0.58 value reflects that extraction is substantial but not total — poor countries still accumulate capital, some development occurs, and elite cooperation with the constraint system suggests some benefit flows to collaborating groups. The upward drift (0.42 → 0.58) indicates that extraction mechanisms have intensified as commodity volatility increased, capital markets globalized, and conditionality regimes expanded. Suppression (0.62): High. Multiple barriers prevent exit: capital controls and currency depreciation make alternative economic models costly; debt-service obligations create fiscal constraints; institutional dependencies (trained bureaucrats, imported technology, currency regimes) embed extraction mechanisms; and global supply-chain integration creates path-dependent specialization. But suppression is not total (some countries maintain alternative institutions, some exit attempts succeed) — value reflects strong barriers rather than impossibility. Theater ratio (0.58): Moderate-high. Development aid apparatus (ODA, NGO programs, capacity building) produces visible outputs that decouple from structural change: projects completed without institutional innovation; targets met (MDGs/SDGs) while inequality persists; knowledge transfer without power redistribution. The theater has increased over time as aid programs proliferated while extraction mechanisms deepened. Aid serves signaling functions for both donors (demonstrating commitment to development) and recipients (providing legitimacy for elite collaboration with external institutions) without requiring structural change.
 *
 * PERSPECTIVAL GAP:
 *   Resource-dependent nation sees snare (trapped, no exit). Capital-rich nation sees rope (arbitrage, net beneficiary). Emerging market sees tangled rope (constrained, mixed). Development bank sees tangled rope from within (constrained by governance, coordinating and extracting simultaneously). Regional coalitions see scaffold with sunset (organized, generational restructuring possible). Aid apparatus sees piton (degraded function, theater persists). Analytical observer risks mountain (inequality inherent to capital accumulation). The gap reveals that agent power and exit options are the primary drivers of perspectival divergence — not disagreement about facts, but about what barriers are surmountable and what directions value flows.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline computes d (beneficiary vs victim status) from declared structural relationships and exit options. Resource-dependent nations are victims with trapped exit → d ≈ 0.95 → maximum f(d) ≈ 1.42. Capital-rich nations are beneficiaries with arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 (negative, meaning extraction flows toward them). Emerging market actors are mixed (some victim status, constrained exit) → d ≈ 0.65 → f(d) ≈ 1.00 (moderate). The global scope multiplier σ(global) = 1.2 amplifies χ for all perspectives, reflecting that inequality constraint operates at planetary scale. The perspectival gap shows agents with different power and exit positions experiencing radically different constraint types despite identical base ε and suppression values — the gap itself is diagnostic of an extraction constraint (if all perspectives produced the same type, the constraint would be more uniform/less politically contested).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE DIAGNOSTIC: The constraint qualifies as tangled rope (not pure snare) because it exhibits genuine coordination alongside asymmetric extraction. The coordination function: development finance reduces information asymmetry, enables capital flows to high-return uses, transfers knowledge and technology, and coordinates cross-border supply chains. These are real coordination services. The extraction function: conditionality constrains fiscal autonomy, terms-of-trade deterioration transfers wealth to capital-rich nations, capital flight drains resources, and commodity dependence limits alternatives. Both functions coexist. The snare perspective (resource-dependent nations) sees maximum extraction with minimal coordination benefit — accurate from their structural position. The rope perspective (capital-rich nations) sees pure coordination — also accurate, because capital flows toward them and coordinated supply chains serve their interests. The mandatrophy is resolved by recognizing that BOTH are correct from their respective positions; the constraint is tangled rope in objective structure but appears differently across the presheaf of observations. The piton perspective (aid apparatus) reveals that theater has increased over time — aid programs produce visible outputs while extraction mechanisms deepen, suggesting the constraint's functional basis (genuine development coordination) is degrading into theater (visibility without impact) — classic piton signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_development_lag,
    'How much measured inequality reflects legitimate development lag (poor countries catching up) versus structural extraction (institutional mechanisms that prevent catch-up)?',
    'Historical convergence analysis: rates of per-capita income growth controlling for initial capital stock, human capital, and institutional stability. If convergence clubs emerge (persistent stratification), extraction dominates; if conditional convergence holds globally, development lag dominates.',
    'If extraction dominates: snare classification correct for resource-dependent nations; scaffold perspective overly optimistic about regional coalitions. If lag dominates: rope classification more accurate; inequality constraint is temporary coordination problem with automatic sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_development_lag, empirical, 'Whether measured inequality reflects catch-up dynamics or structural extraction').

omega_variable(
    conditionality_internalization,
    'Do low-income country elites internalize development bank conditionality as necessary discipline (identity lock) or experience it as external extraction (trapped)?',
    'Elite narrative analysis, policy adoption timing relative to conditionality imposition, coefficient on policy uptake for carrots vs sticks. If elites adopt conditionality ahead of/independent of funding pressure, internalization is real; if adoption correlates tightly with disbursement schedules, externally-imposed constraint dominates.',
    'If internalized: the constraint operates through identity fusion (developing country ministers have fused with IMF orthodoxy), making escape require identity reconstruction; classify as identity_locked. If external: constraint operates through material coercion (funding withdrawal), classify as trapped/constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_internalization, empirical, 'Whether conditionality is internalized or experienced as external coercion').

omega_variable(
    capital_flight_volition,
    'Do capital flows from poor to rich countries represent voluntary arbitrage by wealthy elites or structural extraction through financial market operations?',
    'Flow decomposition analysis: separating FDI, portfolio investment, illicit capital flight, remittances, and debt service; correlation with political risk changes and currency volatility; elite preference revelation through capital location decisions and safe-haven holdings.',
    'If voluntary arbitrage: beneficiary status (capital-rich nations) is self-reinforcing through elite choice; institutional constraint is rope-type coordination of capital markets. If structural extraction: capital flight is predatory (extractive institutions drain resources); constraint is snare for source countries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flight_volition, empirical, 'Whether capital flows reflect elite arbitrage or structural extraction').

omega_variable(
    regional_bloc_coalition_sustainability,
    'Can regional integration coalitions (ASEAN, MERCOSUR, AU) sustain sufficient bargaining power to reduce global inequality structure, or do they fragment under internal competitive pressures?',
    'Coalition stability analysis: tracking member compliance with internal agreements, exit threats, and defection patterns; measuring coalition negotiating strength in WTO and bilateral trade talks; correlation with commodity price cycles.',
    'If sustainable: scaffold classification valid — generational sunset is real structural feature; organized agents can reshape constraint. If fragmentation: scaffold is aspirational; organized perspective is overly optimistic; real constraint persists as snare/tangled rope from powerless/moderate agent views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_bloc_coalition_sustainability, empirical, 'Whether regional coalition power can sustain against fragmentation').

omega_variable(
    aid_effectiveness_threshold,
    'Is the low measured impact of ODA and development programs due to implementation failure (fixable through better design) or structural constraints (theater is all that can be delivered given extraction mechanism)?',
    'Experimental variation in aid conditionality, recipient country institutional quality, and donor coordination. If high-quality implementation produces outsized gains: theater is organizational pathology. If gains remain modest even with best practices: theater reflects structural constraint.',
    'If implementation failure: piton classification correct (degraded function persists through inertia). If structural constraint: aid theater is functional adaptation (donors and recipients both benefit from visibility without requiring real structural change); classify constraint as snare/tangled rope with aid as pressure relief valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aid_effectiveness_threshold, empirical, 'Whether aid ineffectiveness reflects implementation failure or structural constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_development_inequality, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, international_development_inequality, theater_ratio, 0, 0.38).
narrative_ontology:measurement(inte_tr_t20, international_development_inequality, theater_ratio, 20, 0.48).
narrative_ontology:measurement(inte_tr_t40, international_development_inequality, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, international_development_inequality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inte_be_t20, international_development_inequality, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(inte_be_t40, international_development_inequality, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_development_inequality, resource_allocation).
narrative_ontology:affects_constraint(international_development_inequality, commodity_price_volatility).
narrative_ontology:affects_constraint(international_development_inequality, debt_trap_lending).
narrative_ontology:affects_constraint(international_development_inequality, capital_flight_mechanisms).
narrative_ontology:affects_constraint(international_development_inequality, conditional_lending_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_development_inequality, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
