% ============================================================================
% CONSTRAINT STORY: long_term_capital_markets_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_long_term_capital_markets_structure, []).

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
 *   constraint_id: long_term_capital_markets_structure
 *   human_readable: Long-Term Capital Markets Structure
 *   domain: financial_systems/capital_allocation
 *
 * SUMMARY:
 *   Long-term capital markets structure represents a hybrid
 *   extraction-coordination mechanism that has progressively shifted from
 *   coordination (1980s-1990s) toward extraction (2000s-present). The
 *   constraint governs how capital flows from savers to productive
 *   enterprises and how financial intermediation is compensated. The indexing
 *   revolution promised to democratize capital access through low-cost
 *   passive funds; instead, it created a new layer of monopolistic
 *   intermediation through index provider lock-in and benchmarking capture.
 *   Pension funds and endowments, intended as patient capital sources, have
 *   been trapped in short-term benchmarking and quarterly reporting cycles
 *   that systematically underfund long-term productive investment. Retail
 *   investors experience fee extraction disguised as market access.
 *   Productive enterprises face rising costs of long-term capital. The
 *   regulatory framework (SEC disclosure rules, fiduciary standards) has
 *   become largely performative — designed to address 20th-century problems
 *   (information asymmetry through prospectus requirements) while being blind
 *   to 21st-century extraction mechanisms (benchmarking lock-in, passive fee
 *   layering, principal-agent conflicts embedded in index methodology).
 *
 * KEY AGENTS:
 *   - Asset Managers with Scale: Primary beneficiary (institutional/arbitrage) — capture coordination rents through fee structures, network effects, and regulatory advantage
 *   - Index Providers (S&P Global, MSCI, Bloomberg): Secondary beneficiary (institutional/constrained) — coordinate capital allocation but also extract through methodology opacity and switching costs
 *   - Retail Investors: Primary victim (powerless/trapped) — locked into systematic fee extraction with minimal alternatives; information asymmetry prevents exit
 *   - Productive Enterprises: Primary victim (moderate/constrained) — starved of long-term capital; forced to seek private equity or bank financing at higher costs
 *   - Long-Horizon Institutional Investors (Pension Funds, Endowments): Mixed victim-beneficiary (powerful/constrained to mobile) — benefit from coordination mechanisms but trapped in short-term benchmarking; can theoretically exit but fiduciary obligations and identity fusion prevent it
 *   - Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains performative theater; enforcement is reactive and underfunded
 *   - Analytical Observer: Risks naturalizing contingent structure as inherent capital market law (civilizational/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(long_term_capital_markets_structure, 0.58).
domain_priors:suppression_score(long_term_capital_markets_structure, 0.52).
domain_priors:theater_ratio(long_term_capital_markets_structure, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(long_term_capital_markets_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(long_term_capital_markets_structure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(long_term_capital_markets_structure, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(long_term_capital_markets_structure, tangled_rope).
narrative_ontology:human_readable(long_term_capital_markets_structure, "Long-Term Capital Markets Structure").
narrative_ontology:topic_domain(long_term_capital_markets_structure, "financial_systems/capital_allocation").

domain_priors:requires_active_enforcement(long_term_capital_markets_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(long_term_capital_markets_structure, asset_managers_with_scale).
narrative_ontology:constraint_beneficiary(long_term_capital_markets_structure, institutional_investors_with_long_horizons).
narrative_ontology:constraint_beneficiary(long_term_capital_markets_structure, index_providers).
narrative_ontology:constraint_victim(long_term_capital_markets_structure, retail_investors).
narrative_ontology:constraint_victim(long_term_capital_markets_structure, long_term_capital_formation).
narrative_ontology:constraint_victim(long_term_capital_markets_structure, productive_enterprise_investment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in systematic fee extraction disguised as market access. Cannot exit without abandoning wealth-building strategy; faces information asymmetry, algorithmic disadvantage, and structural cost barriers. Bears extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRODUCTIVE ENTREPRENEUR (SNARE) — Cannot access long-term capital at reasonable cost; forced to seek private equity, family offices, or shorter-term bank financing. Capital structure systematically favors financial extraction over productive investment. High suppression through cost barriers and limited alternatives.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LONG-HORIZON INSTITUTIONAL INVESTOR (TANGLED ROPE) — Pension funds and endowments benefit from coordination mechanisms (index access, liquidity provision, price discovery) but also experience embedded extraction through fee layering, principal-agent conflicts, and benchmarking against short-term baselines. Can exit but constrained by fiduciary obligations and benchmark lock-in.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ASSET MANAGER WITH SCALE (ROPE) — Experiences constraint as pure coordination: benefits from standardized indices, trading infrastructure, data access, and regulatory clarity. Net beneficiary of the structure; low extraction experienced because this agent captures the coordination rents.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Securities regulation (SEC, FINRA rules, Dodd-Frank) designed to protect retail investors and ensure market integrity now functions largely as theater: compliance rituals (prospectuses, disclosure documents) have decoupled from actual investor protection. Regulatory enforcement is reactive and underfunded. Theater persists through institutional inertia and regulatory capture.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INDEX PROVIDER (TANGLED ROPE) — Benefits from network effects and benchmarking lock-in (coordination function: standardized measuring sticks enable global capital allocation). Also extracts through index methodology opacity, passive fee layering, and vendor lock-in. Active enforcement required to maintain index methodology authority. Can exit is overstated — competitors are trapped in the same benchmark structure.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT — This perspective naturalizes the structure as inherent to capital markets (principal-agent problems, information asymmetry, agency costs are 'natural'). The analytical observer risks classification as mountain, but the structural data contradicts this: the extractive mechanisms are contingent on regulatory choices, fee structures, and benchmarking conventions — not physical/logical limits.
constraint_indexing:constraint_classification(long_term_capital_markets_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(long_term_capital_markets_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(long_term_capital_markets_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(long_term_capital_markets_structure, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(long_term_capital_markets_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(long_term_capital_markets_structure, TR),
    TR >= 0.70.

:- end_tests(long_term_capital_markets_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The value reflects systematic fee extraction, benchmarking lock-in, and capital starvation for long-term productive investment. The measurement trajectory shows extractiveness growing from 0.32 (1990s) to 0.58 (present) as index provider consolidation, fee layering, and passive fund dominance increased. This is not measurement error — extractiveness genuinely increased as the structure evolved from coordination (low-cost index tracking) toward extraction (monopolistic index provider control + passive fee layering). Suppression (0.52): Moderate. Barriers to exit include regulatory complexity, switching costs, information asymmetry, and institutional identity lock (pension funds trapped in benchmark frameworks). Suppression is not total because sophisticated institutional investors can negotiate lower fees and individuals can access low-cost index funds — but the effort required and path-dependency is high. Theater ratio (0.61): Moderate-high. Significant performative content exists in regulatory compliance (SEC disclosure rules that don't reduce information asymmetry), index methodology opacity masquerading as transparency, and fiduciary language that doesn't prevent benchmarking capture.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the core perspectival gap between powerless victims (retail investors, entrepreneurs) who see pure extraction (snare) and institutional beneficiaries (asset managers, index providers) who see pure coordination (rope). The analytical observer risks the false summit — naturalizing capital market extraction as a natural law of finance (agency costs, information asymmetry, principal-agent problems). But the structural data contradicts this: fee structures, index methodologies, and benchmarking conventions are contingent regulatory choices, not physics. The long-horizon institutional investor's tangled rope perspective is diagnostically crucial: they have the power and information to see the extraction mechanism but are trapped in it through fiduciary obligations and institutional identity fusion with benchmarking. The piton perspective on regulatory frameworks reveals that the entire enforcement apparatus has degraded into theater — prospectuses don't reduce information asymmetry, fiduciary rules don't prevent benchmarking capture, and disclosure requirements don't improve long-term capital allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality reflects the agent's position in the capital flow and their ability to exit. Asset managers with scale are beneficiaries with arbitrage options (d ≈ 0.08) — low d, negative f(d), they experience negative effective extraction (coordination rent). Long-horizon institutional investors are constrained beneficiaries of coordination but victims of extraction (d ≈ 0.55) — moderate d, moderate f(d). Retail investors are trapped victims (d ≈ 0.92) — high d, high f(d), maximum experienced extraction. Index providers occupy an ambiguous position: beneficiaries of their methodologies, constrained by competitive pressure and regulatory scrutiny, so d ≈ 0.45. Productive enterprises are victims facing constrained exit (capital shortages force them to accept extraction through high cost of capital), so d ≈ 0.80. The regulatory framework appears as arbitrage-privileged (d ≈ 0.10) because it maintains the extraction structure without being directly extracted from — but the piton classification reveals this is performative, not functional.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is not misclassified as pure coordination (rope) despite having genuine coordination functions (price discovery, liquidity provision, risk transfer). The tangled_rope classification captures both: beneficiaries experience genuine coordination benefits (index access, trading infrastructure) alongside asymmetric extraction (fee layering, benchmarking lock-in, principal-agent conflicts). The measurement trajectory shows extractiveness rising from 0.32 to 0.58 — this is not classification drift but documentation of how the structure evolved from coordination-dominant (1990s indexing revolution) toward extraction-dominant (2010s onward as passive dominance created index provider monopoly). The mandatrophy resolution prevents the false summit where analysts might claim 'capital markets are naturally extractive' or 'indexing inevitably creates monopoly rents' — these are contingent features, not laws of nature. The regulatory framework piton reveals that the institutional apparatus designed to mitigate these problems has degraded into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    passive_vs_active_extraction,
    'Is the extraction driven by fees and intermediation (readily measurable) or by passive underweighting of long-term productive capital (harder to measure)?',
    'Comparative analysis of capital flow data: percentage of market capitalization flowing to productive enterprise vs financial assets; correlation between market capitalization growth and productive investment growth over 30-year period',
    'If passive underweighting dominates: the constraint is more severe than fee data suggests; productiveness victims experience extraction not just through costs but through capital starvation. If fees dominate: extraction is more transparent and easier to arbitrage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(passive_vs_active_extraction, empirical, 'Whether extraction is driven by transparent fees or passive capital allocation bias').

omega_variable(
    benchmark_lock_in_reversibility,
    'Are pension funds and endowments locked into benchmarking against short-term indices through fiduciary obligation or through identity fusion with index-tracking (institutional identity)?',
    'Policy analysis of fiduciary duty statements; interviews with pension fund trustees on whether they experience benchmark constraints as legal requirements or as cultural norms; analysis of constraints imposed by index methodology changes',
    'If legal lock-in: exit_options: constrained is accurate; regulatory reform could enable long-term benchmarking. If identity fusion: exit_options should shift toward identity_locked; institutional identity fused with index tracking prevents alternatives even when legally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_lock_in_reversibility, conceptual, 'Whether benchmark lock-in is fiduciary obligation or institutional identity').

omega_variable(
    index_provider_monopoly_vs_coordination,
    'Do the three major index providers (S&P Global, MSCI, Bloomberg) coordinate capital allocation through their methodologies, or do they extract monopoly rents while appearing to coordinate?',
    'Network analysis of index methodology changes and correlation with asset flows; counterfactual analysis of what would happen if index methodologies became standardized and transparent; competitive analysis of index provider switching costs',
    'If genuine coordination: index providers are net positive for market function despite fees; constraint is mixed rope/tangled_rope. If monopoly extraction: index providers are pure rent-seekers; constraint slopes toward snare for all non-provider agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_provider_monopoly_vs_coordination, empirical, 'Whether index providers coordinate or extract monopoly rents').

omega_variable(
    long_term_capital_starvation_causation,
    'Is the shortage of long-term capital for productive enterprise caused by the market structure (extraction mechanism) or by real investor time-preference (people want liquid short-term returns)?',
    'Historical comparison of capital structure during periods of less extractive intermediation (e.g., bank-loan based financing pre-1990s); analysis of pension fund asset allocation constraints imposed vs chosen; behavioral economics research on revealed vs stated preferences for long-term returns',
    'If structure: the constraint enables reallocation by changing fee mechanisms and index methodologies. If preference: structural change will not solve the problem; long-term capital will remain scarce regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_capital_starvation_causation, empirical, 'Whether capital starvation is structural or preference-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(long_term_capital_markets_structure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ltcms_tr_t0, long_term_capital_markets_structure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ltcms_tr_t10, long_term_capital_markets_structure, theater_ratio, 10, 0.5).
narrative_ontology:measurement(ltcms_tr_t20, long_term_capital_markets_structure, theater_ratio, 20, 0.61).
narrative_ontology:measurement(ltcms_tr_t30, long_term_capital_markets_structure, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(ltcms_be_t0, long_term_capital_markets_structure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ltcms_be_t10, long_term_capital_markets_structure, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ltcms_be_t20, long_term_capital_markets_structure, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ltcms_be_t30, long_term_capital_markets_structure, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(long_term_capital_markets_structure, resource_allocation).
narrative_ontology:boltzmann_floor_override(long_term_capital_markets_structure, 0.18).
narrative_ontology:affects_constraint(long_term_capital_markets_structure, pension_fund_benchmark_lock_in).
narrative_ontology:affects_constraint(long_term_capital_markets_structure, index_provider_monopoly).
narrative_ontology:affects_constraint(long_term_capital_markets_structure, fee_extraction_mechanisms).
narrative_ontology:affects_constraint(long_term_capital_markets_structure, productive_capital_starvation).

% DUAL FORMULATION NOTE:
% Long-term capital markets structure is decomposed into four constraint families: (1) index provider monopoly (ε=0.65, snare for non-providers), (2) pension fund benchmarking lock (ε=0.48, tangled rope with identity_locked component), (3) fee extraction mechanisms (ε=0.72, snare for retail), (4) productive capital starvation (ε=0.55, snare for entrepreneurs). These stories are linked because index provider lock-in constrains pension fund alternatives, which reduces capital for productive investment, which drives fees higher for remaining retail investors. The umbrella constraint story (long_term_capital_markets_structure) represents the systemic effect; the decomposed stories show structural entry points for intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(long_term_capital_markets_structure, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
