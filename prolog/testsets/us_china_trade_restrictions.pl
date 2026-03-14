% ============================================================================
% CONSTRAINT STORY: us_china_trade_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_trade_restrictions, []).

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
 *   constraint_id: us_china_trade_restrictions
 *   human_readable: US-China Trade Restrictions Framework
 *   domain: international_trade/geopolitics
 *
 * SUMMARY:
 *   US-China trade restrictions represent a hybrid coordination-extraction
 *   constraint that blends genuine strategic supply chain concerns with
 *   asymmetric extraction from consumers and exporters. The tariff regime
 *   emerged from legitimate security analysis (supply chain concentration
 *   risks in critical technologies) but has evolved into a partially
 *   theatrical enforcement mechanism that extracts more from dispersed
 *   consumers and inefficient domestic producers than it captures as
 *   strategic benefits. The constraint demonstrates how geopolitical security
 *   framing can legitimize extraction when the beneficiary group (domestic
 *   manufacturers, strategic industries) exerts concentrated political power
 *   while costs are dispersed across consumers and foreign exporters who lack
 *   domestic political voice. The theater ratio has increased over time as
 *   exclusion request processes have become bureaucratically opaque and
 *   economic rationales have diverged from actual benefit-cost distributions.
 *
 * KEY AGENTS:
 *   - US Consumers: Primary victim (powerless/trapped) — bear elevated prices across electronics, apparel, household goods with no exit option
 *   - Chinese Exporters: Primary victim (powerless/trapped) — face sudden market access loss with capital sunk in US supply relationships
 *   - US Domestic Manufacturing: Primary beneficiary (institutional/arbitrage) — receive tariff protection and price floors; lobby for continued restrictions
 *   - US Strategic Industries: Primary beneficiary (institutional/arbitrage) — semiconductors, rare earths, defense-adjacent sectors gain supply security framing
 *   - Global Supply Chain Actors: Secondary victim (moderate/constrained) — forced to restructure supply chains; face costs of relocation but some can arbitrage new allied supplier positioning
 *   - Multinational Corporations: Mixed position (organized/constrained) — simultaneously benefit from supply chain reorganization opportunities and bear costs of forced restructuring
 *   - Allied Trade Coalition: Organized beneficiary (organized/constrained) — India, Vietnam, Mexico, Taiwan gain manufacturing investment and export opportunity as substitutes for China
 *   - US Trade Administration: Institutional enforcer (institutional/arbitrage) — maintains tariff regime through Section 301 determinations and exclusion request theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_trade_restrictions, 0.58).
domain_priors:suppression_score(us_china_trade_restrictions, 0.65).
domain_priors:theater_ratio(us_china_trade_restrictions, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_trade_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_trade_restrictions, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_china_trade_restrictions, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_trade_restrictions, tangled_rope).
narrative_ontology:human_readable(us_china_trade_restrictions, "US-China Trade Restrictions Framework").
narrative_ontology:topic_domain(us_china_trade_restrictions, "international_trade/geopolitics").

domain_priors:requires_active_enforcement(us_china_trade_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_trade_restrictions, us_domestic_manufacturing).
narrative_ontology:constraint_beneficiary(us_china_trade_restrictions, us_strategic_industries).
narrative_ontology:constraint_beneficiary(us_china_trade_restrictions, us_government_revenue).
narrative_ontology:constraint_victim(us_china_trade_restrictions, us_consumers).
narrative_ontology:constraint_victim(us_china_trade_restrictions, chinese_exporters).
narrative_ontology:constraint_victim(us_china_trade_restrictions, global_supply_chains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: US CONSUMER (SNARE) — Trapped within domestic market with limited alternatives for low-cost goods. Bears full cost through elevated prices on electronics, clothing, household goods. No exit option: tariffs apply uniformly across supply chains. Maximum experienced extraction without benefit.
constraint_indexing:constraint_classification(us_china_trade_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHINESE EXPORTER (SNARE) — Market access suddenly restricted through tariff walls and supply chain decoupling. Cannot exit the US market without absorbing losses; cannot relocate supply chains instantly. Suppression through regulatory enforcement and trade agreement provisions. Extraction of market share through tariff mechanics.
constraint_indexing:constraint_classification(us_china_trade_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL SUPPLY CHAIN ACTOR (TANGLED ROPE) — Genuine coordination function: supply chain transparency, supplier verification, nearshoring logistics benefit actors who reposition. But also extraction: forced relocation costs, restructuring capital expenses, forced abandonment of efficient Chinese suppliers. Constrained exit: repositioning is possible but capital-intensive and carries geopolitical risk.
constraint_indexing:constraint_classification(us_china_trade_restrictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US DOMESTIC MANUFACTURING (ROPE) — Primary beneficiary. Tariffs protect domestic producers and create price floors that enable profitability. No suppression experienced; full agency through lobbying influence. Coordination benefit: tariff walls coordinate demand toward domestic suppliers. Net extraction runs toward this agent.
constraint_indexing:constraint_classification(us_china_trade_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: US STRATEGIC INDUSTRIES (ROPE) — Semiconductors, rare earth processing, defense-adjacent manufacturing benefit from tariff protection framed as national security. Arbitrage exit: these industries can lobby for exemptions and carve-outs. Experience the constraint as coordination (securing supply chains against geopolitical disruption). Net beneficiary.
constraint_indexing:constraint_classification(us_china_trade_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MULTINATIONAL CORPORATIONS (TANGLED ROPE) — Complex structural position. Genuine coordination benefit: tariffs enable 'friend-shoring' to allied nations, reducing geopolitical supply chain risk. But also extraction: forced capital expenditure on supply chain restructuring, reduced manufacturing flexibility, compliance overhead. Constrained exit: cannot ignore tariff regime but can lobby for carve-outs and negotiate bilateral deals.
constraint_indexing:constraint_classification(us_china_trade_restrictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ALLIED TRADE COALITION (SCAFFOLD) — US and allied nations building alternative supply ecosystems (India, Vietnam, Mexico, Taiwan). Organized agents (trade ministries, multinational logistics) see restrictions as temporary forcing function for supply chain regionalization. Sunset logic: as allied production capacity scales, dependence on tariff protection declines. Extract experienced as transitional coordination cost with defined exit path through manufacturing capacity building in allied nations.
constraint_indexing:constraint_classification(us_china_trade_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: US TRADE ADMINISTRATION (PITON) — Maintains tariff enforcement ritual through Section 301 determinations, exclusion requests, and annual review theater. Functionally degraded: the stated purpose (protecting US manufacturing) increasingly conflicts with actual distribution (tariffs shield inefficient producers, damage exporters more than they protect manufacturers). Persists through institutional inertia and bureaucratic self-preservation. Theater ratio elevated by exclusion request process (thousands of applications, opaque criteria).
constraint_indexing:constraint_classification(us_china_trade_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — Structural analysis reveals genuine coordination function (supply chain security, strategic industry protection, geopolitical decoupling from adversarial production) alongside asymmetric extraction (consumer price elevation, exporter market loss, supply chain restructuring costs). Effective extraction chi ≈ 0.58 reflects mixed coordination-extraction hybrid. The constraint is not a disguised snare nor a pure rope — it is authentically tangled.
constraint_indexing:constraint_classification(us_china_trade_restrictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_trade_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_trade_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_trade_restrictions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_trade_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_trade_restrictions, TR),
    TR >= 0.70.

:- end_tests(us_china_trade_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime extracts through tariff pass-through to consumers, market denial to Chinese exporters, and restructuring costs on supply chains. However, extraction is not maximal (would be 0.80+) because: (1) genuine strategic supply chain risks justify some coordination cost, (2) allied nations absorb some production, preventing complete consumer extraction, and (3) some inefficient domestic producers are replaced through automation rather than labor hiring, reducing actual manufacturing benefits. Suppression (0.65): Moderate-high. Both consumers and exporters face significant barriers: tariffs are enforced through legal machinery, retaliation is costly, alternatives are limited. Chinese exporters cannot exit easily (sunk capital, customer relationships), and US consumers cannot access alternative markets (no circumvention possible at scale). Theater ratio (0.55): Moderate. Exclusion request process involves thousands of applications with opaque criteria and political dynamics; Section 301 determinations include strategic justifications that are often overstated relative to actual risk assessments. Theater increased from 0.35 to 0.55 over the interval as exclusions became increasingly politicized.
 *
 * PERSPECTIVAL GAP:
 *   The clearest gap appears between powerless consumers (Snare) and institutional manufacturers (Rope): same constraint, opposite experiences. Consumers see immutable extraction with no alternatives; manufacturers see coordination with beneficial outcomes. The analytical observer recognizes both as true from their respective structural positions — the gap is not a measurement error but a structural property of how tariffs distribute costs and benefits. The mandate dissolution appears most sharply when comparing what each perspective claims the constraint IS: consumers claim it is a transfer mechanism (extraction); manufacturers claim it is security coordination (rope). Both descriptions are partially accurate — the Tangled Rope classification unites them by saying the constraint IS both, with the distribution of coordination vs. extraction benefits heavily skewed toward the beneficiary group.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural position of each actor relative to extraction flow. US consumers: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42. Chinese exporters: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42. US domestic manufacturers: beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12. Strategic industries: beneficiary status + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.05. Global supply chain actors: mixed (victim of forced restructuring + beneficiary of new positioning) + constrained exit → d ≈ 0.50 → f(d) ≈ 0.65. Multinational corporations: similar mixed position + constrained exit → d ≈ 0.48 → f(d) ≈ 0.58. The scope modifier σ(S) = 1.0 for national tariff scope, 1.2 for global supply chain impacts. This produces chi scaling that shows consumers experience higher effective extraction (high d, high f(d), scope amplification) than domestic manufacturers (low d, negative f(d), scope dampening).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint avoids mandatrophy by acknowledging that it genuinely coordinates supply chain security (rope function) while also genuinely extracting from consumers (snare function). The analytical classification as Tangled Rope is correct; the false summit would be claiming it is ONLY a security coordination mechanism (which would be Rope from all perspectives). The mandate dissolution appears in the direction of analysis: from the beneficiary's perspective, security coordination dominates and extraction is justified cost; from the victim's perspective, extraction dominates and security coordination is justified framing. The constraint is authentically hybrid — both perspectives see real structure. Mandatrophy is resolved by recognizing that the Tangled Rope classification correctly captures this dual nature and that perspectival disagreement about whether it is 'really' coordination or 'really' extraction is not a logical error but a reflection of different structural positions in how the constraint operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_chain_security_threshold,
    'What degree of supply chain concentration in China constitutes genuine strategic vulnerability vs. economic efficiency rationalization?',
    'Cross-sector vulnerability mapping: identify which industries face genuine single-source risks vs. those where China dominance is cost-optimization. Compare actual production losses during geopolitical tensions to predicted losses based on supply chain modeling.',
    'If vulnerability threshold is high: restrictions are justified coordination (Rope dominant). If threshold is low: restrictions are disguised extraction (Snare dominant). If mixed: Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_security_threshold, empirical, 'Threshold for genuine supply chain strategic vulnerability').

omega_variable(
    tariff_incidence_distribution,
    'Do tariffs actually protect domestic manufacturing employment or do they primarily extract through consumer price increases and capital reallocation?',
    'Labor market analysis: track employment in protected industries vs. price impact on consumption; compare wage gains in protected sectors to price increases experienced by lower-income households; measure capital flight to automated production vs. labor hiring.',
    'If manufacturing employment gains exceed consumer welfare loss: coordination framing valid. If consumer costs exceed manufacturing benefits: extraction dominant. If mixed: distribution determines whether constraint is rope or snare from different power perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tariff_incidence_distribution, empirical, 'Distribution of tariff incidence between manufacturers and consumers').

omega_variable(
    substitution_capacity_timeline,
    'What is the realistic timeline for allied nations (Vietnam, India, Mexico, Taiwan) to absorb Chinese export capacity at similar cost/quality?',
    'Production capacity buildout analysis: track capital investment in allied manufacturing; compare wage levels, infrastructure, and technical capacity; measure actual substitution rates in electronics, rare earths, consumer goods.',
    'If substitution is feasible within 10 years: scaffold sunset logic is credible. If substitution requires 25+ years: restrictions become permanent extraction regime. Affects whether Scaffold or Snare dominates long-term perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_capacity_timeline, empirical, 'Timeline for allied production capacity substitution').

omega_variable(
    extraction_vs_coordination_dominance,
    'Does the tariff regime coordinate legitimate security concerns or does it primarily extract value from dispersed consumers to concentrated beneficiaries?',
    'Benefit-cost aggregation: sum consumer price increases, exporter losses, supply chain restructuring costs; compare to manufacturing employment gains and strategic capacity improvements. Decompose by income level: does extraction fall primarily on lower-income households?',
    'If coordination benefits exceed extraction costs: classification trend toward Rope. If extraction costs exceed benefits: trend toward Snare. If genuinely mixed: Tangled Rope confirmed. Income-regressivity analysis determines whether lower-power perspectives experience snare vs. higher-power perspectives experience rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_dominance, empirical, 'Aggregate dominance of coordination vs. extraction mechanisms').

omega_variable(
    reciprocal_retaliation_lock,
    'Do Chinese retaliatory tariffs on US agriculture and exports create a mutual suppression lock that prevents both sides from dismantling the regime?',
    'Historical analysis of tariff negotiation attempts; map retaliatory escalation cycles; identify whether domestic political support for continuance derives from genuine strategic conviction or from sunk-cost escalation and inter-group retaliation commitment.',
    'If reciprocal retaliation dominates: both sides trapped in Snare-like dynamics despite mutually harmful extraction. If strategic coordination dominates: Tangled Rope persists. If pure escalation spiral: classification shifts toward Snare with suppression through domestic political commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_retaliation_lock, empirical, 'Whether reciprocal retaliation creates locked suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_trade_restrictions, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_china_trade_restrictions, theater_ratio, 0, 0.35).
narrative_ontology:measurement(us_c_tr_t2, us_china_trade_restrictions, theater_ratio, 2, 0.48).
narrative_ontology:measurement(us_c_tr_t4, us_china_trade_restrictions, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_china_trade_restrictions, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(us_c_be_t2, us_china_trade_restrictions, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(us_c_be_t4, us_china_trade_restrictions, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_trade_restrictions, resource_allocation).
narrative_ontology:affects_constraint(us_china_trade_restrictions, rare_earth_supply_concentration).
narrative_ontology:affects_constraint(us_china_trade_restrictions, semiconductor_supply_chain_dependence).
narrative_ontology:affects_constraint(us_china_trade_restrictions, agricultural_export_retaliation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_trade_restrictions, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
