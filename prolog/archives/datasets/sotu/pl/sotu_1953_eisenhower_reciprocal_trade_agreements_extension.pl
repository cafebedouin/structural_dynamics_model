% ============================================================================
% CONSTRAINT STORY: sotu_1953_eisenhower_reciprocal_trade_agreements_extension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1953_eisenhower_reciprocal_trade_agreements_extension, []).

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
 *   constraint_id: sotu_1953_eisenhower_reciprocal_trade_agreements_extension
 *   human_readable: Eisenhower Reciprocal Trade Agreements Extension with Domestic Safeguards (1953)
 *   domain: trade_policy/customs_regulation
 *
 * SUMMARY:
 *   The 1953 Eisenhower reciprocal trade extension represents a canonical
 *   case of hybrid coordination-extraction: simultaneous reduction of
 *   procedural obstacles to trade and preservation of sectoral safeguards.
 *   The constraint balances genuinely beneficial trade expansion (export
 *   access, consumer pricing, productivity gains) against domestic protective
 *   mechanisms that preserve sectoral incumbent positions. Theater increases
 *   over the measurement interval as procedural thresholds for escape clause
 *   invocation become de facto prohibitive, transforming nominal safeguards
 *   into symbolic concessions to protectionist constituencies rather than
 *   functional relief mechanisms. The constraint exhibits all six
 *   classification types across different stakeholder perspectives, making it
 *   a diagnostic exemplar of how institutional design encodes extraction even
 *   within ostensibly neutral market-enabling frameworks.
 *
 * KEY AGENTS:
 *   - Export-Oriented Manufacturers: Primary beneficiary (institutional/arbitrage) — capture expanded foreign market access and competitive rents during adjustment period
 *   - Import-Competing Domestic Manufacturers: Primary victim (powerless/trapped) — bear displacement costs despite nominal safeguards; procedural obstacles prevent relief invocation
 *   - Protected Agricultural Sectors: Secondary beneficiary (powerful/constrained) — retain explicit statutory exemptions and escape clause access, but face pressure to negotiate away protections in exchange for industrial export market access
 *   - Labor Movement (Organized): Mixed agent (organized/constrained) — simultaneously dependent on manufacturing export markets (union jobs) and threatened by import competition (wage suppression through capital mobility threat)
 *   - Consumer Interests: Beneficiary (moderate/mobile) — benefit from tariff reduction through lower import prices and competitive pressure on domestic producers
 *   - Trade Administration Bureaucracy: Institutional actor (institutional/arbitrage) — administers nominally protective escape clause with increasingly prohibitive procedural requirements; maintains theater through symbolic safeguard authority
 *   - International Trade Institutions: Scaffolding agent (organized/constrained) — GATT framework represents temporary post-war coordination architecture with embedded sunset (successive rounds reduce tariff flexibility)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, 0.58).
domain_priors:suppression_score(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, 0.48).
domain_priors:theater_ratio(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, tangled_rope).
narrative_ontology:human_readable(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, "Eisenhower Reciprocal Trade Agreements Extension with Domestic Safeguards (1953)").
narrative_ontology:topic_domain(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, "trade_policy/customs_regulation").

domain_priors:requires_active_enforcement(sotu_1953_eisenhower_reciprocal_trade_agreements_extension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, export_oriented_manufacturers).
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, consumer_interests).
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, agricultural_exporters).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, import_competing_domestic_manufacturers).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, protected_agricultural_sectors).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, labor_standards_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC IMPORT-COMPETING MANUFACTURERS (SNARE) — Trapped within tariff schedules negotiated at multilateral tables where their interests are subordinated to export sector priorities. Nominal safeguards exist but procedural obstacles delay invocation; by the time relief is granted, market share is lost. Maximum extraction: forced to compete without reciprocal protections while export competitors receive subsidies and preferential access.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROTECTED AGRICULTURAL SECTORS (TANGLED ROPE) — Retain explicit statutory exemptions (sugar, dairy, wool) and escape clause mechanisms, but safeguards are constrained by reciprocity obligations and political pressure to negotiate away protections. Mixed: genuine sectoral protection alongside extraction through forced tariff concessions in exchange for market access elsewhere.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXPORT-ORIENTED MANUFACTURERS (ROPE) — Primary beneficiaries. Reciprocal trade authority expands foreign market access, enabling scale economies and profit growth. Experiences the constraint as pure coordination: tariff reduction is mutual removal of barriers to mutually beneficial exchange. No suppression perceived — exit is costless (can shift to protected sectors if needed). Low effective extraction.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER INTERESTS (ROPE) — Benefit from tariff reduction through lower import prices and increased competitive pressure on domestic producers. Exit options exist (substitute goods, international purchasing). The constraint coordinates price discovery and prevents monopoly pricing by domestic producers. Perceived as efficiency gain, not extraction.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR STANDARDS ADVOCATES (TANGLED ROPE) — Trade expansion creates both coordination benefits (larger markets, higher productivity justify wage growth) and extraction (downward wage pressure from low-cost competition, suppression of labor organizing through capital mobility threat). Safeguards are nominal — labor provisions in trade agreements are advisory, not enforceable. Constrained by dependence on manufactured goods employment and inability to exit labor market.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADE ADMINISTRATION BUREAUCRACY (PITON) — Administers escape clauses and safeguard mechanisms that are increasingly theatrical. Criteria for invocation (serious injury, TA relief qualification) are intentionally high, creating procedural bottlenecks that limit relief availability. The escape clause survives through institutional inertia and symbolic concession to protectionists, not through functional verification. Theater ratio tracks the gap between nominal safeguard authority and actual relief granted.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ORGANIZED LABOR (TANGLED ROPE) — Simultaneously supports trade expansion (benefits from export markets for manufactured goods, wage floors from productivity gains) and opposes it (displacement from import competition, suppressed negotiating power through capital mobility threat). Constrained by dependence on union membership dues (manufacturing), making sectoral withdrawal impossible. Extracts organizational resources into political advocacy with uncertain payoff.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: INTERNATIONAL TRADE INSTITUTION BUILDING (SCAFFOLD) — The reciprocal trade framework is temporary architecture for post-war economic coordination, with sunset embedded in its design: GATT (1947) rounds are conceived as convergence toward ultimate tariff elimination. Theater is high (reciprocity principle, most-favored-nation clauses create procedural complexity), but the institutional direction is clear. Safeguards degrade over successive negotiation rounds as tariff bindings reduce flexibility. Extraction mechanisms weaken as institutions mature and enforcement capacity grows.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / COMPARATIVE ADVANTAGE VIEW (MOUNTAIN) — From a civilizational analytical perspective, trade expansion toward comparative advantage equilibrium is an immutable economic law. Domestic safeguards cannot prevent long-run adjustment without collapsing gains from trade entirely. The constraint appears as natural law: protection costs more than it saves. However, the structural data reveals this as a false summit — the safeguards are designed by powerful actors (protected sectors, capital interests) to reduce extraction on themselves while maintaining extraction on unprotected sectors and labor.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1953_eisenhower_reciprocal_trade_agreements_extension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, TR),
    TR >= 0.70.

:- end_tests(sotu_1953_eisenhower_reciprocal_trade_agreements_extension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits significant extraction relative to stated coordination goals. Reciprocal tariff reduction benefits export-oriented manufacturers and consumers while displacing import-competing manufacturers. The key observation is asymmetry: nominal reciprocity in tariff reduction masks unequal capacity to absorb adjustment costs. Export manufacturers can reallocate capital and labor to new markets; import-competing manufacturers (concentrated in regions dependent on specific industries) face sunk capital losses and immobile labor. Suppression rises from initial 0.35 to 0.58 as procedural thresholds for escape clause relief prove increasingly prohibitive. Suppression (0.48): Moderate. Barriers to sectoral protection include: (a) reciprocity obligation that ties domestic safeguard expansion to concessions elsewhere, (b) high procedural thresholds (serious injury standard, TA relief qualification), (c) political pressure from export-oriented interests to avoid retaliatory closure of foreign markets, (d) multinational investment integration that blurs domestic manufacturer interests. But suppression is not total — statutory exemptions exist for sensitive sectors (agriculture, sugar, dairy), and escape clause authority persists even if underutilized. Theater ratio (0.55): Moderate-high and rising. The escape clause is increasingly theatrical: its existence provides political cover for trade expansion ("we have safeguards"), but procedural complexity prevents invocation. Procedural requirements escalate (injury must be serious, nexus to trade must be direct, alternative remedies must be exhausted) such that by the end of the measurement interval, escape clause grants are rare relative to injury petitions filed. The rise from 0.35 to 0.55 reflects gap expansion between nominal safeguard authority and actual relief granted.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic extraction camouflage through coordination framing. Export-oriented manufacturers genuinely perceive pure coordination (Rope): reciprocal tariff reduction is mutual removal of barriers to beneficial trade. Import-competing manufacturers perceive pure extraction (Snare): safeguards are nominally available but procedurally inaccessible. Protected agricultural sectors perceive mixed coordination-extraction (Tangled Rope): they retain explicit protections alongside pressure to make concessions. Labor perceives asymmetric extraction masked by apparent coordination (Tangled Rope): productivity gains from trade are unevenly distributed, and downward wage pressure from capital mobility threat cancels nominal labor safeguards. The trade administration perceives its own degradation (Piton): escape clause mechanisms persist through inertia but are increasingly theatrical. The analytical observer risks the false summit: naturalizing adjustment costs as inevitable comparative advantage equilibrium rather than contingent institutional design. The perspectival gap reveals that the constraint's claimed type (Tangled Rope) masks asymmetric extraction on import-competing manufacturers, unprotected labor, and trading partners (if reciprocity is nominal rather than genuine).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's two-level design encodes directionality differentiation: tariff reduction benefits export-oriented manufacturers (d ≈ 0.10, low extraction due to beneficiary + arbitrage exit) while displacing import-competing manufacturers (d ≈ 0.90, high extraction due to victim + trapped exit). Protected agricultural sectors occupy intermediate position: they retain safeguard mechanisms (d ≈ 0.45) but face pressure to negotiate concessions, constraining their exit options. Labor's directionality is complex: organized labor can mobilize politically (d ≈ 0.55 for powerful unions) but faces structural constraints from capital mobility (credible threat to relocate production shifts d upward). The analytical observer's mountain perspective risks naturalizing this asymmetry as inevitable comparative advantage adjustment, but the structural data reveals it as contingent institutional design: different threshold settings for escape clause relief would produce different extraction incidence. Directionality is not derived purely from beneficiary/victim status but from the interaction of status + procedural design + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that 'Tangled Rope' is the analytically correct classification when measured at the national aggregate level, but masks sector-specific extraction that appears as Snare from import-competing manufacturer perspectives and Rope from export-oriented manufacturer perspectives. The mandatrophy is resolved by disaggregating to the sectoral level and acknowledging that different measurement baselines produce different types: (1) at the trade-flow level, the constraint is Rope (reciprocal reduction benefits both exporters and importers through efficiency gains); (2) at the import-competing manufacturer level, it is Snare (procedural obstacles prevent relief despite nominal safeguards); (3) at the national fiscal level, it is Tangled Rope (mixed benefits and costs across constituencies). No single type is 'correct' — the presheaf over sector × policy mechanism is the answer. The false summit detection fires because the analytical mountain ('inevitable comparative advantage') naturalizes what are actually contingent procedural choices (escape clause thresholds, reciprocity obligations, negotiation sequencing). Different institutional design would produce different extraction incidence without changing the fundamental efficiency gains from trade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safeguard_invocation_threshold_ambiguity,
    'Are escape clause thresholds (serious injury standard, TA relief qualification) set at levels that permit genuine relief, or are they intentionally prohibitive theater?',
    'Historical data on escape clause invocations vs. total trade injury petitions; timeline analysis of relief approval vs. market displacement; comparison of criteria stringency across administrations',
    'If accessible: safeguards function as genuine coordination mechanism (Tangled Rope shifts toward Rope for protected sectors). If prohibitive: safeguards are theater masking extraction (Snare for import-competing manufacturers confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safeguard_invocation_threshold_ambiguity, empirical, 'Whether escape clause thresholds permit genuine sectoral relief').

omega_variable(
    extraction_incidence_measurement,
    'Who bears the net extraction: protected sectors that lose tariff revenue, or import-competing manufacturers that lose market share despite safeguards?',
    'Disaggregated trade data by sector; price-series analysis pre/post-reciprocal reduction; employment displacement tracking; profit-margin evolution for protected vs. unprotected manufacturers',
    'If protected sectors bear cost: Tangled Rope classification persists. If unprotected manufacturers bear disproportionate cost: constraint reclassifies toward Snare for powerless agent. Directionality d-values change accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_incidence_measurement, empirical, 'Incidence of extraction between protected and unprotected domestic sectors').

omega_variable(
    reciprocity_asymmetry_depth,
    'Does reciprocal tariff reduction genuinely equalize bargaining power, or do structural asymmetries (industrial capacity, capital scale, labor cost) make nominal reciprocity extractive for smaller/developing trading partners?',
    'Analysis of reciprocal rate schedules by sector and partner; investigation of capital-labor substitution patterns in tariff concessions; longitudinal data on trade balance evolution by partner country',
    'If genuine reciprocity: bilateral trade appears as Rope for all parties. If asymmetric: constraint''s extractiveness persists even as nominal negotiation indicates coordination. Domestic safeguards become tools of asymmetric extraction on trading partners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_asymmetry_depth, empirical, 'Whether nominal reciprocity masks structural bargaining asymmetries').

omega_variable(
    labor_standard_enforcement_capacity,
    'Are labor provisions in trade agreements enforceable mechanism or mere signaling theater?',
    'Comparison of labor clause invocations vs. observed violations; investigation of dispute resolution pathways; analysis of remedies granted (trade sanctions vs. empty condemnation); longitudinal data on union density and real wages in high-trade-exposure sectors',
    'If enforceable: Tangled Rope for labor interests (genuine but incomplete protection). If theatrical: Snare for labor (extraction masked as safeguard). Impacts classification for organized labor and labor standards advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_standard_enforcement_capacity, empirical, 'Whether labor provisions provide enforceable constraint on competition').

omega_variable(
    procedural_bottleneck_intentionality,
    'Are high procedural thresholds for escape clause invocation accidents of bureaucratic design, or deliberate construction by administration to limit relief availability?',
    'Archive analysis of legislative debates, administrative rulemaking, and policy guidance; interviews with trade administration officials; longitudinal comparison of procedural requirements across administrations',
    'If accidental: bureaucratic reform could unblock relief (constraint is Scaffold toward Rope). If deliberate: procedural complexity is intentional extraction mechanism (constraint is Snare disguised as Tangled Rope). Determines whether theater_ratio reflects genuine complexity or intentional obscuration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_bottleneck_intentionality, conceptual, 'Whether procedural complexity in escape clauses is intentional or accidental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eise_tr_t0, sotu_1953_eisenhower_reciprocal_trade_agreements_extension, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eise_tr_t3, sotu_1953_eisenhower_reciprocal_trade_agreements_extension, theater_ratio, 3, 0.48).
narrative_ontology:measurement(eise_tr_t6, sotu_1953_eisenhower_reciprocal_trade_agreements_extension, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(eise_be_t0, sotu_1953_eisenhower_reciprocal_trade_agreements_extension, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eise_be_t3, sotu_1953_eisenhower_reciprocal_trade_agreements_extension, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(eise_be_t6, sotu_1953_eisenhower_reciprocal_trade_agreements_extension, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, resource_allocation).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, gatt_most_favored_nation_scheduling).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, agricultural_import_substitution).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, labor_wage_suppression_trade_exposure).

% DUAL FORMULATION NOTE:
% The reciprocal trade extension is upstream of sectoral protection mechanisms (agricultural exemptions, escape clause invocations, labor safeguards). The trade framework's extractiveness affects the feasibility and cost of downstream protections. Procedural bottlenecks in escape clause invocation create spillover effects on labor organizing (capital mobility threat becomes credible due to ease of import substitution). The constraint family should include separate stories for agricultural safeguards (higher ε due to explicit exemption providing genuine relief) and labor standard enforcement (higher ε due to nominal provisions lacking enforcement capacity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1953_eisenhower_reciprocal_trade_agreements_extension, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
