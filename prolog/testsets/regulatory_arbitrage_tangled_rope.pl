% ============================================================================
% CONSTRAINT STORY: regulatory_arbitrage_tangled_rope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_arbitrage_tangled_rope, []).

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
 *   constraint_id: regulatory_arbitrage_tangled_rope
 *   human_readable: Regulatory Arbitrage as Tangled Rope Coordination-Extraction Hybrid
 *   domain: economic_policy/regulatory_governance
 *
 * SUMMARY:
 *   Regulatory arbitrage is the structural outcome of decentralized
 *   governance meeting mobile capital. Firms locate where regulatory costs
 *   (environmental compliance, labor standards, financial disclosure, tax
 *   obligations) are lowest while maintaining market access to
 *   high-regulation jurisdictions. This creates a tangled hybrid: genuine
 *   coordination (capital finds efficient locations) layered over asymmetric
 *   extraction (high-regulation jurisdictions are forced to compete downward,
 *   immobile competitors face fixed costs while mobile competitors escape
 *   them, and low-regulation jurisdictions are trapped in a race-to-bottom
 *   competitive spiral). The constraint exhibits all six DR types from
 *   different structural positions, revealing how the same institutional
 *   arrangement can appear as pure coordination, pure extraction, or hybrid
 *   depending on the agent's mobility and power. The theater ratio (0.45)
 *   reflects that regulatory compliance is substantively functional — firms
 *   cannot entirely evade regulations, and compliance costs are real — not
 *   purely performative. However, enforcement theater increases as
 *   jurisdictions struggle to tax and regulate increasingly footloose
 *   capital, leading to theater_ratio growth over the interval.
 *
 * KEY AGENTS:
 *   - Mobile Multinational Firms: Primary beneficiary (institutional/arbitrage) — capture cost advantages by locating across jurisdictions; full exit capacity enables optimization
 *   - High-Regulation Jurisdiction: Primary victim (powerless/trapped) — forced to compete downward by capital exit threat; cannot coordinate without multilateral commitment
 *   - Low-Regulation Jurisdiction: Secondary beneficiary and victim (institutional/constrained) — gains inbound capital but trapped in competitive deregulation cycle; cannot unilaterally raise standards
 *   - Domestic SME: Secondary victim (moderate/constrained) — lacks mobility to arbitrage; bears fixed compliance costs; benefits from regulatory stability but disadvantaged by inability to relocate
 *   - Regulatory Fragmentation Costs: Collective victim (powerless/trapped) — abstract cost of maintaining multiple regulatory regimes; creates redundant compliance infrastructure
 *   - Regulatory Harmonization Coalition: Organized agents (OECD, Basel Committee, BEPS initiatives) — pushing toward international standards with sunset logic; have institutional power but face firm resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_arbitrage_tangled_rope, 0.58).
domain_priors:suppression_score(regulatory_arbitrage_tangled_rope, 0.62).
domain_priors:theater_ratio(regulatory_arbitrage_tangled_rope, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_arbitrage_tangled_rope, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_arbitrage_tangled_rope, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_arbitrage_tangled_rope, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_arbitrage_tangled_rope, tangled_rope).
narrative_ontology:human_readable(regulatory_arbitrage_tangled_rope, "Regulatory Arbitrage as Tangled Rope Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(regulatory_arbitrage_tangled_rope, "economic_policy/regulatory_governance").

domain_priors:requires_active_enforcement(regulatory_arbitrage_tangled_rope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_tangled_rope, mobile_multinational_firms).
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_tangled_rope, low_regulation_jurisdictions).
narrative_ontology:constraint_victim(regulatory_arbitrage_tangled_rope, high_regulation_jurisdictions).
narrative_ontology:constraint_victim(regulatory_arbitrage_tangled_rope, immobile_domestic_competitors).
narrative_ontology:constraint_victim(regulatory_arbitrage_tangled_rope, regulatory_fragmentation_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-REGULATION JURISDICTION (SNARE) — Trapped by capital mobility: firms relocate if regulations become too onerous. Bears full cost of regulatory fragmentation without ability to exit or coordinate with competing jurisdictions. Experiences the constraint as pure extraction: must lower standards or lose tax base and employment, despite citizens preferring higher protections. Maximum suppression — material barriers (capital flight) prevent exit.
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MOBILE MULTINATIONAL FIRM (ROPE) — Experiences the constraint as coordination: can locate production where regulatory-cost-adjusted economics optimize. Arbitrage options provide full exit capacity. The regulatory mosaic solves a real firm problem (comparative advantage in risk management costs). Net beneficiary — extraction runs toward this agent, coordination serves their interests.
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMESTIC SME (TANGLED ROPE) — Constrained by lack of capital mobility (cannot easily relocate or establish offshore subsidiaries). Experiences both coordination benefit (operates in stable regulatory environment) and extraction (cannot arbitrage to lower-cost jurisdictions; bears fixed compliance costs). Moderate power — can organize with peers but lacks multinational reach. Genuine hybrid: both needs the regulatory framework AND is disadvantaged by its international rigidity.
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY HARMONIZATION COALITION (SCAFFOLD) — Organized actors (OECD, Basel Committee, ISO working groups, BEPS initiatives) see regulatory arbitrage as a temporary coordination failure with a sunset: international standards-setting, minimum-tax agreements (like the 2023 global minimum corporate tax), and regulatory mutual recognition are building pathways toward convergence. High suppression initially (firms resist harmonization) but declining as standards mature. Sunset logic: as harmonization increases, arbitrage opportunities vanish and extraction mechanism loses force. Estimated timeline: 10-20 years for major domains (taxation, environmental, labor).
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REGULATORY FRAMEWORK (PITON) — Within-jurisdiction regulatory boundaries (national labor law, environmental standards, financial regulation) persist through institutional inertia despite erosion by capital mobility. The framework maintains performative compliance (firms file reports, jurisdictions collect fees) but functional coordination has degraded — enforcement capacity is theater when firms can exit. Theater ratio reflects the gap between regulatory ambition and effective enforcement. Maintained because replacing the framework requires international coordination that hasn't yet achieved critical mass.
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: LOW-REGULATION JURISDICTION (TANGLED ROPE) — Benefits from inbound capital and tax revenue (beneficiary role) but constrained by competitive pressure from other low-regulation jurisdictions (race to the bottom). Experiences both coordination (capital mobility coordinates location decisions) and extraction (trapped in competitive deregulation cycle — cannot raise standards without losing investment). Institutional power but constrained exit; cannot unilaterally improve labor or environmental conditions without coordinated peer action. Mixed: benefits in short term, trapped in deteriorating equilibrium long term.
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, regulatory arbitrage might be misclassified as an immutable feature of decentralized governance: 'jurisdictional competition is inherent to federalism and international relations.' This perspective naturalizes what is actually a contingent institutional arrangement (capital mobility, tax competition, regulatory non-recognition). The engine's false summit detector will identify this as naturalization rather than genuine NL. The constraint is coordinate-able through harmonization (OECD, BEPS, Basel) — it is not inherent to federalism itself.
constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_arbitrage_tangled_rope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_arbitrage_tangled_rope, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_arbitrage_tangled_rope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_arbitrage_tangled_rope, TR),
    TR >= 0.70.

:- end_tests(regulatory_arbitrage_tangled_rope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, increasing over interval. The constraint extracts from trapped and constrained agents (high-regulation jurisdictions, SMEs) by forcing competitive deregulation. However, extraction is not total because multinationals do incur real compliance costs and some regulatory coordination persists. The growth trajectory (0.35→0.58) reflects accelerating capital mobility and growing pressure on regulatory regimes. Suppression (0.62): High. Material barriers to exit include capital-flight threats, competitive pressure, and lack of multilateral coordination mechanisms. High-regulation jurisdictions literally cannot exit without coordinated peer commitment. Firms' capital mobility creates credible exit threat. Suppression remains constant over interval — it is structural to the federated governance system. Theater ratio (0.45): Moderate and increasing. Regulatory compliance is substantively functional — firms cannot fully evade or game regulations — so theater is not high. However, enforcement theater increases as enforcement capacity erodes relative to capital mobility (jurisdictions perform inspection and reporting even as actual behavioral compliance declines). The growth reflects Goodhart drift: regulatory metrics become gaming targets rather than true coordination signals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates genuine perspectival divergence despite single base_properties metrics. The beneficiary (mobile firm, institutional/arbitrage) sees Rope — they genuinely solve coordination problems and experience the constraint as enabling. The primary victim (high-regulation jurisdiction, powerless/trapped) sees Snare — they experience maximum extraction with no escape. The secondary victim (SME, moderate/constrained) sees Tangled Rope — real mixed experience of coordination benefit and extraction cost. The low-regulation jurisdiction (institutional/constrained) also sees Tangled Rope but from opposite angle — benefits from inflow but trapped in downward spiral. The regulatory coalition (organized) sees Scaffold with sunset — international harmonization is building alternative pathways. The piton perspective (legacy framework) sees degraded institutional ritual. The false-summit analytical view naturalizes what is coordinate-able institutional arrangement. No single perspective is 'wrong' — they are all locally accurate descriptions of how different agents experience the same constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural position: mobility + beneficiary/victim status + power level. Mobile multinationals with arbitrage options derive low d (beneficiary + escape capacity → extraction runs toward them). High-regulation jurisdictions with no exit options derive high d (victim + trapped → full target status). SMEs with constrained exit derive moderate-high d (victim + constrained costs → partial target). Low-regulation jurisdictions with constrained exit (cannot unilaterally raise standards without losing investment) derive moderate d (mixed beneficiary/victim status + constrained exit → intermediate directionality). The sigmoid f(d) amplifies d for trapped/constrained agents and dampens d for mobile agents, producing the extracted chi values. Scope (national vs global) affects verification difficulty: at national scope, arbitrage is detectably concentrated; at global scope, arbitrage flows are harder to track, suggesting higher effective extraction for mobile actors at global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled_rope classification captures the genuine hybrid: real coordination (capital allocation efficiency) + real extraction (competitive downward pressure on high-regulation jurisdictions). The mandatrophy question 'Is this coordination or extraction?' is answered: 'Both, from different positions.' Multinationals experience coordination; high-regulation jurisdictions experience extraction; SMEs and low-regulation jurisdictions experience hybrid. The classification prevents mislabeling as pure rope (would hide extraction of trapped jurisdictions) or pure snare (would hide genuine coordination benefit to mobile firms and location optimization). The theater ratio (0.45, moderate) indicates that the coordination function is real — this is not performative regulation, it is substantive compliance with real behavioral effects. The scaffold perspective's sunset logic (international harmonization) confirms that the extraction mechanism is coordinate-able — it is not immutable. Mandatrophy resolved: tangled_rope is the correct type because it holds both functions (coordination of location-optimization + extraction of trapped agents) in irreducible tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_mobility_threshold,
    'What level of regulatory cost differential triggers capital relocation? Is the threshold absolute (firm leaves if costs exceed X%) or relative (firms compare jurisdictions)?',
    'Analysis of firm location decisions and sensitivity to specific regulatory cost components; cross-sector comparison of mobility thresholds (capital-intensive vs labor-intensive industries)',
    'If absolute threshold: regulatory arbitrage is bounded and may not constitute true extraction. If relative threshold: arbitrage is unbounded and extraction grows with regulatory differentiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_threshold, empirical, 'Threshold at which regulatory costs trigger capital relocation').

omega_variable(
    harmonization_coordination_capacity,
    'Can international regulatory bodies (OECD, Basel, BEPS) actually enforce standards, or do they rely on voluntary compliance that firms can arbitrage around?',
    'Assessment of enforcement mechanisms in international agreements; tracking of compliance rates and detection of evasion workarounds; comparison of stated vs actual regulatory outcomes post-agreement',
    'If enforcement capacity is real: scaffold sunset is credible, and regulatory arbitrage is genuinely temporary. If enforcement relies on voluntary compliance: sunset is aspirational rather than structural, and arbitrage persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harmonization_coordination_capacity, empirical, 'Whether international harmonization bodies have enforcement capacity').

omega_variable(
    race_to_bottom_inevitability,
    'Is competitive deregulation an inevitable equilibrium (low-regulation jurisdictions cannot unilaterally raise standards) or a coordination failure (could be solved by mutual commitment)?',
    'Game-theoretic analysis of jurisdictional competition; historical case studies of successful regulatory coordination (EU standards, GDPR adoption); identification of mechanisms that break the race-to-bottom dynamic',
    'If inevitable: low-regulation perspective''s trap is structural and long-term. If coordination failure: the trap can be escaped through treaties and enforcement, supporting the scaffold sunset narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_to_bottom_inevitability, conceptual, 'Whether race-to-bottom is inevitable or solvable through coordination').

omega_variable(
    domestic_sme_exit_alternatives,
    'What are SMEs'' actual exit options beyond geographic relocation? (Outsourcing, vertical integration, specialization in high-regulation niches, political coalition-building)',
    'Survey of SME adaptation strategies; analysis of market segments where SMEs out-compete multinationals; tracking of political organization by SME coalitions',
    'If exit alternatives are significant: SME is less trapped than the tangled rope classification suggests — reclassify toward rope. If alternatives are limited: SME truly experiences mixed extraction and coordination, confirming tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_sme_exit_alternatives, empirical, 'SME exit alternatives beyond geographic relocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_arbitrage_tangled_rope, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regarbtr_tr_t0, regulatory_arbitrage_tangled_rope, theater_ratio, 0, 0.35).
narrative_ontology:measurement(regarbtr_tr_t5, regulatory_arbitrage_tangled_rope, theater_ratio, 5, 0.4).
narrative_ontology:measurement(regarbtr_tr_t10, regulatory_arbitrage_tangled_rope, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(regarbtr_be_t0, regulatory_arbitrage_tangled_rope, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regarbtr_be_t5, regulatory_arbitrage_tangled_rope, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(regarbtr_be_t10, regulatory_arbitrage_tangled_rope, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_arbitrage_tangled_rope, resource_allocation).
narrative_ontology:affects_constraint(regulatory_arbitrage_tangled_rope, tax_competition).
narrative_ontology:affects_constraint(regulatory_arbitrage_tangled_rope, capital_flight_threat).
narrative_ontology:affects_constraint(regulatory_arbitrage_tangled_rope, regulatory_harmonization).

% DUAL FORMULATION NOTE:
% Regulatory arbitrage decomposes into domain-specific stories (tax arbitrage, labor standard arbitrage, environmental regulation arbitrage, financial regulation arbitrage) each with distinct ε values. This story represents the generic structural mechanism across all domains. Domain-specific stories have higher ε values (more extraction, less genuine coordination) in contexts where harmonization is weaker (tax arbitrage ε=0.68) and lower ε values where harmonization is stronger (GDPR-style privacy arbitrage ε=0.35). All domain stories link to this parent constraint via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_arbitrage_tangled_rope, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
