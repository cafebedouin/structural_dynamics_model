% ============================================================================
% CONSTRAINT STORY: k_cultural_export_dependence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_k_cultural_export_dependence, []).

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
 *   constraint_id: k_cultural_export_dependence
 *   human_readable: K-Cultural Export Dependence and Soft Power Asymmetry
 *   domain: cultural_political_economy/geopolitics
 *
 * SUMMARY:
 *   K-cultural export dependence emerges from the asymmetric distribution of
 *   media production capital, creative talent concentration, and platform
 *   control. Nations with larger domestic markets, higher per-capita
 *   consumption, and accumulated entertainment infrastructure generate
 *   cultural products that dominate global distribution networks. Peripheral
 *   nations face structural barriers to local content viability: smaller
 *   domestic audiences, limited capital for production investment,
 *   algorithmic prioritization favoring major-market content, and platform
 *   business models that maximize global reach over regional diversity. The
 *   constraint exhibits characteristics of both coordination (solving the
 *   problem of entertaining global audiences) and extraction (appropriating
 *   cultural consumption patterns and audience attention in peripheral
 *   markets). The theater ratio (0.48) reflects moderate performative content
 *   — national cultural protection policies (quota systems, fund-raising,
 *   content mandates) often fail to shift consumption patterns against
 *   platform dynamics, yet persist as symbolic resistance to export
 *   dependence. The extractiveness trajectory (0.32 → 0.58 over 20 years)
 *   indicates accumulating asymmetry as streaming platforms consolidate and
 *   domestic content production capital remains concentrated in high-income
 *   countries.
 *
 * KEY AGENTS:
 *   - Peripheral Nation Audiences: Primary victims (powerless/trapped) — confined to imported content by platform dominance and limited domestic alternatives; bear cost of cultural sovereignty erosion
 *   - Local Content Producers: Secondary victims (moderate/constrained) — face distribution barriers, algorithm discrimination, and funding scarcity; also benefit from shared infrastructure and skill diffusion
 *   - Content Production Companies: Primary beneficiaries (institutional/arbitrage) — capture global audience and revenue streams; experience constraint as coordination mechanism
 *   - Cultural Distribution Platforms: Primary beneficiaries (institutional/arbitrage) — control distribution networks and algorithmic amplification; extract value through dominance of attention infrastructure
 *   - State Soft Power Apparatus: Secondary beneficiary (institutional/constrained) — benefits from exporting national cultural narratives; constrained by platform corporate control
 *   - International Cultural Preservation Coalition: Organized agents (organized/constrained) — attempt to build alternative infrastructure and capacity; see sunset mechanisms in regional platforms and funding programs
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent capital distribution as immutable market law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(k_cultural_export_dependence, 0.58).
domain_priors:suppression_score(k_cultural_export_dependence, 0.65).
domain_priors:theater_ratio(k_cultural_export_dependence, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(k_cultural_export_dependence, extractiveness, 0.58).
narrative_ontology:constraint_metric(k_cultural_export_dependence, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(k_cultural_export_dependence, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(k_cultural_export_dependence, tangled_rope).
narrative_ontology:human_readable(k_cultural_export_dependence, "K-Cultural Export Dependence and Soft Power Asymmetry").
narrative_ontology:topic_domain(k_cultural_export_dependence, "cultural_political_economy/geopolitics").

domain_priors:requires_active_enforcement(k_cultural_export_dependence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(k_cultural_export_dependence, content_production_companies).
narrative_ontology:constraint_beneficiary(k_cultural_export_dependence, cultural_distribution_platforms).
narrative_ontology:constraint_beneficiary(k_cultural_export_dependence, state_soft_power_apparatus).
narrative_ontology:constraint_victim(k_cultural_export_dependence, domestic_cultural_production_diversity).
narrative_ontology:constraint_victim(k_cultural_export_dependence, audience_autonomy_in_periphery_nations).
narrative_ontology:constraint_victim(k_cultural_export_dependence, local_cultural_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL NATION AUDIENCE (SNARE) — Structurally trapped in the consumption of exported cultural products. Limited domestic alternatives due to resource constraints and industrial concentration. Zero exit options: entertainment infrastructure, platform dominance, and distribution networks funnel toward imported content. Maximum experienced extraction — audience bears the cost of cultural sovereignty erosion.
constraint_indexing:constraint_classification(k_cultural_export_dependence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL CONTENT PRODUCERS (TANGLED ROPE) — Constrained by distribution barriers, funding scarcity, and platform algorithmic prioritization of major-market content. High cost to exit means producing for smaller audiences or accepting market subordination. However, genuine coordination benefits exist: they benefit from shared infrastructure, viewer base development, and technical knowledge diffusion. Extraction is asymmetric but not total — some producers reach significant audiences despite the structural constraint.
constraint_indexing:constraint_classification(k_cultural_export_dependence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CULTURAL PRODUCTION COMPANIES (ROPE) — Net beneficiaries with high arbitrage capacity. Export markets create revenue streams, platform distribution enables scale, and global audience access justifies production investment. Experiences the constraint as pure coordination: reaching international audiences solves the core problem of monetizing cultural production. Extraction flows toward this agent — they benefit from audience captivity and distribution network control.
constraint_indexing:constraint_classification(k_cultural_export_dependence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL CULTURAL PRESERVATION COALITION (SCAFFOLD) — Organized agents (UNESCO, regional film councils, open-culture platforms) see export dependence as a temporary coordination failure with sunset mechanisms: capacity-building programs, content subsidies, and regional platform development are creating alternative distribution pathways. Low effective extraction because these agents have policy agency and see exit paths through infrastructure development. Sunset estimated at 15-25 years as regional platforms mature.
constraint_indexing:constraint_classification(k_cultural_export_dependence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY NATIONAL BROADCAST SYSTEMS (PITON) — Public broadcasters and state media channels are substantially theatrical in their resistance to export dependence. They declare cultural protection through public funding and local-content mandates, but their actual reach and relevance in the streaming era has degraded. Theater persists through institutional inertia and nostalgia for post-war cultural settlement (UNESCO, national film industries) despite structural irrelevance in the platform economy. The performative declarations of cultural sovereignty mask actual powerlessness against algorithmic distribution.
constraint_indexing:constraint_classification(k_cultural_export_dependence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, unequal distribution of cultural production capital creates inevitable asymmetries: countries with larger populations, higher per-capita media consumption, and deeper capital reserves will produce more content and reach larger audiences. This perspective treats export dependence as a natural law of cultural economics. However, the structural data contradicts the mountain classification — policy choices (copyright regimes, platform regulation, infrastructure investment) are contingent, not immutable. The engine will compute this as a false summit, revealing that 'inevitable market dynamics' naturalizes what is actually a system design choice.
constraint_indexing:constraint_classification(k_cultural_export_dependence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(k_cultural_export_dependence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(k_cultural_export_dependence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(k_cultural_export_dependence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(k_cultural_export_dependence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(k_cultural_export_dependence, TR),
    TR >= 0.70.

:- end_tests(k_cultural_export_dependence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts audience attention, cultural consumption patterns, and revenue streams from peripheral-market audiences and producers toward concentrated capital centers and platform corporations. The extraction is substantial but not maximal because: (1) genuine coordination benefits exist (creators reach audiences, audiences access content), (2) some local production succeeds despite barriers, (3) technology enables distribution at lower cost than legacy systems. Suppression (0.65): High. Multiple reinforcing mechanisms suppress alternatives: platform algorithms optimize for engagement (concentrating visibility on high-budget productions), capital barriers to entry in content production, network effects favoring established producers, asymmetric payment terms, and international IP regimes that protect major-market copyright. Theater ratio (0.48): Moderate. Cultural protection policies (quota systems, public broadcasting, national film funds) claim to preserve local production but often remain performative in the streaming era — their actual impact on consumption patterns is limited relative to platform algorithmic power. As platforms consolidate, theater declines from historical highs (legacy broadcast-era protectionism), but policy continues to declare resistance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears as pure extraction to trapped audiences (Snare), mixed coordination-and-extraction to moderate producers (Tangled Rope), pure coordination to beneficiary corporations (Rope), temporary with resolvable sunset to organized coalitions (Scaffold), and degraded theater to legacy institutions (Piton). The civilizational analytical observer risks naturalizing contingent capital distribution as immutable market law (false Mountain). The perspectival gap reveals that classification depends on exit options and power level more than on the underlying structural extractiveness — peripheral audiences experience maximum extraction because they are trapped, not because extraction is inherently total. Organized agents see scaffolding and sunset because they have capacity to intervene. Beneficiaries see pure coordination because extraction flows away from them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies across perspectives based on structural position relative to the extraction flow. Content production companies and distribution platforms are beneficiaries with high arbitrage capacity (low d → negative χ), experiencing the constraint as coordination. Peripheral nation audiences are victims with trapped exit options (high d → high f(d) → high χ), bearing maximum extraction. Local producers occupy intermediate positions: they are partly victims of distribution barriers (moderate d) but partly benefit from coordination. The state apparatus occupies an ambiguous position — it benefits from cultural soft power export but is constrained by corporate platform control, creating a constrained-beneficiary hybrid. Platform algorithmic directionality is not transparent to most agents, which is itself a suppression mechanism: audiences and creators experience the extraction as market outcomes rather than design choices, reducing perception of alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: This constraint resolves mandatrophy through policy intervention vectors rather than falsifiable empirical discovery. The mandatrophy is not 'which type is correct?' but 'which type do we want to observe?' The constraint exhibits genuine coordination (creators reaching audiences solves a real problem) AND genuine extraction (asymmetric appropriation of attention and revenue from peripheral agents). The tension is not resolvable by choosing the 'true' classification; it is resolvable by redistributing power through: (1) platform regulation (algorithmic transparency, recommendation system redesign), (2) infrastructure investment (regional streaming platforms, production capacity-building), (3) revenue structure reform (direct creator payments, reduced platform extraction margins), and (4) audience infrastructure (media literacy, alternative distribution networks). The Tangled Rope classification is accurate — the constraint genuinely performs both functions. The mandatrophy resolves not by proving one function is illusory, but by showing that reducing the extraction component while preserving the coordination component is structurally feasible. This requires intentional policy choice, not discovery of hidden structural properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_authenticity_threshold,
    'At what point does exposure to exported cultural content constitute genuine audience preference versus manufactured dependence through platform dominance?',
    'Audience surveys with counter-factual infrastructure scenarios; A/B testing of algorithmic recommendation systems; historical analysis of viewer behavior pre- and post-platform consolidation',
    'If manufactured dependence dominates: classification strengthens toward Snare. If genuine preference dominates: classification weakens toward Rope or pure coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_authenticity_threshold, empirical, 'Threshold between genuine preference and platform-manufactured dependence').

omega_variable(
    local_production_capacity_floor,
    'Does local cultural production capacity exist in peripheral nations but remain suppressed by distribution barriers, or is the capacity itself genuinely limited by capital and human resources?',
    'Comparative analysis of production industry metrics (studios, talent, equipment, training institutions) across regions of equivalent GDP; pilot infrastructure programs measuring audience engagement with locally-produced content when distribution barriers are removed',
    'If suppressed capacity: reducing barriers enables rapid local production growth (suggests Scaffold with real sunset). If genuinely limited: infrastructure investment must precede demand, extending sunset timeline by decades.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_production_capacity_floor, empirical, 'Whether local production capacity is suppressed or genuinely limited').

omega_variable(
    platform_neutrality_achievability,
    'Can algorithmic recommendation systems be designed to amplify locally-produced content without creating new asymmetries (e.g., state propaganda, cultural protectionism that excludes minority voices)?',
    'Pilot programs implementing locality-weighted recommendation systems; analysis of outcomes across different platform designs; comparison with non-algorithmic curation models (human curation, community selection)',
    'If achievable: policy intervention can reduce extraction without creating new harms (Scaffold confirms). If unachiable: local content amplification trades one extraction mechanism for another (complexity shifts extraction rather than eliminating it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_neutrality_achievability, conceptual, 'Whether algorithmic neutrality can amplify local content without new asymmetries').

omega_variable(
    revenue_distribution_mechanism_feasibility,
    'Can revenue from global audiences be genuinely distributed to peripheral-nation creators, or does payment infrastructure concentration recreate extraction at the settlement layer?',
    'Analysis of payment flows from platforms to creators across regions; audit of currency conversion, payment timing, and fees; comparison of effective rates of return (creator revenue as percentage of viewing value) across production countries',
    'If revenue distribution is achievable: Tangled Rope can shift toward Rope through infrastructure reform. If infrastructure concentration recreates extraction: Tangled Rope may degrade into persistent Snare regardless of content access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_distribution_mechanism_feasibility, empirical, 'Whether revenue distribution prevents extraction concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(k_cultural_export_dependence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kcexd_tr_t0, k_cultural_export_dependence, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kcexd_tr_t10, k_cultural_export_dependence, theater_ratio, 10, 0.42).
narrative_ontology:measurement(kcexd_tr_t20, k_cultural_export_dependence, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(kcexd_be_t0, k_cultural_export_dependence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(kcexd_be_t10, k_cultural_export_dependence, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(kcexd_be_t20, k_cultural_export_dependence, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(k_cultural_export_dependence, global_infrastructure).
narrative_ontology:affects_constraint(k_cultural_export_dependence, soft_power_asymmetry).
narrative_ontology:affects_constraint(k_cultural_export_dependence, platform_algorithmic_dominance).
narrative_ontology:affects_constraint(k_cultural_export_dependence, intellectual_property_regime).
narrative_ontology:affects_constraint(k_cultural_export_dependence, attention_economy_concentration).

% DUAL FORMULATION NOTE:
% K-cultural export dependence is downstream of platform algorithmic dominance and attention economy concentration, but represents a distinct structural constraint with its own extractiveness metrics. The upstream constraints (platform dominance, IP regimes) have their own extractiveness reflecting specific technical and legal design choices; this constraint has its own extractiveness reflecting the aggregate effect on cultural production diversity and audience autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(k_cultural_export_dependence, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
