% ============================================================================
% CONSTRAINT STORY: settlement_expansion_encroachment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_settlement_expansion_encroachment, []).

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
 *   constraint_id: settlement_expansion_encroachment
 *   human_readable: Settlement Expansion Encroachment on Indigenous and Communal Lands
 *   domain: geopolitical/land_rights/environmental
 *
 * SUMMARY:
 *   Settlement expansion encroachment on indigenous and communal lands
 *   represents a multi-century structural constraint that combines legitimate
 *   coordination problems (population growth, housing demand, infrastructure
 *   development) with systematic extraction through legal frameworks that
 *   deny indigenous property rights and suppress alternative land-use models.
 *   The constraint exhibits the full range of DR classification: dispossessed
 *   communities experience it as pure extraction (snare); moderate
 *   agricultural populations experience mixed coordination and extraction
 *   (tangled rope); developers and state institutions experience it as pure
 *   coordination (rope and piton); international land rights movements
 *   experience it as a temporary institutional failure being resolved through
 *   norm diffusion (scaffold); and naive civilizational analysis risks
 *   naturalizing it as an immutable law of settlement dynamics (false
 *   mountain). The extractiveness trajectory (0.35→0.58 over 30 years)
 *   reflects accelerating encroachment as frontier areas become economically
 *   valuable and technological capacity for environmental modification
 *   increases. The stable theater_ratio (0.42→0.51) indicates that while
 *   extraction is accelerating, the performative content of legal and
 *   institutional frameworks remains moderate — some genuine coordination
 *   exists alongside the extraction mechanism.
 *
 * KEY AGENTS:
 *   - Indigenous populations: Primary victim (powerless/trapped) — legally dispossessed, geographically immobile, economically dependent on dispossessed territories
 *   - Settler governments: Primary beneficiary (institutional/arbitrage) — expand tax base, population, and territory through land claims; maintain legal title monopoly
 *   - Real estate developers: Primary beneficiary (institutional/arbitrage) — extract land value through state-granted permits and legal title; face minimal compliance costs
 *   - Agricultural smallholders: Secondary victim (moderate/constrained) — lose prime land to settlement but benefit from infrastructure coordination; partially captured by encroachment dynamics
 *   - Environmental ecosystems: Tertiary victim (powerless/trapped) — no organizational capacity or exit option; experience irreversible extraction through habitat loss
 *   - Conservation organizations: Organized victim (organized/mobile) — can shift focus geographically but constrained by funding dependencies; experience mixed coordination and extraction
 *   - International land rights movement: Organized beneficiary (organized/constrained) — benefits from UNDRIP implementation but constrained by state enforcement capacity and political will
 *   - State legal title system: Institutional actor (institutional/arbitrage) — maintains performative cadastral framework that enables extraction; sees alternative title systems as threats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(settlement_expansion_encroachment, 0.58).
domain_priors:suppression_score(settlement_expansion_encroachment, 0.72).
domain_priors:theater_ratio(settlement_expansion_encroachment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(settlement_expansion_encroachment, extractiveness, 0.58).
narrative_ontology:constraint_metric(settlement_expansion_encroachment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(settlement_expansion_encroachment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(settlement_expansion_encroachment, tangled_rope).
narrative_ontology:human_readable(settlement_expansion_encroachment, "Settlement Expansion Encroachment on Indigenous and Communal Lands").
narrative_ontology:topic_domain(settlement_expansion_encroachment, "geopolitical/land_rights/environmental").

domain_priors:requires_active_enforcement(settlement_expansion_encroachment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(settlement_expansion_encroachment, settler_governments).
narrative_ontology:constraint_beneficiary(settlement_expansion_encroachment, real_estate_developers).
narrative_ontology:constraint_beneficiary(settlement_expansion_encroachment, construction_industry).
narrative_ontology:constraint_victim(settlement_expansion_encroachment, indigenous_populations).
narrative_ontology:constraint_victim(settlement_expansion_encroachment, land_dispossessed_communities).
narrative_ontology:constraint_victim(settlement_expansion_encroachment, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED INDIGENOUS COMMUNITY (SNARE) — Trapped by legal frameworks that deny land rights, by geographic immobility (ancestral territories are non-fungible), and by economic dependency on land-based livelihoods. Complete material barriers to exit; extraction of territory with minimal coordination benefit. The constraint's suppression (0.72) reflects legal prohibition, cultural fragmentation through relocation, and epistemic closure around indigenous land claims.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AGRICULTURAL SMALLHOLDER (TANGLED ROPE) — Constrained by capital barriers to relocation and market dependency, but also benefits from infrastructure coordination (roads, water systems) that settlement expansion brings. Mixed experience: genuine coordination of shared resources alongside asymmetric extraction of prime agricultural land. Moderate extraction with constrained exit.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REAL ESTATE DEVELOPER (ROPE) — Experiences settlement expansion as pure coordination: assembling land parcels, building infrastructure, creating residential networks. Benefits from state enforcement (legal title, zoning authority, development permits). Arbitrage exit (can invest elsewhere if local opportunities saturate). Net beneficiary position — constraint flows extraction toward this agent.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSERVATION ORGANIZATION (TANGLED ROPE) — Organized agent with exit capacity (can shift focus to other protected areas, other regions). Experiences both coordination (land management protocols, biodiversity monitoring) and extraction (cannot prevent encroachment, resources diverted to damage control rather than proactive conservation). Mobile exit but constrained by funding dependencies on donor bases in encroaching nations. Mixed extraction across generational time horizon.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL LAND RIGHTS MOVEMENT (SCAFFOLD) — Organized agents (UN UNDRIP framework, NGOs, indigenous confederations) see settlement encroachment as a temporary institutional failure with a sunset clause. UNDRIP (2007) and ILO 169 establish legal precedent for indigenous land sovereignty. The constraint's extraction mechanism (state monopoly on legal title, settler-colonial framing) is being eroded by international norm diffusion. Extractiveness should decline over time as legal and epistemic frameworks shift — estimated 20-30 year sunset as states implement UNDRIP provisions and land commissions restore indigenous title.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE LEGAL TITLE SYSTEM (PITON) — The colonial-era legal framework that grants state monopoly on land title is largely performative: it claims to coordinate land use through property law, but in practice it sustains encroachment through institutional inertia. The system persists because alternative title frameworks (indigenous customary law, community commons, land trusts) have not fully replaced it, not because it functionally solves coordination problems. Theater ratio (0.48) reflects performative aspects (cadastral surveys, title registration) alongside real enforcement mechanisms (police, eviction, legal penalties). The piton classification is weak — theater_ratio does not exceed 0.70, indicating some residual coordination function.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a civilizational/universal scope, settlement expansion might appear as an immutable law: human populations inevitably expand into available land; agricultural frontier dynamics are inherent to population growth and resource scarcity. This perspective risks naturalizing what is actually a contingent institutional arrangement (state property law, settler-colonial frameworks, zoning regimes). The engine's false summit detector will classify this as a false mountain — the structural data reveals that encroachment requires active enforcement (permits, surveys, legal expulsion), not natural law.
constraint_indexing:constraint_classification(settlement_expansion_encroachment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(settlement_expansion_encroachment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(settlement_expansion_encroachment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(settlement_expansion_encroachment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(settlement_expansion_encroachment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(settlement_expansion_encroachment, TR),
    TR >= 0.70.

:- end_tests(settlement_expansion_encroachment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and accelerating. The constraint extracts valuable land from dispossessed communities and ecosystems, diverting it to settler populations and commercial use. The extraction is not maximal (0.70+) because some infrastructure coordination genuinely benefits adjacent populations, and international legal frameworks are beginning to constrain state capacity to dispossess with impunity. The 30-year trajectory shows consistent acceleration (0.35→0.58) as frontier areas become more economically valuable, indicating this is an actively intensifying constraint, not a stable equilibrium. Suppression (0.72): High. Suppression operates through multiple channels: legal prohibition on indigenous land claims, police and military enforcement of evictions, cultural fragmentation through forced relocation, epistemic closure around indigenous land sovereignty (land framed as 'unused' or 'underdeveloped'), economic dependency on settler labor markets post-dispossession. Theater_ratio (0.48): Moderate. The constraint combines performative elements (cadastral surveys, property registration, legal title documentation) with real enforcement mechanisms (police presence, eviction orders, development permits). The theater is not dominant (would require ratio ≥0.70 for piton classification) because land seizure ultimately depends on physical force and property law, not purely on legitimacy theater. The stable ratio over time (0.42→0.51) indicates that legitimacy framing has not increased — the constraint maintains its extractive character even as legal frameworks are internationalized.
 *
 * PERSPECTIVAL GAP:
 *   The tangled_rope classification at the institutional level with enforcement flag captures the constraint's hybrid nature: genuine coordination functions (infrastructure, population distribution, housing) coexist with asymmetric extraction (land seizure, dispossession, ecosystem loss). The snare classification from the dispossessed community perspective reveals that for the primary victims, coordination benefit is minimal — the infrastructure serves extractive access, not autonomous development. The scaffold classification from the organized international perspective reveals an active sunset mechanism: UNDRIP and land commissions are structural pathways to reduce extractiveness. The piton classification from the state legal system perspective reveals that the constraint is maintained through institutional inertia — alternative land tenure systems (indigenous customary law, community commons, land trusts) exist and function, but state title monopoly persists. The false mountain from the civilizational analytical perspective reveals the risk of naturalizing contingent institutional arrangements — settlement expansion requires continuous enforcement (legal, military, epistemic), not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to extraction flow. Dispossessed indigenous communities: d ≈ 0.98 (full victim of extraction, trapped with no exit options) → f(d) ≈ 1.42 → high experienced χ. Settler governments: d ≈ 0.05 (full beneficiary, arbitrage exit to other territories) → f(d) ≈ -0.12 → negative χ (constraint subsidizes them). Real estate developers: d ≈ 0.08 (beneficiary, arbitrage exit to other markets) → f(d) ≈ -0.08 → negative χ. Agricultural smallholders: d ≈ 0.58 (mixed: some victim status via land loss, some beneficiary status via infrastructure) → f(d) ≈ 0.75 → moderate χ. Conservation organizations: d ≈ 0.65 (predominantly victim of habitat loss, but organized with geographic mobility) → f(d) ≈ 1.00 → moderate-high χ. International land rights movement: d ≈ 0.42 (predominantly victim of delayed implementation, but with legal-normative leverage) → f(d) ≈ 0.38 → low χ. These d values are derived from beneficiary/victim declarations plus exit options and power levels; the engine computes f(d) automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that settlement expansion is fundamentally a tangled_rope: it genuinely coordinates population distribution and infrastructure development (coordination function) while asymmetrically extracting land from dispossessed communities (extraction mechanism). The mandatrophy is resolved by refusing to collapse into either pure-coordination (rope) or pure-extraction (snare) framing. Pure-coordination framing ('settlement expansion is natural and beneficial') requires ignoring the dispossessed perspective and the asymmetric impact on indigenous populations. Pure-extraction framing ('settlement expansion is nothing but land theft') requires ignoring genuine infrastructure coordination and acknowledging that population growth and housing demand are real coordination problems. The tangled_rope classification, combined with the perspectival gaps showing snare from victim perspectives and rope from beneficiary perspectives, reveals the true structure: the constraint solves a real coordination problem (housing demand) through a mechanism that extracts from those least able to exit (indigenous populations) and least able to capture benefits (ecosystems). The constraint's legitimacy depends on maintaining this opacity — framing extraction as coordination and calling dispossession 'development.' The scaffold perspective from the international movement shows the sunset mechanism: UNDRIP and land commissions, once implemented, would reframe land as indigenous-sovereign rather than state-monopoly, changing the constraint's structure fundamentally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_title_legitimacy,
    'Is state-granted legal title to indigenous lands a coordinative institution or an extractive legal fiction imposed by settler-colonial power?',
    'Historical analysis of land claim resolution; comparison of outcomes under international frameworks (UNDRIP, ILO 169) vs domestic law; measurement of indigenous land recovery post-legal framework change',
    'If coordinative: settlement expansion is Rope (mutual benefit through coherent property law). If extractive fiction: settlement expansion is Snare (naked dispossession with legality as theater). Current framework bifurcates into tangled_rope because coordination and extraction coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_title_legitimacy, conceptual, 'Whether state legal title represents coordination or imposed extraction').

omega_variable(
    infrastructure_benefit_distribution,
    'Do settlement expansion infrastructure investments (roads, water, electricity) actually benefit dispossessed communities, or does infrastructure primarily serve extractive access and future encroachment?',
    'Cost-benefit analysis of infrastructure access pre- and post-settlement expansion for indigenous communities; measurement of service quality, price, and dependency creation',
    'If genuine benefit: justifies tangled_rope classification for moderate agents. If primarily extraction mechanism: all agents except developers should classify as snare; the infrastructure is a tool of suppression, not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_benefit_distribution, empirical, 'Whether settlement infrastructure benefits or extracts from indigenous communities').

omega_variable(
    undrip_enforcement_timeline,
    'What is the realistic implementation timeline for UNDRIP land rights provisions, and will enforcement precede or follow complete dispossession of remaining indigenous territories?',
    'Tracking state ratification rates, land commission establishment, and actual title restoration in pilot jurisdictions; correlation with settlement expansion rates in same regions',
    'If enforcement accelerates (< 15 years): scaffold sunset is real, extractiveness should decline measurably. If enforcement stalls (> 40 years): scaffold classification is aspirational; constraint should be reclassified as snare with slow institutional drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(undrip_enforcement_timeline, empirical, 'Timeline for UNDRIP enforcement relative to territorial dispossession').

omega_variable(
    ecosystem_irreversibility,
    'Are ecosystems displaced by settlement expansion functionally irreversible, or do they constitute a victim with measurable extraction costs?',
    'Ecological succession modeling; measurement of ecosystem recovery timescales post-settlement removal; valuation of lost ecosystem services (carbon, water, biodiversity)',
    'If irreversible: ecosystem victims should be weighted with infinite extraction cost — snare from ecosystem perspective. If reversible: extractiveness is measurable but not maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_irreversibility, empirical, 'Reversibility of ecosystem impacts from settlement expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(settlement_expansion_encroachment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(settle_tr_t0, settlement_expansion_encroachment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(settle_tr_t10, settlement_expansion_encroachment, theater_ratio, 10, 0.45).
narrative_ontology:measurement(settle_tr_t20, settlement_expansion_encroachment, theater_ratio, 20, 0.48).
narrative_ontology:measurement(settle_tr_t30, settlement_expansion_encroachment, theater_ratio, 30, 0.51).

% Extraction over time
narrative_ontology:measurement(settle_be_t0, settlement_expansion_encroachment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(settle_be_t10, settlement_expansion_encroachment, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(settle_be_t20, settlement_expansion_encroachment, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(settle_be_t30, settlement_expansion_encroachment, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(settlement_expansion_encroachment, resource_allocation).
narrative_ontology:affects_constraint(settlement_expansion_encroachment, indigenous_land_rights_denial).
narrative_ontology:affects_constraint(settlement_expansion_encroachment, ecosystem_habitat_loss).
narrative_ontology:affects_constraint(settlement_expansion_encroachment, settler_colonial_property_law).

% DUAL FORMULATION NOTE:
% Settlement expansion encroachment is upstream to multiple related constraints: denial of indigenous land rights (legal framework enabling encroachment), ecosystem habitat loss (ecological consequence), and settler-colonial property law (institutional foundation). Each linked constraint has its own ε and perspectival structure; this story models the encroachment mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(settlement_expansion_encroachment, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
