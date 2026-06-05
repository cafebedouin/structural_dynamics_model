% ============================================================================
% CONSTRAINT STORY: french_ag_land_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_ag_land_concentration, []).

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
 *   constraint_id: french_ag_land_concentration
 *   human_readable: French Agricultural Land Concentration
 *   domain: economic/agricultural_policy
 *
 * SUMMARY:
 *   French agricultural land concentration represents a four-decade
 *   structural extraction mechanism operating through overlapping policy,
 *   credit, and market mechanisms. Since the 1980s, the average farm size in
 *   France has increased from ~23 hectares to ~65 hectares, while the number
 *   of farms declined by 65%. This is not an inevitable economic law but a
 *   contingent outcome of subsidies, credit policy, and land market dynamics
 *   that systematically benefit large operators while trapping small farmers
 *   and excluding new entrants. The constraint exhibits both genuine
 *   coordination (large-scale mechanization, input optimization, global
 *   market competition) and pure extraction (rent capture through land
 *   appreciation, community degradation, generational poverty traps). The EU
 *   Common Agricultural Policy (CAP) amplifies consolidation through
 *   hectare-based subsidy allocation, creating a structural incentive for
 *   larger holdings. Agricultural credit mechanisms require collateral and
 *   minimum viable scales, locking out small operators. Land markets respond
 *   to these incentives by pricing small parcels out of reach for family
 *   farmers. The result is a snare for powerless actors (small farmers,
 *   aspiring entrants, rural communities) and a rope for institutional actors
 *   (agribusiness, equipment manufacturers, large operators). The 'family
 *   farming protection' rhetoric persists as piton-level theater (ε ≤ 0.25,
 *   theater ≥ 0.70) while material mechanisms contradict stated policy. The
 *   analytical observer sees a true tangled rope: coordination function
 *   (mechanization, efficiency) is real but inseparable from extraction
 *   function (capital accumulation, community extraction).
 *
 * KEY AGENTS:
 *   - Small-scale farmers: Primary victim (powerless/trapped) — declining income, forced consolidation or exit, unable to compete
 *   - Aspiring new entrants: Primary victim (powerless/trapped) — land acquisition costs prohibitive, structural barriers prevent sector entry
 *   - Rural communities: Secondary victim (moderate/constrained) — depopulation from agricultural decline, service closure, cultural erosion
 *   - Large agribusiness operators: Primary beneficiary (institutional/arbitrage) — benefit from scale economies, subsidy concentration, equipment efficiency
 *   - Agricultural equipment manufacturers: Secondary beneficiary (institutional/arbitrage) — benefit from standardized large-farm orders
 *   - Land speculation capital: Beneficiary (institutional/arbitrage) — accumulates land value appreciation driven by consolidation trends
 *   - EU Common Agricultural Policy: Structural enforcer (powerful/constrained) — coordinates markets while embedding consolidation incentives
 *   - French government: Institutional actor (institutional/arbitrage) — maintains family farming rhetoric while implementing consolidation policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_ag_land_concentration, 0.58).
domain_priors:suppression_score(french_ag_land_concentration, 0.68).
domain_priors:theater_ratio(french_ag_land_concentration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_ag_land_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(french_ag_land_concentration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(french_ag_land_concentration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_ag_land_concentration, snare).
narrative_ontology:human_readable(french_ag_land_concentration, "French Agricultural Land Concentration").
narrative_ontology:topic_domain(french_ag_land_concentration, "economic/agricultural_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, large_agribusiness_operators).
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, agricultural_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, land_speculation_capital).
narrative_ontology:constraint_victim(french_ag_land_concentration, small_scale_farmers).
narrative_ontology:constraint_victim(french_ag_land_concentration, aspiring_new_entrants).
narrative_ontology:constraint_victim(french_ag_land_concentration, rural_community_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL FARMER (SNARE) — Trapped in agricultural land market. Rising land prices driven by large operators and capital investment squeeze out small holdings. Cannot scale operations without credit inaccessible at current rates; cannot exit without abandoning generational holdings. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(french_ag_land_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING NEW ENTRANT (SNARE) — Shut out of agricultural sector before entry. Land consolidation eliminates small parcels suitable for new farming. Credit requirements and land acquisition costs create a structural barrier. Trap is total — cannot acquire land to farm; cannot farm without land. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.83.
constraint_indexing:constraint_classification(french_ag_land_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE AGRIBUSINESS OPERATOR (ROPE) — Primary beneficiary. Land consolidation enables economy of scale, mechanization, and capital efficiency. Operates within a coordination system (subsidy allocation by farm size, input supplier networks, equipment leasing). Experiences constraint as enabling coordination. d≈0.08, f(d)≈-0.15, σ=1.0 → χ≈-0.09. Net beneficiary.
constraint_indexing:constraint_classification(french_ag_land_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EQUIPMENT MANUFACTURER (ROPE) — Benefits from consolidation through larger uniform orders. Consolidated farms standardize equipment needs. High-volume sales to large operators reduce transaction costs. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(french_ag_land_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EU CAP FRAMEWORK (TANGLED ROPE) — Simultaneously coordinates (sets common pricing, subsidy mechanisms, market stability) and extracts (land subsidy allocation favors large holdings, creating consolidation pressure). CAP is enforced; extraction is systemic. Benefits large operators; costs borne by small farmers and communities. χ≈0.52, within tangled rope range (0.40-0.90).
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RURAL COMMUNITY (SNARE) — Constrained by depopulation from agricultural land concentration. School closures, service consolidation, cultural heritage erosion follow population loss. Communities cannot collectively reverse consolidation trends. d≈0.82, f(d)≈1.20, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(french_ag_land_concentration, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LAND REFORM RHETORIC (PITON) — Government maintains 'protection of family farming' discourse while CAP subsidy structure and agricultural credit policy actively drive consolidation. theater_ratio=0.55. Rhetoric persists through political inertia (voter expectations of 'peasant farmers') while material mechanisms contradict stated policy. Degraded constraint.
constraint_indexing:constraint_classification(french_ag_land_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global view: land concentration appears as coordination (mechanization, scale efficiency, global market competition) overlaid with extraction (capital accumulation, community extraction, generational poverty traps). Both functions are structural. ε=0.58 reflects genuine mixed function. Not a mountain (false law naturalization); a true tangled rope.
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_ag_land_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_ag_land_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_ag_land_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_ag_land_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_ag_land_concentration, TR),
    TR >= 0.70.

:- end_tests(french_ag_land_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts substantial value from small farmers (through forced sales at depreciated rates, inability to access credit) and from communities (through depopulation). However, it is not maximum extraction (0.66+) because some legitimate coordination value exists — mechanization and scale efficiency are real benefits, not pure rent. The extractiveness trajectory (0.32→0.58) reflects CAP intensification and credit market tightening over the interval. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) credit policy barriers (collateral requirements favor large holdings), (2) market barriers (land prices driven by large-operator bids), (3) information barriers (small farmers lack market data, negotiating power), (4) path dependency (once consolidated, returning to small-holder viability is costly). Small farmers and entrants have limited realistic alternatives. Theater ratio (0.55): Moderate. Government rhetoric emphasizes family farming protection while subsidy design and credit policy drive consolidation. Rhetoric is performative (theater), but the constraint's core mechanism is functional extraction, not theatrical maintenance. Theater has increased (0.35→0.55) as gap between policy rhetoric and actual mechanism has widened.
 *
 * PERSPECTIVAL GAP:
 *   The small farmer sees a snare (trapped, unable to exit); the large operator sees a rope (beneficial coordination); the rural community sees slow-motion extraction (depopulation snare); the analytical observer sees tangled rope (both coordination and extraction are structural). The government maintains a piton perspective (family farming rhetoric without structural function). The EU CAP framework appears as tangled rope to observers inside France but might appear as mountain (economic inevitability) to those naturalizing global commodity market competition. This perspectival gap reveals the constraint's true nature: consolidation is not an immutable law but a contingent outcome of policy choices that could be reversed. The gap also explains persistent policy failure — policymakers claiming to protect small farmers while implementing policies that destroy them.
 *
 * DIRECTIONALITY LOGIC:
 *   Small farmers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction direction. Aspiring entrants: Victim + trapped → d≈0.95, f(d)≈1.42. Total barrier to entry. Rural communities: Victim + constrained → d≈0.82, f(d)≈1.20. Significant extraction without total trap (some communities stabilize). Large operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.15. Net beneficiary with exit options. Equipment manufacturers: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary, minimal extraction exposure. EU CAP: Enforcer + constrained → d≈0.45, f(d)≈0.50. Mixed function (both coordinate and extract). Government: Institutional + arbitrage → d≈0.10, f(d)≈-0.14. Benefits from consolidation (centralization, fewer actors to coordinate).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by distinguishing coordination (mechanization, scale efficiency, global market response) from extraction (rent capture, generational poverty, community destruction). Snare classification (χ≈0.81 for powerless agents) correctly identifies that for small farmers and entrants, the constraint functions as pure extraction: they experience costs without corresponding coordination benefits. Tangled rope classification (χ≈0.52 for EU CAP framework) correctly identifies that the policy framework simultaneously solves coordination problems (unified pricing, subsidy mechanism, market stability) and extracts from victims (hectare-based allocation, credit requirements, consolidation incentives). The constraint is not a false natural law (mountain): it is a policy choice, reversible through subsidy redesign (grower-based instead of hectare-based), credit reform, and land-access programs. The mandatrophy is resolved by showing that the classification varies legitimately by agent perspective — the constraint truly is a snare for powerless actors and a rope for institutional beneficiaries. The apparent contradiction dissolves once perspective is made explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidies_causality_direction,
    'Do EU CAP subsidy structures (hectare-based, not farmer-based) cause land consolidation, or do they reflect and amplify pre-existing economic trends?',
    'Comparative policy analysis: regions with different subsidy designs; counterfactual modeling of CAP reform scenarios; historical analysis of pre-CAP consolidation rates',
    'If subsidy structure is primary cause: land consolidation is extractive policy choice (Snare type confirmed). If subsidy reflects economic inevitability: constraint appears more as coordination under scarcity (Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidies_causality_direction, empirical, 'Whether CAP subsidy design causes or reflects land consolidation').

omega_variable(
    credit_system_role,
    'How much of observed consolidation is driven by agricultural credit mechanisms (bank lending to large operators, government loans requiring collateral) vs market-driven land price appreciation?',
    'Credit market analysis: loan approval rates by farm size; correlation between credit policy changes and consolidation acceleration; comparative analysis with countries using alternative credit models',
    'If credit mechanisms are primary: constraint is reinforced by policy choice and potentially reversible (Scaffold with sunset possible). If market-driven: constraint appears structural (Mountain risk).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credit_system_role, empirical, 'Role of agricultural credit systems in driving consolidation').

omega_variable(
    aspiring_entrant_viability,
    'Are alternative farming models (organic, niche/high-value crops, cooperative land-sharing) genuinely viable pathways for new entrants, or are they aspirational rhetoric masking structural barriers?',
    'Economic viability analysis of alternative models; longitudinal tracking of new-entrant success rates in niche vs commodity agriculture; cost-of-entry comparison across farm types',
    'If alternatives viable: constraint is partial (some exit options exist, classification shifts toward Tangled Rope or Rope from entrant perspective). If alternatives are theater: constraint is total (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspiring_entrant_viability, empirical, 'Whether alternative farming models provide viable entry pathways').

omega_variable(
    rural_depopulation_feedback,
    'Does rural depopulation from consolidation create a feedback loop (fewer residents → service closure → more depopulation) or a stable equilibrium?',
    'Time-series analysis of service availability vs population; identification of tipping points; comparison with regions that stabilized rural populations',
    'If feedback loop: rural communities face existential extraction (Snare confirmed). If equilibrium: constraint settles at lower population but stable (Scaffold or Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_depopulation_feedback, empirical, 'Whether rural depopulation creates feedback loops or settles at equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_ag_land_concentration, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fraglc_tr_t1980, french_ag_land_concentration, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(fraglc_tr_t2000, french_ag_land_concentration, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(fraglc_tr_t2020, french_ag_land_concentration, theater_ratio, 2020, 0.55).

% Extraction over time
narrative_ontology:measurement(fraglc_be_t1980, french_ag_land_concentration, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(fraglc_be_t2000, french_ag_land_concentration, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(fraglc_be_t2020, french_ag_land_concentration, base_extractiveness, 2020, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_ag_land_concentration, resource_allocation).
narrative_ontology:affects_constraint(french_ag_land_concentration, eu_agricultural_subsidy_allocation).
narrative_ontology:affects_constraint(french_ag_land_concentration, rural_service_consolidation).
narrative_ontology:affects_constraint(french_ag_land_concentration, generational_agricultural_succession).

% DUAL FORMULATION NOTE:
% French land concentration is downstream of EU CAP subsidy structure but represents a distinct structural constraint. Upstream constraints (subsidy allocation mechanism, credit policy architecture) have their own ε values reflecting policy design choices; the land concentration constraint has ε=0.58 reflecting the emergent extractiveness of the combined subsidy-credit-market system. Decomposition enables analysis of which policy levers could reduce extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_ag_land_concentration, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
