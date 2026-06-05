% ============================================================================
% CONSTRAINT STORY: strategic_deep_sea_rare_earth_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strategic_deep_sea_rare_earth_mining, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: strategic_deep_sea_rare_earth_mining
 *   human_readable: Strategic Deep-Sea Mining for Rare Earth Minerals
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Japan's strategic deep-sea mining initiative for rare earth minerals aims
 *   to secure a stable supply of critical resources, reducing reliance on
 *   external sources and bolstering national security. This endeavor presents
 *   a complex interplay of economic benefits, environmental concerns, and
 *   geopolitical implications. The initiative involves developing advanced
 *   mining technologies to extract rare earth elements (REEs) from Japan's
 *   exclusive economic zone, with the potential to disrupt existing supply
 *   chains and raise questions about the sustainability of deep-sea
 *   ecosystems.
 *
 * KEY AGENTS:
 *   - Japanese Government: Primary beneficiary (institutional/arbitrage) - gains strategic resource independence.
 *   - Japanese Mining Companies: Secondary beneficiary (powerful/arbitrage) - profits from new resource access.
 *   - Deep-Sea Ecosystems: Primary victim (powerless/trapped) - faces irreversible habitat damage.
 *   - Competing Rare Earth Suppliers: Secondary victim (moderate/constrained) - potentially displaced by new source.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, 0.55).
domain_priors:suppression_score(strategic_deep_sea_rare_earth_mining, 0.65).
domain_priors:theater_ratio(strategic_deep_sea_rare_earth_mining, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, extractiveness, 0.55).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strategic_deep_sea_rare_earth_mining, tangled_rope).
narrative_ontology:human_readable(strategic_deep_sea_rare_earth_mining, "Strategic Deep-Sea Mining for Rare Earth Minerals").
narrative_ontology:topic_domain(strategic_deep_sea_rare_earth_mining, "geopolitical/economic").

domain_priors:requires_active_enforcement(strategic_deep_sea_rare_earth_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, japanese_government).
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, japanese_mining_companies).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, deep_sea_ecosystems).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, competing_rare_earth_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of deep-sea ecosystems: They are trapped and face irreversible damage due to mining activities. Their voice is not heard, and they have no means of escaping the extraction.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of competing rare earth suppliers (e.g., other countries, land-based mining operations): They are constrained by Japan's technological advantage and access to resources, but they also benefit from increased overall demand for rare earths and potential collaborative opportunities. They face pressure but can still participate in the market. This can become a pure snare if land based mining operations get priced out completely.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective of the Japanese government: They benefit from securing a stable supply of rare earth minerals, reducing reliance on external sources and bolstering national security. The government sees this as a means of coordination and resource management to help bolster the country's overall economic position.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of Japanese mining companies: They are arbitrageurs who profit from accessing new sources of rare earth minerals and developing advanced mining technologies. The extraction is helping expand their market share while simultaneously reducing reliance on external sources of these vital materials.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical perspective: The constraint is a tangled rope because it involves both coordination (securing rare earth supply for Japan) and extraction (environmental damage, impact on competing suppliers). A global view looking at the long term trends illustrates that there is a mix of both positive and negative results stemming from this action.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strategic_deep_sea_rare_earth_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(strategic_deep_sea_rare_earth_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The initiative extracts from deep-sea ecosystems and competing suppliers. However, it also provides benefits to Japan in terms of resource security and economic growth. Suppression (0.65): Moderate-High. High suppression of environmental regulation concerns initially, moderate in terms of competitors because they are constrained by the technological lead and geopolitical position of Japan. Theater ratio (0.30): Low. The initiative has a relatively low theater ratio, focusing on actual resource extraction rather than performative aspects. The main goal is to access rare earth minerals; therefore, the operational focus has less theater involved.
 *
 * PERSPECTIVAL GAP:
 *   The deep-sea ecosystems perspective views the initiative as a pure snare because they are trapped and face irreversible damage. Competing rare earth suppliers experience it as a tangled rope, as they are both constrained and potentially benefit from increased demand. The Japanese government and mining companies see it as a rope, securing a stable resource supply and economic opportunities. The analytical observer sees the initiative as a tangled rope, balancing the benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent. The Japanese government, as the primary beneficiary, has a low d value. Deep-sea ecosystems, as the primary victim, have a high d value. Competing suppliers have a moderate d value, reflecting their mixed position. The analytical observer seeks to balance these competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    environmental_impact_severity,
    'How severe and long-lasting will the environmental damage from deep-sea mining be?',
    'Long-term monitoring of mined areas, ecological studies, and development of mitigation technologies.',
    'If severe, the constraint shifts towards a snare with greater global societal costs. If minimal, the constraint remains a tangled rope or potentially even a rope if environmental mitigation is effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_impact_severity, empirical, 'Assessing the long-term environmental impact of deep-sea mining.').

omega_variable(
    resource_abundance_and_alternatives,
    'Are there viable alternative sources of rare earth minerals or substitutes that could reduce the need for deep-sea mining?',
    'Exploration of new land-based deposits, research into material substitutes, and development of recycling technologies.',
    'If alternatives are found, the justification for deep-sea mining diminishes, potentially leading to its abandonment or stricter regulation. If not, the extraction pressure remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_abundance_and_alternatives, empirical, 'Availability of alternative rare earth sources or substitutes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strategic_deep_sea_rare_earth_mining, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stra_tr_t0, strategic_deep_sea_rare_earth_mining, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stra_tr_t5, strategic_deep_sea_rare_earth_mining, theater_ratio, 5, 0.25).
narrative_ontology:measurement(stra_tr_t10, strategic_deep_sea_rare_earth_mining, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(stra_be_t0, strategic_deep_sea_rare_earth_mining, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stra_be_t5, strategic_deep_sea_rare_earth_mining, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(stra_be_t10, strategic_deep_sea_rare_earth_mining, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strategic_deep_sea_rare_earth_mining, resource_allocation).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, global_rare_earth_supply_chain).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, deep_sea_environmental_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
