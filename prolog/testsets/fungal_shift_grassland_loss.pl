% ============================================================================
% CONSTRAINT STORY: fungal_shift_grassland_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fungal_shift_grassland_loss, []).

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
 *   constraint_id: fungal_shift_grassland_loss
 *   human_readable: Ecological Transition from Grassland to Scrub via Fungal Network Disruption
 *   domain: ecological/climate/soil_biology
 *
 * SUMMARY:
 *   Warming winters in mountain grassland ecosystems disrupt the soil
 *   mycorrhizal fungal networks upon which grassland plant communities
 *   depend. Stable snowpack maintains soil temperatures and moisture
 *   conditions that favor arbuscular mycorrhizal fungi — species that form
 *   obligate partnerships with grassland roots for nutrient uptake and
 *   drought tolerance. Reduced snow cover exposes soil to greater winter
 *   temperature fluctuations and desiccation, favoring drought-adapted
 *   saprotrophic and ectomycorrhizal fungi associated with scrub vegetation.
 *   Over 30-50 years, this fungal community shift cascades into irreversible
 *   grassland loss as scrub species outcompete grassland plants for
 *   resources. The constraint exhibits characteristics of a pure extraction
 *   mechanism (Snare) from the perspective of grassland organisms and
 *   dependent fauna, which bear ecological costs with no exit option.
 *   However, from institutional and pastoral perspectives, it exhibits mixed
 *   characteristics: conservation sectors benefit from research opportunities
 *   while being constrained by inability to reverse the transition; pastoral
 *   communities face reduced carrying capacity but possess some adaptive
 *   mobility. The theater ratio (0.38) reflects that early policy responses
 *   (grassland protection, restoration initiatives) operate at cross-purposes
 *   with the underlying driver (climate forcing) and become increasingly
 *   performative as they fail to halt the transition.
 *
 * KEY AGENTS:
 *   - Grassland Plant Communities: Primary victim (powerless/trapped) — lose fungal partnerships required for survival; no geographic or metabolic exit; forced competitive disadvantage against scrub species
 *   - Grassland-Dependent Fauna: Primary victim (powerless/trapped) — birds, insects, small mammals dependent on grassland structure cannot adapt rapidly enough to scrub conversion; limited migratory capacity
 *   - Pastoral Communities: Secondary victim (moderate/mobile) — face reduced pasture carrying capacity and income but possess partial exit options through herd migration, livestock composition shifts, economic diversification
 *   - Conservation and Ecological Science Sector: Dual agent (organized/constrained) — benefits from research funding and expertise valuation (beneficiary) while constrained by inability to prevent ecosystem change and outpacing of knowledge by events (victim)
 *   - Agricultural Policy Institutions: Institutional beneficiary (institutional/arbitrage) — maintain grassland-focused subsidies and regulations that persist through inertia despite becoming maladapted; possess exit options to shift policy frameworks
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing climate-driven constraint as ecological natural law, obscuring human causation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fungal_shift_grassland_loss, 0.52).
domain_priors:suppression_score(fungal_shift_grassland_loss, 0.68).
domain_priors:theater_ratio(fungal_shift_grassland_loss, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, extractiveness, 0.52).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fungal_shift_grassland_loss, snare).
narrative_ontology:human_readable(fungal_shift_grassland_loss, "Ecological Transition from Grassland to Scrub via Fungal Network Disruption").
narrative_ontology:topic_domain(fungal_shift_grassland_loss, "ecological/climate/soil_biology").

% --- Structural relationships ---
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, grassland_plant_communities).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, grassland_dependent_fauna).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, human_pastoral_economies).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, regional_biodiversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRASSLAND PLANT COMMUNITIES (SNARE) — Cannot exit the altered soil mycorrhizal environment. Warm-winter conditions disrupt arbuscular mycorrhizal fungal networks that grassland species depend upon for nutrient uptake and water access. Loss of fungal partnerships forces competitive disadvantage against drought-tolerant scrub species. No alternative metabolic pathway; no mobility. Maximum experienced extraction — the constraint removes the biological foundation of their survival.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GRASSLAND-DEPENDENT FAUNA (SNARE) — Birds, insects, and small mammals dependent on grassland structure and plant diversity cannot adapt to rapid scrub conversion. No geographic exit available within their range before habitat converts. Migratory capacity is limited. Bears full cost of ecosystem shift with no agency or alternative habitat access.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PASTORAL COMMUNITIES (ROPE) — Experience the constraint as a coordination problem with partial exit options. Herding animals can migrate to higher elevations or shift to scrub-compatible livestock. Economic adaptation is costly but possible. Communities face reduced carrying capacity and income, but possess some agency through transhumance, herd composition shifts, or economic diversification. Effective extraction is moderate — they can partially escape.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: CONSERVATION AND ECOLOGICAL SCIENCE SECTOR (TANGLED ROPE) — Possess institutional capacity to study and document the transition, creating career opportunities in climate adaptation research and management. Benefit from the constraint through research funding and expertise valuation. Simultaneously constrained by rapid ecosystem change that outpaces intervention capacity and by the tragedy of documenting loss without preventing it. Active enforcement of conservation protocols (protected areas, restoration) often fails against the underlying driver (warmer winters). Exhibits both coordination function (monitoring networks, scientific knowledge) and asymmetric extraction (institutional benefit from studying ecosystem degradation).
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL AGRICULTURAL POLICY FRAMEWORKS (PITON) — Policy institutions (FAO, regional agricultural ministries) maintain subsidies and grazing regulations designed for stable grassland conditions. These policies persist despite becoming maladapted to the new ecological reality. Theater ratio is high: continued grassland-management regulations are largely performative — they cannot reverse fungal network disruption or restore grassland competitiveness. Institutions maintain the policy apparatus through inertia even as its functional basis erodes. Beneficiaries (large pastoral operations leveraging subsidies) have arbitrage options to shift to other regions or commodity types.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a thermodynamic and evolutionary perspective, the transition from grassland to scrub via fungal network disruption is a form of ecological phase transition. Stable snowpack thresholds create bistability: above the threshold, grassland fungal networks are favored; below it, scrub-adapted communities dominate. The transition is not reversible without restoring the snowpack — it is a structural reorganization of the soil-plant system. However, this perspective risks naturalizing what is fundamentally a climate-driven constraint imposed by human greenhouse gas forcing. The 'natural law' framing obscures the causal attribution.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fungal_shift_grassland_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fungal_shift_grassland_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fungal_shift_grassland_loss, TR),
    TR >= 0.70.

:- end_tests(fungal_shift_grassland_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint mechanisms include: (1) direct ecological extraction from grassland organisms — loss of fungal symbioses removes a core biological input with no alternative source; (2) temporal extraction from pastoral economies — carrying capacity declines over 20-40 year span, creating forced economic transition; (3) indirect extraction from conservation efforts — resources devoted to grassland protection yields diminishing returns as the underlying driver (snowpack loss) continues. The value (0.52) reflects that this is primarily an ecological constraint with significant human extractiveness components, but not maximum extraction because some agents (pastoral communities, conservation sector) retain partial agency and adaptation pathways. Suppression (0.68): High. Barriers to grassland persistence include: fundamental incompatibility of grassland plants with scrub-associated fungal networks; geographic constraints preventing migration to cooler elevations (space constraints); socioeconomic constraints on pastoral adaptation (capital requirements for livestock composition change, land tenure barriers, market access); institutional path dependence in agricultural policy. The snowpack threshold creates a bistability trap — once crossed, grassland recovery requires external intervention (snowpack restoration via climate policy) beyond the capacity of local land management. Theater ratio (0.38): Moderate-low. Initial policy responses (grassland protection areas, restoration initiatives) represent genuine coordination attempts but become progressively performative as their functional basis (stable snowpack) erodes. By year 40, continued grassland management policies are substantially theatrical — they maintain institutional roles and funding streams while failing to address the underlying driver. The low initial theater reflects that the constraint itself is genuinely ecophysiological (not socially constructed), but institutional responses progressively become performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classifications arising from structural asymmetries in exit options and beneficiary/victim status. Grassland organisms (Snare) have no exit; pastoral communities (Rope) have costly but real exit options; conservation institutions (Tangled Rope) benefit from documenting loss while being constrained by inability to prevent it; policy institutions (Piton) maintain increasingly performative grassland frameworks through institutional inertia. The analytical observer's thermodynamic perspective risks misclassifying a climate-driven constraint as an immutable ecological law, thereby obscuring the causal attribution to greenhouse gas emissions and policy choices. The key perspectival gap is between organisms experiencing the constraint in real time (immediate/biographical timeframes, regional scope) and policy institutions operating at civilizational timescales with global scope — the latter can rationalize grassland loss as inevitable while the former experience it as catastrophic extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the structural position of each agent relative to the fungal-network disruption mechanism. Grassland plants and dependent fauna have d ≈ 0.95 (full targets): they bear costs with no capacity to exit or benefit from the transition. Pastoral communities have d ≈ 0.55-0.65 (victims with exit options): they bear significant costs but possess adaptation pathways via migration and economic diversification. Conservation scientists have d ≈ 0.30-0.40 (mixed beneficiary and victim): they benefit from research opportunities and institutional funding while being constrained by inability to prevent ecosystem loss. Policy institutions have d ≈ 0.05-0.15 (beneficiaries): they maintain grazing subsidies and regulatory frameworks that concentrate benefits among large pastoral operations while dispersing costs across grassland organisms and small-scale pastoralists. No explicit directionality overrides required — the structural derivation captures the real asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PATHWAY: The constraint is classified as a Snare (ε=0.52, χ ≥ 0.66) for the powerless agents bearing primary costs (grassland organisms, dependent fauna). However, the classification is not uniform — it resolves mandatrophy by showing that the same ecological mechanism exhibits different constraint types depending on structural position. For grassland organisms, it is a Snare (no exit, maximum extraction). For pastoral communities, it is a Rope or Tangled Rope (partial exit, mixed extraction/coordination). For conservation institutions, it is a Tangled Rope (asymmetric extraction despite coordination function). For policy institutions, it is a Piton (degraded institutional framework). The mandatrophy is resolved by recognizing that 'the fungal shift grassland loss' is not a single homogeneous constraint but a structural phenomenon that generates different extraction/coordination dynamics depending on the agent's position within it. The snare classification for primary victims (grassland and fauna) is unambiguous. The institutional classifications differ because institutions possess buffers (funding, policy flexibility, geographic mobility of operations) that individual organisms lack. The constraint becomes a snare precisely at the point where exit options collapse — which occurs at different thresholds for different agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fungal_network_recovery_timeline,
    'Can arbuscular mycorrhizal fungal networks recover if winters re-stabilize, or has the community state shifted irreversibly?',
    'Long-term snowpack restoration experiments; seed bank and fungal spore bank viability assays; paleoecological reconstruction of post-recovery scenarios',
    'If reversible: constraint is a temporary Scaffold with sunset tied to climate policy. If irreversible: constraint is a permanent Mountain-like transition with no recovery pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fungal_network_recovery_timeline, empirical, 'Whether fungal network collapse is reversible or represents permanent state change').

omega_variable(
    snowpack_threshold_specificity,
    'Is there a precise snowpack depth/duration threshold above which grassland fungi persist and below which scrub fungi dominate, or is the transition gradual and context-dependent?',
    'Controlled snowpack manipulation experiments; multivariate analysis of snowpack vs fungal community composition across elevation and geographic gradients; dynamic systems modeling',
    'If precise threshold exists: constraint is a Mountain with well-defined accessibility collapse. If gradual: constraint is a Tangled Rope with negotiable but costly transitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snowpack_threshold_specificity, empirical, 'Whether fungal transition exhibits sharp or gradual thresholds').

omega_variable(
    fungal_network_functional_redundancy,
    'Do scrub-associated fungal networks provide equivalent nutrient-acquisition and drought-resilience functions for grassland plant species, or is the incompatibility fundamental?',
    'Cross-inoculation experiments; nutrient uptake kinetics comparisons; hydraulic function analysis of grassland species with scrub fungal partners',
    'If functional redundancy exists: grassland species can persist in modified form (reduces snare severity). If incompatible: grassland species face obligate decline (confirms snare for perspective 1).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fungal_network_functional_redundancy, empirical, 'Whether alternative fungal networks can sustain grassland species').

omega_variable(
    human_adaptation_capacity_ceiling,
    'At what rate of grassland loss do pastoral adaptation strategies (migration, herd composition change, diversification) cease being effective, triggering systemic economic collapse?',
    'Ethnographic case studies of pastoral adaptation limits; economic modeling of carrying capacity thresholds; historical analysis of pastoral collapse events',
    'If adaptation ceiling is high: pastoral communities remain Rope (moderate extraction). If ceiling is low and rapidly approaching: communities move toward Snare (powerless/trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_adaptation_capacity_ceiling, empirical, 'Human adaptive capacity ceiling for pastoral economies under grassland loss').

omega_variable(
    policy_framework_inertia_drivers,
    'Are agricultural policies maintained by institutional path dependence, vested interests in grassland subsidies, or genuine belief in grassland management viability?',
    'Policy audit and legislative history analysis; stakeholder interviews; budget flow analysis for grassland vs scrub-adapted programs',
    'If path dependence only: Piton classification is correct. If vested interests: reveals Snare extraction by policy beneficiaries at expense of pastoral communities. If genuine belief: policy is misclassified as Piton and should be Tangled Rope (enforcement + learning).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_framework_inertia_drivers, conceptual, 'Root cause of maladapted policy persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fungal_shift_grassland_loss, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fungal_tr_t0, fungal_shift_grassland_loss, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fungal_tr_t20, fungal_shift_grassland_loss, theater_ratio, 20, 0.28).
narrative_ontology:measurement(fungal_tr_t40, fungal_shift_grassland_loss, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(fungal_be_t0, fungal_shift_grassland_loss, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fungal_be_t20, fungal_shift_grassland_loss, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(fungal_be_t40, fungal_shift_grassland_loss, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fungal_shift_grassland_loss, global_infrastructure).
narrative_ontology:affects_constraint(fungal_shift_grassland_loss, snowpack_decline_mountain_hydrology).
narrative_ontology:affects_constraint(fungal_shift_grassland_loss, climate_driven_ecosystem_state_change).

% DUAL FORMULATION NOTE:
% The fungal network disruption is downstream of climate forcing (snowpack decline). The upstream constraint is the climate system's thermodynamic tendency to reduce stable winter precipitation in this region. The fungal shift is the mechanism through which the climate constraint is translated into ecological extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
