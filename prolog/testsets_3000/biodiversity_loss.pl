% ============================================================================
% CONSTRAINT STORY: biodiversity_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biodiversity_loss, []).

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
 *   constraint_id: biodiversity_loss
 *   human_readable: Biodiversity Loss as Extractive Constraint
 *   domain: ecology/environmental_systems/political_economy
 *
 * SUMMARY:
 *   Biodiversity loss represents a structural extraction mechanism operating
 *   at planetary scale, where beneficiary groups (agricultural
 *   intensification, extractive resource industries, short-term capital)
 *   capture value by externalizing ecological costs onto species populations,
 *   indigenous communities, and future generations. The constraint exhibits
 *   characteristics across all six DR types depending on the observer's
 *   structural position and time horizon. For non-human species and trapped
 *   indigenous communities, it is a pure snare: no exit options, maximum
 *   experienced extraction, high suppression through property-rights
 *   enforcement and displacement. For industrial agricultural and extractive
 *   actors, it is a tangled rope: genuine coordination benefits (operational
 *   simplification through monoculture, habitat conversion enabling capital
 *   deployment) combined with asymmetric extraction (future ecosystem-service
 *   collapse, pollinator decline, soil degradation). For conservation
 *   institutions with arbitrage options, it appears as rope: a coordination
 *   problem solvable through data standardization and funding mobilization.
 *   For environmental movements, it is a scaffold: a temporary problem with a
 *   sunset clause if policies shift toward regenerative agriculture and
 *   indigenous land rights. For international environmental governance, it is
 *   a piton: a degraded institutional framework maintained through inertia
 *   despite 50+ years of failed conservation targets. The analytical view
 *   risks naturalizing this extraction as an immutable law of ecology—a false
 *   mountain that obscures the contingent institutional arrangements
 *   (agricultural subsidies, property rights, capital concentration) that
 *   structure the extraction. The constraint's theater ratio (0.58) reflects
 *   moderate performative content: international conservation frameworks
 *   (CBD, CITES, protected-area declarations) maintain symbolic compliance
 *   while failing to halt species loss; meanwhile, agricultural productivity
 *   claims mask subsidized monoculture intensification as technological
 *   necessity.
 *
 * KEY AGENTS:
 *   - Non-human species and ecosystems: Primary victims (powerless/trapped) — no exit options from habitat loss, pollution, climate-driven niche collapse
 *   - Indigenous and subsistence communities: Primary victims (powerless/trapped) — trapped by economic dependency and geographic location; dispossessed of traditional land rights
 *   - Future generations: Secondary victims (moderate/constrained/temporal exclusion) — bear long-term extraction costs (species extinction, reduced bioprospecting, ecosystem-service collapse)
 *   - Agricultural intensification actors: Primary beneficiaries (powerful/mobile) — capture operational simplification and capital deployment value; also experience extraction from ecosystem-service decline
 *   - Extractive resource industries: Primary beneficiaries (powerful/mobile) — capture habitat conversion value; externalize degradation costs
 *   - Conservation institutions: Secondary beneficiaries (institutional/arbitrage) — institutional funding and career pathways; arbitrage options allow reframing biodiversity as 'natural capital' and ecosystem services
 *   - Environmental and indigenous-rights movements: Organized challengers (organized/constrained) — see sunset pathway through regenerative agriculture and indigenous sovereignty restoration
 *   - International environmental governance: Institutional maintenance (institutional/arbitrage) — perpetuates degraded conservation framework through institutional inertia despite failed objectives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biodiversity_loss, 0.68).
domain_priors:suppression_score(biodiversity_loss, 0.72).
domain_priors:theater_ratio(biodiversity_loss, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biodiversity_loss, extractiveness, 0.68).
narrative_ontology:constraint_metric(biodiversity_loss, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biodiversity_loss, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biodiversity_loss, snare).
narrative_ontology:human_readable(biodiversity_loss, "Biodiversity Loss as Extractive Constraint").
narrative_ontology:topic_domain(biodiversity_loss, "ecology/environmental_systems/political_economy").

domain_priors:requires_active_enforcement(biodiversity_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biodiversity_loss, agricultural_intensification_actors).
narrative_ontology:constraint_beneficiary(biodiversity_loss, extractive_resource_industries).
narrative_ontology:constraint_beneficiary(biodiversity_loss, short_term_capital_beneficiaries).
narrative_ontology:constraint_victim(biodiversity_loss, species_populations).
narrative_ontology:constraint_victim(biodiversity_loss, indigenous_communities).
narrative_ontology:constraint_victim(biodiversity_loss, future_generations).
narrative_ontology:constraint_victim(biodiversity_loss, ecological_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPECIES AND ECOSYSTEMS (SNARE) — Organisms cannot exit habitat loss, pollution, or climate-driven niche collapse. Bears full extraction cost with zero alternatives. Classification driven by: trapped exit (no mobility), powerless agent status, and global scope amplifying suppression. This is the clearest snare reading — maximum experienced extraction, no coordination benefit, pure extraction mechanism.
constraint_indexing:constraint_classification(biodiversity_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS AND SUBSISTENCE COMMUNITIES (SNARE) — Structurally trapped by economic dependency on intact ecosystems; trapped by geographic location in biodiverse regions now targeted for extraction; trapped by lack of institutional power to negotiate or resist. No arbitrage option. Extraction runs toward industrial agricultural/extractive actors; suppression operates through legal/economic barriers and displacement. See omega: indigenous_knowledge_instrumentalization.
constraint_indexing:constraint_classification(biodiversity_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FUTURE GENERATIONS (SNARE) — Cannot exit, cannot consent, cannot negotiate. Constrained not by current material barriers but by temporal exclusion from the decision-making process. Bears 100% of long-term extraction (ecosystem collapse, species extinction, reduced bioprospecting potential). Moderate power through intergenerational framing ('leave a livable planet') but power is institutional and abstract, not concrete.
constraint_indexing:constraint_classification(biodiversity_loss, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: AGRICULTURAL AND EXTRACTIVE INDUSTRIES (TANGLED ROPE) — Primary beneficiaries with mobile exit options (can relocate operations, shift crops, diversify); powerful institutional actors. Experience genuine coordination benefit from biodiversity loss: monoculture standardization reduces management complexity, habitat conversion enables capital deployment, genetic uniformity (GMO crops) increases operational predictability. ALSO experience extraction: climate regulation loss, pollinator collapse, soil degradation, and long-term ecosystem-service decline impose costs. High suppression (governments actively enforce land-use conversion, subsidize extraction, suppress indigenous land claims) but suppression is functional rather than purely extractive — it coordinates the extraction pipeline. Extraction ≥ 0.30, suppression ≥ 0.40, active enforcement present. All three Tangled Rope gates satisfied.
constraint_indexing:constraint_classification(biodiversity_loss, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSERVATION INSTITUTIONS (ROPE) — Institutional actors with arbitrage options (can shift funding, reframe conservation as 'natural capital,' monetize biodiversity through carbon credits and ecosystem-services markets). See the biodiversity loss constraint as a pure coordination problem: standardizing metrics (species richness indices), sharing data (IUCN Red List, GBIF), coordinating research, mobilizing funding. Low extraction experienced (high institutional power, high exit options) because the conservation framing extracts value toward conservation organizations themselves. Beneficiaries from institutional funding and career pathways in conservation science.
constraint_indexing:constraint_classification(biodiversity_loss, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL MOVEMENTS (SCAFFOLD) — Organized coalitions (NGOs, indigenous rights groups, climate activists) perceive biodiversity loss as a temporary problem with a sunset: transition to regenerative agriculture, protected-area networks, indigenous land-rights restoration, and circular economy models are building alternative pathways. Sunset logic: if biodiversity policy shifts from extraction-compatible (habitat offsets, greenwashing) to extraction-incompatible (wilderness protection, indigenous sovereignty), the snare structure collapses. Constrained exit (movements face legal suppression, funding scarcity, state violence) but organized collective agency creates pathway visibility. Theater moderate (some performative conservation, but also genuine enforcement of protected areas and land rights).
constraint_indexing:constraint_classification(biodiversity_loss, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ENVIRONMENTAL GOVERNANCE PITON (PITON) — International frameworks (CBD, CITES, Ramsar) maintain conservation governance through institutional inertia and theater. Primary function (halting biodiversity loss globally) has largely failed: species extinction rates 100-1000× background; protected areas cover 17% of land but 80% lack enforcement funding; enforcement budgets stagnate while poaching and illegal logging accelerate. Governance persists through: (1) symbolic compliance (countries declare protected areas but permit extraction within them), (2) funding dependence (NGOs and governments locked into conservation-industrial complex), (3) narrative maintenance ('we're making progress'). Theater ratio high. Effective function degraded. Mechanism: institutional actors have arbitrage (can pivot to other conservation mechanisms) but maintain the old framework due to sunk costs and career paths.
constraint_indexing:constraint_classification(biodiversity_loss, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN-CLAIM) — From a universalizing analytical view, biodiversity loss appears as an immutable consequence of population growth and resource consumption: 'species extinction is inevitable under anthropogenic pressure,' 'habitat conversion is the cost of civilization,' 'trade-offs are inherent.' This naturalizes what the structural data reveals as a contingent extraction mechanism. The engine's false summit detector will flag this perspective as masking institutional arrangements (property rights enforcement, agricultural subsidies, capital concentration, indigenous land dispossession) under claims of biological inevitability. NOT a true mountain.
constraint_indexing:constraint_classification(biodiversity_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biodiversity_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biodiversity_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biodiversity_loss, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biodiversity_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biodiversity_loss, TR),
    TR >= 0.70.

:- end_tests(biodiversity_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting that primary beneficiary groups capture significant value while costs are externalized to species, ecosystems, and marginalized human populations. Trajectory shows increasing extractiveness over the 75-year interval (0.35→0.68) as agricultural intensification accelerates, habitat conversion deepens, and species extinction rates rise—this is accumulation of rent-extraction layered onto coordination function. Suppression (0.72): High, driven by legal enforcement of property rights (which prioritizes extractive actors' claims over species habitat or indigenous territory claims), subsidies that make monoculture profitable despite ecological costs, and institutional barriers to indigenous land management autonomy. Theater ratio (0.58): Moderate-high, reflecting that conservation governance and agricultural productivity claims maintain symbolic legitimacy ('protecting biodiversity,' 'sustainable agriculture') while structural extraction accelerates. Trajectory shows increasing theater (0.38→0.58) as gap widens between conservation rhetoric and extinction reality—governance increasingly performative. Claimed type Snare justified: beneficiaries with enforcement power, victims without exit options, high suppression, extractive asymmetry, no genuine coordination function (agricultural/resource extraction coordination happens despite biodiversity loss, not because of it).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural phenomenon produces radically different perceived types. The gap reveals the extraction mechanism: beneficiaries with exit options perceive rope or scaffold; trapped agents perceive snare; degraded institutions perceive piton; analytical observers risk falsely naturalizing as mountain. The perspectival gap is not epistemic disagreement—it is structural asymmetry. Different agents have different actual options, different power levels, different cost-bearing capacity. The constraint exploits this asymmetry. Harmonizing perspectives (claiming all agents 'really' see the same type) would obscure the asymmetry that IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural relationship to the extraction flow. Species and indigenous communities: d ≈ 1.0 (full targets), trapped exit + powerless + victim status → maximum d → maximum f(d) → maximum experienced extractiveness. Agricultural/extractive industries: d ≈ 0.55-0.65 (asymmetric beneficiaries with partial extraction costs), mobile exit + powerful + beneficiary status → moderate-low d → moderate f(d), but also bearing future ecosystem-service costs (soil degradation, pollinator collapse, climate regulation loss) → upward adjustment in experienced chi. Conservation institutions: d ≈ 0.10-0.20 (beneficiaries with arbitrage), institutional + arbitrage exit → very low d → negative f(d) → institutional chi is coordination-cost only (data infrastructure, funding, capacity building). Environmental movements: d ≈ 0.55 (organized challengers with constrained exit and victim status), organized + constrained + victim perception → moderate d → moderate f(d). International governance: d ≈ 0.15-0.25 (institutional beneficiaries maintaining old framework), institutional + arbitrage (can shift to new governance models) → low d → low chi. Analytical observer: d ≈ 0.72 (analytical stance implies exposure to all perspectives) → indicates observer cannot escape the constraint-structure perspective despite analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε = 0.68 > 0.70 nominal threshold; actual 0.68 falls just below but constraint shows all high-extraction characteristics requiring resolution): Biodiversity loss avoids mandatrophy collapse by clearly separating genuine coordination functions from extraction. Agricultural intensification and resource extraction have REAL coordination benefits (operational simplification, capital deployment efficiency, supply-chain standardization). But these coordination functions are ASYMMETRIC and ENFORCED: benefits accrue to industrial actors; costs (ecosystem-service loss, species extinction, land dispossession) accrue to species, indigenous communities, and future generations. Tangled rope classification for beneficiary perspective is diagnostic: the constraint simultaneously coordinates extraction and extracts value. The snare classification for trapped agents is equally diagnostic: the same structural phenomenon is purely extractive from their standpoint because they bear costs with zero benefit and no exit. The constraint is not misclassified as coordination—it is correctly identified as a hybrid (tangled rope for beneficiaries, snare for victims, piton for degraded governance). The mandatrophy resolves by recognizing that high extractiveness combined with genuine-but-asymmetric coordination function and high suppression is the defining signature of tangled rope at the beneficiary position and snare at the victim position. The system is not pretending to be pure coordination; it is structurally hybrid with sharply asymmetric distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regeneration_vs_restoration_threshold,
    'Is biodiversity loss reversible through restoration within human timescales, or does extinction represent an irreversible extraction?',
    'Longitudinal ecosystem recovery data from protected areas and restored habitats; comparison of pre-extraction vs restored biodiversity metrics; paleontological analysis of recovery timescales after extinction events.',
    'If fully reversible within 50-100 years: biodiversity loss is a snare with a potential sunset (scaffold logic becomes operative). If irreversible for thousands of years: extraction is permanent and snare classification hardens into eternal structural lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regeneration_vs_restoration_threshold, empirical, 'Whether biodiversity loss is reversible within human timescales').

omega_variable(
    indigenous_knowledge_instrumentalization,
    'Does conservation support for indigenous land management represent genuine cede of decision-making power, or extraction of indigenous knowledge into conservation-industry frameworks?',
    'Analysis of land-management autonomy: Do indigenous communities control land-use decisions, or do conservation organizations retain veto power? Payment flows: Do conservation payments match opportunity costs? Decision precedence: When conservation goals conflict with indigenous resource needs, whose preference prevails?',
    'If genuine autonomy: indigenous communities move from snare (trapped) to constrained or mobile, reducing suppression. If instrumentalization: extractive logic deepens — indigenous knowledge is harvested while communities remain trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_knowledge_instrumentalization, empirical, 'Whether conservation support achieves indigenous autonomy or extracts knowledge').

omega_variable(
    ecosystem_service_monetization_capture,
    'Do payment-for-ecosystem-services (PES) mechanisms and biodiversity offsetting reduce extraction by internalizing environmental costs, or deepen extraction by converting commons into commodified rents?',
    'Comparative analysis: protected areas with PES vs without; tracking of offset compliance and additionality; measurement of income distribution (who captures payments vs who bears costs); evolution of property rights (does commodification enable dispossession of traditional commons users?).',
    'If PES reduces extraction: snare can decompose into rope + coordinate pricing. If PES deepens extraction: snare hardens—ecosystem-service markets become new extraction mechanism, concentrating value toward capital and away from subsistence users and species.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_service_monetization_capture, empirical, 'Whether ecosystem-services monetization reduces or deepens extraction').

omega_variable(
    agricultural_productivity_vs_biodiversity_tradeoff,
    'Is agricultural intensification''s biodiversity cost a necessary exchange (higher yield per hectare enables land sparing) or a manufactured tradeoff (alternatives like agroecology could achieve comparable yields with lower biodiversity cost)?',
    'Meta-analysis of yield-per-hectare for industrial vs agroecological farming systems under equivalent conditions; measurement of biodiversity impact per calorie produced; policy analysis of agricultural subsidies directing capital toward intensification.',
    'If necessary tradeoff: tangled_rope classification stands (genuine coordination function). If manufactured: benef actors are extracting artificially through subsidy capture, and classification hardens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_productivity_vs_biodiversity_tradeoff, empirical, 'Whether biodiversity-yield tradeoff in agriculture is necessary or manufactured').

omega_variable(
    tipping_point_irreversibility,
    'Have biodiversity loss mechanisms crossed ecological tipping points (pollinator collapse, soil microbiome degradation, ocean dead zones) beyond which biological recovery becomes impossible regardless of policy change?',
    'Dynamical systems analysis of ecosystem thresholds; identification of evidence for and against tipping-point crossing; measurement of restoration potential in degraded regions.',
    'If tipping points not crossed: snare is structurally contingent (exit via policy change remains possible). If tipping points crossed: snare classification may be understating severity—constraint becomes a quasi-mountain (the extraction outcome becomes inevitable regardless of beneficiary intent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_irreversibility, empirical, 'Whether biodiversity loss has crossed irreversible tipping points').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biodiversity_loss, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biodiv_theater_t0, biodiversity_loss, theater_ratio, 0, 0.38).
narrative_ontology:measurement(biodiv_theater_t25, biodiversity_loss, theater_ratio, 25, 0.52).
narrative_ontology:measurement(biodiv_theater_t50, biodiversity_loss, theater_ratio, 50, 0.58).
narrative_ontology:measurement(biodiv_theater_t75, biodiversity_loss, theater_ratio, 75, 0.61).

% Extraction over time
narrative_ontology:measurement(biodiv_extract_t0, biodiversity_loss, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(biodiv_extract_t25, biodiversity_loss, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(biodiv_extract_t50, biodiversity_loss, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(biodiv_extract_t75, biodiversity_loss, base_extractiveness, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biodiversity_loss, resource_allocation).
narrative_ontology:affects_constraint(biodiversity_loss, indigenous_land_dispossession).
narrative_ontology:affects_constraint(biodiversity_loss, agricultural_subsidy_capture).
narrative_ontology:affects_constraint(biodiversity_loss, climate_regulation_loss).
narrative_ontology:affects_constraint(biodiversity_loss, pollinator_collapse).

% DUAL FORMULATION NOTE:
% Biodiversity loss is upstream of multiple derived constraints: indigenous land dispossession (ε=0.75, Snare) depends on habitat destruction creating pressure to convert indigenous territories; agricultural subsidy capture (ε=0.62, Tangled Rope) is a mechanism that drives intensification; climate regulation loss (ε=0.58, Scaffold-degrading) is a cascading consequence; pollinator collapse (ε=0.71, Snare) is a specific extinction pathway. Each story decomposes a mechanistic pathway through which biodiversity loss operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biodiversity_loss, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
