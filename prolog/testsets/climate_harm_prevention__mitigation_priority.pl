% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Harm Prevention via Mitigation Priority (Growth-Compatible Transition)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of the climate harm prevention kernel
 *   asserts that emissions reductions via technological transition
 *   (renewables, efficiency, electrification) can occur within a maintained
 *   growth framework, and that this transition should be prioritized over
 *   adaptation investment or degrowth restructuring. This reading creates a
 *   structural constraint because it binds climate policy to economic growth
 *   commitments, creating asymmetric costs: future generations and renewable
 *   energy sectors benefit; present-generation workers in carbon industries
 *   and global poor populations bear concentrated transition costs. The
 *   constraint exhibits Tangled Rope structure: genuine coordination function
 *   (accelerating decarbonization requires technological coordination,
 *   capital deployment, and sectoral transformation) coexists with asymmetric
 *   extraction (costs are front-loaded on vulnerable populations, benefits
 *   are diffuse and long-term). The theater ratio (0.68) reflects growing gap
 *   between decarbonization targets and actual policy mechanisms: official
 *   commitments to rapid decarbonization are frequently unsupported by
 *   demand-side policies, just-transition funding, or degrowth
 *   acknowledgment. The extractiveness trajectory (0.35 → 0.58 over interval)
 *   shows accumulating extraction as mitigation policies intensify without
 *   corresponding just-transition infrastructure, creating widening
 *   cost-concentration on trapped populations.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary beneficiary (organized representation via climate NGOs, youth movements) — primary beneficiary of emissions reductions; benefits accrue across 50+ year horizon
 *   - Renewable Energy Sector: Institutional beneficiary (institutional/arbitrage) — gains capital deployment, policy support, market creation from mitigation priority
 *   - Climate Finance Institutions: Institutional beneficiary (institutional/arbitrage) — gains mandates, political support, capital deployment opportunities
 *   - Coal and Carbon-Intensive Workers: Primary victim (powerless/trapped) — geographically immobile, skill-locked, politically isolated; bear immediate transition costs with minimal voice in policy
 *   - Global Poor in Transition Economies: Primary victim (powerless/trapped) — bear disproportionate costs through energy poverty, subsidy withdrawal, carbon pricing without compensation
 *   - Mid-Career Transition Workers: Secondary victim (moderate/constrained) — face retraining costs and regional disruption but some mobility and policy support access
 *   - Incumbent Fossil Fuel Companies: Mixed (powerful/constrained) — experience stranded assets and demand destruction but retain coordination opportunities in transition
 *   - Just Transition Coalition: Organized advocates (organized/constrained) — pushing for worker and climate-poor protection; see extraction as solvable through policy design
 *   - Growth-Compatible Decarbonization Narrative: Institutional institution (institutional/arbitrage) — maintains the frame that grounds mitigation priority as compatible with growth; increasingly performative as targets tighten
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Harm Prevention via Mitigation Priority (Growth-Compatible Transition)").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'ae44ef2e-19a7-45af-bcf2-1a91e9557f38').
narrative_ontology:cs_kernel_codification('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', formalized).
narrative_ontology:cs_authority_grounding('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', distributed).
narrative_ontology:cs_reading_relation('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', foundational, growth_compatible_decarbonization).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', growth_compatible_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', foundational, mitigation_priority_over_adaptation).
narrative_ontology:cs_axiom_status(mitigation_priority_over_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', mitigation_priority_over_adaptation, instrumental).
narrative_ontology:cs_reference_frame('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', emissions_reduction_via_technology).
narrative_ontology:cs_drift_state('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', contemporary_post_paris_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ae44ef2e-19a7-45af-bcf2-1a91e9557f38', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_finance_institutions).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_generation_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, global_poor_transition_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CARBON-DEPENDENT WORKERS (SNARE) — Trapped by geographic immobility, skill lock-in, and political inability to coordinate alternative economic transitions. Bear transition costs immediately while benefits accrue to future generations they may not live to see. Suppression is structural: economic dependency on carbon industries, lack of credible transition pathways, political isolation of worker interests in climate discourse.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL POOR IN TRANSITION ECONOMIES (SNARE) — Bear disproportionate costs of carbon pricing and industrial restructuring (energy poverty, job loss, subsidy withdrawal) while having minimal responsibility for historical emissions and minimal ability to influence policy. Trapped by economic dependency, geographic location, and lack of political voice. Maximum experienced extraction.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-CAREER TRANSITION SECTOR WORKERS (TANGLED ROPE) — Face significant retraining costs and regional unemployment but also benefit from coordination aspects: accessible retraining programs, geographic labor mobility (where available), emerging sectors creating new employment. Not fully trapped but constrained by education barriers, family obligations, and geographic immobility. Experience both extraction (transition burden) and coordination benefits (if transition support exists).
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE ENERGY SECTOR (ROPE) — Primary institutional beneficiary. Experiences the constraint as pure coordination: emissions pricing and mitigation targets create demand for their products and services. Access to capital, technology markets, and policy support. Net positive directionality — extraction flows toward this actor. No exit barriers; can arbitrage across jurisdictions and technologies.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE FINANCE INSTITUTIONS (ROPE) — Institutional beneficiaries with arbitrage. Mitigation priority creates large capital deployment opportunities, institutional mandates, and political support. Can exit to other sectors or jurisdictions. Experience the constraint as coordination: channeling capital to decarbonization aligns with institutional mission and financial opportunity.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT FOSSIL FUEL COMPANIES (TANGLED ROPE) — Face regulatory suppression and demand destruction but retain significant coordination function: their infrastructure (ports, distribution, capital) is repurposed; technology transfer to renewables; some mitigation pathways (carbon capture, renewable hydrogen) benefit fossil incumbents. Constrained by regulatory exposure but not eliminated — stranded assets are real extraction, but incumbent companies experience mixed dynamics: direct losses balanced against coordination opportunities in transition.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FUTURE GENERATIONS (ORGANIZED REPRESENTATION) (ROPE) — Primary beneficiary from mitigation priority. When represented by organized advocates (climate NGOs, indigenous groups, youth movements), future-generation interests appear as the coordination function the entire constraint exists to serve. Mobile in the sense that representation can shift political power. Low experienced extraction relative to beneficiary status — the constraint is structured to protect their interests.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: JUST TRANSITION COALITION (SCAFFOLD) — Organized actors (unions, development NGOs, community organizations) advocating for worker and climate-poor protection within the mitigation framework. See the extraction problem as temporary and solvable through policy design: job guarantees, retraining, geographic redistribution. Sunset logic: if transition support policies achieve sufficient coverage and speed, extraction is dampened and constraint becomes pure coordination. Theater is moderate — genuine coordination mechanisms exist (job training, sectoral bargaining) alongside performative commitment (targets without funding).
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: GROWTH-COMPATIBLE DECARBONIZATION (PITON) — The core claim that emissions can be reduced without degrowth or fundamental economic restructuring is increasingly performative as decoupling targets become more stringent. Theater ratio high (0.68) because: official growth continues to be claimed while energy intensity targets require deeper structural change; technology deployment is celebrated while demand-side policies remain politically impossible; carbon markets are portrayed as sufficient while relying on unproven carbon removal at scale. The narrative persists through institutional inertia — policymakers committed to growth framework maintain decarbonization theater to avoid confronting contradiction.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From thermodynamic universality, energy transition is a natural law: all mature economies undergo energy transitions (wood to coal, coal to oil/gas, gas+nuclear to renewables). The constraint appears inevitable. However, structural data reveals this as a false summit: the 'inevitability' naturalizes the contingent timing, distributional choices, and political resistance to fastest transition. The mountain claim obscures that speed, equity, and cost distribution are all contingent political choices, not natural laws.
constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_harm_prevention__mitigation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mitigation-priority reading creates immediate extraction from workers and global poor (concentrated costs, minimal compensation) while distributing benefits across future generations and institutional beneficiaries. The extractiveness is not maximum (0.72+) because genuine coordination mechanisms exist (renewable deployment is technically feasible, sectoral transformation is possible), but extraction is substantial because distributional choices (front-loading costs on trapped populations, promising compensation without funding) are built into the constraint. The upward trajectory (0.35 → 0.58) reflects intensifying mitigation policies without corresponding just-transition support — extraction accumulates. Suppression (0.62): High. Multiple suppression mechanisms operate: geographic immobility of fossil fuel workers, economic dependency on carbon industries, political underrepresentation of affected workers in climate discourse, absence of credible alternative employment pathways, global-poor lack of voice in policy design. Suppression is structural (barriers to exit) rather than purely internalized. Theater ratio (0.68): Moderate-high. The growth-compatible decarbonization narrative is increasingly performative: official growth commitments are maintained while decarbonization requires deeper structural change; carbon markets are celebrated as sufficient while relying on unproven carbon removal; technology is portrayed as the primary solution while demand-side policies remain politically impossible. Theater has increased over interval as gap between targets and actual mechanisms has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across power levels. Trapped workers see a Snare — pure extraction with no coordination benefit and no exit option. The renewable energy sector sees a Rope — pure coordination with beneficiary positioning. The just-transition coalition sees a Scaffold with sunset — a temporary problem solvable through policy design. The growth-compatible decarbonization narrative sees itself as a natural law (Mountain/false summit) — inevitable energy transition — but structural data reveals this as naturalization of contingent timing and distributional choices. Future generations (organized) see coordination, but future generations (unorganized) experience extraction. The perspectival gap reveals that the constraint's classification depends entirely on one's structural position: beneficiary or victim, trapped or mobile, powerless or institutional. The analytical observer identifies the false summit: the 'naturalness' of growth-compatible mitigation obscures that it is a specific political choice to front-load costs on the vulnerable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value d is derived from their power level, exit options, and beneficiary/victim status. Trapped powerless victims (coal workers, global poor) have highest d (→ 0.95) approaching full targets. Constrained moderate victims have moderate d (→ 0.65). Institutional beneficiaries with arbitrage have lowest d (→ 0.05) approaching full beneficiaries. The mitigation-priority reading's core distributional choice is to maximize benefits for future generations (ultimate beneficiaries, d near 0.0) while minimizing present-generation compensation for trapped workers (victims, d near 1.0). The constraint's structure front-loads extraction on those with least mobility and least political voice. The false summit (mountain) perspective uses analytical d (→ 0.72), which is appropriate because the analytical observer can see the contingency that trapped agents cannot: the 'naturalness' is constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   The mitigation-priority reading resolves mandatrophy by acknowledging that it is one reading of the climate harm prevention kernel, not the unique correct interpretation. The reading is defensible on empirical grounds (decoupling is technically feasible) and on philosophical grounds (protecting future generations is a legitimate priority). But the reading does NOT deny that cost distribution choices are embedded in its structure. The mandatrophy resolution requires: (1) acknowledging the adaptation-priority and degrowth-reading alternatives coexist as live policy positions; (2) recognizing that 'harm prevention via mitigation priority' IS extraction from present-generation workers and global poor, not just coordination for future benefit; (3) accepting that the growth-compatible framing is a specific distributional choice, not a natural law. The constraint is classified as Tangled Rope (not Rope + fake coordination, not Snare + fake future benefit) because both genuine coordination and genuine extraction coexist in the structure. The just-transition coalition's scaffold perspective represents the reading's internal attempt to resolve extraction — if adequate worker and climate-poor protection is achieved, the constraint could transition to pure coordination. The theater ratio indicates this resolution is incomplete: protection is promised more often than funded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_empirical_status,
    'Can carbon emissions be decoupled from economic growth at the speed and scale required (50%+ reduction by 2035) while maintaining growth rates compatible with current policy commitments?',
    'Longitudinal tracking of GDP growth vs emissions intensity in leading-transition economies (Denmark, Costa Rica, Germany). Cross-check against renewable energy deployment rates, energy demand elasticity, and rebound effects. Compare projected decoupling curves to actual trajectories.',
    'If decoupling is empirically sufficient: growth-compatible mitigation is a genuine coordination mechanism (Rope classification strengthens). If decoupling is insufficient: constraint reclassifies toward Snare — growth framework becomes extractive shell for postponing deeper restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_empirical_status, empirical, 'Feasibility of growth-compatible emissions decoupling at required scale and speed').

omega_variable(
    worker_transition_cost_distribution,
    'What fraction of transition costs are borne by fossil-fuel workers vs distributed across society through carbon pricing, taxation, and public investment?',
    'Comparative analysis of transition policies: wage replacement ratios in just-transition programs, retraining success rates, regional employment recovery timelines. Cross-jurisdictional comparison (Germany Energiewende, US coal regions, South Africa coal transition). Track actual vs promised funding for worker support.',
    'If >70% of costs are socialized: worker extraction is suppressed, classification shifts from Snare toward Tangled Rope for affected workers. If <30% socialized: extraction remains concentrated, Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_transition_cost_distribution, empirical, 'Distribution of transition costs between workers and broader society').

omega_variable(
    mitigation_vs_adaptation_priority_logical_relationship,
    'Does the mitigation-priority reading logically foreclose the adaptation-priority reading within climate policy frameworks, or do they coexist as alternative emphases?',
    'Analytical: examine whether mitigation and adaptation can both be pursued simultaneously at adequate scale, or whether resource constraints, institutional capacity, and political will create genuine mutual exclusion. Empirical: track policy commitments and funding ratios across jurisdictions — are mitigation and adaptation investments moving in tandem or inverse?',
    'If foreclose: reading_relations should be ''forecloses''. If coexist: ''coexists_with''. If influences (one creates pressure on the other): ''influences''. Classification of the logical relationship determines how the engine models the kernel''s internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_vs_adaptation_priority_logical_relationship, conceptual, 'Logical relationship between mitigation-priority and adaptation-priority readings').

omega_variable(
    degrowth_forecast_incompatibility,
    'If credible climate science indicates that required emissions reductions are incompatible with growth above 0-2% annually, does the mitigation-priority reading (committed to growth compatibility) become overridden by degrowth necessity?',
    'Monitor climate modeling consensus on carbon budgets vs projected global growth paths. Track whether mitigation-priority advocates shift position or introduce ''planned degrowth'' language when growth-compatible pathways become scientifically implausible.',
    'If growth incompatibility becomes dominant scientific consensus: the mitigation-priority reading''s foundational axiom (growth_compatible_decarbonization) transitions from ''holdable'' to ''overridden''. The reading persists but with acknowledged internal contradiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_forecast_incompatibility, empirical, 'Scientific trajectory of growth-decarbonization compatibility').

omega_variable(
    global_poor_cost_concentration,
    'Are transition costs concentrated on global poor through carbon pricing, industrial restructuring, and subsidy withdrawal in ways that exceed historical emissions responsibility?',
    'Comparative burden analysis: carbon-intensity of consumption by income quintile vs carbon tax/pricing burden; employment loss concentration in low-income regions; energy poverty correlation with carbon pricing regimes. Track whether mitigation prioritizes present-generation cost minimization over intergenerational equity.',
    'If cost concentration on global poor is severe: the reading''s beneficiary claim (future generations) is undermined by present-generation extraction from the vulnerable. Omega resolution would shift commentary toward acknowledgment that growth-compatible mitigation exports costs to those least responsible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_poor_cost_concentration, empirical, 'Concentration of transition costs on global poor and historically non-responsible populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_mitig_tr_t0, climate_harm_prevention__mitigation_priority, theater_ratio, 0, 0.52).
narrative_ontology:measurement(clim_mitig_tr_t5, climate_harm_prevention__mitigation_priority, theater_ratio, 5, 0.62).
narrative_ontology:measurement(clim_mitig_tr_t10, climate_harm_prevention__mitigation_priority, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(clim_mitig_be_t0, climate_harm_prevention__mitigation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_mitig_be_t5, climate_harm_prevention__mitigation_priority, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(clim_mitig_be_t10, climate_harm_prevention__mitigation_priority, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_mitig_su_t0, climate_harm_prevention__mitigation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(clim_mitig_su_t5, climate_harm_prevention__mitigation_priority, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(clim_mitig_su_t10, climate_harm_prevention__mitigation_priority, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, just_transition_wage_protection).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, carbon_pricing_regressive_burden).

% DUAL FORMULATION NOTE:
% The climate harm prevention kernel has three structurally distinct readings (mitigation_priority, adaptation_priority, degrowth_reading) with different ε values reflecting different empirical assumptions and distributional choices. This file instantiates mitigation_priority (ε=0.58, growth-compatible decarbonization). Sibling readings are separate constraints with their own ε values, base properties, and perspectives. The network edges reflect that these readings are not independent — mitigation priority creates structural pressure on adaptation (diverts funding, deprioritizes resilience) and forecloses degrowth (commits to growth compatibility). Downstream constraints (just_transition_wage_protection, carbon_pricing_regressive_burden) inherit the distributional structure of the mitigation-priority reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
