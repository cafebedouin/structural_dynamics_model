% ============================================================================
% CONSTRAINT STORY: mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mitigation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mitigation_priority
 *   human_readable: Mitigation-First Climate Response: Technological Decoupling and Growth Preservation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate response legitimacy positions
 *   technological innovation and carbon pricing as the primary mechanisms for
 *   emissions reduction while preserving economic growth trajectories and
 *   consumption patterns in affluent nations. This constraint exhibits
 *   tangled rope structure: it coordinates a genuine collective good (climate
 *   stabilization through emissions reduction) while enabling asymmetric
 *   extraction (present affluent populations externalize transition costs to
 *   future generations and current vulnerable populations). The constraint's
 *   core claim is that decoupling economic growth from emissions is both
 *   physically feasible and normatively preferable to managed contraction or
 *   consumption restructuring. This reading is one of three contested
 *   framings of climate legitimacy, alongside adaptation-priority (which
 *   emphasizes present resilience and accepts longer emissions timelines) and
 *   degrowth-transformation (which treats growth-compatible decoupling as
 *   aspirational overreach and proposes managed consumption reduction). The
 *   extractiveness and theater metrics have risen over time (0.42 → 0.63 and
 *   0.55 → 0.72 respectively), indicating that as the mitigation-priority
 *   reading becomes institutionalized through carbon pricing and climate
 *   finance architecture, the performative element (accounting, offset
 *   verification, compliance theater) increases while the functional climate
 *   impact per unit institutional effort declines. This trajectory suggests
 *   the constraint may degrade from tangled rope toward piton if the
 *   decoupling bet fails and the institutional apparatus persists through
 *   inertia rather than function.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) — bear intergenerational extraction if decoupling fails; excluded from policy negotiation
 *   - Climate-Vulnerable Populations (Global South, island nations): Primary victim (powerless/trapped) — face present climate impacts deferred by temporal deferral logic in mitigation-priority framing
 *   - Affluent Consumer Populations (OECD): Secondary victim/beneficiary (organized/constrained) — coordinated around climate action while growth-preservation preserves consumption privilege asymmetrically
 *   - Transition Workers (Energy/Industrial Sectors): Secondary victim (moderate/constrained) — bear disproportionate adjustment costs; genuine transition support (rope element) mixed with asymmetric burden (extraction element)
 *   - Renewable Technology & Financial Capital: Primary beneficiary (institutional/arbitrage) — capital flows mandated by mitigation-priority reading; maximum arbitrage optionality
 *   - Carbon-Intensive Industries: Secondary beneficiary (institutional/constrained) — face emissions reduction requirements (constraint) but receive decades of phased timelines and carbon pricing revenue; operate with reduced exit options but preserved growth expectations
 *   - Carbon Pricing Architecture: Institutional actor (institutional/arbitrage) — manages the formal constraint; experiences own degradation (piton element) as theater rises and functional climate impact per institutional effort declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mitigation_priority, 0.58).
domain_priors:suppression_score(mitigation_priority, 0.62).
domain_priors:theater_ratio(mitigation_priority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(mitigation_priority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mitigation_priority, tangled_rope).
narrative_ontology:human_readable(mitigation_priority, "Mitigation-First Climate Response: Technological Decoupling and Growth Preservation").
narrative_ontology:topic_domain(mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mitigation_priority, '6a44345d-9200-48eb-98c9-8aba48788eb5').
narrative_ontology:cs_created_at('6a44345d-9200-48eb-98c9-8aba48788eb5', '').
narrative_ontology:cs_kernel_codification('6a44345d-9200-48eb-98c9-8aba48788eb5', distributed).
narrative_ontology:cs_authority_grounding('6a44345d-9200-48eb-98c9-8aba48788eb5', distributed).
narrative_ontology:cs_kernel_id(mitigation_priority, climate_response_legitimacy).
narrative_ontology:cs_reading_relation('6a44345d-9200-48eb-98c9-8aba48788eb5', adaptation_priority, influences).
narrative_ontology:cs_reading_relation('6a44345d-9200-48eb-98c9-8aba48788eb5', degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('6a44345d-9200-48eb-98c9-8aba48788eb5', foundational, technological_decoupling_feasible).
narrative_ontology:cs_axiom_status(technological_decoupling_feasible, holdable).
narrative_ontology:cs_axiom('6a44345d-9200-48eb-98c9-8aba48788eb5', foundational, growth_preservation_normatively_required).
narrative_ontology:cs_axiom_status(growth_preservation_normatively_required, holdable).
narrative_ontology:cs_reference_frame('6a44345d-9200-48eb-98c9-8aba48788eb5', growth_compatible_emissions_reduction).
narrative_ontology:cs_drift_state('6a44345d-9200-48eb-98c9-8aba48788eb5', contemporary_carbon_budget_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mitigation_priority, current_affluent_populations).
narrative_ontology:constraint_beneficiary(mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(mitigation_priority, technological_innovation_sector).
narrative_ontology:constraint_beneficiary(mitigation_priority, financial_capital_holders).
narrative_ontology:constraint_victim(mitigation_priority, future_generations).
narrative_ontology:constraint_victim(mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(mitigation_priority, workers_in_transition_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Trapped by intergenerational time structure. Cannot exit the constraint or negotiate terms. Bear full extraction cost if decoupling fails and emissions continue. No voice in the present-day policy bargaining. Maximum experienced suppression — the constraint is enforced through temporal asymmetry and demographic powerlessness. The mitigation-priority reading extracts from future agents by deferring the hard choices (phase-out timelines, consumption limits) to a later period when climate impacts may make adaptation more costly.
constraint_indexing:constraint_classification(mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Primarily located in Global South, island nations, and climate-sensitive regions. Powerless in the policy architecture that prioritizes growth preservation over immediate emissions reduction. Trapped by geography and economic dependency. Suppression operates through: lack of representation in carbon-pricing mechanisms, delayed adaptation funding, and the assumption that technological innovation will address harms they are already experiencing. The mitigation-priority reading extracts through temporal deferral — promises of future tech-fix impose present-day climate risks.
constraint_indexing:constraint_classification(mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: TRANSITION WORKERS (TANGLED ROPE) — Workers in coal, oil, gas, steel, cement sectors. Constrained by labor market mobility barriers, geographic dependence on regional industries, pension/benefit lock-in. The mitigation-priority reading coordinates genuine transition support and job creation in renewables (rope elements) while imposing significant extraction: wage suppression during transition, skill retraining costs, pension haircuts, geographic displacement pressure. The constraint has both coordination (enabling sectoral transition) and asymmetric extraction (workers bear disproportionate adjustment costs).
constraint_indexing:constraint_classification(mitigation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE TECHNOLOGY & FINANCIAL CAPITAL (ROPE) — Tech firms, venture capital, and institutional investors in renewable energy, carbon capture, battery tech, grid modernization. Experience the mitigation-priority reading as pure coordination: the constraint mandates massive capital flows toward decarbonization, enabling profitable innovation and scale. No extraction flows away from this agent — capital flows toward it. Exit is arbitrage: capital can move between clean-tech ventures, carbon markets, and traditional assets. This is the primary beneficiary with maximum arbitrage optionality.
constraint_indexing:constraint_classification(mitigation_priority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-EMISSION CONSUMER POPULATIONS (TANGLED ROPE) — Affluent populations in OECD nations. Coordinated by the constraint (carbon pricing, emissions reduction targets, renewable transition) around a shared collective good (climate stabilization) while extracting asymmetrically: consumption patterns and mobility privileges are preserved through growth-dependent framing, while lower-income strata bear disproportionate transition costs through energy bills, housing pressure, and job vulnerability. The constraint coordinates genuine climate action (rope element) while extracting through growth-preservation assumptions that defer equity costs.
constraint_indexing:constraint_classification(mitigation_priority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CARBON PRICING ARCHITECTURE (PITON) — The formal carbon trading systems, carbon offset markets, and emissions accounting frameworks. Theater ratio is high (0.68): much of the operational activity is procedural — MRV (measurement, reporting, verification), offset certification, compliance auditing — with lower functional climate impact. The architecture has degraded from its aspirational coordination function (creating price signals for emissions reduction) to maintenance through institutional inertia. Offset quality is contested, additionality is difficult to verify, and the system persists because alternative mechanisms haven't fully replaced it, not because it delivers climate outcomes at designed efficiency.
constraint_indexing:constraint_classification(mitigation_priority, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC LIMIT (MOUNTAIN) — From a universal/civilizational perspective, the constraint might appear as immutable: finite atmosphere and climate physics impose absolute biophysical limits on emissions. Decoupling from growth is a technical problem to be solved by innovation, not a policy choice. The reader is tempted to classify growth-compatible emissions reduction as a natural law — 'you must reduce emissions while maintaining growth, or you collapse the economy.' However, the structural data contradicts this: identified beneficiaries (capital, tech sector), victims (future generations, vulnerable populations), and enforcement requirements (policy mandate, carbon pricing) reveal that the mitigation-priority reading is a contingent political-economic choice, not a natural law. False summit indicator: beneficiaries present.
constraint_indexing:constraint_classification(mitigation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mitigation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mitigation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mitigation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mitigation_priority, TR),
    TR >= 0.70.

:- end_tests(mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mitigation-priority reading extracts from future generations by deferring hard choices (phase-out timelines, consumption restructuring) to later periods when climate impacts may constrain optionality. Current affluent populations externalize transition costs to both future agents and present vulnerable populations. However, extractiveness is not maximal (not ≥0.66 snare threshold) because the constraint does coordinate genuine emissions reduction and deploys capital toward decarbonization—the coordination function is real, not merely performative. The reading is tangled rope, not snare, because beneficiaries include both capital-holders (pure extraction) and the broader high-emission populations (mixed coordination and extraction). Suppression (0.62): Moderate-high. Suppression operates through multiple mechanisms: (1) temporal asymmetry—future generations cannot negotiate present-day policy; (2) geographic/economic dependency—vulnerable populations lack alternatives to carbon-intensive development; (3) labor market barriers—transition workers face retraining costs and regional lock-in; (4) policy architecture—the mitigation-priority reading is institutionalized through carbon pricing, renewable mandates, and climate finance commitments that define acceptable alternatives. Suppression is not total (≤0.60 for snare) because alternatives exist: degrowth proposals, adaptation-priority readings, rapid phase-outs. Theater ratio (0.68): Elevated and rising. Carbon pricing architecture, MRV systems, offset certification, and compliance frameworks have substantial performative content—procedural activity that demonstrates institutional commitment without proportional climate outcome. As the interval advances, theater rises (0.55 → 0.72) while base extractiveness rises more slowly, indicating increasing proceduralism relative to functional impact. This pattern is characteristic of piton degradation—the institutional apparatus persists through inertia while functional climate efficacy declines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint instantiates a fundamental perspectival asymmetry rooted in temporal and power structure. The primary beneficiary (capital, tech sector) has maximum arbitrage exit and low d → low χ → experiences the constraint as pure coordination (rope). The primary victims (future generations, vulnerable populations) have zero/minimal exit and high d → high χ → experiences the constraint as pure extraction (snare). Affluent populations occupy an intermediate position: genuinely coordinated around climate action (beneficiary status) but extraction embedded in growth-preservation assumptions (victim status for transition costs and future climate risk). The gap reveals that the mitigation-priority reading solves the coordination problem (emissions reduction) by deferring the distributional problem (who bears costs) to future time and vulnerable populations. From the beneficiary's analytical perspective, this is efficient coordination. From the victim's perspective, it is extraction with a temporal dimension.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain is core to explaining perspectival divergence. Beneficiaries with arbitrage options (capital, tech) experience low or negative extraction regardless of base ε—the constraint is net beneficial. Trapped agents (future generations, vulnerable populations) experience high extraction regardless of base ε—the constraint is net harmful. Constrained agents (transition workers, affluent consumers) experience moderate extraction based on their mixed beneficiary/victim status. The mitigation-priority reading structures the beneficiary set (current capital, high-emission populations) and victim set (future generations, climate-vulnerable now) such that experienced extractiveness varies by d from 0.08 to 0.95. This explains why unified policy action appears rational to capital (rope perspective, low χ) while appearing predatory to future agents (snare perspective, high χ). The directive asymmetry is not a measurement error—it is the structure of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mitigation-priority reading resolves mandatrophy by demonstrating that the constraint is genuinely tangled rope, not misclassified snare or rope. The coordination function (emissions reduction, renewable capital flows, global climate stabilization) is real—this is not pure extraction theater. However, the coordination is asymmetric: costs are borne by victims (future generations, vulnerable populations, transition workers) while benefits accrue to beneficiaries (capital, tech sector, affluent consumer populations). The constraint would be snare if the coordination function were absent—if mitigation delivered no actual emissions reduction and served only to extract value from vulnerable agents while preserving growth. It would be rope if extraction were absent—if costs and benefits were distributed equitably. The tangled rope classification captures the structural truth: both coordination and extraction are present, neither can be removed without destroying the other, and the reading's legitimacy depends on whether the coordination justifies the extraction asymmetry. The theater rise (0.55 → 0.72) indicates the constraint's integrity is degrading—as climate outcomes diverge from promises, institutional activity becomes more performative (MRV, offset verification, compliance) and less functionally climate-impactful. If theater reaches 0.75–0.80, the piton gate (theater_ratio ≥ 0.70) activates and the engine may reclassify the constraint as degraded institutional inertia rather than legitimate tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_empirical,
    'Can absolute decoupling of economic growth from emissions be achieved at the pace and scale required by climate targets (1.5°C, net-zero 2050)?',
    'Long-term empirical observation of emissions trajectories vs GDP growth in decarbonizing economies; analysis of historical decoupling rates vs required forward rates; assessment of hard-to-decarbonize sectors (cement, steel, aviation, agriculture)',
    'If decoupling is achievable: mitigation-priority reading is viable, extraction is temporary, future generations do not enter victim set in expectation. If decoupling is not achievable: extraction is permanent, future generations bear unavoidable climate cost, classification shifts toward snare across all agent perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_empirical, empirical, 'Whether absolute decoupling is physically achievable at required pace and scale').

omega_variable(
    technological_innovation_path_dependency,
    'Does prioritizing mitigation-first innovation create lock-in to technologies that later prove maladapted (carbon removal at excessive cost, renewable intermittency unresolved, adaptation gaps unclosed)?',
    'Historical analysis of technological lock-in (fossil fuel infrastructure, urban form); comparison of decarbonization pathways that maintain growth vs those requiring consumption restructuring; assessment of carbon removal cost curves and scalability',
    'If lock-in occurs: future generations face higher adaptation costs and reduced optionality. If path is flexible: future agents have capacity to course-correct. Determines whether extraction cost to future generations is temporary or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_innovation_path_dependency, empirical, 'Whether mitigation-first prioritization creates technological lock-in risks').

omega_variable(
    growth_preservation_assumption_validity,
    'Is the assumption that economic growth can be decoupled from material throughput and ecosystem extraction valid, or does it merely displace extraction to embodied carbon, supply chains, and ecological peripheries?',
    'Full-scope carbon accounting including embodied, supply-chain, and outsourced emissions; analysis of material throughput trends in decarbonizing economies; assessment of whether growth is purely ''dematerialized'' or appears dematerialized through accounting boundaries',
    'If growth truly decouples: the mitigation-priority reading has low extraction (balanced coordination). If decoupling is apparent only: the reading has high extraction (extraction displaced to vulnerable populations and future time), classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_preservation_assumption_validity, conceptual, 'Whether growth decoupling is real or accounting displacement').

omega_variable(
    intergenerational_equity_under_uncertainty,
    'Under deep uncertainty about climate impacts and technological capability, is deferring hard mitigation choices to future generations consistent with intergenerational justice, or does it extract by imposing asymmetric burden-shifting?',
    'Comparison of mitigation cost trajectories (early aggressive vs phased) under different climate scenarios; analysis of how uncertainty affects future agent optionality; examination of whether present growth preservation at future climate risk constitutes extraction',
    'If early mitigation is cost-effective: deferral is extractive (constraint shifts toward snare). If phased approach is economically optimal under uncertainty: deferral may be justified (constraint remains tangled rope). The question is whether the growth-preservation motivation is analytically sound or a rationalization for present benefit at future cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_under_uncertainty, conceptual, 'Whether deferring mitigation under uncertainty constitutes intergenerational extraction').

omega_variable(
    reading_kernel_conflict,
    'Does the mitigation-priority reading inherently conflict with the degrowth-transformation reading within any single normative framework, or can both coexist as live policy positions held by different political coalitions?',
    'Examination of whether the two readings share any foundational axioms; analysis of whether embracing one reading requires rejecting the other''s core premise, or whether they differ only in empirical bets and risk tolerance',
    'If forecloses: one reading must be abandoned in any coherent framework. If coexists_with: both remain live options under different assumptions about technology, politics, and intergenerational risk. Determines the logical structure of the climate-response-legitimacy kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_conflict, conceptual, 'Logical relationship between mitigation-priority and degrowth readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mitpri_tr_t0, mitigation_priority, theater_ratio, 0, 0.55).
narrative_ontology:measurement(mitpri_tr_t10, mitigation_priority, theater_ratio, 10, 0.64).
narrative_ontology:measurement(mitpri_tr_t20, mitigation_priority, theater_ratio, 20, 0.68).
narrative_ontology:measurement(mitpri_tr_t30, mitigation_priority, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(mitpri_be_t0, mitigation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mitpri_be_t10, mitigation_priority, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mitpri_be_t20, mitigation_priority, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(mitpri_be_t30, mitigation_priority, base_extractiveness, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(mitigation_priority, 0.18).
narrative_ontology:affects_constraint(mitigation_priority, carbon_pricing_mechanism).
narrative_ontology:affects_constraint(mitigation_priority, renewable_energy_transition).
narrative_ontology:affects_constraint(mitigation_priority, adaptation_priority).
narrative_ontology:affects_constraint(mitigation_priority, degrowth_transformation).
narrative_ontology:affects_constraint(mitigation_priority, intergenerational_climate_equity).

% DUAL FORMULATION NOTE:
% The mitigation-priority reading is part of a three-reading constraint family on climate-response-legitimacy kernel. The sibling readings (adaptation-priority, degrowth-transformation) are separate constraint stories with different ε values, different beneficiary/victim structures, and different temporal horizons. The mitigation-priority reading has ε=0.58 (tangled rope); adaptation-priority likely has ε≈0.45 (rope/tangled rope hybrid); degrowth-transformation likely has ε≈0.35 (scaffold with sunset to managed contraction). These are not perspectives on a single constraint—they are structurally distinct constraint stories linked by kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mitigation_priority, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
