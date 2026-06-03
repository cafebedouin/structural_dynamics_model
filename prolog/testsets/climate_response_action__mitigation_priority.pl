% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Climate Mitigation Through Emissions Reduction and Carbon Markets (GDP-Compatible)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate response is a contested
 *   institutional commitment: it defines climate action as limiting warming
 *   below 2°C through emissions reductions in high-emitting sectors, enabled
 *   by technological innovation (renewable energy, carbon capture, industrial
 *   efficiency) and market mechanisms (carbon pricing, climate finance).
 *   Critically, this reading commits to maintaining GDP growth in high-income
 *   economies as both prerequisite and outcome. This constraint exhibits
 *   tangled-rope structure: it coordinates genuine global emissions reduction
 *   (solving a collective action problem of atmospheric CO2 stabilization)
 *   while simultaneously extracting from vulnerable populations through
 *   adaptation cost deferral and from future generations through assumptions
 *   about technological feasibility. The reading is one of three competing
 *   frameworks offered by the kernel climate_response_action:
 *   mitigation-priority (this constraint), adaptation-priority (investment in
 *   resilience infrastructure, accepting temperature rise), and
 *   degrowth-transformation (structural economic reorientation rejecting GDP
 *   growth). The selection of mitigation-priority as the dominant
 *   international framework (Paris Agreement, IPCC 1.5°C framing)
 *   concentrates costs on current emissions reductions in high-emitting
 *   sectors while deferring adaptation investments to vulnerable regions—a
 *   distribution that benefits nations with innovation capacity and financial
 *   leverage. The extractiveness value (0.54) reflects moderate asymmetric
 *   extraction: genuine coordination function (emissions reduction) coupled
 *   with embedded distributional unfairness (who bears costs, who captures
 *   benefits, who decides acceptable residual climate change). Theater ratio
 *   (0.65) reflects performative elements in carbon accounting, additionality
 *   verification, and the 2°C temperature target—a metric that lacks physical
 *   or equity justification but persists as negotiation focal point.
 *
 * KEY AGENTS:
 *   - High-income nations with innovation capacity (EU, US, Japan, Korea): Institutional/arbitrage beneficiaries — capture technology markets, carbon credit revenues, climate finance intermediation; experience constraint as coordination mechanism with positive externalities
 *   - Vulnerable populations (Global South, island nations, low-income communities): Powerless/trapped victims — bear adaptation costs deferred by mitigation strategy; face residual climate impacts from incomplete emissions reduction; have no agency over reading selection
 *   - High-emitting sectors (coal, oil, cement, steel): Moderate/constrained — face transition costs and stranded assets; retain political influence and arbitrage options; benefit from carbon market hedging and transition investment
 *   - Future generations: Powerless/trapped (analytical treatment) — bear accumulated residual climate impacts; assumption of successful carbon removal is unverified; their interests are not directly represented in mitigation-priority framework
 *   - Climate justice and degrowth movements: Organized/mobile — see mitigation-priority as embedding structural inequality; have exit options via alternative readings but constrained by institutional power asymmetry
 *   - Carbon market and climate finance infrastructure: Institutional/arbitrage — benefit from market expansion and financial intermediation; have incentive to maintain theater and complexity that sustains intermediary role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.54).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.54).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Climate Mitigation Through Emissions Reduction and Carbon Markets (GDP-Compatible)").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '5bdbc07e-66ce-4df9-bebd-ce77ff3205c7').
narrative_ontology:cs_kernel_codification('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', formalized).
narrative_ontology:cs_authority_grounding('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', extraction).
narrative_ontology:cs_interpretation_layer_present('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7').
narrative_ontology:cs_reading_relation('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', foundational, carbon_minimization_ensures_equity).
narrative_ontology:cs_axiom_status(carbon_minimization_ensures_equity, holdable).
narrative_ontology:cs_axiom_grounding('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', carbon_minimization_ensures_equity, instrumental).
narrative_ontology:cs_axiom('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', foundational, growth_compatible_mitigation).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', growth_compatible_mitigation, empirically_contingent).
narrative_ontology:cs_reference_frame('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', market_efficient_climate_response).
narrative_ontology:cs_drift_state('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', contemporary_2026, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5bdbc07e-66ce-4df9-bebd-ce77ff3205c7', '2026-02-26T14:30:00Z').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, high_income_nations_with_innovation_capacity).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, fossil_fuel_transition_industries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_financiers).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_sectors_immediate_costs).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, vulnerable_populations_adaptation_deferral).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations_residual_impacts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATIONS (SNARE) — Face mandatory adaptation to climate impacts deferred by mitigation strategy; bear costs of both current warming and failed mitigation promises. Trapped by geography and resource scarcity; no exit from climate exposure. Maximum extraction: adaptation burden concentrates on those with least emissions responsibility and fewest resources.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-EMITTING SECTORS (TANGLED ROPE) — Face transition costs and stranded asset write-downs, but also benefit from investment in cleaner production technologies and market guarantees for carbon-intensive activities via offsets. Constrained by regulatory pressure and capital reallocation, but retain exit option via lobbying and political influence. Mixed extraction and coordination: the constraint both enforces transition and protects incumbent profitability.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME NATIONS WITH INNOVATION CAPACITY (ROPE) — Experience mitigation strategy as coordination mechanism: emissions reduction targets drive demand for clean technology, creating markets and competitive advantage in green industries. Arbitrage options abundant (technology export, carbon credit sales, financial engineering of climate finance). Net beneficiary: coordination mechanism creates economic opportunity.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE AND DEGROWTH MOVEMENTS (TANGLED ROPE) — See the mitigation-priority reading as embedding structural inequality through carbon markets and deferral of adaptation funding. Organized capacity to challenge the framework, but constrained by political marginality. The constraint both enables their coordination (shared climate concern) and extracts from them (their alternative framings are systematically delegitimized). Mobile exit via alternative readings (adaptation-priority, degrowth-transformation) but require institutional power to make alternatives official.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE FINANCE AND CARBON MARKET INFRASTRUCTURE (SCAFFOLD) — Coordinating mechanism with temporary function and embedded sunset. Carbon markets and climate finance mechanisms are designed as transitional tools: as emissions fall and adaptation investments mature, the need for financial intermediaries should decline. However, the constraint embeds theater (carbon accounting, additionality verification, baseline disputes) that may persist longer than the underlying coordination function. Sunset clause: when emissions reach net-zero trajectory and adaptation capacity is funded in vulnerable regions, these institutions can dissolve. Currently enforced; sunset requires political will and counterfactual verification.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: 2°C TEMPERATURE TARGET AS SACRED METRIC (PITON) — The 2°C ceiling is analytically degraded as a regulatory anchor: the threshold is not derived from climate physics (impacts scale continuously, not at 2.0°C threshold), nor from equity logic (distributes risk unequally), nor from economic logic (damages function and abatement cost curves don't inflect at 2°C). It persists as an institutional ritual and negotiation focal point because of treaty path-dependence (Paris Agreement, IPCC AR5). The theater ratio (0.65) reflects that enormous institutional effort maintains the 2°C framing despite its analytical indefensibility — scientific presentations, policy summits, national pledges all anchor to this number that has become decoupled from its original justification.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the mitigation-priority reading genuinely coordinates global emissions reduction (the constraint solves a collective action problem about stabilizing atmospheric CO2). However, it simultaneously extracts from vulnerable populations and future generations by deferring adaptation investment and assuming technological solutions that may fail. The reading is NOT a natural law or immutable necessity — it is a political choice that benefits innovation-rich nations and defers costs to those with least agency. The constraint's classification as tangled_rope is stable across time horizons: it is a hybrid, not a false summit.
constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_action__mitigation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_action__mitigation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The mitigation-priority reading coordinates genuine emissions reduction (a public good with positive global externality), but extracts value through asymmetric cost distribution. High-emitting sectors pay transition costs immediately; vulnerable populations pay adaptation costs later; future generations pay residual climate risk. Innovation-rich nations capture technology rent and carbon credit arbitrage. The extractiveness is not minimal (this is not pure coordination—rope would be ε ≤ 0.45) because the asymmetry is structural and enforced via market mechanisms that favor high-income actors. Rising extractiveness over the interval (0.38 → 0.54) reflects that as emissions reduction targets tighten, suppression mechanisms intensify—carbon pricing rises, sectoral mandates deepen, and alternative readings (adaptation-priority, degrowth) are more forcefully delegitimized. Suppression (0.62): Moderate-high and rising. Enforced via carbon pricing, regulatory mandates, technology standards, and financial architecture that privileges mitigation spending over adaptation spending. Alternative framings (degrowth, adaptation-priority) are actively suppressed through institutional control of climate science communication, policy forums, and climate finance allocation. Rising suppression reflects enforcement intensification as the reading becomes institutionalized in Paris Agreement, IPCC framing, and national Nationally Determined Contributions (NDCs). Theater ratio (0.65): Moderately high. The 2°C target is analytically indefensible (impacts scale continuously, not at 2.0°C; the threshold was political compromise, not science). Carbon market additionality verification is often performative—many offset projects would happen anyway. Climate finance tracking involves significant accounting theater. Renewable energy deployment is genuine, but framing as 'GDP-compatible mitigation' is partially theatrical—the actual path requires higher energy intensity reduction or lower growth than governments admit. Rising theater reflects that maintaining the fiction of growth-compatible mitigation becomes harder as implementation reveals the trade-offs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. High-income innovation nations experience pure coordination (Rope)—emissions targets drive their competitive advantage in clean tech. Vulnerable populations experience pure extraction (Snare)—they bear costs of both current warming and deferred adaptation. High-emitting sectors experience hybrid coordination-extraction (Tangled Rope)—transition investment alongside stranded assets. Organized movements experience tangled rope from opposite direction—excluded from decisions that concentrate cost on them. The 2°C metric itself (Piton perspective) is revealed as degraded ritual, analytically unjustified but institutionally entrenched. The analytical observer (Tangled Rope perspective at civilizational scale) identifies the genuine coordination function coupled to asymmetric distribution. The perspectival gap is not a measurement artifact—it reflects real structural differences in agent power, exit options, and benefit-cost asymmetry. Mandatrophy is resolved not by collapsing to one type, but by acknowledging that the constraint IS a tangled rope: it coordinates emissions reduction AND extracts from powerless agents. The reading's defenders emphasize the coordination function; critics emphasize the extraction. Both are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from the structural relationships declared in beneficiaries/victims. High-income nations with innovation capacity are net beneficiaries with arbitrage options (low d → negative effective extraction χ). Vulnerable populations are victims with trapped exit (high d → high f(d) → high experienced extraction). High-emitting sectors are victims of regulatory pressure but with political influence (moderate d → moderate χ). Future generations are victims with no exit options and no representation (would produce very high d if included, but institutional framework excludes their direct participation). The engine derives d from these relationships and applies the sigmoid f(d) to produce perspectival χ values. The piton classification (2°C metric) derives from theater_ratio ≥ 0.70 threshold combined with ε ≤ 0.25—wait: piton requires ε ≤ 0.25, and this constraint has ε = 0.54, so piton is not a gate classification. The piton classification at the civilizational/analytical perspective is a valid perspectival reading but not structural—it reflects that from the analytical view, the 2°C target is revealed as degraded ritual. This is the power of indexed classification: the same base properties generate different types from different observation positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The mitigation-priority reading resolves the coordination-vs.-extraction ambiguity by accepting that the constraint is tangled rope, not pure coordination. The 'mandatrophy' posed by climate policy is: 'Does climate mitigation coordination work?' The reading answers: 'Yes, emissions reduction is coordinated. But the coordination mechanism distributes costs asymmetrically, and the beneficiaries of the asymmetry have institutional power to enforce the distribution.' This is precisely the structure tangled rope describes—genuine coordination function coupled with asymmetric extraction. The resolution is NOT to redefine the constraint as pure coordination (rope) by denying the asymmetry, nor as pure extraction (snare) by denying the coordination function. It is to acknowledge the hybrid and analyze how the balance shifts under different futures (empirical omegas test feasibility of carbon removal, additionality enforcement, growth-compatible decoupling). The constraint is resilient under this reading precisely because it embeds both coordination and extraction—it can adapt if extraction becomes intolerable by shifting toward the coordination function, or if coordination proves infeasible by shifting toward explicit extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_removal_technological_feasibility,
    'Will direct air capture (DAC) and other carbon removal technologies scale to offset residual emissions at economically viable costs (< $200/ton CO2) by 2070?',
    'Technology cost curves (learning rates, deployment scale); thermodynamic feasibility analysis; historical precedent for industrial scaling of energy-intensive processes',
    'If feasible: mitigation-priority reading supported — removals can offset residual emissions, delaying deep adaptation. If infeasible: mitigation fails; constraint reclassifies as snare from powerless agents'' perspective (deferral without compensation). If feasible but only at massive cost: constraint reclassifies toward snare — extraction becomes explicit as costs rise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_removal_technological_feasibility, empirical, 'Whether carbon removal technologies will achieve required cost and scale').

omega_variable(
    adaptation_cost_deferral_equity,
    'Does deferring adaptation investment until 2050+ create intergenerational inequity that violates the mitigation-priority reading''s implicit fairness premise?',
    'Intergenerational accounting: compare cumulative adaptation costs borne by each generation under mitigation-priority vs. adaptation-priority pathways; measure welfare loss from delayed adaptation vs. welfare gain from mitigation-enabled development',
    'If deferral violates fairness: reading is foreclosed by its own axiom (carbon_minimization_ensures_equity). If deferral is acceptable given development benefits: reading sustained. If trade-off is genuine and unresolvable: constraint becomes a preference omega, not empirical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_cost_deferral_equity, preference, 'Whether adaptation cost deferral is justified by intergenerational equity principle').

omega_variable(
    carbon_market_additionality_enforcement,
    'Can carbon market mechanisms enforce actual emissions reduction (additionality) or do they launder business-as-usual projects as climate action?',
    'Meta-analysis of offset effectiveness: baseline comparison studies; satellite-verified deforestation/reforestation; sectoral emissions accounting under offset vs. non-offset scenarios',
    'If additionality enforced: carbon markets function as stated coordination mechanism. If additionality fails: markets become pure financial theater (theater_ratio → 0.80); extracted value is redistributed to offset financiers without emissions reduction; constraint reclassifies as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_market_additionality_enforcement, empirical, 'Whether carbon markets achieve additionality and actual emissions reduction').

omega_variable(
    gdp_growth_decoupling_feasibility,
    'Can advanced economies achieve 2°C-compatible emissions reductions while maintaining 2-3% annual GDP growth, or does the reading''s core assumption require immediate contradiction?',
    'Empirical decoupling analysis: energy intensity trends, sectoral emissions intensity, renewable energy substitution rates; projection of required decoupling factors vs. historical rates',
    'If decoupling feasible: reading''s axiom (growth_compatible_mitigation) supported. If impossible: reading embedded in performative contradiction — the constraint enforces GDP growth while requiring emissions reductions that thermodynamically conflict with it. Reclassifies as piton (theater maintains incompatible goals).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gdp_growth_decoupling_feasibility, empirical, 'Whether emissions reduction is compatible with sustained GDP growth').

omega_variable(
    kernel_reading_selection_bias,
    'Is the mitigation-priority reading selected as ''official'' climate policy framework because it is analytically optimal, or because it distributes costs away from high-income nations that set international standards?',
    'Institutional analysis: compare lobbying intensity, media framing, academic journal citation patterns, and policy document prevalence of mitigation-priority vs. adaptation-priority and degrowth-transformation readings; test whether selection correlates with beneficiary power',
    'If selection is unbiased: reading''s legitimacy confirmed. If selection correlates with beneficiary power: reading is enforced via institutional capture, not merits. Reclassifies as snare (disguised institutional extraction). This omega is conceptual — it tests whether the kernel contest is genuine or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether mitigation-priority reading is selected for analytical merit or power distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climm_theater_baseline, climate_response_action__mitigation_priority, theater_ratio, 0, 0.52).
narrative_ontology:measurement(climm_theater_2040, climate_response_action__mitigation_priority, theater_ratio, 15, 0.62).
narrative_ontology:measurement(climm_theater_2055, climate_response_action__mitigation_priority, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(climm_extractiveness_baseline, climate_response_action__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(climm_extractiveness_2040, climate_response_action__mitigation_priority, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(climm_extractiveness_2055, climate_response_action__mitigation_priority, base_extractiveness, 30, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(climm_suppression_baseline, climate_response_action__mitigation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(climm_suppression_2040, climate_response_action__mitigation_priority, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(climm_suppression_2055, climate_response_action__mitigation_priority, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, carbon_market_additionality).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, technological_carbon_removal_feasibility).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, intergenerational_climate_equity).

% DUAL FORMULATION NOTE:
% The mitigation-priority reading is part of the climate_response_action kernel family. The sibling readings (adaptation-priority, degrowth-transformation) are separate constraint stories with different beneficiary/victim structures, different ε values, and different classification distributions. All three readings share the same kernel (how to respond to climate change) but instantiate different institutional commitments. This story documents the mitigation-priority reading; the sibling stories are structurally linked via network.affects_constraints but represent genuine alternatives, not variations of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__mitigation_priority, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
