% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Climate Mitigation Priority: Intergenerational Justice & Rapid Decarbonization
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation_priority reading of the climate response obligation kernel
 *   asserts that preventing future harm through rapid decarbonization
 *   (limiting warming to 1.5-2°C) is the paramount ethical and practical
 *   imperative. This reading grounds legitimacy in intergenerational justice:
 *   future generations cannot participate in current emissions decisions but
 *   bear the consequences of climate impacts for centuries. The constraint
 *   structures a massive intergenerational transfer: current generations
 *   (particularly the Global North) bear transition costs (economic
 *   restructuring, energy price increases, consumption constraints); future
 *   generations and climate-vulnerable nations receive benefits (avoided
 *   warming, reduced climate impacts). Fossil capital enters the victim set
 *   via stranded assets: $20+ trillion in proven reserves and
 *   carbon-intensive infrastructure cannot be profitably exploited under
 *   rapid-decarbonization mandates. The constraint exhibits characteristics
 *   of a Tangled Rope: it contains genuine coordination functions (solving
 *   collective action problems in energy systems transition, enabling
 *   renewable energy deployment) AND significant asymmetric extraction
 *   (bearing down on stranded workers, fossil capital, current-generation
 *   global consumers, while benefiting future generations who cannot
 *   advocate). The measurement trajectory shows rising extractiveness (0.35 →
 *   0.58) and rising suppression (0.48 → 0.68) over 20 years, indicating that
 *   the mitigation mandate's enforcement machinery intensifies — carbon
 *   pricing accelerates, regulatory mandates tighten, alternative energy
 *   sources are suppressed by fuel-switching requirements. Theater ratio
 *   rises modestly (0.38 → 0.52) as the international climate governance
 *   apparatus maintains legitimacy through ritual (COP meetings, pledge
 *   cycles, assessment reports) while actual implementation lags behind
 *   commitments.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary beneficiary (powerless/trapped) — cannot participate in current decisions; face unavoidable climate consequences from locked-in warming; mitigation constraint protects them
 *   - Global North Current Generation: Primary victim (moderate/constrained) — bears transition costs (energy prices, consumption constraints, economic restructuring) but also benefits from climate risk reduction; constrained but not trapped
 *   - Climate Vulnerable Nations: Organized victim bloc (organized/constrained) — paradoxically primary victims of inaction but structured to bear mitigation costs through finance obligations and development constraints; both benefit from reduced warming and extract from through transition burdens
 *   - Fossil Fuel Dependent Workers: Stranded victims (powerless/trapped) — coal miners, power plant workers, oil refinery employees face plant closures within 10-15 years; retraining/transition support is underfunded; no viable exit within the constraint's timeline
 *   - Fossil Capital (Oil Majors, Coal Companies, Gas Utilities): High-power victim (powerful/constrained) — faces $20+ trillion in stranded assets; legal exit routes blocked by global policy consensus; suppression of alternative (continued fossil dependence is mandated away)
 *   - Renewable Energy Sector: Primary beneficiary institution (institutional/arbitrage) — benefits from $trillions in market creation, policy support, industrial growth; low suppression; pure arbitrage capacity
 *   - International Climate Governance Apparatus: Institutional degraded mechanism (institutional/arbitrage) — maintains legitimacy through performative compliance (COP rituals, binding targets with loopholes, delayed implementation); theater ratio indicates degradation
 *   - Analytical Observer: Thermophysical necessity view (analytical/analytical) — from civilizational perspective, rapid decarbonization solves collective action problem; no suppression of alternatives, only thermophysical limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.68).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Climate Mitigation Priority: Intergenerational Justice & Rapid Decarbonization").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'a6818fef-e24d-4bd9-8124-50f9e35dd656').
narrative_ontology:cs_kernel_codification('a6818fef-e24d-4bd9-8124-50f9e35dd656', formalized).
narrative_ontology:cs_authority_grounding('a6818fef-e24d-4bd9-8124-50f9e35dd656', lineage).
narrative_ontology:cs_interpretation_layer_present('a6818fef-e24d-4bd9-8124-50f9e35dd656').
narrative_ontology:cs_reading_relation('a6818fef-e24d-4bd9-8124-50f9e35dd656', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('a6818fef-e24d-4bd9-8124-50f9e35dd656', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('a6818fef-e24d-4bd9-8124-50f9e35dd656', foundational, intergenerational_justice_obligation).
narrative_ontology:cs_axiom_status(intergenerational_justice_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a6818fef-e24d-4bd9-8124-50f9e35dd656', intergenerational_justice_obligation, deontological).
narrative_ontology:cs_axiom('a6818fef-e24d-4bd9-8124-50f9e35dd656', secondary, decoupling_feasibility).
narrative_ontology:cs_axiom_status(decoupling_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('a6818fef-e24d-4bd9-8124-50f9e35dd656', decoupling_feasibility, empirically_contingent).
narrative_ontology:cs_reference_frame('a6818fef-e24d-4bd9-8124-50f9e35dd656', paris_agreement_framework).
narrative_ontology:cs_drift_state('a6818fef-e24d-4bd9-8124-50f9e35dd656', contemporary_2026, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a6818fef-e24d-4bd9-8124-50f9e35dd656', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_generation_global_north).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL MINER / STRANDED WORKER (SNARE) — Trapped in regional economy dependent on fossil fuel extraction. Rapid decarbonization mandates closure of mines and power plants within 10-15 years. No exit: retraining programs are underfunded, relocation assistance is minimal, pension liabilities are externalized. Experiences maximum extraction with minimal coordination benefit. The constraint offers no genuine transition pathway, only mandate + abandonment.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GLOBAL NORTH MIDDLE CLASS (TANGLED ROPE) — Bears significant transition costs (energy price increases, consumption constraints, lifestyle adjustments) but also benefits from reduced climate risk and green job creation. Exit is possible but costly: can reduce consumption, invest in efficiency, switch sectors — but faces substantial barriers (infrastructure lock-in, capital requirements, social penalty). Mixed extraction and coordination.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY SECTOR (ROPE) — Primary beneficiary. Mitigation mandate creates $trillions in market opportunity, policy support, and industrial growth. Experiences the constraint as pure coordination: rapid decarbonization solves the coordination problem of transitioning energy systems. Low suppression, high arbitrage capacity (can exit fossil fuels entirely). Chi is low or negative — extraction runs toward this agent.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOSSIL CAPITAL / STRANDED ASSETS (SNARE) — High-power victim. Oil majors, coal companies, gas utilities face mandate to strand $20+ trillion in proven reserves and infrastructure assets. Legal exit routes are slow (regulatory capture, litigation). Structural exit is foreclosed by rapid decarbonization timeline. Suppression of alternatives (continued fossil dependence is mandated by global policy consensus). Victim status is paradoxical — powerful agent with no escape.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FUTURE GENERATIONS (MOUNTAIN, POWERLESS VIEW) — Framed as having no exit from climate impacts already committed by current emissions. Mitigation constraint appears as immutable natural law: the physics of carbon accumulation and temperature response cannot be negotiated. Future generations cannot participate in the decision; they face the consequences. From this powerless view, the constraint appears as an unchangeable law of physics masking the current generation's structural inability to imagine their own obligation.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE VULNERABLE NATIONS (TANGLED ROPE) — Bangladesh, Pacific Islands, Sub-Saharan Africa. Paradoxically both primary victims of inaction AND structured to bear mitigation costs through climate finance obligations and development constraints. Organized enough to articulate interests but constrained by dependency on Global North capital and technology. Mitigation benefits them (reduced warming) but also extracts from them (forced transition from agriculture to green energy dependency, debt from climate adaptation). Mixed coordination and extraction from a lower-power institutional position.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL CLIMATE GOVERNANCE (PITON) — UNFCCC, Paris Agreement, IPCC assessments. Theater ratio (0.52) reflects performative compliance: binding emission reduction targets coexist with vast loopholes (carbon offsets, baselines, accounting gimmicks), non-enforcement mechanisms, and delayed implementation. The apparatus maintains legitimacy through ritual (annual COPs, assessment reports, pledges) while actual decarbonization lags pledges by 10-15 years. Degraded coordination mechanism maintained by institutional inertia.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMOPHYSICAL VIEW (ROPE) — From civilizational/universal perspective, rapid decarbonization is pure coordination problem: all agents benefit from preventing runaway warming (via thermophysical feedback mechanisms), and the constraint solves the collective action problem of reducing emissions. From this view, the constraint is coordination enabling survival, not extraction. No suppression of alternatives — only thermophysical limits on acceptable warming scenarios.
constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_obligation__mitigation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising from 0.35 over the interval. The mitigation constraint extracts significantly from current-generation Global North (consumption restrictions, energy costs, economic restructuring) and from fossil capital (asset stranding, revenue elimination). The rising trajectory reflects intensifying decarbonization mandates — carbon pricing accelerates, regulatory requirements tighten, alternative energy sources suppress fossil fuel demand. However, extractiveness is not at snare levels (0.66+) because genuine coordination functions persist: renewable energy deployment is solving the real coordination problem of energy system transition; future climate risk reduction is a real benefit (not just transferred extraction); green job creation and industrial growth provide some offsetting economic benefit to current generations. Suppression (0.68): High and rising. The constraint suppresses alternatives to rapid decarbonization: continued fossil fuel dependence is mandated away by global policy consensus; fossil capital faces regulatory closure timelines with minimal legal exit routes; stranded workers have few viable career paths outside energy transition programs. Rising suppression indicates that enforcement machinery intensifies — carbon borders, fuel-switching mandates, permitting restrictions all tighten over the interval. Theater ratio (0.52): Moderate. The international climate governance apparatus exhibits significant performativity — COP rituals, binding pledge cycles, assessment reports maintain legitimacy while actual decarbonization lags pledges by 10-15 years; carbon offset schemes include vast accounting loopholes; net-zero commitments are often met by purchased offsets rather than domestic emissions reductions. Theater is not dominant (piton-level 0.70+) because some functional decarbonization is occurring — renewable deployment is real, some coal plants are closing, some sectors are genuinely decarbonizing. But the gap between declared ambition and actual implementation indicates substantial performative content.
 *
 * PERSPECTIVAL GAP:
 *   The mitigation_priority reading produces a striking perspectival disaggregation. The coal miner sees pure snare — trapped by plant closure mandates with no transition support. Fossil capital sees snare-like extraction despite high structural power — stranded assets with no legal exit. The Global North middle class sees tangled rope — significant costs but also climate benefit and some economic opportunity. The renewable energy sector sees rope — pure coordination benefit with minimal extraction. Future generations see the constraint as mountain (natural law of thermophysics) from a powerless perspective, but this naturalizes the current generation's structural inability to conceive of their own obligation. The climate governance apparatus sees itself as rope but is actually piton — degraded coordination mechanism sustained by institutional inertia. The analytical observer sees rope — pure coordination problem solved by rapid decarbonization. The perspectival spread from snare to rope to mountain reveals that the constraint's classification is deeply observer-dependent. Whether it is extractive or coordinative hinges entirely on whose burden you measure and whose benefit you count.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to the constraint. Future generations are trapped powerless agents with maximum d ≈ 0.95 (bearing all consequences, no exit). Coal miners are trapped at d ≈ 0.92 (no exit, no benefit). Fossil capital is constrained powerful at d ≈ 0.75 (exit is legally foreclosed despite structural power). Global North middle class is constrained moderate at d ≈ 0.55 (faces costs but also benefits; can reduce consumption at high price). Renewable energy sector is arbitrage institutional at d ≈ 0.10 (benefits flow to this agent; exit is costless). Climate vulnerable nations are constrained organized at d ≈ 0.60 (organized enough to negotiate but dependent on Global North capital). The chi formula (χ = ε × f(d) × σ(S)) scales effective extractiveness by the sigmoid f(d) and scope modifier σ(S). At d ≈ 0.95 (future generations), f(d) ≈ 1.42 (maximum amplification), producing high experienced χ despite moderate base ε. At d ≈ 0.10 (renewable sector), f(d) ≈ -0.12 (maximum dampening), producing negative χ (experienced benefit). Global scope (σ=1.2) amplifies extraction for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via reading-specific axioms: The mitigation_priority reading resolves the potential mandatrophy (Is this primarily coordination or extraction?) by grounding legitimacy in the axiom that intergenerational justice obligates present-generation burden-bearing. Under this axiom, the extraction from current generation to future generation is not a flaw but a feature — it is the coordination solution to the intergenerational collective action problem. Fossil capital's stranding is not an unintended side effect but the necessary condition for decarbonization. The reading does NOT resolve the empirical ambiguity (will rapid decarbonization actually prevent harmful warming?) or the implementation ambiguity (will just transition pathways actually be funded?). These remain omegas. But the ethical principle is settled within this reading: preventing future harm justifies extracting from fossil capital and from current-generation consumers. The sibling readings (adaptation_priority, degrowth_reading) reject this axiom or add additional constraints (adaptation must be prioritized; growth itself must be questioned). The mandatrophy is resolved not by showing extraction is absent but by showing extraction is justified within the reading's axiom set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_feasibility,
    'Are genuinely just transition pathways (job guarantee, pension security, regional reinvestment) technically and fiscally feasible within the mitigation timeline, or is ''just transition'' rhetorical cover for accepting inequality?',
    'Post-transition analysis: actual employment outcomes, wage trajectories, and mobility for displaced fossil workers 10+ years after plant closures; comparison of committed transition funds to actual deployment; wage/pension replacement rates vs promises',
    'If feasible: snare classification for stranded workers is misdiagnosed — tangled_rope is more accurate (high cost but genuine exit pathways exist). If infeasible: snare is correct — extraction is real and unavoidable within the constraint''s timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_feasibility, empirical, 'Feasibility of just transition pathways for displaced fossil workers').

omega_variable(
    adaptation_vs_mitigation_sufficiency,
    'At rapid-mitigation effort levels (consistent with 1.5°C), how much residual warming and climate impact will adaptation investment be required to manage? Is the mitigation burden justified by the adaptation savings, or do both pathways extract from current generations simultaneously?',
    'Integrated assessment models comparing full-lifecycle costs of mitigation + residual adaptation vs lower mitigation + higher adaptation across multiple climate scenarios; sectoral breakdowns (food, water, health, infrastructure)',
    'If residual adaptation is minimal: mitigation_priority reading is economically sound (benefit-cost favorable). If residual adaptation is massive: mitigation burden appears less justified, and adaptation_priority reading gains structural credibility. Both pathways extracting from current generation shifts the classification toward snare for all contemporary actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_mitigation_sufficiency, empirical, 'Residual adaptation burden under rapid-mitigation scenarios').

omega_variable(
    historical_responsibility_allocation,
    'Should mitigation burden (decarbonization mandate) be distributed by historical cumulative emissions (Global North pays more), current per-capita emissions (developing high-growth economies pay more), or equal cost-sharing across nations?',
    'Policy outcome analysis: actual mitigation burden distribution across nations as implemented in NDCs and climate finance; comparison to three competing allocation principles; measurement of wealth transfer (if any) from Global North to climate-vulnerable nations',
    'If historical-responsibility allocation is adopted: Global North faces snare classification (high mandatory burden with minimal exit), and climate-vulnerable nations face tangled_rope with genuine support. If equal cost-sharing prevails: poor nations face snare, rich nations face tangled_rope. If current-emissions allocation: high-growth Asia bears the burden. Each allocation produces different victim/beneficiary sets and changes χ values across perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_responsibility_allocation, preference, 'Allocation principle for mitigation burden across nations').

omega_variable(
    stranded_asset_compensation_depth,
    'If fossil capital receives compensation for stranded assets (market-based phase-out), what depth of compensation counts as ''fair'' — asset book value, replacement cost, forgone future profits, or nothing (regulatory taking)? Does the compensation mechanism transform fossil capital from victim to beneficiary?',
    'Comparison of actual compensation mechanisms across jurisdictions (EU ETS, just transition funds, asset buybacks); analysis of net wealth transfer to fossil sector; long-term capital redeployment trajectories',
    'If deep compensation (replacement cost + some profit recovery): fossil capital becomes tangled_rope or rope (high extraction but significant beneficiary role), and current-generation Global North extraction increases. If minimal compensation: fossil capital remains snare, but social stability risk increases (stranded workers + stranded capitalists both experience extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_compensation_depth, preference, 'Depth of compensation for stranded fossil assets').

omega_variable(
    reading_kernel_ambiguity,
    'Is the climate response obligation kernel contested fundamentally at the level of ethical principle (intergenerational justice vs present-generation welfare vs ecological sufficiency), empirical estimates (climate sensitivity, damage functions, cost curves), or implementation design (pace, burden-sharing, technology choice)?',
    'Discourse analysis of policy disagreements: isolate whether sibling readings (adaptation_priority, degrowth_reading) reject the mitigation_priority reading on ethical grounds, empirical grounds, or implementation grounds. Test whether agreement on any one dimension (e.g., ''climate change is real'') would resolve the reading contest.',
    'If ethical disagreement: readings coexist (different values lead to different choices). If empirical disagreement: readings should converge as evidence accumulates. If implementation disagreement: readings influence each other (different designs produce different distributions of burden). Omega classification determines expected resolution pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Locus of disagreement among climate response kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_mit_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.38).
narrative_ontology:measurement(clim_mit_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.5).
narrative_ontology:measurement(clim_mit_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(clim_mit_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_mit_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(clim_mit_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_mit_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(clim_mit_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(clim_mit_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_capital_stranding).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, just_transition_feasibility).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, intergenerational_justice_distribution).

% DUAL FORMULATION NOTE:
% The climate response obligation is a contested kernel with three structurally distinct readings: mitigation_priority (this story), adaptation_priority (separate constraint file), and degrowth_reading (separate constraint file). Each reading has its own ε value, beneficiary/victim set, and perspective distribution. They are linked via network.affects_constraints because they are readings of the same kernel and each reading's adoption affects the structural feasibility and burden distribution of the others. The mitigation_priority reading assumes adaptation is necessary-but-secondary; the adaptation_priority reading treats mitigation as increasingly costly and ineffective; the degrowth_reading treats both mitigation and adaptation as insufficient without systemic economic change. Constraint families are: (1) climate response obligation triplet (the three readings); (2) fossil capital stranding (downstream of mitigation_priority adoption); (3) just transition mechanisms (downstream of stranded worker impact).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__mitigation_priority, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
