% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_reading, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_reading
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Policy
 *   domain: energy_policy/risk_governance/public_safety
 *
 * SUMMARY:
 *   The catastrophic-tail-reading of acceptable risk for energy policy frames
 *   collective risk decisions as dominated by the prevention of maximum
 *   credible catastrophes regardless of their probability. Under this
 *   reading, policy should prioritize outcomes that prevent
 *   civilization-disrupting events (cascading grid failure, uncontrolled
 *   climate transition, pandemic-enabled infrastructure collapse) even when
 *   expected-value analysis would prefer alternative strategies with lower
 *   average harm but higher tail risk. This is ONE reading of a contested
 *   kernel shared with the expected-value-reading (minimize expected harm
 *   across all outcomes) and the precautionary-reading (account for
 *   irreducible uncertainty in probability estimation itself). The
 *   catastrophic-tail reading creates a structural asymmetry: potential
 *   victims of worst-case scenarios are weighted equally in policy regardless
 *   of the probability those scenarios materialize, while those bearing the
 *   cost of tail-risk mitigation (through higher energy costs, reduced grid
 *   reliability) are weighted by their proximity to the constraint. The
 *   constraint exhibits high suppression (0.62) because it forces communities
 *   dependent on energy affordability and grid reliability to accept reduced
 *   choice sets; moderate extractiveness (0.58) because renewable energy
 *   advocates and catastrophe-prevention constituencies benefit from policy
 *   alignment while fossil fuel industries and reliability-dependent
 *   infrastructure bear costs; and low theater (0.45) because the decision
 *   mechanism is relatively transparent (catastrophic outcome identification,
 *   policy principle application) even though the underlying probability
 *   estimates are contested.
 *
 * KEY AGENTS:
 *   - Potential Catastrophe Victims: Populations in disaster-vulnerable regions (trapped/powerless) — theoretically protected but bear asymmetric risk between crises
 *   - Energy-Affordability-Dependent Populations: Low-income households, small manufacturers, rural communities (constrained/moderate) — face coordination benefit (grid protection) and extraction (higher costs, reduced reliability)
 *   - Renewable Energy Industry & Climate Coalition: Organized advocacy bodies (mobile/organized) — benefit from policy alignment and contribute to coordination mechanism
 *   - Fossil Fuel Industry: Energy incumbents (constrained/powerful) — experience extraction through stranded assets and capital reallocation, partial coordination benefit from grid reliability
 *   - Grid Reliability Engineers & Critical Infrastructure: Hospitals, water treatment, emergency services (trapped/moderate) — face extraction through reliability uncertainty, receive only probabilistic protection
 *   - Regulatory/Policy Authority: Agencies adopting tail-risk decision criterion (arbitrage/institutional) — experience constraint as coordination mechanism, maintain legitimacy through principle-based policy
 *   - Analytical Observer: Civilizational/universal perspective (analytical/analytical) — risks naturalizing contested policy choice as immutable principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_reading, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_reading, 0.62).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_reading, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_reading, "Catastrophic Tail Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_reading, "energy_policy/risk_governance/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_reading, '1f1024c0-62e5-42c8-8b40-dc431fbcd605').
narrative_ontology:cs_kernel_codification('1f1024c0-62e5-42c8-8b40-dc431fbcd605', fixed_text).
narrative_ontology:cs_authority_grounding('1f1024c0-62e5-42c8-8b40-dc431fbcd605', distributed).
narrative_ontology:cs_reading_relation('1f1024c0-62e5-42c8-8b40-dc431fbcd605', acceptable_risk_for_energy__expected_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f1024c0-62e5-42c8-8b40-dc431fbcd605', acceptable_risk_for_energy__precautionary_reading, influences).
narrative_ontology:cs_axiom('1f1024c0-62e5-42c8-8b40-dc431fbcd605', foundational, catastrophic_outcomes_incommensurable).
narrative_ontology:cs_axiom_status(catastrophic_outcomes_incommensurable, holdable).
narrative_ontology:cs_axiom_grounding('1f1024c0-62e5-42c8-8b40-dc431fbcd605', catastrophic_outcomes_incommensurable, deontological).
narrative_ontology:cs_axiom('1f1024c0-62e5-42c8-8b40-dc431fbcd605', foundational, equal_weighting_potential_victims).
narrative_ontology:cs_axiom_status(equal_weighting_potential_victims, holdable).
narrative_ontology:cs_axiom_grounding('1f1024c0-62e5-42c8-8b40-dc431fbcd605', equal_weighting_potential_victims, deontological).
narrative_ontology:cs_reference_frame('1f1024c0-62e5-42c8-8b40-dc431fbcd605', catastrophic_dominance_imperative).
narrative_ontology:cs_drift_state('1f1024c0-62e5-42c8-8b40-dc431fbcd605', contemporary_energy_transition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f1024c0-62e5-42c8-8b40-dc431fbcd605', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_reading, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_reading, disaster_prevention_constituencies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_reading, energy_affordability_dependent_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_reading, grid_reliability_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL CATASTROPHE VICTIMS (SNARE) — Populations in disaster-vulnerable regions (flood zones, hurricane corridors, infrastructure-dependent areas) face maximum extraction: their welfare is theoretically protected by policy but only at moments of acute crisis. Between crises, they bear asymmetric risk (trapped geographically, structurally dependent on grid reliability). The constraint extracts their acceptance of lower energy reliability in exchange for marginally lower tail-risk probability. No exit option — relocation is economically infeasible for most.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY-AFFORDABILITY-DEPENDENT POPULATIONS (TANGLED ROPE) — Low-income households, small manufacturers, rural communities experience genuine coordination benefit (catastrophic grid failure would devastate them disproportionately) but also bear extraction through higher energy costs and reduced reliability. Constrained by economic dependency on grid access — can reduce consumption but cannot exit. The constraint offers both protection and cost burden.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY INDUSTRY & CLIMATE COALITION (ROPE) — Organized advocacy for tail-risk dominance in policy produces genuine coordination: catastrophic climate outcomes are averted through collective commitment to carbon reduction. This actor benefits from policy alignment with their interests AND contributes to the coordination mechanism. Mobile because they can adapt to multiple energy regimes; the constraint's framing aligns their interests with public safety rather than forcing a conflict.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: FOSSIL FUEL INDUSTRY (TANGLED ROPE) — Experiences the constraint as both extraction and partial coordination. Extraction: tail-risk policy drives transition away from profitable assets, stranding capital, reducing market share. Coordination: the industry does benefit from grid reliability and catastrophe prevention (business continuity, infrastructure protection). Constrained exit — can invest in renewables, can relocate capital, but cannot easily exit energy markets. The constraint forces reallocation of capital rather than destruction, leaving coordination potential.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GRID RELIABILITY ENGINEERS & GRID-DEPENDENT CRITICAL INFRASTRUCTURE (SNARE) — Hospitals, water treatment, emergency services depend on grid stability. Tail-risk policy constrains their operational choices: renewable-heavy grids with lower tail risk also have higher moment-to-moment variability and reliability uncertainty. These agents are trapped in the constraint (cannot build private grids economically), bear extraction through reduced reliability certainty, and receive only probabilistic protection (lower tail risk, not guaranteed availability). The constraint forces choice between catastrophic outcome prevention and operational control.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGULATORY/POLICY AUTHORITY ADOPTING TAIL-RISK FRAMING (ROPE) — Policy bodies that adopt catastrophic-tail-risk dominance as their decision criterion experience this as genuine coordination: they solve the collective action problem of catastrophe prevention through principle-based policy. Arbitrage exit option: can shift to expected-value framing at any point; maintain legitimacy through claim of scientific rigor. The constraint aligns their authority with public safety narrative and provides clear decision rules.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW / IRREDUCIBILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, catastrophic outcomes ARE structurally different from expected-value outcomes: a civilization-ending event (asteroid impact, uncontrolled climate cascade, cascading grid failure) has a different ontological status than a distribution of harms. Some philosophical frameworks treat tail-risk dominance as an immutable principle of rational collective action — catastrophes cannot be mathematically traded for other benefits. However, this mountain classification is vulnerable to false-summit detection: the constraint's beneficiary structure and suppression data suggest the 'immutable principle' naturalizes a contested policy choice.
constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acceptable_risk_for_energy__catastrophic_tail_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. The catastrophic-tail reading creates genuine extraction for fossil fuel incumbents (capital stranding, market share loss) and energy-affordability-dependent populations (higher costs, reliability uncertainty). The extractiveness increases over time (0.35 → 0.58) because early policy signals create optionality (gradual transition), but as the reading becomes formalized as policy principle, the extraction becomes harder to exit. The reading coordinates on catastrophe prevention but does so asymmetrically — some populations bear costs while others reap benefits. Suppression (0.62): Moderate-high, also rising. Suppression reflects the constraint's forcing of choice set reduction: fossil fuel producers cannot invest in carbon-intensive assets without policy penalty; energy-affordability-dependent populations must accept grid reliability uncertainty; grid operators cannot optimize for moment-to-moment stability at expense of tail-risk reduction. The suppression increases as the constraint becomes more formalized (policy mandate rather than advisory principle). Theater ratio (0.45): Low-moderate. The decision mechanism itself is relatively transparent — catastrophic outcomes are identified, policy principles are applied — but the underlying probability estimates that make tail-risk dominance coherent are highly contested and partly performative (confidence in models of unprecedented events is inherently theatrical).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits severe perspectival divergence across structural positions. Potential catastrophe victims see protection (Mountain natural law, catastrophic prevention is immutable). Renewable energy advocates see coordination (Rope, solving collective action problem of climate transition). Fossil fuel incumbents see extraction (Snare, capital stranding without compensation). Energy-affordability populations see mixed coordination-extraction (Tangled Rope, protection and cost). Grid reliability engineers see extraction (Snare, reduced operational control). Regulatory authority sees coordination (Rope, principle-based governance). The analytical observer risks false-summit classification (Mountain, naturalizing contested policy choice). The gap reveals that catastrophic-tail dominance is NOT a natural law but a distributional choice that benefits some victims at cost to others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural position relative to extraction flow. Renewable energy advocates (beneficiaries, mobile, organized) experience low d (0.15–0.25), giving them negative or minimal effective extraction — the policy benefits them directly and they can adapt to multiple energy regimes. Fossil fuel industry (victim of stranded assets, constrained to capital reallocation, powerful) experiences high d (0.70–0.80) — they bear substantial extraction with limited exit options. Energy-affordability populations (both beneficiary of catastrophe prevention and victim of cost inflation, constrained by economic dependency) experience moderate d (0.55–0.65) — mixed relationship produces tangled-rope experience. The analytical observer (observing from universal scope, analytical position) derives d from the aggregate victim/beneficiary structure, which tips toward extraction dominance (net d ≈ 0.72), producing a snare-like effective experience even from the mountain logical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (apparent contradiction between classification and ethical content) is resolved by recognizing that 'acceptable risk' is not a single constraint but a contested kernel with multiple legitimate readings. The catastrophic-tail-reading produces tangled rope at the aggregate level (genuine coordination on catastrophe prevention + asymmetric extraction from some groups) but appears as snare from fossil fuel industry and grid-reliability-dependent populations. The expected-value-reading, by contrast, would appear as rope from most perspectives (optimized expected-value distribution, transparent decision rules) but might appear as snare to populations bearing tail-risk exposure. The readings genuinely coexist in actual policy (some jurisdictions use tail-risk dominance, others use expected-value framework), and no reading logically forecloses the others — they represent different legitimate distributions of values and risk tolerance. The classification RESOLVES the mandatrophy by refusing to pick a single 'correct' type and instead mapping the perspectival structure: different observers see different types because they occupy different structural positions, and all classifications are accurate from their respective contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_probability_estimation_ambiguity,
    'How should probability be estimated for low-frequency, high-consequence events where historical data is sparse or non-existent?',
    'Empirical validation of tail-risk probability models against historical catastrophe frequencies; comparison of models with different epistemic assumptions (Bayesian priors, model averaging, structural uncertainty quantification)',
    'If probability estimates are reliable: tail-risk dominance is rationally defensible. If probability estimates are systematically biased (optimistic or pessimistic): the reading''s core principle is applied to a distorted input, and classification shifts toward expected-value reading being more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tail_probability_estimation_ambiguity, empirical, 'Reliability of tail-risk probability estimation for unprecedented events').

omega_variable(
    catastrophe_definition_boundary,
    'What constitutes a catastrophe for policy purposes? Is it defined by death toll, economic loss, ecosystem collapse, civilization disruption, or some weighted combination?',
    'Policy analysis of actual catastrophe thresholds used in risk decisions; survey of expert definitions across domains (climate, nuclear, pandemic, financial); identification of whether policy treats different catastrophe types as equivalent or weighted differently',
    'If catastrophe boundary is narrow (only civilization-ending): tail-risk dominance affects narrow subset of policies. If boundary is broad (any outcome exceeding threshold): tail-risk framing dominates nearly all collective decisions, increasing suppression and extraction dramatically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_definition_boundary, conceptual, 'Definition of catastrophe for policy classification').

omega_variable(
    victim_weighting_across_time_and_space,
    'Should all potential victims of a catastrophe be weighted equally in policy regardless of probability and temporal/spatial distance, or should weight scale with probability and proximity?',
    'Policy analysis of discount rates, risk tolerance levels, and affected-population weighting in actual decisions; comparison of tail-risk vs expected-value policies'' distributional impacts across income, geography, time horizons',
    'If equal weighting by policy design: constraint''s suppression and extractiveness values are accurate. If weighting is proximity/probability-scaled: tail-risk reading becomes less coherent (the catastrophe is weighted less than it appears), and expected-value reading better describes actual policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_weighting_across_time_and_space, preference, 'Weighting scheme for distant/low-probability victims').

omega_variable(
    cost_bearers_vs_beneficiaries_asymmetry,
    'Are the populations bearing the cost of tail-risk mitigation (through energy affordability, reliability constraints) materially overlapping with the populations who would benefit from catastrophe prevention?',
    'Geographic mapping of renewable energy infrastructure location vs fossil fuel plant location vs disaster-vulnerability distribution; income-stratified analysis of energy costs vs climate-vulnerability; temporal analysis of who bears costs now vs who benefits from prevention later',
    'If asymmetric (poor regions bear costs, wealthy regions reap benefits): extraction is severe, snare perspective is accurate, and constraint enables unjust distribution. If overlapping: coordination is genuine, tangled-rope classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_bearers_vs_beneficiaries_asymmetry, empirical, 'Whether cost-bearers and beneficiaries of tail-risk mitigation overlap').

omega_variable(
    kernel_reading_contest__expected_value_vs_catastrophic_tail,
    'Is the catastrophic-tail-dominance reading''s core premise (catastrophic outcomes dominate policy regardless of probability) logically foreclose-able by the expected-value reading, or do both remain live options within the same legitimacy framework?',
    'Logical analysis of the axioms: Does an expectation-value framework that acknowledges tail risks but weights them by probability logically rule out a framework that weights all potential victims equally regardless of probability? Can both frameworks coexist as live policy options?',
    'If forecloses: the readings are genuinely incompatible — only one can be adopted within a coherent policy framework. If coexists_with: the readings represent different legitimate distributions of values (risk tolerance, justice definitions), and both can persist in different jurisdictions or policy domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__expected_value_vs_catastrophic_tail, conceptual, 'Logical relationship between catastrophic-tail and expected-value readings of acceptable-risk kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_tail_tr_t0, acceptable_risk_for_energy__catastrophic_tail_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cat_tail_tr_t5, acceptable_risk_for_energy__catastrophic_tail_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(cat_tail_tr_t10, acceptable_risk_for_energy__catastrophic_tail_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(cat_tail_be_t0, acceptable_risk_for_energy__catastrophic_tail_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cat_tail_be_t5, acceptable_risk_for_energy__catastrophic_tail_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cat_tail_be_t10, acceptable_risk_for_energy__catastrophic_tail_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cat_tail_su_t0, acceptable_risk_for_energy__catastrophic_tail_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cat_tail_su_t5, acceptable_risk_for_energy__catastrophic_tail_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(cat_tail_su_t10, acceptable_risk_for_energy__catastrophic_tail_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_reading, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_reading, acceptable_risk_for_energy__expected_value_reading).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_reading, acceptable_risk_for_energy__precautionary_reading).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_reading, climate_policy_carbon_price_incidence).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_reading, grid_reliability_vs_decarbonization_tradeoff).

% DUAL FORMULATION NOTE:
% acceptable_risk_for_energy is a contested kernel with three structurally distinct readings: catastrophic_tail_reading (this constraint), expected_value_reading, and precautionary_reading. Each reading has its own ε, its own beneficiary/victim structure, and its own classification profiles. They are not the same constraint viewed from different angles — they are genuinely different axiomatizations of what constitutes acceptable risk. The readings affect downstream constraints (carbon pricing, grid optimization) differently. The network links preserve the kernel structure while respecting ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
