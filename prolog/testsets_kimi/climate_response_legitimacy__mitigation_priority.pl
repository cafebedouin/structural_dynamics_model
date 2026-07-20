% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate-response legitimacy holds that
 *   legitimate climate action must prioritize emissions reductions through
 *   technological innovation and carbon pricing, preserving economic growth
 *   while decoupling it from emissions. This reading dominates international
 *   climate institutions and OECD policy. It presents itself as solving a
 *   global collective-action problem, but structurally preserves growth
 *   trajectories for wealthy nations and technology sectors while offloading
 *   transition costs to fossil-fuel-dependent workers, constraining Global
 *   South development, and transferring intergenerational risk to future
 *   generations if decoupling fails. This story instantiates the
 *   mitigation_priority reading of the climate_response_legitimacy kernel;
 *   sibling readings include adaptation_priority and degrowth_transformation.
 *
 * KEY AGENTS:
 *   - international_climate_institutions: Agenda-setter (institutional/global) â designs and enforces the mitigation framework through IPCC cycles and NDC architectures
 *   - current_wealthy_nations: Primary beneficiary (powerful/global) â preserves growth trajectories and captures green-technology rents
 *   - green_technology_sector: Secondary beneficiary (powerful/global) â receives subsidy flows and regulatory mandates
 *   - future_generations: Primary target (powerless/universal) â bears locked-in warming risk if decoupling fails
 *   - fossil_fuel_dependent_workers: Secondary target (moderate/national) â bears transition costs and job displacement
 *   - global_south_developing_nations: Tertiary target (organized/global) â faces constrained development paths and underdelivered climate finance
 *   - climate_scientists: Analytical observer (institutional/global) â provides assessment and legitimation for the framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, 'e5daa933-2c7a-43ee-8136-798415be373f').
narrative_ontology:cs_kernel_codification('e5daa933-2c7a-43ee-8136-798415be373f', distributed).
narrative_ontology:cs_authority_grounding('e5daa933-2c7a-43ee-8136-798415be373f', expertise).
narrative_ontology:cs_interpretation_layer_present('e5daa933-2c7a-43ee-8136-798415be373f').
narrative_ontology:cs_reading_relation('e5daa933-2c7a-43ee-8136-798415be373f', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e5daa933-2c7a-43ee-8136-798415be373f', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('e5daa933-2c7a-43ee-8136-798415be373f', foundational, decoupling_feasibility).
narrative_ontology:cs_axiom_status(decoupling_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('e5daa933-2c7a-43ee-8136-798415be373f', decoupling_feasibility, empirically_contingent).
narrative_ontology:cs_axiom('e5daa933-2c7a-43ee-8136-798415be373f', foundational, growth_preservation_mandate).
narrative_ontology:cs_axiom_status(growth_preservation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e5daa933-2c7a-43ee-8136-798415be373f', growth_preservation_mandate, instrumental).
narrative_ontology:cs_reference_frame('e5daa933-2c7a-43ee-8136-798415be373f', market_led_decarbonization).
narrative_ontology:cs_drift_state('e5daa933-2c7a-43ee-8136-798415be373f', post_paris_accounting_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5daa933-2c7a-43ee-8136-798415be373f', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, current_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, green_technology_sector).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_developing_nations).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, green_growth_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, technological_optimism_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the framework for legitimate climate response through IPCC assessment cycles, NDC architectures, and carbon pricing templates. Enforce compliance through reporting mechanisms and climate finance conditionality. Dependent on nation-state funding and political buy-in for continued authority.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, international_climate_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Preserve economic growth trajectories and consumption levels while meeting nominal emission targets through carbon markets and offshore manufacturing. Capture green technology rents and carbon market financial flows. Exit options include carbon leakage, offset purchasing, and technological outsourcing.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, current_wealthy_nations, beneficiary,
    powerful, biographical, arbitrage, global).

% Receives subsidy flows, guaranteed offtake agreements, and regulatory mandates under decarbonization policy. Benefits from the technological-optimism narrative that preserves growth-dependent investment structures. Can pivot to other markets if policy shifts.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, green_technology_sector, beneficiary,
    powerful, biographical, mobile, global).

% Inherit the atmospheric and ecological outcome of current decoupling bets. If technological innovation fails to deliver absolute decoupling, they face locked-in warming, ocean acidification, and climate instability with no ability to opt out of the future they receive. Have no representation in current policy fora.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Bear the transition costs of carbon pricing and fossil phase-out through job displacement, wage suppression, and community decline. Expected to retrain for green jobs that may not materialize at comparable wages. Geographic concentration in extraction regions limits mobility.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_fuel_dependent_workers, payer,
    moderate, biographical, constrained, national).

% Requested to leapfrog fossil development without the historical subsidy wealth that built OECD economies. Face carbon border adjustments that penalize development paths. Climate finance promised for transition remains underdelivered. Constrained by debt and trade structures that limit alternative development models.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, global_south_developing_nations, payer,
    organized, generational, constrained, global).

% Produce the emissions scenarios and physical risk assessments that legitimate the mitigation-priority framing. Dependent on government funding and peer consensus. Can dissent individually but the assessment process aggregates toward conservative, consensus-driven conclusions that favor incremental, growth-compatible scenarios.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_scientists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduce global greenhouse gas emissions through price signals and technological innovation to solve the collective-action problem of atmospheric commons degradation.
% TRANSFER_FUNCTION: Moves transition costs and decoupling failure risks from current wealthy populations to fossil-fuel-dependent workers, Global South development trajectories, and future generations; moves subsidy flows and market expansion to green technology sectors.
% ABSENT_VOICES: Future generations have no seat at climate negotiations; degrowth advocates and Indigenous territorial sovereignty movements are structurally underrepresented in mainstream policy fora; fossil-fuel-dependent communities are consulted but rarely shape the agenda.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished overnight, national climate strategies would shift toward adaptation-first or degrowth transformation, carbon markets would collapse, green technology subsidy flows would reorganize, and the global institutional architecture for emissions accounting would dissolve.
% FOUNDING_PROBLEM: Atmospheric carbon accumulation poses a collective-action tragedy-of-the-commons threat to long-term human flourishing, requiring coordinated emission reductions that unregulated markets fail to deliver.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and the IPCC attest the physical problem is live from outside the beneficiary set. Labor and climate-justice movements attest the growth-preservation framing is contested from outside the green-technology beneficiary set. No independent party outside the benefiting nations and sectors corroborates that growth preservation is empirically necessary for the solution.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately-high because the constraint coordinates genuine emission reductions while simultaneously preserving wealth-concentrated growth and offloading structural transition costs to less powerful agents. Suppression (0.58) reflects the active marginalization of degrowth alternatives in policy discourse and the enforcement requirements of carbon pricing and NDC compliance. Theater ratio (0.45) captures the growing performative element: net-zero pledges, offset markets, and technological optimism that outpaces deployment. Accessibility collapse (0.48) is moderate â degrowth and deep adaptation alternatives persist in activist and academic spaces but are structurally excluded from mainstream policy fora. Resistance (0.55) reflects mounting climate-justice and labor opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (international institutions) experiences the constraint as necessary coordination architecture for a global commons tragedy. The beneficiary seats (wealthy nations, green tech) experience growth preservation and rent capture. The payer seats (future generations, fossil workers, Global South) experience risk offloading and cost-bearing. The engine should compute tangled_rope for payer seats, rope or scaffold for agenda-setter, and elevated effective extraction for future generations given their powerlessness and trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to current_wealthy_nations and green_technology_sector, who receive growth preservation, subsidies, and market expansion. Victim declarations map to future_generations (intergenerational risk), fossil_fuel_dependent_workers (transition costs), and global_south_developing_nations (development constraint). The structural asymmetry is clearest for future_generations: zero power, trapped exit, universal scope amplifies effective extraction. Current wealthy nations have arbitrage-grade exit (carbon leakage, outsourcing) and global scope, damping their directionality toward beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination-function requirement, this constraint might read as pure intergenerational extraction (snare). However, it does solve a genuine collective-action problem â unpriced carbon externalities â and the constraint's enforcement does produce some emission reductions. The tangled_rope classification captures that the coordination is real but asymmetric: the same carbon-pricing structure that coordinates abatement also extracts from workers and the future. The scaffold classification is rejected because there is no credible sunset clause â the constraint carries no declared transition to a post-growth or fully decoupled steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_status,
    'Is absolute decoupling of GDP from emissions and material throughput actually occurring at the scale and speed required to stabilize climate under the growth-preservation mandate?',
    'Historical decomposition analysis and future scenario validation against observed emissions trajectories and resource-use accounts.',
    'If decoupling is not feasible, the constraint offloads impossible risks to future generations and its coordination function is undermined by structurally false premises; if feasible, the extraction is the necessary cost of genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_empirical_status, empirical, 'Whether absolute decoupling is empirically achievable or an optimistic axiom.').

omega_variable(
    suppression_of_alternatives,
    'Is the dominance of mitigation-through-growth a result of genuine coordination superiority or structural suppression of degrowth and deep-adaptation framings in policy discourse?',
    'Discourse analysis of policy-fora participation, funding-allocation comparisons, and media-framing studies across national climate strategies.',
    'If structural suppression, the constraint''s effective extraction is higher than measured and its coordination function is partly theatrical; if genuine consensus, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives, conceptual, 'Whether alternative climate framings are marginalized by power or by demonstrable inferiority.').

omega_variable(
    intergenerational_risk_transfer,
    'Does the technological-optimism framing constitute a concealed intergenerational transfer of risk that is structurally equivalent to direct extraction?',
    'Scenario-weighted outcome analysis comparing mitigation-priority, adaptation-priority, and degrowth pathways across generational welfare distributions.',
    'Would reclassify future generations from potential victims to certain victims if the decoupling bet fails, and would raise the effective extractiveness computed for the powerless/universal seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_risk_transfer, preference, 'Whether intergenerational risk offloading is normatively equivalent to extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.15).
narrative_ontology:measurement(climate_mitigation_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.2).
narrative_ontology:measurement(climate_mitigation_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.25).
narrative_ontology:measurement(climate_mitigation_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.32).
narrative_ontology:measurement(climate_mitigation_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.38).
narrative_ontology:measurement(climate_mitigation_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.42).
narrative_ontology:measurement(climate_mitigation_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(climate_mitigation_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(climate_mitigation_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(climate_mitigation_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(climate_mitigation_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(climate_mitigation_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(climate_mitigation_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(climate_mitigation_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(climate_mitigation_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(climate_mitigation_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(climate_mitigation_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(climate_mitigation_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(climate_mitigation_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The climate_response_legitimacy kernel decomposes into three structurally distinct constraints. The mitigation_priority reading (this story) assigns costs to future generations and transition workers while preserving growth for wealthy nations; adaptation_priority and degrowth_transformation assign costs and benefits to different agent sets and carry different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
