% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__comparative_harm_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__comparative_harm_reading, []).

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
 *   constraint_id: acceptable_risk_energy__comparative_harm_reading
 *   human_readable: Acceptable Risk via Comparative Harm Analysis in Energy Portfolio
 *   domain: energy_policy/risk_governance/climate_justice
 *
 * SUMMARY:
 *   The acceptable_risk_energy kernel defines what counts as 'acceptable' in
 *   energy infrastructure decisions when multiple catastrophic risks are
 *   present. This constraint instantiates the COMPARATIVE-HARM READING: a
 *   framework that incorporates climate-related mortality and ecosystem loss
 *   into the risk denominator, making nuclear deployment appear as a
 *   necessary harm-reduction mechanism rather than an additional catastrophic
 *   risk. Under this reading, populations subject to climate migration, heat
 *   stress, freshwater depletion, and agricultural failure enter the victim
 *   set of NON-DEPLOYMENT, inverting the traditional nuclear-risk framing.
 *   The constraint exhibits the characteristic structure of a tangled_rope:
 *   it provides genuine coordination benefit (forcing explicit comparison of
 *   energy system harms rather than treating nuclear as an isolated decision)
 *   while generating asymmetric extraction (climate-vulnerable populations
 *   bear both climate risk AND nuclear deployment risk, while anti-nuclear
 *   advocates preserve their risk preferences at others' expense). The
 *   constraint's extractiveness (0.58) reflects that the reframing does
 *   impose real costs on some agents — it removes the option to reject
 *   nuclear while accepting climate risk 'in principle' and then
 *   externalizing the actual climate harm. Theater ratio (0.62) reflects that
 *   regulatory bodies invoking comparative-harm often omit explicit baseline
 *   scenarios, making their risk frames appear objective when they encode
 *   specific (and often hidden) baseline assumptions.
 *
 * KEY AGENTS:
 *   - Climate-Vulnerable Populations (global South, small island states, flood zones): Victim group (powerless/trapped) — bear both climate risk and nuclear-risk acceptance; no exit option under this reading
 *   - Coal-Dependent Regional Economies (Appalachia, Eastern Europe, Australia): Mixed victim/beneficiary (moderate/constrained) — bear accelerated coal exit costs but benefit from lower-carbon baseload and air quality improvements; high transition cost but some agency
 *   - Nuclear Industry and Deployment Advocates (utilities, technology vendors, some climate scientists): Primary beneficiary (institutional/arbitrage) — experience the reading as solving coordination problem; gain licensing pathways and investment certainty
 *   - Anti-Nuclear Environmental Coalition (some NGOs, radiation-sensitive activism): Secondary victim (organized/constrained) — experience the reading as extraction (their risk priorities imposed on others) but also responsible for preventing degradation of safety standards
 *   - Renewable + Storage Coalition (technology companies, some climate advocates): Temporary institutional actor (organized/mobile) — occupy scaffold position; see comparative-harm as temporary framing pending renewable maturity
 *   - Nuclear Regulatory Bodies: Institutional actor (institutional/arbitrage) — maintain performative risk assessment while baseline assumptions remain implicit; both pro- and anti-nuclear coalitions benefit from regulatory theater
 *   - Analytical Observer: Sees the reading as one choice among contested framings; recognizes false-summit risk in any naive naturalization of the comparative-harm logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__comparative_harm_reading, 0.58).
domain_priors:suppression_score(acceptable_risk_energy__comparative_harm_reading, 0.65).
domain_priors:theater_ratio(acceptable_risk_energy__comparative_harm_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__comparative_harm_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__comparative_harm_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_energy__comparative_harm_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__comparative_harm_reading, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__comparative_harm_reading, "Acceptable Risk via Comparative Harm Analysis in Energy Portfolio").
narrative_ontology:topic_domain(acceptable_risk_energy__comparative_harm_reading, "energy_policy/risk_governance/climate_justice").

domain_priors:requires_active_enforcement(acceptable_risk_energy__comparative_harm_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__comparative_harm_reading, 'dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7').
narrative_ontology:cs_kernel_codification('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', formalized).
narrative_ontology:cs_authority_grounding('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', extraction).
narrative_ontology:cs_interpretation_layer_present('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7').
narrative_ontology:cs_reading_relation('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', acceptable_risk_energy__catastrophic_tail_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', acceptable_risk_energy__expected_value_reading, influences).
narrative_ontology:cs_axiom('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', foundational, climate_harm_energy_system_dependent).
narrative_ontology:cs_axiom_status(climate_harm_energy_system_dependent, holdable).
narrative_ontology:cs_axiom_grounding('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', climate_harm_energy_system_dependent, empirically_contingent).
narrative_ontology:cs_axiom('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', foundational, distributional_incidence_morally_relevant).
narrative_ontology:cs_axiom_status(distributional_incidence_morally_relevant, holdable).
narrative_ontology:cs_axiom_grounding('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', distributional_incidence_morally_relevant, deontological).
narrative_ontology:cs_reference_frame('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', energy_system_aggregate_harm_minimization).
narrative_ontology:cs_drift_state('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', renewable_intermittency_uncertainty_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dbe3e0f1-58eb-4c9c-9ddf-041b1f173ad7', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__comparative_harm_reading, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__comparative_harm_reading, nuclear_deployment_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__comparative_harm_reading, climate_vulnerable_populations_under_comparative_analysis).
narrative_ontology:constraint_victim(acceptable_risk_energy__comparative_harm_reading, radiation_exposure_risk_bearers).
narrative_ontology:constraint_victim(acceptable_risk_energy__comparative_harm_reading, fossil_fuel_displaced_coal_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__comparative_harm_reading, nuclear_waste_legacy_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — This reading frames climate-harm victims in the baseline scenario as bearing extraction from anti-nuclear positions. They experience the constraint as a snare: forced to accept elevated climate risk because high-consequence but lower-probability nuclear risks are treated as categorically unacceptable. No exit — trapped in geography and absent carbon-free alternatives. Extraction is real under this framing: their aggregate harm is sacrificed for others' risk preferences.
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COAL-DEPENDENT ECONOMIES (TANGLED ROPE) — Experience the constraint as both coordination and extraction. Nuclear deployment offers genuine coordination benefit (lower-carbon energy, reduced local air pollution casualties) but imposes asymmetric costs (accelerated coal exit, worker retraining burden, regional economic transition concentrated on coal communities). Constrained exit: they can adapt but only at high cost. Partial beneficiary (cleaner air, climate benefit) and partial victim (economic dislocation).
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NUCLEAR ADVOCATES (ROPE) — From the comparative-harm reading, advocates experience the constraint as pure coordination: the risk frame solves the legitimate problem of decarbonizing energy without forcing choice between nuclear and climate catastrophe. They benefit from the reframing (licensing pathways, public acceptance, investment certainty) and frame themselves as enabling coordination (providing carbon-free baseload). Arbitrage exit: can exit into other technologies if nuclear becomes uneconomical.
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-NUCLEAR COALITION (TANGLED ROPE) — From the comparative-harm reading, the coalition experiences extraction: their risk-prioritization logic (nuclear zero-tolerance) inadvertently imposes higher aggregate harm on climate-vulnerable populations. But they also provide genuine coordination function (preventing rushed decommissioning of safety systems, maintaining scrutiny of waste management). Constrained by moral commitment to non-nuclear, but also constrained by realization that their position may maximize total harm. Active enforcement: must argue against their own reading's implications.
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NUCLEAR REGULATORY SYSTEM (PITON) — The comparative-harm reading exposes regulatory theater: risk assessment procedures maintain appearance of independent evaluation while the denominator (acceptable baseline risk from fossil fuels) remains invisible in the risk frame. Regulatory theater persists because it serves both pro- and anti-nuclear coalitions: advocates use regulations to signal safety; opponents use them to create licensing friction. Theater ratio high because actual risk comparison is rarely foregrounded in regulatory decision-making.
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RENEWABLES-FIRST COALITION (SCAFFOLD) — From the comparative-harm reading, this coalition occupies a temporary position: if renewable capacity + storage + demand flexibility can reliably deliver baseload at lower cost than nuclear + waste management, then the comparative-harm frame becomes moot (renewables dominate both on risk and economics). Coalition sees the constraint as having a sunset: 10-15 years for storage technology maturity and grid coordination infrastructure. Mobile exit: can switch emphasis to nuclear as backup if renewables timeline fails.
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (NAIVE MOUNTAIN) — This reading sees the risk trade-off as an immutable natural law: decarbonization inherently requires choosing between nuclear catastrophe risk and climate catastrophe risk; there is no third option. This perspective naturalizes the comparative-harm frame as a discovery about reality rather than a constructed analytical choice. FSM candidate: the engine will flag this as false summit because beneficiaries are identifiable (nuclear advocates, climate-vulnerable populations accepting nuclear risk) and the 'natural law' framing obscures the policy choice that benefits some and harms others.
constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__comparative_harm_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acceptable_risk_energy__comparative_harm_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__comparative_harm_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acceptable_risk_energy__comparative_harm_reading, TR),
    TR >= 0.70.

:- end_tests(acceptable_risk_energy__comparative_harm_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading generates real asymmetric burdens — it forces populations to accept nuclear deployment risk in order to escape climate risk, removing the option to reject both. But the extraction is not maximal (snare-level 0.66+) because: (1) nuclear deployment genuinely reduces total energy-system harm under the comparative-harm frame, (2) beneficiaries (climate-vulnerable populations under the reading) do gain risk reduction, (3) other alternatives (renewables, gas, conservation) remain available in principle. The asymmetry lies in who bears the transaction cost of institutional change: anti-nuclear movements must reverse decades of organizing; coal economies must exit; populations must accept simultaneous risks. Suppression (0.65): High. The comparative-harm reading suppresses alternative risk frameworks through institutionalization of baseline assumptions — once the baseline is chosen, the conclusion follows mechanically. Suppression mechanisms include: regulatory capture (baseline assumptions written into law without public deliberation), epistemic authority (risk experts speak for climate-vulnerable populations without their participation), and temporal urgency (climate crisis is invoked to bypass deliberation on nuclear risk trade-offs). Theater ratio (0.62): Moderate-high. The comparative-harm frame offers genuine analytical insight (we should compare total harms rather than treating nuclear in isolation) but also performs theatrical functions (makes contested risk preferences appear empirical, justifies predetermined conclusions through baseline choice). The performance increases over time as the reading becomes institutionalized and the baseline assumptions recede from view.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces radical perspectival divergence. Climate-vulnerable populations under the reading see a SNARE (forced choice between catastrophes). Coal communities see TANGLED ROPE (genuine benefit but asymmetric cost). Nuclear advocates see ROPE (solving coordination problem). Anti-nuclear activists see TANGLED ROPE (their constraints on deployment now appear as extraction imposed on others). The regulatory system appears as PITON (theater persisting through institutional inertia). Renewables coalition sees SCAFFOLD (temporary framing pending technical maturity). The naive analytical observer risks seeing MOUNTAIN (an immutable trade-off inherent to decarbonization). The perspectival gap reveals that the 'comparative harm' framing is not a natural discovery but a constructed reading that becomes more constraining as it is institutionalized — it transforms from a deliberative choice (how should we compare harms?) into an apparently technical conclusion (nuclear is demonstrably beneficial).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by structural position in THIS READING'S CONSTRAINT. The comparative-harm reading constructs a new harm-comparison space, and agents occupy different positions within it. Climate-vulnerable populations: beneficiary by the reading's logic (nuclear reduces their total exposure) BUT their exit options are trapped (cannot opt out of either climate or nuclear risk), so d approaches 1.0 (high experienced extraction despite being nominally beneficiary — the reading constrains them to accept risks). Coal economies: mixed (beneficiary from lower-carbon energy, victim from economic transition), constrained exit → d ≈ 0.55. Nuclear advocates: beneficiary, arbitrage exit → d ≈ 0.15 (low extraction, high benefit). Anti-nuclear coalition: victim by the reading's logic (their risk-prioritization appears extractive to climate populations), but organized with some agency → d ≈ 0.65 (significant extraction but not maximal). The directionality divergence reveals the reading's redistributive logic: it shifts the beneficiary/victim frame such that previously powerless climate populations nominally benefit while previously powerful anti-nuclear movements appear extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The comparative-harm reading resolves mandatrophy not by finding 'the truth' about energy risk but by making explicit the choice of how to compare harms. The mandate is: GIVEN that we must choose between energy systems with different catastrophic profiles, THEN we should compare total harm including climate externalities. This resolves the mandatrophy by accepting (rather than denying) that the risk frame is constructed — but arguing that the comparative construction is more morally coherent than pretending nuclear risk is isolated from energy-system alternatives. The reading does not claim nuclear is safe (it isn't) or that climate risk doesn't matter (it does); it claims that EXCLUDING climate harm from the denominator is incoherent when climate harm is energy-system dependent. Different readings will reject this resolution: catastrophic-tail reading rejects any harm comparison; expected-value reading may accept comparison but reject distributional focus. But within the comparative-harm frame itself, the mandatrophy is stable — the constraint's role (make baseline assumptions explicit) is consistent with its classification (tangled_rope providing coordination at cost of asymmetric risk distribution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_harm_denominator_construction,
    'What harm profile constitutes the ''baseline'' against which nuclear risk is compared? Fossil-fuel status quo, natural-gas + intermittency, or hypothetical 100% renewables?',
    'Explicit auditing of baseline scenario in risk assessment documents; comparison of stated baseline across regulatory jurisdictions and NGO analyses',
    'Different baselines produce different comparative-harm conclusions. Fossil baseline → nuclear beneficial constraint (snare becomes rope/tangled_rope). Natural-gas baseline → nuclear marginal. Renewables baseline → nuclear unnecessary. The choice of baseline IS the reading — it determines whether the constraint narrows to rope or expands to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_harm_denominator_construction, conceptual, 'The choice of baseline scenario determines comparative-harm conclusion').

omega_variable(
    intermittency_cost_quantification,
    'What are the actual lifecycle costs of renewable intermittency (battery storage, grid hardening, gas peaker backup, demand response infrastructure) in terms of carbon, material extraction, and economic burden?',
    'Comprehensive lifecycle analyses of renewable + storage systems; empirical data on grid stability costs in high-renewable regions (Denmark, Germany, California); comparison of social welfare under different intermittency management strategies',
    'If intermittency costs are low: renewables dominate fossil + nuclear on all dimensions, and the comparative-harm frame collapses (scaffold sunset thesis confirmed). If intermittency costs are high: the frame remains relevant and the beneficiary/victim structure is stable. If costs are uncertain: the frame persists but with omega uncertainty — neither reading (comparative-harm vs catastrophic-tail) can claim empirical closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittency_cost_quantification, empirical, 'Lifecycle costs of renewable intermittency management').

omega_variable(
    temporal_risk_discounting_morality,
    'Is it legitimate to discount remote catastrophic risks (nuclear waste governance 1000+ years hence) relative to near-term certain harms (coal deaths, methane emissions, climate migration 20-50 years hence)?',
    'Philosophical coherence of different discounting frameworks; empirical data on intergenerational responsibility attitudes; analysis of how different discount rates change the comparative-harm conclusion',
    'High discount rates favor nuclear (reduces long-tail waste cost). Low discount rates favor renewables (makes remote nuclear failure more salient than immediate climate harms). This is partly empirical (how much weight should posterity bear in our decisions?) and partly preferential (do we value climate-vulnerable populations'' immediate survival more than our descendants'' waste management burden?). The comparative-harm reading assumes a particular discount rate — if the rate is made explicit and contested, the reading''s contingency becomes visible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_risk_discounting_morality, preference, 'Temporal discounting of catastrophic vs near-term harm').

omega_variable(
    radiation_risk_perception_vs_actuarial,
    'Do risk assessments using comparative-harm correctly weight radiation-specific psychological aversion (dread risk, unfamiliarity, catastrophic potential), or do they treat all harms as fungible expected values?',
    'Behavioral economics research on risk perception; analysis of whether regulatory risk thresholds are calibrated to psychological factors rather than actuarial expected value; comparison of public willingness-to-pay for radiation safety vs fossil-fuel pollution reduction',
    'If psychological aversion is rational (captures real information about catastrophic tail events or psychological vulnerability), then the comparative-harm frame is incomplete — it understates nuclear risk by treating dread as bias. If aversion is bias, then comparative-harm correctly reframes the tradeoff. This difference determines whether the constraint is legitimately tangled_rope (real asymmetric benefit) or performative (theater using ''comparative harm'' to naturalize risk preference).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radiation_risk_perception_vs_actuarial, empirical, 'Psychological vs actuarial treatment of radiation risk').

omega_variable(
    kernel_reading_contest_location,
    'Which sibling reading (catastrophic-tail vs expected-value) becomes foreclosed if this comparative-harm reading is adopted institutionally?',
    'Policy implementation analysis: do jurisdictions that adopt comparative-harm risk frames explicitly reject catastrophic-tail logic, or do they coexist as competing framings within the same institutions? Historical analysis of shifts from multi-reading (coexistent) to single-reading (foreclosed) institutional logic.',
    'If readings can coexist: institutions maintain plural risk logics and the kernel remains contested (policy becomes stable via institutional ambiguity). If readings foreclose each other: adoption of comparative-harm entails rejection of catastrophic-tail, and the omega becomes a prediction about institutional evolution under different adoption scenarios.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether comparative-harm reading institutionally forecloses sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__comparative_harm_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(risk_comp_tr_t0, acceptable_risk_energy__comparative_harm_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(risk_comp_tr_t5, acceptable_risk_energy__comparative_harm_reading, theater_ratio, 5, 0.57).
narrative_ontology:measurement(risk_comp_tr_t10, acceptable_risk_energy__comparative_harm_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(risk_comp_be_t0, acceptable_risk_energy__comparative_harm_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(risk_comp_be_t5, acceptable_risk_energy__comparative_harm_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(risk_comp_be_t10, acceptable_risk_energy__comparative_harm_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(risk_comp_su_t0, acceptable_risk_energy__comparative_harm_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(risk_comp_su_t5, acceptable_risk_energy__comparative_harm_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(risk_comp_su_t10, acceptable_risk_energy__comparative_harm_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__comparative_harm_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__comparative_harm_reading, 0.2).
narrative_ontology:affects_constraint(acceptable_risk_energy__comparative_harm_reading, acceptable_risk_energy__catastrophic_tail_reading).
narrative_ontology:affects_constraint(acceptable_risk_energy__comparative_harm_reading, acceptable_risk_energy__expected_value_reading).
narrative_ontology:affects_constraint(acceptable_risk_energy__comparative_harm_reading, renewable_intermittency_cost_underestimation).
narrative_ontology:affects_constraint(acceptable_risk_energy__comparative_harm_reading, coal_transition_distributional_harm).
narrative_ontology:affects_constraint(acceptable_risk_energy__comparative_harm_reading, climate_migration_governance_externality).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel is contested across three readings with structurally distinct harm denominators. This file instantiates the comparative-harm reading (nuclear as necessary harm-reduction). Sibling files for catastrophic-tail and expected-value readings use different baseline assumptions (catastrophic-tail: nuclear unacceptable regardless of climate harm; expected-value: all harms fungible, no distributional weight). The three readings do NOT coexist peacefully in institutional practice — they influence which research gets funded, which regulations get written, which populations bear burden. This reading affects downstream constraints by: (1) making coal-exit timing a key variable (faster exit makes comparative-harm logic stronger), (2) making renewable storage costs decisive (if storage costs fall, comparative-harm advantage declines), (3) shifting climate-migration governance from climate-only problem to energy-policy consequence (populations displaced by climate harm now appear as victims of non-deployment decisions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__comparative_harm_reading, powerless, 0.95).
constraint_indexing:directionality_override(acceptable_risk_energy__comparative_harm_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
