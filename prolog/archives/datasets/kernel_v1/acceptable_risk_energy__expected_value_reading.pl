% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_reading, []).

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
 *   constraint_id: acceptable_risk_energy__expected_value_reading
 *   human_readable: Acceptable Risk via Expected Value (Deaths-per-TWh Aggregation)
 *   domain: energy_policy/risk_governance/nuclear_economics
 *
 * SUMMARY:
 *   The expected-value reading of acceptable risk aggregates energy source
 *   mortality across the full probability distribution without categorical
 *   weighting of accident scenarios, rendering nuclear power's low
 *   operational death rate (0.07 deaths/TWh) statistically superior to coal
 *   (24 deaths/TWh) and gas (4 deaths/TWh). This reading dominates risk
 *   assessment institutions globally (UNECE, IPCC, IAEA) and legitimates
 *   nuclear deployment policies in climate-decarbonization contexts. However,
 *   the expected-value framework systematically renders invisible the
 *   structural position of waste-sequestration populations (constrained
 *   across 10,000+ year timescales) and low-dose chronic-exposure cohorts
 *   (non-linear dose-response), while enabling incumbent fossil-fuel and
 *   nuclear industries to compete on aggregated mortality metrics that
 *   obscure categorical tail risks (Chernobyl/Fukushima exclusion zones,
 *   intergenerational consent violations). This constraint is ONE READING of
 *   a contested kernel: the acceptable_risk_energy kernel has multiple
 *   readings instantiating different risk aggregation methods
 *   (expected-value, catastrophic-tail, comparative-harm). Each reading
 *   produces different victim sets, different beneficiary structures, and
 *   different constraint types. The expected-value reading is structurally a
 *   tangled_rope: it provides genuine coordination benefit (enables rational
 *   fuel comparison during climate emergency) while embedding asymmetric
 *   extraction (benefits industries and present-generation decision-makers
 *   while imposing uncompensated risks on powerless future populations).
 *
 * KEY AGENTS:
 *   - Nuclear Industry: Primary beneficiary (institutional/arbitrage) — expected-value metric shifts nuclear from perceived catastrophe-prone to lowest-mortality option; captures climate-emergency coalition support
 *   - Coal/Gas Incumbents: Secondary beneficiary (institutional/arbitrage) — expected-value metric enables continued operation during transition period by obscuring chronic cumulative mortality behind daily death counts
 *   - Waste Sequestration Communities: Primary victim (powerless/trapped) — geographically and temporally imprisoned; expected-value aggregation of low-annual-probability × 10,000-year timescale renders their actual exposure invisible
 *   - Low-Dose Chronic Exposure Cohorts: Secondary victim (moderate/constrained) — siting-community residents, groundwater-dependent populations; dose-response nonlinearities obscured by linear aggregation
 *   - Climate Decarbonization Coalition: Organized beneficiary-and-user (organized/mobile) — treats expected-value as temporary coordination framework for the decarbonization window; sees sunset as renewables mature
 *   - International Risk Assessment Institutions: Institutional maintainers (institutional/constrained) — UNECE, IPCC, IAEA maintain expected-value protocols as formal standard despite recognized inadequacy; constrained by member-state negotiation requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_reading, 0.38).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_reading, 0.48).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_reading, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_reading, "Acceptable Risk via Expected Value (Deaths-per-TWh Aggregation)").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_reading, "energy_policy/risk_governance/nuclear_economics").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_reading, '3135b429-59df-488b-87ae-341e3dad36d8').
narrative_ontology:cs_kernel_codification('3135b429-59df-488b-87ae-341e3dad36d8', formalized).
narrative_ontology:cs_authority_grounding('3135b429-59df-488b-87ae-341e3dad36d8', extraction).
narrative_ontology:cs_interpretation_layer_present('3135b429-59df-488b-87ae-341e3dad36d8').
narrative_ontology:cs_reading_relation('3135b429-59df-488b-87ae-341e3dad36d8', acceptable_risk_energy__catastrophic_tail_reading, coexists_with).
narrative_ontology:cs_reading_relation('3135b429-59df-488b-87ae-341e3dad36d8', acceptable_risk_energy__comparative_harm_reading, coexists_with).
narrative_ontology:cs_axiom('3135b429-59df-488b-87ae-341e3dad36d8', foundational, expected_value_maximization_axiom).
narrative_ontology:cs_axiom_status(expected_value_maximization_axiom, holdable).
narrative_ontology:cs_axiom_grounding('3135b429-59df-488b-87ae-341e3dad36d8', expected_value_maximization_axiom, empirically_contingent).
narrative_ontology:cs_axiom('3135b429-59df-488b-87ae-341e3dad36d8', foundational, timescale_homogenization_axiom).
narrative_ontology:cs_axiom_status(timescale_homogenization_axiom, holdable).
narrative_ontology:cs_axiom_grounding('3135b429-59df-488b-87ae-341e3dad36d8', timescale_homogenization_axiom, instrumental).
narrative_ontology:cs_reference_frame('3135b429-59df-488b-87ae-341e3dad36d8', rational_risk_governance_statistical_optimization).
narrative_ontology:cs_drift_state('3135b429-59df-488b-87ae-341e3dad36d8', contemporary_climate_emergency_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3135b429-59df-488b-87ae-341e3dad36d8', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_reading, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_reading, coal_gas_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_reading, centralized_generation_operators).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_reading, categorical_tail_risk_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_reading, low_dose_chronic_exposure_cohorts).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_reading, waste_sequestration_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NUCLEAR INDUSTRY (ROPE) — Experiences expected-value aggregation as legitimate coordination mechanism. The metric enables comparative risk assessment, shifting nuclear from perceived catastrophe-prone to lowest-mortality-per-TWh technology. Benefits from statistical reading that classifies accidents by frequency×severity rather than categorical worst-case. Net beneficiary with genuine coordination function: enables rational technology selection.
constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: SITING COMMUNITIES (TANGLED ROPE) — Constrained exit (cannot opt out of proximity; relocation cost prohibitive). The expected-value metric provides some benefit (transparent probabilistic risk communication replaces opaque assurance). But metric systematically underweights tail events and chronic low-dose exposure — community bears disproportionate weight in probability distribution (high-probability low-harm events weighted equally with low-probability catastrophic ones). Coordination function exists but asymmetric extraction embedded in how risk is aggregated.
constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WASTE SEQUESTRATION POPULATIONS (SNARE) — Geographically trapped, multi-generational (10,000+ year institutional continuity required for waste storage). Expected-value framing aggregates the 10,000-year tail — extremely low annual probability of breach weighted against civilization-scale consequences — into a 'manageable' expected mortality. The metric's mathematical structure renders the victim population's actual risk exposure invisible: low annual probability looks acceptable until multiplied by generational timescale. No coordination benefit, maximum suppression via metric aggregation.
constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: CLIMATE DECARBONIZATION COALITION (SCAFFOLD) — Organized actors (climate scientists, energy modelers, policy networks) treat expected-value aggregation as temporary coordination framework for the decarbonization window (2025-2050). The metric solves the immediate problem: comparing nuclear's actual operational mortality to fossil fuels' immediate kill rate (coal: 24 deaths/TWh, gas: 4 deaths/TWh, nuclear: 0.07 deaths/TWh operationally). This enables rational fuel selection during climate emergency. Exit path (long-term: renewable build-out + storage maturity + waste solutions mature) implies sunset — expected value becomes unnecessary once decarbonization achieves base load without nuclear. Theater low (metric is functionally used). Coordination with sunset.
constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL RISK ASSESSMENT INSTITUTIONS (PITON) — UNECE, IPCC, IAEA maintain expected-value protocols as formal standard despite recognition that tail risk aggregation is theoretically inadequate. The metric persists through institutional inertia: reorienting frameworks toward catastrophic-scenario analysis or weighted-tail-risk approaches would require renegotiating legitimacy with member states. Theater is moderate (institutions publish caveats about tail-risk limitations) but functional capacity has degraded — the frameworks are known to be inadequate for sequestration timescales and low-frequency/high-consequence scenarios. Maintenance cost (publications, workshops documenting limitations) exceeds functional benefit.
constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EXPECTED VALUE AS NATURAL LAW (MOUNTAIN) — From a mathematical-foundational perspective, expected value aggregation IS the correct risk metric under von Neumann-Morgenstern axioms (rational decision theory). The metric is not constructed but discovered: it follows logically from preferences over lotteries. This perspective risks naturalizing a mathematical abstraction as if it were a law of nature. However, the false-summit detector reveals the structural problem: the metric's appropriateness is axiom-dependent (von Neumann-Morgenstern), not universal. Different axiom systems (minimax, regret, catastrophic-bounds-first) produce different aggregation methods. The mountain classification is therefore contingent on mathematical premises, not inherent to reality.
constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acceptable_risk_energy__expected_value_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(acceptable_risk_energy__expected_value_reading, TR),
    TR >= 0.70.

:- end_tests(acceptable_risk_energy__expected_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The expected-value reading provides genuine coordination benefit — enables rational fuel comparison and supports climate decarbonization — but embeds extraction through metric design: present-generation decision-makers benefit from a risk aggregation method that renders future populations' and tail-risk populations' exposure statistically invisible. The extraction is not malicious but structural — the mathematical framework itself is the mechanism. Suppression (0.48): Moderate. Siting communities and waste-sequestration populations face significant barriers to exit (geographic entrenchment, relocation cost, institutional inertia in policy), but the primary suppression mechanism is epistemic — the expected-value metric itself suppresses perception of actual risk through mathematical aggregation. Theater ratio (0.55): Moderate. Expected-value calculations are transparent and defensible within decision-theoretic frameworks, but the framework selection itself is performed theater — presenting one axiom system as if it were universal or natural when alternative aggregation methods (minimax, catastrophic-bounds-first, weighted-tail) exist and would produce different policy conclusions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates distinct perspectival readings arising from the same mathematical framework applied with different temporal and categorical boundaries. The nuclear industry and decarbonization coalition see coordination and legitimate risk comparison (rope/scaffold) because expected-value aggregation enables technology competition at their timescale (80-year facility lifespans, present-generation climate policy). Siting communities see mixed coordination and extraction (tangled_rope) because they benefit from transparent probabilistic communication but the metric systematically underweights their chronic exposure and low-frequency/high-consequence siting-specific risks. Waste-sequestration populations see pure extraction (snare) because the metric's mathematical structure renders their 10,000-year institutional burden invisible — low annual probability averaged across generational timescales produces 'acceptable' expected mortality that masks their actual irreversible exposure. The analytical observer risks seeing the expected-value metric as a natural law (mountain) — a discovery of decision theory rather than a choice among axiomatically distinct frameworks. The false-summit detector reveals the naturalization: expected value is one axiom system among others, and its selection distributes risk asymmetrically toward powerless, temporally-trapped populations.
 *
 * DIRECTIONALITY LOGIC:
 *   The expected-value reading locates different agents along the directionality spectrum through their structural relationship to the risk aggregation mechanism. Nuclear industry and climate coalition have low d (beneficiaries with mobile/arbitrage exit) — they experience negative or near-zero effective extraction. Siting communities have moderate d (constrained exit, victim status) — they experience moderate extraction through the metric's systematic underweighting of their chronic exposure and siting-specific tail risks. Waste-sequestration populations have near-maximal d (trapped exit, multi-generational victim status) — they experience severe extraction through the metric's mathematical aggregation of low-annual-probability across timescales they cannot exit. The expected-value metric itself functions as a directionality device: it translates structural power (ability to choose aggregation method) into mathematical authorization (the metric 'legitimates' decisions that benefit present institutional actors while imposing future costs on powerless populations).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through explicit reading-level analysis. The expected-value reading IS a tangled_rope: it solves a genuine coordination problem (comparing technologies for rational fuel selection) while embedding asymmetric extraction (renders future and tail-risk populations invisible through mathematical aggregation). The mandatrophy is not 'which reading is correct?' but 'who benefits from choosing this reading?' The constraint demonstrates that risk aggregation methods are not discovered truths but institutional choices with distributional consequences. Alternative readings (catastrophic-tail reading, comparative-harm reading) would classify different constraint types and produce different victim/beneficiary sets. The system's integrity depends on making the reading selection explicit and observable, not naturalizing the expected-value reading as mathematically necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_weighting_axiom_dependence,
    'Is expected-value aggregation the correct risk metric or is it one choice among axiomatically distinct frameworks (expected utility, minimax, regret-minimization, catastrophic-bounds-first)?',
    'Comparative analysis of decision-theoretic axiom systems and their risk-aggregation outputs. Empirical test: which framework predicts actual policy decisions? Which produces outcomes retrospectively justified as ''optimal''?',
    'If axiom-dependent: expected value is NOT a mountain but a tangled_rope with built-in extraction (benefits expected-value beneficiaries while rendering tail-weighted populations invisible). If universal: current framework correct and victim populations'' concerns are orthogonal to rational risk assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_risk_weighting_axiom_dependence, conceptual, 'Whether expected value is axiomatically universal or one choice among frameworks').

omega_variable(
    timescale_commensurability_problem,
    'Can expected-value mortality aggregation meaningfully compare 80-year human lifespans (nuclear operational workers, coal miners) to 10,000-year institutional continuity requirements (waste sequestration)?',
    'Discount-rate analysis: What social discount rate makes 10,000-year tail risks commensurate with 80-year individual exposure? Literature review: have risk economists justified the timescale commensurability?',
    'If incommensurable: expected-value metric is mathematically fraudulent when applied across timescale orders of magnitude — masks structural incomparability as aggregated risk. If commensurable: requires explicit discount-rate specification that is normative (not empirical) and highly contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timescale_commensurability_problem, empirical, 'Whether expected value can aggregate across 80-year and 10,000-year timescales').

omega_variable(
    chronic_exposure_nonlinearity,
    'Are low-dose chronic exposures (tritium in groundwater, radon from tailings) linearly aggregable with acute accident mortality, or do they exhibit dose-response nonlinearities that violate expected-value assumptions?',
    'Radiobiology dose-response curve literature; comparison of linear no-threshold (LNT) model against hormesis and hormetic-region models. Epidemiological data on low-dose cohorts.',
    'If nonlinear: expected-value aggregation commits a category error (treats dose-response curves as linear when they are not) — renders chronic exposure victim population''s actual risk invisible. If linear: LNT assumption justified and current aggregation valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronic_exposure_nonlinearity, empirical, 'Whether low-dose chronic exposure is linearly aggregable with acute mortality').

omega_variable(
    categorical_irreversibility_weighting,
    'Should Chernobyl/Fukushima-scale exclusion zones (indefinite land-use prohibition, multi-generational displacement) be aggregated by mortality count or weighted separately as categorical irreversibility threshold events?',
    'Policy analysis: how do regulatory frameworks actually treat exclusion zones? Are they modeled as low-probability-high-consequence or as constraints outside the expected-value frame? Precedent analysis in other domains (catastrophic ecosystem collapse, species extinction).',
    'If categorical: expected-value metric is fundamentally incorrect framing — some outcomes are not commensurable with mortality aggregation. If aggregable: current metric appropriate but requires defensible willingness-to-pay conversion (lives per square-km of exclusion zone).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_irreversibility_weighting, preference, 'Whether catastrophic exclusion zones are aggregable by mortality or categorical constraints').

omega_variable(
    intergenerational_consent_validity,
    'Can present-generation risk acceptance (expected-value decision) be justified on behalf of future waste-sequestration populations who did not consent and cannot exit?',
    'Philosophical analysis of intergenerational justice; comparison with other irreversible harm frameworks (climate, persistent toxins). Precedent: how do legal systems treat binding commitments imposed on non-consenting future parties?',
    'If invalid: expected-value framing is procedurally illegitimate regardless of mathematical correctness — constitutes imposition on powerless populations. If valid: current risk governance framework coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_consent_validity, preference, 'Whether present-generation risk acceptance is valid for future non-consenting populations').

omega_variable(
    reading_observability_distinction,
    'This constraint is ONE READING of the acceptable_risk_energy kernel. Which reading governs actual policy decisions in a given jurisdiction? Is the reading-to-decision mapping observable?',
    'Policy document analysis: trace regulatory decision pathways back to risk aggregation method. Interviews: which risk metric do decision-makers cite when justifying deployment/prohibition? Institutional archaeology: when did shifts between readings occur and what triggered them?',
    'If one reading dominates: the ''minority'' readings are systematically suppressed and constitute genuine victims. If readings coexist: different jurisdictions instantiate different readings and comparison enables outcome analysis (which reading produces better actual safety outcomes?).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_observability_distinction, empirical, 'Which reading of acceptable risk governs actual policy decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(accept_risk_ev_tr_t0, acceptable_risk_energy__expected_value_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(accept_risk_ev_tr_t15, acceptable_risk_energy__expected_value_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(accept_risk_ev_tr_t30, acceptable_risk_energy__expected_value_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(accept_risk_ev_be_t0, acceptable_risk_energy__expected_value_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(accept_risk_ev_be_t15, acceptable_risk_energy__expected_value_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(accept_risk_ev_be_t30, acceptable_risk_energy__expected_value_reading, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_reading, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_reading, acceptable_risk_energy__catastrophic_tail_reading).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_reading, acceptable_risk_energy__comparative_harm_reading).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_reading, waste_sequestration_burden_intergenerational).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_reading, low_dose_chronic_exposure_nonlinearity).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel has three structurally distinct readings with different ε values and victim sets. This file instantiates the expected-value reading (ε=0.38, tangled_rope). The catastrophic-tail reading would have higher ε due to exclusion-zone irreversibility; the comparative-harm reading would have lower ε by rendering probabilistic futures secondary. These are not observables of the same constraint but commitments to different risk aggregation axioms. Each reading is a separate constraint story linked via network.affects_constraints to enable comparative analysis of which reading governs actual policy decisions and what distributional consequences follow.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
