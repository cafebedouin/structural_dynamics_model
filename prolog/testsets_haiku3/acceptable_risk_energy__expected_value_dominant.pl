% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value Dominant Risk Framing for Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint instantiates the EXPECTED-VALUE-DOMINANT reading of the
 *   contested kernel 'acceptable risk in energy policy.' Under this reading,
 *   acceptable risk is operationalized as minimizing aggregate expected harm
 *   across all energy pathways using mortality-per-TWh as the metric. Fossil
 *   fuel deaths (air pollution, mining) are counted annually and
 *   cumulatively; nuclear accidents are probability-weighted and therefore
 *   numerically suppressed. The reading does NOT assert that this is the only
 *   defensible framing—it asserts this is the specific technical and
 *   normative structure that has come to dominate policy analysis and capital
 *   allocation. The sibling readings (catastrophic-tail-dominant and
 *   option-value-preserving) are structurally distinct constraints with
 *   different ε values, different victim/beneficiary sets, and different
 *   classifications; they are NOT alternative perspectives on this
 *   constraint.
 *
 * KEY AGENTS:
 *   - Nuclear industry: benefits from low expected-value scores for nuclear pathways; legitimacy of expansion depends on probability discounting
 *   - Fossil fuel affected communities: bear distributed annual mortality that accumulates heavily in the metric; economically dependent on extraction wages
 *   - Quantitative risk modelers: set and maintain the metric; professional authority invested in expected-value framework
 *   - Policy makers: adopt metric as decision rule; defer value choices to mathematical optimization
 *   - Tail-risk advocates: excluded from metric construction; would frame acceptable risk differently
 *   - Climate scientists: benefit from metric's alignment with decarbonization advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value Dominant Risk Framing for Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'db652261-839b-4511-a503-8cbe25af221d').
narrative_ontology:cs_kernel_codification('db652261-839b-4511-a503-8cbe25af221d', distributed).
narrative_ontology:cs_authority_grounding('db652261-839b-4511-a503-8cbe25af221d', expertise).
narrative_ontology:cs_interpretation_layer_present('db652261-839b-4511-a503-8cbe25af221d').
narrative_ontology:cs_reading_relation('db652261-839b-4511-a503-8cbe25af221d', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('db652261-839b-4511-a503-8cbe25af221d', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('db652261-839b-4511-a503-8cbe25af221d', foundational, expected_value_commensurability).
narrative_ontology:cs_axiom_status(expected_value_commensurability, holdable).
narrative_ontology:cs_axiom_grounding('db652261-839b-4511-a503-8cbe25af221d', expected_value_commensurability, instrumental).
narrative_ontology:cs_axiom('db652261-839b-4511-a503-8cbe25af221d', foundational, probability_quantification_epistemically_grounded).
narrative_ontology:cs_axiom_status(probability_quantification_epistemically_grounded, holdable).
narrative_ontology:cs_axiom_grounding('db652261-839b-4511-a503-8cbe25af221d', probability_quantification_epistemically_grounded, empirically_contingent).
narrative_ontology:cs_reference_frame('db652261-839b-4511-a503-8cbe25af221d', scientific_risk_optimization).
narrative_ontology:cs_drift_state('db652261-839b-4511-a503-8cbe25af221d', contemporary_epistemic_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db652261-839b-4511-a503-8cbe25af221d', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, quantitative_risk_modelers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_affected_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, air_pollution_victims).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, mining_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, policy_makers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, climate_scientists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The expected-value metric ranks nuclear pathways as dramatically safer than fossil pathways because rare accidents (Chernobyl, Fukushima) are probability-weighted into low statistical contribution to per-TWh mortality, while fossil fuel deaths (air pollution from combustion, mining accidents) accumulate annually and cumulatively at high rates. This metric structure makes nuclear expansion appear technically justified on safety grounds alone, independent of climate or economic arguments. Capital, regulatory approval, and political support flow to pathways that score well on this metric. The industry benefits directly from the metric's framing because it transforms what might be framed as 'rare catastrophic risk' (which would invoke precaution) into 'low expected value' (which appears to justify expansion).
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Coal-mining towns, oil-extraction regions, and areas downwind of refineries and coal plants bear the routine, annual mortality from occupational exposure and ambient air pollution. These communities face multiple mortality pathways: mining accidents, occupational disease, respiratory illness from emissions. Under the expected-value metric, their deaths are counted—often meticulously in the per-TWh calculation—but simultaneously rendered less politically urgent than rare catastrophic risks. Their exit is trapped because alternative livelihoods are scarce; transition away from extraction or fossil-dependent economies requires resources and time that policy does not provide. The metric makes their suffering visible to analysis while simultaneously legitimating policy choices that do not prioritize their protection.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_affected_communities, payer,
    powerless, biographical, trapped, local).

% Populations exposed to chronic air pollution from fossil fuel combustion (particulate matter, sulfur dioxide, nitrogen oxides, ozone) suffer elevated cardiovascular and respiratory mortality and morbidity. These harms are distributed across large populations; any individual's probability of dying from pollution-related causes is small, but the population-level toll is large. The expected-value metric counts all these deaths in the per-TWh calculation for fossil pathways, which is why fossil pathways score poorly on the metric. However, the very mechanism that makes their suffering visible in numbers (mortality-per-unit-energy) also means they are averaged into a statistical sum. Their exit is constrained by inability to relocate or avoid ambient exposure; they cannot arbitrage.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, air_pollution_victims, payer,
    powerless, biographical, constrained, regional).

% Coal and uranium miners face occupational mortality from accidents, silicosis, radon exposure, and other mining-specific harms. Their mortality is included in the per-TWh metric for both fossil and nuclear pathways—coal mining deaths weight coal's per-TWh score upward; uranium mining deaths weight nuclear's score slightly upward. However, the absolute number of mining deaths is smaller for uranium than coal, so the net effect is that mining workers' contribution to nuclear's score is modest. Their exit is constrained by economic dependence on extraction wages and geographic concentration in mining regions. As an observer role, they recognize they bear harm regardless of pathway but have little voice in setting the metric or the broader energy policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, mining_workers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, mining_workers, observer).

% Risk analysts, engineers, and policy modelers set and maintain the expected-value metric. They define which harms count (operational deaths vs. lifecycle externalities), at what time horizons (annual vs. discounted present value), and with what probability distributions (empirical frequencies vs. expert judgment on unprecedented events). They defend the metric against critiques by noting its technical rigor and scientific foundation. They have arbitrage because their expertise is portable across framings; if the expected-value metric fell out of favor, they could adapt to alternative risk frameworks. But their professional standing and funding are invested in quantitative analysis, which gives them strong incentive to maintain the expected-value paradigm.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, quantitative_risk_modelers, agenda_setter,
    institutional, generational, arbitrage, global).

% Researchers in catastrophic risk, low-probability high-consequence events, and precautionary approaches argue that acceptable risk should not minimize expected value but rather protect against tail outcomes, even at cost of higher expected harm. They point to climate tipping points, weapons-material proliferation, and unknown unknowns as reasons to weight catastrophic scenarios non-linearly. They are excluded from the metric's construction by institutional standards requiring quantification and probability assignment—their frameworks are characterized as 'unscientific' or 'unmeasurable' rather than as legitimate alternative approaches. Their constrained exit reflects that challenging the metric's authority requires institutional platforms (peer review, policy access) that are gatekept by the modeling community.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, tail_risk_advocates, excluded,
    organized, generational, constrained, global).

% Government officials and regulators adopt the expected-value metric as their decision standard because it appears technical and objective, enabling them to defer controversial value choices (whose lives matter, what risks are acceptable) to mathematical optimization. The metric provides political cover: 'We are following the science' and 'The data shows nuclear is safer.' They benefit from this framing because it shields them from direct accountability for distributional choices (benefiting nuclear investors while harming fossil communities). Their constrained exit reflects that abandoning the metric would require alternative decision frameworks that are less mathematically defensible and more explicitly normative.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, policy_makers, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, policy_makers, agenda_setter).

% Climate researchers use the expected-value framework to argue that fossil fuels must be rapidly phased out because their per-TWh harm (including climate damages) exceeds all alternatives. The metric aligns climate advocacy with quantitative policy analysis, giving their work political salience and funding. They benefit from the metric's legitimacy. They have mobile exit because their expertise in climate science is useful in multiple policy contexts; if the expected-value metric fell out of favor, they could shift to alternative framings (tail risk from climate tipping points, option value of keeping multiple pathways open to adaptation).
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, climate_scientists, beneficiary,
    institutional, generational, mobile, global).

% Specialists in uncertainty quantification, Knightian uncertainty, and model risk observe that the expected-value metric requires probability estimates for unprecedented or rare events where historical data does not exist—nuclear incidents, climate tipping points—and that assigning probabilities to deep uncertainty is epistemically problematic. Their research suggests the metric's foundation is more assumption-dependent than its defenders acknowledge. They are observers because their critiques do not change how the metric is used; institutional actors continue applying expected-value analysis despite the epistemic concerns, which suggests the metric persists by institutional inertia rather than justified by evidence.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, uncertainty_research_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, nuclear_industry).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single quantitative standard (mortality-per-TWh) for comparing energy pathways so that policy trade-offs can be analyzed using a common metric rather than incommensurable narratives about safety, equity, and uncertainty.
% TRANSFER_FUNCTION: Moves policy priority and capital investment toward pathways (nuclear, renewables) that score well on expected-value mortality reduction, and away from fossil pathways (whose continuous, distributed mortality accumulates heavily in the metric). The losers are fossil-dependent communities whose harm becomes visible in the metric but less politically salient because it is routine rather than catastrophic.
% ABSENT_VOICES: Tail-risk advocates, option-value preservationists, and communities bearing concentrated harm from fossil pathways are excluded from the metric's construction and defense. Tail-risk advocates would contest the probability discounting; option-value advocates would contest the assumed future availability of energy pathways; affected communities would contest whether per-TWh averaging erases their local, intergenerational burden.
% DISAPPEARANCE_RATIONALE: If the expected-value framing disappeared, policy would reorganize around precautionary, tail-risk, and option-value framings. Nuclear projects would face higher approval barriers; fossil pathways would not be permitted to trade their distributed harms for lower expected-value scores; energy policy would diversify rather than converge on a single quantitative metric. The current capital and regulatory structure is built on expected-value rankings; removing the metric dissolves that rationale.
% FOUNDING_PROBLEM: Energy policy required a common language to compare pathways with different risk profiles: fossil fuels produce slow-accumulating, distributed mortality but no catastrophic low-probability events; nuclear produces negligible routine mortality but rare catastrophic risk. Expected-value analysis promises to make this trade-off technical and comparable.
% FOUNDING_PROBLEM_CORROBORATION: Risk analysts and quantitative policy communities attest the founding problem remains live and that expected-value framing solves it. Climate scientists and low-carbon advocates affirm the need for a common metric to compare alternatives. Tail-risk advocates, uncertainty researchers, and affected communities contest that the problem is solved: they argue the metric smuggles in value choices (probability discounting, time-aggregation, boundary conditions) that determine the outcome before analysis begins, and that the metric obscures rather than solves the underlying incommensurability. Legislative testimony and independent risk studies from outside the modeling community support the contested verdict.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the metric's structure: it systematically ranks pathways in ways that benefit nuclear and low-carbon advocates while disadvantaging fossil-dependent actors, and this ranking is maintained by institutional enforcement (suppression = 0.71). The extractiveness is not from an individual extractor but from the metric's systematic directionality—it makes certain outcomes appear inevitable through technical analysis. Theater rises from 0.25 to 0.42 over the interval as the metric becomes institutionalized and more of the enforcement effort defends the metric against critiques rather than calculating genuine harm reduction. Suppression is substantial and rising because tail-risk and precautionary objections must be actively discounted and excluded to maintain the framework's dominance; without this suppression the metric's authority would erode. Accessibility collapse is moderate (0.58) because alternatives do exist (tail-risk framing is live, option-value approaches exist) but require departure from quantitative policy consensus. Resistance is high (0.72) because the metric's value-laden structure generates sustained pushback from affected communities, uncertainty researchers, and precautionary advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the modeling community seat, the constraint is genuine coordination: establishing a common metric solves a real problem of comparing incommensurable pathways. From the affected-communities seat, the constraint is extraction: a metric was chosen that makes their harm visible but politically inert. From the tail-risk seat, the constraint is suppression: their alternative framings are excluded before analysis begins by the requirement to quantify probabilities. The engine computes per-seat classifications; the authored claim does not adjudicate whose perspective is 'correct'—it names the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry and modeling institutions sit near d=0.0 (full beneficiary end): the metric's structure predetermines rankings favoring their positions. Fossil-affected communities sit near d=1.0 (full target end): their harm is made visible by the metric, but its structure simultaneously renders that harm less politically actionable because it is routine rather than catastrophic. Policy makers sit near d=0.2-0.3 (slight beneficiary, with some cost): they gain decision clarity from the metric but bear political cost when affected communities contest it. Tail-risk advocates sit near d=0.8 (strong target): their alternative frameworks are systematically excluded from authority. The engine derives d from these positions; the authored claim (tangled_rope) reflects that the metric coordinates policy analysis AND extracts by systematically favoring some positions over others through its structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (comparison of energy pathways) remains live but contested. The expected-value metric addresses it by making trade-offs calculable. However, the metric smuggles in value choices (probability discounting, harm aggregation, boundary conditions) that determine outcomes before substantive analysis. Mandatrophy does not clearly apply here because the founding problem has not yet atrophied—the metric continues to be used in policy. What is present is increasing theater (defensive enforcement of the metric's legitimacy) and increasing suppression (exclusion of alternative framings). This pattern suggests the constraint is shifting toward snare-like operation: the metric persists not because it solves the founding problem better than alternatives, but because institutional actors benefit from the outcomes it produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_quantification_epistemic_boundary,
    'Are the probability estimates used to discount rare nuclear and climate events epistemically grounded, or do they represent deep uncertainty where historical data does not exist and forecasting is not reliable?',
    'Expert elicitation comparing nuclear historical event rate with climate modeling uncertainty; assessment of whether probability assignments reflect observed frequencies or represent bets on unprecedented scenarios.',
    'If probabilities are epistemically grounded, the expected-value discounting is defensible. If they represent deep uncertainty, the metric''s foundation is assumption-dependent and the constraint becomes more snare-like (persists by enforcing quantification rather than justified by it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probability_quantification_epistemic_boundary, empirical, 'Whether rare-event probability discounting rests on empirical frequency or epistemic assumption.').

omega_variable(
    harm_aggregation_temporal_boundary,
    'Should distributed, continuous annual mortality from fossil fuels be weighted equally with rare catastrophic mortality? Is there a principled way to aggregate harms across time horizons and populations?',
    'Normative analysis from ethics and risk philosophy; comparison of metric outcomes under alternative aggregation rules (eg. equity-weighted, maximum-harm-to-most-vulnerable, precautionary floors).',
    'If aggregation is arbitrary, the metric''s outputs are determined by value choice, not technical analysis. Different aggregation rules would rerank pathways, changing beneficiary/victim sets and shifting d values for institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_aggregation_temporal_boundary, conceptual, 'Whether harm aggregation rules are technical or normative.').

omega_variable(
    boundary_condition_hidden_value_choices,
    'Which harms count in the metric? Are occupational mining deaths counted? Lifecycle emissions? Supply-chain externalities? Climate damages discounted to present value? Coal-plant decommissioning? These boundary choices determine ranking outcomes before calculation.',
    'Sensitivity analysis showing how alternative boundary definitions rerank pathways; stakeholder testimony on which boundaries are treated as fixed vs. negotiable.',
    'Narrow boundaries (only direct operational mortality) favor nuclear. Broad boundaries (lifecycle, externalities, climate) may shift rankings. The constraint''s extractiveness depends on boundary choices; changing boundaries would reclassify pathways and shift which actors benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_condition_hidden_value_choices, preference, 'Whether metric boundaries are technical or normative.').

omega_variable(
    tail_risk_suppression_mechanism,
    'Is the exclusion of tail-risk advocates and catastrophic-outcome framings from the metric''s construction structural (the expected-value framework makes tail risks inexpressible) or institutional (deliberate choice to not engage alternative framings)?',
    'Genealogy of metric adoption; analysis of whether tail-risk framings were considered and rejected or simply never entered the conversation.',
    'If structural exclusion, the suppression is a feature of the framework itself. If institutional, the suppression is a choice enforced by those maintaining the metric. Both lead to high suppression; the distinction affects whether suppression is perceived as inevitable or changeable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_suppression_mechanism, empirical, 'Is tail-risk suppression structural to expected-value framing or institutional enforcement choice?').

omega_variable(
    reading_kernel_committer_structure,
    'Is the expected-value-dominant reading''s persistence the result of its superior epistemic standing, or the result of institutional actors benefiting from its outcomes and maintaining it through authority enforcement?',
    'Comparison with sibling readings'' empirical performance and legitimacy; analysis of actor incentives and institutional lock-in; historical contingency: what if a different reading had achieved institutional adoption first?',
    'If epistemic superiority, the reading is a mountain-like discovery. If institutional, it is a constructed arrangement sustained by power, making the constraint snare-like. This determines whether reclassification is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_committer_structure, conceptual, 'Whether the expected-value reading is epistemically dominant or institutionally imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__expected_value_dominant, theater_ratio, 5, 0.29).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.34).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__expected_value_dominant, theater_ratio, 15, 0.38).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.4).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__expected_value_dominant, theater_ratio, 25, 0.41).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.42).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, information_standard).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.06).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% The 'acceptable risk in energy policy' kernel decomposes into three structurally distinct constraints with non-overlapping ε values and victim/beneficiary sets. EXPECTED-VALUE-DOMINANT (this file) dominates policy and capital allocation, systematically benefiting nuclear and disfavoring precaution. CATASTROPHIC-TAIL-DOMINANT (sibling) would elevate rare-event protection and constrain nuclear expansion; it coexists as a live alternative held by tail-risk researchers and environmental advocates. OPTION-VALUE-PRESERVING (sibling) maintains multiple pathways and resists rapid closure; it influences both other readings by creating political pressure for diversification. These three are not views of one constraint—they are three constraints with different ε, beneficiary/victim structure, and classification. They are linked because adoption of the expected-value reading systematically suppresses the alternatives at every institutional level where energy policy is set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
