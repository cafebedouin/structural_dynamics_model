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
 *   human_readable: Expected-Value-Dominant Energy Risk Framework
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   The expected-value-dominant reading of acceptable risk constrains energy
 *   policy by asserting that rational policy must minimize aggregate expected
 *   mortality across all energy pathways, measured in deaths per TWh. This
 *   reading treats fossil fuel deaths (air pollution, mining accidents) as
 *   distributed probabilistic harms to be aggregated with, and potentially
 *   offset by, the averted deaths from deploying fossil energy instead of
 *   alternative sources. Nuclear accident risk is discounted by probability —
 *   Chernobyl-scale events are treated as statistically rare enough to be
 *   outweighed by routine fossil mortality when expected values are computed.
 *   This framework benefits fossil fuel operators and cost-minimizing
 *   utilities while imposing extraction on populations bearing distributed
 *   health costs and those bearing tail risks. The reading is CLAIMED as
 *   tangled_rope (coordination + enforcement + asymmetric extraction) while
 *   competing sibling readings (catastrophic_tail_dominant,
 *   option_value_preserving) offer fundamentally different axioms about what
 *   constitutes 'acceptable' risk.
 *
 * KEY AGENTS:
 *   - fossil_fuel_operators: institutional power, arbitrage exit — benefit from framework that rationalizes routine mortality as acceptable
 *   - energy_cost_minimizers: organized power, constrained exit — benefit from cost-minimization logic
 *   - air_pollution_affected_populations: powerless, trapped — bear chronic mortality, counted but discounted
 *   - mining_workers: organized power, constrained exit — bear occupational mortality treated as sunk production cost
 *   - nuclear_accident_risk_bearers: powerless, trapped — bear low-probability catastrophe risk, mathematically discounted
 *   - energy_modelers: institutional power, analytical exit — set and defend the framework itself
 *   - catastrophic_tail_advocates: moderate power, analytical exit — excluded from beneficiary set, their axiom contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.72).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Energy Risk Framework").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '87cdebdd-419c-4dcc-a01e-cefd57fdedf6').
narrative_ontology:cs_kernel_codification('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', formalized).
narrative_ontology:cs_authority_grounding('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', expertise).
narrative_ontology:cs_interpretation_layer_present('87cdebdd-419c-4dcc-a01e-cefd57fdedf6').
narrative_ontology:cs_reading_relation('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', foundational, expected_value_commensurable_across_harms).
narrative_ontology:cs_axiom_status(expected_value_commensurable_across_harms, holdable).
narrative_ontology:cs_axiom_grounding('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', expected_value_commensurable_across_harms, instrumental).
narrative_ontology:cs_axiom('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', foundational, probabilistic_discounting_rational_risk_weighing).
narrative_ontology:cs_axiom_status(probabilistic_discounting_rational_risk_weighing, holdable).
narrative_ontology:cs_axiom_grounding('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', probabilistic_discounting_rational_risk_weighing, deontological).
narrative_ontology:cs_reference_frame('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', mathematical_rationality_expected_utility_maximization).
narrative_ontology:cs_drift_state('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', climate_accumulation_catastrophe_recognition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87cdebdd-419c-4dcc-a01e-cefd57fdedf6', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, energy_cost_minimizers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, air_pollution_affected_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, mining_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_accident_risk_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, renewable_energy_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a risk framework that discounts their routine mortality (air pollution, mining accidents) by assigning them to 'acceptable' baseline risk categories. Their operations produce high statistical deaths per unit energy, but the framework's mortality-per-TWh metric obscures the distributed nature of fossil causation and treats it as diffuse background risk. Can lobby for continued preferential treatment under this framework or threaten energy reliability arguments.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from grid operators and utilities using this framework to justify continued fossil deployment where it minimizes grid cost. The framework licenses them to defer renewable investment and externalize health costs. They include utilities with stranded fossil assets, industrial energy consumers with fixed infrastructure, and grid operators optimizing for short-term reliability over long-term health outcomes.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_cost_minimizers, beneficiary,
    organized, biographical, constrained, global).

% Bear the chronic mortality and morbidity from fossil fuel combustion emissions (particulate matter, NOx, SO2). The expected-value framework counts them but discounts them probabilistically — their deaths are 'statistically expected' and therefore rationalized as acceptable. They cannot exit the air they breathe, have no seat at the decision table, and receive no compensation for the health extraction.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, air_pollution_affected_populations, payer,
    powerless, biographical, trapped, global).

% Bear occupational mortality and injury from coal, uranium, and rare-earth extraction. The framework counts mining deaths per TWh but treats them as an already-internalized cost of fuel production, not as a separable extraction to be weighed against alternatives. Their choice is occupational hazard acceptance or economic exit from mining regions.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, mining_workers, payer,
    organized, biographical, constrained, regional).

% Live near nuclear facilities and bear the low-probability, high-consequence accident risk. The expected-value framework mathematically discounts this risk (Chernobyl, Fukushima mortality divided by frequency) such that it appears less harmful than the high-probability, distributed mortality of fossil fuels, even though the local catastrophic tail risk is viscerally present. They cannot choose their proximity to the risk source and receive ex-ante compensation only through abstract statistical arguments about total harm minimization.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_accident_risk_bearers, payer,
    powerless, generational, trapped, regional).

% Are structurally disadvantaged by a framework that counts only direct operational mortality. Renewables' health benefits (averted air pollution death) are computed as negatives of a counterfactual, not as positive coordination gains. Their exclusion from the beneficiary set means they do not steer the framework's parameters; they can advocate for revision but cannot reshape the constraint from within.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, renewable_energy_developers, excluded).

% Maintain and operate the expected-value-dominant framework itself. They author the mortality-per-TWh metrics, calibrate the risk discounting, conduct the policy analysis that recommends continued fossil deployment, and defend the framework against critiques from catastrophic-risk perspectives and option-value approaches. Their institutional authority over 'correct' risk accounting gives them agenda-setting power.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_modelers_and_economists, agenda_setter,
    institutional, generational, analytical, global).

% Represent an alternative reading of acceptable risk: prioritizing avoidance of low-probability catastrophes (nuclear accidents, climate tipping points) over minimizing expected aggregate harm. They are excluded from the beneficiary set and actively opposed by the expected-value framework's institutional advocates; their position is framed as 'risk-averse' and 'inefficient' rather than as a legitimate alternative ethical stance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, policy_advocates_catastrophic_tail, excluded,
    moderate, generational, analytical, global).

% Observes from outside the risk framework itself: the framework's mortality-per-TWh metric does not account for climate system tipping points, planetary boundary violations, or non-linear risks that exceed the scope of current harm aggregation. They provide testimony on whether the framework's boundaries adequately capture the true risk landscape.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, climate_scientific_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, fossil_fuel_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common metric (mortality-per-TWh) for comparing disparate energy pathways so rational energy policy can select whichever minimizes total human harm. Solves the otherwise intractable problem of weighing incompatible risk profiles (routine distributed deaths vs. rare catastrophes) into a single policy-relevant number.
% TRANSFER_FUNCTION: Transfers the right to externalize routine distributed health costs (fossil air pollution, mining deaths) to operators and consumers of fossil energy, while pricing catastrophic-outcome risk into nuclear and renewable pathways through explicit probability discounting. The metric makes routine harms invisible as 'acceptable baseline' while rendering nuclear accidents hypervisible as 'unacceptable tail risks'.
% ABSENT_VOICES: Catastrophic-risk advocates argue the framework is fundamentally incomparable with risk preferences that prioritize avoiding tails. Option-value advocates argue the framework forecloses future technologies by not preserving decision flexibility. Neither group has a seat at the framework-setting table; their positions are treated as 'inefficient' rather than as legitimate alternative axioms.
% DISAPPEARANCE_RATIONALE: If the expected-value framework disappeared, policy would revert to risk politics: local and catastrophic risks would carry more weight, renewable deployment would accelerate (catastrophe avoidance), fossil investment would face explicit health-cost accounting, and mining regions would see stronger occupational-hazard regulation. The grid's operational optimization parameters would shift away from cost-minimization toward resilience and harm-minimization on non-aggregated grounds.
% FOUNDING_PROBLEM: Early energy policy lacked any systematic basis for comparing different risk profiles (coal pollution vs. nuclear accident potential vs. renewable variability). The founding problem was: how do we rationally choose between technologies whose harms are qualitatively different and occur at different temporal and spatial scales?
% FOUNDING_PROBLEM_CORROBORATION: Energy modelers and economists attest the founding problem is live and that expected-value metrics are the rational solution. Climate scientists, catastrophic-risk advocates, and affected populations contest whether the founding problem is correctly stated — they argue the problem is not 'how to compare,' but rather 'how to preserve safety margins and collective decision-making authority.' Regulatory economists outside the benefiting institution note that the framework was adopted without public deliberation over its axioms.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises over the interval (0.45 to 0.68) as the framework becomes institutionalized and deployed at scale to justify fossil investment despite mounting evidence of its distributional harms. Theater rises (0.22 to 0.38) as the framework's performative function — appearing to be rational and objective while masking the axiomatic choice — becomes more prominent. The apparent 'settling' of extractiveness at 0.68 after time-point 32 reflects institutional lock-in: the framework is defended as settled science, alternative readings are marginalized, and suppression effort focuses on preventing framework revision rather than on winning new rounds of debate. Suppression requirement rises monotonically (0.58 to 0.72) because catastrophic-risk and option-value advocates mount increasing resistance as climate risk accumulates and nuclear deployment becomes geopolitically relevant. The framework must actively suppress competing readings to maintain its agenda-setting authority. One shared time grid ensures every metric is authored at every examined point; no back-filling or metric-specific intervals.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute dramatically differently. From the modeler's institutional seat, the framework is legitimate policy technology — genuine coordination around a hard problem, neutral in its mathematics. From the air-pollution-population seat, the same structure is enforced extraction: their deaths are real numbers but are made 'acceptable' through a mathematical sleight of hand (probability discounting, baseline normalization). From the catastrophic-risk-advocate seat, the framework forecloses a legitimate ethical stance (precaution) by treating it as inefficient rather than as a competing axiom. The computed per-seat types should diverge sharply: beneficiary seats compute rope or coordination dominance; payer seats compute snare (pure extraction with coordination framing); advocacy seats compute tangled_rope or snare depending on their partial institutional status.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel operators and energy cost-minimizers derive d ≈ 0.1–0.25 (near beneficiary end): they profit from the framework's persistence, face no enforcement costs, and have high exit option if an alternative framework emerges (they can re-compete under a catastrophic-risk or option-value framework). Air pollution populations derive d ≈ 0.85–0.95 (near target end): they bear the extraction (uncompensated health costs), cannot exit (trapped by geography/biology), and have no seat at framework-setting. Mining workers derive d ≈ 0.72 (target end but somewhat constrained by occupational choice). Nuclear risk bearers derive d ≈ 0.88 (trapped, bearing explicitly modeled risk that is then discounted by the framework). Modelers derive d ≈ 0.15 (they benefit from institutional authority and grants to maintain the framework but face moderate exit if the framework's legitimacy collapses). Catastrophic-risk advocates derive d ≈ 0.65 (they are partially excluded, their positions are suppressed, but they retain some institutional presence as 'precautionary principle' voices).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to compare qualitatively different energy risks) was live when the framework was adopted in the 1970s–1990s. By 2020+, the founding problem status is contested: the framework's proponents argue it remains necessary because energy choices are still hard; critics argue the founding problem is now dead and the framework persists as pure institutional inertia and rent-seeking (piton). The six_questions evidence includes explicit testimony from outside the benefiting parties (climate scientists, catastrophic-risk advocates, affected populations) that the founding problem has been *reframed*, not solved — the actual problem is now how to preserve collective decision-making authority and safety margins in the face of deep uncertainty, not how to aggregate commensurable risks. This reframing prevents mandatrophy resolution via the benignity gate: the framework cannot be defended as having solved its founding problem because that problem is no longer what the framework claims to address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commensurability_of_risk_profiles,
    'Are distributed probabilistic harms and low-probability catastrophic harms genuinely commensurable on a single mortality-per-unit metric, or does averaging them together commit a category error?',
    'Philosophical and pragmatic resolution: does the framework''s use of expected value produce policy that retrospectively avoids the worst outcomes across a range of scenarios, or does it systematically underprice tail risks? Historical comparison with jurisdictions using different risk frameworks.',
    'If commensurable, the framework is legitimate cost-benefit analysis and the extraction reflects real tradeoffs. If not commensurable, the framework is a false precision that rationalizes tail-risk acceptance, and the reading should be reclassified as pure snare (cover story for extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commensurability_of_risk_profiles, conceptual, 'Whether expected-value aggregation is philosophically justified for incommensurable risk categories.').

omega_variable(
    beneficiary_circularity_in_framework_design,
    'To what extent was the expected-value framework deliberately designed by fossil fuel advocates to rationalize continued fossil deployment, versus arising from genuine technical need for risk comparison?',
    'Documentary and archival evidence of framework development, including the institutions funding early risk research and the distribution of professional incentives. Testimony from original researchers about design constraints and who set the boundaries.',
    'If deliberately designed for beneficiary advantage, the extraction is intentional and the classification may shift from tangled_rope (mixed coordination + extraction) to snare (extraction with coordination cover). If technically driven, the extraction reflects a real but unintended asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_circularity_in_framework_design, empirical, 'Whether framework design was driven by technical necessity or by beneficiary capture.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of catastrophic-risk and option-value readings primarily structural (institutional control of research funding, journal access, policy seats) or internalized (researchers convinced the expected-value reading is objectively correct)?',
    'Survey of researchers in the field about their perception of competing frameworks, analysis of publication patterns and funding source distributions, historical tracking of career consequences for advocates of non-expected-value approaches.',
    'If primarily structural, the suppression metric is accurately capturing external coercion. If internalized, the measured suppression understates the actual constraint''s force — affected populations may internalize the framework''s legitimacy even after barrier removal. If mixed, the piton risk is that the framework persists by theatrical maintenance even after funding dries up.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression operates through external barriers or internalized belief in the framework''s correctness.').

omega_variable(
    kernel_vs_reading_distinction_in_policy,
    'Is ''acceptable risk in energy'' a genuine kernel (stabilized, transmissible commitment with multiple defensible readings) or a false kernel — a label applied retroactively to what is actually a settled beneficiary-capture outcome?',
    'Examination of whether catastrophic-tail and option-value advocates can articulate their readings from within a common authority structure (e.g., can they cite physics, decision theory, or institutional rules that legitimate their position), or whether their opposition is purely external.',
    'If true kernel, this story (expected_value_dominant) is one legitimate reading among three. If false kernel, the ''multiple readings'' framing is a cover story for beneficiary enforcement, and the constraint is snare throughout, not tangled_rope. The cs_structure block presumes true-kernel status; if the omega resolves false-kernel, the cs_structure is retroactively invalidated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction_in_policy, conceptual, 'Whether the acceptable-risk energy commitment is a true kernel admitting multiple readings or a false kernel masking enforced beneficiary capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_ev_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.22).
narrative_ontology:measurement(acceptable_risk_ev_tr_t8, acceptable_risk_energy__expected_value_dominant, theater_ratio, 8, 0.26).
narrative_ontology:measurement(acceptable_risk_ev_tr_t16, acceptable_risk_energy__expected_value_dominant, theater_ratio, 16, 0.32).
narrative_ontology:measurement(acceptable_risk_ev_tr_t24, acceptable_risk_energy__expected_value_dominant, theater_ratio, 24, 0.36).
narrative_ontology:measurement(acceptable_risk_ev_tr_t32, acceptable_risk_energy__expected_value_dominant, theater_ratio, 32, 0.37).
narrative_ontology:measurement(acceptable_risk_ev_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_ev_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(acceptable_risk_ev_be_t8, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(acceptable_risk_ev_be_t16, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(acceptable_risk_ev_be_t24, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(acceptable_risk_ev_be_t32, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(acceptable_risk_ev_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_ev_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(acceptable_risk_ev_su_t8, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(acceptable_risk_ev_su_t16, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(acceptable_risk_ev_su_t24, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(acceptable_risk_ev_su_t32, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(acceptable_risk_ev_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel decomposes into three constraint stories corresponding to three competing readings of what 'acceptable risk' means in energy policy. Each reading has a different beneficiary/victim structure, different epsilon, and different operative suppression mechanism. All three readings share the kernel (the stabilized but contested commitment to 'acceptable risk'), but disagree on its axioms. The expected_value_dominant reading (this constraint) treats distributed harms and rare catastrophes as commensurable on a mortality-per-TWh metric. It structurally differs from catastrophic_tail_dominant (which inverts the probability weighting and makes tail avoidance the primary goal) and from option_value_preserving (which refuses commensuration and prioritizes preserving future decision-making authority). These are not the same constraint viewed from different angles — they have different epsilon values because the *referent* of 'acceptable risk' is interpreted differently by each reading. They are linked through network.affects_constraints because changes to institutional authority over one reading propagate to the others' viability conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, powerless, 0.89).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
