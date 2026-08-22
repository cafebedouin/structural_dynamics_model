% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value Risk Acceptability Framework for Energy Systems
 *   domain: energy_policy/risk_assessment/public_safety
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel
 *   'acceptable_risk_for_energy': the expected-value-dominant reading. Under
 *   this reading, annual expected costs and climate benefits determine
 *   whether a nuclear system is acceptable; rare events are weighted by
 *   probability × consequence in a unified optimization calculus. This
 *   reading vindicates probabilistic risk analysis as a legitimate policy
 *   framework and treats nuclear waste disposal as a solvable engineering
 *   problem once the expected-value math is favorable. Nuclear operators,
 *   regulators, and cost-benefit economists inhabit this reading; they
 *   structure licensing, financing, and waste policy around it. The sibling
 *   readings—catastrophic_tail_dominant and comparative_risk_dominant—contest
 *   this framework's legitimacy and its treatment of tail risk and
 *   intergenerational burden. This story generates the constraint AS THIS
 *   READING SEES IT: extraction arises because the framework's procedure for
 *   making tail risks invisible transfers concentrated hazard to powerless
 *   host communities while distributing diffuse climate benefits across
 *   energy consumers. The framework itself is the mechanism of suppression:
 *   exclusion from the decision algorithm is a suppression of voice, not
 *   coercion, so suppression is moderate. Theater is present but not
 *   dominant: the framework makes real contribution to energy optimization;
 *   it is not pure performance. The measurement series shows extractiveness
 *   rising early (as climate and waste costs are better quantified) then
 *   plateauing once the framework's parameters stabilize.
 *
 * KEY AGENTS:
 *   - nuclear_operators: set framework parameters, benefit from acceptability verdicts
 *   - regulatory_authorities: enforce the framework, delegate cost-benefit authority
 *   - waste_repository_host_communities: bear concentrated non-probabilistic hazard, excluded from decision procedure
 *   - energy_consumers: benefit from low-cost zero-carbon electricity, bear diffuse risk priced as expected cost
 *   - tail_risk_advocates: hold competing reading, structurally excluded from framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.42).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Risk Acceptability Framework for Energy Systems").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "energy_policy/risk_assessment/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, '93e42cd0-8491-46a3-b0c1-5892c01e67cd').
narrative_ontology:cs_kernel_codification('93e42cd0-8491-46a3-b0c1-5892c01e67cd', formalized).
narrative_ontology:cs_authority_grounding('93e42cd0-8491-46a3-b0c1-5892c01e67cd', expertise).
narrative_ontology:cs_interpretation_layer_present('93e42cd0-8491-46a3-b0c1-5892c01e67cd').
narrative_ontology:cs_reading_relation('93e42cd0-8491-46a3-b0c1-5892c01e67cd', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('93e42cd0-8491-46a3-b0c1-5892c01e67cd', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('93e42cd0-8491-46a3-b0c1-5892c01e67cd', foundational, expected_value_decision_criterion_legitimate).
narrative_ontology:cs_axiom_status(expected_value_decision_criterion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('93e42cd0-8491-46a3-b0c1-5892c01e67cd', expected_value_decision_criterion_legitimate, instrumental).
narrative_ontology:cs_axiom('93e42cd0-8491-46a3-b0c1-5892c01e67cd', foundational, tail_events_are_appropriately_probability_weighted).
narrative_ontology:cs_axiom_status(tail_events_are_appropriately_probability_weighted, holdable).
narrative_ontology:cs_axiom_grounding('93e42cd0-8491-46a3-b0c1-5892c01e67cd', tail_events_are_appropriately_probability_weighted, empirically_contingent).
narrative_ontology:cs_reference_frame('93e42cd0-8491-46a3-b0c1-5892c01e67cd', probabilistic_risk_analysis_framework).
narrative_ontology:cs_drift_state('93e42cd0-8491-46a3-b0c1-5892c01e67cd', contemporary_climate_crisis_epoch, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('93e42cd0-8491-46a3-b0c1-5892c01e67cd', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_baseline_electricity).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, waste_repository_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, climate_externality_bearers_global).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_externality_bearers_global).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_baseline_electricity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate nuclear reactors under the expected-value framework. They calculate annual expected costs (operations, insurance, waste management, decommissioning) against climate benefits (avoided coal emissions, stable baseload power). When the framework shows positive net present value, they license and expand; when negative, they retire reactors. They directly influence the parameters that operationalize 'acceptability': discount rates, consequence valuations for rare events, and the boundary between 'engineered solution' and 'unacceptable hazard.'
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive low-cost zero-carbon electricity when nuclear is licensed and operational under expected-value acceptability. They benefit from the climate benefit term in the calculation (avoided coal emissions). They also bear diffuse tail risk: statistical probability of harm is small (priced into the expected-value calculation) but non-zero. If a rare event occurs (reactor accident, waste release), they may experience acute health or property impacts. The framework prices their exposure as expected cost; they experience it as non-probabilistic presence.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_baseline_electricity, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_baseline_electricity, payer).

% Host centralized or distributed waste repositories (spent fuel, vitrified waste, other long-lived isotopes) on the premise that the expected-value framework has deemed them acceptable and engineered solutions are adequate. They are geographically immobilized: their livelihood, property, and generations depend on the site. They carry the repository hazard non-probabilistically: whether or not failure occurs, they live with active confinement systems. The framework weights the risk of their community's harm as probability × consequence, a small annual expected cost. They live with the consequence.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, waste_repository_host_communities, payer,
    powerless, civilizational, trapped, local).

% Benefit from avoided coal-plant emissions (the climate-benefit term) when nuclear is licensed under expected-value favorability. They also bear climate risks themselves from residual energy-sector emissions. The framework treats the climate benefit as a monetized value (dollars per ton of CO2 avoided) that offsets nuclear waste risk in the calculation. They are diffusely exposed to climate hazard and cannot exit; they are also statistically unlikely to experience direct nuclear hazard but cannot opt out of the electricity system.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_externality_bearers_global, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, climate_externality_bearers_global, payer).

% Administer and enforce the expected-value framework. They set the discount rates, consequence valuations, and probability thresholds that determine whether a reactor is licensed or a waste repository approved. They are answerable to legislatures and public comment, but the expected-value method constrains what inputs are valid: tail-risk concerns are acknowledged but weighted as expected cost. They do not operate reactors or host waste; their role is to enforce the framework's legitimacy and constrain alternatives.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, regulatory_authorities, observer).

% Hold the position that low-probability high-consequence events should dominate risk calculus; that irreversibility, intergenerational burden, and non-probabilistic presence of hazard outweigh expected-value optimization. They argue for banning nuclear energy or establishing absolute safety thresholds (e.g., 'no waste repositories,' 'reactor failure probability <10^-6 per year'). They participate in public comment and litigation but are structurally excluded from the expected-value framework: their perspective is not a valid input to the calculation. They can object, but the framework does not compute their objection as a decision constraint.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_advocates, excluded,
    moderate, civilizational, constrained, global).

% Provide empirical estimates of climate damages and coal-emissions costs that feed the framework's benefit term. They estimate cost per ton of CO2, long-term temperature trajectory, and avoided-coal-emissions crediting. They do not set the framework or weight the risks; their role is to supply the climate-science input. The framework's treatment of these inputs as a monetized benefit variable determines how much their evidence amplifies nuclear's acceptability.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_scientists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a quantitative decision procedure for comparing energy systems under risk and uncertainty: converts multidimensional hazards (reactor accident probability, waste longevity, coal-emissions consequences, renewable intermittency) into a single commensurable metric (annual expected cost in dollars or net present value) so energy policy can optimize across competing sources and scales.
% TRANSFER_FUNCTION: Moves the rare-event tail risk of nuclear failures and waste-repository breaches from the statistical accounting of the expected-value calculation into the lived, non-probabilistic experience of powerless host communities. Simultaneously moves the climate benefit (avoided coal emissions, statistical reduction in future climate damage) into the current-generation national energy policy calculation. Both movements are rendered as expected-value terms, making both invisible in their actual asymmetry: beneficiaries collect current electricity and carbon avoidance; bearers live with the presence of hazard for generations.
% ABSENT_VOICES: Host communities and environmental-justice advocates who would object to centralized waste disposal on the grounds that non-probabilistic coexistence with rare-event hazard is intolerable regardless of expected value. Tail-risk theorists who argue that irreversibility and intergenerational burden cannot be optimized away in a monetary calculation. These voices are present in public comment and testimony but are structurally excluded from the expected-value framework: their concerns are acknowledged and probability-weighted, but the framework does not grant them decision-relevance beyond their statistical contribution. This is suppression without coercion—it is algorithmic exclusion.
% DISAPPEARANCE_RATIONALE: If the expected-value framework and its enforcement machinery disappeared, energy licensing would revert to negotiated hazard thresholds, absolute prohibitions on certain failure modes (e.g., repositories must not exist, or must meet impossibly stringent conditions), or comparative-risk standards that do not mechanically downgrade tail events. Nuclear capacity would likely contract substantially. Waste disposition policy would shift from 'engineered solution optimized for cost-effectiveness' to 'categorical judgment'—'we will not create this hazard' or 'we will manage it only under conditions we deem intolerable.' The energy system would reorganize, most likely toward renewable investment, demand reduction, and climate-mitigation alternatives that do not require intergenerational waste management.
% FOUNDING_PROBLEM: Mid-20th-century energy expansion required decision procedures for comparing nuclear's novel hazards against established energy sources (coal pollution, renewable intermittency) and against each other. Probabilistic risk analysis and expected-value optimization emerged as methods to quantify and compare incommensurable risks on a single scale, making energy policy mathematically tractable.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear operators and regulatory economists attest the founding problem is live: energy systems require quantitative comparison and optimization under uncertainty; the expected-value method is the standard analytical tool. Climate scientists attest the founding problem is real but is evolving: the original concern was comparing baseline hazards (coal vs. nuclear). The contemporary problem is comparing catastrophic planetary-scale hazards (climate tipping points) against baseline hazards; this requires intergenerational and absolute-risk considerations that expected-value optimization may not adequately capture. Environmental justice advocates and host communities attest that the founding problem was never about optimizing hazard distribution in the abstract; it was a political device to find a decision procedure that would *permit* nuclear expansion despite opposition. The framework was designed for that task, not for genuine risk comparison; the founding problem was manufactured.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the framework systematically downgrades the salience of low-probability high-consequence events—the mathematical procedure itself is the extraction mechanism. A waste repository failure with 0.1% annual probability and catastrophic consequence gets weighted as 0.001 × high consequence, yielding a small expected annual cost that can be optimized away against competing benefits. The host community experiences the non-probabilistic reality: they live with the hazard whether or not it occurs. Suppression is moderate (0.42) because the framework's exclusion of tail-risk perspectives is not enforced by law or violence—it is enforced by the decision algorithm itself: you cannot appeal to irreversibility or intergenerational burden within the expected-value calculation. You can object, but your objection is not a valid input to the framework. Resistance is high (0.72) because environmental justice movements, some regulatory bodies, and host communities actively resist the framework's application to their communities. Theater is moderate (0.28): the framework makes genuine contributions to energy optimization and risk quantification; it is not pure performance. But a growing portion of its maintenance consists of rationalizing why tail events need not be central to policy, which is performative—it serves the operators' interest in continued licensing more than it serves decision-making accuracy. The measurement series shows extractiveness rising in the first 25 periods as climate damages are better monetized (increasing the framework's apparent justification) and then stabilizing as the framework's parameters settle.
 *
 * PERSPECTIVAL GAP:
 *   The operator and regulator seats should compute as beneficiaries of a rope or weak tangled_rope (genuine coordination, manageable extraction). The host-community and climate-bearer seats should compute as victims of a snare or strong tangled_rope (the framework is enforced against their interests, suppression of alternative framings is active). The energy-consumer seat sits in between: they benefit from the electricity and from the framework's cost-efficiency, but are exposed to tail risk that the framework renders invisible. The engine's per-seat computation should reveal this divergence from the structural data: beneficiaries at institutional power with arbitrage exit vs. victims at powerless power with trapped exit, mediated by a framework that is called coordination by some and called capture by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and regulators are beneficiaries (d near 0.0): they collect licensing authority, operational margins, and the ability to externalize rare-event risk. Waste-host communities and climate-externality bearers are victims (d near 1.0): they bear concentrated or diffuse hazard and are excluded from the decision procedure. Energy consumers sit symmetric-to-moderate-beneficiary (d near 0.25–0.35): they benefit from cheap electricity and climate benefits, but carry diffuse risk exposure that the framework prices down. The framework itself operates as the agent that structures directionality: by defining 'acceptable' as 'expected value favorable,' it systematically migrates tail risk from the calculation into the lived experience of the powerless. The directionality override would not be needed here; the structural data (beneficiary/victim declarations, power atoms, exit options) derive the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: mid-20th-century energy expansion required a decision procedure for comparing incommensurable risks. Probabilistic risk analysis and expected-value optimization were real solutions to that coordination problem. The mandatrophy question: does the founding problem still exist, or has it been superseded? Climate science has shifted the problem: the founding concern was comparing *baseline hazards* (coal pollution vs. reactor failure). The contemporary problem is comparing *climate-catastrophe hazard* (planetary-scale, high-consequence, low-probability in real-time timescale, intergenerational) against baseline hazards. The expected-value framework's treatment of climate as a monetized benefit variable and nuclear waste as a probabilistic cost makes *sense* in the old problem (comparing energy sources for steady-state operation). It makes *less* sense in the new problem (comparing hazards at scales where tail events dominate and where intergenerational equity matters). The constraint has not become pure extraction—the coordination function is real—but the framework has become partially obsolete. A tangled_rope classification (genuine coordination + asymmetric extraction via framework design) captures this better than snare (pure extraction) or rope (pure coordination). The theater ratio's moderate-low and plateauing trajectory suggests the framework is performing maintenance on a solution that no longer fully fits the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_weighting_legitimacy,
    'Is the expected-value weighting of tail events (probability × consequence) the correct way to represent decision-relevant risk, or does it systematically obscure catastrophic scenarios that should be decision-dominant?',
    'Empirical observation of real rare events (reactor failures, repository breaches, climate tipping points) and comparison between expected-value predictions and actual consequences; theoretical analysis of whether mathematical expectation is the right decision criterion for irreversible or intergenerational hazards.',
    'If expected-value weighting is incorrect or incomplete, the entire extractiveness assessment shifts: the constraint becomes snare (pure extraction via illegitimate framework) rather than tangled_rope (coordinated with extractive side effect). The founding problem is not actually solved; it is masked.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_risk_weighting_legitimacy, conceptual, 'Whether expected-value weighting is appropriate for long-tail, irreversible, or intergenerational risks.').

omega_variable(
    intergenerational_commensurability,
    'Is climate externality (avoided coal emissions) commensurable with nuclear waste burden when the benefits accrue in the current generation and the costs extend across centuries?',
    'Ethical and empirical analysis of intergenerational discounting; time-horizon studies on actual long-term repository stewardship costs and climate damage across generations; examination of whether current-generation beneficiaries should be permitted to impose future-generation costs on the same scale.',
    'If the costs and benefits are incommensurable across generations, the framework is illegitimate as a decision procedure for intergenerational hazards. Extractiveness increases (the framework transfers all future-generation risk to the present-generation benefit calculation). The constraint becomes snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_commensurability, preference, 'Whether expected-value optimization is legitimate across generational boundaries.').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is the suppression of tail-risk framing structural (the algorithm mechanically excludes low-probability perspectives) or cognitive (decision-makers believe tail risks are adequately captured in the expected-value metric)?',
    'Interviews and historical analysis of regulatory decision-making; examination of whether tail-risk advocates are excluded by the framework design or by the decision-makers'' judgment that tail-risk concerns are already internalized; comparison between frameworks that explicitly include tail-risk inputs vs. those that do not.',
    'If suppression is purely structural, the constraint''s persistence depends entirely on defending the expected-value method against alternative frameworks. If suppression is partly cognitive (decision-makers genuinely believe tails are handled), the framework''s vulnerability to evidence shifts: empirical demonstration that tails are *not* adequately captured undermines both the structure and the belief. The remediation path changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Whether tail-risk suppression is enforced by the algorithm or by belief.').

omega_variable(
    kernel_reading_substitutability,
    'Can policy adopt a hybrid approach that preserves expected-value optimization for baseline-hazard comparison while introducing tail-risk dominance for intergenerational or irreversibility questions? Or does allowing tail-risk salience automatically foreclose the expected-value reading?',
    'Policy design experimentation: frameworks that weight both expected value and tail-risk criteria; empirical observation of whether such frameworks are stable or collapse toward one reading or the other under political pressure.',
    'If hybrid frameworks are stable, the readings coexist_with rather than foreclose; if they collapse, they foreclose. This affects whether the constraint is a contested kernel with live alternatives or a foreclosure in disguise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_substitutability, conceptual, 'Whether the expected-value and tail-risk readings are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.18).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 5, 0.2).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 10, 0.23).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 15, 0.25).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 20, 0.27).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 25, 0.28).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 30, 0.28).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel acceptable_risk_for_energy. Three structurally distinct constraints emerge from the kernel depending on which reading is adopted: expected_value_dominant (this file), catastrophic_tail_dominant, and comparative_risk_dominant. The readings differ in what counts as a valid decision input, how tail events are weighted, and whether intergenerational or absolute-risk considerations override optimization. The network links record this kinship: all three constrain energy policy, but they do so from incompatible epistemic positions. The expected-value reading influences the other two by setting the baseline framework that tail-risk and comparative-risk readings must contest or reframe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
