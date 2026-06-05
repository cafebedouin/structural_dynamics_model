% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Distinction/Proportionality via Outcomes-Based Metrics (Autonomous Systems)
 *   domain: international_humanitarian_law/military_ethics/autonomous_weapons
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested IHL kernel:
 *   'What constitutes compliance with the distinction and proportionality
 *   principles?' The outcomes-based reading holds that LAWS demonstrably
 *   achieving distinction/proportionality performance equal to or exceeding
 *   human operators satisfy these principles—the law governs outcomes, not
 *   means. This reading is structurally distinct from two siblings: a
 *   human-agency reading (which holds that human judgment is constitutive of
 *   lawful distinction/proportionality, not merely instrumental) and a
 *   categorical-prohibition reading (which holds that autonomous targeting of
 *   protected persons is inherently unlawful regardless of performance
 *   metrics). Each reading produces a different constraint with different
 *   beneficiaries, victims, and classification profiles. This story models
 *   the outcomes-based reading only—a tangled rope that combines genuine
 *   coordination (technical standardization enabling interoperability and
 *   specificity) with asymmetric extraction (military efficiency and
 *   contractors benefit from metric-based legitimization; humanitarian law
 *   custodians lose interpretive authority; civilians' protection depends on
 *   whether metrics validly predict real-world performance).
 *
 * KEY AGENTS:
 *   - Military Operational Efficiency (institutional/arbitrage): Primary beneficiary—outcomes-based framing legitimizes autonomous systems where measurable performance justifies them.
 *   - Defense Contractors (institutional/arbitrage): Primary beneficiary—technical metrics open market for autonomous systems; competitive advantage through benchmark-optimized design.
 *   - Civilian Populations (powerless/trapped): Primary victim—no exit from systems deployed in conflict; protection depends on metric validity and metric-governance integrity.
 *   - Humanitarian Law Custodians: ICRC, international legal experts (moderate/constrained): Constrained by outcomes-based logic; must accept systems that pass metrics even if skeptical; lose authority to interpretive framework shift toward engineering-based compliance.
 *   - International Verification Bodies (organized/constrained): Must verify compliance with metrics but lack field capacity to test combat-realistic conditions.
 *   - Traditional IHL Doctrine (institutional/arbitrage): Persists formally but operative force shifts to metrics; maintains backward compatibility through theater (invocations of humanitarian principles) while substantive legitimacy chain runs through technical benchmarking.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.52).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.48).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Distinction/Proportionality via Outcomes-Based Metrics (Autonomous Systems)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/autonomous_weapons").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '2c8c4072-48bb-4190-ad3c-14825039768b').
narrative_ontology:cs_kernel_codification('2c8c4072-48bb-4190-ad3c-14825039768b', formalized).
narrative_ontology:cs_authority_grounding('2c8c4072-48bb-4190-ad3c-14825039768b', lineage).
narrative_ontology:cs_interpretation_layer_present('2c8c4072-48bb-4190-ad3c-14825039768b').
narrative_ontology:cs_reading_relation('2c8c4072-48bb-4190-ad3c-14825039768b', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c8c4072-48bb-4190-ad3c-14825039768b', ihl_distinction_proportionality__categorical_prohibition_reading, influences).
narrative_ontology:cs_axiom('2c8c4072-48bb-4190-ad3c-14825039768b', foundational, compliance_is_outcome_performance).
narrative_ontology:cs_axiom_status(compliance_is_outcome_performance, holdable).
narrative_ontology:cs_axiom_grounding('2c8c4072-48bb-4190-ad3c-14825039768b', compliance_is_outcome_performance, deontological).
narrative_ontology:cs_axiom('2c8c4072-48bb-4190-ad3c-14825039768b', secondary, metric_validity_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(metric_validity_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('2c8c4072-48bb-4190-ad3c-14825039768b', metric_validity_sufficient_for_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('2c8c4072-48bb-4190-ad3c-14825039768b', metric_equality_legitimacy).
narrative_ontology:cs_drift_state('2c8c4072-48bb-4190-ad3c-14825039768b', contemporary_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2c8c4072-48bb-4190-ad3c-14825039768b', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operational_efficiency).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, interpretive_humanitarian_law_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_if_metrics_fail).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIANS (SNARE) — No exit from autonomous systems deployed in conflict. Trapped by geography and inability to influence weapons architecture. Protection depends entirely on whether technical metrics embedded in systems actually achieve distinction/proportionality in real conditions. No direct representation in metric-setting bodies. Maximum extraction risk: misclassified targets, proportionality failures, metric gaming.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITARIAN LAW CUSTODIANS (TANGLED ROPE) — Constrained by the outcomes-based framing: if metrics demonstrably pass, they must accept autonomous systems, even if they believe human judgment is irreplaceable. They also benefit from clarity and operational specificity that outcome metrics provide. High suppression of alternative framings (human-in-the-loop mandates, categorical prohibitions) through the logic that 'if performance is equal or better, the law is satisfied.' Genuine coordination function (establishing measurable compliance standards) paired with asymmetric extraction (interpretive authority shifts from law custodians to engineers and statisticians who design metrics).
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY OPERATIONAL EFFICIENCY (ROPE) — Net beneficiary. Outcomes-based framing legitimizes autonomous systems where performance metrics justify them. Experiences the constraint as coordination: technical standards enable interoperability and rapid iteration. Can arbitrage: adopt metrics that favor their performance envelope; exit through selective deployment. No significant suppression experienced — the framing enables their preferred outcome.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Net beneficiary. Outcomes-based compliance opens market for autonomous systems where human-in-the-loop mandates would require costly redesign. Technical metrics become competitive advantage. Can arbitrage: design systems to pass published benchmarks; influence benchmark design through R&D partnerships. Genuine coordination function: standardized metrics enable rapid scaling and interoperability.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL VERIFICATION BODIES (TANGLED ROPE) — Constrained by both the outcomes-based framing and by institutional capacity. Must accept systems that pass metrics, but lack field capacity to verify that test conditions match combat conditions. Also benefit from the clarity that measurable standards provide (coordination function). But suppression is high: skepticism about metric robustness is pre-foreclosed by the reading's logic ('law is satisfied if metrics are met'). Cannot mandate additional safeguards once metrics pass without appearing to reject the outcomes-based framework.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL IHL DOCTRINE (PITON) — The classical emphasis on human judgment, command responsibility, and proportionality assessments grounded in context persists formally but loses substantive authority under the outcomes-based reading. The doctrine is not formally rejected; rather, it is reframed as one way of achieving compliance (humans performing at X% accuracy) alongside other means (autonomous systems at ≥X% accuracy). Theater ratio high: traditional doctrine is invoked in preambles and compliance frameworks but the actual legitimacy chain now runs through technical metrics. The constraint maintains backward compatibility through theater (invocations of 'respect for humanitarian principles') while the operative mechanism has shifted to technical benchmarking.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW FRAMING (FALSE SUMMIT CANDIDATE) — From a civilizational perspective, the reading appears to rest on a natural law: 'the law's purpose is to minimize civilian harm and military excess; any means that achieves this purpose equally satisfies the law.' This framing naturalizes a specific reading of IHL's underlying commitment. However, the structural data reveals beneficiaries: military efficiency and defense contractors benefit from this particular interpretation of what 'satisfying the law' means. Engine will compute as false summit, revealing that the naturalization conceals a contested interpretive choice.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ihl_distinction_proportionality__outcomes_based_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, TR),
    TR >= 0.70.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The outcomes-based reading enables significant military efficiency gains and contractor profits through metric-based legitimization, but the extraction is paired with genuine coordination benefits (standardized performance metrics enable interoperability and specificity). The value reflects that the reading solves real coordination problems—forces need clear compliance standards—while simultaneously creating asymmetric benefits for military/industrial actors and risks for civilian protection if metrics fail or become gamed. Theater ratio (0.58): Moderate. Traditional IHL doctrine is invoked and maintained in formal compliance frameworks, but the operative mechanism has shifted to technical benchmarking. The constraint achieves moderate theater—not as performative as traditional review (0.72 in verification_bottleneck) because metrics are quantitative and testable, but not as functionally pure as pure coordination (0.30 in rope) because maintaining the traditional doctrine's authority requires ongoing invocation despite loss of substantive control. Suppression (0.48): Moderate. Barriers to alternative framings are significant but not total. The outcomes-based logic suppresses human-judgment and categorical-prohibition readings by establishing a framework in which 'equal or exceeding human performance' forecloses legal objections. However, alternative readings are not physically or legally prohibited—they remain available but require explicit resistance to the outcomes-based frame. This is softer suppression than snare (0.60+) but harder than rope (0.05).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival collapse and divergence simultaneously. All institutional actors acknowledge the same metric framework, but they experience it as different constraint types: rope for beneficiaries (coordination gains), tangled_rope for constrained custodians and verifiers (mixed extraction/coordination), snare for trapped civilians. The piton classification of traditional doctrine is particularly diagnostic—it shows how a previous legitimacy structure (human judgment as constitutive) persists formally while losing substantive authority to a new mechanism (metric benchmarking). This is institutional degradation, not institutional replacement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies dramatically across perspectives. Military and contractors are beneficiaries with arbitrage exit options, deriving low d (~0.15) and negative/low χ. Humanitarian custodians are trapped in the logic ('if metrics pass, law is satisfied') despite moderate power and some constrained exit, deriving high d (~0.65) and substantial χ. Civilians have zero exit, deriving maximum d (0.95) and maximum f(d). Verification bodies are organized but constrained by institutional mandates, deriving moderate d (~0.55). The analytics observer sees the structure from civilizational scope, deriving d from the revelation that the reading naturalizes a contestable interpretation (mountain appears but FSM detects beneficiaries, triggering reclassification logic).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by explicitly modeling the interpretive choice as a contested reading of a kernel, not as a neutral technical framework. The outcomes-based reading genuinely combines coordination (technical standardization, clarity, interoperability) with extraction (military/contractor benefits, humanitarian authority loss, civilian protection risk). Mandatrophy is not avoided but made transparent: the reading's coherence as tangled_rope depends on whether the beneficiaries' gains (military efficiency, contractor market access) actually align with humanitarian protection outcomes. If metrics prove invalid or become gamed, the extraction component becomes dominant and the constraint reclassifies to snare. If verification bodies gain institutional power to govern metrics, the extraction diminishes and the constraint reclassifies to rope. The classification is conditional on structural outcomes, not merely on metric definitions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_validity_test_condition_gap,
    'Do test conditions for distinction/proportionality metrics validly predict real-world combat performance, or do they select for systems optimized to test conditions rather than actual operational contexts?',
    'Post-deployment empirical analysis: comparison of system performance on original test metrics vs. performance in actual conflict environments; analysis of failure modes that were absent in test conditions; longitudinal tracking of metric-gaming (optimization for test benchmarks at the cost of robustness to out-of-distribution targets).',
    'If metrics are predictive: outcomes-based reading is structurally sound; tangled_rope classification holds. If test-condition optimized: metrics become a suppression mechanism (false assurance of compliance); snare classification appropriate for civilian perspective. Classification changes from tangled_rope to snare at institutional levels if metric validity collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_validity_test_condition_gap, empirical, 'Whether metrics from test conditions predict real-world performance').

omega_variable(
    human_performance_baseline_contestation,
    'What human performance baseline should systems be compared against? Median operator? Best trained operator? Operator under stress? Different baselines yield incomparable metrics.',
    'Analysis of baseline selection in published autonomous weapons standards; comparison of results under different baseline assumptions; empirical study of how baseline choice affects system certification rates.',
    'If baseline is median operator: many systems pass; extraction accelerates. If baseline is best operator under optimal conditions: few systems pass; extraction suppressed. If baseline varies by conflict zone: metric becomes context-dependent, undermining the technology-neutral abstraction. High baseline variation would reclassify from rope to snare from military perspective (arbitrage-seeking behavior would appear).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_performance_baseline_contestation, empirical, 'Human performance baseline for autonomous system comparison').

omega_variable(
    measurement_regime_interpretive_authority_shift,
    'Who holds authority to define and update the metrics themselves? If metrics are controlled by military technical standards bodies or contractor consortia, the outcomes-based reading transfers interpretive authority from IHL custodians to engineers and industrialists.',
    'Institutional analysis: audit of governance structures for metric definition (which organizations set standards, what representation exists, change-control procedures); historical analysis of metric evolution (have metrics tightened, loosened, or remained stable?); power analysis of who can propose changes and who can veto them.',
    'If authority remains with IHL institutions: reading maintains coordination function. If authority shifts to military-contractor bodies: reading becomes pure extraction mechanism for these actors. Institutional victim (humanitarian law custodians) would reclassify from tangled_rope to snare. This is the critical structural vulnerability of the outcomes-based reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_regime_interpretive_authority_shift, conceptual, 'Institutional control over metric definition and governance').

omega_variable(
    civilian_harm_metric_specification,
    'Can ''proportionality'' be reduced to a scalar metric (e.g., ''civilian harm threshold ≤ 2%''), or does the concept inherently require contextual judgment about specific combatant advantage gained?',
    'Conceptual analysis: review of published proportionality standards and their specificity; case study analysis of historical IHL proportionality disputes (did they turn on metric comparisons or on contextual interpretation of military advantage?); test whether metric-based proportionality standards accurately classify historical scenarios that IHL bodies have evaluated.',
    'If reducible to metrics: outcomes-based reading is coherent. If contextual judgment is irreducible: outcomes-based reading supplies false precision; suppresses legitimate legal uncertainty. If partially reducible: measurement regime applies to distinction but not proportionality, weakening the reading''s universality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_harm_metric_specification, conceptual, 'Whether proportionality is reducible to scalar metrics').

omega_variable(
    kernel_reading_contested_commitment,
    'Is this reading a legitimate interpretation of IHL''s actual commitment (minimizing harm through whatever means work), or a reinterpretation that substitutes technical performance for humanitarian judgment?',
    'Textual and historical analysis: do the founding texts of IHL (Additional Protocols, ICRC commentaries) support outcomes-based evaluation? Do they foreclose it? Or is the kernel genuinely ambiguous? Institutional consensus analysis: what do IHL custodian institutions (ICRC, state IHL bodies, academic IHL scholars) say about whether outcomes-based metrics satisfy IHL?',
    'If legitimate: reading coexists with human-agency and categorical-prohibition readings. If reinterpretation: reading forecloses or influences the siblings differently than stated. This omega directly tests whether cs_structure.reading_relations are correctly specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested_commitment, conceptual, 'Whether reading is legitimate interpretation of IHL kernel or reinterpretation').

omega_variable(
    false_summit_naturalization,
    'Does declaring that ''law is satisfied by technical metrics'' naturalize a specific reading of humanitarian obligation, foreclosing human-judgment and categorical-prohibition readings that might be equally defensible under the kernel?',
    'Comparative analysis: articulate each reading''s core axiom and show whether the outcomes-based axiom logically forecloses the others, or whether all remain simultaneously holdable within different interpretive frameworks. If simultaneously holdable, the mountain classification is a false summit.',
    'If false summit: the constraint should be reclassified from mountain/rope to tangled_rope/snare at institutional level. Beneficiaries (military efficiency, contractors) would be revealed as actively maintaining the ''natural law'' framing to suppress alternative readings. FSM engine would trigger reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether outcomes-based framing naturalizes one IHL reading, foreclosing others').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_outcomes_tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ihl_outcomes_tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ihl_outcomes_tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ihl_outcomes_be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ihl_outcomes_be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ihl_outcomes_be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.18).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, autonomous_weapons_metric_governance).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, contractor_profit_alignment_with_humanitarian_outcomes).

% DUAL FORMULATION NOTE:
% The IHL distinction/proportionality principle decomposes into three structurally distinct readings, each with its own ε value and institutional implications. Outcomes-based reading (ε≈0.52, tangled_rope) permits autonomy where metrics justify it. Human-agency reading would be ε≈0.35 (rope: coordination with constraint that humans remain final decision-makers). Categorical-prohibition reading would be ε≈0.08 (mountain or extreme rope: absolute constraint regardless of technical performance). These are not different measurements of one constraint but different interpretive commitments about what IHL requires. The three readings compete for institutional authority. Outcomes-based reading influences metric governance bodies and contractor behavior; human-agency and categorical-prohibition readings influence humanitarian law custodians and civil-society oversight bodies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
