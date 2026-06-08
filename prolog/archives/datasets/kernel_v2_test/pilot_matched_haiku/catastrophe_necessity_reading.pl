% ============================================================================
% CONSTRAINT STORY: catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_necessity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The catastrophe-necessity reading asserts that genuine competence in
 *   high-reliability organizations (nuclear operations, aviation, emergency
 *   response, medical trauma) requires exposure to irreducible uncertainty
 *   that only actual catastrophic events provide. Simulation, by definition,
 *   removes existential stakes and allows operators to reset after failure —
 *   properties that fundamentally distinguish it from catastrophic reality.
 *   This reading claims that competence decays in catastrophe-free periods
 *   because the stress-inoculation mechanism that maintains readiness
 *   requires genuine uncertainty about outcomes. The constraint operates as a
 *   natural law: the psychological and organizational mechanisms that sustain
 *   competence under catastrophic conditions cannot be fully replicated in
 *   simulation. This reading is one of four competing interpretations of the
 *   kernel 'catastrophe_proxy_sufficiency' — the contested question of
 *   whether catastrophic events are necessary for competence or whether
 *   sufficiently high-fidelity proxies can substitute. The
 *   catastrophe-necessity reading naturalizes the constraint as immutable;
 *   the simulation-as-proxy reading treats it as a technological problem; the
 *   hybrid-degradation reading sees both as necessary; the
 *   simulation-fidelity-threshold reading treats it as a matter of
 *   engineering specification. Each reading has different implications for
 *   safety engineering practice, training investment, and acceptable risk.
 *
 * KEY AGENTS:
 *   - Operators in catastrophe-free periods: Primary victim (powerless/trapped) — competence decays without catastrophic stress; cannot exit the constraint that their skills atrophy during safe periods
 *   - Safety-critical organizations: Secondary victim (moderate/constrained) — constrained by the irreducible gap between simulation fidelity and catastrophic reality; cannot fully escape the constraint
 *   - Catastrophe-driven learning advocates: Primary beneficiary (institutional/arbitrage) — benefit from the doctrine that catastrophe is necessary; can shift between simulation-based and catastrophe-based training regimes; naturalize the constraint to resist simulation investment
 *   - Safety engineering establishment: Institutional actor (institutional/arbitrage) — maintains the catastrophe-necessity doctrine; sees it as a natural law; benefits from the authority to certify competence through catastrophe exposure
 *   - Simulation technology vendors: Secondary beneficiary (powerful/mobile) — have incentive to challenge the catastrophe-necessity doctrine; benefit from simulation investment; can exit the constraint by developing higher-fidelity systems
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as an immutable law of human psychology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(catastrophe_necessity_reading, 0.72).
domain_priors:theater_ratio(catastrophe_necessity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_necessity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_necessity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(catastrophe_necessity_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_necessity_reading, "Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_necessity_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:emerges_naturally(catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_necessity_reading, '71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d').
narrative_ontology:cs_kernel_codification('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', distributed).
narrative_ontology:cs_authority_grounding('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', extraction).
narrative_ontology:cs_interpretation_layer_present('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d').
narrative_ontology:cs_reading_relation('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', catastrophe_necessity_reading__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', catastrophe_necessity_reading__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', catastrophe_necessity_reading__simulation_fidelity_threshold_reading, influences).
narrative_ontology:cs_axiom('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', foundational, irreducible_uncertainty_principle).
narrative_ontology:cs_axiom_status(irreducible_uncertainty_principle, holdable).
narrative_ontology:cs_axiom_grounding('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', irreducible_uncertainty_principle, empirically_contingent).
narrative_ontology:cs_axiom('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', foundational, stress_inoculation_necessity).
narrative_ontology:cs_axiom_status(stress_inoculation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', stress_inoculation_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', catastrophe_necessity_doctrine).
narrative_ontology:cs_drift_state('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', contemporary_simulation_advancement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71cdf1df-c69c-4f36-b1d4-e4eb1ab1883d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_necessity_reading, catastrophe_driven_learning_advocates).
narrative_ontology:constraint_victim(catastrophe_necessity_reading, operational_safety_margins).
narrative_ontology:constraint_victim(catastrophe_necessity_reading, personnel_exposed_to_catastrophic_risk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATOR IN CATASTROPHE-FREE PERIODS (MOUNTAIN) — Trapped in a system where competence decays without catastrophic stress. Cannot exit the constraint that their skills atrophy during safe periods. The operator experiences the necessity as immutable: no simulation can substitute for the irreducible uncertainty of actual catastrophe. Competence maintenance requires exposure to genuine risk — this is perceived as an unchangeable law of human psychology and organizational learning.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY-CRITICAL ORGANIZATION (MOUNTAIN) — Constrained by the irreducible gap between simulation fidelity and catastrophic reality. The organization cannot fully escape the constraint that genuine competence requires exposure to actual high-stakes scenarios. Even with advanced simulation, the organization faces the structural reality that operators trained only in simulation show degraded performance under actual catastrophic conditions. This is experienced as a natural limit on training effectiveness.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY ENGINEERING ESTABLISHMENT (MOUNTAIN) — Institutional actors with arbitrage options (can shift between simulation-based and catastrophe-based training regimes) still perceive the constraint as a natural law: the irreducible stress of actual catastrophe cannot be fully replicated. The establishment sees this as a fundamental property of human learning under uncertainty — simulation is always insufficient because it lacks the existential stakes of real catastrophe. This perspective naturalizes the constraint as a civilizational fact about how competence is maintained.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational/universal perspective, the constraint appears as an immutable property of human psychology and organizational learning: genuine competence in high-stakes domains requires exposure to irreducible uncertainty that only actual catastrophe provides. Simulation, by definition, removes the existential stakes. This perspective sees the constraint as a natural law of learning under uncertainty — no amount of technological advancement can eliminate the gap between simulated and actual catastrophic stress.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(catastrophe_necessity_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_necessity_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from operators and safety-critical organizations by requiring exposure to catastrophic risk to maintain competence. The extraction is substantial because it forces a choice between competence and safety — organizations cannot simultaneously minimize catastrophe risk and maintain operator readiness. The beneficiaries (catastrophe-driven learning advocates, safety engineering establishment) collect authority and institutional legitimacy from the doctrine that catastrophe is necessary. The extractiveness increases over time (0.55 → 0.71) as simulation technology improves and the gap between simulation and catastrophe becomes more salient — the constraint's extraction mechanism strengthens as alternatives become more visible. Suppression (0.72): High. The constraint suppresses alternatives through institutional authority: the safety engineering establishment certifies competence through catastrophe-exposure doctrine; simulation-based training is treated as insufficient; operators cannot exit the constraint without accepting reduced competence certification. The suppression increases over time (0.60 → 0.75) as the doctrine becomes more entrenched and simulation technology improvements are resisted. Theater ratio (0.55): Moderate. The constraint has genuine functional content (catastrophic stress does produce learning effects) but also performative content (the doctrine naturalizes what may be a contingent institutional arrangement; catastrophe-necessity advocates resist fidelity improvements to maintain their authority). The theater increases over time (0.48 → 0.58) as the doctrine becomes more theatrical — maintained through institutional inertia rather than empirical necessity.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as mountain, but the gap is not in classification type — it is in the underlying axiomatic disagreement about whether the constraint is truly natural or constructed. The powerless operator and moderate organization experience the constraint as immutable natural law because they have no exit options and cannot challenge the doctrine. The institutional beneficiary experiences it as natural law because naturalizing the constraint legitimizes their authority. The analytical observer risks naturalizing it as a civilizational fact about human learning. But the structural data reveals a false summit: the constraint has identifiable beneficiaries (catastrophe-driven learning advocates) who benefit from the doctrine; the extractiveness is increasing over time as simulation technology improves; the suppression is increasing as the doctrine becomes more entrenched. These are signatures of a constructed constraint, not a natural law. The false summit detector should flag this constraint as a candidate for reclassification to tangled_rope or snare once the empirical resolution mechanisms (fidelity asymptote, frequency threshold, performance comparison) produce data.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the constraint. Operators and safety-critical organizations are trapped or constrained — they cannot exit without accepting reduced competence certification. They experience high directionality (d ≈ 0.8-0.9) toward the constraint as targets. Catastrophe-driven learning advocates and the safety engineering establishment are institutional actors with arbitrage options — they can shift between simulation-based and catastrophe-based training regimes. They experience low directionality (d ≈ 0.1-0.2) as beneficiaries. The analytical observer has analytical exit options and no structural stake — d ≈ 0.5 (symmetric). The constraint's effective extractiveness (χ) is amplified for trapped/constrained agents and damped for institutional beneficiaries. The false summit signature emerges because the constraint has declared beneficiaries (catastrophe-driven learning advocates) despite claiming to be a natural law — this triggers the FSM evaluation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The catastrophe-necessity reading resolves mandatrophy by showing that the constraint's mandate (maintaining competence in high-reliability organizations) has not outlived its function — competence maintenance is still necessary. However, the reading leaves open the question of whether catastrophe is the only mechanism for maintaining competence, or whether the doctrine naturalizes a contingent institutional arrangement. The mandatrophy is not resolved by the constraint itself but by the empirical resolution mechanisms: if simulation fidelity can approach sufficiency, the mandate persists but the catastrophe-necessity doctrine is revealed as constructed. If catastrophe is irreducibly necessary, the mandate persists and the doctrine is vindicated. The constraint story exists to model this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_beneficiary,
    'Is the constraint that ''only catastrophe maintains competence'' a genuine natural law of human psychology, or a constructed narrative that benefits catastrophe-driven learning advocates who resist simulation investment?',
    'Longitudinal comparison of operator performance: catastrophe-trained cohorts vs simulation-trained cohorts with equivalent total training hours and fidelity levels. Controlled studies isolating stress-inoculation effects from selection bias (catastrophe-exposed operators may be self-selected for higher baseline competence). Cross-domain analysis: domains that have successfully transitioned to simulation-only training (aviation, nuclear operations) vs domains that maintain catastrophe-necessity doctrine.',
    'If natural law: the mountain classification is correct; simulation is fundamentally limited; catastrophe exposure is irreducible. If constructed: the constraint is a tangled_rope or snare — beneficiaries (catastrophe-driven learning advocates, certain training institutions) naturalize a contingent institutional arrangement to resist simulation investment and maintain their authority over competence certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_beneficiary, empirical, 'Whether catastrophe necessity is a natural law or a constructed narrative benefiting certain institutional actors').

omega_variable(
    simulation_fidelity_asymptote,
    'Is there a theoretical asymptote to simulation fidelity that prevents replication of catastrophic stress, or is the gap between simulation and catastrophe a matter of current technological/economic constraints that could be overcome?',
    'Analysis of simulation fidelity improvements over time: has the gap between simulated and actual catastrophic performance narrowed as simulation technology advanced? Identification of specific irreducible elements (existential stakes, genuine uncertainty about outcomes, time pressure under genuine risk) that cannot be simulated vs elements that are merely expensive to simulate. Neuroscientific investigation: do catastrophe-trained and simulation-trained operators show different neural activation patterns under stress, and if so, can simulation be designed to produce equivalent activation?',
    'If asymptote exists: mountain classification holds; the constraint is a natural limit. If gap is technological: the constraint may be a scaffold (temporary, with sunset as simulation improves) or a snare (beneficiaries resist fidelity improvements to maintain catastrophe-necessity doctrine).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_asymptote, empirical, 'Whether simulation fidelity has a theoretical limit or is constrained by current technology').

omega_variable(
    catastrophe_frequency_threshold,
    'What frequency of actual catastrophic events is necessary to maintain competence, and is that frequency compatible with acceptable safety margins?',
    'Historical analysis of operator competence decay rates in catastrophe-free periods. Identification of minimum catastrophe frequency required to prevent skill atrophy. Comparison with acceptable safety margins: if competence requires catastrophes every N years, but acceptable safety margins allow catastrophes only every 10N years, the constraint creates an irreducible tension between learning and safety.',
    'If frequency is compatible with safety margins: the constraint is a coordination problem (rope). If frequency exceeds acceptable margins: the constraint is a snare — maintaining competence requires accepting unacceptable catastrophe risk. If frequency is zero (competence can be maintained without catastrophe): the mountain classification is false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_frequency_threshold, empirical, 'Minimum catastrophe frequency required for competence maintenance vs acceptable safety margins').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the contested kernel ''catastrophe_proxy_sufficiency'' — the question of whether catastrophic events are necessary for competence or whether sufficiently high-fidelity proxies (simulations) can substitute. What distinguishes this reading from the sibling reading that simulation can serve as an adequate proxy?',
    'The distinction is axiomatic: this reading holds that catastrophe is irreducibly necessary (axiom: irreducible_uncertainty_principle); the sibling reading holds that simulation fidelity can approach sufficiency (axiom: simulation_fidelity_convergence). These axioms coexist across different institutional positions — catastrophe-necessity advocates vs simulation-investment advocates — but neither logically forecloses the other within a single framework. The empirical resolution mechanisms above (fidelity asymptote, frequency threshold, performance comparison) will determine which reading''s axioms are vindicated.',
    'If this reading''s axioms are vindicated: catastrophe necessity is a natural law; simulation-based training is fundamentally limited. If the sibling reading''s axioms are vindicated: the constraint is a scaffold or snare, not a mountain. The kernel reading ambiguity is the irreducible uncertainty that this constraint story exists to model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between catastrophe-necessity and simulation-sufficiency readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_necessity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catast_theater_t0, catastrophe_necessity_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(catast_theater_t5, catastrophe_necessity_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement(catast_theater_t10, catastrophe_necessity_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(catast_theater_t15, catastrophe_necessity_reading, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(catast_extract_t0, catastrophe_necessity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(catast_extract_t5, catastrophe_necessity_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(catast_extract_t10, catastrophe_necessity_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(catast_extract_t15, catastrophe_necessity_reading, base_extractiveness, 15, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(catast_suppress_t0, catastrophe_necessity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(catast_suppress_t5, catastrophe_necessity_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(catast_suppress_t10, catastrophe_necessity_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(catast_suppress_t15, catastrophe_necessity_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_necessity_reading, simulation_fidelity_threshold_reading).

% DUAL FORMULATION NOTE:
% The catastrophe-necessity reading is one of four structurally distinct constraints that share the kernel 'catastrophe_proxy_sufficiency'. Each reading has a different ε value reflecting different empirical claims about whether catastrophe is necessary. The catastrophe-necessity reading (this file) claims high extractiveness (0.68) because it asserts that competence requires catastrophic exposure. The simulation-as-proxy reading claims lower extractiveness because it asserts that simulation can substitute. The hybrid-degradation reading claims moderate extractiveness because it asserts both are necessary. The simulation-fidelity-threshold reading claims low extractiveness because it treats the question as a matter of engineering specification. These are not the same constraint viewed from different angles — they are different constraints with different ε values, linked by the kernel they contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
