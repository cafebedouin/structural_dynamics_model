% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested International
 *   Humanitarian Law interpretation of the Martens Clause and its application
 *   to lethal autonomous weapons systems (LAWS). The
 *   categorical_prohibition_reading asserts that the Martens Clause
 *   principles of humanity and public conscience prohibit autonomous weapons
 *   per se, regardless of technical performance. Machine-decided killing
 *   violates human dignity categorically. This reading is one of three
 *   structurally distinct interpretations of the same kernel: a
 *   human_agency_reading emphasizes irreducible human moral judgment as the
 *   requirement; an outcomes_based_reading permits autonomous systems if
 *   performance equals or exceeds human operators. This JSON instantiates the
 *   categorical_prohibition_reading as a clean, ε-invariant constraint. The
 *   reading's claim (mountain: natural humanitarian principle) and its
 *   metrics (high extraction, high suppression for military powers) are
 *   authored independently per the schema's claim/metric independence rule.
 *   The divergence between claim and metrics is diagnostic — it enables the
 *   false-summit detector to flag whether this reading is genuinely natural
 *   law or a constructed constraint that benefits identifiable civil society
 *   and non-capable-state actors.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society: International NGO coalitions (Human Rights Watch, International Committee of the Red Cross advocacy arms) that author and defend the categorical prohibition reading.
 *   - states_without_laws_capability: Non-advanced military powers that benefit strategically from a ban that prevents technological elites from achieving asymmetric advantage.
 *   - states_with_advanced_autonomous_systems: The United States, China, Russia, Israel, and allied military powers that have invested in autonomous system development and face technological constraint.
 *   - military_technological_advantage_holders: Defense contractors (Palantir, Booz Allen, Northrop Grumman, Chinese and Russian military research institutes) sunk in autonomous system development.
 *   - international_humanitarian_law_interpreters: ICRC, UN mechanisms, and legal scholarship that authoritatively resolve the Martens Clause interpretation.
 *   - civilian_populations: Bearing both benefit (no machine-decided targeting) and cost (potential escalation to less precise weapons if autonomous systems are banned).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.88).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.76).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, mountain).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics").

domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '6b2a1475-ad2e-4d44-b844-da3362266c4d').
narrative_ontology:cs_kernel_codification('6b2a1475-ad2e-4d44-b844-da3362266c4d', fixed_text).
narrative_ontology:cs_authority_grounding('6b2a1475-ad2e-4d44-b844-da3362266c4d', lineage).
narrative_ontology:cs_interpretation_layer_present('6b2a1475-ad2e-4d44-b844-da3362266c4d').
narrative_ontology:cs_reading_relation('6b2a1475-ad2e-4d44-b844-da3362266c4d', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b2a1475-ad2e-4d44-b844-da3362266c4d', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('6b2a1475-ad2e-4d44-b844-da3362266c4d', foundational, human_dignity_inviolable_against_delegated_killing).
narrative_ontology:cs_axiom_status(human_dignity_inviolable_against_delegated_killing, holdable).
narrative_ontology:cs_axiom_grounding('6b2a1475-ad2e-4d44-b844-da3362266c4d', human_dignity_inviolable_against_delegated_killing, deontological).
narrative_ontology:cs_axiom('6b2a1475-ad2e-4d44-b844-da3362266c4d', foundational, martens_clause_supremacy_over_military_necessity).
narrative_ontology:cs_axiom_status(martens_clause_supremacy_over_military_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6b2a1475-ad2e-4d44-b844-da3362266c4d', martens_clause_supremacy_over_military_necessity, deontological).
narrative_ontology:cs_reference_frame('6b2a1475-ad2e-4d44-b844-da3362266c4d', categorical_humanitarian_boundary).
narrative_ontology:cs_drift_state('6b2a1475-ad2e-4d44-b844-da3362266c4d', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b2a1475-ad2e-4d44-b844-da3362266c4d', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_laws_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_holders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, ExtMetricName, E),
    domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.42 → 0.88 over 25 years) reflects mounting pressure on military powers and defense contractors as the categorical prohibition reading gains legal and diplomatic legitimacy. Early 2010s saw the reading as marginal (low extractiveness); by 2020 it became the dominant interpretation in UN forums, treaty negotiations, and ICRC doctrine (extractiveness ~0.74); projections to 2035 assume further consolidation as more states codify the prohibition, making it increasingly costly for advanced powers to field autonomous systems (extractiveness 0.88). Suppression (0.76) is high because enforcement depends on treaty verification (drone/satellite surveillance of military testing), sanctions against violators, and diplomatic pressure — all active suppressive mechanisms required to keep states from defecting to autonomous system deployment. Theater_ratio (0.28) is moderate-low because the humanitarian principle is genuine (not purely performative), but enforcement rhetoric sometimes overstates the humanitarian benefit and understates the strategic cost-transfer to military powers. Accessibility_collapse (0.92) is very high because the categorical boundary (human vs. machine decision-making) is semantically sharp — once the principle is articulated, alternatives (outcomes-based evaluation, human-in-the-loop hybrids) appear as compromises rather than principled positions. Resistance (0.72) reflects strong pushback from military establishments, technology firms, and strategic doctrine communities that view the prohibition as inefficient and escalatory.
 *
 * PERSPECTIVAL GAP:
 *   The categorical_prohibition_reading produces radically divergent seated experiences. From the perspective of anti-militarist civil society and humanitarian law interpreters, the constraint is a discovery of inviolable principle — it *feels* like a mountain, a natural law that flows from respect for human dignity. From the perspective of military powers and defense contractors, the same constraint is experienced as a politically motivated extraction of technological advantage — a snare that uses humanitarian rhetoric to achieve strategic parity redistribution. The engine computes this seated divergence from the structural data: the beneficiary/victim declarations, the power atoms, the exit options. A civil society observer (power: organized, exit: arbitrage) computes a different d than a military power (power: institutional, exit: constrained, victim status). This is not a measurement error — it is the core signal the constraint story exists to capture. The claim (mountain: categorical prohibition as natural law) and the metrics (high extraction, high suppression for identified victims) are authored independently. Their divergence is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   The categorical prohibition reading creates a structural asymmetry in who benefits and who bears costs. Anti-militarist civil society benefits (d near 0.0 — beneficiary, organized power, arbitrage exit, vindicates their platform). States without autonomous capability benefit (d near 0.2 — moderate power, mobile exit, strategic relief from asymmetric disadvantage). Military powers and defense contractors are targets (d near 1.0 — powerful but constrained exit, victims of technology foreclosure, bear the cost of system redesign and forgoing advantage). Civilian populations in conflict zones are ambiguous (d near 0.5 — genuine benefit from no machine-decided targeting, but also bear diffuse cost if militaries respond with less precise alternatives). The directionality derivation flows from these structural relationships; no overrides are needed because the beneficiary/victim declarations and exit options are coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was live in 2010-2015 as autonomous system development accelerated and humanitarian law interpreters faced an urgent interpretive choice. The categorical_prohibition_reading resolves the ambiguity by asserting the Martens Clause permits no delegation of life-death decisions to machines, regardless of performance. By 2025, the reading has been adopted in ICRC position papers, UN Group of Governmental Experts resolutions, and advocacy by over 150 states. The mandatrophy question is whether the founding problem remains live or has become contested/dead. The six_questions entry declares it 'live' because militaries continue to develop autonomous systems, debate persists over outcomes-based evaluation, and the prohibition has not been formalized in binding treaty language. The constraint persists as active negotiation, not as settled doctrine. The measurement series (suppression_requirement rising from 0.38 to 0.76) reflects mounting enforcement effort to prevent defection as the technological incentive to deploy autonomous systems grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the categorical prohibition of autonomous weapons a discovery of inviolable humanitarian principle (natural law — Martens Clause as transhistorical constraint on human behavior), or a constructed international legal position that benefits certain state actors and civil society coalitions?',
    'Comparative historical and anthropological analysis of how different political systems and military traditions ground their prohibition or acceptance of autonomous systems; examination of whether the Martens Clause principle (human dignity, public conscience) exhibits universality or is culturally/institutionally contingent.',
    'If natural law: the constraint is genuinely a mountain (emerging necessarily from human ethical commitment). If constructed: beneficiary presence (anti-militarist civil society, non-capable states) and victim presence (advanced-capability military powers) suggest FSM — a false-summit constraint that claims naturalness to legitimize political extraction. Classification shifts from mountain to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'Whether the categorical prohibition is grounded in irreducible humanitarian principle or constructed to serve political interests.').

omega_variable(
    outcomes_vs_rules_interpretive_dominance,
    'Within the Martens Clause framework itself, is the humanitarian principle satisfied by outcomes (distinguishing combatants from civilians, respecting proportionality) or by rules (requiring irreducible human moral judgment in the act of lethal force)?',
    'Textual analysis of Martens Clause drafting history and interpretive traditions; comparative examination of how the ICRC and competing legal scholars resolve the outcomes/rules tension; empirical study of whether autonomous systems can achieve distinction/proportionality performance that equals or exceeds human operators.',
    'If outcomes-based interpretation is equally valid: the categorical prohibition is one reading among coherent alternatives, not a discovery of natural law. The constraint becomes a *choice* made by humanitarian law interpreters in favor of the deontological (rules-based) reading over the consequentialist (outcomes-based) reading. Classification remains mountain in the categorical_prohibition_reading but acknowledges the sibling readings as structurally legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(outcomes_vs_rules_interpretive_dominance, conceptual, 'Whether the Martens Clause is satisfied by outcomes or rules in autonomous weapon regulation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.76 at interval end) structural (military powers prevented by treaty enforcement, verification, and sanctions from deploying autonomous systems) or internalized (military and defense communities have come to believe in the humanitarian principle and voluntarily comply)?',
    'Post-prohibition behavior monitoring: if military powers caught deploying autonomous systems face enforcement (sanctions, treaty withdrawal, international legal proceedings), suppression is structural. If compliance persists even when enforcement attention wanes, suppression is internalized. Survey data from military leadership on true acceptance of the prohibition versus rhetorical compliance.',
    'If structural: the constraint depends on continuous enforcement infrastructure and faces defection risk when enforcement weakens. If internalized: the constraint is more stable but may be more fragile to technological change (if military operators genuinely internalize the humanitarian principle, they may resist it when autonomous systems demonstrably improve civilian protection). If both: the split between structural and internalized suppression informs the sustainability of the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of autonomous weapons deployment is structural (external enforcement) or internalized (acceptance of principle).').

omega_variable(
    reading_contingency_on_technological_trajectory,
    'The categorical prohibition assumes a categorical boundary between human-decided and machine-decided lethal force. What happens if autonomous systems become indistinguishable from human judgment in real time (operator-invisible algorithmic processes)? Does the reading depend on maintaining the *perceptual* boundary of machine agency, or the *structural* reality of machine decision-making?',
    'Thought experiment and legal doctrine analysis: if an autonomous system''s targeting logic becomes empirically indistinguishable from human judgment (same accuracy, same proportionality reasoning, same explanation capability), can the categorical prohibition hold? Comparative examination of whether other IHL prohibitions (biological weapons, chemical weapons) survive if the prohibited agent becomes functionally equivalent to permitted agents.',
    'If the prohibition depends on perceptual boundary maintenance: it is vulnerable to technological obfuscation and to advances that blur human/machine agency. If it depends on structural reality: it remains stable even if technology advances. If it depends on both: the constraint faces a long-term tension between its stated principle (human dignity, humanity) and its operational implementation (machine indistinguishability from human judgment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_on_technological_trajectory, conceptual, 'Whether the categorical prohibition can survive technological convergence between human and machine decision-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2010, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(ihl__tr_t2025, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2025, 0.24).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2030, 0.26).
narrative_ontology:measurement(ihl__tr_t2035, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2035, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement(ihl__be_t2025, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2030, 0.85).
narrative_ontology:measurement(ihl__be_t2035, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2035, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2020, 0.64).
narrative_ontology:measurement(ihl__su_t2025, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2030, 0.74).
narrative_ontology:measurement(ihl__su_t2035, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2035, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.18).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member constraint family decomposing the contested Martens Clause interpretation of autonomous weapons. Each reading (categorical_prohibition, human_agency, outcomes_based) has distinct ε, distinct beneficiary/victim structure, and distinct classification. The categorical_prohibition_reading instantiated here claims mountain status (natural humanitarian principle) with high extractiveness and suppression metrics authored to capture the political asymmetry it imposes. The outcomes_based_reading (a sibling) would have substantially lower extractiveness (permits technology if outcomes are met), different beneficiaries (military powers, technology firms), and different suppression requirements. The three readings are linked by network.affects_constraints; each story documents its reading and its relationship to siblings in cs_structure and omegas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
