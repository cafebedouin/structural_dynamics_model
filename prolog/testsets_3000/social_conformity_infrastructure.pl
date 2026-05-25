% ============================================================================
% CONSTRAINT STORY: social_conformity_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_conformity_infrastructure, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_conformity_infrastructure
 *   human_readable: Social Conformity Infrastructure as Cognitive Efficiency Mechanism
 *   domain: philosophy_of_mind/social_psychology/intellectual_autonomy
 *
 * SUMMARY:
 *   The social conformity infrastructure represents the cognitive and social
 *   mechanisms by which individuals adopt group beliefs with minimal
 *   conscious deliberation. This constraint is a candidate natural law — a
 *   structural feature of human social cognition rather than a contingent
 *   institutional arrangement. The infrastructure manifests in Asch-type
 *   conformity experiments (individuals adopt obviously incorrect group
 *   judgments), opinion clustering in social networks (beliefs correlate more
 *   strongly with social ties than with evidence), and the speed differential
 *   between belief adoption and belief examination (people accept group
 *   positions faster than they could independently verify them). The key
 *   structural question is whether this infrastructure is an immutable
 *   feature of human psychology (mountain from all perspectives) or whether
 *   it contains extractive components that could be reduced through
 *   institutional design, education, or technological intervention. The
 *   measurements show minimal drift over civilizational timescales,
 *   suggesting deep structural stability.
 *
 * KEY AGENTS:
 *   - Unreflective Conformer: Powerless/trapped — operates within conformity infrastructure without conscious awareness; experiences it as natural feature of social reality
 *   - Reflective Individual: Moderate/constrained — aware of conformity pressure but recognizes it as coordination mechanism; can resist specific instances but not general phenomenon
 *   - Counter-Cultural Movement: Organized/mobile — attempts to exit mainstream conformity but discovers alternative conformity structures; demonstrates infrastructure invariance
 *   - Educational Institution: Institutional/arbitrage — benefits from efficient belief transmission but recognizes double-edged nature; sees coordination function rather than extraction
 *   - Cognitive Scientist: Analytical/analytical — identifies evolutionary and neurological basis; measures infrastructure as emergent property of social learning architecture
 *   - Intellectual Contrarian: Powerful/mobile — high-agency resistance reveals that contrarian beliefs cluster with same efficiency as conventional ones; infrastructure is substrate-neutral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_conformity_infrastructure, 0.12).
domain_priors:suppression_score(social_conformity_infrastructure, 0.03).
domain_priors:theater_ratio(social_conformity_infrastructure, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_conformity_infrastructure, extractiveness, 0.12).
narrative_ontology:constraint_metric(social_conformity_infrastructure, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(social_conformity_infrastructure, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(social_conformity_infrastructure, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(social_conformity_infrastructure, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_conformity_infrastructure, mountain).
narrative_ontology:human_readable(social_conformity_infrastructure, "Social Conformity Infrastructure as Cognitive Efficiency Mechanism").
narrative_ontology:topic_domain(social_conformity_infrastructure, "philosophy_of_mind/social_psychology/intellectual_autonomy").

domain_priors:emerges_naturally(social_conformity_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_conformity_infrastructure, social_coordination_systems).
narrative_ontology:constraint_beneficiary(social_conformity_infrastructure, group_decision_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREFLECTIVE CONFORMER (MOUNTAIN) — Experiences social conformity as an unchangeable feature of group membership. At immediate timescales and local scope, the cognitive efficiency of adopting group beliefs appears as natural as breathing. No perception of extraction — the mechanism operates below conscious awareness.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REFLECTIVE INDIVIDUAL (MOUNTAIN) — Even with biographical perspective and moderate power, recognizes conformity pressure as a structural feature of human social cognition. Can resist specific instances but cannot exit the general phenomenon. Sees the infrastructure as coordination mechanism rather than extraction — groups that share beliefs coordinate more efficiently than those that don't.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COUNTER-CULTURAL MOVEMENT (MOUNTAIN) — Organized resistance to specific conformity pressures (punk, hippie, rationalist communities) discovers that rejecting mainstream beliefs simply creates alternative conformity structures. The infrastructure itself — the cognitive efficiency of belief-sharing — remains invariant. Can exit specific belief clusters but not the general mechanism.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL INSTITUTION (MOUNTAIN) — Benefits from conformity infrastructure (students adopt pedagogical frameworks efficiently) but recognizes it as a double-edged coordination mechanism, not an extractive tool. The same cognitive architecture that enables rapid knowledge transmission also enables rapid misinformation spread. Sees the infrastructure as a natural law of social learning.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Identifies conformity infrastructure as an emergent property of human social cognition with deep evolutionary roots. Asch experiments, opinion cascades, and belief clustering are surface manifestations of cognitive architecture optimized for rapid social learning. The infrastructure is not a policy choice or institutional arrangement — it is a feature of how human brains process social information. Extractiveness is minimal because the mechanism serves genuine coordination function: groups with shared beliefs can act collectively without constant negotiation.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: INTELLECTUAL CONTRARIAN (MOUNTAIN) — High-agency individual who actively resists conformity pressures discovers that even deliberate non-conformity operates within the same infrastructure. Contrarian positions cluster just as mainstream ones do. The infrastructure is substrate-neutral — it processes contrarian beliefs with the same efficiency as conventional ones. Experiences low extraction because contrarian identity provides social rewards in specific communities.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_conformity_infrastructure_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(social_conformity_infrastructure, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_conformity_infrastructure, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(social_conformity_infrastructure, ExtMetricName, E),
    domain_priors:suppression_score(social_conformity_infrastructure, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(social_conformity_infrastructure),
    narrative_ontology:constraint_metric(social_conformity_infrastructure, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(social_conformity_infrastructure, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(social_conformity_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The conformity infrastructure primarily serves coordination function — groups with shared beliefs can act collectively without constant negotiation. The small extractive component reflects cases where belief adoption bypasses individual judgment in domains where independent verification would be valuable (scientific claims, policy positions, moral judgments). But most conformity serves genuine efficiency: adopting group traffic norms, language conventions, and social protocols without independent derivation is coordination, not extraction. Suppression (0.03): Minimal. Individuals can resist specific conformity pressures with effort, and organized groups can create alternative belief clusters. The infrastructure does not suppress alternatives — it makes shared beliefs cognitively cheaper than independent ones, which is a coordination mechanism. Accessibility collapse (0.92): Very high. The conformity infrastructure is accessible to all social animals with theory of mind. No special training, resources, or institutional position required — the mechanism operates automatically in social contexts. Resistance (0.08): Very low. Attempts to eliminate conformity infrastructure (radical individualist movements, rationalist training programs) consistently fail to prevent belief clustering. The infrastructure persists across cultures, historical periods, and deliberate resistance efforts. Theater ratio (0.15): Very low. The conformity infrastructure is functional, not performative. Belief adoption via social learning genuinely enables coordination. The small theatrical component reflects cases where individuals perform agreement without genuine belief adoption (preference falsification), but this is a minority of conformity instances.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all six perspectives classify as mountain, which is the diagnostic signature of a genuine natural law. The unreflective conformer experiences conformity as unchangeable because they lack awareness of the mechanism. The reflective individual experiences it as unchangeable despite awareness because resistance is costly and only partially effective. The counter-cultural movement experiences it as unchangeable because alternative belief clusters form with the same efficiency as mainstream ones. The educational institution experiences it as unchangeable because the same cognitive architecture that enables teaching also enables conformity. The cognitive scientist measures it as unchangeable because it is rooted in evolutionary and neurological constraints. The intellectual contrarian experiences it as unchangeable because even deliberate non-conformity operates within the same infrastructure. The uniformity across perspectives, combined with high accessibility collapse and low resistance, confirms mountain classification. The constraint passes the natural law certification: emerges naturally (true), accessibility collapse ≥ 0.85 (0.92), resistance ≤ 0.15 (0.08), extractiveness ≤ 0.25 (0.12), suppression ≤ 0.05 (0.03).
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive low directionality values because the conformity infrastructure is primarily a coordination mechanism rather than an extraction mechanism. Educational institutions and social coordination systems are listed as beneficiaries because they gain efficiency from shared belief structures, but this benefit does not come at the expense of victims — it is a positive-sum coordination gain. No victims are declared because the infrastructure does not systematically extract from any agent class. Individual cases of harmful conformity (adopting false beliefs, suppressing dissent) exist, but they are failure modes of a generally beneficial coordination mechanism, not structural extraction. The analytical perspective computes the lowest effective extraction because it sees the full evolutionary and neurological basis — the infrastructure is optimized for rapid social learning, which is net beneficial despite occasional costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that not all social influence mechanisms are extractive. The conformity infrastructure could be misclassified as a snare if one focused only on cases where individuals adopt false beliefs against their interests. But the structural data reveals that the infrastructure is primarily a coordination mechanism: the cognitive efficiency of belief-sharing enables collective action, knowledge transmission, and social learning. The small extractive component (ε = 0.12) reflects genuine costs — cases where conformity bypasses valuable independent judgment — but these costs are outweighed by coordination benefits across most domains. The mountain classification from all perspectives indicates that the infrastructure is not a contingent institutional arrangement that could be redesigned for lower extraction, but rather an emergent property of human social cognition. Attempts to eliminate conformity (radical individualism, rationalist training) consistently fail because the infrastructure serves essential coordination functions. The mandatrophy resolution: social conformity infrastructure is a mountain (natural law of social cognition), not a snare (extractive mechanism), because its primary function is coordination and its extractive component is minimal and structurally unavoidable given current human cognitive architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolutionary_optimality,
    'Is the current conformity infrastructure evolutionarily optimal, or does it reflect ancestral environment mismatch?',
    'Cross-cultural conformity rate comparison; historical analysis of belief adoption speeds in pre-modern vs modern information environments; computational modeling of optimal social learning rates',
    'If optimal: mountain classification confirmed across all contexts. If mismatched: some conformity pressure may be extractive artifact of scale mismatch between evolved mechanisms and modern group sizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_optimality, empirical, 'Whether conformity rates reflect evolutionary optimality or environment mismatch').

omega_variable(
    conscious_override_capacity,
    'What proportion of conformity-driven belief adoption can be consciously overridden with training?',
    'Longitudinal studies of rationality training, meditation practice, or philosophical education on conformity resistance; measurement of sustained vs temporary effects',
    'If override capacity is high and trainable: classification shifts toward rope (coordination mechanism with accessible alternatives). If override capacity is low or temporary: mountain classification confirmed (infrastructure is largely immutable at individual level).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conscious_override_capacity, empirical, 'Trainability of conscious resistance to conformity pressure').

omega_variable(
    digital_amplification,
    'Do digital social networks amplify conformity infrastructure beyond ancestral parameters, creating novel extraction mechanisms?',
    'Comparison of conformity rates and belief adoption speeds in digital vs face-to-face environments; analysis of algorithmic curation effects on opinion clustering',
    'If digital environments merely scale existing mechanisms: mountain classification holds. If digital environments create qualitatively new conformity dynamics: some perspectives may shift to tangled_rope (coordination infrastructure with embedded algorithmic extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_amplification, empirical, 'Whether digital networks create novel conformity mechanisms vs scaling existing ones').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_conformity_infrastructure, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conform_theater_ancestral, social_conformity_infrastructure, theater_ratio, 0, 0.12).
narrative_ontology:measurement(conform_theater_premodern, social_conformity_infrastructure, theater_ratio, 50, 0.14).
narrative_ontology:measurement(conform_theater_modern, social_conformity_infrastructure, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(conform_extract_ancestral, social_conformity_infrastructure, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(conform_extract_premodern, social_conformity_infrastructure, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(conform_extract_modern, social_conformity_infrastructure, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_conformity_infrastructure, information_standard).
narrative_ontology:boltzmann_floor_override(social_conformity_infrastructure, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is formulated at the level of general cognitive infrastructure. Specific instances of harmful conformity (cult dynamics, groupthink in organizations, political polarization) should be modeled as separate constraints with higher extractiveness values. Those constraints exploit the conformity infrastructure but are not identical to it. The infrastructure itself is the substrate; specific exploitative uses are distinct constraint stories that should reference this one via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
