% ============================================================================
% CONSTRAINT STORY: desire_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_desire_opacity, []).

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
 *   constraint_id: desire_opacity
 *   human_readable: Desire Opacity: The Structural Unknowability of Preference Under Uncertainty
 *   domain: moral_psychology/existential_philosophy/decision_theory
 *
 * SUMMARY:
 *   Desire opacity — the structural difficulty of knowing what you want
 *   before experiencing the outcome of your choice — is a candidate natural
 *   law constraint at the intersection of moral psychology, existential
 *   philosophy, and decision theory. The constraint manifests as systematic
 *   preference reversals under reflection, gaps between stated and revealed
 *   preferences in longitudinal studies, and the constructed (rather than
 *   discovered) nature of preference formation. Unlike extractive constraints
 *   that benefit some agents at the expense of others, desire opacity is a
 *   universal feature of selfhood under uncertainty: all agents face the same
 *   temporal asymmetry between choice and experience. The constraint's very
 *   low extractiveness (0.08) reflects that the 'cost' of not knowing your
 *   preferences in advance is not extraction by another agent but an inherent
 *   information limit. The low theater ratio (0.15) reflects that most
 *   decision-making practices (deliberation, introspection, advice-seeking)
 *   have genuine functional value even though they cannot overcome the
 *   underlying opacity. This constraint is a strong candidate for mountain
 *   classification from all perspectives because it appears invariant across
 *   power levels, time horizons, exit options, and spatial scopes.
 *
 * KEY AGENTS:
 *   - The Deliberating Agent: Universal subject (powerless/trapped at immediate horizon) — faces total opacity in the moment of choice; cannot access future preferences
 *   - The Reflective Self: Biographical observer (moderate/constrained) — observes systematic preference reversals over lifetime; constraint persists despite self-knowledge efforts
 *   - Behavioral Economics Community: Research collective (organized/mobile) — documents preference construction across decades of studies; finds constraint invariant
 *   - Philosophical Tradition: Institutional knowledge (institutional/arbitrage) — existentialism identifies opacity as condition of freedom, not a problem to solve
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees opacity as logical consequence of temporal asymmetry and constructed preference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(desire_opacity, 0.08).
domain_priors:suppression_score(desire_opacity, 0.03).
domain_priors:theater_ratio(desire_opacity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(desire_opacity, extractiveness, 0.08).
narrative_ontology:constraint_metric(desire_opacity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(desire_opacity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(desire_opacity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(desire_opacity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(desire_opacity, mountain).
narrative_ontology:human_readable(desire_opacity, "Desire Opacity: The Structural Unknowability of Preference Under Uncertainty").
narrative_ontology:topic_domain(desire_opacity, "moral_psychology/existential_philosophy/decision_theory").

domain_priors:emerges_naturally(desire_opacity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DELIBERATING AGENT / IMMEDIATE HORIZON (MOUNTAIN) — In the moment of choice, the agent cannot access their own future preferences. The opacity is total and structural: no amount of introspection reveals what you will want after experiencing the outcome. This is not a failure of self-knowledge but a temporal constraint on information access.
constraint_indexing:constraint_classification(desire_opacity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: REFLECTIVE SELF / BIOGRAPHICAL HORIZON (MOUNTAIN) — Over a lifetime, the agent observes systematic preference reversals and stated-revealed gaps. The pattern is invariant: preferences are constructed through experience, not discovered through introspection. The constraint persists across all attempts to 'know thyself' — it is a feature of how preferences form, not a bug in self-awareness.
constraint_indexing:constraint_classification(desire_opacity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: BEHAVIORAL ECONOMICS COMMUNITY / GENERATIONAL HORIZON (MOUNTAIN) — Decades of preference reversal studies, framing effects, and constructed preference research converge on the same finding: preferences are not pre-existing objects waiting to be discovered but are constructed in the act of choosing. The research community has high mobility (can study other phenomena) but finds the constraint invariant across cultures, contexts, and methodologies.
constraint_indexing:constraint_classification(desire_opacity, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: PHILOSOPHICAL TRADITION / CIVILIZATIONAL HORIZON (MOUNTAIN) — Existentialist philosophy (Sartre, Heidegger, Kierkegaard) identifies desire opacity as a fundamental feature of human freedom: we are 'condemned to be free' precisely because there is no pre-existing essence to discover. The constraint is not a problem to solve but the condition of authentic choice. Institutional actors (philosophy departments, existentialist schools) have arbitrage exit but find the constraint universal.
constraint_indexing:constraint_classification(desire_opacity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the analytical position, desire opacity is a logical consequence of temporal asymmetry and the constructed nature of preference. An agent at time T1 cannot know their preferences at T2 because those preferences do not yet exist — they will be constructed through the experience of the choice and its outcome. This is not contingent on culture, technology, or institutional arrangement. It is a structural feature of selfhood under uncertainty.
constraint_indexing:constraint_classification(desire_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(desire_opacity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(desire_opacity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(desire_opacity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(desire_opacity, ExtMetricName, E),
    domain_priors:suppression_score(desire_opacity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(desire_opacity),
    narrative_ontology:constraint_metric(desire_opacity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(desire_opacity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(desire_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes a cost (uncertainty, potential regret, decision difficulty) but this cost is not extraction by another agent — it is an inherent information limit. The small non-zero value reflects that some institutional actors (therapy industries, decision consulting, self-help markets) do extract rent from the constraint by promising to overcome it, but the core constraint itself is not extractive. Suppression (0.03): Minimal. Agents are free to deliberate, seek advice, experiment with choices, and revise preferences. The constraint does not suppress alternatives — it is the condition under which alternatives are evaluated. The small non-zero value reflects that some cultural or institutional contexts discourage preference exploration (authoritarian regimes, rigid social norms), but these are contingent suppressions layered on top of the natural law, not intrinsic to it. Theater ratio (0.15): Low. Most decision-making practices (introspection, deliberation, advice-seeking, trial-and-error) have genuine functional value even though they cannot eliminate opacity. The theater component reflects practices that claim to reveal pre-existing preferences (personality tests, values clarification exercises) when preferences are actually constructed through choice. Accessibility collapse (0.92): Very high. No amount of effort, resources, or technology currently available can give an agent at T1 full access to their preferences at T2 before experiencing the choice. The collapse is nearly total. Resistance (0.08): Very low. Attempts to overcome the constraint (better introspection, predictive modeling, neurotechnology) consistently fail to eliminate preference reversals and stated-revealed gaps. The constraint resists intervention because it is structural, not contingent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap in classification — all five perspectives return mountain. This uniformity is the diagnostic signature of a natural law constraint: the classification is invariant across power levels (powerless to institutional), time horizons (immediate to civilizational), exit options (trapped to arbitrage), and spatial scopes (all universal). The deliberating agent in the moment of choice faces the same structural opacity as the philosophical tradition observing across millennia. The behavioral economics community with high mobility finds the same constraint as the powerless individual with no exit. This invariance distinguishes desire opacity from contingent institutional arrangements that appear as mountains from some perspectives but reveal extraction from others. The constraint's resistance to perspectival variation is evidence for its natural law status.
 *
 * DIRECTIONALITY LOGIC:
 *   Desire opacity has no beneficiaries or victims in the conventional sense — it is a universal constraint that all agents face equally. The directionality values for all perspectives derive from the canonical fallback for their power atoms, not from structural relationships to extraction flow. All perspectives classify as mountain because the constraint is invariant: powerless agents at immediate horizons face the same opacity as institutional actors at civilizational horizons. The constraint does not concentrate costs on the powerless or benefits on the powerful — it is a feature of the decision-making structure itself. The small extractiveness value (0.08) reflects secondary extraction by industries that claim to overcome the constraint, not primary extraction by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: Desire opacity resolves the mandatrophy by demonstrating a constraint that is genuinely non-extractive and universal. The mandatrophy question 'Is this coordination or extraction?' does not apply because the constraint is neither — it is a logical feature of temporal asymmetry and preference construction. There is no hidden beneficiary extracting from the opacity, no suppressed alternative that would eliminate it, and no institutional arrangement that could be reformed to overcome it. The constraint is not maintained by power but by the structure of selfhood under uncertainty. The small extractiveness value (0.08) reflects secondary markets (therapy, consulting, self-help) that extract rent by claiming to overcome the constraint, but these are parasitic on the natural law, not constitutive of it. The mountain classification from all perspectives is not a failure to detect extraction but a correct identification of a structural limit. This constraint demonstrates that not all costs are extraction and not all constraints are maintained by power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_construction_mechanism,
    'Are preferences constructed entirely through experience, or is there a discoverable pre-existing preference structure that experience merely reveals?',
    'Longitudinal studies tracking preference stability across repeated exposure to identical choice scenarios; neuroimaging studies of preference formation vs preference retrieval; cross-cultural invariance testing',
    'If preferences are entirely constructed: mountain classification confirmed across all perspectives. If partially discoverable: some agents with better introspective access might experience lower opacity, creating a perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preference_construction_mechanism, empirical, 'Whether preferences are constructed or discovered').

omega_variable(
    technological_introspection_enhancement,
    'Could future neurotechnology (brain-computer interfaces, predictive preference modeling) reduce desire opacity by providing direct access to preference formation processes?',
    'Development and testing of neural preference prediction systems; comparison of predicted vs experienced preferences; assessment of whether prediction accuracy improves with technology or hits a structural ceiling',
    'If technology can reduce opacity: mountain classification weakens over time (becomes scaffold with technological sunset). If opacity persists despite technology: mountain classification confirmed as fundamental limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_introspection_enhancement, empirical, 'Whether technology can overcome desire opacity').

omega_variable(
    cultural_variation_in_opacity,
    'Do cultures with different models of selfhood (individualist vs collectivist, narrative vs experiential) experience different levels of desire opacity?',
    'Cross-cultural preference reversal studies; comparison of stated-revealed preference gaps across cultural contexts; ethnographic analysis of decision-making practices',
    'If opacity varies by culture: mountain classification is too strong — the constraint is partly cultural (tangled rope from some perspectives). If invariant: mountain classification confirmed as universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_variation_in_opacity, empirical, 'Whether desire opacity varies across cultures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(desire_opacity, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(desire_opacity_tr_t0, desire_opacity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(desire_opacity_tr_t50, desire_opacity, theater_ratio, 50, 0.15).
narrative_ontology:measurement(desire_opacity_tr_t100, desire_opacity, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(desire_opacity_be_t0, desire_opacity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(desire_opacity_be_t50, desire_opacity, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(desire_opacity_be_t100, desire_opacity, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(desire_opacity, information_standard).

% DUAL FORMULATION NOTE:
% Desire opacity is a foundational constraint in decision theory and moral psychology. It does not decompose into multiple observables with different epsilon values — the preference reversal phenomenon, stated-revealed gaps, and constructed preference findings all point to the same structural feature. This constraint is a singleton, not a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
