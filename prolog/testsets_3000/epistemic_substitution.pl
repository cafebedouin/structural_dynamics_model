% ============================================================================
% CONSTRAINT STORY: epistemic_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_substitution, []).

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
 *   constraint_id: epistemic_substitution
 *   human_readable: Epistemic Substitution in Relational Knowledge
 *   domain: philosophy_of_mind/social_psychology/ethics_of_relationship
 *
 * SUMMARY:
 *   Epistemic substitution describes the cognitive phenomenon where
 *   understanding another person's mind is achieved through predictive
 *   modeling of their behavior rather than direct phenomenological access. In
 *   long-term intimate relationships, partners develop highly accurate
 *   behavioral predictions — they can anticipate reactions, preferences,
 *   emotional states — yet this predictive accuracy does not constitute
 *   access to the partner's subjective experience. The substitution is not a
 *   failure of intimacy but a structural feature of intersubjectivity: minds
 *   model other minds through inference, not through direct phenomenological
 *   coupling. This constraint appears invariant across all perspectives,
 *   power levels, time horizons, and spatial scopes. The very low
 *   extractiveness (0.08) reflects that the constraint imposes minimal
 *   asymmetric cost — all agents face the same epistemic boundary. The very
 *   low suppression (0.02) reflects that the constraint is not maintained
 *   through coercion or suppression of alternatives — it emerges from the
 *   computational architecture of theory of mind. The low theater ratio
 *   (0.15) reflects that the constraint is not performative — the
 *   substitution is a genuine cognitive limit, not a social ritual. The high
 *   accessibility collapse (0.92) and low resistance (0.08) reflect that the
 *   constraint is experienced as immutable across all contexts.
 *
 * KEY AGENTS:
 *   - Intimate Partners: Experience the substitution as an unavoidable boundary even in contexts of maximum vulnerability and trust (powerless/trapped)
 *   - Long-Term Relationship Participants: Observe that predictive accuracy improves over biographical time but phenomenological access does not (moderate/constrained)
 *   - Therapeutic Community: Recognize the substitution as a fundamental limit that therapy can work around but not eliminate (organized/mobile)
 *   - Cognitive Science Institution: Model the substitution as a consequence of theory-of-mind architecture (institutional/arbitrage)
 *   - Neuroscience Research Programs: Find that neural transparency provides correlates but not first-person access (powerful/arbitrage)
 *   - Analytical Observer: Identifies the substitution as a hard limit of intersubjectivity independent of social arrangement (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_substitution, 0.08).
domain_priors:suppression_score(epistemic_substitution, 0.02).
domain_priors:theater_ratio(epistemic_substitution, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_substitution, extractiveness, 0.08).
narrative_ontology:constraint_metric(epistemic_substitution, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(epistemic_substitution, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(epistemic_substitution, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(epistemic_substitution, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_substitution, mountain).
narrative_ontology:human_readable(epistemic_substitution, "Epistemic Substitution in Relational Knowledge").
narrative_ontology:topic_domain(epistemic_substitution, "philosophy_of_mind/social_psychology/ethics_of_relationship").

domain_priors:emerges_naturally(epistemic_substitution).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTIMATE PARTNER / IMMEDIATE (MOUNTAIN) — Even from the position of maximum vulnerability and minimum exit, the substitution of prediction for comprehension appears as an unavoidable cognitive limit. The partner cannot force genuine interior access through effort or demand. The gap between behavioral modeling and phenomenological understanding is experienced as a natural boundary of intersubjectivity.
constraint_indexing:constraint_classification(epistemic_substitution, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LONG-TERM PARTICIPANT / BIOGRAPHICAL (MOUNTAIN) — Over biographical time, the constraint remains invariant. Decades of shared history improve predictive accuracy (can anticipate reactions, preferences, habits) but do not dissolve the epistemic boundary. The substitution is not a failure of intimacy but a structural feature of how minds model other minds. Prediction asymptotes; access does not.
constraint_indexing:constraint_classification(epistemic_substitution, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THERAPEUTIC COMMUNITY / GENERATIONAL (MOUNTAIN) — Organized practitioners (therapists, relationship counselors, attachment researchers) across generational time recognize the substitution as a fundamental limit. Therapeutic interventions can improve communication, reduce projection, increase empathy — but cannot eliminate the gap between third-person prediction and first-person access. The constraint is not pathology but ontology.
constraint_indexing:constraint_classification(epistemic_substitution, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: COGNITIVE SCIENCE / CIVILIZATIONAL (MOUNTAIN) — From the institutional perspective of cognitive science and philosophy of mind, epistemic substitution is a consequence of the computational architecture of theory of mind. Simulation theory, theory-theory, and hybrid models all predict that other-mind understanding operates via inference from behavior rather than direct phenomenological access. The constraint is not contingent on culture, technology, or social arrangement.
constraint_indexing:constraint_classification(epistemic_substitution, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / UNIVERSAL (MOUNTAIN) — The analytical position recognizes epistemic substitution as a hard limit of intersubjectivity. No amount of intimacy, communication technology, or neural coupling can grant direct access to another's qualia. Predictive modeling is not a degraded form of understanding but the only form available across the phenomenological boundary. The constraint is universal — it applies to all dyadic relationships regardless of power, culture, or historical period.
constraint_indexing:constraint_classification(epistemic_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: NEUROSCIENCE RESEARCH / GENERATIONAL (MOUNTAIN) — Advanced neuroimaging, brain-computer interfaces, and neural synchrony studies reveal correlates of shared experience but do not dissolve the substitution. Observing another's neural activity provides third-person data, not first-person access. The explanatory gap persists even with complete neural transparency. The constraint is not technological but metaphysical.
constraint_indexing:constraint_classification(epistemic_substitution, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_substitution_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(epistemic_substitution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_substitution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(epistemic_substitution, ExtMetricName, E),
    domain_priors:suppression_score(epistemic_substitution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(epistemic_substitution),
    narrative_ontology:constraint_metric(epistemic_substitution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(epistemic_substitution, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(epistemic_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes a cognitive limit that all agents face symmetrically. There is no beneficiary group that extracts advantage from the substitution and no victim group that bears disproportionate cost. The minimal extractiveness reflects the inherent information-processing cost of maintaining theory-of-mind models — a necessary overhead for social cognition, not an asymmetric extraction mechanism. Suppression (0.02): Very low. The constraint is not maintained through active enforcement, coercion, or suppression of alternatives. It emerges naturally from the computational architecture of how minds model other minds. No institution enforces the epistemic boundary; it is a consequence of the phenomenological gap between first-person and third-person access. Theater ratio (0.15): Low. The constraint is not performative. The substitution of prediction for comprehension is a genuine cognitive operation, not a social ritual. The minimal theater reflects that agents are not pretending to have access they lack — they are using the only access mechanism available (behavioral inference). Accessibility collapse (0.92): Very high. Across all contexts, agents experience the substitution as an immutable limit. Intimacy, communication, time, and technology improve prediction but do not dissolve the epistemic boundary. Resistance (0.08): Very low. The constraint shows minimal variation across power levels, cultural contexts, or historical periods. The substitution is a universal feature of intersubjectivity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap — all six perspectives classify as Mountain. The uniformity is diagnostically significant: it demonstrates that epistemic substitution is not a contingent social arrangement that appears natural from some perspectives but extractive from others. The powerless partner with no exit, the long-term participant with biographical experience, the organized therapeutic community, the institutional cognitive science research program, the neuroscience lab with advanced imaging technology, and the analytical observer all encounter the same immutable boundary. The constraint is a genuine natural law of intersubjectivity, not a naturalized extraction mechanism. The absence of a perspectival gap is the signal — when a constraint classifies identically from all structural positions, it is either a true mountain or a universally internalized snare. The very low extractiveness and suppression distinguish this case as a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims — it is a symmetric cognitive limit that all agents face equally. The directionality values for all perspectives default to the analytical fallback (d ≈ 0.72), but because base extractiveness is very low (0.08), even the analytical perspective experiences minimal effective extraction. The constraint is not an extraction mechanism but a structural feature of how minds access other minds. The mountain classification is uniform across all perspectives because the constraint is invariant to power, exit, time, and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint resolves the mandatrophy by demonstrating that not all cognitive limits are extraction mechanisms in disguise. The substitution of prediction for comprehension is not a failure of intimacy that could be solved with better communication, more vulnerability, or technological enhancement. It is a consequence of the phenomenological gap between first-person experience and third-person observation. The constraint passes all mountain gates: very low extractiveness (no asymmetric cost), very low suppression (emerges naturally without enforcement), high accessibility collapse (experienced as immutable across all contexts), low resistance (minimal variation across power and culture), and emerges naturally (consequence of theory-of-mind architecture). The uniform mountain classification across all perspectives confirms that this is not a false summit — it is a genuine limit of intersubjectivity. The mandatrophy is resolved by recognizing that some constraints are not extractive social arrangements but inherent features of the cognitive and phenomenological architecture within which social arrangements operate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qualia_realism,
    'Does phenomenological experience have irreducible first-person properties, or is the appearance of epistemic substitution an artifact of incomplete information?',
    'Philosophical resolution of the hard problem of consciousness; empirical test would require demonstration that complete third-person neural data either does or does not exhaust first-person phenomenology',
    'If qualia are reducible: epistemic substitution is a temporary information problem (Scaffold). If irreducible: substitution is a permanent metaphysical boundary (Mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qualia_realism, conceptual, 'Whether phenomenological experience has irreducible first-person properties').

omega_variable(
    prediction_accuracy_ceiling,
    'Is there an asymptotic limit to behavioral prediction accuracy in long-term relationships, or does accuracy continue improving indefinitely with exposure?',
    'Longitudinal studies tracking prediction accuracy vs relationship duration; identification of plateau points; comparison across relationship types and cultural contexts',
    'If accuracy plateaus: supports substitution as structural limit. If accuracy continues improving: suggests substitution is a coordination problem solvable through sufficient data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prediction_accuracy_ceiling, empirical, 'Whether behavioral prediction accuracy asymptotes in long-term relationships').

omega_variable(
    surprise_frequency_interpretation,
    'Does persistent surprise in long-term relationships indicate epistemic substitution (prediction without access) or relationship dysfunction (insufficient attention)?',
    'Correlation analysis between surprise frequency, relationship satisfaction, and other intimacy metrics; cross-cultural comparison of surprise norms',
    'If surprise indicates substitution: mountain classification confirmed. If surprise indicates dysfunction: the constraint is relational pathology (Snare or Tangled Rope), not cognitive architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surprise_frequency_interpretation, empirical, 'Whether surprise frequency indicates epistemic limit or relationship quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_substitution, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(episub_tr_t0, epistemic_substitution, theater_ratio, 0, 0.15).
narrative_ontology:measurement(episub_tr_t50, epistemic_substitution, theater_ratio, 50, 0.15).
narrative_ontology:measurement(episub_tr_t100, epistemic_substitution, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(episub_be_t0, epistemic_substitution, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(episub_be_t50, epistemic_substitution, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(episub_be_t100, epistemic_substitution, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_substitution, information_standard).

% DUAL FORMULATION NOTE:
% Epistemic substitution is a foundational constraint in the philosophy of mind and social cognition. It is upstream of many relational and therapeutic constraints but is not decomposable into multiple stories with different epsilon values — the substitution is a single structural phenomenon with a single empirical signature (prediction accuracy improves; phenomenological access does not). No observable-dependent classification exists because the constraint is invariant to measurement methodology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
