% ============================================================================
% CONSTRAINT STORY: hawthorne_effect
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_hawthorne_effect, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hawthorne_effect
 *   human_readable: The Hawthorne Effect (Observer Effect)
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Hawthorne Effect is a type of reactivity in which individuals modify
 *   their behavior in response to their awareness of being observed. While it
 *   resembles a natural law of psychology, its application in organizational
 *   and economic contexts functions as a tool for managing productivity. This
 *   constraint story models the *application* of the effect as a managerial
 *   tool, which has both a coordination function (gathering data, signaling
 *   priorities) and an extractive one (increasing output through pressure).
 *
 * KEY AGENTS (by structural relationship):
 *   - Monitored Worker: Primary target (powerless/trapped) — bears the psychological cost and pressure to perform, losing autonomy and authentic behavior.
 *   - Factory Manager/Researcher: Primary beneficiary (institutional/arbitrage) — uses observation as a low-cost tool to coordinate activity and increase output.
 *   - Analytical Observer: Sees the full structure as a hybrid coordination/extraction mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: High. The effect extracts "authentic behavior" and additional
% labor/effort through the psychological pressure of the gaze. The value is
% set high enough to trigger a Snare classification for the trapped agent.
domain_priors:base_extractiveness(hawthorne_effect, 0.60).

% Rationale: High. It suppresses the "natural state." Once an observer is
% present, the alternative—how the worker behaves when alone—is rendered
% inaccessible.
domain_priors:suppression_score(hawthorne_effect, 0.65).

% Rationale: Low. The effect is real and substantive, not performative.
domain_priors:theater_ratio(hawthorne_effect, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hawthorne_effect, extractiveness, 0.60).
narrative_ontology:constraint_metric(hawthorne_effect, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hawthorne_effect, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
% This constraint is not a Mountain; it is a human application of a
% psychological tendency. The 'emerges_naturally' flag is not set.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hawthorne_effect, tangled_rope).
narrative_ontology:human_readable(hawthorne_effect, "The Hawthorne Effect (Observer Effect)").
narrative_ontology:topic_domain(hawthorne_effect, "social/economic").

% --- Binary flags ---
% The "gaze" of the observer or the knowledge of a study constitutes a form
% of active, albeit soft, enforcement. Required for Tangled Rope.
domain_priors:requires_active_enforcement(hawthorne_effect).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hawthorne_effect, management).
narrative_ontology:constraint_beneficiary(hawthorne_effect, study_leads).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hawthorne_effect, monitored_workers).
narrative_ontology:constraint_victim(hawthorne_effect, experimental_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE MONITORED WORKER (SNARE)
% Agent who bears the most extraction. The awareness of being watched creates
% an involuntary "tightening" of performance. They cannot relax into their
% natural rhythm; the gaze extracts extra vitality and focus, strangling their
% autonomy and mental peace until the observer leaves.
constraint_indexing:constraint_classification(hawthorne_effect, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CORPORATE SUPERVISOR (ROPE)
% For management, the effect is a Rope. It is a coordination mechanism used
% to "nudge" behavior. By simply placing a supervisor on the floor or
% announcing a "study," they pull the workforce toward higher productivity
% without needing to change wages or hardware.
constraint_indexing:constraint_classification(hawthorne_effect, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% From a systemic view, the effect is a Tangled Rope. It has a genuine
% coordination function (aligning effort, gathering data) but also imposes
% a significant, asymmetric extractive cost on the observed subjects. It
% requires the active enforcement of the observer's presence to function.
constraint_indexing:constraint_classification(hawthorne_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hawthorne_effect_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and analyst.
    constraint_indexing:constraint_classification(hawthorne_effect, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hawthorne_effect, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hawthorne_effect, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that the structural data needed for a Tangled Rope classification exists.
    narrative_ontology:constraint_beneficiary(hawthorne_effect, _),
    narrative_ontology:constraint_victim(hawthorne_effect, _),
    domain_priors:requires_active_enforcement(hawthorne_effect).

:- end_tests(hawthorne_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.60) and suppression (0.65) were chosen to be
 *   high enough to meet the metric thresholds for a Snare (ε ≥ 0.46, supp ≥ 0.60)
 *   and Tangled Rope (ε ≥ 0.30, supp ≥ 0.40). This allows the perspectival
 *   classifications to be metrically sound. The original file's attempt to
 *   classify this as a Mountain for the analyst violated the ε-invariance
 *   principle; the application of a psychological law is not the same as the
 *   law itself. This version models the application, which is a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - The Worker (powerless/trapped) experiences the constraint as a Snare. The
 *     high directionality (d≈0.95) amplifies the base extraction, capturing
 *     them in a state of performance anxiety with no exit.
 *   - Management (institutional/arbitrage) experiences it as a Rope. Their
 *     beneficiary status and exit options give them negative directionality
 *     (d≈0.05), making the effective extraction negative. For them, it is a
 *     pure coordination tool.
 *   - The Analyst sees both sides: a genuine coordination function for the
 *     beneficiary and a clear extractive cost for the victim, enforced by the
 *     observer's presence. This hybrid nature is the definition of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are 'management' and 'study_leads' who gain productivity or
 *   data. Victims are 'monitored_workers' who lose autonomy and psychological
 *   comfort, and 'experimental_integrity' which is compromised by the effect.
 *   These declarations drive the directionality `d`, creating the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the effect as a Tangled Rope from the analytical view prevents
 *   mislabeling it as either pure coordination (Rope) or pure extraction (Snare).
 *   It correctly identifies that the mechanism has a dual function, which is
 *   key to understanding its persistence in organizational management.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_hawthorne_effect,
    "How long does it take for a subject to 'habituate' and return to natural behavior, effectively reducing the constraint's extraction?",
    "Longitudinal study of 24/7 surveillance vs. intermittent observation.",
    "If habituation is fast, the effect is a weak Tangled Rope. If slow or non-existent, it is a persistent Snare for the subject.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_hawthorne_effect, empirical, "Uncertainty over the long-term habituation to observation.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hawthorne_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46 requires temporal data. The Hawthorne effect has been
% a stable feature of social systems since its identification, so the metrics
% are modeled as consistently high over the interval.
%
% Theater ratio over time:
narrative_ontology:measurement(hawthorne_effect_tr_t0, hawthorne_effect, theater_ratio, 0, 0.19).
narrative_ontology:measurement(hawthorne_effect_tr_t5, hawthorne_effect, theater_ratio, 5, 0.19).
narrative_ontology:measurement(hawthorne_effect_tr_t10, hawthorne_effect, theater_ratio, 10, 0.19).

% Extraction over time:
narrative_ontology:measurement(hawthorne_effect_ex_t0, hawthorne_effect, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(hawthorne_effect_ex_t5, hawthorne_effect, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(hawthorne_effect_ex_t10, hawthorne_effect, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The application of the effect is an enforcement mechanism to align behavior.
narrative_ontology:coordination_type(hawthorne_effect, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the directionality for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */