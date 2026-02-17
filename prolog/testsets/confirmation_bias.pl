% ============================================================================
% CONSTRAINT STORY: confirmation_bias
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_confirmation_bias, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: confirmation_bias
 *   human_readable: Confirmation Bias (Socially Amplified)
 *   domain: social/cognitive/technological
 *
 * SUMMARY:
 *   Confirmation bias is the tendency to search for, interpret, favor, and recall
 *   information in a way that confirms one's prior beliefs. While having a
 *   biological basis, this story models its socially and algorithmically
 *   amplified form, where it functions as a powerful mechanism for social
 *   control and epistemic closure, extracting objectivity and entrenching polarization.
 *
 * KEY AGENTS (by structural relationship):
 *   - Polarized Citizen: Primary target (powerless/trapped) — bears the cost of epistemic closure and inability to process objective reality.
 *   - Ideological Leaders: Primary beneficiary (institutional/arbitrage) — benefit from a predictable and cohesive in-group that is resistant to dissent.
 *   - Social Media Platforms: Secondary beneficiary (institutional/arbitrage) — benefit from engagement driven by reinforcing user beliefs.
 *   - Analytical Observer: Analytical observer — sees the full structure, including the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(confirmation_bias, 0.48).
domain_priors:suppression_score(confirmation_bias, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(confirmation_bias, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(confirmation_bias, extractiveness, 0.48).
narrative_ontology:constraint_metric(confirmation_bias, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(confirmation_bias, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(confirmation_bias, tangled_rope).
narrative_ontology:human_readable(confirmation_bias, "Confirmation Bias (Socially Amplified)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(confirmation_bias). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(confirmation_bias, ideological_leaders).
narrative_ontology:constraint_beneficiary(confirmation_bias, social_media_platforms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(confirmation_bias, polarized_citizens).
narrative_ontology:constraint_victim(confirmation_bias, minority_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE POLARIZED CITIZEN (SNARE)
% Agent trapped within an information ecosystem that reinforces their biases.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.48 * 1.42 * 1.0 ≈ 0.68, which is >= 0.66 (Snare threshold).
constraint_indexing:constraint_classification(confirmation_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE IDEOLOGICAL LEADER (ROPE)
% Agent who benefits from the coordination function of shared belief.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(confirmation_bias, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function (Rope) and the asymmetric extraction (Snare).
% The high suppression and extraction, combined with a clear beneficiary, define a Tangled Rope.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(confirmation_bias_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and analyst.
    constraint_indexing:constraint_classification(confirmation_bias, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeAnalyst == tangled_rope.

test(tangled_rope_structural_properties) :-
    % Verify the structural requirements for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(confirmation_bias),
    narrative_ontology:constraint_beneficiary(confirmation_bias, _),
    narrative_ontology:constraint_victim(confirmation_bias, _).

test(snare_threshold_validation) :-
    % Verify the metrics support a Snare classification from the target's perspective.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(snare_epsilon_floor, SnareEpsilonFloor),
    narrative_ontology:constraint_metric(confirmation_bias, ExtMetricName, E),
    E >= SnareEpsilonFloor.

:- end_tests(confirmation_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This model departs from viewing confirmation bias as a low-extraction,
 *   biological 'Mountain'. Instead, it models the socially and algorithmically
 *   amplified version, which is highly extractive.
 *   - Base Extractiveness (ε=0.48): Set just above the Snare floor (0.46) to
 *     reflect the high cost of epistemic closure. What is extracted is not
 *     money, but cognitive agency and access to objective reality.
 *   - Suppression (0.80): The bias is defined by its function of suppressing
 *     contradictory information, making alternatives invisible or devalued.
 *   - Active Enforcement: This is met through social pressure (in-group policing)
 *     and algorithmic filtering, which actively curate a biased information diet.
 *
 * PERSPECTIVAL GAP:
 *   - The Polarized Citizen (Snare): They are trapped in an echo chamber. Their
 *     ability to think independently is constrained, and the cost is high.
 *   - The Ideological Leader (Rope): They leverage the bias as a pure coordination
 *     tool to build social cohesion and a loyal base, experiencing no extraction.
 *   - The Analyst (Tangled Rope): They see the complete picture: a system that
 *     provides genuine coordination for one group (the 'Rope' aspect) by
 *     asymmetrically extracting cognitive freedom from another (the 'Snare' aspect).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two major errors. It is
 *   not a 'Mountain' of pure biology, because its social impact is constructed
 *   and enforced. It is not a pure 'Snare', because it serves a genuine (if
 *   problematic) coordination function for the in-group. The Tangled Rope
 *   classification acknowledges this duality, which is critical for analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_confirmation_bias,
    "To what extent is modern polarization caused by innate biological bias vs. contingent algorithmic amplification?",
    "Comparative study of polarized groups with and without digital social media exposure.",
    "If primarily biological, the constraint is closer to a Mountain and requires education. If primarily algorithmic, it is a Tangled Rope/Snare that can be mitigated by regulation.",
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_confirmation_bias, empirical, 'Disentangling biological predisposition from algorithmic amplification in modern polarization.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(confirmation_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the bias from a biological
% baseline to an algorithmically amplified state. Required as ε > 0.46.

% Theater ratio over time (remains low as the function is genuine):
narrative_ontology:measurement(confirmation_bias_tr_t0, confirmation_bias, theater_ratio, 0, 0.10).
narrative_ontology:measurement(confirmation_bias_tr_t5, confirmation_bias, theater_ratio, 5, 0.10).
narrative_ontology:measurement(confirmation_bias_tr_t10, confirmation_bias, theater_ratio, 10, 0.10).

% Extraction over time (increases with communication technology):
narrative_ontology:measurement(confirmation_bias_ex_t0, confirmation_bias, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(confirmation_bias_ex_t5, confirmation_bias, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(confirmation_bias_ex_t10, confirmation_bias, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: Information standard (enforces a shared reality).
narrative_ontology:coordination_type(confirmation_bias, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */