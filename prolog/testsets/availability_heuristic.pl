% ============================================================================
% CONSTRAINT STORY: availability_heuristic
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_availability_heuristic, []).

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
 *   constraint_id: availability_heuristic
 *   human_readable: Availability Heuristic (as exploited by information systems)
 *   domain: cognitive/social/economic
 *
 * SUMMARY:
 *   The availability heuristic is a mental shortcut relying on immediate, vivid
 *   examples that come to mind when evaluating a topic. This constraint story
 *   models not the biological heuristic itself (which would be a Mountain with
 *   low ε), but the socio-technical system that actively exploits it. This
 *   system leverages the heuristic to distort risk perception and influence
 *   behavior for commercial or political gain.
 *
 * KEY AGENTS (by structural relationship):
 *   - Anxious Citizen: Primary target (powerless/constrained) — bears extraction in the form of distorted risk perception and reduced autonomy.
 *   - Media & Advertising Executives: Primary beneficiaries (institutional/arbitrage) — benefit by shaping public opinion and consumer behavior.
 *   - Risk Analyst / Cognitive Psychologist: Analytical observer — sees the full structure of the biological trait and its societal exploitation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(availability_heuristic, 0.40).
domain_priors:suppression_score(availability_heuristic, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(availability_heuristic, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(availability_heuristic, extractiveness, 0.40).
narrative_ontology:constraint_metric(availability_heuristic, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(availability_heuristic, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable. This constraint is a human-constructed system of exploitation,
% not a natural law. The underlying biological heuristic would be a Mountain,
% but this story models the system built on top of it.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(availability_heuristic, tangled_rope).
narrative_ontology:human_readable(availability_heuristic, "Availability Heuristic (as exploited by information systems)").
narrative_ontology:topic_domain(availability_heuristic, "cognitive/social/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(availability_heuristic). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(availability_heuristic, news_media_outlets).
narrative_ontology:constraint_beneficiary(availability_heuristic, advertising_agencies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(availability_heuristic, anxious_citizens).
narrative_ontology:constraint_victim(availability_heuristic, public_health_policymakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE ANXIOUS CITIZEN (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + constrained exit → d ≈ 0.90 → f(d) ≈ 1.35 → high χ
constraint_indexing:constraint_classification(availability_heuristic, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ADVERTISING EXECUTIVE (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(availability_heuristic, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function (for
% advertisers) and the asymmetric extraction (from citizens).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(availability_heuristic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(availability_heuristic_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(availability_heuristic, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(availability_heuristic, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget = snare,
    TypeBeneficiary = rope.

test(analytical_claim_consistency) :-
    % Verify the analytical perspective matches the constraint_claim.
    narrative_ontology:constraint_claim(availability_heuristic, ClaimedType),
    constraint_indexing:constraint_classification(availability_heuristic, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

test(tangled_rope_structural_properties) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(availability_heuristic, _),
    narrative_ontology:constraint_victim(availability_heuristic, _),
    domain_priors:requires_active_enforcement(availability_heuristic).

:- end_tests(availability_heuristic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.40): Represents the extraction of rational
 *     accuracy and autonomous decision-making from the target population. It's
 *     not total control, but a significant distortion.
 *   - Suppression (S=0.70): High score reflects how the heuristic, when
 *     exploited, effectively suppresses consideration of statistical base
 *     rates and less vivid (but more probable) data.
 *   - Theater Ratio (TR=0.15): The system is highly functional for its
 *     beneficiaries; there is little performative waste.
 *
 * PERSPECTIVAL GAP:
 *   - The Anxious Citizen (Snare): Experiences the system as a coercive trap.
 *     Vivid media images of rare disasters (e.g., plane crashes) strangle
 *     their ability to make rational risk assessments about common threats
 *     (e.g., driving), extracting their peace of mind and influencing behavior.
 *   - The Advertising Executive (Rope): Views the heuristic as a pure
 *     coordination tool. It's a reliable mechanism to "top-load" a brand into
 *     consumer minds through repetition and vivid imagery, coordinating
 *     attention toward a purchase.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are media and advertising entities who profit from
 *   directing attention and shaping perception. The victims are citizens whose
 *   decision-making is distorted and policymakers who must contend with a
 *   public whose risk perceptions do not match reality. This clear asymmetric
 *   relationship is why the constraint is a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This story avoids mislabeling the constraint by adhering to the
 *   ε-invariance principle. The innate biological heuristic is a Mountain
 *   (ε≈0.1), but the system of its *exploitation* by media has a much higher
 *   extraction (ε=0.40) and requires active enforcement (selective reporting).
 *   By modeling the latter, we correctly identify a Tangled Rope, capturing
 *   both its coordination function for advertisers and its extractive nature
 *   for the public.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
omega_variable(
    omega_availability_heuristic,
    'Is the negative impact of the availability heuristic an inherent biological cost of a useful shortcut, or is it primarily a result of deliberate systemic exploitation by modern information systems?',
    'Comparative analysis of decision errors in information-poor vs. information-saturated societies.',
    'If inherent cost -> closer to Mountain. If systemic exploit -> confirms Tangled Rope/Snare classification.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_availability_heuristic, conceptual, 'Distinguishing inherent biological cost from systemic exploitation in the availability heuristic.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(availability_heuristic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required as base_extractiveness (0.40) is below the
% threshold (0.46) for mandatory lifecycle drift monitoring.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(availability_heuristic, information_standard).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint is one of two stories decomposed from "the availability heuristic".
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - availability_heuristic_biological (ε=0.10, Mountain) - The innate cognitive shortcut.
%   - availability_heuristic (this file) (ε=0.40, Tangled Rope) - The socio-technical system of its exploitation.
%
% narrative_ontology:affects_constraint(availability_heuristic_biological, availability_heuristic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */