% ============================================================================
% CONSTRAINT STORY: social_narrative_casting
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_social_narrative_casting, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_narrative_casting
 *   human_readable: Social Narrative Casting (Criticism-as-Projection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the act of criticism as an attempt by a critic (the "Director")
 *   to "hire" the subject into a specific role (villain, victim, obstacle) within the
 *   critic's internal narrative. The subject must choose whether to internalize this
 *   casting (a Snare) or consciously play the role to learn from it (a Rope). The
 *   underlying mechanism is the ego's tendency to reduce complex reality into simple,
 *   self-serving stories.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Subject (Actor): Primary target (powerless/trapped) — bears the extraction of having their identity constrained by another's narrative.
 *   - The Critic (Director): Primary beneficiary (organized/mobile) — benefits by reinforcing their ego and worldview through the narrative casting of others.
 *   - The Therapist/Coach: Secondary beneficiary (institutional/arbitrage) — uses the understanding of this dynamic as a tool to help others.
 *   - The Analytical Observer: Sees the full structure of ego-driven narrative projection as a system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_narrative_casting, 0.30).
domain_priors:suppression_score(social_narrative_casting, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(social_narrative_casting, 0.06).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_narrative_casting, extractiveness, 0.30).
narrative_ontology:constraint_metric(social_narrative_casting, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(social_narrative_casting, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain candidates) ---
% This constraint EMERGES NATURALLY from ego psychology, making it a mountain
% candidate. However, its metrics (high suppression, alternatives exist) mean it
% FAILS the natural law certification. These values ensure the `false_natural_law`
% signature fires correctly.
narrative_ontology:constraint_metric(social_narrative_casting, accessibility_collapse, 0.40). % Fails NL gate (>= 0.85). Alternatives (conscious awareness) are accessible.
narrative_ontology:constraint_metric(social_narrative_casting, resistance, 0.70).             % Fails NL gate (<= 0.15). Resistance (therapy, self-awareness) is common.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(social_narrative_casting, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(social_narrative_casting). % The critic must actively enforce the narrative.

% --- Emergence flag (required for mountain candidates) ---
% This constraint emerges from the ego's need to project itself and create
% meaning through stories. This makes it a mountain candidate, triggering the
% NL profile metric checks above.
domain_priors:emerges_naturally(social_narrative_casting).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(social_narrative_casting, critics_ego).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(social_narrative_casting, criticized_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE CRITICIZED SUBJECT (Unaware Actor)
% For a subject who internalizes the criticism, it is a Snare. The critic's
% narrative constrains their identity, leaving them trapped in someone else's movie.
constraint_indexing:constraint_classification(social_narrative_casting, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (The Critic/Director)
% For the critic, casting others into roles is a coordination mechanism (Rope) for
% managing their internal world and reinforcing their ego. The extraction is externalized.
constraint_indexing:constraint_classification(social_narrative_casting, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees the full system: a coordination function for the critic's ego
% (beneficiary) layered on top of asymmetric extraction from the subject (victim),
% requiring active enforcement. This is a classic Tangled Rope. It appears like a
% Mountain (natural law of the ego) but its high suppression and available
% alternatives reveal its constructed, extractive nature.
constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE THERAPIST / COACH
% For a therapist, understanding this dynamic is a powerful diagnostic and
% therapeutic tool (Rope). They use the concept as a pure coordination mechanism
% to help clients build resilience and social awareness.
constraint_indexing:constraint_classification(social_narrative_casting, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_narrative_casting_tests).

test(perspectival_gap) :-
    % Verify the gap between the subject who internalizes the role (Snare)
    % and the therapist who uses the concept as a tool (Rope).
    constraint_indexing:constraint_classification(social_narrative_casting, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_narrative_casting, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(social_narrative_casting, tangled_rope, context(agent_power(analytical), _, _, _)).

test(power_based_extractiveness_scaling) :-
    % Demonstrates that the powerless, trapped subject experiences higher effective
    % extraction than the institutional agent who can arbitrage the situation.
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextInstitutional = context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(regional)),
    constraint_indexing:extractiveness_for_agent(social_narrative_casting, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(social_narrative_casting, ContextInstitutional, Score2),
    Score1 > Score2.

:- end_tests(social_narrative_casting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - ε=0.30: The extraction is moderate because the subject retains ultimate agency to refuse belief in the cast role.
 *   - Suppression=0.60: High, as the critic's narrative actively suppresses the subject's complex reality in favor of a simple, useful illusion.
 *   - `emerges_naturally`: The constraint arises from fundamental ego psychology, making it appear like a natural law.
 *   - NL Profile Metrics: The `accessibility_collapse` (0.40) and `resistance` (0.70) scores are set to fail the mountain certification thresholds. This is intentional. It models a "false natural law" — a constraint that seems immutable but is actually a constructed social dynamic with high suppression and available exits (e.g., therapy, self-awareness).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: the subject who internalizes the criticism sees a Snare (their identity is trapped). The critic who projects it sees a Rope (a tool to manage their worldview). The therapist who analyzes it also sees a Rope (a diagnostic tool). The analytical observer sees the complete picture: a Tangled Rope that combines a coordination function for one party with extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `critics_ego`. The critic benefits by having their internal narrative validated and simplified, reducing cognitive dissonance.
 *   - Victim: `criticized_subject`. The subject bears the cost of having their identity and lived experience reduced to a simplistic role in someone else's story.
 *   This clear beneficiary/victim structure drives the directionality calculation, leading to high effective extraction (χ) for the victim and low/negative χ for the beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This model correctly identifies the constraint as a Tangled Rope from the analytical view, preventing the misclassification of this psychological mechanism as either a pure Mountain (an unchangeable law of nature) or a pure Snare (ignoring the coordination function it serves for the critic's ego). The `emerges_naturally` flag combined with the failing NL profile metrics is key to detecting this "false natural law" structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_criticism_objectivity,
    "Can criticism ever reflect an objective truth about the subject, or is it always a projection of the critic's internal narrative?",
    "Cross-cultural studies of criticism reception and longitudinal tracking of projected vs. observed behavioral patterns.",
    "If objective: Criticism is a Rope for self-improvement. If always projection: It remains a Snare until consciously reframed.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_criticism_objectivity, empirical, "Is criticism always projection, or can it be objective feedback?").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(social_narrative_casting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is 0.30, which is below the 0.46 threshold for
% mandatory temporal data. No measurements are required for this constraint.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or Network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed. The structural derivation from
% beneficiary/victim declarations accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */