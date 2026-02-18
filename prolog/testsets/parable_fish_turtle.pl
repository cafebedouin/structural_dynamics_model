% ============================================================================
% CONSTRAINT STORY: parable_fish_turtle
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_parable_fish_turtle, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: parable_fish_turtle
 * human_readable: The Ontological Lake (Fish and Turtle Parable)
 * domain: philosophical/social
 * * SUMMARY:
 * This constraint models the limits of understanding based on lived experience, as illustrated by the parable of the fish and the turtle. A fish, trapped in an aquatic ontology, cannot conceptualize "dry land" because it lacks the only attributes the fish knows (wetness, buoyancy). The turtle, who can traverse both domains, fails to communicate its experience, revealing a fundamental constraint on knowledge transfer.
 * * KEY AGENTS:
 * - The Fish: Subject (Powerless), trapped in its experiential domain.
 * - The Educator/Institution: Beneficiary (Institutional), uses the parable as a tool for teaching.
 * - The Systems Analyst: Auditor (Analytical), observes the structure of the cognitive trap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(parable_fish_turtle, 0.70). % The aquatic ontology extracts the totality of terrestrial reality from the fish's universe.
domain_priors:suppression_score(parable_fish_turtle, 0.85).   % The experience of being "wet" completely suppresses the possibility of a "dry" reality.
domain_priors:theater_ratio(parable_fish_turtle, 0.10).       % The parable is a functional teaching tool, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(parable_fish_turtle, extractiveness, 0.70).
narrative_ontology:constraint_metric(parable_fish_turtle, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(parable_fish_turtle, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The parable presents itself as a constructed lesson about natural limits.
narrative_ontology:constraint_claim(parable_fish_turtle, tangled_rope).
narrative_ontology:human_readable(parable_fish_turtle, "The Ontological Lake (Fish and Turtle Parable)").
narrative_ontology:topic_domain(parable_fish_turtle, "philosophical/social").

% Binary flags
% The "rules" of the fish's world are enforced by physics (it cannot breathe air).
domain_priors:requires_active_enforcement(parable_fish_turtle).

% Structural property derivation hooks:
% The Educator benefits from the parable's existence as a teaching tool.
narrative_ontology:constraint_beneficiary(parable_fish_turtle, educators).
% The Fish is a victim of the cognitive trap the parable describes.
narrative_ontology:constraint_victim(parable_fish_turtle, ontologically_trapped_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The fish is trapped by its own perceptual framework.
constraint_indexing:constraint_classification(parable_fish_turtle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The educator uses the parable as a pure coordination tool for teaching.
constraint_indexing:constraint_classification(parable_fish_turtle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the pedagogical function (coordination) and the
% severe cognitive extraction imposed on the subject.
constraint_indexing:constraint_classification(parable_fish_turtle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE TURTLE (ROPE)
% The turtle, able to move between worlds, experiences the boundary as a simple
% coordination problem, not a trap.
constraint_indexing:constraint_classification(parable_fish_turtle, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parable_fish_turtle_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the fish (powerless) and the educator (institutional).
    constraint_indexing:constraint_classification(parable_fish_turtle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parable_fish_turtle, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer should see the full picture.
    constraint_indexing:constraint_classification(parable_fish_turtle, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(parable_fish_turtle, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(parable_fish_turtle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.70) is high because the fish's ontology completely
 * extracts the possibility of another form of existence. The suppression (0.85)
 * is also high, as the physical reality of water prevents any alternative experience.
 *
 * The Perspectival Gap is stark:
 * - For the Fish (powerless, trapped), this is a Snare. Its own biology and
 *   conceptual framework form a trap that liquidates external truths.
 * - For the Educator (institutional, mobile), the parable itself is a Rope. It's a
 *   perfect, low-cost tool to coordinate a shared understanding of cognitive limits.
 * - The Analytical observer sees both sides. The parable has a valid coordination
 *   function (teaching) but is built upon a scenario of asymmetric extraction
 *   (the fish's reality being invalidated). This duality, combined with the
 *   physical enforcement (the fish can't breathe air), makes it a canonical
 *   Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A naive analysis might label the fish's situation a Mountain (an immutable
 * law of biology). However, the Tangled Rope classification is superior because
 * it captures the social/pedagogical layer built on top of this physical limit.
 * The constraint is not just the physical limit, but the *parable about the limit*,
 * which has both beneficiaries (educators) and victims (the fish as a symbol
 * for the intellectually trapped). This prevents misclassifying a constructed
 * teaching tool as a pure natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fish_turtle,
    'Is the fishs cognitive limit a permanent biological Mountain, or a solvable conceptual Rope that could be untangled with a different communication protocol?',
    'Comparative neurobiology of sensory integration and abstract thought in species with radically different environments.',
    'If Mountain, the parable describes a hard limit on empathy/understanding. If Rope, it describes a communication failure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(parable_fish_turtle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a perennial parable, so its core metrics are stable over time.
% The lifecycle data reflects this stability, showing no drift.
%
% Theater ratio over time:
narrative_ontology:measurement(parable_fish_turtle_tr_t0, parable_fish_turtle, theater_ratio, 0, 0.10).
narrative_ontology:measurement(parable_fish_turtle_tr_t5, parable_fish_turtle, theater_ratio, 5, 0.10).
narrative_ontology:measurement(parable_fish_turtle_tr_t10, parable_fish_turtle, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(parable_fish_turtle_ex_t0, parable_fish_turtle, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(parable_fish_turtle_ex_t5, parable_fish_turtle, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(parable_fish_turtle_ex_t10, parable_fish_turtle, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The fish's internal, experience-based worldview acts as an information standard.
% Any new data (from the turtle) is validated against this standard and rejected.
narrative_ontology:coordination_type(parable_fish_turtle, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */