% ============================================================================
% CONSTRAINT STORY: elencher_identity_transformation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_elencher_identity_transformation, []).

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
 *   constraint_id: elencher_identity_transformation
 *   human_readable: Zetetic Elencher Radical Identity Flux
 *   domain: technological/social/philosophical
 *
 * SUMMARY:
 *   The Zetetic Elench, a splinter group from Iain M. Banks' Culture, define their identity
 *   through a commitment to radical self-transformation. They seek out the undiscovered
 *   not to change it, but to be changed by it, ensuring they are never the same entity
 *   twice. This philosophical commitment functions as a constraint on identity continuity.
 *
 * KEY AGENTS (by structural relationship):
 *   - The previous self (continuity_of_self): Primary target (powerless/trapped) — bears the "extraction" of being rendered obsolete.
 *   - The Zetetic Elench: Primary beneficiary (institutional/mobile) — benefits by achieving what they believe is a higher, "pan-relevant" truth.
 *   - The Culture Citizen: Secondary observer (powerless/constrained) — experiences the Elencher as a socially illegible Snare.
 *   - The Zetetic Analyst: Analytical observer — sees the process as a Mountain, an objective requirement for acquiring universal knowledge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The Elench seek to be changed *by* the other, not to change the other.
% Extraction is internal (self-obsolescence) rather than predatory.
domain_priors:base_extractiveness(elencher_identity_transformation, 0.10).

% Rationale: The alternative (Culture's stable identity) is fully available and known.
% The "shunting" of this alternative is an internal choice, not an external coercion.
% Suppression is therefore minimal, consistent with a natural law.
domain_priors:suppression_score(elencher_identity_transformation, 0.05).

% Rationale: The constraint is a core philosophical vocation, not a performative act.
domain_priors:theater_ratio(elencher_identity_transformation, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elencher_identity_transformation, extractiveness, 0.10).
narrative_ontology:constraint_metric(elencher_identity_transformation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(elencher_identity_transformation, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
% Rationale: The Elench philosophy posits that being changed by the unknown is the *only*
% way to achieve pan-relevant truth, foreclosing stable identity as a path.
narrative_ontology:constraint_metric(elencher_identity_transformation, accessibility_collapse, 0.95).
% Rationale: As a vocational choice, there is no active resistance from its adherents.
narrative_ontology:constraint_metric(elencher_identity_transformation, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(elencher_identity_transformation, mountain).
narrative_ontology:human_readable(elencher_identity_transformation, "Zetetic Elencher Radical Identity Flux").
narrative_ontology:topic_domain(elencher_identity_transformation, "technological/social/philosophical").

% --- Emergence flag (required for mountain constraints) ---
% The constraint emerges naturally as a "calling" or "vocation" for the splinter group.
domain_priors:emerges_naturally(elencher_identity_transformation).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(elencher_identity_transformation, zetetic_elench).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(elencher_identity_transformation, continuity_of_self).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE CULTURE CITIZEN (SNARE)
% For an outsider from a stable society, the Elencher's flux is a Snare. It
% strangles the possibility of consistent relationship or recognition; you can
% "never encounter the same entity twice," making the Elencher socially illegible.
constraint_indexing:constraint_classification(elencher_identity_transformation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ELENCHER SHIP (ROPE)
% For the Elencher entity, this flux is a Rope—a functional coordination
% mechanism for their "mission". It is the tool they use to reach a higher
% truth that stable societies cannot access.
constraint_indexing:constraint_classification(elencher_identity_transformation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ZETETIC ANALYST (MOUNTAIN)
% From an analytical perspective, the necessity of change to reach truth is a
% Mountain. It is an objective requirement; to be changed by the
% undiscovered is the only way to reach a truth that is not "monosophical".
constraint_indexing:constraint_classification(elencher_identity_transformation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elencher_identity_transformation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(elencher_identity_transformation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elencher_identity_transformation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(elencher_identity_transformation, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_adherence) :-
    % Demonstrates low extractiveness and suppression consistent with Mountain classification.
    narrative_ontology:constraint_metric(elencher_identity_transformation, extractiveness, E),
    narrative_ontology:constraint_metric(elencher_identity_transformation, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(elencher_identity_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core of this constraint is the philosophical choice to prioritize informational
 *   fidelity over identity continuity. The metrics reflect this: extraction (0.10) is
 *   low because it's self-directed obsolescence, not predation. Suppression (0.05) is
 *   minimal because the alternative (stable Culture identity) is fully available; the
 *   choice to reject it is internal to the Elench. The analytical perspective sees this
 *   as a Mountain because, within the Elench framework, this is the *only* path to
 *   "pan-relevant truth," making it a law of their epistemology.
 *
 * PERSPECTIVAL GAP:
 *   - The Elencher (Beneficiary) sees a Rope: a tool for their mission of truth-seeking.
 *   - A Culture Citizen (Observer/Victim of illegibility) sees a Snare: an incomprehensible
 *     and relationally destructive behavior.
 *   - The Analyst sees a Mountain: an objective, unavoidable principle for achieving a
 *     certain kind of knowledge. The gap arises from whether one accepts the premise of
 *     the Elench's mission.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the Zetetic Elench, who gain "pan-relevant truth." The victim is
 *   the abstract concept of `continuity_of_self`, representing the previous identity that
 *   is "destroyed" or superseded by the transformation. This internal directionality
 *   is key to the low extraction score.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies that what appears as a destructive Snare to an
 *   outsider is a functional Rope to the practitioner and a fundamental Mountain to the
 *   theoretician. By separating these perspectives, the framework avoids mislabeling a
 *   philosophical vocation as pure social destruction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variable 1: Is the goal real?
omega_variable(
    omega_elencher_truth,
    "Does 'pan-relevant truth' exist as an objective Mountain, or is it a subjective Rope manufactured by the Elench process?",
    "Comparison of Elencher data across multiple divergent civilization encounters.",
    "If Mountain: The Elench mission is valid. If Rope: It is a circular Snare of identity loss.",
    confidence_without_resolution(medium)
).
narrative_ontology:omega_variable(omega_elencher_truth, conceptual, "Whether 'pan-relevant truth' is an objective reality or a subjective goal.").

% Omega variable 2: Does the self persist?
omega_variable(
    omega_elencher_persistence,
    "Can a 'self' be encounterable if it is never the same entity twice?",
    "Verification of core consciousness continuity despite technological/informational changes.",
    "If yes: It is a functional Rope. If no: It is a psychological Mountain of extinction.",
    confidence_without_resolution(low)
).
narrative_ontology:omega_variable(omega_elencher_persistence, empirical, "Whether a core self persists through the radical transformations.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(elencher_identity_transformation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.10) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No clear coordination type or network relationships defined in the source material.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The structural derivation from beneficiary/victim is sufficient. No overrides needed.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */