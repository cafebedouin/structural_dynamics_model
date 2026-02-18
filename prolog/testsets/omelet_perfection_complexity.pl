% ============================================================================
% CONSTRAINT STORY: omelet_perfection_complexity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_omelet_perfection_complexity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: omelet_perfection_complexity
 *   human_readable: The French Omelet Paradox (Chasing Perfection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the hidden complexity within seemingly simple tasks, using the
 *   classic French omelet as a proxy. The pursuit of "perfection" in a simple task
 *   reveals a deep, complex reality that is suppressed by a "no-brainer" mindset.
 *   The constraint is the socially enforced standard of perfection, which acts as a
 *   coordination mechanism for masters but a snare for novices.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Unaware Layperson: Primary target (powerless/trapped) — trapped by a "no-brainer" mindset, settling for mediocrity without realizing an alternative exists.
 *   - The Obsessive Practitioner: Primary beneficiary (moderate/mobile) — uses the standard of perfection as a coordination device for self-improvement and learning.
 *   - The Master Chef: Institutional beneficiary (institutional/arbitrage) — upholds and benefits from the standard, perceiving it as an unchangeable law of technique.
 *   - Analytical Observer: Sees the full structure, recognizing the standard as a socially constructed rope, not a natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low extraction (0.25). The "obsession" extracts time and energy, but
% the result is "always a really tasty egg dish," a significant improvement over
% the unexamined alternative.
domain_priors:base_extractiveness(omelet_perfection_complexity, 0.25).
% Rationale: Moderate suppression (0.45). The "simple answer" mindset actively
% suppresses the "world-openingly complex" reality until the agent begins to
% question the standard. This high suppression score is why the constraint cannot
% be a Mountain.
domain_priors:suppression_score(omelet_perfection_complexity, 0.45).
% Rationale: Low theater (0.08). The pursuit of the perfect omelet is about
% tangible technique and results, not performative ritual.
domain_priors:theater_ratio(omelet_perfection_complexity, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omelet_perfection_complexity, extractiveness, 0.25).
narrative_ontology:constraint_metric(omelet_perfection_complexity, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(omelet_perfection_complexity, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
% The analytical view reveals this is not a natural law (Mountain) due to high
% suppression, but a coordination device (Rope).
narrative_ontology:constraint_claim(omelet_perfection_complexity, rope).
narrative_ontology:human_readable(omelet_perfection_complexity, "The French Omelet Paradox (Chasing Perfection)").
narrative_ontology:topic_domain(omelet_perfection_complexity, "social/psychological").

% --- Binary flags ---
% Mastery of the curds and roll "requires active enforcement" of technique.
domain_priors:requires_active_enforcement(omelet_perfection_complexity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, culinary_practitioners).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(omelet_perfection_complexity, adherents_of_simplicity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNAWARE LAYPERSON (PRIMARY TARGET)
% Trapped by a "no-brainer" mindset, the layperson is a victim of the standard's
% suppression of alternatives. The high suppression and their trapped status
% lead to a Snare classification, where potential for quality is strangled.
constraint_indexing:constraint_classification(omelet_perfection_complexity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE OBSESSIVE PRACTITIONER (BENEFICIARY)
% For the practitioner, the standard is a Rope—a tool for coordination and
% self-growth. It provides a framework to learn the "vital lesson" that simple
% things can be wonderfully complex.
constraint_indexing:constraint_classification(omelet_perfection_complexity, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE MASTER CHEF (INSTITUTIONAL BENEFICIARY)
% The Chef perceives the standard as a Mountain—an unchangeable law of technique.
% However, the metrics (suppression=0.45) reveal its true nature as a socially
% enforced Rope. The engine correctly classifies it as Rope, exposing a
% "false natural law" perception.
constraint_indexing:constraint_classification(omelet_perfection_complexity, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view confirms the constraint is a Rope. Its high suppression
% score (0.45) disqualifies it from being a Mountain, and its low base
% extractiveness (0.25) prevents it from being a Snare or Tangled Rope from
% this perspective.
constraint_indexing:constraint_classification(omelet_perfection_complexity, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omelet_perfection_complexity_tests).

test(perspectival_gap) :-
    % Verify the gap between the layperson (target) and practitioner (beneficiary).
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypeBeneficiary, context(agent_power(moderate), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(false_natural_law_detection) :-
    % The Chef's institutional perspective classifies as Rope, not Mountain,
    % due to the high suppression score, revealing a perception-reality gap.
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypeInstitutional == rope.

:- end_tests(omelet_perfection_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this constraint was a 'mountain'. This
 *   created a linter conflict because the suppression score of 0.45 far exceeds
 *   the mountain ceiling of 0.05. A mountain is a natural limit with zero-to-no
 *   suppression. A culinary standard, however perfect, is a social construct that
 *   requires active suppression of alternatives (like scrambled eggs being called
 *   an omelet). The claim has been corrected to 'rope' to reflect the analytical
 *   view that this is a coordination device.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: the unaware layperson is trapped in a Snare of mediocrity,
 *   believing the task is a "no-brainer." In contrast, the practitioner and chef
 *   use the same standard as a Rope for coordination, learning, and mastery. The
 *   constraint's nature changes based on the agent's engagement and awareness.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'culinary_practitioners' benefit from the standard as a clear
 *     goal for technique and a path to "world-opening" insight.
 *   - Victim: 'adherents_of_simplicity' are victimized by the suppression inherent
 *     in the standard. They are denied the discovery of complexity because the
 *     "simple answer" is presented as sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the standard of perfection not as a
 *   natural law (Mountain) but as a social technology (Rope). Misclassifying it
 *   as a Mountain would hide the suppression (0.45) and active enforcement
 *   required to maintain it, creating a "false natural law" that obscures its
 *   socially constructed nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_omelet_perfection_complexity,
    "Is 'perfection' an objective Mountain of culinary physics or a subjective Rope of narrative storytelling?",
    "Comparison of 'perfect' omelets across divergent cultures and chemical analyses of curds.",
    "If Mountain: Technique is a law. If Rope: Technique is a ritual.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(omelet_perfection_complexity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is below the 0.46 threshold for required temporal data.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(omelet_perfection_complexity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */