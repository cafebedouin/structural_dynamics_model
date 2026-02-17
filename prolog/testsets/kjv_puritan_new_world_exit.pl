% ============================================================================
% CONSTRAINT STORY: kjv_puritan_new_world_exit
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_kjv_puritan_new_world_exit, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kjv_puritan_new_world_exit
 *   human_readable: The Puritan Textual Re-Indexing (KJV in the New World)
 *   domain: political/religious
 *
 * SUMMARY:
 *   This story tracks the movement of the King James Version (KJV) of the Bible
 *   across a geographic "Exit Option." In England, the KJV was a tool of the
 *   Crown used to suppress Puritan identity (perceived as a Snare). Upon arrival
 *   in the New World, the Puritans—now holding institutional power—adopted the
 *   KJV as a functional coordination tool (Rope) to maintain social order and
 *   literacy in a "wilderness" environment. For subsequent generations born in
 *   the colonies, it became an immutable feature of reality (perceived as a Mountain).
 *
 * KEY AGENTS (by structural relationship):
 *   - English Puritan Dissenters: Primary target (in the original English context) (moderate/trapped) — bears suppression.
 *   - New England Magistrates: Primary beneficiary (in the New World context) (institutional/mobile) — benefits from the text as a coordination tool.
 *   - Second-Generation Colonists: Inheritor of the system (powerless/trapped) — experiences the text as a fixed reality.
 *   - Analytical Observer: Sees the full re-indexing dynamic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics (reflecting the New World context) ---
domain_priors:base_extractiveness(kjv_puritan_new_world_exit, 0.20).
domain_priors:suppression_score(kjv_puritan_new_world_exit, 0.50).
domain_priors:theater_ratio(kjv_puritan_new_world_exit, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, extractiveness, 0.20).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain perspectives) ---
% These metrics describe the *perception* of the constraint by the second
% generation, for whom it was an immutable social fact.
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, accessibility_collapse, 0.90).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(kjv_puritan_new_world_exit, rope).
narrative_ontology:human_readable(kjv_puritan_new_world_exit, "The Puritan Textual Re-Indexing (KJV in the New World)").

% --- Binary flags ---
% The social contract around the KJV required active teaching, preaching, and
% social reinforcement. This resolves the SCAFFOLD_DANGER_ZONE lint error.
domain_priors:requires_active_enforcement(kjv_puritan_new_world_exit).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(kjv_puritan_new_world_exit, new_england_magistrates).
%
% Who bears disproportionate cost? (In the original context)
narrative_ontology:constraint_victim(kjv_puritan_new_world_exit, english_puritan_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENGLISH PURITAN (STAYED BEHIND)
% Agent who experiences the KJV as a tool of state suppression in England.
% NOTE: The 'snare' classification reflects the agent's experience in a
% different context (England), while the base metrics (ε=0.2) reflect the
% constraint's re-indexed function in the New World. This creates a logical
% tension that highlights the core theme of re-indexing.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE NEW ENGLAND MAGISTRATE (PRIMARY BENEFICIARY)
% Agent who uses the KJV as a coordination tool in the new colony.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE BORN-COLONIST (SECOND GENERATION)
% Agent born into the system, for whom the KJV is an unchangeable social fact.
% The high suppression score (0.5) prevents a true Mountain classification by
% the engine, but the *perceptual* classification is Mountain due to the lack
% of experienced alternatives.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Default analytical context, which sees the low-extraction coordination function.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_puritan_new_world_exit_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the inheritor and the architect.
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, TypeInheritor, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, TypeArchitect, context(agent_power(institutional), _, _, _)),
    TypeInheritor \= TypeArchitect,
    assertion(TypeInheritor == mountain),
    assertion(TypeArchitect == rope).

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(kjv_puritan_new_world_exit, ClaimedType),
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, AnalyticalType, context(agent_power(analytical), _, _, _)),
    assertion(ClaimedType == AnalyticalType).

:- end_tests(kjv_puritan_new_world_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics (ε=0.20, S=0.50) are set to reflect the KJV's function
 *   in the New World, its primary context in this story. Here, it served as a
 *   low-extraction coordination tool (a Rope) for law, literacy, and social
 *   cohesion. The suppression score of 0.5 reflects that alternatives like the
 *   Geneva Bible existed but were socially marginalized in favor of the KJV as
 *   a standard. The addition of `requires_active_enforcement` reflects the
 *   social reality that maintaining this standard was not passive.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the institutional magistrate who chose to adopt it,
 *   the KJV is a flexible 'Rope' for building a society. For the powerless
 *   colonist born a generation later, it is an immutable 'Mountain', a fixed
 *   feature of reality with no conceivable alternative. For the dissenter
 *   in England, it remains a 'Snare' of state power. This story's core insight
 *   is that a single artifact can occupy three distinct classifications depending
 *   on the agent's power, exit options, and historical context.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `new_england_magistrates` benefit from a standardized text
 *     to enforce laws and build a cohesive society.
 *   - Victim: `english_puritan_dissenters` are the targets of the KJV's
 *     coercive power *in England*. Their inclusion highlights the context from
 *     which the beneficiaries exited, explaining the motivation for re-indexing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the KJV's New World function as
 *   coordination (Rope), preventing a mislabeling based on its history as a
 *   tool of suppression (Snare). The framework's perspectivalism allows both
 *   realities to be captured simultaneously without contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_kjv_puritan_new_world_exit,
    "Did the KJV truly lose its 'Snare' qualities in the New World, or did the Puritan re-indexing merely suppress its coercive elements, which could re-emerge under different conditions?",
    "Comparative historical analysis of textual authority and social control in different colonial contexts.",
    "If permanent: The re-indexing creates a lasting 'Rope'. If temporary: The 'Snare' remains latent, awaiting re-emergence.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_kjv_puritan_new_world_exit, conceptual, "Whether the coercive potential of a re-indexed text remains latent or is permanently neutralized.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(kjv_puritan_new_world_exit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction is low (< 0.46), so temporal data is not strictly required.
% Placeholder values are included for structural completeness.
narrative_ontology:measurement(kjv_tr_t0, kjv_puritan_new_world_exit, theater_ratio, 0, 0.17).
narrative_ontology:measurement(kjv_tr_t5, kjv_puritan_new_world_exit, theater_ratio, 5, 0.17).
narrative_ontology:measurement(kjv_tr_t10, kjv_puritan_new_world_exit, theater_ratio, 10, 0.17).

narrative_ontology:measurement(kjv_ex_t0, kjv_puritan_new_world_exit, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(kjv_ex_t5, kjv_puritan_new_world_exit, base_extractiveness, 5, 0.20).
narrative_ontology:measurement(kjv_ex_t10, kjv_puritan_new_world_exit, base_extractiveness, 10, 0.20).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(kjv_puritan_new_world_exit, information_standard).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(kjv_puritan_new_world_exit, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% groups and exit options is sufficient to model the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */