% ============================================================================
% CONSTRAINT STORY: ritualistic_transition_scaffold
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ritualistic_transition_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ritualistic_transition_scaffold
 *   human_readable: The Habit-Building Scaffold
 *   domain: social/institutional
 *
 * SUMMARY:
 *   An intentional use of procedural theater to stabilize a chaotic
 *   organization. This habit-building scaffold leverages existing ritual
 *   mechanisms to reinforce desired behaviors during periods of turbulence.
 *   As members become accustomed to the new behaviors, they become
 *   increasingly autonomous, meaning extraction through this constraint
 *   diminishes.
 *
 * KEY AGENTS:
 *   - organization_members: Primary target (moderate/mobile) — must invest time/energy into the ritual.
 *   - leadership: Primary beneficiary (institutional/arbitrage) — experiences improved organizational performance and stability.
 *   - consultant: (institutional/constrained) - experiences reduced organization performance because of dependence on theatrics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ritualistic_transition_scaffold, 0.35).
domain_priors:suppression_score(ritualistic_transition_scaffold, 0.25).
domain_priors:theater_ratio(ritualistic_transition_scaffold, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(ritualistic_transition_scaffold, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ritualistic_transition_scaffold, scaffold).
narrative_ontology:human_readable(ritualistic_transition_scaffold, "The Habit-Building Scaffold").
narrative_ontology:topic_domain(ritualistic_transition_scaffold, "social/institutional").

narrative_ontology:has_sunset_clause(ritualistic_transition_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ritualistic_transition_scaffold, organization_members).
narrative_ontology:constraint_beneficiary(ritualistic_transition_scaffold, leadership).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Member experiences scaffold as temporary support for behavioral change. Sees benefit in improved habits, but can leave if the ritual becomes burdensome.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Leadership benefits from improved organizational culture and stability. They can adjust the ritual to maintain engagement, experiencing it as a pure coordination mechanism.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Outside consultants may find that rituals are implemented as pure theater without the intended behavioral effects. The organization has become reliant on the theater to maintain its appearance.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer views this as a Tangled Rope because of the mixed benefits/drawbacks. The organization coordination is improved but some extraction may occur due to the time/energy cost.
constraint_indexing:constraint_classification(ritualistic_transition_scaffold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritualistic_transition_scaffold_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(ritualistic_transition_scaffold, TR),
    TR >= 0.70.

:- end_tests(ritualistic_transition_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low-moderate. Members invest time/energy, but the intended return is worth the cost in their immediate experience. Suppression (0.25): Low. Members can opt out of the ritual. Theater Ratio (0.65): Moderate-High. The ritual has performative elements intended to motivate member compliance. In early phases, pure ritual has high utility, but has rapidly diminishing returns. If the utility falls to zero, the practice shifts to a theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   The consultant views the ritual as a piton, a non-functional artifact maintained through inertia. The members view it initially as a useful scaffold. The organization's leadership sees it as an essential rope to keep the organization performing well. Over time the rope degrades to a tangled rope, or the scaffold is dismantled.
 *
 * DIRECTIONALITY LOGIC:
 *   Members are targets, leaders are beneficiaries, and external consultants (if present) are observers. The directionality is derived from those relations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_drift,
    'Will the ritual drift into pure theater, losing its function as a habit-building aid?',
    'Regular audits of ritual effectiveness, surveys of member engagement, tracking behavioral metrics.',
    'If the ritual becomes pure theater, the classification shifts from scaffold to piton. The coordination benefit disappears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_drift, empirical, 'Likelihood of ritualistic drift.').

omega_variable(
    member_engagement,
    'How engaged are members with the ritual and its intended outcomes?',
    'Measure participation rates, feedback surveys, observed behavior change, and qualitative interviews.',
    'Low engagement can lead to the ritual becoming a snare for members, while high engagement reinforces its scaffold function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_engagement, empirical, 'Member engagement measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ritualistic_transition_scaffold, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ritu_tr_t0, ritualistic_transition_scaffold, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ritu_tr_t6, ritualistic_transition_scaffold, theater_ratio, 6, 0.6).
narrative_ontology:measurement(ritu_tr_t12, ritualistic_transition_scaffold, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(ritu_be_t0, ritualistic_transition_scaffold, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ritu_be_t6, ritualistic_transition_scaffold, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(ritu_be_t12, ritualistic_transition_scaffold, base_extractiveness, 12, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ritualistic_transition_scaffold, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
