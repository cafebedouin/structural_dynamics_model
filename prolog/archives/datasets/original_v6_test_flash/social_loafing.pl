% ============================================================================
% CONSTRAINT STORY: social_loafing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_loafing, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_loafing
 *   human_readable: Social Loafing (The Ringelmann Effect)
 *   domain: social/economic
 *
 * SUMMARY:
 *   Social loafing, also known as the Ringelmann effect, describes the
 *   tendency of individuals to exert less effort when working in a group than
 *   when working alone. This phenomenon arises from a diffusion of
 *   responsibility, where individuals feel less accountable for the group's
 *   output. This can lead to reduced overall productivity and create an
 *   imbalance within the group, where some members contribute more than
 *   others.
 *
 * KEY AGENTS:
 *   - Social Loafers: Individuals who reduce their effort in group settings.
 *   - Diligent Group Members: Individuals who maintain a high level of effort regardless of the group setting.
 *   - Group Productivity: The overall output and effectiveness of the group.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_loafing, 0.5).
domain_priors:suppression_score(social_loafing, 0.3).
domain_priors:theater_ratio(social_loafing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_loafing, extractiveness, 0.5).
narrative_ontology:constraint_metric(social_loafing, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(social_loafing, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_loafing, tangled_rope).
narrative_ontology:human_readable(social_loafing, "Social Loafing (The Ringelmann Effect)").
narrative_ontology:topic_domain(social_loafing, "social/economic").

domain_priors:requires_active_enforcement(social_loafing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_loafing, social_loafers).
narrative_ontology:constraint_victim(social_loafing, diligent_group_members).
narrative_ontology:constraint_victim(social_loafing, group_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of a diligent group member who is trapped in a group project and must compensate for the reduced effort of others. They cannot easily exit the group or reduce their effort without risking the project's success or their reputation.
constraint_indexing:constraint_classification(social_loafing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of an individual who benefits from social loafing. They exert less effort while still sharing in the group's rewards. They have the arbitrage option to continue loafing as long as the group's overall productivity remains acceptable.
constraint_indexing:constraint_classification(social_loafing, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% From an analytical perspective, social loafing is a tangled rope. It represents a coordination failure where individual incentives are misaligned with group goals, leading to reduced overall productivity. There's both extraction (reduced individual effort) and coordination (group project completion).
constraint_indexing:constraint_classification(social_loafing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_loafing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_loafing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_loafing, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_loafing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(social_loafing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The extractiveness score reflects the reduction in effort by social loafers, which extracts from the overall group productivity and the efforts of diligent members. Suppression: The suppression score represents the factors that prevent diligent members from effectively addressing the loafing behavior, such as social pressure, fear of conflict, or lack of clear individual accountability. Theater Ratio: The theater ratio is relatively low, as there is not a significant performative aspect associated with social loafing itself. It's more about a reduction in actual effort than a display of effort.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different experiences and incentives of group members. Social loafers benefit from reduced effort, while diligent members bear the cost of compensating for the loafing. From an analytical perspective, the phenomenon represents a coordination failure where individual incentives detract from the collective good.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by who benefits and who bears the costs. Social loafers are the beneficiaries, as they exert less effort. Diligent group members and overall group productivity are the victims, as they suffer from the reduced effort.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    group_size_threshold,
    'What group size threshold exacerbates social loafing?',
    'Empirical studies varying group sizes and measuring individual effort levels.',
    'Determines the point at which interventions are most effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_size_threshold, empirical, 'Group size threshold for social loafing').

omega_variable(
    task_visibility,
    'How does the visibility of individual contributions affect social loafing?',
    'Experimental manipulations of task visibility and measurement of individual effort.',
    'Informs the design of tasks and evaluation methods.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(task_visibility, empirical, 'Impact of individual contribution visibility on social loafing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_loafing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soci_tr_t0, social_loafing, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soci_tr_t5, social_loafing, theater_ratio, 5, 0.2).
narrative_ontology:measurement(soci_tr_t10, social_loafing, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(soci_be_t0, social_loafing, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soci_be_t5, social_loafing, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(soci_be_t10, social_loafing, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_loafing, resource_allocation).
narrative_ontology:affects_constraint(social_loafing, groupthink).
narrative_ontology:affects_constraint(social_loafing, bystander_effect).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
