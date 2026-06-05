% ============================================================================
% CONSTRAINT STORY: dionysaic_frenzy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dionysaic_frenzy, []).

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
 *   constraint_id: dionysaic_frenzy
 *   human_readable: Dionysiac Religious Possession
 *   domain: religious/social
 *
 * SUMMARY:
 *   Dionysiac religious possession involves a state of divinely induced
 *   madness, compelling subjects to abandon their social roles for mountain
 *   rites. This phenomenon can be viewed as a constraint that extracts from
 *   individual autonomy and social order while simultaneously providing a
 *   release valve for social tensions and strengthening community bonds. The
 *   degree of extraction and suppression varies based on the perspective of
 *   the individual, the community, and the ruling elite.
 *
 * KEY AGENTS:
 *   - Possessed Individuals: Primary target (powerless/trapped) — experience loss of control and potential social stigma.
 *   - Dionysus Cult: Primary beneficiary (institutional/arbitrage) — gains followers and influence.
 *   - Local Community: Mixed impact (moderate/constrained) — experiences both disruption and communal bonding.
 *   - Ruling Elite: Potential victim (institutional/constrained) — may see the frenzy as a threat to social order.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dionysaic_frenzy, 0.6).
domain_priors:suppression_score(dionysaic_frenzy, 0.7).
domain_priors:theater_ratio(dionysaic_frenzy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dionysaic_frenzy, extractiveness, 0.6).
narrative_ontology:constraint_metric(dionysaic_frenzy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dionysaic_frenzy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dionysaic_frenzy, tangled_rope).
narrative_ontology:human_readable(dionysaic_frenzy, "Dionysiac Religious Possession").
narrative_ontology:topic_domain(dionysaic_frenzy, "religious/social").

domain_priors:requires_active_enforcement(dionysaic_frenzy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dionysaic_frenzy, dionysus_cult).
narrative_ontology:constraint_beneficiary(dionysaic_frenzy, community_cohesion).
narrative_ontology:constraint_victim(dionysaic_frenzy, social_order).
narrative_ontology:constraint_victim(dionysaic_frenzy, possessed_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the individual overtaken by the frenzy, losing control and potentially facing social repercussions.
constraint_indexing:constraint_classification(dionysaic_frenzy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the local community, experiencing both the disruption of social order and potential revitalization of communal bonds.
constraint_indexing:constraint_classification(dionysaic_frenzy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of the Dionysus cult leadership, benefiting from increased followers and influence, shaping and managing the rituals.
constraint_indexing:constraint_classification(dionysaic_frenzy, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of established political/religious authorities who may see the frenzy as a threat to social order and their power.
constraint_indexing:constraint_classification(dionysaic_frenzy, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of a detached observer, recognizing both the disruptive and cohesive elements of the phenomenon across different cultures and time periods.
constraint_indexing:constraint_classification(dionysaic_frenzy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dionysaic_frenzy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dionysaic_frenzy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dionysaic_frenzy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dionysaic_frenzy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dionysaic_frenzy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Moderate-high. The frenzy extracts from the possessed individual, as social norms are abandoned. It extracts from social order due to disruption. Suppression (0.7): High. Social pressures, fear of reprisal, and the allure of the ritual suppress alternatives to participation. Theater Ratio (0.3): Moderate. While there is performative display involved, it is secondary to the felt experience and transformative potential for participants.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of agents. The possessed individual experiences a snare, while the cult leadership benefits as a rope. The ruling elite may see the frenzy as an institutional snare, while the local community experiences mixed effects of a tangled rope. This difference highlights the contextual nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural positions. Victims (possessed individuals) experience extraction. Beneficiaries (cult leadership) benefit through access to followers, resources and status. Authority is undermined, a further extraction of power. The degree of agency determines how strongly the d-score moves from 0.5
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_of_possession,
    'Is the possession genuine divine intervention, a form of altered mental state, or a constructed performance?',
    'Neurological studies of subjects during rituals; historical analysis of accounts of possession across cultures',
    'Affects classification: if performance, then piton for leadership and snare for followers; if divine, rope/tangled_rope depending on exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_possession, empirical, 'Nature of the religious experience').

omega_variable(
    coercion_level,
    'To what extent is participation in the frenzy voluntary or coerced?',
    'Sociological studies of cult membership; analysis of power dynamics within the community',
    'Influences the extractiveness felt by possessed individuals; high coercion yields stronger snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_level, empirical, 'Extent of individual agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dionysaic_frenzy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dion_tr_t0, dionysaic_frenzy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dion_tr_t5, dionysaic_frenzy, theater_ratio, 5, 0.3).
narrative_ontology:measurement(dion_tr_t10, dionysaic_frenzy, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(dion_be_t0, dionysaic_frenzy, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dion_be_t5, dionysaic_frenzy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dion_be_t10, dionysaic_frenzy, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dionysaic_frenzy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
