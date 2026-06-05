% ============================================================================
% CONSTRAINT STORY: collective_stupidity_2026
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_stupidity_2026, []).

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
 *   constraint_id: collective_stupidity_2026
 *   human_readable: The Cipolla-Galloway Stupidity Snare
 *   domain: social/behavioral
 *
 * SUMMARY:
 *   Stupidity, defined as causing damage to others without deriving personal
 *   gain, can create a tangled rope for communities. This tangled rope occurs
 *   when individuals or groups engage in behaviors that harm others, while
 *   not personally benefiting from those actions. This creates a collective
 *   action problem where the community as a whole suffers, but no individual
 *   has the incentive to stop the behavior. The enforcement of norms against
 *   stupidity is often weak or non-existent, contributing to the persistence
 *   of the problem.
 *
 * KEY AGENTS:
 *   - Affected Communities: Primary victim (powerless/trapped) — bears the costs of stupid behavior
 *   - Community Leaders: Secondary victim (moderate/constrained) — constrained by reputational damage if they challenge the stupid behavior, but may derive benefit by maintaining the status quo
 *   - Researchers: Secondary victim (organized/constrained) — constrained by access to funding and professional reputational concerns if they push back too hard.
 *   - The Perpetrator: Primary Beneficiary (powerful/constrained) — may be constrained by norms, but is not usually penalized. Actively benefits through indifference.
 *   - Analytical Observer: Civilizational View (analytical/analytical) — no power to change behavior, only observe
 *   - Government Agencies: Institutional actor (institutional/mobile) — may benefit from the status quo if it allows them to maintain power or control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_stupidity_2026, 0.6).
domain_priors:suppression_score(collective_stupidity_2026, 0.7).
domain_priors:theater_ratio(collective_stupidity_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_stupidity_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(collective_stupidity_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(collective_stupidity_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_stupidity_2026, tangled_rope).
narrative_ontology:human_readable(collective_stupidity_2026, "The Cipolla-Galloway Stupidity Snare").
narrative_ontology:topic_domain(collective_stupidity_2026, "social/behavioral").

domain_priors:requires_active_enforcement(collective_stupidity_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_stupidity_2026, perpetrators).
narrative_ontology:constraint_victim(collective_stupidity_2026, affected_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual bears the cost of collective stupidity and is often powerless to change it.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Community leaders are constrained by the potential for reputational damage if they challenge the stupid behavior. They may, however, derive some benefit from maintaining the status quo.
constraint_indexing:constraint_classification(collective_stupidity_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Academics who study collective action failure are often constrained by access to funding and professional reputational concerns if they push back too hard.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The perpetrator may be constrained by norms and culture, but actively benefits through indifference and is often not penalized.
constraint_indexing:constraint_classification(collective_stupidity_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observers can see the stupidity, but lack any power to change it. In effect, they are only observers.
constraint_indexing:constraint_classification(collective_stupidity_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Government agencies may benefit from the status quo of collective stupidity if it allows them to maintain power or control.
constraint_indexing:constraint_classification(collective_stupidity_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_stupidity_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_stupidity_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(collective_stupidity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is 0.6 because the stupid behavior causes significant harm to others. The suppression is 0.7 because stupid behavior is difficult to combat due to social norms and lack of enforcement. The theater_ratio is low because little effort is expended to combat the behavior, or the efforts are performative rather than effective. The constraint requires active enforcement because without it, the stupid behavior persists.
 *
 * PERSPECTIVAL GAP:
 *   The affected individual sees a snare. The community leader sees a tangled rope, and has an economic interest to perpetuate the problem. The analytical observer sees that a problem exists, but has no power to stop it. Government agencies may see a rope, where the stupidity allows them to maintain control.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value (d) is determined by the agent's structural position — their power level, exit options, and relationship to the extraction flow. The pipeline computes d from these context parameters and applies the sigmoid f(d) to produce experienced extractiveness chi. Trapped agents with no exit bear maximal extraction. Perpetrators benefit through indifference, resulting in a lower d value. Government agencies benefit from the status quo, resulting in a low d value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stupidity_definition,
    'Is causing damage to others without deriving personal gain the correct definition of stupidity?',
    'Sociological and psychological study to determine common understanding of stupidity.',
    'If the definition is incorrect, the model will not be relevant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stupidity_definition, conceptual, 'Definition of Stupidity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_stupidity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_tr_t0, collective_stupidity_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coll_tr_t5, collective_stupidity_2026, theater_ratio, 5, 0.2).
narrative_ontology:measurement(coll_tr_t10, collective_stupidity_2026, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(coll_be_t0, collective_stupidity_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(coll_be_t5, collective_stupidity_2026, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(coll_be_t10, collective_stupidity_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_stupidity_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
