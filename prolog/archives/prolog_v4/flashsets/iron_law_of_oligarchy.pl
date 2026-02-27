% ============================================================================
% CONSTRAINT STORY: iron_law_of_oligarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iron_law_of_oligarchy, []).

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
 *   constraint_id: iron_law_of_oligarchy
 *   human_readable: The Iron Law of Oligarchy
 *   domain: political/social
 *
 * SUMMARY:
 *   The Iron Law of Oligarchy, proposed by Robert Michels, asserts that all
 *   complex organizations, irrespective of their initial democratic
 *   structure, inevitably evolve into oligarchies. This occurs as power
 *   becomes concentrated in the hands of a few leaders who prioritize their
 *   own interests and the organization's survival over the democratic ideals
 *   it initially espoused.
 *
 * KEY AGENTS:
 *   - Organizational Leaders: Primary beneficiary (institutional/arbitrage) - Consolidate power and influence.
 *   - Rank-and-File Members: Primary victim (powerless/trapped) - Experience loss of agency.
 *   - Middle Management: Secondary actor (moderate/constrained) - Benefit from stability, limited by leadership.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iron_law_of_oligarchy, 0.6).
domain_priors:suppression_score(iron_law_of_oligarchy, 0.7).
domain_priors:theater_ratio(iron_law_of_oligarchy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iron_law_of_oligarchy, extractiveness, 0.6).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iron_law_of_oligarchy, tangled_rope).
narrative_ontology:human_readable(iron_law_of_oligarchy, "The Iron Law of Oligarchy").
narrative_ontology:topic_domain(iron_law_of_oligarchy, "political/social").

domain_priors:requires_active_enforcement(iron_law_of_oligarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iron_law_of_oligarchy, organizational_leaders).
narrative_ontology:constraint_victim(iron_law_of_oligarchy, rank_and_file_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Rank-and-file members (SNAPE) - Feel trapped within the organization due to social ties, shared goals, or lack of alternatives. They experience the extraction of their agency and democratic input as the organization becomes increasingly dominated by a small group of leaders.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Middle Management (TANGLED ROPE) - Constrained by their position within the hierarchy. They benefit from the stability and career opportunities provided by the organization, but also experience the limitations imposed by the oligarchy. They have some mobility but are also dependent on the leaders.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Organizational leaders (ROPE) - Benefit from the system they create; they experience it as pure coordination, as they use their power to direct the organization and consolidate their influence.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: Analytical observer (TANGLED ROPE) - Observing the process over a long period, sees the organization as a Tangled Rope, where there is a mix of coordination benefits and extractive costs. Observes both the coordination and the asymmetric extraction.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_law_of_oligarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iron_law_of_oligarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(iron_law_of_oligarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Leaders extract agency and democratic input from the members. Suppression (0.70): High. Members face social and economic costs for opposing the leaders. Theater ratio (0.30): Low. Actions are somewhat theatrical, but still functional.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions within the organizational structure. Leaders see the organization as serving its purpose, while the members feel their democratic input has been extracted.
 *
 * DIRECTIONALITY LOGIC:
 *   Leaders benefit, members are victimized. Middle management has mixed experience. The analytical observer sees the trend over time.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification differentiates legitimate organizational processes from extraction by examining the balance of power and the degree to which the members' interests are represented. If the leaders only focus on their interest, then it can be labelled as a case of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internal_democracy_viability,
    'Can internal mechanisms within large organizations effectively prevent the emergence of oligarchical power structures?',
    'Comparative case studies of organizations with different internal governance structures, examining the long-term distribution of power and influence.',
    'If internal democracy is viable, the Iron Law is a contingent tendency, not an absolute law (Tangled Rope or Scaffold). If not viable, the Iron Law approaches a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_democracy_viability, empirical, 'The viability of internal democracy as a countermeasure to oligarchy.').

omega_variable(
    grassroots_movements_longevity,
    'How long can grassroots movements sustain their initial democratic ethos before succumbing to oligarchical tendencies?',
    'Longitudinal tracking of grassroots movements, measuring changes in leadership accountability, member participation, and decision-making transparency over time.',
    'If movements can sustain democracy, Iron Law is a weaker constraint. If movements succumb quickly, the Iron Law is a stronger constraint (closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grassroots_movements_longevity, empirical, 'The sustainability of democracy in grassroots movements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iron_law_of_oligarchy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iron_tr_t0, iron_law_of_oligarchy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(iron_tr_t25, iron_law_of_oligarchy, theater_ratio, 25, 0.2).
narrative_ontology:measurement(iron_tr_t50, iron_law_of_oligarchy, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(iron_be_t0, iron_law_of_oligarchy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(iron_be_t25, iron_law_of_oligarchy, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(iron_be_t50, iron_law_of_oligarchy, base_extractiveness, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iron_law_of_oligarchy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
