% ============================================================================
% CONSTRAINT STORY: silklink_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silklink_2026, []).

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
 *   constraint_id: silklink_2026
 *   human_readable: SilkLink Syria-Saudi Telecom Project
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The SilkLink project is a nearly $1B infrastructure project led by Saudi
 *   Telecom Company (STC). It presents a complex interplay of benefits and
 *   risks for both Syria and Saudi Arabia. While the project promises
 *   improved telecommunications infrastructure for Syria, it also raises
 *   concerns about potential loss of autonomy and control. Saudi Arabia
 *   benefits from the project through increased regional influence and
 *   strategic partnerships, but it also faces potential reputational risks.
 *
 * KEY AGENTS:
 *   - Saudi Telecom Company: Primary beneficiary (institutional/arbitrage) - Benefits from increased revenue and regional influence.
 *   - Syrian Telecom Users: Primary victim (powerless/trapped) - Face potential surveillance and limited alternatives.
 *   - Syrian Government: Secondary actor (moderate/constrained) - Benefits from improved infrastructure but risks loss of autonomy.
 *   - Saudi Arabia (Government): Institutional actor (institutional/constrained) - Benefits politically and economically but faces reputational risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silklink_2026, 0.6).
domain_priors:suppression_score(silklink_2026, 0.4).
domain_priors:theater_ratio(silklink_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silklink_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(silklink_2026, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(silklink_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silklink_2026, tangled_rope).
narrative_ontology:human_readable(silklink_2026, "SilkLink Syria-Saudi Telecom Project").
narrative_ontology:topic_domain(silklink_2026, "technological/economic").

domain_priors:requires_active_enforcement(silklink_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silklink_2026, saudi_telecom_company).
narrative_ontology:constraint_beneficiary(silklink_2026, saudi_arabia).
narrative_ontology:constraint_victim(silklink_2026, syrian_telecom_users).
narrative_ontology:constraint_victim(silklink_2026, syrian_government_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Syrian telecom users are largely trapped within the system. They face limited alternatives and have little power to influence the terms. The benefits of improved infrastructure are offset by potential surveillance and control, making this a snare.
constraint_indexing:constraint_classification(silklink_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Syrian government benefits from improved infrastructure and international cooperation, but it is also constrained by the potential loss of autonomy and control over its telecommunications infrastructure. The extraction represents limits on autonomy.
constraint_indexing:constraint_classification(silklink_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% STC benefits from the project through increased revenue, regional influence, and strategic partnerships. It has the power and exit options to arbitrage its position, experiencing this constraint primarily as coordination.
constraint_indexing:constraint_classification(silklink_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Saudi Arabia benefits politically and economically from the project but is also constrained by potential reputational risks and the need to manage its relationship with Syria. The investment provides long-term strategic benefits but also involves potential political constraints.
constraint_indexing:constraint_classification(silklink_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% From an analytical perspective, the project appears as a tangled rope, combining elements of coordination (infrastructure development) and extraction (potential loss of autonomy and control for Syria).
constraint_indexing:constraint_classification(silklink_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silklink_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silklink_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silklink_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silklink_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(silklink_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is rated at 0.6 because the project potentially allows Saudi Arabia to exert influence over Syrian telecommunications infrastructure and data, despite providing infrastructure benefits. Suppression is at 0.4 due to the limited exit options available to Syrian users and the potential for the Syrian government to cede some control over its telecommunications infrastructure. The theater ratio is relatively low at 0.3, indicating that the functional aspects of the project (infrastructure development) are more prominent than the performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing power and exit options available to the various actors. STC sees the project as a beneficial coordination mechanism, while Syrian users may perceive it as a snare. The Syrian government experiences a mixed outcome, balancing the benefits of infrastructure development against the risks of dependency. The analytical observer sees the complex interplay of coordination and extraction, classifying the project as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural relationships between the agents. STC benefits from the project, giving it a low directionality value. Syrian users are potential victims, giving them a high directionality value. The Syrian government's directionality is moderate, reflecting its mixed position as both a beneficiary and a potential victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by acknowledging that the SilkLink project is neither purely beneficial coordination nor purely exploitative extraction. It incorporates elements of both, reflecting the complex reality of the situation. Classifying it as a pure snare would ignore the infrastructure benefits, while classifying it as a pure rope would overlook the potential for exploitation and loss of autonomy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_access_terms,
    'What are the terms of data access and control under the SilkLink agreement?',
    'Review of the full agreement and any side agreements related to data access; assessment of technical safeguards and monitoring mechanisms.',
    'If data access is fully controlled by STC/Saudi Arabia, the project is a snare for Syrian users. If data access is jointly managed with strong Syrian controls, it''s a more balanced tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_access_terms, empirical, 'Terms of data access and control').

omega_variable(
    infrastructure_dependency,
    'To what extent does Syria become dependent on Saudi Arabia for maintaining and upgrading the telecom infrastructure?',
    'Analysis of maintenance agreements, technical support arrangements, and the availability of alternative providers.',
    'High dependency makes Syria vulnerable to Saudi influence. Low dependency allows for greater Syrian autonomy and reduces the extractive element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_dependency, empirical, 'Syrian dependency on Saudi Arabia for infrastructure maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silklink_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silk_tr_t0, silklink_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(silk_tr_t5, silklink_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(silk_tr_t10, silklink_2026, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(silk_be_t0, silklink_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(silk_be_t5, silklink_2026, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(silk_be_t10, silklink_2026, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silklink_2026, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
