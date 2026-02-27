% ============================================================================
% CONSTRAINT STORY: bay_of_pigs_operational_silo
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bay_of_pigs_operational_silo, []).

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
 *   constraint_id: bay_of_pigs_operational_silo
 *   human_readable: The CIA-Pentagon Intelligence/Action Silo (Operation Zapata)
 *   domain: political/military
 *
 * SUMMARY:
 *   Operation Zapata, the Bay of Pigs invasion, was severely hampered by an
 *   extreme 'need to know' operational silo within the CIA and between the
 *   CIA and the Pentagon. This silo prevented dissenting opinions and
 *   critical information from reaching key decision-makers, ultimately
 *   contributing to the operation's failure and significant strategic
 *   consequences.
 *
 * KEY AGENTS:
 *   - CIA Leadership: Primary beneficiary (institutional/constrained) - sought increased power and influence, initially benefitting from the operation's planning.
 *   - Cuban Exiles: Primary victim (powerless/trapped) - bore the brunt of the operation's failure and its immediate consequences.
 *   - US National Security: Secondary victim (moderate/constrained) - suffered long-term reputational and strategic damage from the failed operation.
 *   - Pentagon Planners: Supporting Institution (institutional/constrained) - contributed resources but lacked full operational control and complete information.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bay_of_pigs_operational_silo, 0.75).
domain_priors:suppression_score(bay_of_pigs_operational_silo, 0.8).
domain_priors:theater_ratio(bay_of_pigs_operational_silo, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, extractiveness, 0.75).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bay_of_pigs_operational_silo, tangled_rope).
narrative_ontology:human_readable(bay_of_pigs_operational_silo, "The CIA-Pentagon Intelligence/Action Silo (Operation Zapata)").
narrative_ontology:topic_domain(bay_of_pigs_operational_silo, "political/military").

domain_priors:requires_active_enforcement(bay_of_pigs_operational_silo).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bay_of_pigs_operational_silo, cia_leadership).
narrative_ontology:constraint_victim(bay_of_pigs_operational_silo, cuban_exiles).
narrative_ontology:constraint_victim(bay_of_pigs_operational_silo, us_national_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Cuban exiles were trapped by the operation, unable to exit once the invasion began, and bore the brunt of the failure. They had little to no power to alter the course of events and suffered the most immediate consequences.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% The US national security interests were damaged by the failed operation, facing long-term reputational and strategic costs. While not immediately 'powerless,' the long-term damage constrained future policy options and undermined credibility.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The Pentagon initially viewed this as a coordination effort to assist an ally, but later found themselves constrained by the CIA's operational control and the limited information sharing. They did not fully benefit as the plan did not originate from the pentagon and were only supporting, and had limited exit due to hierarchy.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The CIA leadership initially benefitted from the operation by enhancing their power and influence. However, the failure of the Bay of Pigs constrained their future operations and damaged their reputation, turning their benefit into a liability.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Looking back, the intelligence/action silo appears as a degraded process. The CIA's initial benefit has long since atrophied, and the continued failures to learn from this case suggest the silo persists more as a performance than a functional component of national security strategy. The initial coordination benefits have degraded due to the long term damage.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bay_of_pigs_operational_silo_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bay_of_pigs_operational_silo, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bay_of_pigs_operational_silo, TR),
    TR >= 0.70.

:- end_tests(bay_of_pigs_operational_silo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The 'need to know' silo extracted critical information and alternative perspectives from the decision-making process, leading to a flawed plan and ultimately a strategic failure. Suppression (0.80): High. Active suppression of dissenting opinions and alternative assessments within the CIA and the intentional lack of communication with external agencies led to significant barriers to effective decision-making. Theater Ratio (0.75): High. While some performative elements existed, the silo was primarily functional in suppressing information and controlling the operation, but also had elements of theater to maintain the silo.
 *
 * PERSPECTIVAL GAP:
 *   The Cuban exiles experienced the operation as a snare, trapped and powerless against its unfolding consequences. US National Security suffered long-term damage, feeling the constraints of the failed operation for years to come. The CIA leadership initially benefited, but the ultimate failure constrained their agency and damaged their reputation. The historical analysis views this as a cautionary tale, a degraded process with limited remaining coordination value.
 *
 * DIRECTIONALITY LOGIC:
 *   CIA leadership benefits from control. Cuban exiles and US National Security bear the costs of failure. Pentagon is constrained by limited information. The CIA leadership's constrained exit acknowledges the reputational damage, lowering their chi relative to pure beneficiaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_flow,
    'To what extent did the extreme ''need to know'' prevent critical information and dissenting opinions from reaching key decision-makers?',
    'Declassification of internal CIA memos and after-action reports detailing the flow of information and the suppression of dissenting viewpoints.',
    'Determines the degree to which the silo functioned as an active suppression mechanism (snare) versus a coordination failure (rope) or a temporary, necessary measure (scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_flow, empirical, 'Degree to which the ''need to know'' suppressed dissenting opinions.').

omega_variable(
    success_definition,
    'How was ''success'' defined and measured within the CIA leading up to the operation, and how did this definition influence decision-making?',
    'Analysis of internal CIA planning documents and interviews with surviving personnel to determine the criteria used to assess the likelihood of success and the factors considered most important.',
    'If success was defined narrowly (e.g., solely on the military aspects of the invasion) it would support the Snare classification. A broader, more realistic definition would suggest a less extractive coordination failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(success_definition, conceptual, 'Definition of ''success'' within the CIA.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bay_of_pigs_operational_silo, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bay__tr_t0, bay_of_pigs_operational_silo, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bay__tr_t1, bay_of_pigs_operational_silo, theater_ratio, 1, 0.4).
narrative_ontology:measurement(bay__tr_t2, bay_of_pigs_operational_silo, theater_ratio, 2, 0.75).

% Extraction over time
narrative_ontology:measurement(bay__be_t0, bay_of_pigs_operational_silo, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bay__be_t1, bay_of_pigs_operational_silo, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(bay__be_t2, bay_of_pigs_operational_silo, base_extractiveness, 2, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bay_of_pigs_operational_silo, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
