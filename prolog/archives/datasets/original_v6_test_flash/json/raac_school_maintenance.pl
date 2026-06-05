% ============================================================================
% CONSTRAINT STORY: raac_school_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_raac_school_maintenance, []).

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
 *   constraint_id: raac_school_maintenance
 *   human_readable: Systemic Response to RAAC Concrete Failures in UK Schools
 *   domain: economic/political
 *
 * SUMMARY:
 *   The RAAC concrete failures in UK schools represent a systemic failure of
 *   infrastructure maintenance and risk management. The crisis highlights the
 *   tension between short-term cost savings and long-term safety, with
 *   students, teachers, and parents bearing the brunt of the consequences.
 *   Construction companies and government consultants benefit from
 *   remediation contracts, while the Department for Education struggles to
 *   address the problem effectively. This analysis models the constraint from
 *   different perspectives, highlighting the different ways it affects
 *   different agents.
 *
 * KEY AGENTS:
 *   - Students and Teachers: Primary victims (powerless/trapped)
 *   - Parents: Secondary victims (moderate/constrained)
 *   - Construction Companies: Primary beneficiaries (institutional/arbitrage)
 *   - Department for Education (DFE): Constrained institutional actor (institutional/constrained)
 *   - Taxpayers: Ultimate bearers of cost (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(raac_school_maintenance, 0.55).
domain_priors:suppression_score(raac_school_maintenance, 0.45).
domain_priors:theater_ratio(raac_school_maintenance, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(raac_school_maintenance, extractiveness, 0.55).
narrative_ontology:constraint_metric(raac_school_maintenance, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(raac_school_maintenance, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(raac_school_maintenance, tangled_rope).
narrative_ontology:human_readable(raac_school_maintenance, "Systemic Response to RAAC Concrete Failures in UK Schools").
narrative_ontology:topic_domain(raac_school_maintenance, "economic/political").

domain_priors:requires_active_enforcement(raac_school_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(raac_school_maintenance, construction_companies).
narrative_ontology:constraint_beneficiary(raac_school_maintenance, government_consultants).
narrative_ontology:constraint_victim(raac_school_maintenance, students).
narrative_ontology:constraint_victim(raac_school_maintenance, teachers).
narrative_ontology:constraint_victim(raac_school_maintenance, parents).
narrative_ontology:constraint_victim(raac_school_maintenance, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Students and teachers directly affected by school closures and relocations. Trapped due to mandatory attendance and limited alternatives.
constraint_indexing:constraint_classification(raac_school_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Parents face disruption to childcare and education, but have some capacity to influence school policies and advocate for better resources. Constrained by geographic limitations and financial concerns related to private education.
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Construction companies benefit from contracts for remediation and new school construction. Arbitrage through government contracts.
constraint_indexing:constraint_classification(raac_school_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The DFE, while formally responsible, is constrained by budget limitations, political pressures, and bureaucratic inertia. The system of school maintenance and oversight has degraded, leading to the current crisis. Theater via performative inspections and inadequate funding.
constraint_indexing:constraint_classification(raac_school_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Observes the systemic failure to adequately maintain public infrastructure and the political incentives that prioritize short-term cost savings over long-term safety and stability.
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(raac_school_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(raac_school_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(raac_school_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(raac_school_maintenance, TR),
    TR >= 0.70.

:- end_tests(raac_school_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to high. The system extracts resources and opportunities from students and teachers through school closures and disruptions, while construction companies and consultants extract profit from remediation contracts. Suppression (0.45): Moderate. The affected parties (students, teachers, parents) have limited ability to influence the system, contributing to the suppression. Theater ratio (0.70): Moderate. Much of the activity is performative and does not result in improved conditions.
 *
 * PERSPECTIVAL GAP:
 *   The crisis manifests differently for different agents. For students and teachers, it is a direct and immediate threat to their education and well-being. For parents, it is a source of anxiety and disruption. For construction companies, it is a business opportunity. The DFE struggles to balance competing interests and address the problem effectively. The analytical observer sees a larger pattern of neglect and mismanagement of public infrastructure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position relative to the extraction flow. Students and teachers are primary targets, experiencing high d values. Construction companies are beneficiaries, experiencing low d values. The DFE is a constrained institutional actor, experiencing moderate d values. The analytical observer seeks to understand the systemic dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by understanding that the system exhibits aspects of both coordination and extraction. While there is a genuine need to remediate the RAAC concrete, the system is also prone to rent-seeking and misallocation of resources. The various perspectives highlight the different ways in which these competing forces play out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_safety_vs_short_term_cost,
    'To what extent is the focus on short-term cost savings contributing to the neglect of long-term safety and maintenance of public infrastructure?',
    'Comparative analysis of government spending priorities and maintenance budgets over time.',
    'Understanding the balance between cost savings and safety informs policy recommendations for future infrastructure projects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_safety_vs_short_term_cost, empirical, 'The balance between short-term cost savings and long-term safety.').

omega_variable(
    transparency_of_risk_assessments,
    'How transparent are the risk assessments and inspections related to RAAC concrete, and to what extent is this information accessible to the public and affected stakeholders?',
    'Assessment of the availability and accessibility of risk assessment reports and inspection data.',
    'Increased transparency can empower stakeholders to advocate for necessary interventions and improve accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_of_risk_assessments, empirical, 'The level of transparency surrounding risk assessments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(raac_school_maintenance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(raac_tr_t0, raac_school_maintenance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(raac_tr_t5, raac_school_maintenance, theater_ratio, 5, 0.55).
narrative_ontology:measurement(raac_tr_t10, raac_school_maintenance, theater_ratio, 10, 0.7).

% Extraction over time
narrative_ontology:measurement(raac_be_t0, raac_school_maintenance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(raac_be_t5, raac_school_maintenance, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(raac_be_t10, raac_school_maintenance, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(raac_school_maintenance, resource_allocation).
narrative_ontology:affects_constraint(raac_school_maintenance, uk_public_infrastructure_maintenance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
