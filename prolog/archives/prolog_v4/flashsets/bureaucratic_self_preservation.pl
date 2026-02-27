% ============================================================================
% CONSTRAINT STORY: bureaucratic_self_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_self_preservation, []).

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
 *   constraint_id: bureaucratic_self_preservation
 *   human_readable: The Inertial Office
 *   domain: political
 *
 * SUMMARY:
 *   The bureaucratic_self_preservation constraint describes a phenomenon
 *   where a government office prioritizes its own survival and budget growth
 *   over its original mission. Over time, the office's activities become more
 *   focused on maintaining its existence than on effectively serving its
 *   intended beneficiaries. This leads to inefficiency, waste, and a loss of
 *   public trust. The office engages in theatrical activities to justify its
 *   continued funding.
 *
 * KEY AGENTS:
 *   - Office Employees: Primary beneficiaries (institutional/constrained) – benefit from the office's continued operation, even if it is inefficient.
 *   - Taxpayers: Primary victims (powerless/trapped) – bear the cost of the office's inefficiency and waste.
 *   - Intended Beneficiaries: Secondary victims (moderate/constrained) – receive some services, but their needs are not fully met due to the office's self-preservation focus.
 *   - Analytical Observer: (analytical/analytical) - observes long term decline.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_self_preservation, 0.65).
domain_priors:suppression_score(bureaucratic_self_preservation, 0.7).
domain_priors:theater_ratio(bureaucratic_self_preservation, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_self_preservation, extractiveness, 0.65).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_self_preservation, piton).
narrative_ontology:human_readable(bureaucratic_self_preservation, "The Inertial Office").
narrative_ontology:topic_domain(bureaucratic_self_preservation, "political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_self_preservation, office_employees).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, taxpayers).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, intended_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The taxpayer is trapped, lacking effective means to exit the system or significantly influence the allocation of resources. Their perspective is of a snare, where extraction is high and alternatives are suppressed.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of the office employees, the office serves as a piton. While initially created for a specific purpose, the office's primary function has atrophied, but the constraint remains due to institutional inertia and theatrical maintenance. The employees benefit from the continued operation of the office, even if its original mission is no longer effectively pursued. Their exit options are constrained, as their livelihoods depend on the office's existence.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The intended beneficiaries, those the office was originally designed to serve, often find themselves in a snare. While they may receive some services, the office's focus on self-preservation means that their needs are not fully met, and they are subject to the office's bureaucratic processes with limited exit options.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the office represents a piton. Over time, the original function has degraded, and the primary purpose becomes self-preservation. The office continues to exist due to inertia and theatrical performances, despite diminishing returns.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_self_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_self_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_self_preservation, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_self_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): The office extracts resources from taxpayers to maintain its operations. Suppression (0.70): Alternatives are suppressed through the office's control over resources and its ability to influence policy decisions. Theater Ratio (0.80): The office engages in a high degree of theatrical activity to justify its existence and secure funding. Mandatrophy: The office has become more focused on self-preservation than on serving its intended beneficiaries. Its original mission has atrophied, and its activities are now largely performative.
 *
 * PERSPECTIVAL GAP:
 *   The taxpayer views the office as a snare, extracting resources without providing adequate services. The office employees see it as a means of survival and a source of livelihood, with limited exit options. The intended beneficiaries experience the office as a weak service provider with diminishing returns.
 *
 * DIRECTIONALITY LOGIC:
 *   The taxpayer, with limited exit options and little power, experiences high extraction. The office employees, benefiting from the office's existence, experience minimal extraction. The intended beneficiaries, somewhat served by the office but also constrained by its bureaucratic processes, experience moderate extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mission_drift_detectability,
    'How easily can mission drift be detected and quantified?',
    'Regular audits, performance reviews, and beneficiary feedback mechanisms.',
    'If mission drift is easily detectable, corrective action can be taken, and the piton classification can be avoided. If not, self-preservation continues unchecked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_drift_detectability, empirical, 'The degree to which mission drift is detectable.').

omega_variable(
    alternative_service_delivery,
    'Are there alternative, more efficient ways to deliver the intended services?',
    'Comparative cost-benefit analyses of different service delivery models.',
    'If alternatives exist, the office''s self-preservation is less justifiable. If not, the office may be necessary, even if inefficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_service_delivery, empirical, 'The existence of alternative, more efficient service delivery models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_self_preservation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bure_tr_t0, bureaucratic_self_preservation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bure_tr_t5, bureaucratic_self_preservation, theater_ratio, 5, 0.6).
narrative_ontology:measurement(bure_tr_t10, bureaucratic_self_preservation, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(bure_be_t0, bureaucratic_self_preservation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bure_be_t5, bureaucratic_self_preservation, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(bure_be_t10, bureaucratic_self_preservation, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_self_preservation, resource_allocation).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, government_inefficiency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
