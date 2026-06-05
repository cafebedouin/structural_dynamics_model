% ============================================================================
% CONSTRAINT STORY: uk_unpaid_care_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_unpaid_care_system, []).

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
 *   constraint_id: uk_unpaid_care_system
 *   human_readable: The UK's reliance on unpaid carers for social and healthcare
 *   domain: economic/social
 *
 * SUMMARY:
 *   The UK's social care system relies heavily on a large population of
 *   unpaid carers, typically family members, who provide essential support to
 *   ill, elderly, or disabled relatives. This reliance creates a complex
 *   system where the government and NHS benefit from reduced costs, while
 *   unpaid carers often face significant personal and financial burdens.
 *
 * KEY AGENTS:
 *   - Unpaid Carers: Primary target (powerless/trapped) - bear the brunt of care responsibilities with limited support.
 *   - UK Government: Primary beneficiary (institutional/constrained) - benefits from cost savings but constrained by social expectations.
 *   - NHS: Secondary beneficiary (moderate/constrained) - benefits from reduced demand but constrained by healthcare obligations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_unpaid_care_system, 0.65).
domain_priors:suppression_score(uk_unpaid_care_system, 0.7).
domain_priors:theater_ratio(uk_unpaid_care_system, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_unpaid_care_system, extractiveness, 0.65).
narrative_ontology:constraint_metric(uk_unpaid_care_system, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(uk_unpaid_care_system, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_unpaid_care_system, tangled_rope).
narrative_ontology:human_readable(uk_unpaid_care_system, "The UK's reliance on unpaid carers for social and healthcare").
narrative_ontology:topic_domain(uk_unpaid_care_system, "economic/social").

domain_priors:requires_active_enforcement(uk_unpaid_care_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, uk_government).
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, nhs).
narrative_ontology:constraint_victim(uk_unpaid_care_system, unpaid_carers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Unpaid carers, often family members, are trapped by their sense of duty and lack of affordable alternatives, bearing the brunt of the extraction.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The UK government benefits from the unpaid care system as it reduces the financial burden on the state. However, they are constrained by public opinion and legal obligations to provide some level of support.
constraint_indexing:constraint_classification(uk_unpaid_care_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The NHS also benefits from the unpaid care system, as it reduces demand for their services. However, they are constrained by their duty to provide healthcare to all citizens.
constraint_indexing:constraint_classification(uk_unpaid_care_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a global, civilizational perspective, the system is a tangled rope, balancing cost savings with potential exploitation of carers.
constraint_indexing:constraint_classification(uk_unpaid_care_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_unpaid_care_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_unpaid_care_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_unpaid_care_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_unpaid_care_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(uk_unpaid_care_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Significant extraction from unpaid carers due to limited financial and practical support, high time commitment, and emotional burden. Suppression (0.70): High suppression due to lack of affordable alternatives and social expectations. Theater ratio (0.30): Relatively low theater, as the system is primarily functional, but some performative aspects exist, such as symbolic gestures of support.
 *
 * PERSPECTIVAL GAP:
 *   Unpaid carers experience the system as a snare, feeling trapped by their care responsibilities with limited support. The government and NHS view it as a rope, enabling them to provide social and healthcare services at a reduced cost. The analytical observer recognizes the tangled nature of the system, acknowledging both its benefits and potential exploitation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by structural relationships. The government and NHS are beneficiaries because they save resources. Unpaid carers are victims because they bear the costs of providing care. Exit options influence the degree of extraction experienced. Trapped carers experience the highest extraction, while the government and NHS, though constrained, have more flexibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The system is classified as a tangled rope, recognizing both its coordination function (providing care) and its extractive nature (relying on unpaid labor). This classification prevents mislabeling the system as either pure coordination (rope) or pure extraction (snare) by acknowledging the complex balance of benefits and burdens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_value_unpaid_care,
    'What is the true economic value of unpaid care in the UK?',
    'Independent economic analysis, considering opportunity cost, health impacts, and social contributions.',
    'Impacts the perception of extractiveness. If high, the system is more extractive. If low, less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_value_unpaid_care, empirical, 'Economic value of unpaid care.').

omega_variable(
    availability_affordable_alternatives,
    'To what extent are affordable and accessible social care alternatives available?',
    'Assessment of social care provision, costs, and eligibility criteria across the UK.',
    'Impacts the perception of suppression and exit options for carers. More alternatives equal less suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(availability_affordable_alternatives, empirical, 'Availability of affordable alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_unpaid_care_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_u_tr_t0, uk_unpaid_care_system, theater_ratio, 0, 0.4).
narrative_ontology:measurement(uk_u_tr_t5, uk_unpaid_care_system, theater_ratio, 5, 0.35).
narrative_ontology:measurement(uk_u_tr_t10, uk_unpaid_care_system, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(uk_u_be_t0, uk_unpaid_care_system, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uk_u_be_t5, uk_unpaid_care_system, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(uk_u_be_t10, uk_unpaid_care_system, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_unpaid_care_system, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
