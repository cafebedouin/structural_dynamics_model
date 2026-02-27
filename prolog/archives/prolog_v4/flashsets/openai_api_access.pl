% ============================================================================
% CONSTRAINT STORY: openai_api_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_api_access, []).

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
 *   constraint_id: openai_api_access
 *   human_readable: OpenAI API Access Controls
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's control over access to its API creates a complex relationship
 *   with developers and users. While access controls are necessary for
 *   quality and preventing misuse, they also introduce a degree of extraction
 *   and dependence. The API access acts as a coordination function by
 *   enabling development of tools and services, but also creates an
 *   asymmetric power dynamic due to OpenAI's gatekeeping role.
 *
 * KEY AGENTS:
 *   - openai: The primary beneficiary (institutional/arbitrage) — benefits by controlling API access.
 *   - strategic_partners: Beneficiaries with preferential access (powerful/arbitrage).
 *   - dependent_developers: Primary target (powerless/trapped) — faces dependence and potential service disruption.
 *   - end_users: Secondary target (moderate/constrained) — benefits and affected by limitations.
 *   - analytical_observer: Outside analyst (analytical/analytical) — assesses the overall structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_api_access, 0.5).
domain_priors:suppression_score(openai_api_access, 0.6).
domain_priors:theater_ratio(openai_api_access, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_api_access, extractiveness, 0.5).
narrative_ontology:constraint_metric(openai_api_access, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(openai_api_access, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_api_access, tangled_rope).
narrative_ontology:human_readable(openai_api_access, "OpenAI API Access Controls").
narrative_ontology:topic_domain(openai_api_access, "technological/economic").

domain_priors:requires_active_enforcement(openai_api_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_api_access, openai).
narrative_ontology:constraint_beneficiary(openai_api_access, strategic_partners).
narrative_ontology:constraint_victim(openai_api_access, dependent_developers).
narrative_ontology:constraint_victim(openai_api_access, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% OpenAI benefits from controlling access. This enables prioritization, experimentation, and revenue generation through strategic partnerships. They experience the API as a coordination mechanism for managing resources and allocating access.
constraint_indexing:constraint_classification(openai_api_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Developers heavily reliant on the OpenAI API are vulnerable to sudden access changes, pricing adjustments, or service disruptions. Their exit options are limited. They bear the cost of the access controls without a ready alternative.
constraint_indexing:constraint_classification(openai_api_access, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% End-users may benefit from the API's capabilities, but are also subject to limitations and restrictions imposed by OpenAI's access controls. They can potentially seek alternative solutions, but are constrained by the current market dominance.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer sees the OpenAI API access control as a tangled rope: it enables a new technology paradigm, but also concentrates power and generates extraction. Access controls can prevent misuse and ensure quality but can also be used to shape markets.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_api_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_api_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_api_access, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_api_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(openai_api_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.5 because OpenAI extracts value through pricing and control, but also provides a valuable service. Suppression is 0.6 due to the lack of immediate alternatives for many use cases. Theater ratio is 0.3 because OpenAI’s access controls serve a genuine function related to ensuring API quality and preventing misuse.
 *
 * PERSPECTIVAL GAP:
 *   OpenAI sees the API access as a coordination mechanism (Rope), necessary for managing resources and innovating safely. Dependent developers experience a snare, as they are vulnerable to sudden changes. End users see a tangled rope, benefiting from the API but also being subject to its limitations. The analytical observer sees the tangled rope nature of the constraint: it enables but also extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI and its strategic partners benefit, giving them low d values and shifting them to rope or scaffold. Dependent developers are victims with few exit options, thus high d values and classification as a snare. End-users have some degree of mobility, but the limitations create a moderate d value and a tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Tangled Rope and prevents mislabeling because the access mechanism is a coordination for controlled AI use and commercialization, rather than pure extraction. This classification also captures the reality that AI tools built on OpenAI services are simultaneously powerful and limited by the API access. If it was just a Snare, it would ignore the massive benefits of OpenAI services to downstream users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_alternative_emergence,
    'To what extent will competing models and open-source alternatives reduce dependence on OpenAI''s API?',
    'Track the growth of alternative language models, the adoption rates of open-source tools, and the willingness of developers to switch platforms.',
    'If strong alternatives emerge, the constraint shifts towards a rope (coordination). If OpenAI maintains market dominance, the constraint remains a snare (extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_alternative_emergence, empirical, 'Market emergence of alternatives to OpenAI API.').

omega_variable(
    openai_policy_drift,
    'How will OpenAI''s access policies evolve over time, and what factors will influence these changes?',
    'Analyze OpenAI''s public statements, API documentation, and pricing structure for signals of policy shifts. Also, observe the impacts of competitive pressures and regulatory changes.',
    'Policy shifts could either tighten access (increasing the snare aspect) or broaden access (shifting towards a rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(openai_policy_drift, preference, 'OpenAI''s access policy trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_api_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(open_tr_t0, openai_api_access, theater_ratio, 0, 0.1).
narrative_ontology:measurement(open_tr_t5, openai_api_access, theater_ratio, 5, 0.2).
narrative_ontology:measurement(open_tr_t10, openai_api_access, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(open_be_t0, openai_api_access, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(open_be_t5, openai_api_access, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(open_be_t10, openai_api_access, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_api_access, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
