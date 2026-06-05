% ============================================================================
% CONSTRAINT STORY: codex_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_codex_access, []).

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
 *   constraint_id: codex_access
 *   human_readable: OpenAI Codex Access Control
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI controls access to its Codex models via an API or dedicated
 *   application. This control creates a constraint with both coordination and
 *   extraction elements. OpenAI benefits from revenue and maintaining a
 *   leading position, while independent developers may face barriers to entry
 *   and innovation. The early access customers get the benefits from the
 *   technology but they are still constrained by OpenAI's policies.
 *
 * KEY AGENTS:
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — benefits from revenue, control, and maintaining leadership.
 *   - Independent Developers: Primary victim (powerless/trapped) — face barriers to entry and innovation.
 *   - Early Access Customers: Moderate actor (moderate/constrained) — benefit from access but are subject to OpenAI's control and policies.
 *   - Competing AI Models: Victim (moderate/constrained) - restricted datasets limit the competition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(codex_access, 0.55).
domain_priors:suppression_score(codex_access, 0.65).
domain_priors:theater_ratio(codex_access, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(codex_access, extractiveness, 0.55).
narrative_ontology:constraint_metric(codex_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(codex_access, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(codex_access, tangled_rope).
narrative_ontology:human_readable(codex_access, "OpenAI Codex Access Control").
narrative_ontology:topic_domain(codex_access, "technological/economic").

domain_priors:requires_active_enforcement(codex_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(codex_access, openai).
narrative_ontology:constraint_beneficiary(codex_access, early_access_customers).
narrative_ontology:constraint_victim(codex_access, independent_developers).
narrative_ontology:constraint_victim(codex_access, competing_ai_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Independent developers face high barriers to entry due to lack of access to Codex. This limits their ability to innovate and compete in the AI-assisted coding space. They are trapped by OpenAI's control.
constraint_indexing:constraint_classification(codex_access, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Early access customers benefit from Codex but are constrained by the terms of service and limitations of the API. They have some influence but also experience some extraction.
constraint_indexing:constraint_classification(codex_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% OpenAI benefits through control of Codex, generating revenue and maintaining its leading position. Control enables efficient resource allocation and enforcement of quality standards.
constraint_indexing:constraint_classification(codex_access, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% The analytical observer sees the control as a tangled rope. It facilitates efficient AI model deployment and revenue generation (coordination), but also creates barriers to entry and slows innovation within the field. The long-term impact is complex.
constraint_indexing:constraint_classification(codex_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(codex_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(codex_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(codex_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(codex_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(codex_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. OpenAI extracts value by charging for access, but also coordinates by providing a useful service. Suppression (0.65): Moderate-high. High barriers to entry are in place due to OpenAI's proprietary technology and restrictive access policies. Theater ratio (0.20): Low. OpenAI genuinely provides a useful service and the performative aspect is relatively low.
 *
 * PERSPECTIVAL GAP:
 *   Independent developers view the access control as a snare, trapping them from the technology. Early access customers see it as a tangled rope. OpenAI views this as a rope that facilitates market control and innovation. The Analytical Observer acknowledges the mixed outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI, as the primary controller of the Codex model, benefits significantly. Early access customers benefit from the service, but are constrained. Independent developers and competing models bear the cost of limited access. The derived directionality is consistent with the structural relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_model_emergence,
    'Will competing AI models emerge that provide similar capabilities to Codex, reducing OpenAI''s control?',
    'Monitor development of open-source and commercial AI code generation tools.',
    'If viable alternatives arise, the constraint shifts towards a rope or scaffold. If OpenAI maintains dominance, the constraint remains a tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_emergence, empirical, 'The emergence of alternative AI code generation models.').

omega_variable(
    openai_access_policy_shift,
    'Will OpenAI shift its access policy to be more open and inclusive?',
    'Track OpenAI''s public statements, API changes, and pricing models.',
    'A more open policy would weaken the extractive aspects of the constraint, shifting it toward a rope. A more restrictive policy would strengthen the extractive aspects, moving it toward a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(openai_access_policy_shift, preference, 'A potential shift in OpenAI''s access policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(codex_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(code_tr_t0, codex_access, theater_ratio, 0, 0.1).
narrative_ontology:measurement(code_tr_t5, codex_access, theater_ratio, 5, 0.15).
narrative_ontology:measurement(code_tr_t10, codex_access, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(code_be_t0, codex_access, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(code_be_t5, codex_access, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(code_be_t10, codex_access, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(codex_access, resource_allocation).
narrative_ontology:affects_constraint(codex_access, ai_model_training_data).
narrative_ontology:affects_constraint(codex_access, model_deployment_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
