% ============================================================================
% CONSTRAINT STORY: codex_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
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
 *   application. This access control creates a structural asymmetry, as
 *   OpenAI and their early access partners benefit from the technology while
 *   smaller developers and researchers without access are disadvantaged. The
 *   limited access allows OpenAI to refine and improve the model and generate
 *   revenue but also suppresses potential innovation from those without
 *   access.
 *
 * KEY AGENTS:
 *   - OpenAI: The primary beneficiary (institutional/arbitrage) - Controls access and benefits from the technology.
 *   - Early Access Partners: Beneficiaries (institutional/arbitrage) - Leverage access for their own business needs.
 *   - Small Developers: Primary victim (powerless/trapped) - Hindered in their ability to compete and innovate.
 *   - Researchers without Access: Secondary victim (moderate/constrained) - Constrained in their research capabilities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(codex_access, 0.55).
domain_priors:suppression_score(codex_access, 0.7).
domain_priors:theater_ratio(codex_access, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(codex_access, extractiveness, 0.55).
narrative_ontology:constraint_metric(codex_access, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(codex_access, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(codex_access, tangled_rope).
narrative_ontology:human_readable(codex_access, "OpenAI Codex Access Control").
narrative_ontology:topic_domain(codex_access, "technological/economic").

domain_priors:requires_active_enforcement(codex_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(codex_access, openai).
narrative_ontology:constraint_beneficiary(codex_access, early_access_partners).
narrative_ontology:constraint_victim(codex_access, small_developers).
narrative_ontology:constraint_victim(codex_access, researchers_without_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% OpenAI benefits from controlling access to Codex, allowing them to manage compute resources, refine the model based on usage, and maintain a competitive advantage. They see it as a rope.
constraint_indexing:constraint_classification(codex_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Early access partners benefit from access to a powerful tool and can leverage it for their own business needs. They likely experience it as a rope, enabling new capabilities.
constraint_indexing:constraint_classification(codex_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Small developers are at a disadvantage if they cannot access Codex, as it hinders their ability to compete and innovate. They may see this as a snare, trapping them in a less competitive position.
constraint_indexing:constraint_classification(codex_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Researchers without access are constrained in their ability to study and build upon Codex. They may see this as a tangled rope, as they can still conduct research with alternative models but face a significant disadvantage.
constraint_indexing:constraint_classification(codex_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer would see this as a tangled rope, as OpenAI extracts value while also providing a valuable service to some. There is a suppression of alternatives, but not a complete one.
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
    constraint_indexing:constraint_classification(codex_access, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness: 0.55 - Significant because access to Codex gives a competitive advantage. Suppression: 0.70 - Alternatives exist, but Codex is considered superior. Theater Ratio: 0.20 - Low as Codex provides real value, but some performative marketing is also involved.
 *
 * PERSPECTIVAL GAP:
 *   OpenAI sees the access control as a rope, facilitating development and resource management. Small developers see it as a snare, preventing them from competing. Researchers experience it as a tangled rope, as they face constraints but are not entirely blocked.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI: Beneficiary + arbitrage. Small Developers: Victim + trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The access control prevents the technology from being a pure coordination mechanism, ensuring a competitive advantage for OpenAI and early access partners.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    availability_of_alternatives,
    'How many alternatives exist to OpenAI''s Codex, and how effective are they?',
    'Benchmarking and comparison of code generation models.',
    'If good alternatives exist, Codex''s restrictive access becomes less of a snare. If OpenAI maintains a significant lead, the tangled rope turns more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(availability_of_alternatives, empirical, 'The number and effectiveness of alternative models').

omega_variable(
    openai_licensing_strategy,
    'How will OpenAI evolve its licensing and access strategy over time?',
    'Monitoring of OpenAI''s announcements and API changes.',
    'A shift to more open access makes the constraint weaker; increased restrictions strengthen the snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(openai_licensing_strategy, preference, 'Future openness/restrictiveness of licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(codex_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(code_tr_t0, codex_access, theater_ratio, 0, 0.25).
narrative_ontology:measurement(code_tr_t5, codex_access, theater_ratio, 5, 0.22).
narrative_ontology:measurement(code_tr_t10, codex_access, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(code_be_t0, codex_access, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(code_be_t5, codex_access, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(code_be_t10, codex_access, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(codex_access, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
