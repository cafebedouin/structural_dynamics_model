% ============================================================================
% CONSTRAINT STORY: g7_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_g7_debt_trap, []).

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
 *   constraint_id: g7_debt_trap
 *   human_readable: G7 Debt Trap for Developing Nations
 *   domain: economic
 *
 * SUMMARY:
 *   The G7 debt trap refers to the cycle of lending by G7 nations and
 *   associated institutions (IMF, World Bank) to developing nations, often
 *   with conditions that require structural adjustment policies. These
 *   policies, while ostensibly aimed at promoting economic growth, often lead
 *   to austerity measures, privatization of public resources, and increased
 *   dependence on G7 economies. This creates a situation where developing
 *   nations are perpetually indebted, unable to escape the cycle of
 *   extraction. The debt acts as a snare, extracting resources and limiting
 *   sovereign policy choices.
 *
 * KEY AGENTS:
 *   - Developing Nations: Primary target (powerless/trapped) - bear the cost of debt repayment and structural adjustment.
 *   - G7 Nations: Primary beneficiary (institutional/arbitrage) - benefit from debt repayments, access to resources, and political influence.
 *   - Multinational Corporations: Secondary beneficiary (organized/constrained) - benefit from access to resources and markets in developing nations.
 *   - Developing Nation Citizens: Victims of austerity measures and resource extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(g7_debt_trap, 0.75).
domain_priors:suppression_score(g7_debt_trap, 0.8).
domain_priors:theater_ratio(g7_debt_trap, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(g7_debt_trap, extractiveness, 0.75).
narrative_ontology:constraint_metric(g7_debt_trap, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(g7_debt_trap, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(g7_debt_trap, snare).
narrative_ontology:human_readable(g7_debt_trap, "G7 Debt Trap for Developing Nations").
narrative_ontology:topic_domain(g7_debt_trap, "economic").

domain_priors:requires_active_enforcement(g7_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(g7_debt_trap, g7_nations).
narrative_ontology:constraint_beneficiary(g7_debt_trap, multinational_corporations).
narrative_ontology:constraint_victim(g7_debt_trap, developing_nations).
narrative_ontology:constraint_victim(g7_debt_trap, developing_nation_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Developing nations are trapped by the debt, structural adjustment policies, and lack of alternative funding sources. They bear the full cost of extraction.
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% G7 nations benefit from the debt repayments, access to resources, and political influence. They arbitrage the system.
constraint_indexing:constraint_classification(g7_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Multinational corporations benefit from access to resources and markets in developing nations, but are also constrained by regulations and political instability.
constraint_indexing:constraint_classification(g7_debt_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees the mixed coordination and extraction aspects of the G7 debt system.
constraint_indexing:constraint_classification(g7_debt_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(g7_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(g7_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(g7_debt_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(g7_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(g7_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The debt burden and structural adjustment policies extract significant resources from developing nations, hindering their long-term development. Suppression (0.80): High. Developing nations have limited alternatives to G7 lending, suppressing their ability to pursue independent economic policies. Theater ratio (0.30): Low. While there is some discussion and negotiation around debt terms, the fundamental power imbalance limits the effectiveness of these processes.
 *
 * PERSPECTIVAL GAP:
 *   The developing nations perspective is that of a snare, as they are trapped by the debt. G7 nations see it as a rope because they benefit from the coordination of global finance. Multinational corporations see a tangled rope: access to resources but also regulatory burdens. The analytical perspective sees the mixed effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing nations (powerless/trapped) experience maximum extraction. G7 nations (institutional/arbitrage) experience the debt system as a coordination mechanism that benefits them. Multinational corporations (organized/constrained) have a mixed experience. This drives the calculation of chi for each perspective. Beneficiaries are d=0, victims are d=1.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare because it primarily extracts value from developing nations with limited benefits to those nations. While there may be some coordination aspects (global financial stability), the primary outcome is extraction. Alternative framings would require demonstrating a more equitable distribution of benefits or a clear path for developing nations to escape the debt cycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_funding_sources,
    'To what extent can developing nations access alternative funding sources, bypassing the G7 debt trap?',
    'Analysis of South-South lending, Chinese investment, and other non-G7 funding mechanisms.',
    'If alternative funding is readily available, the G7 debt trap is less of a snare and more of a tangled rope. If not, it is a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_sources, empirical, 'Availability of alternative funding sources for developing nations.').

omega_variable(
    structural_adjustment_policy_effectiveness,
    'Are structural adjustment policies genuinely beneficial for developing nations, or do they primarily serve the interests of G7 nations and multinational corporations?',
    'Comparative analysis of economic development indicators in nations that adopted vs. did not adopt structural adjustment policies.',
    'If policies are beneficial, the constraint is a tangled rope. If they are harmful, it is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_adjustment_policy_effectiveness, empirical, 'The effectiveness and beneficiaries of structural adjustment policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(g7_debt_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(g7_d_tr_t0, g7_debt_trap, theater_ratio, 0, 0.4).
narrative_ontology:measurement(g7_d_tr_t10, g7_debt_trap, theater_ratio, 10, 0.35).
narrative_ontology:measurement(g7_d_tr_t20, g7_debt_trap, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(g7_d_be_t0, g7_debt_trap, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(g7_d_be_t10, g7_debt_trap, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(g7_d_be_t20, g7_debt_trap, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(g7_debt_trap, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
