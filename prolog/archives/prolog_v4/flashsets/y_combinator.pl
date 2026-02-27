% ============================================================================
% CONSTRAINT STORY: y_combinator
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_y_combinator, []).

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
 *   constraint_id: y_combinator
 *   human_readable: Y Combinator Standard Equity Terms
 *   domain: economic
 *
 * SUMMARY:
 *   Y Combinator's standard SAFE (Simple Agreement for Future Equity)
 *   agreement and equity terms place a constraint on early-stage startups.
 *   This creates a power dynamic where YC-backed companies have easier access
 *   to resources than non-YC startups.
 *
 * KEY AGENTS:
 *   - Y Combinator: Primary beneficiary (institutional/arbitrage)
 *   - YC Batch Startups: Beneficiary (powerful/mobile)
 *   - YC Non-Batch Startups: Victim (powerless/trapped)
 *   - YC Founder Equity: Impacted (moderate/constrained)
 *   - Analytical Observer: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(y_combinator, 0.55).
domain_priors:suppression_score(y_combinator, 0.4).
domain_priors:theater_ratio(y_combinator, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(y_combinator, extractiveness, 0.55).
narrative_ontology:constraint_metric(y_combinator, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(y_combinator, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(y_combinator, tangled_rope).
narrative_ontology:human_readable(y_combinator, "Y Combinator Standard Equity Terms").
narrative_ontology:topic_domain(y_combinator, "economic").

domain_priors:requires_active_enforcement(y_combinator).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(y_combinator, y_combinator).
narrative_ontology:constraint_beneficiary(y_combinator, yc_batch_startups).
narrative_ontology:constraint_victim(y_combinator, yc_non_batch_startups).
narrative_ontology:constraint_victim(y_combinator, yc_founder_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Startups outside the YC batch program that still have to compete with YC-backed companies face a significant disadvantage in attracting funding and talent.  They cannot easily exit this environment. These startups are often in a weaker negotiating position and are essentially 'trapped' by the outsized influence of YC-backed startups. Their access to capital is suppressed as investors favor YC companies.
constraint_indexing:constraint_classification(y_combinator, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% YC founders see a tangle of rope.  While they benefit from the brand name and resources of the YC program, they also often have to give up a larger chunk of equity than they would have otherwise, thus diluting their ownership and long-term potential gains. Their exit is 'constrained' in that they chose to be a part of YC, but their long-term equity is impacted.
constraint_indexing:constraint_classification(y_combinator, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Y Combinator experiences the equity terms as a rope.  The terms are essential to ensuring the program's continued operation and success.  They allow YC to take a standardized amount of equity in each company, which it uses to reinvest in future batches. They have complete arbitrage exit options. 
constraint_indexing:constraint_classification(y_combinator, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Startups within the YC batch program benefit from the network, resources, and potential funding opportunities that YC provides. They see this as a rope, providing valuable coordination.
constraint_indexing:constraint_classification(y_combinator, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% An analytical observer sees the YC equity terms as a tangled rope: it enables coordination and funding opportunities for early-stage startups, but also creates a power dynamic where YC benefits disproportionately compared to startups outside the YC ecosystem.  It provides value, but also extractiveness.
constraint_indexing:constraint_classification(y_combinator, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(y_combinator_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(y_combinator, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(y_combinator, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(y_combinator, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(y_combinator_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55):  Moderate, reflecting the value provided and the equity demanded.  Suppression (0.40): Limited options outside of YC create suppression. Theater Ratio: the theater ratio is low (0.20). The SAFE agreements are relatively straightforward and there's little 'theater' in the sense of performative activity not tied to functionality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap stems from the power imbalance and the varying degrees to which agents can navigate or exit the arrangement. YC sees the terms as fair (rope), non-YC startups experience suppression (snare), and YC founders must give up equity (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries of the program receive the derived coordination benefits. Victims must give up more equity than they might in other agreements. Extraction flow goes from the smaller startups towards Y Combinator, increasing asymmetry in the space. Each perspective's classification depends on the degree to which the agent experiences coordination vs. extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    yc_network_effect,
    'How much of YC''s success is due to its network effect versus its actual value add?',
    'Compare the success rates of YC-backed startups to those of startups with similar profiles but no YC backing, controlling for other factors.',
    'If network effect is dominant, YC''s extraction is higher. If the value-add is dominant, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yc_network_effect, empirical, 'Assessing the true value of YC beyond its network effect.').

omega_variable(
    alternative_funding_access,
    'Do startups outside of YC have comparable access to funding opportunities?',
    'Analyze funding data to compare the amounts and terms of funding received by YC-backed startups versus non-YC startups.',
    'If funding is more difficult to obtain, then YC''s terms create a snare. If other funding options exist, the snare is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_access, empirical, 'Determine outside-YC funding access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(y_combinator, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(y_co_tr_t0, y_combinator, theater_ratio, 0, 0.1).
narrative_ontology:measurement(y_co_tr_t5, y_combinator, theater_ratio, 5, 0.2).
narrative_ontology:measurement(y_co_tr_t10, y_combinator, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(y_co_be_t0, y_combinator, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(y_co_be_t5, y_combinator, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(y_co_be_t10, y_combinator, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(y_combinator, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
