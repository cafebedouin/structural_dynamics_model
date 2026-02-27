% ============================================================================
% CONSTRAINT STORY: rule_update_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rule_update_failure, []).

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
 *   constraint_id: rule_update_failure
 *   human_readable: Obsolete Protocol Enforcement
 *   domain: technological/social
 *
 * SUMMARY:
 *   This constraint arises when a rule or protocol, initially designed to
 *   serve a specific purpose, persists even after its relevance has
 *   diminished due to technological advancements or changes in the
 *   environment. The continued enforcement of obsolete protocols can stifle
 *   innovation, create security vulnerabilities, and impose unnecessary costs
 *   on end-users.
 *
 * KEY AGENTS:
 *   - End Users: Primary target (powerless/trapped) - bear the cost of reduced functionality and security vulnerabilities.
 *   - Innovative Entrants: Secondary target (moderate/constrained) - face significant hurdles in competing with established protocols.
 *   - Legacy System Maintainers: Primary beneficiary (institutional/arbitrage) - retain control over the system and generate revenue from maintenance and support.
 *   - Incumbent Service Providers: Secondary beneficiary (powerful/constrained) - benefit from the lock-in effect created by obsolete protocols.
 *   - Analytical Observer: Societal observer (analytical/analytical) - sees the obsolete protocol enforcement as a tangled rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rule_update_failure, 0.6).
domain_priors:suppression_score(rule_update_failure, 0.7).
domain_priors:theater_ratio(rule_update_failure, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rule_update_failure, extractiveness, 0.6).
narrative_ontology:constraint_metric(rule_update_failure, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rule_update_failure, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rule_update_failure, tangled_rope).
narrative_ontology:human_readable(rule_update_failure, "Obsolete Protocol Enforcement").
narrative_ontology:topic_domain(rule_update_failure, "technological/social").

domain_priors:requires_active_enforcement(rule_update_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rule_update_failure, legacy_system_maintainers).
narrative_ontology:constraint_beneficiary(rule_update_failure, incumbent_service_providers).
narrative_ontology:constraint_victim(rule_update_failure, end_users).
narrative_ontology:constraint_victim(rule_update_failure, innovative_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% End users are often trapped by obsolete protocols because they lack the technical expertise or power to change them. They bear the cost of reduced functionality, security vulnerabilities, and limited interoperability.
constraint_indexing:constraint_classification(rule_update_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% New companies that seek to introduce new technologies face significant hurdles in competing with established protocols, even if the newer protocols are significantly superior. Their exit options are constrained by network effects and high adoption barriers.
constraint_indexing:constraint_classification(rule_update_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The maintainers of legacy systems benefit from the continued enforcement of obsolete protocols as they retain control over the system and generate revenue from maintenance and support.
constraint_indexing:constraint_classification(rule_update_failure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% These actors benefit from the lock-in effect created by obsolete protocols, which prevents users from easily switching to competing services. This represents a degraded coordination mechanism since it no longer optimizes resource allocation. The performative aspect is maintaining the appearance of stability and compatibility while hindering innovation.
constraint_indexing:constraint_classification(rule_update_failure, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees the obsolete protocol enforcement as a tangled rope because it presents both benefits and costs to society. It provides stability and backward compatibility but also hinders innovation and progress.
constraint_indexing:constraint_classification(rule_update_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rule_update_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rule_update_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rule_update_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rule_update_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rule_update_failure, TR),
    TR >= 0.70.

:- end_tests(rule_update_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Moderate-high. The obsolete protocol extracts value from end-users and innovative entrants by limiting their options and increasing their costs. Suppression (0.7): High. The obsolete protocol actively suppresses innovation by making it difficult for new technologies to compete. Theater ratio (0.75): High. The maintenance of obsolete protocols now includes a significant performative element, as actors maintain backward compatibility for appearance's sake, even when it hinders progress.
 *
 * PERSPECTIVAL GAP:
 *   The end users see the obsolete protocol as a snare because they are trapped by it. The innovative entrants see it as a tangled rope because they are constrained by it. The legacy system maintainers see it as a rope because they benefit from it. The incumbent service providers see it as a piton because it is a degraded coordination mechanism. The analytical observer sees it as a tangled rope because it presents both benefits and costs to society.
 *
 * DIRECTIONALITY LOGIC:
 *   End users are victims and trapped, hence high d and snare classification. Innovative entrants are victims and constrained, hence high d and tangled rope classification. Legacy system maintainers are beneficiaries and have arbitrage opportunities, hence low d and rope classification. Incumbent service providers are beneficiaries but have constrained exit options, hence institutional perspective and piton. The analytical observer considers all sides and classifies as tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The analysis avoids mislabeling coordination as pure extraction by considering the historical context and the initial purpose of the protocol. Even though the protocol is now obsolete, it may have served a useful purpose in the past. The analysis also considers the cost and difficulty of transitioning to a new protocol. If the transition is too costly or difficult, it may be rational to continue enforcing the obsolete protocol, even if it is not ideal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_cost_threshold,
    'What is the acceptable transition cost to migrate from the obsolete protocol to a modern protocol?',
    'Conduct a cost-benefit analysis comparing the cost of maintaining the obsolete protocol against the cost of migrating to a new protocol.',
    'If the transition cost is too high, it will be difficult to convince stakeholders to migrate. If the transition cost is too low, the cost-benefit analysis may be flawed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_threshold, empirical, 'Acceptable transition cost to migrate').

omega_variable(
    network_effect_strength,
    'How strong are the network effects of the obsolete protocol?',
    'Measure the number of users and applications that rely on the obsolete protocol.',
    'Strong network effects make it difficult to transition to a new protocol. Weak network effects make it easier to transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_strength, empirical, 'Network effect strength of the obsolete protocol').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rule_update_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rule_tr_t0, rule_update_failure, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rule_tr_t5, rule_update_failure, theater_ratio, 5, 0.4).
narrative_ontology:measurement(rule_tr_t10, rule_update_failure, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(rule_be_t0, rule_update_failure, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rule_be_t5, rule_update_failure, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(rule_be_t10, rule_update_failure, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rule_update_failure, information_standard).
narrative_ontology:affects_constraint(rule_update_failure, legacy_software_vulnerabilities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
