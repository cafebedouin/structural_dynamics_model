% ============================================================================
% CONSTRAINT STORY: adversarial_surface_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adversarial_surface_inflation, []).

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
 *   constraint_id: adversarial_surface_inflation
 *   human_readable: The Infinite Vulnerability Horizon
 *   domain: technological/cybernetic/security
 *
 * SUMMARY:
 *   The increasing complexity and interconnectedness of digital systems
 *   creates a non-linear expansion of possible attack vectors, forming an
 *   'infinite vulnerability horizon.' This scenario presents a significant
 *   challenge to security, as the number of potential vulnerabilities grows
 *   faster than the ability to identify and mitigate them. This constraint
 *   story aims to analyze the structural forces that drive this phenomenon
 *   and classify it from different perspectives.
 *
 * KEY AGENTS:
 *   - Malicious Actors: Primary beneficiary (powerful/arbitrage) - They can profit from exploiting vulnerabilities.
 *   - Digital Infrastructure: Secondary actor (moderate/constrained) - They are both beneficiaries (demand increase) and victims (reputation damage, remediation costs).
 *   - End Users: Primary target (powerless/trapped) - They are victims of attacks but lack the power to protect themselves.
 *   - Analytical Observer: Neutral observer (analytical/analytical) - Can see the entire pattern, but has no power to change the situation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adversarial_surface_inflation, 0.6).
domain_priors:suppression_score(adversarial_surface_inflation, 0.7).
domain_priors:theater_ratio(adversarial_surface_inflation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adversarial_surface_inflation, extractiveness, 0.6).
narrative_ontology:constraint_metric(adversarial_surface_inflation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(adversarial_surface_inflation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adversarial_surface_inflation, tangled_rope).
narrative_ontology:human_readable(adversarial_surface_inflation, "The Infinite Vulnerability Horizon").
narrative_ontology:topic_domain(adversarial_surface_inflation, "technological/cybernetic/security").

domain_priors:requires_active_enforcement(adversarial_surface_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adversarial_surface_inflation, malicious_actors).
narrative_ontology:constraint_victim(adversarial_surface_inflation, digital_infrastructure).
narrative_ontology:constraint_victim(adversarial_surface_inflation, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% End users, largely unaware and unable to effectively defend against the growing attack surface, are trapped in a system where vulnerabilities are constantly exploited.
constraint_indexing:constraint_classification(adversarial_surface_inflation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Digital infrastructure providers are constrained by economic incentives and competitive pressures, making it difficult to fully address the expanding attack surface. They benefit from the growing complexity as it increases demand for their services, but they are also victims of it when attacks succeed.
constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Malicious actors benefit from the expanding attack surface, as it provides more opportunities for exploitation and arbitrage.
constraint_indexing:constraint_classification(adversarial_surface_inflation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the increasing attack surface presents a complex challenge that requires a multi-faceted approach to address. This perspective sees the inherent coordination issues and exploitative rent-seeking.
constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adversarial_surface_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adversarial_surface_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_surface_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adversarial_surface_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(adversarial_surface_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is set to 0.6 because malicious actors are able to extract significant value from the system. Suppression is set to 0.7 because there are significant barriers to preventing the expansion of the attack surface. The theater ratio is set to 0.3 because only a small portion of the activity is performative.
 *
 * PERSPECTIVAL GAP:
 *   End users perceive the situation as a snare because they are trapped in a vulnerable system. Digital infrastructure providers see it as a tangled rope because they are both constrained and benefit from the situation. Malicious actors view it as a rope because it's simply providing them arbitrage opporutnities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's ability to exit the system and/or profit from it. Malicious actors have high exit options and high profit potential, so their directionality is low. End users have low exit options and suffer the consequences, so their directionality is high.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint highlights the tension between coordination (digital infrastructure providing useful services) and extraction (malicious actors exploiting vulnerabilities). The classification prevents mislabeling coordination as pure extraction by recognizing the value provided by the digital infrastructure, even though that infrastructure also creates opportunities for exploitation. This also prevents misclassifying the criminal behavior as just a cost of doing business, by accounting for the large-scale impact on end users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_threshold,
    'At what level of system complexity does the attack surface grow non-linearly?',
    'Empirical analysis of vulnerability reports across systems of varying complexity.',
    'Determines the effectiveness of different mitigation strategies. If the threshold is low, preventative measures are crucial. If high, reactive measures may suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_threshold, empirical, 'Complexity threshold for non-linear attack surface growth.').

omega_variable(
    economic_incentives_alignment,
    'Can economic incentives be aligned to encourage better security practices among digital infrastructure providers?',
    'Analysis of different regulatory and market-based incentive mechanisms.',
    'Determines the feasibility of addressing the problem through market forces versus regulatory intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_incentives_alignment, preference, 'Feasibility of aligning economic incentives for better security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adversarial_surface_inflation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adve_tr_t0, adversarial_surface_inflation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(adve_tr_t5, adversarial_surface_inflation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(adve_tr_t10, adversarial_surface_inflation, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(adve_be_t0, adversarial_surface_inflation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(adve_be_t5, adversarial_surface_inflation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(adve_be_t10, adversarial_surface_inflation, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adversarial_surface_inflation, global_infrastructure).
narrative_ontology:affects_constraint(adversarial_surface_inflation, supply_chain_vulnerabilities).
narrative_ontology:affects_constraint(adversarial_surface_inflation, data_privacy_erosion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
