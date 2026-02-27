% ============================================================================
% CONSTRAINT STORY: private_identity_integration
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_identity_integration, []).

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
 *   constraint_id: private_identity_integration
 *   human_readable: The Closed-Door Identity Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Closed-Door Identity Protocol describes the implicit social
 *   expectation that individuals should have the space and time to privately
 *   integrate new understandings of their needs, wants, and identity before
 *   disclosing them publicly. This constraint balances individual well-being
 *   with social cohesion. It suggests that a period of private integration
 *   can lead to a more stable and authentic public self.
 *
 * KEY AGENTS:
 *   - individual_self: Primary beneficiary (powerful/arbitrage)
 *   - close_confidants: Secondary beneficiary and victim (moderate/constrained)
 *   - social_observers: Primary victim (powerless/trapped)
 *   - Social Media Platforms: Institutional beneficiary (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_identity_integration, 0.5).
domain_priors:suppression_score(private_identity_integration, 0.6).
domain_priors:theater_ratio(private_identity_integration, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_identity_integration, extractiveness, 0.5).
narrative_ontology:constraint_metric(private_identity_integration, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(private_identity_integration, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_identity_integration, tangled_rope).
narrative_ontology:human_readable(private_identity_integration, "The Closed-Door Identity Protocol").
narrative_ontology:topic_domain(private_identity_integration, "social/psychological").

domain_priors:requires_active_enforcement(private_identity_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_identity_integration, individual_self).
narrative_ontology:constraint_beneficiary(private_identity_integration, close_confidants).
narrative_ontology:constraint_victim(private_identity_integration, social_observers).
narrative_ontology:constraint_victim(private_identity_integration, casual_acquaintances).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The general public is largely excluded from the process of private identity integration, often facing misinterpretations or incomplete understanding due to the lack of access to the individual's internal journey. The general public cannot 'exit' the digital public square.
constraint_indexing:constraint_classification(private_identity_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The individual benefits from this protocol by having the space to integrate new aspects of their identity without external pressure or premature judgment. They can 'arbitrage' different social contexts, revealing aspects of themselves at a comfortable pace.
constraint_indexing:constraint_classification(private_identity_integration, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% Close confidants are both beneficiaries and potential victims. They gain deeper insight into the individual's evolving identity but are also constrained by the expectation of maintaining privacy and confidentiality. Their 'exit' is constrained by loyalty and social bonds.
constraint_indexing:constraint_classification(private_identity_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% From an analytical perspective, this protocol is a tangled rope: it serves a coordination function by allowing individuals to develop a coherent sense of self, but it also involves asymmetric extraction, as the general public may be excluded from aspects of an individual's life. Understanding the balance between personal privacy and public disclosure is vital for a functional society.
constraint_indexing:constraint_classification(private_identity_integration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Social media platforms benefit from users sharing aspects of their identity, as it increases engagement and data collection. They can 'arbitrage' different user behaviors to optimize their algorithms and advertising strategies.
constraint_indexing:constraint_classification(private_identity_integration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_identity_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_identity_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_identity_integration, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(private_identity_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(private_identity_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.5): The general public's access to the evolving identity of the individual is limited, creating a degree of extraction. The individual benefits from controlling the narrative. Suppression (0.6): Social norms and potential for judgment suppress premature disclosure. Theater Ratio (0.2): Low theater ratio as the primary function is genuine private integration, not public performance.
 *
 * PERSPECTIVAL GAP:
 *   The individual sees a coordination mechanism (rope), while the general public experiences exclusion (snare). Close confidants experience a mixed dynamic (tangled rope). Social media platforms benefit from increased user engagement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's relationship to the protocol. The individual benefits, while the general public bears the cost of exclusion. Social media platforms benefit from increased user engagement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_integration_period,
    'What is the optimal length of the private integration period?',
    'Longitudinal studies tracking individual identity development and social integration.',
    'Shorter period: potential for premature disclosure and social friction. Longer period: potential for social isolation and delayed integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_integration_period, empirical, 'Determine length of private integration period').

omega_variable(
    scope_of_disclosure,
    'What aspects of identity require private integration, and which can be disclosed more openly?',
    'Qualitative research exploring individual experiences with identity disclosure.',
    'Overly restrictive: individual may feel stifled. Overly permissive: may lead to confusion, public ridicule, or premature categorization',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_disclosure, preference, 'Determine the aspect of identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_identity_integration, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(priv_tr_t0, private_identity_integration, theater_ratio, 0, 0.1).
narrative_ontology:measurement(priv_tr_t2, private_identity_integration, theater_ratio, 2, 0.2).
narrative_ontology:measurement(priv_tr_t4, private_identity_integration, theater_ratio, 4, 0.3).

% Extraction over time
narrative_ontology:measurement(priv_be_t0, private_identity_integration, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(priv_be_t2, private_identity_integration, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(priv_be_t4, private_identity_integration, base_extractiveness, 4, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_identity_integration, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
