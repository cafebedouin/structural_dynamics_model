% ============================================================================
% CONSTRAINT STORY: trade_secret_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trade_secret_law, []).

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
 *   constraint_id: trade_secret_law
 *   human_readable: Trade Secret Law (Information Ownership)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   Trade Secret Law protects confidential business information (formulae,
 *   practices, designs) that provides an enterprise a competitive advantage.
 *   This law creates a tension between incentivizing innovation by protecting
 *   information ownership and potentially hindering competition and employee
 *   mobility. Different stakeholders experience trade secret law differently,
 *   leading to varied perspectives on its overall impact.
 *
 * KEY AGENTS:
 *   - Information Owners: Beneficiaries (institutional/arbitrage) - use the law to protect valuable assets.
 *   - Employees: Constrained (moderate/constrained) - face limitations on their ability to leverage skills.
 *   - Competing Firms: Victims (powerless/trapped) - risk legal action for misappropriation.
 *   - Analytical Observer: Assesses overall impact (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trade_secret_law, 0.45).
domain_priors:suppression_score(trade_secret_law, 0.35).
domain_priors:theater_ratio(trade_secret_law, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trade_secret_law, extractiveness, 0.45).
narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(trade_secret_law, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trade_secret_law, tangled_rope).
narrative_ontology:human_readable(trade_secret_law, "Trade Secret Law (Information Ownership)").
narrative_ontology:topic_domain(trade_secret_law, "legal/economic").

domain_priors:requires_active_enforcement(trade_secret_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trade_secret_law, information_owners).
narrative_ontology:constraint_victim(trade_secret_law, competing_firms).
narrative_ontology:constraint_victim(trade_secret_law, employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Trade secret law allows firms to protect valuable information, incentivizing innovation and competitive advantage. Information owners can use legal recourse to prevent misappropriation, experiencing this as a coordination mechanism to protect their investments. They have arbitrage options by licensing or selling the information.
constraint_indexing:constraint_classification(trade_secret_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Employees are constrained by trade secret law through non-compete agreements and confidentiality clauses. While they benefit from stable employment and compensation, their ability to leverage their skills and knowledge at other firms is limited. They may experience this as a tangled rope – some benefits, but also constraints on mobility and career advancement.
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Competing firms are potentially snared by trade secret law. They face legal risks and costs if they inadvertently or intentionally misappropriate trade secrets. This can stifle legitimate competitive intelligence gathering and reverse engineering efforts. Trapped because they cannot easily access the protected information legally.
constraint_indexing:constraint_classification(trade_secret_law, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% From an analytical perspective, trade secret law represents a tangled rope. It fosters innovation by protecting intellectual property but also restricts the free flow of information and potentially hinders competition and employee mobility. The overall effect is a mix of coordination and extraction.
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trade_secret_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trade_secret_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trade_secret_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(trade_secret_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate. Trade secret law allows firms to extract value from their confidential information, but this extraction is limited by the need to balance protection with competition. Suppression (0.35): Moderate. Trade secret law suppresses the free flow of information, but this suppression is not absolute. Competitors can still engage in reverse engineering and independent discovery. Theater Ratio (0.20): Relatively low. The primary function of trade secret law is to protect valuable information, with relatively little performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the inherent trade-off between protection and competition. Information owners see trade secret law as a necessary coordination mechanism to safeguard their investments and incentivize innovation. Employees experience a mix of benefits and constraints, as trade secret law can limit their career options but also contribute to job stability. Competing firms, on the other hand, may view trade secret law as a barrier to competition, hindering their ability to innovate and grow.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural relationship of each agent to the constraint. Information owners, as beneficiaries, experience a low effective extraction. Employees, as constrained actors, experience a moderate level of extraction. Competing firms, as potential victims, experience the highest level of extraction, as they risk legal action and financial penalties if they misappropriate trade secrets. The analytical observer attempts to balance these competing perspectives to assess the overall impact of trade secret law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that trade secret law is inherently a tangled rope. It provides benefits (incentivizing innovation) and imposes costs (restricting competition and employee mobility). The challenge is to strike the right balance between these competing interests. A purely rope-like interpretation would ignore the potential for over-protection and stifling of competition. A purely snare-like interpretation would ignore the benefits of incentivizing innovation and protecting valuable information.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_protection,
    'How broadly should ''trade secret'' be defined? Does it extend to general know-how or only specific documented information?',
    'Legal precedent, economic analysis of the impact of different definitions on innovation and competition.',
    'Narrow definition: Less protection for information owners, greater competition. Broad definition: More protection, potentially reduced competition and employee mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_protection, conceptual, 'The ambiguity in the scope of ''trade secret'' definition.').

omega_variable(
    enforcement_effectiveness,
    'How effective is the enforcement of trade secret law? Are damages sufficient to deter misappropriation?',
    'Empirical analysis of trade secret litigation outcomes and their impact on firm behavior.',
    'Weak enforcement: Reduced incentive to invest in innovation. Strong enforcement: Potential for over-protection and stifling of competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'The effectiveness of legal enforcement mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trade_secret_law, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trad_tr_t0, trade_secret_law, theater_ratio, 0, 0.1).
narrative_ontology:measurement(trad_tr_t10, trade_secret_law, theater_ratio, 10, 0.2).
narrative_ontology:measurement(trad_tr_t20, trade_secret_law, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(trad_be_t0, trade_secret_law, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trad_be_t10, trade_secret_law, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(trad_be_t20, trade_secret_law, base_extractiveness, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trade_secret_law, enforcement_mechanism).
narrative_ontology:affects_constraint(trade_secret_law, patent_law).
narrative_ontology:affects_constraint(trade_secret_law, copyright_law).

% DUAL FORMULATION NOTE:
% Trade secret law, patent law, and copyright law are all mechanisms for protecting intellectual property. They are related but distinct constraints, each with its own set of base properties and perspectival implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
