% ============================================================================
% CONSTRAINT STORY: franchisee_corporate_squeeze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_franchisee_corporate_squeeze, []).

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
 *   constraint_id: franchisee_corporate_squeeze
 *   human_readable: Franchise Agreement Squeeze
 *   domain: economic
 *
 * SUMMARY:
 *   Franchise agreements, while intended for mutual benefit, can become
 *   mechanisms for extracting wealth from franchisees by the corporate
 *   franchisor. This squeeze occurs through various means, including high
 *   royalty fees, mandatory purchases from the franchisor at inflated prices,
 *   and restrictive operational requirements. The power imbalance between the
 *   franchisor and franchisee often leads to the franchisee bearing a
 *   disproportionate share of the risk and cost, while the franchisor
 *   benefits from brand expansion and revenue generation with limited
 *   liability.
 *
 * KEY AGENTS:
 *   - corporate_franchisor: Primary beneficiary (institutional/arbitrage) - benefits from royalty fees and brand expansion.
 *   - individual_franchisees: Primary victim (powerless/trapped) - trapped in agreements with limited exit options.
 *   - franchisee_association: Organized actor (organized/constrained) - attempts to exert influence but is limited by the agreement and franchisor power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(franchisee_corporate_squeeze, 0.65).
domain_priors:suppression_score(franchisee_corporate_squeeze, 0.7).
domain_priors:theater_ratio(franchisee_corporate_squeeze, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, extractiveness, 0.65).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(franchisee_corporate_squeeze, tangled_rope).
narrative_ontology:human_readable(franchisee_corporate_squeeze, "Franchise Agreement Squeeze").
narrative_ontology:topic_domain(franchisee_corporate_squeeze, "economic").

domain_priors:requires_active_enforcement(franchisee_corporate_squeeze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(franchisee_corporate_squeeze, corporate_franchisor).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, individual_franchisees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual franchisees often find themselves trapped in agreements with limited exit options and high costs associated with breaking the contract. They are highly vulnerable to extraction.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The corporate franchisor benefits from the franchise agreement through royalties, fees, and brand expansion. They can arbitrage different aspects of the agreement for their benefit.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a global perspective, the franchise agreement represents a complex interaction involving elements of both coordination and extraction. The analytical observer sees the structural tensions inherent in the arrangement.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A franchisee association could potentially organize and exert some influence, but is still constrained by the legal agreement and the power of the franchisor.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(franchisee_corporate_squeeze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(franchisee_corporate_squeeze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(franchisee_corporate_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.65) because the franchisor often dictates terms that heavily favor them, extracting wealth from the franchisees. Suppression is also high (0.70) because franchisees have limited exit options and are often bound by restrictive covenants. The theater ratio is low (0.30) as the actions undertaken have very little to do with theatrical performances or signaling.
 *
 * PERSPECTIVAL GAP:
 *   The franchisor views the agreement as a mutually beneficial partnership (Rope), while the franchisee experiences it as a Snare. This is because the franchisor benefits from the franchisee's labor and capital investment while bearing minimal risk, whereas the franchisee is heavily dependent on the franchisor and bears most of the operational risks. The theorist recognizes elements of both coordination and extraction (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The corporate franchisor benefits directly from the franchise agreement through royalties and fees. Individual franchisees bear the costs of operation and are subject to the franchisor's control. The franchisee association attempts to balance these dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contractual_power_imbalance,
    'To what extent does the franchise agreement structurally favor the franchisor, limiting franchisee autonomy and profitability?',
    'Legal analysis of standard franchise agreements, empirical studies of franchisee profitability vs. franchisor revenue.',
    'If high power imbalance: Snare classification more apt. If balanced: could shift to Rope or Tangled Rope depending on other factors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractual_power_imbalance, empirical, 'Extent of contractual power imbalance between franchisor and franchisee.').

omega_variable(
    enforcement_effectiveness,
    'How effectively are franchise agreements enforced, and what recourse do franchisees have against unfair practices?',
    'Analysis of franchise litigation outcomes, surveys of franchisee experiences with dispute resolution.',
    'Weak enforcement: mitigates Snare classification. Strong enforcement: strengthens Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Effectiveness of franchise agreement enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(franchisee_corporate_squeeze, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fran_tr_t0, franchisee_corporate_squeeze, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fran_tr_t5, franchisee_corporate_squeeze, theater_ratio, 5, 0.2).
narrative_ontology:measurement(fran_tr_t10, franchisee_corporate_squeeze, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(fran_be_t0, franchisee_corporate_squeeze, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fran_be_t5, franchisee_corporate_squeeze, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(fran_be_t10, franchisee_corporate_squeeze, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(franchisee_corporate_squeeze, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
