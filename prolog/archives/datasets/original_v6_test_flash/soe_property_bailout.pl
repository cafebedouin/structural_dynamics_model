% ============================================================================
% CONSTRAINT STORY: soe_property_bailout
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soe_property_bailout, []).

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
 *   constraint_id: soe_property_bailout
 *   human_readable: State-Directed Purchase of Distressed Real Estate Assets
 *   domain: economic
 *
 * SUMMARY:
 *   A state-directed policy in China where State-Owned Enterprises (SOEs) are
 *   instructed to purchase foreclosed or distressed properties from failing
 *   private developers. The stated goal is to prevent systemic risk and
 *   stabilize the property market. However, this policy introduces various
 *   tensions, including moral hazard, potential misallocation of resources,
 *   and extraction from SOE shareholders.
 *
 * KEY AGENTS:
 *   - Failing Private Developers: Primary beneficiary (institutional/arbitrage) - offloads distressed assets.
 *   - SOE Shareholders: Primary victim (powerless/trapped) - bear the cost of suboptimal investments.
 *   - SOE Management: Mixed (institutional/constrained) - benefits from political capital but constrained by government directives.
 *   - Other Private Developers: Victims (moderate/constrained) - face increased competition from subsidized SOEs.
 *   - Local Governments: Mixed (institutional/constrained) - benefits from averting collapse but constrained by inefficient SOE involvement.
 *   - General Taxpayers: Victims - bear the ultimate financial burden of inefficient resource allocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soe_property_bailout, 0.6).
domain_priors:suppression_score(soe_property_bailout, 0.7).
domain_priors:theater_ratio(soe_property_bailout, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soe_property_bailout, extractiveness, 0.6).
narrative_ontology:constraint_metric(soe_property_bailout, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(soe_property_bailout, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soe_property_bailout, tangled_rope).
narrative_ontology:human_readable(soe_property_bailout, "State-Directed Purchase of Distressed Real Estate Assets").
narrative_ontology:topic_domain(soe_property_bailout, "economic").

domain_priors:requires_active_enforcement(soe_property_bailout).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soe_property_bailout, failing_private_developers).
narrative_ontology:constraint_beneficiary(soe_property_bailout, local_governments).
narrative_ontology:constraint_victim(soe_property_bailout, soe_shareholders).
narrative_ontology:constraint_victim(soe_property_bailout, other_private_developers).
narrative_ontology:constraint_victim(soe_property_bailout, general_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Failing private developers benefit in the immediate term through the offloading of distressed assets, improving their balance sheets and preventing bankruptcy. Exit option: arbitrage, as they can reinvest freed-up capital.
constraint_indexing:constraint_classification(soe_property_bailout, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% SOE shareholders are trapped, as they must bear the cost of the SOE's purchase of distressed assets without receiving commensurate returns. These investments may be suboptimal compared to other potential uses of capital, reducing long-term profitability.
constraint_indexing:constraint_classification(soe_property_bailout, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% SOE management are both beneficiaries and victims. They benefit from political capital by fulfilling the mandate and averting systemic risk. However, they are constrained by government directives and may face long-term losses from acquiring distressed assets at inflated prices. Exit: Constrained – limited ability to refuse directives.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Other private developers that are not failing are at a disadvantage, as they do not receive similar bailouts and face increased competition from subsidized SOEs. Exit option: Constrained – cannot access SOE advantages.
constraint_indexing:constraint_classification(soe_property_bailout, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Local governments benefit from averting a collapse of the property sector within their jurisdiction, protecting their tax base and employment. However, they become constrained by the SOE's acquisition of potentially low-value assets, reducing overall market efficiency and introducing moral hazard.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer views the policy as a Tangled Rope, recognizing the coordination aspect of preventing systemic risk but also the extraction from SOE shareholders and the potential for misallocation of resources.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soe_property_bailout_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soe_property_bailout, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soe_property_bailout, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soe_property_bailout, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(soe_property_bailout_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The policy extracts value from SOE shareholders and other private developers through increased competition and resource misallocation. Suppression (0.70): The policy suppresses market signals and distorts competition. The theater_ratio is low (0.30) as the policy's stated goals align to some degree with actual coordination function (prevent systemic risk).
 *
 * PERSPECTIVAL GAP:
 *   The failing private developers see the policy as a Rope, a lifeline that allows them to survive. However, SOE shareholders see the policy as a Snare, extracting value from their investments. Other private developers view it as a competitive disadvantage. SOE management experience is mixed. The analytical observer acknowledges the coordination aspect of preventing systemic risk but also the extraction from SOE shareholders and the potential for misallocation of resources, leading to a Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural position of each actor. Failing developers have arbitrage, experiencing minimal extraction. SOE shareholders are trapped, bearing the full cost. SOE management are constrained and experience a mixed influence. Local governments benefit from stability but are constrained by SOE inefficiency. Other private developers face constrained competition.
 *
 * MANDATROPHY ANALYSIS:
 *   The policy could be mislabeled as a pure Rope (coordination mechanism) if only the perspective of the failing developers and the immediate stabilization of the property market are considered. However, the victims (SOE shareholders and other private developers) reveal the extraction aspect, making Tangled Rope the more accurate classification. The moral hazard and resource misallocation are crucial aspects in resolving this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_valuation_accuracy,
    'To what extent are the distressed real estate assets accurately valued during the SOE purchase?',
    'Independent audits of purchased properties, comparison of transaction prices with market comparables.',
    'If undervalued: SOEs are unfairly burdened, leading to long-term financial strain. If overvalued: Private developers are excessively bailed out, creating moral hazard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_valuation_accuracy, empirical, 'Accuracy of property valuation during the distressed asset purchase.').

omega_variable(
    soe_investment_efficiency,
    'How efficiently can SOEs manage and redevelop the acquired distressed properties?',
    'Track occupancy rates, rental income, and redevelopment costs for the purchased properties over time, compared to benchmarks for private developers.',
    'If inefficient: The policy represents a long-term drain on state resources, with limited economic benefit. If efficient: The policy may succeed in stabilizing the property market and supporting economic growth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soe_investment_efficiency, empirical, 'Efficiency of SOE investment and redevelopment of distressed properties.').

omega_variable(
    moral_hazard_extent,
    'To what extent does the policy create a moral hazard, encouraging future reckless behavior by private developers?',
    'Observe changes in lending standards and risk-taking behavior by private developers following the implementation of the bailout policy.',
    'If high moral hazard: Private developers will increase their risk appetite and debt levels, knowing that they will be bailed out if they fail, leading to future instability. If low moral hazard: The policy will have little impact on developer behavior, with market discipline continuing to prevail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_extent, empirical, 'Extent to which the policy creates moral hazard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soe_property_bailout, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soe__tr_t0, soe_property_bailout, theater_ratio, 0, 0.2).
narrative_ontology:measurement(soe__tr_t5, soe_property_bailout, theater_ratio, 5, 0.3).
narrative_ontology:measurement(soe__tr_t10, soe_property_bailout, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(soe__be_t0, soe_property_bailout, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(soe__be_t5, soe_property_bailout, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(soe__be_t10, soe_property_bailout, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soe_property_bailout, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
