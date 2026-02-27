% ============================================================================
% CONSTRAINT STORY: capital_misallocation_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_misallocation_spiral, []).

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
 *   constraint_id: capital_misallocation_spiral
 *   human_readable: The Zombie Asset Loop
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Zombie Asset Loop describes a scenario where low interest rates or
 *   institutional mandates force capital into increasingly unproductive
 *   'zombie' assets. This misallocation of capital suppresses new entrants,
 *   reduces overall economic productivity, and benefits incumbent firms and
 *   financial institutions who profit from lending to these unproductive
 *   ventures. This creates a feedback loop where the initial problem
 *   perpetuates and worsens over time.
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) - benefits from continued funding despite low productivity.
 *   - Financial Institutions: Secondary beneficiary (institutional/arbitrage) - benefits from lending to zombie firms and collecting interest.
 *   - New Entrants: Primary victim (powerless/trapped) - face suppressed access to funding and market share.
 *   - Overall Economic Productivity: Secondary victim (moderate/constrained) - constrained by the misallocation of capital.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_misallocation_spiral, 0.6).
domain_priors:suppression_score(capital_misallocation_spiral, 0.7).
domain_priors:theater_ratio(capital_misallocation_spiral, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_misallocation_spiral, extractiveness, 0.6).
narrative_ontology:constraint_metric(capital_misallocation_spiral, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(capital_misallocation_spiral, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_misallocation_spiral, tangled_rope).
narrative_ontology:human_readable(capital_misallocation_spiral, "The Zombie Asset Loop").
narrative_ontology:topic_domain(capital_misallocation_spiral, "economic/technological").

domain_priors:requires_active_enforcement(capital_misallocation_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, incumbent_firms).
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, financial_institutions).
narrative_ontology:constraint_victim(capital_misallocation_spiral, new_entrants).
narrative_ontology:constraint_victim(capital_misallocation_spiral, overall_economic_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% New entrants are trapped by the misallocation of capital. They face suppressed access to funding and market share due to the advantages given to zombie firms.
constraint_indexing:constraint_classification(capital_misallocation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Overall economic productivity is constrained by the zombie asset loop. It experiences a mix of coordination (some capital is still allocated) and extraction (significant capital is misallocated), making it a tangled rope.
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Incumbent firms benefit from the zombie asset loop, as they receive continued funding despite low productivity, allowing them to maintain their market position.
constraint_indexing:constraint_classification(capital_misallocation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Financial institutions benefit from the zombie asset loop, as they can continue lending to unproductive firms and collect interest payments, even if the loans are unlikely to be repaid.
constraint_indexing:constraint_classification(capital_misallocation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer sees the zombie asset loop as a tangled rope, characterized by a mix of coordination and extraction. Low interest rates and institutional mandates misallocate capital to unproductive firms, suppressing new entrants and reducing overall economic productivity.
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_misallocation_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_misallocation_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_misallocation_spiral, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_misallocation_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(capital_misallocation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Significant. A considerable amount of capital is misallocated, diverting resources away from productive uses. Suppression (0.7): High. New entrants face significant barriers to entry, and overall economic productivity is significantly constrained. Theater Ratio (0.4): Moderate. There is some genuine economic activity, but a significant portion is performative or unproductive.
 *
 * PERSPECTIVAL GAP:
 *   The new entrants see the situation as a snare because they cannot escape the suppressed funding and market opportunities. Incumbent firms and financial institutions, however, perceive the situation as a rope because they benefit from the continued flow of capital, although the benefits are increasingly concentrated at the top. The analytical observer views it as a tangled rope, balancing the coordination and extraction aspects, recognizing the long-term damage to economic productivity despite some continued economic activity.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic aligns with the structural positions of the agents. Incumbent firms and financial institutions benefit from the capital flow and have arbitrage exit options, yielding low effective extraction. New entrants are trapped by the limited opportunities, leading to high experienced extraction. Overall economic productivity is constrained, facing a mix of coordination and extraction, resulting in a moderate level of experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by recognizing that some capital is still allocated and some economic activity is still occurring. However, the dominant aspect is the misallocation of capital and the suppression of new entrants, justifying the tangled rope classification. The analysis considers the zombie firms' benefits, thus resolving the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interest_rate_threshold,
    'What is the interest rate threshold below which capital misallocation becomes significant?',
    'Empirical analysis of the relationship between interest rates and capital allocation efficiency.',
    'Identifying the threshold will help policymakers determine the appropriate level of interest rates to maintain economic stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interest_rate_threshold, empirical, 'Interest rate threshold for significant capital misallocation.').

omega_variable(
    institutional_mandate_stringency,
    'How stringent are institutional mandates in driving capital towards unproductive assets?',
    'Analysis of the impact of institutional mandates on capital allocation decisions.',
    'Determining the stringency of institutional mandates will help understand the extent to which they contribute to capital misallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_mandate_stringency, conceptual, 'Stringency of institutional mandates in driving capital misallocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_misallocation_spiral, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capi_tr_t0, capital_misallocation_spiral, theater_ratio, 0, 0.2).
narrative_ontology:measurement(capi_tr_t5, capital_misallocation_spiral, theater_ratio, 5, 0.3).
narrative_ontology:measurement(capi_tr_t10, capital_misallocation_spiral, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(capi_be_t0, capital_misallocation_spiral, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(capi_be_t5, capital_misallocation_spiral, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(capi_be_t10, capital_misallocation_spiral, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_misallocation_spiral, resource_allocation).
narrative_ontology:affects_constraint(capital_misallocation_spiral, low_interest_rate_environment).
narrative_ontology:affects_constraint(capital_misallocation_spiral, regulatory_capture).

% DUAL FORMULATION NOTE:
% This constraint represents a specific instance of capital misallocation arising from low interest rates and institutional mandates, and is thus distinct from more general issues of market inefficiency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
