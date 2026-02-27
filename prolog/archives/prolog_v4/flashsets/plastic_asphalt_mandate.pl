% ============================================================================
% CONSTRAINT STORY: plastic_asphalt_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plastic_asphalt_mandate, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plastic_asphalt_mandate
 *   human_readable: Government Mandate for Plastic-Infused Asphalt
 *   domain: economic/environmental/political
 *
 * SUMMARY:
 *   A government mandate requiring the incorporation of recycled plastic into
 *   asphalt road construction represents a complex interplay of economic,
 *   environmental, and political factors. While intended to address plastic
 *   waste and promote sustainability, such mandates can create unintended
 *   consequences, including increased costs, reduced road quality, and
 *   potential environmental harm. The success of the mandate hinges on
 *   factors like the quality of recycled plastic, the economic feasibility of
 *   plastic asphalt, and robust enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Municipal Taxpayers: Primary victim (powerless/trapped) — bear the cost of potentially lower-quality roads and higher project expenses.
 *   - Independent Road Builders: Secondary victim (moderate/constrained) — faced with increased costs and potential equipment changes. 
 *   - Plastic Recycling Companies: Primary beneficiary (institutional/arbitrage) — guaranteed market for recycled plastic.
 *   - Asphalt Additive Suppliers: Secondary beneficiary (institutional/arbitrage) — benefit from sales of specialized additives.
 *   - Environmental Integrity: Ultimate victim (powerless/trapped) - potential microplastic pollution and disincentive for source reduction.
 *   - Government Regulators: Enforcer (institutional/constrained) — face the challenge of balancing economic incentives with environmental goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plastic_asphalt_mandate, 0.55).
domain_priors:suppression_score(plastic_asphalt_mandate, 0.65).
domain_priors:theater_ratio(plastic_asphalt_mandate, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plastic_asphalt_mandate, extractiveness, 0.55).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plastic_asphalt_mandate, tangled_rope).
narrative_ontology:human_readable(plastic_asphalt_mandate, "Government Mandate for Plastic-Infused Asphalt").
narrative_ontology:topic_domain(plastic_asphalt_mandate, "economic/environmental/political").

domain_priors:requires_active_enforcement(plastic_asphalt_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, plastic_recycling_companies).
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, asphalt_additive_suppliers).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, municipal_taxpayers).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, independent_road_builders).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, environmental_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MUNICIPAL TAXPAYERS (SNARE) — Trapped by the mandate, they bear the cost of potentially lower-quality roads and higher project expenses due to the artificial demand for plastic asphalt. Limited ability to influence policy or exit the system.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INDEPENDENT ROAD BUILDERS (TANGLED ROPE) — Constrained by the mandate but may also benefit from new market opportunities. However, they bear increased costs if plastic asphalt is more expensive or requires new equipment/training. Exit options are limited, but not completely absent, e.g., shifting focus to private projects.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLASTIC RECYCLING COMPANIES (ROPE) — Benefit directly from the mandate, creating a guaranteed market for recycled plastic. They can arbitrage by selling to asphalt producers. See this as pure coordination and a solution to a collective action problem. 
constraint_indexing:constraint_classification(plastic_asphalt_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ASPHALT ADDITIVE SUPPLIERS (ROPE) - Benefit from selling specialized additives to incorporate the plastic into the asphalt mix. Arbitrage exit via product diversification. They view the mandate as creating a new line of business, i.e., coordination.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL INTEGRITY (SNARE) - The mandate may lead to the use of lower-quality recycled plastic, potentially releasing microplastics into the environment, offsetting the benefits. Also, it might disincentivize source reduction efforts by artificially creating demand for plastic. The environment is trapped with no ability to exit.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the mandate as a mixed bag: a potential solution to plastic waste, but also a risk of creating a market for low-quality plastic, incentivizing plastic production. A genuine coordination problem with extraction and unintended consequences.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plastic_asphalt_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plastic_asphalt_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plastic_asphalt_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The mandate extracts value from taxpayers and independent road builders by potentially increasing project costs and creating an artificial market. The value is transferred to plastic recycling companies. Suppression (0.65): High. The mandate suppresses the use of traditional asphalt alternatives and limits the choices available to road builders. Theater Ratio (0.40): Moderate. While there's a clear intention to address plastic waste, the lack of rigorous quality control and environmental impact assessment contributes to a degree of performative action.
 *
 * PERSPECTIVAL GAP:
 *   The mandate is perceived differently by different stakeholders. Recycling companies see a solution, municipal taxpayers bear the cost, road builders are constrained, and the environmental integrity faces a risk.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) are derived from the structural positions of the agents. Plastic recycling companies and additive suppliers (beneficiaries) have a low d value, indicating a beneficial relationship. Taxpayers, road builders, and environmental integrity (victims) have a high d value, indicating a detrimental relationship. 
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate aims for coordination to solve a waste problem, but introduces significant extraction from taxpayers and may degrade the environment. The analytical observer and powerless perspective are crucial to detect whether this attempt to solve one problem is causing worse problems. The key is that it is not a true Pareto improvement, as victims bear the cost while select others benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plastic_quality_threshold,
    'What is the minimum quality threshold for recycled plastic used in asphalt to ensure durability and prevent microplastic release?',
    'Long-term testing of asphalt samples with varying plastic quality; environmental impact assessment of microplastic release rates.',
    'If the threshold is too low, the mandate becomes a net negative for the environment. If the threshold is too high, it makes the mandate economically unfeasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plastic_quality_threshold, empirical, 'Minimum quality threshold for recycled plastic in asphalt.').

omega_variable(
    economic_feasibility_vs_environmental_impact,
    'What is the optimal balance between the economic feasibility of plastic asphalt and its environmental impact compared to traditional asphalt?',
    'Full life-cycle cost analysis; comparative environmental impact assessment of plastic vs. traditional asphalt, including production, use, and disposal.',
    'If environmental benefits are marginal and costs are high, the mandate becomes a wealth transfer scheme. If costs are low but environmental damage is high, it undermines long-term sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_feasibility_vs_environmental_impact, conceptual, 'Economic vs Environmental trade-off.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plastic_asphalt_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plas_tr_t0, plastic_asphalt_mandate, theater_ratio, 0, 0.2).
narrative_ontology:measurement(plas_tr_t5, plastic_asphalt_mandate, theater_ratio, 5, 0.3).
narrative_ontology:measurement(plas_tr_t10, plastic_asphalt_mandate, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(plas_be_t0, plastic_asphalt_mandate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(plas_be_t5, plastic_asphalt_mandate, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(plas_be_t10, plastic_asphalt_mandate, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
