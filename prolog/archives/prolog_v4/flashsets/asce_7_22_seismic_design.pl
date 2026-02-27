% ============================================================================
% CONSTRAINT STORY: asce_7_22_seismic_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asce_7_22_seismic_design, []).

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
 *   constraint_id: asce_7_22_seismic_design
 *   human_readable: ASCE 7-22 Seismic Design Requirements
 *   domain: technological/legal
 *
 * SUMMARY:
 *   ASCE 7-22 provides minimum seismic design requirements for buildings in
 *   the US. While aimed at protecting life and property, it introduces costs
 *   and complexities that affect various stakeholders differently, acting as
 *   a tangled rope with coordination and extraction aspects. Building codes,
 *   in general, tend towards ratchet effects, where requirements are rarely
 *   relaxed. The complexity of the model creates a barrier to non-expert
 *   evaluation of true costs and benefits.
 *
 * KEY AGENTS:
 *   - Developers: Primary targets (moderate/constrained) - bear increased costs.
 *   - Building Owners: Primary targets (powerless/trapped) - face higher construction/renovation expenses.
 *   - Structural Engineers: Primary beneficiaries (institutional/arbitrage) - professional expertise in demand.
 *   - Construction Material Suppliers: Secondary beneficiaries (institutional/arbitrage) - increased sales of specialized materials.
 *   - Insurance Companies: Secondary beneficiaries (institutional/arbitrage) - reduced risk, lower payouts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asce_7_22_seismic_design, 0.5).
domain_priors:suppression_score(asce_7_22_seismic_design, 0.6).
domain_priors:theater_ratio(asce_7_22_seismic_design, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asce_7_22_seismic_design, extractiveness, 0.5).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asce_7_22_seismic_design, tangled_rope).
narrative_ontology:human_readable(asce_7_22_seismic_design, "ASCE 7-22 Seismic Design Requirements").
narrative_ontology:topic_domain(asce_7_22_seismic_design, "technological/legal").

domain_priors:requires_active_enforcement(asce_7_22_seismic_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, structural_engineers).
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, construction_material_suppliers).
narrative_ontology:constraint_beneficiary(asce_7_22_seismic_design, insurance_companies).
narrative_ontology:constraint_victim(asce_7_22_seismic_design, developers).
narrative_ontology:constraint_victim(asce_7_22_seismic_design, building_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: Small building owner faces increased construction costs with no exit option. They must comply to legally operate their building.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: Developers face increased costs but can pass some of them on to buyers or renters. They are constrained by market conditions but can choose to build in areas with less stringent requirements.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: Structural engineers benefit from increased demand for their services due to code requirements. They can arbitrage their expertise across different projects and locations.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: Construction material suppliers (e.g., steel, concrete) benefit from increased demand for their products, particularly specialized materials required for seismic resistance. They can arbitrage this demand by shifting supply to regions with stricter enforcement.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: Insurance companies benefit from reduced risk of payouts due to structural failures in seismic events, leading to potentially lower premiums and increased profitability. They can arbitrage this risk reduction across their portfolio.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: Analytical observer sees a tangled rope - necessary coordination to reduce seismic risk, but with asymmetric extraction impacting developers and building owners.
constraint_indexing:constraint_classification(asce_7_22_seismic_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asce_7_22_seismic_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asce_7_22_seismic_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asce_7_22_seismic_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.50) as there is a significant cost imposed by the code. Suppression (0.60) is moderate-high because building owners generally must comply to legally operate their buildings. The theater ratio (0.20) is low, as while there's bureaucratic overhead in complying, the code ultimately provides an actual function.
 *
 * PERSPECTIVAL GAP:
 *   Small building owners experience the code as a snare due to the disproportionate impact of cost increases. Developers see it as a tangled rope, balancing increased costs with market opportunities. Structural engineers, material suppliers, and insurance companies view the code as a coordination mechanism (rope) that benefits them. The analytical observer recognizes that the code serves to reduce seismic risk but also creates economic burdens, making it a tangled rope overall.
 *
 * DIRECTIONALITY LOGIC:
 *   Building owners are victims with no exit, experiencing pure extraction. Developers are constrained, experiencing a mix of extraction and benefit. Engineers and material suppliers benefit and can arbitrage their positions, experiencing the constraint as coordination. Insurers also benefit via reduced risk. This structural relationship drives the directionality score.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seismic_risk_assessment_accuracy,
    'How accurate are the seismic risk assessments used to determine design requirements?',
    'Compare predicted vs. actual damage from seismic events in areas with different code enforcement levels.',
    'If risk is overestimated, requirements are unnecessarily costly. If risk is underestimated, the code fails to provide adequate protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seismic_risk_assessment_accuracy, empirical, 'Accuracy of seismic risk assessments').

omega_variable(
    enforcement_consistency,
    'How consistently are the ASCE 7-22 seismic design requirements enforced across different jurisdictions?',
    'Audit building permit approval processes and conduct random inspections of construction sites.',
    'Inconsistent enforcement creates arbitrage opportunities for developers and undermines the code''s effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_consistency, empirical, 'Consistency of code enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asce_7_22_seismic_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asce_tr_t0, asce_7_22_seismic_design, theater_ratio, 0, 0.1).
narrative_ontology:measurement(asce_tr_t5, asce_7_22_seismic_design, theater_ratio, 5, 0.15).
narrative_ontology:measurement(asce_tr_t10, asce_7_22_seismic_design, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(asce_be_t0, asce_7_22_seismic_design, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(asce_be_t5, asce_7_22_seismic_design, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(asce_be_t10, asce_7_22_seismic_design, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asce_7_22_seismic_design, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
