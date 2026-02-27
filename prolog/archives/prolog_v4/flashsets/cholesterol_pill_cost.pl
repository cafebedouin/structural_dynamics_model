% ============================================================================
% CONSTRAINT STORY: cholesterol_pill_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cholesterol_pill_cost, []).

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
 *   constraint_id: cholesterol_pill_cost
 *   human_readable: Cost of Patented Cholesterol Medication
 *   domain: economic
 *
 * SUMMARY:
 *   The high cost of patented cholesterol medication creates a tension
 *   between incentivizing pharmaceutical innovation and ensuring affordable
 *   access to essential medicines. The patent system, while intended to
 *   promote innovation, can lead to monopolistic pricing and limited access,
 *   particularly for vulnerable populations. The lack of generic alternatives
 *   and the complexities of drug pricing further exacerbate the problem,
 *   making it a snare for patients and taxpayers.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Company: Primary beneficiary (institutional/arbitrage) - benefits from patent protection and high prices
 *   - Patients: Primary victim (powerless/trapped) - bear the cost of high prices with limited alternatives
 *   - Taxpayers: Secondary victim (moderate/constrained) - indirectly bear the cost through government healthcare programs
 *   - Uninsured Individuals: Severely impacted (powerless/trapped) - Face exorbitant costs with almost no access
 *   - Government Healthcare Systems: Tangled between cost and patient access (organized/constrained)
 *   - Shareholders: Benefit greatly from high pricing strategies (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cholesterol_pill_cost, 0.75).
domain_priors:suppression_score(cholesterol_pill_cost, 0.8).
domain_priors:theater_ratio(cholesterol_pill_cost, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cholesterol_pill_cost, extractiveness, 0.75).
narrative_ontology:constraint_metric(cholesterol_pill_cost, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(cholesterol_pill_cost, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cholesterol_pill_cost, snare).
narrative_ontology:human_readable(cholesterol_pill_cost, "Cost of Patented Cholesterol Medication").
narrative_ontology:topic_domain(cholesterol_pill_cost, "economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cholesterol_pill_cost, pharmaceutical_company).
narrative_ontology:constraint_beneficiary(cholesterol_pill_cost, shareholders).
narrative_ontology:constraint_victim(cholesterol_pill_cost, patients).
narrative_ontology:constraint_victim(cholesterol_pill_cost, taxpayers).
narrative_ontology:constraint_victim(cholesterol_pill_cost, uninsured_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients with high cholesterol and limited financial resources are trapped by the high cost of the medication and have limited exit options due to health needs and lack of alternatives. They experience the constraint as a snare.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The pharmaceutical company benefits from the patent protection and high pricing, enabling them to recoup R&D costs and generate profit. From their perspective, the patent system and pricing strategy act as a rope, facilitating coordination and investment.
constraint_indexing:constraint_classification(cholesterol_pill_cost, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Insured individuals are constrained by insurance premiums and co-pays, but also benefit from access to the medication. They experience the constraint as a tangled rope, with both benefits and extraction.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Government healthcare systems (e.g., Medicare, NHS) are constrained by the high cost of the drug and the need to provide access to patients, but also benefit from improved patient health outcomes. They experience the constraint as a tangled rope, balancing costs and benefits.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The patent system, while intended to incentivize innovation, can become a piton when it allows for excessive pricing and limited access, hindering overall societal welfare. The original coordination function has atrophied.
constraint_indexing:constraint_classification(cholesterol_pill_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the cost of patented cholesterol medication is a tangled rope. It balances the need to incentivize pharmaceutical innovation with the need to ensure affordable access to essential medicines, reflecting asymmetric extraction.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cholesterol_pill_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cholesterol_pill_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cholesterol_pill_cost, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cholesterol_pill_cost, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cholesterol_pill_cost, TR),
    TR >= 0.70.

:- end_tests(cholesterol_pill_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): The high price of the patented medication extracts significant value from patients and taxpayers, particularly those with limited financial resources and those needing the medication. Suppression (0.80): The patent protection and lack of generic alternatives suppress competition and limit access to affordable options. Theater Ratio (0.30): There is limited performative action, and more functional action, as the company does continue researching and making new drugs.
 *
 * PERSPECTIVAL GAP:
 *   Patients see a snare as they are trapped by the high cost. Pharmaceutical companies see a rope, where the patent protection and pricing helps them to continue creating drugs. The government is in the middle as a tangled rope between patient access and cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharmaceutical company benefits from the patent protection and pricing, while patients bear the costs. The government is in a constrained position, balancing the need to provide access with the financial burden of high drug prices.
 *
 * MANDATROPHY ANALYSIS:
 *   While the patent system is intended to incentivize innovation, in this case, the high cost of the medication suggests a potential imbalance, where the extraction outweighs the coordination benefits for society as a whole. Resolved by identifying patients as trapped, while pharma companies have arbitrage, indicating a transfer of wealth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patent_term_vs_innovation,
    'What is the optimal patent term that balances incentivizing innovation with ensuring timely access to affordable medicines?',
    'Economic modeling and empirical studies on the impact of patent term length on pharmaceutical R&D investment and drug prices.',
    'If patent term is too long: delayed access and higher healthcare costs. If patent term is too short: reduced incentive for pharmaceutical innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_term_vs_innovation, empirical, 'Optimal patent term balancing innovation and access').

omega_variable(
    pricing_regulation_impact,
    'What is the impact of government price regulation on pharmaceutical R&D investment and drug availability?',
    'Comparative analysis of pharmaceutical innovation and drug prices in countries with and without price regulation.',
    'If price regulation is too strict: reduced pharmaceutical R&D investment and drug availability. If price regulation is too weak: excessive drug prices and limited access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_regulation_impact, empirical, 'Impact of government price regulation on pharmaceutical R&D').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cholesterol_pill_cost, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chol_tr_t0, cholesterol_pill_cost, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chol_tr_t5, cholesterol_pill_cost, theater_ratio, 5, 0.25).
narrative_ontology:measurement(chol_tr_t10, cholesterol_pill_cost, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(chol_be_t0, cholesterol_pill_cost, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(chol_be_t5, cholesterol_pill_cost, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(chol_be_t10, cholesterol_pill_cost, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cholesterol_pill_cost, resource_allocation).
narrative_ontology:affects_constraint(cholesterol_pill_cost, drug_patent_system).
narrative_ontology:affects_constraint(cholesterol_pill_cost, healthcare_access_disparities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
