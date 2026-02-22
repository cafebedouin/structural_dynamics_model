% ============================================================================
% CONSTRAINT STORY: policy_implementation_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_policy_implementation_gap, []).

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
 *   constraint_id: policy_implementation_gap
 *   human_readable: Public Health Policy Implementation Gap
 *   domain: public_health/policy
 *
 * SUMMARY:
 *   This constraint models the persistent gap between established,
 *   evidence-based public health interventions (e.g., HPV vaccination,
 *   tobacco control, lead abatement) and their comprehensive and equitable
 *   implementation. The primary observable is the discrepancy between
 *   scientific consensus or WHO recommendations and the actual adoption,
 *   funding, and enforcement of these policies at national and sub-national
 *   levels, leading to preventable morbidity and mortality.
 *
 * KEY AGENTS:
 *   - Populations lacking access to prevention: Primary victims (powerless/trapped)
 *   - Status-quo healthcare systems: Primary beneficiaries (institutional/arbitrage)
 *   - Pharmaceutical industry (treatment focus): Secondary beneficiaries (institutional/arbitrage)
 *   - Underfunded public health agencies: Institutional victims (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(policy_implementation_gap, 0.48).
domain_priors:suppression_score(policy_implementation_gap, 0.62).
domain_priors:theater_ratio(policy_implementation_gap, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(policy_implementation_gap, extractiveness, 0.48).
narrative_ontology:constraint_metric(policy_implementation_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(policy_implementation_gap, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(policy_implementation_gap, tangled_rope).
narrative_ontology:human_readable(policy_implementation_gap, "Public Health Policy Implementation Gap").
narrative_ontology:topic_domain(policy_implementation_gap, "public_health/policy").

domain_priors:requires_active_enforcement(policy_implementation_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(policy_implementation_gap, status_quo_healthcare_systems).
narrative_ontology:constraint_beneficiary(policy_implementation_gap, pharmaceutical_industry_treatment_focus).
narrative_ontology:constraint_victim(policy_implementation_gap, populations_lacking_access_to_prevention).
narrative_ontology:constraint_victim(policy_implementation_gap, underfunded_public_health_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual in a high-risk, low-access population, the gap between known prevention and available care is a Snare. They bear the full cost of systemic failure with no exit.
constraint_indexing:constraint_classification(policy_implementation_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For established healthcare and pharmaceutical systems focused on treatment, the implementation gap is a Rope. It coordinates market behavior around profitable, high-margin treatments for preventable diseases, preserving revenue streams.
constraint_indexing:constraint_classification(policy_implementation_gap, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% For the public health body, the gap is a Tangled Rope. They participate in the coordination (setting guidelines) but are victims of the extraction (underfunding, political suppression) that prevents effective implementation.
constraint_indexing:constraint_classification(policy_implementation_gap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view sees the full structure: a coordination system (maintaining a stable healthcare market) that relies on asymmetric extraction (externalizing costs of preventable disease onto vulnerable populations).
constraint_indexing:constraint_classification(policy_implementation_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(policy_implementation_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(policy_implementation_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(policy_implementation_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(policy_implementation_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(policy_implementation_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) quantifies the societal cost (excess deaths, disability-adjusted life years, economic loss) imposed on victims by the failure to implement known solutions. The suppression score (0.62) reflects the high political and economic barriers, including lobbying by incumbent industries and targeted misinformation, that actively prevent the adoption of more effective preventative strategies.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for a citizen who contracts a preventable disease, the system is a coercive Snare. For an incumbent healthcare provider whose business model relies on treating that same disease, the system is a coordinating Rope that stabilizes their market. This difference is driven entirely by their structural position and directionality relative to the flow of costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (status_quo_healthcare_systems) profit from a market skewed towards expensive treatments over low-cost prevention. Their institutional power and arbitrage options give them a low directionality (d), resulting in a low or negative effective extraction (χ). Victims (populations_lacking_access) bear the direct health costs and have no exit, giving them a high directionality (d≈1.0) and thus experience a high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   A naive analysis might label this gap a pure Snare (malicious extraction) or a Piton (bureaucratic incompetence). The Tangled Rope classification is more accurate because it acknowledges the genuine, albeit perverse, coordination function: the current system effectively coordinates a massive, stable economic sector around disease treatment. This coordination is inextricably tangled with the extraction of health and wealth from the most vulnerable, a reality captured by the model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_inertia,
    'Is the implementation gap primarily driven by deliberate lobbying to protect incumbent profits, or is it an emergent property of bureaucratic inertia and political complexity?',
    'Comparative analysis of lobbying expenditures by treatment-focused industries versus public health advocacy budgets, correlated with the defeat of specific preventative policies.',
    'Resolution towards deliberate action would shift the analytical classification closer to a pure Snare. Resolution towards inertia reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_inertia, empirical, 'Distinguishing between deliberate profit-seeking and emergent bureaucratic failure as the primary driver of the policy gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(policy_implementation_gap, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poli_tr_t1990, policy_implementation_gap, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(poli_tr_t2005, policy_implementation_gap, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(poli_tr_t2024, policy_implementation_gap, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(poli_be_t1990, policy_implementation_gap, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(poli_be_t2005, policy_implementation_gap, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(poli_be_t2024, policy_implementation_gap, base_extractiveness, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(policy_implementation_gap, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is structurally downstream of 'uneven_risk_distribution' (a Mountain of epidemiology) and 'individual_vs_systemic_causation' (a Tangled Rope of political ideology). Those constraints create the preconditions for this implementation gap to form and persist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
