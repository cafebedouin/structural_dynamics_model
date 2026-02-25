% ============================================================================
% CONSTRAINT STORY: acip_hep_b_infant_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acip_hep_b_infant_mandate, []).

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
 *   constraint_id: acip_hep_b_infant_mandate
 *   human_readable: ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)
 *   domain: social/medical
 *
 * SUMMARY:
 *   In 1991, the CDC's ACIP recommended universal hepatitis B vaccination for
 *   all infants to combat perinatal transmission, which a targeted high-risk
 *   approach was failing to control. The policy was extraordinarily
 *   successful, reducing pediatric HBV cases by 99% over three decades.
 *   However, it mandated a medical procedure for all infants to protect a
 *   small, hard-to-identify minority. This created a structural tension
 *   between public health utilitarianism and individual medical autonomy. As
 *   maternal screening became highly reliable and incidence plummeted, the
 *   justification for the *universality* of the mandate weakened, leading to
 *   its reversal in 2025 in favor of a risk-based approach.
 *
 * KEY AGENTS:
 *   - Public Health System (ACIP/CDC): Primary beneficiary (institutional/arbitrage) — sees a successful coordination tool for disease eradication.
 *   - Parents Seeking Autonomy: Primary victim (powerless/trapped) — experiences a coercive mandate with high barriers to exit.
 *   - Infants of Undetected HBV+ Mothers: Silent beneficiary — protected from a 90% risk of chronic liver disease.
 *   - Pharmaceutical Manufacturers: Secondary beneficiary (institutional/arbitrage) — benefit from a stable, universal market.
 *   - Policy Reform Advocates: Organized agents (organized/mobile) — view the mandate as a temporary scaffold whose success justifies its own removal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acip_hep_b_infant_mandate, 0.48).
domain_priors:suppression_score(acip_hep_b_infant_mandate, 0.65).
domain_priors:theater_ratio(acip_hep_b_infant_mandate, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, extractiveness, 0.48).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acip_hep_b_infant_mandate, tangled_rope).
narrative_ontology:human_readable(acip_hep_b_infant_mandate, "ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)").
narrative_ontology:topic_domain(acip_hep_b_infant_mandate, "social/medical").

domain_priors:requires_active_enforcement(acip_hep_b_infant_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, public_health_system).
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, infants_of_undetected_hbv_positive_mothers).
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(acip_hep_b_infant_mandate, parents_seeking_autonomy).
narrative_ontology:constraint_victim(acip_hep_b_infant_mandate, infants_in_low_risk_families).
narrative_ontology:constraint_victim(acip_hep_b_infant_mandate, proponents_of_targeted_interventions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARENT (SNARE) — For a parent of a low-risk infant who desires medical autonomy, the mandate is coercive. Exit is blocked by school enrollment requirements and social pressure. The universal cost is borne with minimal perceived individual benefit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH (ROPE) — From the perspective of the CDC/ACIP, the mandate is a pure coordination mechanism. It solves the complex problem of identifying and protecting a vulnerable, hard-to-track population (infants of mothers with undiagnosed HBV), leading to a 99% reduction in pediatric cases. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL (TANGLED ROPE) — The observer sees both the immense coordination benefit (disease eradication) and the asymmetric extraction (universal cost for targeted benefit, suppression of alternatives). The policy is a hybrid, using coercion to achieve a public good. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM ADVOCATE (SCAFFOLD) — This perspective sees the universal mandate as a temporary measure whose success (high screening reliability, low incidence) created the conditions for its own obsolescence. The 2025 policy change acts as the sunset clause, transitioning to a more targeted approach. The original mandate was a scaffold to build a robust screening and prevention system.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SKEPTICAL PHYSICIAN (PITON) — By the 2020s, with highly reliable maternal screening, some physicians viewed the *universal* aspect as an inertial policy. Its primary function had been superseded by better technology (screening), but the rule persisted due to institutional momentum until it was actively dismantled. The theater is the performance of universal application when a targeted approach is sufficient.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acip_hep_b_infant_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acip_hep_b_infant_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acip_hep_b_infant_mandate, TR),
    TR >= 0.70.

:- end_tests(acip_hep_b_infant_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48): Represents the cost (financial, bodily autonomy, potential side-effect risk) imposed on the entire population of newborns, the vast majority of whom are at near-zero risk, to protect the few who would be missed by targeted screening. The value increased over time as screening improved, making the universal imposition less necessary. Suppression (0.65): High. For decades, compliance was enforced through school and daycare entry requirements, creating significant social and administrative barriers for non-compliant parents. Theater Ratio (0.30): Low to moderate. The policy was highly functional for most of its existence. The theater component grew only in its final years, as its universal nature became more of a legacy feature than a necessity due to improved screening.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For public health officials, the policy is a canonical Rope, a triumph of coordination that saved hundreds of thousands of children. For a parent of a low-risk child, it is a Snare, a coercive imposition of medical risk without consent or individual benefit. The Analytical view must hold both truths, classifying it as a Tangled Rope: a system that uses coercive, extractive means (the universal mandate) to achieve an undeniable coordination good (disease prevention). The 2025 reversal is seen as a sunset clause by reformers (Scaffold) but as the decay of a functional rule by others (Piton).
 *
 * DIRECTIONALITY LOGIC:
 *   The Public Health System and Pharmaceutical Manufacturers are beneficiaries with arbitrage, leading to a negative effective extraction (Rope). Parents are victims with trapped exit options, leading to maximum effective extraction (Snare). The Analytical observer's position balances these, resulting in the high positive extraction of a Tangled Rope. The measurement data reflects the lifecycle: extractiveness and theater both drift upward as the initial crisis recedes and the universal nature of the mandate becomes less critical and more coercive.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution of mandatrophy. Labeling the policy as simply a 'Rope' (as public health advocates would) ignores the coercive extraction felt by parents. Labeling it a 'Snare' (as critics would) ignores the massive, life-saving coordination benefit. The Tangled Rope classification is essential as it validates both perspectives, acknowledging that the system is simultaneously a coordination mechanism and an extractive one. The framework correctly identifies that the 'type' is not an intrinsic property but an indexical one, depending entirely on the observer's structural relationship to the costs and benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_calculus_shift,
    'At what point did the reliability of maternal screening and low incidence of perinatal transmission outweigh the public health risk of missed cases, justifying a shift from a universal to a targeted strategy?',
    'Quantitative risk analysis comparing the number of adverse events from universal vaccination against the number of preventable infections under a targeted-only strategy.',
    'An earlier crossover point supports the Piton/Snare perspectives, suggesting the policy was maintained past its optimal window. A later crossover point supports the Rope/Tangled Rope view, justifying its longevity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_calculus_shift, empirical, 'Crossover point where targeted screening risk became lower than universal mandate risk.').

omega_variable(
    political_vs_scientific_motive,
    'Was the 2025 ACIP policy change driven primarily by an updated scientific risk assessment or by the political ideology of new, vaccine-skeptical appointees?',
    'Analysis of ACIP meeting minutes, voting records, and comparison of the scientific evidence presented versus the final recommendation''s alignment with appointees'' prior public statements.',
    'If primarily political, it suggests the constraint''s lifecycle was terminated by external network influence rather than internal logic (scaffold sunset). If scientific, it confirms the Scaffold perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_vs_scientific_motive, empirical, 'Primary driver for the 2025 policy reversal: science or politics.').

omega_variable(
    cumulative_risk_validity,
    'Is the ''cumulative risk'' of combined birth-dose vaccines, cited as a reason for the policy change, a scientifically validated concern with empirical evidence, or a theoretical construct?',
    'Systematic review of epidemiological studies on health outcomes for infants receiving multiple birth-dose vaccines versus those on delayed schedules.',
    'If risk is validated, the extraction (ε) of the mandate is higher than estimated. If it is not, the rationale for the change is weaker, and the theater of the late-era policy debate was higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_risk_validity, empirical, 'Empirical validity of the ''cumulative risk'' argument against birth-dose vaccines.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acip_hep_b_infant_mandate, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acip_tr_t1991, acip_hep_b_infant_mandate, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(acip_tr_t2008, acip_hep_b_infant_mandate, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(acip_tr_t2025, acip_hep_b_infant_mandate, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(acip_be_t1991, acip_hep_b_infant_mandate, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement(acip_be_t2008, acip_hep_b_infant_mandate, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(acip_be_t2025, acip_hep_b_infant_mandate, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acip_hep_b_infant_mandate, resource_allocation).
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, future_vaccine_mandate_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
