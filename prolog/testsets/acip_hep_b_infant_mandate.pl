% ============================================================================
% CONSTRAINT STORY: acip_hep_b_infant_mandate
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acip_hep_b_infant_mandate
 *   human_readable: "ACIP Universal Hepatitis B Vaccination Mandate for Infants (1991-2025)"
 *   domain: social/medical
 *
 * SUMMARY:
 *   In 1991, the US CDC's Advisory Committee on Immunization Practices (ACIP)
 *   recommended universal hepatitis B vaccination for all infants, regardless of
 *   the mother's infection status. This policy became a de facto mandate in
 *   most US hospitals. As of 2025, with widespread maternal screening and
 *   lowered transmission risks, the ACIP has rescinded this recommendation for
 *   infants of non-infected mothers, acknowledging its function has become
 *   largely inertial and unnecessary for the majority population. This story models
 *   the constraint in its final, degraded state.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-Risk Infants & Parents: Primary target (powerless/trapped) — bear the cost and procedural burden of an unnecessary medical intervention.
 *   - Vaccine Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from the guaranteed, universal market created by the mandate.
 *   - Public Health System (ACIP): Analytical observer & former architect — initially created the rule as a coordination mechanism, now recognizes its degraded state.
 *   - Hospitals: Enforcers (institutional/constrained) — implement the policy, benefiting from standardization but bearing administrative load.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acip_hep_b_infant_mandate, 0.18).
domain_priors:suppression_score(acip_hep_b_infant_mandate, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(acip_hep_b_infant_mandate, 0.75).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, extractiveness, 0.18).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(acip_hep_b_infant_mandate, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(acip_hep_b_infant_mandate, piton).

% --- Binary flags ---
% N/A

% --- Emergence flag (required for mountain constraints) ---
% N/A

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(acip_hep_b_infant_mandate, vaccine_manufacturers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(acip_hep_b_infant_mandate, low_risk_infants_and_parents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (PARENTS OF LOW-RISK INFANTS)
% Experience an inertial, unnecessary, and hard-to-refuse procedure.
% With ε=0.18 and d derived from victim+trapped (d≈0.95), the effective
% extraction χ ≈ 0.18 * 1.42 * 1.0 = 0.255. This is low extraction.
% Combined with the high theater ratio (0.75), this classifies as a Piton.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (VACCINE MANUFACTURERS)
% Agent who benefits from a guaranteed, simplified market. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% The constraint appears as a pure coordination mechanism that stabilizes demand.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ACIP / PUBLIC HEALTH ANALYSTS)
% Recognizes that the original function has atrophied and the constraint
% now primarily exists due to institutional inertia. The high theater ratio
% is the key indicator, leading to a Piton classification.
constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acip_hep_b_infant_mandate_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the gap between parents (Piton) and manufacturers (Rope).
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == piton),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_is_piton) :-
    % Verify the analytical observer sees the constraint as a Piton.
    constraint_indexing:constraint_classification(acip_hep_b_infant_mandate, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == piton).

test(piton_threshold_validation) :-
    % Verify the metrics align with Piton classification criteria.
    domain_priors:theater_ratio(acip_hep_b_infant_mandate, TR),
    domain_priors:base_extractiveness(acip_hep_b_infant_mandate, E),
    assertion(TR >= 0.70),
    assertion(E =< 0.25).

:- end_tests(acip_hep_b_infant_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a classic case of institutional inertia, making it a
 *   canonical Piton. The scores reflect a policy whose original, valid public
 *   health function (high at inception in 1991) has atrophied over decades as
 *   the underlying risk profile of the population changed (e.g., better
 *   maternal screening).
 *   - Base Extractiveness (ε=0.18): Low. The cost is not primarily financial rent-seeking,
 *     but the aggregate of procedural overhead, parental consent friction, and the
 *     imposition of a medical procedure with non-zero (though small) risk on millions
 *     for whom it provides negligible benefit.
 *   - Suppression Score (0.55): Moderately high. While opt-outs exist in theory,
 *     the default protocol in a hospital setting makes refusal difficult for the
 *     average parent, effectively suppressing a risk-based alternative.
 *   - Theater Ratio (0.75): High. This is the defining feature. The act of universal
 *     vaccination for this specific group became more performative ("following the
 *     protocol") than functionally necessary. The ACIP's decision to rescind it is
 *     an explicit recognition of this high theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap is sharp and clear.
 *   - For parents of low-risk infants (the target), it's a Piton: a pointless,
 *     coercive bureaucratic hurdle.
 *   - For vaccine manufacturers (the beneficiary), it's a perfect Rope: a simple,
 *     elegant coordination mechanism that creates a stable, predictable market,
 *     eliminating the complexity of risk-based demand. They experience negative
 *     extraction (subsidy) from this market stability.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the flow of costs and benefits from the
 *   *inertial* state of the constraint. Vaccine manufacturers are declared
 *   beneficiaries because the universal mandate guarantees them a market far larger
 *   than a risk-adjusted one. Low-risk infants and their parents are victims
 *   because they bear the procedural and medical costs without a corresponding
 *   benefit. This structural relationship directly informs the directionality
 *   derivation, producing the observed perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This case perfectly illustrates "mandatrophy"—the decay of a mandate.
 *   Without the Piton classification, this might be mislabeled. It's not a Snare,
 *   as the extraction is too low. It's not a Rope, because for most participants
 *   it no longer serves a coordination function. It's not a Scaffold, as it
 *   lacked an explicit sunset clause and persisted through inertia. The Piton
 *   type, identified by the high theater ratio, correctly captures the nature of
 *   a once-functional rule that has outlived its purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_acip_hep_b_infant_mandate,
    'What is the true, quantified public health cost of moving from a universal to a risk-based strategy, accounting for potential failures in maternal screening or reporting?',
    'Longitudinal epidemiological studies comparing outcomes in populations under the old and new policies over a 10-year period.',
    'If the cost is higher than assumed, the constraint was less of a Piton and more of a low-efficiency Rope. If the cost is negligible or negative (due to avoided side effects), the Piton classification is strongly confirmed.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Represents the period from 1991 to 2025.
narrative_ontology:interval(acip_hep_b_infant_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is a prime example of lifecycle drift. The data models its
% decay from a functional coordination mechanism (Rope/Scaffold) into a Piton.
% T=0 represents ~1991, T=5 represents ~2008, T=10 represents ~2025.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(acip_hep_b_mandate_tr_t0, acip_hep_b_infant_mandate, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acip_hep_b_mandate_tr_t5, acip_hep_b_infant_mandate, theater_ratio, 5, 0.40).
narrative_ontology:measurement(acip_hep_b_mandate_tr_t10, acip_hep_b_infant_mandate, theater_ratio, 10, 0.75).

% Extraction over time (triggers extraction_accumulation detection):
% As the benefit declined, the net extraction (cost-for-benefit) increased.
narrative_ontology:measurement(acip_hep_b_mandate_ex_t0, acip_hep_b_infant_mandate, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(acip_hep_b_mandate_ex_t5, acip_hep_b_infant_mandate, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(acip_hep_b_mandate_ex_t10, acip_hep_b_infant_mandate, base_extractiveness, 10, 0.18).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Originally, the policy was a mechanism to allocate a public health resource.
narrative_ontology:coordination_type(acip_hep_b_infant_mandate, resource_allocation).

% Network relationships (structural influence edges)
% An unnecessary mandate can degrade public trust in the wider system.
narrative_ontology:affects_constraint(acip_hep_b_infant_mandate, public_trust_in_vaccines).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options correctly models the dynamics of this scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */