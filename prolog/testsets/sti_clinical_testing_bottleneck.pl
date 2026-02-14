% ============================================================================
% CONSTRAINT STORY: sti_clinical_testing_bottleneck
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_sti_clinical_testing_bottleneck, []).

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
 *   constraint_id: sti_clinical_testing_bottleneck
 *   human_readable: Requirement for Clinical Lab Testing for Chlamydia/Gonorrhea
 *   domain: technological
 *
 * SUMMARY:
 *   The traditional system for diagnosing common STIs like chlamydia and gonorrhea
 *   requires a visit to a clinician, sample collection, lab processing, and a
 *   multi-day waiting period for results. This process creates a significant
 *   bottleneck, delaying treatment, increasing patient anxiety, and allowing
 *   for continued transmission. The constraint is the mandatory channeling of
 *   all diagnostics through this centralized, high-friction process, which is
 *   now being challenged by technologies like at-home testing.
 *
 * KEY AGENTS (by structural relationship):
 *   - individuals_at_risk: Primary target (powerless/trapped) — bears the costs of delayed diagnosis, health risks, and anxiety.
 *   - clinical_labs_and_incumbent_providers: Primary beneficiary (institutional/arbitrage) — benefits from the revenue and structural centrality of the lab-based testing monopoly.
 *   - fda_regulators: Mediating institution (institutional/constrained) — enforces the safety and accuracy standards that create the bottleneck, while also being the gatekeeper for solutions that bypass it.
 *   - public_health_analysts: Analytical observer — sees the full structure of the public health problem, including the trade-offs between centralized accuracy and decentralized access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sti_clinical_testing_bottleneck, 0.50).
domain_priors:suppression_score(sti_clinical_testing_bottleneck, 0.75).   % Structural property (raw, unscaled). High, as alternatives were historically unavailable.
domain_priors:theater_ratio(sti_clinical_testing_bottleneck, 0.15).       % Piton detection (>= 0.70). Low, the system is functional, just extractive.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, extractiveness, 0.50).
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sti_clinical_testing_bottleneck, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(sti_clinical_testing_bottleneck). % Required for Tangled Rope. Enforced by medical regulations and licensing.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-constructed system.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sti_clinical_testing_bottleneck, clinical_labs_and_incumbent_providers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, individuals_at_risk).
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, public_health_systems). % Bears cost of continued transmission

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For an individual at risk, the system is highly extractive and coercive.
% Engine derives d from victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.50 * 1.42 * 1.0 (national) = 0.71. This exceeds the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For labs, the system is a pure coordination mechanism for diagnostic workflow and revenue.
% Engine derives d from beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ = 0.50 * -0.12 * 1.0 = -0.06. This is well within Rope range (χ ≤ 0.35).
% NOTE: This classification passes on χ but FAILS on base extraction ε (0.50 > 0.45).
% This is a "high-ε Rope", a structural contradiction the model highlights. See commentary.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A public health analyst sees both the coordination function (ensuring accuracy)
% and the severe asymmetric extraction (delayed care, public health costs).
% Engine derives analytical d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.50 * 1.15 * 1.2 (global) = 0.69. This is in Tangled Rope range [0.40, 0.90].
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE REGULATOR (TANGLED ROPE)
% The FDA experiences the constraint as a complex trade-off space. They are not direct
% beneficiaries but are constrained by their mandate for safety, which upholds the system.
% Their exit is constrained, and they are structurally between beneficiaries and victims.
% We model them as `both` + `constrained`, giving a derived d ≈ 0.6 → f(d) ≈ 0.88.
% χ = 0.50 * 0.88 * 1.0 = 0.44. This is a Tangled Rope.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sti_clinical_testing_bottleneck_tests).

test(perspectival_gap_victim_beneficiary) :-
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_victim_analyst) :-
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    assertion(TypeTarget == snare),
    assertion(TypeAnalyst == tangled_rope),
    TypeTarget \= TypeAnalyst.

test(tangled_rope_structural_gates_pass) :-
    % A constraint must have all three to be a Tangled Rope.
    narrative_ontology:constraint_beneficiary(sti_clinical_testing_bottleneck, _), % Has coordination
    narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, _), % Has asymmetric extraction
    domain_priors:requires_active_enforcement(sti_clinical_testing_bottleneck).

test(beneficiary_rope_epsilon_conflict, [fail]) :-
    % This test is DESIGNED TO FAIL to programmatically document the structural paradox.
    % The beneficiary classifies it as Rope, but the constraint's base extractiveness (ε)
    % is higher than the maximum allowed for a Rope classification (ε ≤ 0.45).
    % This highlights that beneficiaries can perceive a Rope even when the underlying
    % structure is too extractive to be one.
    config:param(rope_base_ext_max, RopeEpsilonMax),
    domain_priors:base_extractiveness(sti_clinical_testing_bottleneck, Epsilon),
    Epsilon =< RopeEpsilonMax.

:- end_tests(sti_clinical_testing_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.50): Set to be high enough to trigger a Snare classification for the victim (χ > 0.66) but not so high that the analytical perspective also becomes a Snare. This value represents significant costs in time, health risk, and anxiety borne by patients.
 *   - Suppression (0.75): High, reflecting the historical lack of any alternative to the clinical lab monopoly for certified diagnostics.
 *   - Theater (0.15): Low. The system, while inefficient and extractive, is not performative; it delivers a real diagnostic result.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the 'individuals_at_risk' (powerless, trapped), the system is a Snare (χ=0.71), a coercive trap that extracts health and well-being. For the 'clinical_labs' (institutional, arbitrage), it is a Rope (χ=-0.06), a smooth, profitable coordination mechanism. The analytical observer and regulator split the difference, classifying it as a Tangled Rope (χ=0.69 and χ=0.44 respectively), acknowledging both the coordination function and the severe asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'clinical_labs_and_incumbent_providers' directly profit from the testing volume and the barriers to entry that suppress competition. Their `arbitrage` exit option and beneficiary status yield a low `d`, resulting in negative effective extraction (χ).
 *   - Victims: 'individuals_at_risk' bear all the costs. Their `trapped` exit status and victim status yield a high `d`, resulting in very high effective extraction (χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a prime example of preventing Mandatrophy. A naive analysis might label the entire system a "public health coordination system" (Rope). The indexical classification reveals this is only true from the beneficiary's highly privileged perspective. For everyone else, it is a Tangled Rope or an outright Snare.
 *   Crucially, the framework flags a structural contradiction in the beneficiary's 'Rope' classification. While their effective extraction (χ) is low, the constraint's intrinsic extractiveness (ε=0.50) is too high for a pure Rope (ε ≤ 0.45). The system correctly identifies that they are benefiting from a mechanism that is fundamentally too extractive to be considered pure coordination. This prevents the beneficiary's narrative from masking the underlying reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sti_bottleneck,
    'Is the testing bottleneck an intentional market-protection Snare designed by incumbents, or an emergent Tangled Rope resulting from uncoordinated, path-dependent regulations focused on safety?',
    'Analysis of historical regulatory decisions and lobbying efforts by lab providers.',
    'If intentional, the constraint is a pure Snare with a theatrical coordination element. If emergent, it is a classic Tangled Rope where good intentions (accuracy) created severe negative externalities.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sti_clinical_testing_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint intensifying over time, as administrative
% and insurance complexities added to the initial clinical friction.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(sti_tr_t0, sti_clinical_testing_bottleneck, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sti_tr_t5, sti_clinical_testing_bottleneck, theater_ratio, 5, 0.12).
narrative_ontology:measurement(sti_tr_t10, sti_clinical_testing_bottleneck, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(sti_ex_t0, sti_clinical_testing_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sti_ex_t5, sti_clinical_testing_bottleneck, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(sti_ex_t10, sti_clinical_testing_bottleneck, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system allocates diagnostic capacity.
narrative_ontology:coordination_type(sti_clinical_testing_bottleneck, resource_allocation).

% Network relationships (structural influence edges)
% The bottleneck directly causes negative health outcomes.
narrative_ontology:affects_constraint(sti_clinical_testing_bottleneck, delayed_sti_treatment_outcomes).
% It is also upheld by other constraints in the medical system.
narrative_ontology:affects_constraint(physician_prescription_monopoly, sti_clinical_testing_bottleneck).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */