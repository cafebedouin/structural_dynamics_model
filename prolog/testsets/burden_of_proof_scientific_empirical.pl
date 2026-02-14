% ============================================================================
% CONSTRAINT STORY: burden_of_proof_scientific
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_burden_of_proof_scientific, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burden_of_proof_scientific
 *   human_readable: Statistical Significance Threshold (p < 0.05)
 *   domain: technological/social
 *
 * SUMMARY:
 *   In the empirical sciences, the burden of proof is codified through
 *   statistical significance, typically requiring a p-value of less than 0.05
 *   to reject the null hypothesis. This constraint is designed to filter out
 *   random noise and ensure that claims are reproducible. However, its rigid
 *   application creates perverse incentives, such as p-hacking and the
 *   suppression of null results (the "file drawer problem"), leading to the
 *   replication crisis.
 *
 * KEY AGENTS (by structural relationship):
 *   - Early Career Researchers: Primary target (powerless/trapped) — must achieve the threshold to publish and secure a career.
 *   - Journal Editors & Funding Agencies: Primary beneficiary (institutional/arbitrage) — enforce the standard, which provides a simple heuristic for quality and maintains institutional prestige.
 *   - Meta-Analysts: Analytical observer — sees the full system dynamics, including perverse incentives and aggregate failures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_scientific, 0.48).
domain_priors:suppression_score(burden_of_proof_scientific, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(burden_of_proof_scientific, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_scientific, extractiveness, 0.48).
narrative_ontology:constraint_metric(burden_of_proof_scientific, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(burden_of_proof_scientific, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(burden_of_proof_scientific, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(burden_of_proof_scientific). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific, established_journals).
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific, research_funding_agencies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(burden_of_proof_scientific, early_career_researchers).
narrative_ontology:constraint_victim(burden_of_proof_scientific, novel_high_risk_hypotheses).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
constraint_indexing:constraint_classification(burden_of_proof_scientific, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(burden_of_proof_scientific, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(burden_of_proof_scientific, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_scientific_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(burden_of_proof_scientific, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_scientific, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_classification_is_snare) :-
    constraint_indexing:constraint_classification(burden_of_proof_scientific, snare,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation_snare) :-
    narrative_ontology:constraint_metric(burden_of_proof_scientific, extractiveness, E),
    narrative_ontology:constraint_metric(burden_of_proof_scientific, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(burden_of_proof_scientific_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file classified this as a Mountain, but its metrics (ε=0.3, S=0.6)
 *   violated the Mountain thresholds (ε≤0.25, S≤0.05). The constraint is a social
 *   construct, not a natural law. I have re-evaluated it as a Snare that retains
 *   a coordination function, which is why it appears as a Rope to beneficiaries.
 *   - Base Extractiveness (ε=0.48): Represents the significant waste of resources
 *     on studies that are not published due to null results, and the intellectual
 *     distortion caused by p-hacking. This value is high enough to trigger Snare
 *     classification from relevant perspectives.
 *   - Suppression (S=0.65): Alternatives like Bayesian inference or a focus on
 *     effect sizes are well-known but are actively suppressed by institutional
 *     inertia, journal standards, and reviewer expectations.
 *
 * PERSPECTIVAL GAP:
 *   - The Early Career Researcher (powerless/trapped) experiences this as a Snare.
 *     Their career depends on satisfying a rigid, extractive rule with no viable
 *     alternatives. While they may feel it is a "Mountain" due to its immovability,
 *     structurally it is a man-made trap.
 *   - Journal Editors (institutional/arbitrage) experience it as a Rope. It is a
 *     highly effective, low-cost coordination tool for filtering submissions and
 *     maintaining a shared standard of evidence, from which they derive prestige
 *     and operational efficiency. The extractive costs are externalized to researchers.
 *   - The Meta-Analyst (analytical) sees the full picture: a system with a valid
 *     coordination goal that has become a Snare due to its perverse incentives,
 *     high suppression, and extractive side effects (the replication crisis).
 *
 * MANDATROPHY ANALYSIS:
 *   This re-classification resolves a potential Mandatrophy error. By labeling the
 *   constraint a Snare, we correctly identify the extractive harm (suppression of
 *   truth via the file-drawer problem) that is masked by its stated goal of
 *   coordination (Rope function). It has both a coordination function (beneficiary
 *   is declared) and asymmetric extraction (victim is declared), making it a
 *   classic Tangled Rope that has degraded into a Snare from most perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_burden_of_proof_scientific,
    "Is the 0.05 threshold an arbitrary historical artifact (Piton-like) or does it represent a near-optimal tradeoff between Type I and Type II errors for scientific progress?",
    "Simulate scientific progress across various alpha-level thresholds (0.01, 0.05, 0.10) under different publication bias models.",
    "If arbitrary, the constraint is a pure Snare. If optimal, it is a necessary Tangled Rope where the extraction is a feature, not a bug.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(burden_of_proof_scientific, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint degrading from a functional Rope to a Snare.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (p-hacking as performative science):
narrative_ontology:measurement(bops_tr_t0, burden_of_proof_scientific, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bops_tr_t5, burden_of_proof_scientific, theater_ratio, 5, 0.10).
narrative_ontology:measurement(bops_tr_t10, burden_of_proof_scientific, theater_ratio, 10, 0.15).

% Extraction over time (increasing pressure to publish drives waste):
narrative_ontology:measurement(bops_ex_t0, burden_of_proof_scientific, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(bops_ex_t5, burden_of_proof_scientific, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(bops_ex_t10, burden_of_proof_scientific, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(burden_of_proof_scientific, information_standard).

% Network relationships (structural influence edges)
narrative_ontology:affects_constraint(burden_of_proof_scientific, academic_publishing_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality for
% the key agents in this system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */