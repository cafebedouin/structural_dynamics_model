% ============================================================================
% CONSTRAINT STORY: uk_hicbc_trap
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_uk_hicbc_trap, []).

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
 *   constraint_id: uk_hicbc_trap
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK's High Income Child Benefit Charge (HICBC) is a tax rule designed
 *   to claw back child benefit payments from households where one partner's
 *   adjusted net income exceeds a certain threshold (£60,000 as of 2024).
 *   The benefit is tapered away, disappearing entirely at £80,000. This
 *   creates a "trap" with an extremely high effective marginal tax rate for
 *   those in the taper zone, which has been exacerbated by fiscal drag
 *   (frozen thresholds). It also creates an anomaly where a single-earner
 *   household on £60,001 is penalised while a dual-earner household on
 *   £119,998 (£59,999 each) is not.
 *
 * KEY AGENTS (by structural relationship):
 *   - Single-Earner Households (£60k-£80k): Primary target (powerless/trapped) — bears the high marginal tax rate and administrative complexity.
 *   - HM Treasury: Primary beneficiary (institutional/arbitrage) — receives the tax revenue and designed the policy.
 *   - Policy Analyst: Analytical observer — sees the structure as a Tangled Rope, with a legitimate coordination goal (progressive taxation) but a highly extractive and inequitable implementation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_hicbc_trap, 0.55).
domain_priors:suppression_score(uk_hicbc_trap, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_hicbc_trap, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_hicbc_trap, extractiveness, 0.55).
narrative_ontology:constraint_metric(uk_hicbc_trap, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(uk_hicbc_trap, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_hicbc_trap, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_hicbc_trap). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_hicbc_trap, hm_treasury).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_hicbc_trap, single_earner_households_60k_to_80k).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE AFFECTED HOUSEHOLD (PRIMARY TARGET)
% This household is a victim and has few practical ways to avoid the charge
% without significant career/financial changes (trapped).
% The engine derives d ≈ 0.95, giving a high f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.0 = 0.78. This exceeds the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(uk_hicbc_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HM TREASURY (PRIMARY BENEFICIARY)
% As the institutional architect and beneficiary, the Treasury has full control
% over the policy (arbitrage).
% The engine derives d ≈ 0.05, giving a negative f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.0 = -0.066. This is Rope by definition (χ ≤ 0.35).
constraint_indexing:constraint_classification(uk_hicbc_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLICY ANALYST (ANALYTICAL OBSERVER)
% The analyst sees both the coordination function (tax collection) and the
% asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% Using global scope σ(S)=1.2 amplifies the perceived extraction.
% χ = 0.55 * 1.15 * 1.2 = 0.76. This is within the Tangled Rope range (0.40 ≤ χ ≤ 0.90),
% and all structural requirements (beneficiary, victim, enforcement) are met.
constraint_indexing:constraint_classification(uk_hicbc_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_hicbc_trap_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(uk_hicbc_trap, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_hicbc_trap, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Verify the analytical observer sees the hybrid nature.
    constraint_indexing:constraint_classification(uk_hicbc_trap, tangled_rope, context(agent_power(analytical), _, _, _)).

test(structural_requirements_for_tangled_rope_met) :-
    % The analytical claim is Tangled Rope, so its gates must be satisfied.
    narrative_ontology:constraint_beneficiary(uk_hicbc_trap, _),
    narrative_ontology:constraint_victim(uk_hicbc_trap, _),
    domain_priors:requires_active_enforcement(uk_hicbc_trap).

:- end_tests(uk_hicbc_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This score reflects the very high effective
 *     marginal tax rate imposed on the target group. For a family with two
 *     children, the rate can exceed 53% in the £60k-£80k income band, a
 *     significant extraction relative to other taxpayers.
 *   - Suppression (0.70): The complexity of the rule, the requirement for
 *     self-assessment, and the lack of easy alternatives for salaried employees
 *     create high suppression. Fiscal drag exacerbates this by pulling more
 *     unwitting people into the trap, suppressing the alternative of simply
 *     earning below the threshold.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For HM Treasury (institutional), the HICBC is a
 *   technical adjustment to the tax system to ensure progressivity, making it
 *   a pure coordination tool (Rope). For the affected household (powerless), it
 *   is a punitive and confusing penalty for earning slightly more, with no
 *   perceived benefit—a classic Snare. The analytical view recognizes both
 *   aspects, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `hm_treasury` directly benefits from the revenue collected.
 *     This declaration ensures the institutional perspective derives a low 'd'
 *     value, resulting in a low/negative effective extraction (χ).
 *   - Victim: `single_earner_households_60k_to_80k` are the specific group that
 *     bears the disproportionate cost. This ensures the powerless/trapped
 *     perspective derives a high 'd' value, leading to high χ and a Snare
 *     classification.
 *   The structure of the rule itself creates this directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the policy,
 *   preventing a simplistic mislabeling. Calling it a pure Snare would ignore
 *   its stated (and partially achieved) goal of making the benefit system
 *   more progressive. Calling it a pure Rope would ignore the "brutal" and
 *   inequitable extraction imposed on a specific demographic. The Tangled Rope
 *   classification, derived from the analytical perspective, correctly captures
 *   the reality: a coordination mechanism tangled with severe, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_uk_hicbc_trap,
    'Was the inequity between single-earner and dual-earner households an intended design feature to incentivize dual incomes, or an unintended consequence of simplifying the rule for administrative ease?',
    'Review of internal Treasury and HMRC policy design documents from its creation (circa 2012-2013).',
    'If intended, it suggests a more complex social engineering goal, slightly increasing the coordination aspect. If unintended, it confirms the structure is primarily a result of administrative path dependence and poor design.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(uk_hicbc_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), requiring temporal data.
% The primary drift mechanism is fiscal drag: as wages rise with inflation while
% the HICBC threshold remains frozen, the effective extractiveness of the policy
% increases over its lifecycle (2013-2024).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(uk_hicbc_trap_tr_t0, uk_hicbc_trap, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uk_hicbc_trap_tr_t5, uk_hicbc_trap, theater_ratio, 5, 0.15).
narrative_ontology:measurement(uk_hicbc_trap_tr_t10, uk_hicbc_trap, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation drift):
% T=0 (2013): Policy introduced. Fewer people affected.
narrative_ontology:measurement(uk_hicbc_trap_ex_t0, uk_hicbc_trap, base_extractiveness, 0, 0.40).
% T=5 (c. 2018): Fiscal drag begins to bite, pulling more people into the bracket.
narrative_ontology:measurement(uk_hicbc_trap_ex_t5, uk_hicbc_trap, base_extractiveness, 5, 0.48).
% T=10 (c. 2024): The trap is well-established and affects a much larger cohort.
narrative_ontology:measurement(uk_hicbc_trap_ex_t10, uk_hicbc_trap, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is part of a national tax system for managing public funds.
narrative_ontology:coordination_type(uk_hicbc_trap, resource_allocation).

% Network relationships (structural influence edges)
% The severity of the HICBC trap is directly exacerbated by the broader policy
% of freezing tax thresholds, known as fiscal drag.
narrative_ontology:affects_constraint(uk_fiscal_drag_policy, uk_hicbc_trap).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% based on the declared beneficiary (HM Treasury) and victim (households)
% groups, combined with their respective exit options (arbitrage vs. trapped),
% accurately models the directionality and produces the correct perspectival
% classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */