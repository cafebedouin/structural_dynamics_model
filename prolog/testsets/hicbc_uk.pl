% ============================================================================
% CONSTRAINT STORY: hicbc_uk
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_hicbc_uk, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hicbc_uk
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic
 *
 * SUMMARY:
 *   The High Income Child Benefit Charge (HICBC) is a UK tax rule that
 *   claws back Child Benefit payments from families where at least one
 *   partner earns over a set threshold (e.g., £60,000 as of 2024). This
 *   creates significant "cliff edge" effects and is perceived as unfair,
 *   as it is based on individual, not household, income. A single-parent
 *   household earning £61,000 loses the benefit, while a dual-income
 *   household earning £118,000 (£59k each) keeps it in full.
 *
 * KEY AGENTS (by structural relationship):
 *   - Higher-Earning Parents: Primary target (powerless/trapped) — bear the extraction by losing the benefit and facing complex tax compliance.
 *   - UK Treasury (HMRC): Primary beneficiary (institutional/arbitrage) — benefits from increased revenue and reduced welfare outlay.
 *   - Low-to-Middle Income Families: Implicit beneficiary (moderate/constrained) - the system of child benefit is maintained, and this clawback funds it in part.
 *   - Analytical Observer: Sees the full structure, including the coordination function of the underlying benefit and the extractive nature of the charge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hicbc_uk, 0.65).
domain_priors:suppression_score(hicbc_uk, 0.70).   % Structural property (raw, unscaled). High due to lack of viable alternatives for PAYE employees and penalties for non-compliance.
domain_priors:theater_ratio(hicbc_uk, 0.20).       % Piton detection (>= 0.70). Primarily a functional tool, not theatrical.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hicbc_uk, extractiveness, 0.65).
narrative_ontology:constraint_metric(hicbc_uk, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(hicbc_uk, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hicbc_uk, tangled_rope).
narrative_ontology:human_readable(hicbc_uk, "UK High Income Child Benefit Charge (HICBC)").
narrative_ontology:topic_domain(hicbc_uk, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(hicbc_uk). % Required for Tangled Rope. The charge is enforced by HMRC via the tax system.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hicbc_uk, uk_treasury).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hicbc_uk, higher_earning_parents).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement) -> NOT MET
%   Snare:        victim required; beneficiary optional -> MET

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
%   χ = 0.65 * 1.42 * 1.0 (national scope) = 0.923. Meets Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(hicbc_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
%   χ = 0.65 * -0.12 * 1.0 (national scope) = -0.078. Meets Rope threshold (χ ≤ 0.35).
constraint_indexing:constraint_classification(hicbc_uk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.65 * 1.15 * 1.2 (global scope) = 0.897. Meets Tangled Rope thresholds
% (0.40 ≤ χ ≤ 0.90, ε ≥ 0.30, suppression ≥ 0.40).
constraint_indexing:constraint_classification(hicbc_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hicbc_uk_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(hicbc_uk, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hicbc_uk, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hicbc_uk, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements) :-
    % Verify that all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(hicbc_uk, _),
    narrative_ontology:constraint_victim(hicbc_uk, _),
    domain_priors:requires_active_enforcement(hicbc_uk).

test(threshold_validation) :-
    % Base extraction is high, as expected for a Snare/Tangled Rope.
    domain_priors:base_extractiveness(hicbc_uk, E), E >= 0.46,
    domain_priors:suppression_score(hicbc_uk, S), S >= 0.60.

:- end_tests(hicbc_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): The charge can claw back 100% of the
 *     Child Benefit, representing a very direct and high level of extraction
 *     from the targeted group.
 *   - Suppression (0.70): High. For most affected families (especially PAYE
 *     employees), there are no easy alternatives to avoid the charge other
 *     than earning less or not claiming the benefit (which has negative
 *     knock-on effects like losing National Insurance credits). The policy
 *     actively suppresses a more equitable household-income-based alternative.
 *   - This combination of high extraction and high suppression, coupled with
 *     a genuine coordination function (the underlying Child Benefit system),
 *     makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - The gap is stark. From the perspective of a parent hit by the charge
 *     (powerless, trapped), it's a pure Snare. It feels like a punitive,
 *     arbitrary penalty for crossing an income line, with high compliance
 *     costs (self-assessment tax returns).
 *   - From the perspective of the UK Treasury (institutional, arbitrage), it's
 *     a Rope. It's a policy tool for managing the welfare budget, raising
 *     revenue, and making the system "fairer" by targeting benefits. The
 *     implementation details are secondary to its function as a coordination
 *     mechanism for public funds.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `uk_treasury`. They directly receive the clawed-back funds
 *     or save on welfare expenditure. This clear structural benefit gives them
 *     a low directionality score (d), resulting in a negative effective
 *     extraction (χ) from their perspective.
 *   - Victim: `higher_earning_parents`. They are the specific group targeted
 *     by the policy who bear the direct financial cost and administrative
 *     burden. This makes their directionality score (d) very high, resulting
 *     in a high χ and a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the policy.
 *   A simpler analysis might label the entire Child Benefit system a Snare,
 *   which would be incorrect as it serves millions as a pure coordination
 *   (Rope) mechanism. Conversely, labeling the HICBC a Rope would ignore
 *   the severe, targeted extraction it imposes. The Tangled Rope classification
 *   resolves this by acknowledging the system has both a coordination
 *   function (from the beneficiary's view) and a highly extractive function
 *   (from the victim's view), preventing mischaracterization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hicbc_uk,
    'Was the individual-income basis of the HICBC an intentional design to maximize extraction from single-earner households, or an unintended consequence of implementation simplicity?',
    'Release of internal Treasury and Cabinet Office policy design documents from its 2013 inception.',
    'If intentional, the constraint is closer to a pure Snare disguised as policy. If unintentional, it is a classic case of a poorly designed Tangled Rope where negative externalities were ignored for expediency.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hicbc_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the effect of "fiscal drag," where the fixed income
% threshold (frozen from 2013-2024) pulled more families into the charge over
% time due to wage inflation, intensifying the extraction.

% Theater ratio over time (declines as initial "fairness" justification wears thin):
narrative_ontology:measurement(hicbc_uk_tr_t0, hicbc_uk, theater_ratio, 0, 0.30).
narrative_ontology:measurement(hicbc_uk_tr_t5, hicbc_uk, theater_ratio, 5, 0.25).
narrative_ontology:measurement(hicbc_uk_tr_t10, hicbc_uk, theater_ratio, 10, 0.20).

% Extraction over time (increases due to fiscal drag):
narrative_ontology:measurement(hicbc_uk_ex_t0, hicbc_uk, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hicbc_uk_ex_t5, hicbc_uk, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(hicbc_uk_ex_t10, hicbc_uk, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The HICBC is part of a system for managing state funds.
narrative_ontology:coordination_type(hicbc_uk, resource_allocation).

% Network relationships (structural influence edges)
% Fiscal drag (the freezing of tax thresholds) makes the HICBC more extractive.
narrative_ontology:affects_constraint(uk_income_tax_fiscal_drag, hicbc_uk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the directionality for the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */