% ============================================================================
% CONSTRAINT STORY: uk_hicbc_2024
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_uk_hicbc_2024, []).

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
 *   constraint_id: uk_hicbc_2024
 *   human_readable: UK High Income Child Benefit Charge (HICBC)
 *   domain: economic
 *
 * SUMMARY:
 *   The High Income Child Benefit Charge (HICBC) is a UK tax policy that claws
 *   back Child Benefit payments from families where at least one partner earns
 *   over a certain threshold (£60,000 as of 2024). The clawback is tapered, with
 *   the full benefit being repaid once income reaches £80,000. This creates a
 *   structural asymmetry: a household with a single earner at £80,001 loses the
 *   entire benefit, while a household with two earners at £60,000 each (total
 *   £120,000) keeps the full benefit. This results in extremely high effective
 *   marginal tax rates for the affected group.
 *
 * KEY AGENTS (by structural relationship):
 *   - Single-earner high-income families: Primary target (powerless/trapped) — bears the punitive marginal tax rates.
 *   - The UK Treasury (HMRC): Primary beneficiary (institutional/arbitrage) — receives the clawed-back revenue.
 *   - Dual-earner high-income families: Indirect beneficiary (moderate/mobile) — unaffected by the charge, thus facing a relatively lower tax burden at similar household incomes.
 *   - Policy Analyst: Analytical observer — sees the full structure of coordination (revenue) and asymmetric extraction (punitive rates).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_hicbc_2024, 0.65).
domain_priors:suppression_score(uk_hicbc_2024, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(uk_hicbc_2024, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_hicbc_2024, extractiveness, 0.65).
narrative_ontology:constraint_metric(uk_hicbc_2024, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(uk_hicbc_2024, theater_ratio, 0.10).

% --- NL Profile Metrics --- N/A. This is a human-designed policy.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(uk_hicbc_2024, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(uk_hicbc_2024). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(uk_hicbc_2024, uk_treasury).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(uk_hicbc_2024, single_earner_high_income_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This produces χ = 0.65 * 1.42 * 1.0 (national scope) ≈ 0.92, well into snare territory.
constraint_indexing:constraint_classification(uk_hicbc_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% The constraint is a pure revenue-coordination tool for the Treasury.
constraint_indexing:constraint_classification(uk_hicbc_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the coordination function
% (revenue) and the asymmetric extraction. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% This produces χ = 0.65 * 1.15 * 1.2 (global scope) ≈ 0.90, which matches Tangled Rope.
constraint_indexing:constraint_classification(uk_hicbc_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_hicbc_2024_tests).

test(perspectival_gap) :-
    % Verify the gap between the targeted family (Snare) and Treasury (Rope).
    constraint_indexing:constraint_classification(uk_hicbc_2024, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(uk_hicbc_2024, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(uk_hicbc_2024, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_validation) :-
    % Verify that all three conditions for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(uk_hicbc_2024),
    narrative_ontology:constraint_beneficiary(uk_hicbc_2024, _),
    narrative_ontology:constraint_victim(uk_hicbc_2024, _).

:- end_tests(uk_hicbc_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High, reflecting the creation of punitive marginal
 *     tax rates that can exceed 50% or even 100% in the clawback income band.
 *   - Suppression (0.70): High. The charge is mandatory and enforced by HMRC. While
 *     some avoidance is possible (pension contributions), exit is highly constrained
 *     for those within the income bracket. Opting out of Child Benefit entirely has
 *     negative knock-on effects for state pension credits.
 *   - Theater Ratio (0.10): Low. This is a functional tax policy designed to raise
 *     revenue, not a performative act.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a single-earner family earning £60,001, the policy is a
 *   punitive trap that extracts a significant portion of their income or benefit,
 *   making it a Snare. For the UK Treasury, it's an effective mechanism for raising
 *   revenue from higher earners to fund public services, a pure coordination
 *   function (Rope). The analytical view recognizes both functions co-exist, hence
 *   Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value from a narrowly defined group (`single_earner_high_income_families`)
 *   to the state (`uk_treasury`). The `victim` declaration captures the group
 *   bearing the asymmetric cost, leading to a high directionality `d` and high effective
 *   extraction `χ` for them. The `beneficiary` declaration captures the institution
 *   receiving the funds, leading to a low `d` and negative `χ`.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of where the Deferential Realism framework
 *   prevents misclassification. A purely libertarian analysis might label all taxes
 *   Snares, ignoring their coordination function. A purely statist analysis might
 *   label it a Rope, ignoring the disproportionate and punitive extraction imposed
 *   on a specific, structurally disadvantaged group (in this tax context). The Tangled
 *   Rope classification is crucial because it correctly identifies that a legitimate
 *   coordination function (funding government) is being implemented via a highly
 *   extractive and asymmetric mechanism. It separates the "what" (revenue) from the
 *   "how" (punitive marginal rates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_uk_hicbc_2024,
    'Was the asymmetric impact on single-earner households a deliberate policy choice to favor dual-earner households, or an unintended consequence of using individual income as a simple administrative shortcut?',
    'Review of internal Treasury and Cabinet Office policy documents from the design phase of the HICBC (circa 2012).',
    'If deliberate, the constraint has a stronger extractive/social engineering intent. If accidental, it represents a failure of policy design and foresight.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(uk_hicbc_2024, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from its introduction in 2013 (T=0)
% to the state after the 2024 budget changes (T=11). The high extraction (ε > 0.46)
% makes this tracking mandatory.

% Theater ratio over time (stable, as the function has not changed):
narrative_ontology:measurement(uk_hicbc_2024_tr_t0, uk_hicbc_2024, theater_ratio, 0, 0.10).
narrative_ontology:measurement(uk_hicbc_2024_tr_t5, uk_hicbc_2024, theater_ratio, 5, 0.10).
narrative_ontology:measurement(uk_hicbc_2024_tr_t11, uk_hicbc_2024, theater_ratio, 11, 0.10).

% Extraction over time: Started high, worsened due to fiscal drag (thresholds not rising
% with inflation), then slightly ameliorated by the 2024 threshold increase.
narrative_ontology:measurement(uk_hicbc_2024_ex_t0, uk_hicbc_2024, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(uk_hicbc_2024_ex_t9, uk_hicbc_2024, base_extractiveness, 9, 0.70).
narrative_ontology:measurement(uk_hicbc_2024_ex_t11, uk_hicbc_2024, base_extractiveness, 11, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The HICBC is a mechanism for funding public services.
narrative_ontology:coordination_type(uk_hicbc_2024, resource_allocation).

% Network relationships: This policy is coupled with the broader tax system.
% Fiscal drag (the failure of tax thresholds to keep pace with inflation)
% significantly intensified the extractive nature of the HICBC over time.
narrative_ontology:affects_constraint(uk_fiscal_drag, uk_hicbc_2024).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain, using the declared beneficiary/victim groups and their exit options,
% accurately models the directionality of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */