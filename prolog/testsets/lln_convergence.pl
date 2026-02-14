% ============================================================================
% CONSTRAINT STORY: lln_convergence
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_lln_convergence, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lln_convergence
 *   human_readable: Law of Large Numbers (LLN)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Law of Large Numbers (LLN) is a theorem stating that the average of
 *   results from a large number of independent, identical trials will converge
 *   to the expected value. This constraint models the mathematical law itself,
 *   not its specific applications in extractive systems (e.g., casino house edge),
 *   which would constitute a separate, higher-extraction constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Individual Trial: Primary subject (powerless/trapped) — whose variance is smoothed away by the aggregate.
 *   - Casino Operators / Insurance Firms: Primary beneficiary (institutional/arbitrage) — uses the LLN to ensure predictable profitability.
 *   - Gamblers with Finite Capital: Victim (powerless/constrained) — experiences the law's convergence as a ruinous force due to finite resources.
 *   - Analytical Observer: Sees the full mathematical structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lln_convergence, 0.25).
domain_priors:suppression_score(lln_convergence, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(lln_convergence, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lln_convergence, extractiveness, 0.25).
narrative_ontology:constraint_metric(lln_convergence, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(lln_convergence, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(lln_convergence, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(lln_convergence, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lln_convergence, mountain).

% --- Binary flags ---
% No active enforcement needed for a mathematical theorem.

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(lln_convergence).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain. While the claim is mountain,
% the law is experienced differently by various agents, justifying these declarations.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(lln_convergence, insurance_industry).
narrative_ontology:constraint_beneficiary(lln_convergence, casino_operators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(lln_convergence, gamblers_with_finite_capital).

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

% PERSPECTIVE 1: THE SINGLE TRIAL (MOUNTAIN)
% For an individual event (e.g., one coin flip), the law is an unchangeable
% feature of the mathematical space it inhabits. It has no agency.
constraint_indexing:constraint_classification(lln_convergence, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL BENEFICIARY (ROPE)
% For an actuary or casino, the LLN is a pure coordination tool. It allows
% the conversion of individual chaos into institutional order and profit.
% χ = 0.25 * f(d≈0.05) * σ(1.2) ≈ 0.25 * -0.12 * 1.2 ≈ -0.036 (negative extraction)
constraint_indexing:constraint_classification(lln_convergence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE FINITE-CAPITAL GAMBLER (ROPE)
% For a gambler with limited funds, the convergence guarantees ruin. While the
% outcome is snare-like, the underlying law's low extraction (ε=0.25) classifies
% it as a Rope. The "snare" is an emergent property of repeated interaction,
% not the law itself.
% χ = 0.25 * f(d≈0.95) * σ(0.8) ≈ 0.25 * 1.42 * 0.8 ≈ 0.284 (Rope, not Snare)
constraint_indexing:constraint_classification(lln_convergence, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% From a detached analytical view, the LLN is a tool for understanding and
% prediction, a form of coordination between theory and observation.
% χ = 0.25 * f(d≈0.73) * σ(1.2) ≈ 0.25 * 1.15 * 1.2 ≈ 0.345 (Rope)
constraint_indexing:constraint_classification(lln_convergence, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lln_convergence_tests).

test(perspectival_gap) :-
    % Verify the gap between the single trial (Mountain) and the institution (Rope).
    constraint_indexing:constraint_classification(lln_convergence, mountain, context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))),
    constraint_indexing:constraint_classification(lln_convergence, rope, context(agent_power(institutional), _, _, _)),
    true.

test(finite_capital_perspective) :-
    % A powerless agent with constrained options sees the law as a Rope, not a Snare,
    % due to the low base extractiveness of the mathematical principle itself.
    constraint_indexing:constraint_classification(lln_convergence, rope, context(agent_power(powerless), time_horizon(biographical), exit_options(constrained), spatial_scope(local))).

test(mountain_metric_validation) :-
    % Verify that the base metrics are consistent with the 'mountain' claim.
    narrative_ontology:constraint_metric(lln_convergence, extractiveness, E),
    narrative_ontology:constraint_metric(lln_convergence, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(lln_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.25): Set at the maximum for a Mountain. This reflects that while the LLN is a mathematical truth, its structure enables extractive applications. It "extracts" the significance of individual variance in favor of the mean.
 *   - Suppression Score (0.05): A mathematical theorem does not suppress alternatives through coercion; it simply describes a feature of reality. Alternatives (e.g., Cauchy distributions) are different systems, not suppressed ones.
 *   - NL Profile: As a mathematical law, it emerges naturally (emerges_naturally), alternatives are logically incoherent (accessibility_collapse=1.0), and it faces no meaningful resistance (resistance=0.0).
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the law as an abstract, unchangeable fact (Mountain for a single trial) and the law as a functional tool (Rope for institutions and analysts). For the gambler, the ruinous outcome feels like a Snare, but the underlying structure is a Rope due to the low base extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `insurance_industry`, `casino_operators`. These entities build business models on the predictability the LLN provides. Their institutional power and arbitrage exit options result in a negative effective extraction (χ < 0).
 *   - Victims: `gamblers_with_finite_capital`. They are victims of the convergence process itself. Their finite resources mean they cannot withstand the variance required to reach the "long run," ensuring their capital is extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   This story carefully avoids mislabeling the LLN as a Snare. The original story classified the gambler's perspective as Snare, but the math (χ ≈ 0.284) does not support this. The ruinous outcome (Gambler's Ruin) is an emergent property of applying a low-extraction Rope repeatedly to a resource-constrained agent. The Snare is not the law itself, but the *game* built upon it (e.g., `casino_house_edge`), which would be a separate constraint with ε > 0.46. This decomposition respects the ε-invariance principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_lln_convergence,
    'Is the ruinous experience of the gambler a property of the mathematical law, or an artifact of human cognitive biases (e.g., Gambler''s Fallacy) interacting with the law?',
    'Experimental trials of gambling behavior with vs. without live-mean visualizations and cognitive bias training.',
    'If it is a property of the law, the Rope classification holds. If it is primarily cognitive, the law is a Mountain and the bias is a separate Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lln_convergence, 1713, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. Base extractiveness is 0.25, below the 0.46 threshold
% for required temporal tracking. As a mathematical law, its properties
% are considered stable over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% As a fundamental mathematical theorem, it could be considered a type of
% 'information_standard' for probabilistic systems.
narrative_ontology:coordination_type(lln_convergence, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */