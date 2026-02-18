% ============================================================================
% CONSTRAINT STORY: private_credit_market_opacity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_private_credit_market_opacity, []).

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
 *   constraint_id: private_credit_market_opacity
 *   human_readable: Opacity and Systemic Risk Externalization in Private Credit Markets
 *   domain: economic
 *
 * SUMMARY:
 *   The global private credit market has swelled to over $1.7 trillion by channeling capital
 *   from large investors (pension funds, sovereign wealth funds) to corporate borrowers,
 *   bypassing regulated public markets. This constraint is the structural opacity and lack of
 *   regulatory oversight that enables this market. While providing a genuine coordination
 *   function (capital allocation), this opacity allows beneficiaries to capture high returns while
 *   externalizing systemic financial risks onto the public, who have no visibility or say.
 *
 * KEY AGENTS (by structural relationship):
 *   - public_financial_system: Primary target (powerless/trapped) — bears the uncompensated systemic risk of market collapse.
 *   - private_equity_and_investors: Primary beneficiary (institutional/arbitrage) — captures high returns ("alpha") enabled by opacity and regulatory arbitrage.
 *   - financial_regulators: Inter-institutional actor (institutional/constrained) — attempts to monitor and contain risk but is hampered by a lack of data and jurisdiction.
 *   - analytical_observer: Analytical observer — sees both the coordination function and the asymmetric risk transfer, identifying the full tangled structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_credit_market_opacity, 0.55).
domain_priors:suppression_score(private_credit_market_opacity, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(private_credit_market_opacity, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_credit_market_opacity, extractiveness, 0.55).
narrative_ontology:constraint_metric(private_credit_market_opacity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(private_credit_market_opacity, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(private_credit_market_opacity, tangled_rope).
narrative_ontology:human_readable(private_credit_market_opacity, "Opacity and Systemic Risk Externalization in Private Credit Markets").
narrative_ontology:topic_domain(private_credit_market_opacity, "economic").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(private_credit_market_opacity).
domain_priors:requires_active_enforcement(private_credit_market_opacity). % Enforcement is via contractual secrecy and lobbying against regulation.

% --- Emergence flag (required for mountain constraints) ---
% Not applicable.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(private_credit_market_opacity, private_equity_and_investors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(private_credit_market_opacity, public_financial_system).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The public financial system is trapped; it cannot opt out of the consequences of a
% systemic crisis. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.55 * 1.42 * 1.2 (global) ≈ 0.94. This easily clears the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Private equity firms and their investors see a highly efficient coordination mechanism
% for generating high returns. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.55 * -0.12 * 1.2 ≈ -0.08. This is a Rope.
constraint_indexing:constraint_classification(private_credit_market_opacity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the capital allocation function and the
% severe extractive element of risk externalization.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 ≈ 0.76. This is in the Tangled Rope/Snare range (0.40 ≤ χ ≤ 0.90).
% Given the declared coordination function, it classifies as Tangled Rope.
constraint_indexing:constraint_classification(private_credit_market_opacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% Perspective 4: Financial Regulators (TANGLED ROPE)
% Regulators are institutional actors but are structurally disadvantaged by the opacity.
% They have a constrained exit; they cannot ignore the market but lack the tools to
% fully govern it. They perceive both the economic function and the danger.
% Engine derives d from institutional power + constrained exit, likely d≈0.4-0.5.
% Let's assume d=0.45 -> f(d)≈0.5.
% χ = 0.55 * 0.5 * 1.2 ≈ 0.33. This falls short of Tangled Rope. The derived d for
% an `institutional` actor is too low even with `constrained` exit.
% This is a good case for a directionality override. Regulators are targets of lobbying
% and information asymmetry, pushing their d higher than the default derivation.
% Let's override their d to 0.60 to reflect their structural disadvantage.
% New χ = 0.55 * f(0.60) * 1.2 ≈ 0.55 * 0.90 * 1.2 ≈ 0.59. This places it firmly in Tangled Rope territory.
constraint_indexing:directionality_override(private_credit_market_opacity, institutional, 0.60).
constraint_indexing:constraint_classification(private_credit_market_opacity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_credit_market_opacity_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(private_credit_market_opacity, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(private_credit_market_opacity, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_claim_match) :-
    % The analytical observer's classification must match the declared constraint claim.
    narrative_ontology:constraint_claim(private_credit_market_opacity, Claim),
    constraint_indexing:constraint_classification(private_credit_market_opacity, Claim, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gates_pass) :-
    % Verify that all structural requirements for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(private_credit_market_opacity, _),
    narrative_ontology:constraint_victim(private_credit_market_opacity, _),
    domain_priors:requires_active_enforcement(private_credit_market_opacity).

:- end_tests(private_credit_market_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the "juicy" returns captured by beneficiaries, which are derived not just from lending but from regulatory arbitrage and the externalization of systemic risk. This is the value extracted from systemic stability.
 *   - Suppression (S=0.75): Set high because the market's opacity and high returns actively suppress transparent, regulated alternatives. Capital and talent flow to this sector, hollowing out public markets. The lack of data is a powerful form of suppression.
 *   - The combination of a genuine economic function (capital allocation) with high asymmetric extraction (risk externalization) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For beneficiaries (PE firms, investors), the constraint is a `Rope`—a brilliant coordination tool for generating alpha with low perceived extraction (χ is negative from their view). For the primary target (the public), who bears the latent tail risk, it is a `Snare`—a high-extraction, high-suppression system they are trapped in. The classification system correctly captures this diametric opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `private_equity_and_investors`. They directly profit from the opacity and lack of regulation. Their `arbitrage` exit options and beneficiary status drive their directionality `d` towards 0, resulting in a negative effective extraction (χ).
 *   - Victims: `public_financial_system`. This group bears the unpriced externality of systemic risk. Their `trapped` status drives their `d` towards 1, resulting in a very high χ, classifying the constraint as a Snare from their perspective.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes `financial_regulators` as an `institutional` actor with `constrained` exit. They are not direct beneficiaries or victims but are structurally involved. The automatic `d` derivation for an institutional actor would be too low to reflect their true position of disadvantage against an opaque, powerful industry. Therefore, a `directionality_override` to d=0.60 is used. This correctly models them as being partially targeted by the constraint's opacity, leading them to also classify it as a `Tangled Rope`, albeit with a different χ than the neutral analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   A simplistic analysis might label this system a pure `Snare` (focusing only on risk) or a pure `Rope` (focusing only on capital allocation). The `Tangled Rope` classification is crucial for preventing this Mandatrophy. It acknowledges that the system performs a real coordination function, which is why it's so powerful and resilient, while simultaneously identifying the severe, asymmetric extraction that makes it dangerous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_private_credit_risk,
    'Is the systemic risk from correlated, opaque leverage manageable by market participants, or is it a "ticking time bomb" that will cascade during the next major economic downturn?',
    'A severe, prolonged global recession that tests covenant-lite loans and recovery rates across the entire private credit portfolio.',
    'If risk is manageable, the constraint is a stable (though extractive) Tangled Rope. If it cascades, the Snare classification becomes dominant and catastrophic.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(private_credit_market_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε > 0.46), requiring temporal data.
% The market grew rapidly post-2008, so extraction has likely increased.

% Theater ratio over time (stable):
narrative_ontology:measurement(pco_tr_t0, private_credit_market_opacity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pco_tr_t5, private_credit_market_opacity, theater_ratio, 5, 0.18).
narrative_ontology:measurement(pco_tr_t10, private_credit_market_opacity, theater_ratio, 10, 0.20).

% Extraction over time (increasing as market scaled):
narrative_ontology:measurement(pco_ex_t0, private_credit_market_opacity, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pco_ex_t5, private_credit_market_opacity, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(pco_ex_t10, private_credit_market_opacity, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(private_credit_market_opacity, resource_allocation).

% Network relationships (structural influence edges)
% The opacity and risk in private credit directly impacts the stability and
% function of traditional, regulated lending markets.
narrative_ontology:affects_constraint(private_credit_market_opacity, regulated_public_lending_markets).
narrative_ontology:affects_constraint(private_credit_market_opacity, systemic_financial_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% As noted in section 3, an override is used for the 'financial_regulators'
% perspective. The default derivation for an 'institutional' agent would
% result in a d value too low to reflect their structural disadvantage
% (information asymmetry, regulatory capture pressures). The override to 0.60
% models them as partial targets of the constraint's opacity, not neutral
% observers or beneficiaries. This is declared in section 3.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */