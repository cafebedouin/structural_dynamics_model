% ============================================================================
% CONSTRAINT STORY: us_sanctions_moex_2024
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-13
% ============================================================================

:- module(constraint_us_sanctions_moex_2024, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_sanctions_moex_2024
 *   human_readable: U.S. Sanctions Halting Dollar/Euro Trading on Moscow Exchange
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   In June 2024, the United States imposed a new package of sanctions against
 *   Russia, targeting its financial infrastructure to impede financing for the
 *   war in Ukraine. A key component of this package was sanctioning the Moscow
 *   Exchange (MOEX), which immediately halted all trading in U.S. dollars and
 *   euros, forcing Russia into less transparent and less efficient
 *   over-the-counter (OTC) markets for currency conversion.
 *
 * KEY AGENTS (by structural relationship):
 *   - Russian Financial System (MOEX, associated banks): Primary target (institutional/trapped) — bears direct costs of financial exclusion and increased transaction friction.
 *   - U.S. Treasury & Allied Governments: Primary beneficiary (institutional/arbitrage) — uses the sanction as a tool to exert geopolitical pressure and coordinate foreign policy.
 *   - Ukrainian State: Secondary beneficiary (organized/constrained) — benefits from the degradation of Russia's war financing capabilities.
 *   - Analytical Observer: Sees the full dual-function structure of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_moex_2024, 0.75). % High extraction of financial efficiency and access.
domain_priors:suppression_score(us_sanctions_moex_2024, 0.80).   % Structural property (raw, unscaled). Alternatives (OTC, Yuan) are significantly inferior.
domain_priors:theater_ratio(us_sanctions_moex_2024, 0.10).       % Piton detection (>= 0.70). This is a functional, not theatrical, constraint.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_moex_2024, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_sanctions_moex_2024, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_sanctions_moex_2024, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_sanctions_moex_2024, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_sanctions_moex_2024). % Required for Tangled Rope. Sanctions are actively enforced by OFAC.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_sanctions_moex_2024, us_treasury_and_allies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_sanctions_moex_2024, russian_financial_system).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

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
% Russian commercial entities needing USD/EUR. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This constraint functions as an inescapable economic trap.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% U.S. Treasury Department. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% The sanction is a low-cost tool to coordinate allied policy and project power.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An observer who sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INTER-INSTITUTIONAL TARGET (SNARE)
% The Moscow Exchange (MOEX) itself. It's an institutional actor, but it is
% a direct victim with its operations fundamentally constrained.
% victim membership + institutional power + trapped exit → high d → high χ
% This demonstrates one institution snaring another.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_moex_2024_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_inter_institutional) :-
    % Verify that two institutional agents with different exit options see different things.
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypeTrapped, context(agent_power(institutional), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypeArbitrage, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTrapped \= TypeArbitrage.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(us_sanctions_moex_2024, _),
    narrative_ontology:constraint_victim(us_sanctions_moex_2024, _),
    domain_priors:requires_active_enforcement(us_sanctions_moex_2024).

:- end_tests(us_sanctions_moex_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.75): The sanction directly removes a primary function of a
 *     national financial exchange, imposing severe and unavoidable efficiency costs.
 *     This is a direct extraction of economic capability.
 *   - Suppression (0.80): The dominance of the USD financial system makes alternatives
 *     structurally inferior. While Russia can pivot to OTC markets or the Yuan, these
 *     options come with lower transparency, higher risk, and reduced access, meaning
 *     alternatives are heavily suppressed.
 *   - Classification (Tangled Rope): The constraint has a clear dual function. It
 *     coordinates the foreign policy of the U.S. and its allies (the coordination
 *     function for beneficiaries) while simultaneously imposing heavy, coercive costs on
 *     Russia (the asymmetric extraction function from victims). It also requires
 *     active enforcement by OFAC. This triad—coordination, extraction, enforcement—is the
 *     canonical signature of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the U.S. Treasury (beneficiary), the sanction is a Rope—a
 *   powerful tool for coordinating international policy with minimal direct cost to
 *   itself. For the Russian financial system (victim), it is a Snare—an externally
 *   imposed trap that cripples core functions and from which there is no easy escape.
 *   The analytical perspective resolves this by identifying the structure as a Tangled
 *   Rope, acknowledging both the coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_treasury_and_allies`. They designed and deployed the constraint to
 *     achieve a strategic goal. Their `arbitrage` exit option reflects their ability to
 *     modify, escalate, or withdraw the sanction at will. This leads to a low derived `d`
 *     value and negative effective extraction (χ).
 *   - Victim: `russian_financial_system`. They are the explicit target and bear the full
 *     cost. Their `trapped` exit option reflects their inability to opt-out of the
 *     consequences. This leads to a high derived `d` value and extremely high χ.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes two institutional perspectives with different outcomes. The U.S.
 *   Treasury (`institutional`/`arbitrage`) sees a Rope. The Moscow Exchange
 *   (`institutional`/`trapped`) sees a Snare. This correctly captures the power
 *   asymmetry: being an "institution" does not grant immunity when a more powerful
 *   institution can trap you within its rule-set. The `exit_options` dimension is key to
 *   differentiating these two experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This classification correctly avoids two common errors. It
 *   does not mistake the sanction for a pure coordination mechanism (Rope), which would
 *   ignore the immense coercive extraction. It also does not mistake it for pure
 *   mindless coercion (Snare), which would miss the sophisticated policy coordination
 *   function it serves for the sanctioning coalition. The Tangled Rope classification
 *   captures its nature as an instrument of power that is both functional for its
 *   wielder and extractive for its target.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_us_sanctions_moex_2024,
    'Will this sanction degrade into a Piton over time by accelerating Russia''s successful pivot to non-dollar financial systems (e.g., Yuan, digital currencies)?',
    'Observation of Russian trade settlement data, military expenditure, and the growth of non-SWIFT payment systems over a 3-5 year horizon.',
    'If Russia pivots successfully, the sanction becomes theatrical (Piton) as its real impact diminishes. If the pivot fails, it remains an effective Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_us_sanctions_moex_2024, empirical, 'Will the sanction accelerate Russia''s successful pivot to non-dollar financial systems, degrading its impact over time?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_sanctions_moex_2024, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required since base_extractiveness > 0.46.
% The sanction was imposed suddenly, so extraction starts high and remains high.

% Theater ratio over time (starts and stays low):
narrative_ontology:measurement(us_sanctions_moex_2024_tr_t0, us_sanctions_moex_2024, theater_ratio, 0, 0.05).
narrative_ontology:measurement(us_sanctions_moex_2024_tr_t5, us_sanctions_moex_2024, theater_ratio, 5, 0.10).
narrative_ontology:measurement(us_sanctions_moex_2024_tr_t10, us_sanctions_moex_2024, theater_ratio, 10, 0.10).

% Extraction over time (starts high, slight intensification as secondary effects kick in):
narrative_ontology:measurement(us_sanctions_moex_2024_ex_t0, us_sanctions_moex_2024, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(us_sanctions_moex_2024_ex_t5, us_sanctions_moex_2024, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(us_sanctions_moex_2024_ex_t10, us_sanctions_moex_2024, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This sanction is a mechanism for enforcing a geopolitical objective.
narrative_ontology:coordination_type(us_sanctions_moex_2024, enforcement_mechanism).

% Network relationships (structural influence edges)
% This sanction directly impacts Russia's ability to finance its war effort.
narrative_ontology:affects_constraint(us_sanctions_moex_2024, russia_war_financing).
% It also has second-order effects on the global role of the dollar.
narrative_ontology:affects_constraint(us_sanctions_moex_2024, global_dollar_dominance).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation chain,
% using the declared beneficiary/victim groups and the `trapped` vs `arbitrage`
% exit options, accurately captures the structural power dynamics and produces
% the correct directionality (d) values for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */