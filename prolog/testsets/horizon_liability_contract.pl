% ============================================================================
% CONSTRAINT STORY: horizon_liability_contract
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_horizon_liability_contract, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: horizon_liability_contract
 *   human_readable: Post Office Horizon Contractual Liability
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The contractual requirement for UK sub-postmasters to use the faulty Horizon
 *   IT system and personally cover any financial shortfalls it reported. This
 *   arrangement, enforced by the Post Office's unique private prosecution powers,
 *   led to the wrongful conviction and financial ruin of hundreds of individuals
 *   based on flawed software data, constituting one of the UK's most widespread
 *   miscarriages of justice.
 *
 * KEY AGENTS (by structural relationship):
 *   - Sub-postmasters: Primary target (powerless/trapped) — bore the full financial and legal cost of system errors.
 *   - Post Office & Fujitsu: Primary beneficiary (institutional/arbitrage) — financially benefited and avoided accountability by enforcing the contract.
 *   - UK Government: Inter-institutional actor (institutional/constrained) — as sole shareholder, initially benefited from perceived efficiency but became trapped by the political and financial fallout.
 *   - Analytical Observer: Sees the full structure, including the failed coordination function and the massive asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(horizon_liability_contract, 0.85).
domain_priors:suppression_score(horizon_liability_contract, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(horizon_liability_contract, 0.60).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(horizon_liability_contract, extractiveness, 0.85).
narrative_ontology:constraint_metric(horizon_liability_contract, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(horizon_liability_contract, theater_ratio, 0.60).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable; this is a human-constructed system.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(horizon_liability_contract, tangled_rope).
narrative_ontology:human_readable(horizon_liability_contract, "Post Office Horizon Contractual Liability").

% --- Binary flags ---
domain_priors:requires_active_enforcement(horizon_liability_contract). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(horizon_liability_contract, post_office_and_fujitsu).
narrative_ontology:constraint_beneficiary(horizon_liability_contract, uk_government_shareholder).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(horizon_liability_contract, sub_postmasters).
narrative_ontology:constraint_victim(horizon_liability_contract, uk_government_shareholder). % Becomes a victim of the fallout.

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
% The sub-postmasters, forced to use the system and cover its phantom debts.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → very high χ
constraint_indexing:constraint_classification(horizon_liability_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Post Office & Fujitsu, who profited and enforced the system.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(horizon_liability_contract, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% This view recognizes both the stated coordination purpose (a unified accounting
% system) and the actual function (asymmetric extraction), classifying it as a
% Tangled Rope due to the high ε, suppression, and required enforcement.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
constraint_indexing:constraint_classification(horizon_liability_contract, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL ACTOR (TANGLED ROPE)
% The UK Government as the Post Office's sole shareholder. Its position is
% ambiguous: it benefits from a "profitable" entity but is politically trapped
% by the scandal. Its exit is constrained. We use a directionality override
% to capture this conflicted state, where it is both beneficiary and victim.
constraint_indexing:constraint_classification(horizon_liability_contract, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(horizon_liability_contract_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the massive perspectival gap between sub-postmasters (Snare) and the Post Office (Rope).
    constraint_indexing:constraint_classification(horizon_liability_contract, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(horizon_liability_contract, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical view must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(horizon_liability_contract, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify all three conditions for Tangled Rope classification are declared.
    narrative_ontology:constraint_beneficiary(horizon_liability_contract, _),
    narrative_ontology:constraint_victim(horizon_liability_contract, _),
    domain_priors:requires_active_enforcement(horizon_liability_contract).

:- end_tests(horizon_liability_contract_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): Extremely high. The contract could, and did,
 *     extract life savings, homes, and livelihoods from victims for phantom debts.
 *   - Suppression (0.95): Extremely high. Sub-postmasters had no alternative system.
 *     The Post Office used its status and prosecution powers to silence dissent and
 *     treat Horizon data as infallible, creating a near-total suppression field.
 *   - The analytical classification is Tangled Rope because the constraint possesses
 *     both a genuine (though failed) coordination function (a unified accounting
 *     platform) and a massive, asymmetrically enforced extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For sub-postmasters (powerless, trapped), it was a pure Snare.
 *   There was no coordination benefit, only ruinous extraction. For the Post Office
 *   and Fujitsu (institutional, arbitrage), it was a perfect Rope. It coordinated
 *   their financial and legal positions, externalized all risk, and generated revenue
 *   while insulating them from accountability. The difference in classification is a
 *   direct result of their diametrically opposed structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `post_office_and_fujitsu`. They received direct financial benefits
 *     and were shielded from liability. Their arbitrage exit option gives them a low `d`.
 *   - Victims: `sub_postmasters`. They bore 100% of the cost of system failure. Their
 *     trapped status gives them a very high `d`.
 *   - The `uk_government_shareholder` is listed as both beneficiary and victim. It
 *     benefited from the Post Office's apparent financial health but ultimately bears
 *     the massive cost of compensation and the political damage. This conflicted role
 *     is resolved with a directionality override.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The UK Government's perspective highlights a key inter-institutional dynamic. As an
 *   institutional actor, its default power is high, but its `constrained` exit (it cannot
 *   simply divest from the Post Office) and dual role as beneficiary/victim makes its
 *   position complex. The directionality override (d=0.60) places it as a net victim of
 *   the arrangement, reflecting the reality that the long-term costs far outweighed the
 *   short-term benefits of propping up a faulty system. This correctly classifies the
 *   constraint as a Tangled Rope from its perspective, but with a different effective
 *   extraction (χ) than the purely analytical view.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This case is a canonical example of a corrupted Rope.
 *   A system designed for coordination (accounting) became a tool for pure
 *   extraction. Classifying it as a Tangled Rope analytically prevents the
 *   Mandatrophy error of seeing it as only a Snare (ignoring the coordination
 *   pretense that gave it legitimacy) or only a Rope (ignoring the catastrophic
 *   extraction). The framework correctly identifies the hybrid nature that made
 *   it so uniquely destructive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_horizon_liability_contract,
    'Was the Post Office leadership aware of the Horizon system''s flaws from the outset, or did their belief in its infallibility only curdle into willful ignorance and cover-up over time?',
    'Internal communications from Fujitsu and the Post Office from the 1999-2005 period.',
    'If aware from outset: The constraint was a Snare by design. If ignorance curdled: It was a Rope that catastrophically failed and was retrofitted into a Snare to hide the failure.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(horizon_liability_contract, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint was highly extractive from its inception, but the theatrical
% component (public denials, blaming victims) intensified over time as the
% evidence of system failure became undeniable.
% Timeline: t=0 (2000), t=10 (2010), t=20 (2020)

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(horizon_tr_t0, horizon_liability_contract, theater_ratio, 0, 0.10).
narrative_ontology:measurement(horizon_tr_t10, horizon_liability_contract, theater_ratio, 10, 0.45).
narrative_ontology:measurement(horizon_tr_t20, horizon_liability_contract, theater_ratio, 20, 0.60).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(horizon_ex_t0, horizon_liability_contract, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(horizon_ex_t10, horizon_liability_contract, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(horizon_ex_t20, horizon_liability_contract, base_extractiveness, 20, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system's stated purpose was to be a standard for information.
narrative_ontology:coordination_type(horizon_liability_contract, information_standard).

% Network relationships (structural influence edges)
% The constraint's enforcement mechanism was critically enabled by the Post Office's
% ability to act as a private prosecutor, a separate but linked constraint.
narrative_ontology:affects_constraint(uk_private_prosecution_powers, horizon_liability_contract).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The UK Government's structural position is ambiguous. The default derivation
% for an institutional actor with constrained exit might not capture the fact that
% the long-term political and financial costs made it a net victim. We override
% d to 0.60 to reflect a position that is more targeted than symmetric, but less
% so than a powerless agent.
constraint_indexing:directionality_override(horizon_liability_contract, institutional, 0.60).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */