% ============================================================================
% CONSTRAINT STORY: ergo_mixer_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_ergo_mixer_protocol, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_mixer_protocol
 *   human_readable: ErgoMixer Privacy Mechanism
 *   domain: technological/social
 *
 * SUMMARY:
 *   ErgoMixer is a non-interactive, non-custodial cryptocurrency mixer that
 *   utilizes Zero-Knowledge Proofs to break on-chain links between deposit
 *   and withdrawal addresses. It provides a coordination mechanism for users
 *   to enhance their financial privacy and the fungibility of their assets,
 *   but this same mechanism suppresses the ability of external observers to
 *   conduct chain analysis, and can transfer risk to users if the anonymity
 *   pool is too small.
 *
 * KEY AGENTS (by structural relationship):
 *   - User in a Small Pool (powerless/trapped): Primary target — bears the risk of deanonymization, a form of extraction.
 *   - Privacy-Seeking User (moderate/mobile): Primary beneficiary — benefits from enhanced fungibility and privacy.
 *   - Regulatory Body / Chain Analyst (institutional/constrained): Secondary victim — their function (tracing funds) is suppressed by the protocol.
 *   - Analytical Observer (analytical/analytical): Sees the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_mixer_protocol, 0.48).
domain_priors:suppression_score(ergo_mixer_protocol, 0.70).
domain_priors:theater_ratio(ergo_mixer_protocol, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_mixer_protocol, extractiveness, 0.48).
narrative_ontology:constraint_metric(ergo_mixer_protocol, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ergo_mixer_protocol, theater_ratio, 0.32).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ergo_mixer_protocol, tangled_rope).
narrative_ontology:human_readable(ergo_mixer_protocol, "ErgoMixer Privacy Mechanism").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ergo_mixer_protocol). % Enforcement is cryptographic.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ergo_mixer_protocol, privacy_seeking_users).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ergo_mixer_protocol, chain_analysts).
narrative_ontology:constraint_victim(ergo_mixer_protocol, small_pool_users).

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

% PERSPECTIVE 1: THE USER IN A SMALL POOL (SNARE)
% Agent who bears the risk of deanonymization. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(ergo_mixer_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIVACY-SEEKING USER (ROPE)
% Agent who benefits from the coordination function. Engine derives d from:
%   beneficiary membership + mobile exit → d ≈ 0.15 → f(d) ≈ -0.01 → low/negative χ
constraint_indexing:constraint_classification(ergo_mixer_protocol, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE REGULATORY BODY / CHAIN ANALYST (SNARE)
% This agent's function is suppressed by the protocol. They are a victim.
% Engine derives d from: victim membership + constrained exit -> d ~ 0.9 -> f(d) ~ 1.35 -> high χ
constraint_indexing:constraint_classification(ergo_mixer_protocol, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function (for users) and the asymmetric
% extraction/suppression (risk transfer to small pool users, suppression of analysis).
constraint_indexing:constraint_classification(ergo_mixer_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_mixer_protocol_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between beneficiary and victims.
    constraint_indexing:constraint_classification(ergo_mixer_protocol, TypeBeneficiary, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(ergo_mixer_protocol, TypeVictim, context(agent_power(powerless), _, _, _)),
    TypeBeneficiary \= TypeVictim,
    TypeBeneficiary == rope,
    TypeVictim == snare.

test(institutional_victim_perspective) :-
    % Verify the institutional perspective is also a victim (Snare).
    constraint_indexing:constraint_classification(ergo_mixer_protocol, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ergo_mixer_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    TypeInstitutional == TypePowerless,
    TypeInstitutional == snare.

:- end_tests(ergo_mixer_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics have been adjusted to align with the `Tangled Rope`
 *   classification. Base extractiveness (ε=0.48) is not financial but
 *   represents the transfer of deanonymization risk to users, especially
 *   those in small anonymity pools. The suppression score (0.70) reflects
 *   the protocol's primary function: suppressing the ability of external
 *   parties to trace transaction histories, which is a high degree of coercion
 *   against the alternative of a fully transparent ledger. The `emerges_naturally`
 *   flag was removed as the protocol is a human-designed cryptographic system,
 *   not a law of nature.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a user with sufficient peers (a large anonymity set),
 *   the protocol is a pure coordination tool (Rope) to achieve privacy. For
 *   a user without enough peers, the promise of privacy becomes a trap (Snare),
 *   as their activity is statistically visible. For regulators and chain analysts,
 *   whose function is predicated on transparency, the protocol is a Snare that
 *   obstructs their work. The analytical view sees both functions simultaneously,
 *   classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `privacy_seeking_users` directly benefit from the coordination function.
 *   - Victims: `chain_analysts` are victims because their function is directly
 *     suppressed. `small_pool_users` are victims because the system transfers
 *     the risk of failure (deanonymization) onto them. This risk is the source
 *     of the constraint's extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the protocol.
 *   A naive analysis might label it a pure Rope (focusing only on the user's
 *   intent) or a pure Snare (focusing only on the regulator's view). The
 *   Tangled Rope classification, supported by the high suppression and risk-transfer
 *   extraction, captures the reality that it is a coordination tool whose
 *   effectiveness is built upon coercive suppression of an alternative (transparency)
 *   and extracts value in the form of privacy risk from its most vulnerable users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_ergo_mixer_protocol,
    "Is the anonymity set of a non-interactive mixer ever dense enough to provide long-term security against a state-level actor with quantum computing capabilities?",
    "Advances in post-quantum cryptography and statistical deanonymization research.",
    "If True: The protocol remains a viable Rope for privacy. If False: The protocol degrades into a time-delayed Snare for all users, as past transactions become retroactively traceable.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_ergo_mixer_protocol, empirical, "Long-term cryptographic security of the anonymity set against future state-level attacks.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ergo_mixer_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (base_extractiveness > 0.46), so
% temporal data is required for lifecycle drift detection.
% We model a scenario where the protocol starts with high ideals (low extraction)
% but the systemic risk (extraction) becomes more apparent over time.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(ergo_mixer_protocol_tr_t0, ergo_mixer_protocol, theater_ratio, 0, 0.30).
narrative_ontology:measurement(ergo_mixer_protocol_tr_t5, ergo_mixer_protocol, theater_ratio, 5, 0.31).
narrative_ontology:measurement(ergo_mixer_protocol_tr_t10, ergo_mixer_protocol, theater_ratio, 10, 0.32).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(ergo_mixer_protocol_ex_t0, ergo_mixer_protocol, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ergo_mixer_protocol_ex_t5, ergo_mixer_protocol, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(ergo_mixer_protocol_ex_t10, ergo_mixer_protocol, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(ergo_mixer_protocol, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */