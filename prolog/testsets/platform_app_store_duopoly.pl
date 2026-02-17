% ============================================================================
% CONSTRAINT STORY: platform_app_store_duopoly
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_platform_app_store_duopoly, []).

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
 *   constraint_id: platform_app_store_duopoly
 *   human_readable: Platform Mandate for Proprietary App Stores and In-App Payments
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models the mobile operating system duopoly (Apple's iOS and
 *   Google's Android) that requires app developers to use proprietary app stores
 *   for distribution and in-app payment systems, which charge commissions of
 *   15-30%. This structure centralizes control, extracts significant revenue
 *   from the ecosystem, and suppresses alternative distribution and payment
 *   mechanisms. This model reflects the state of the market that prompted
 *   regulatory action in Japan and other regions.
 *
 * KEY AGENTS (by structural relationship):
 *   - platform_owners (e.g., Apple, Google): Primary beneficiary (institutional/arbitrage) — they set the rules, collect commissions, and control the ecosystem.
 *   - app_developers: Primary target (powerless/trapped) — they must comply to access the mobile market, bearing the cost of commissions and platform restrictions.
 *   - national_regulators (e.g., Japan Fair Trade Commission): Inter-institutional actor (institutional/constrained) — they seek to dismantle or weaken the constraint to foster competition.
 *   - analytical_observer: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_app_store_duopoly, 0.55).
domain_priors:suppression_score(platform_app_store_duopoly, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(platform_app_store_duopoly, 0.25).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_app_store_duopoly, extractiveness, 0.55).
narrative_ontology:constraint_metric(platform_app_store_duopoly, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(platform_app_store_duopoly, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(platform_app_store_duopoly, tangled_rope).
narrative_ontology:human_readable(platform_app_store_duopoly, "Platform Mandate for Proprietary App Stores and In-App Payments").

% --- Binary flags ---
domain_priors:requires_active_enforcement(platform_app_store_duopoly). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, platform_owners).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(platform_app_store_duopoly, app_developers).
narrative_ontology:constraint_victim(platform_app_store_duopoly, national_regulators). % Regulators are structurally opposed, thus on the victim side of the d-spectrum.

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
% App developers who are forced to use the platform's systems. Engine derives d
% from victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(platform_app_store_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Platform owners who operate the stores. Engine derives d from beneficiary
% membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(platform_app_store_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% National regulators attempting to break the duopoly. They are an institutional
% power, but their exit options are constrained by the global nature of the
% platforms. They are structurally opposed to the constraint.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_app_store_duopoly_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between developer (target) and platform (beneficiary).
    constraint_indexing:constraint_classification(platform_app_store_duopoly, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(platform_app_store_duopoly, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    % Ensure the analytical view correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, _),
    narrative_ontology:constraint_victim(platform_app_store_duopoly, _),
    domain_priors:requires_active_enforcement(platform_app_store_duopoly).

:- end_tests(platform_app_store_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Represents the 15-30% commission, a direct and substantial extraction of value from developers.
 *   - Suppression Score (0.85): Reflects the high technical and contractual barriers to alternative app stores and payment systems, especially on iOS. Sideloading is actively prevented.
 *   - Theater Ratio (0.25): The platforms' justifications (security, privacy, user experience) have a basis in reality but are also used to defend a highly profitable business model. The core function is not pure theater.
 *   - This combination of high extraction, high suppression, and a genuine (but monopolized) coordination function makes it a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - For an app developer (powerless, trapped), the constraint is a Snare. There is no viable alternative to access the market, and the terms are non-negotiable and highly extractive. The coordination function is overshadowed by the coercive extraction.
 *   - For a platform owner (institutional, arbitrage), the constraint is a Rope. It is a highly efficient system for coordinating developers and users, generating immense value. From their perspective, the extraction is a fee for a service they created and maintain, resulting in a negative effective extraction (χ) due to their beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `platform_owners` benefit from commission revenues and total control over the ecosystem.
 *   - Victims: `app_developers` bear the direct financial cost and lack of autonomy. `national_regulators` are also structurally victimized in the sense that their goal is to break the constraint; their success is the constraint's failure, placing them in an oppositional role that the directionality engine maps to the victim side of the spectrum (high d).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The dynamic between `platform_owners` (institutional/arbitrage) and `national_regulators` (institutional/constrained) is a key feature. Both are institutional actors, but their relationship to the constraint is opposite. The regulator's exit is 'constrained' because its legal power is limited to its national jurisdiction, while the platform is global. The system derives a different directionality (d) for each, capturing why the regulator sees an extractive system in need of reform, while the platform sees a functional coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the constraint. A simplistic analysis might label it a pure Snare (ignoring the massive coordination value it provides) or a pure Rope (ignoring the coercive, asymmetric extraction). By classifying it as a Tangled Rope, the system acknowledges that it solves a real coordination problem (connecting developers to users) while simultaneously functioning as a vehicle for rent-seeking due to suppressed competition. This prevents mislabeling a complex hybrid system as purely malicious or purely benign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_platform_app_store_duopoly,
    'Is the 15-30% commission a fair price for coordination and security, or a supracompetitive rent extracted via duopolistic power?',
    'Empirical analysis of platform operational costs vs. revenue, and market data from jurisdictions (like the EU post-DMA) where alternative stores are mandated.',
    'If deemed a fair price, ε would be lower, pushing the classification towards Rope. If deemed supracompetitive rent, the current Tangled Rope/Snare classification is strongly validated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(platform_app_store_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time. Initially a method to build an
% ecosystem, it has since solidified into a powerful extraction mechanism.
% Since base_extractiveness (0.55) > 0.46, temporal data is required.

% Theater ratio over time:
narrative_ontology:measurement(as_duopoly_tr_t0, platform_app_store_duopoly, theater_ratio, 0, 0.15).
narrative_ontology:measurement(as_duopoly_tr_t5, platform_app_store_duopoly, theater_ratio, 5, 0.20).
narrative_ontology:measurement(as_duopoly_tr_t10, platform_app_store_duopoly, theater_ratio, 10, 0.25).

% Extraction over time:
narrative_ontology:measurement(as_duopoly_ex_t0, platform_app_store_duopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(as_duopoly_ex_t5, platform_app_store_duopoly, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(as_duopoly_ex_t10, platform_app_store_duopoly, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(platform_app_store_duopoly, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the relationships between platforms, developers, and regulators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */