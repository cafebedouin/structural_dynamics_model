% ============================================================================
% CONSTRAINT STORY: ua_mobilization_protector_cert
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ua_mobilization_protector_cert, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ua_mobilization_protector_cert
 *   human_readable: "Ukrainian 'Protector' Certificate Mobilization System"
 *   domain: political/technological
 *
 * SUMMARY:
 *   Based on a speculative 2026 scenario, this constraint models a mandatory
 *   digital certificate system ("Protector") in Ukraine for managing military
 *   mobilization. All citizens of mobilization age must maintain a valid
 *   digital status by complying with the military commissariat (TCC). An
 *   invalid status blocks essential civil and economic activities, including
 *   leaving the country, major financial transactions, and property sales,
 *   effectively creating a digital enclosure for the mobilization pool.
 *
 * KEY AGENTS (by structural relationship):
 *   - Citizens of mobilization age: Primary target (powerless/trapped) — their autonomy, mobility, and economic freedom are extracted to serve national defense needs.
 *   - The Ukrainian State (Ministry of Defence): Primary beneficiary (institutional/arbitrage) — gains a highly efficient, coercive tool for enforcing conscription and managing personnel.
 *   - Financial & State Service Institutions: Secondary Actor / Enforcer (institutional/constrained) — Legally mandated to enforce the certificate system, bearing implementation costs and acting as agents of state coercion.
 *   - Analytical Observer: Sees the full structure, recognizing both the coordination function and the severe asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_mobilization_protector_cert, 0.65).
domain_priors:suppression_score(ua_mobilization_protector_cert, 0.85).   % Structural property (raw, unscaled). High due to digital locks on civil life.
domain_priors:theater_ratio(ua_mobilization_protector_cert, 0.10).       % Piton detection (>= 0.70). This system is brutally functional, not theatrical.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, extractiveness, 0.65).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ua_mobilization_protector_cert, tangled_rope).
narrative_ontology:human_readable(ua_mobilization_protector_cert, "Ukrainian 'Protector' Certificate Mobilization System").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ua_mobilization_protector_cert). % Required for Tangled Rope. Enforcement is automated via API calls from banks, border control etc.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, the_ukrainian_state_mod).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, citizens_of_mobilization_age).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, financial_and_state_service_institutions). % They are also victims of coercion, forced to enforce.

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

% PERSPECTIVE 1: THE PRIMARY TARGET (CITIZENS OF MOBILIZATION AGE)
% Victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.65 * 1.42 * 1.0 (national) ≈ 0.92. This is a clear Snare.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (UKRAINIAN STATE)
% Beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.65 * -0.12 * 1.0 (national) ≈ -0.078. From the state's view, this is a
% pure Rope that subsidizes its coordination effort for national defense.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both coordination and extraction.
% Derives d ≈ 0.72 → f(d) ≈ 1.15. Global scope amplifies perceived extraction.
% χ = 0.65 * 1.15 * 1.2 (global) ≈ 0.90. Meets Tangled Rope criteria because
% the structural flags (beneficiary, victim, enforcement) are all present.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ENFORCING INSTITUTIONS (INTER-INSTITUTIONAL)
% Victim membership + constrained exit for an institutional actor.
% The engine derives a higher d than for a beneficiary, but lower than for a trapped target.
% For this case, the engine derives d≈0.7, so f(d)≈1.1.
% χ = 0.65 * 1.1 * 1.0 (national) ≈ 0.715. They experience it as a Tangled Rope:
% a coordination task imposed on them with coercive, extractive force.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_mobilization_protector_cert_tests).

test(perspectival_gap_target_vs_beneficiary, [nondet]) :-
    % Verify the core perspectival gap between the citizen (Snare) and the state (Rope).
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope, [nondet]) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, _),
    narrative_ontology:constraint_victim(ua_mobilization_protector_cert, _),
    domain_priors:requires_active_enforcement(ua_mobilization_protector_cert).

:- end_tests(ua_mobilization_protector_cert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High, representing the severe cost imposed on the target demographic — loss of autonomy, mobility, economic freedom, and potentially life if mobilized.
 *   - Suppression (S=0.85): Very high. The system is designed to eliminate alternatives. Non-compliance results in digital ostracization, blocking access to fundamental aspects of modern life. It is not just coercive; it makes evasion nearly impossible without dropping out of the formal economy entirely.
 *   - Theater (T=0.10): Low. This is a brutally pragmatic tool of state power in wartime, not a performative gesture. Its function is direct and unambiguous.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the state (beneficiary), the system is a pure coordination tool (Rope) that solves a critical logistics problem for national survival. For the citizen of mobilization age (target), it is a digital cage (Snare) that removes their agency and traps them. This massive disparity is characteristic of Tangled Ropes, where a genuine coordination function is coupled with severe, asymmetrically applied extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The Ukrainian state apparatus, particularly the Ministry of Defence, benefits directly from a predictable and controllable pool of manpower. This is declared via `constraint_beneficiary`.
 *   - Victim: Citizens of mobilization age are the primary victims, bearing the costs. This is declared via `constraint_victim`. Financial institutions are also listed as victims because they are coerced into enforcing the system, bearing compliance costs and client friction, showcasing the constraint's inter-institutional impact.
 *   The engine uses these declarations and the agents' exit options (arbitrage vs. trapped vs. constrained) to derive the directionality `d` and compute the wildly different effective extraction (χ) values.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the system, preventing two potential errors. It avoids mislabeling it as a pure Snare, which would ignore its genuine and necessary coordination function in a defensive war. It also avoids mislabeling it as a pure Rope, which would whitewash the immense coercive extraction imposed on a specific segment of the population. The Tangled Rope classification captures the uncomfortable reality that a mechanism can be both essential for collective survival (coordination) and brutally extractive at the same time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ua_mobilization_protector_cert,
    'Will the extreme coercion of the Protector system ultimately undermine national morale and social cohesion, thereby backfiring on the war effort?',
    'Long-term (3-5 year) empirical data tracking mobilization rates, rates of illegal evasion, public polling on trust in government, and social cohesion indices.',
    'If it backfires, the system is a net-negative Tangled Rope degrading toward a pure Snare. If it succeeds without major social fracture, it remains a "successful" (from the state perspective) but highly extractive Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing, modelling the system's first 10 years
narrative_ontology:interval(ua_mobilization_protector_cert, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required as ε > 0.46.
% This model assumes the system was introduced with slightly lower coercion
% that was intensified over time as the situation became more dire.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(ua_mob_tr_t0, ua_mobilization_protector_cert, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ua_mob_tr_t5, ua_mobilization_protector_cert, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ua_mob_tr_t10, ua_mobilization_protector_cert, theater_ratio, 10, 0.10).

% Extraction over time (shows intensification of coercion):
narrative_ontology:measurement(ua_mob_ex_t0, ua_mobilization_protector_cert, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ua_mob_ex_t5, ua_mobilization_protector_cert, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ua_mob_ex_t10, ua_mobilization_protector_cert, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system is fundamentally about allocating human capital.
narrative_ontology:coordination_type(ua_mobilization_protector_cert, resource_allocation).

% Network relationships (structural influence edges)
% This system would be deeply integrated with and affect the banking system.
narrative_ontology:affects_constraint(ua_mobilization_protector_cert, ua_banking_system_liquidity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain,
% which uses the beneficiary/victim declarations in combination with the
% agents' exit options (trapped, constrained, arbitrage), accurately
% models the directionality of the constraint for all key perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */