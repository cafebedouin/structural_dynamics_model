% ============================================================================
% CONSTRAINT STORY: openclaw_data_lock_in
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_openclaw_data_lock_in, []).

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
 *   constraint_id: openclaw_data_lock_in
 *   human_readable: Data Lock-In by the OpenClaw AI Personal Assistant
 *   domain: technological
 *
 * SUMMARY:
 *   OpenClaw is a viral AI personal assistant that offers profound life
 *   optimization by creating a hyper-personalized digital twin ('claw') of
 *   its user. While providing genuine coordination benefits, its proprietary
 *   and non-portable data structure creates extreme switching costs,
 *   effectively locking users into its ecosystem. This allows its parent
 *   company to extract significant value from a captive user base.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individual Users: Primary target (powerless/trapped) — receives initial
 *     benefits but ultimately bears the cost of data immobility and reduced
 *     future choice.
 *   - Claw Corp (and its investors): Primary beneficiary (institutional/arbitrage) —
 *     benefits from the deep competitive moat created by user lock-in, enabling
 *     long-term revenue extraction and data monetization.
 *   - Competitor AI Services: Secondary actors (organized/constrained) — are
 *     suppressed by the high barriers to entry created by OpenClaw's data silo.
 *   - Regulatory Analysts: Analytical observer — sees both the genuine
 *     coordination function and the asymmetric extractive structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openclaw_data_lock_in, 0.52).
domain_priors:suppression_score(openclaw_data_lock_in, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(openclaw_data_lock_in, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openclaw_data_lock_in, extractiveness, 0.52).
narrative_ontology:constraint_metric(openclaw_data_lock_in, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(openclaw_data_lock_in, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(openclaw_data_lock_in, tangled_rope).
narrative_ontology:human_readable(openclaw_data_lock_in, "Data Lock-In by the OpenClaw AI Personal Assistant").

% --- Binary flags ---
domain_priors:requires_active_enforcement(openclaw_data_lock_in). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(openclaw_data_lock_in, claw_corp_and_investors).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(openclaw_data_lock_in, individual_openclaw_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL USER (PRIMARY TARGET)
% The user experiences profound utility but also a complete inability to leave
% without losing years of accumulated personalization. The high switching cost
% and loss of autonomy make it feel like a pure trap.
% Engine derives d from victim membership + trapped exit → d≈0.95 → f(d)≈1.42.
% χ = 0.52 * 1.42 * 1.2 (global) ≈ 0.88, which is a Snare (χ ≥ 0.66).
constraint_indexing:constraint_classification(openclaw_data_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLAW CORP (PRIMARY BENEFICIARY)
% The company views the lock-in not as extraction, but as a successful
% coordination mechanism that retains customers by offering unparalleled value.
% From their view, it's a feature, not a bug.
% Engine derives d from beneficiary membership + arbitrage exit → d≈0.05 → f(d)≈-0.12.
% χ = 0.52 * -0.12 * 1.2 (global) ≈ -0.07, which is a Rope (χ ≤ 0.35).
constraint_indexing:constraint_classification(openclaw_data_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% An analyst recognizes both the genuine user benefit (coordination) and the
% severe, coercive lock-in (asymmetric extraction). It possesses the defining
% features of a Tangled Rope: a real coordination function, asymmetric
% extraction, and active enforcement through technological design.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.52 * 1.15 * 1.2 (global) ≈ 0.72, which is a Tangled Rope (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(openclaw_data_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openclaw_data_lock_in_tests).

test(perspectival_gap_user_vs_corp, [nondet]) :-
    constraint_indexing:constraint_classification(openclaw_data_lock_in, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(openclaw_data_lock_in, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(openclaw_data_lock_in, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_conditions_met) :-
    narrative_ontology:constraint_beneficiary(openclaw_data_lock_in, _),
    narrative_ontology:constraint_victim(openclaw_data_lock_in, _),
    domain_priors:requires_active_enforcement(openclaw_data_lock_in).

:- end_tests(openclaw_data_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): This value reflects the significant economic
 *     surplus Claw Corp can capture through supra-competitive pricing, data
 *     monetization, and the market power derived from a locked-in user base.
 *   - Suppression (0.75): This is high because the core design of the 'claw'
 *     system makes data export and transfer to a competitor technologically
 *     infeasible, effectively suppressing all alternative services for a committed user.
 *     Suppression is a raw structural property, not scaled by perspective.
 *   - Theater Ratio (0.30): The service's marketing likely emphasizes user
 *     empowerment and personalization, which masks the underlying lock-in
 *     mechanism. However, the core function is still highly effective, so
 *     theater is not yet dominant.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the user (powerless/trapped), the constraint is a
 *   Snare because the cost of exit is destructively high, foreclosing future
 *   choice. For Claw Corp (institutional/arbitrage), it is a Rope because they see
 *   it as a legitimate coordination mechanism for delivering value that happens
 *   to ensure customer loyalty. The analytical observer sees both sides,
 *   identifying the hybrid Tangled Rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `claw_corp_and_investors` benefit directly from the annuity-like
 *     revenue stream and the data asset created by user lock-in. Their arbitrage
 *     exit option gives them a very low directionality `d`.
 *   - Victim: `individual_openclaw_users` are the victims. While they receive a
 *     service, they pay for it with their future autonomy. Their trapped status
 *     gives them a very high directionality `d`, leading to a high effective
 *     extraction `χ`.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of where Mandatrophy can occur. A naive
 *   analysis might label OpenClaw a Snare (focusing only on the lock-in) or a
 *   Rope (focusing only on the utility). The Tangled Rope classification is
 *   crucial because it correctly identifies that *both* are true: there is a
 *   genuine coordination function that is inextricably linked to an asymmetric
 *   extractive mechanism. This prevents mischaracterizing a useful but coercive
 *   system as either purely beneficial or purely malicious.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_openclaw_intent,
    'Is the data non-portability an unavoidable technical side-effect of deep personalization, or a deliberate "moat-building" strategy for monopolistic extraction?',
    'Internal company documents, whistleblower testimony, or analysis of whether a less-restrictive architecture was technically feasible at similar performance.',
    'If an unavoidable side-effect, the system is a tragic Tangled Rope. If a deliberate strategy, it is a malicious Snare masquerading as a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openclaw_data_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (ε > 0.46).
% Models the shift from a useful tool to an extractive ecosystem.
% T=0: Initial launch. T=5: Viral growth phase. T=10: Mature, locked-in market.

% Theater ratio over time (marketing vs. reality):
narrative_ontology:measurement(ocl_tr_t0, openclaw_data_lock_in, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ocl_tr_t5, openclaw_data_lock_in, theater_ratio, 5, 0.20).
narrative_ontology:measurement(ocl_tr_t10, openclaw_data_lock_in, theater_ratio, 10, 0.30).

% Extraction over time (from fair price to monopoly rent):
narrative_ontology:measurement(ocl_ex_t0, openclaw_data_lock_in, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(ocl_ex_t5, openclaw_data_lock_in, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(ocl_ex_t10, openclaw_data_lock_in, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It standardizes and optimizes a user's personal
% information space into a coherent, actionable model.
narrative_ontology:coordination_type(openclaw_data_lock_in, information_standard).

% Network relationships: This constraint directly impacts any potential
% regulation around data portability.
narrative_ontology:affects_constraint(openclaw_data_lock_in, gdpr_style_data_portability_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately reflects the
% structural power dynamics of the scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */