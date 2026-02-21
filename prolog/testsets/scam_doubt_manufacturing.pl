% ============================================================================
% CONSTRAINT STORY: scam_doubt_manufacturing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_scam_doubt_manufacturing, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scam_doubt_manufacturing
 *   human_readable: The playbook for manufacturing scientific doubt (SCAMs)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint models the systematic, coordinated strategy used by various
 *   industries (e.g., tobacco, fossil fuels, chemicals) to manufacture
 *   uncertainty about scientific evidence that links their products or activities
 *   to public harm. Termed "Scientific Certainty Argumentation Methods" (SCAMs),
 *   this strategy uses rhetoric, lobbying, and funding of contrarian research
 *   to delay regulation, protect profits, and externalize costs onto the public.
 *
 * KEY AGENTS (by structural relationship):
 *   - Cost Externalization Targets: Primary target (powerless/trapped) — the public groups who bear the health, environmental, and economic costs of delayed regulation.
 *   - Regulated Industries: Primary beneficiary (institutional/arbitrage) — coordinates to use the strategy to maintain profits and avoid compliance costs.
 *   - Regulatory Agencies: Institutional victim (institutional/constrained) — tasked with protecting the public but institutionally constrained by the manufactured "debate".
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination for asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scam_doubt_manufacturing, 0.75).
domain_priors:suppression_score(scam_doubt_manufacturing, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(scam_doubt_manufacturing, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scam_doubt_manufacturing, extractiveness, 0.75).
narrative_ontology:constraint_metric(scam_doubt_manufacturing, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(scam_doubt_manufacturing, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(scam_doubt_manufacturing, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(scam_doubt_manufacturing). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% This constraint is socially constructed, not natural.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(scam_doubt_manufacturing, regulated_industries).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(scam_doubt_manufacturing, cost_externalization_targets).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, regulatory_agencies).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (COST EXTERNALIZATION TARGETS)
% As a victim with trapped exit, d is high (≈0.95), f(d) is high (≈1.42).
% The massive base extraction (ε=0.75) and global scope (σ=1.2) result in
% χ ≈ 0.75 * 1.42 * 1.2 ≈ 1.28, a clear Snare.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE INDUSTRIES)
% As a beneficiary with arbitrage exit, d is low (≈0.05), f(d) is negative (≈-0.12).
% χ ≈ 0.75 * -0.12 * 1.2 ≈ -0.11. This is a highly effective coordination tool,
% a pure Rope from their perspective.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees both coordination and extraction. The analytical perspective's
% default d (≈0.72) and f(d) (≈1.15) yield χ ≈ 0.75 * 1.15 * 1.2 ≈ 1.04.
% This high χ combined with the presence of a coordination function
% (beneficiary declared) and asymmetric extraction (victim declared) leads
% to the Tangled Rope classification.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% This captures the dynamic between industry and government regulators.

% Perspective 4: The constrained regulatory agency
% As an institutional actor, but a victim with constrained exit, its derived
% d value is higher than the industry's but lower than the public's.
% It perceives the coordination function but is also subject to the extraction
% (in the form of undermined authority and inability to fulfill its mandate).
% It correctly identifies the structure as a Tangled Rope.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_doubt_manufacturing_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the targets see a Snare while industry sees a Rope.
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_detection) :-
    % Verify the analytical and constrained institutional views see a Tangled Rope.
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, tangled_rope, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify the structural preconditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(scam_doubt_manufacturing, _), % Has coordination
    narrative_ontology:constraint_victim(scam_doubt_manufacturing, _),     % Has asymmetric extraction
    domain_priors:requires_active_enforcement(scam_doubt_manufacturing).   % Has enforcement

:- end_tests(scam_doubt_manufacturing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.75) is very high, representing the immense economic
 *   value (trillions of dollars over decades) captured by industries by avoiding
 *   and delaying regulations on harmful products and activities. Suppression
 *   (0.80) is also high, as the core function is to suppress the prevailing
 *   scientific consensus and prevent it from being translated into policy.
 *   The structure requires constant, active enforcement via lobbying, PR, and
 *   media campaigns.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the beneficiaries (regulated industries), this is a
 *   masterful coordination tool (Rope) that allows them to solve a collective
 *   action problem: how to fight costly regulation. For the victims (the public),
 *   it is a pure Snare, trapping them in a system where they are exposed to
 *   harm and their own regulatory institutions are paralyzed by misinformation.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `regulated_industries` clearly benefit by protecting revenue
 *     streams and externalizing costs. Their high power and `arbitrage` exit
 *     (they can shift funding, change tactics) gives them a very low directionality `d`.
 *   - Victim: `cost_externalization_targets` (the public) clearly bear the costs
 *     through negative health and environmental outcomes. Their `trapped` status
 *     gives them a very high `d`.
 *   - Victim: `regulatory_agencies` are also victims, as their function is
 *     undermined. Their `constrained` exit reflects their inability to simply
 *     ignore the manufactured controversy, forcing them into a defensive posture.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the asymmetric relationship between the industries and
 *   the agencies meant to regulate them. Both are `institutional` actors, but
 *   their different structural relationships (beneficiary vs. victim) and exit
 *   options (`arbitrage` vs. `constrained`) cause the engine to derive different
 *   directionality values, leading to different classifications (Rope vs.
 *   Tangled Rope). This quantifies the effect of regulatory capture and influence.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] A simplistic analysis might label this a pure Snare.
 *   However, that misses the crucial coordination element that makes it so
 *   effective and resilient. By classifying it as a Tangled Rope from the
 *   analytical perspective, the system correctly identifies that dismantling
 *   it requires addressing not only the extraction but also the underlying
 *   coordination mechanisms (e.g., trade associations, shared PR firms) that
 *   industries use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scam_doubt_manufacturing,
    'Is the doubt-manufacturing strategy a centrally coordinated conspiracy (e.g., by a few key PR firms) or an emergent, convergent strategy independently adopted by various industries after seeing its success?',
    'Analysis of historical internal corporate and PR firm documents; network analysis of funding flows to contrarian scientists and think tanks.',
    'If centrally coordinated, it is a more brittle structure with key nodes that can be targeted. If emergent, it is more resilient and must be countered by changing the incentives and information environment for all industries.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_scam_doubt_manufacturing, empirical, 'Centralized conspiracy vs. emergent convergent strategy').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(scam_doubt_manufacturing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has been refined and intensified since its origins in the
% tobacco industry's 1950s playbook. The base extraction has grown as the
% strategy has been applied to ever-larger sectors of the global economy.
% This data is required because base_extractiveness (0.75) > 0.46.

% Theater ratio over time (slight increase in sophistication):
narrative_ontology:measurement(scam_doubt_manufacturing_tr_t0, scam_doubt_manufacturing, theater_ratio, 0, 0.20).
narrative_ontology:measurement(scam_doubt_manufacturing_tr_t5, scam_doubt_manufacturing, theater_ratio, 5, 0.25).
narrative_ontology:measurement(scam_doubt_manufacturing_tr_t10, scam_doubt_manufacturing, theater_ratio, 10, 0.30).

% Extraction over time (growing application from tobacco to global energy):
narrative_ontology:measurement(scam_doubt_manufacturing_ex_t0, scam_doubt_manufacturing, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(scam_doubt_manufacturing_ex_t5, scam_doubt_manufacturing, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(scam_doubt_manufacturing_ex_t10, scam_doubt_manufacturing, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The industries are coordinating to establish a (false) standard of what
% constitutes "sound science" or "debatable evidence" in the public square.
narrative_ontology:coordination_type(scam_doubt_manufacturing, information_standard).

% Network relationships (structural influence edges)
% The SCAMs playbook is a foundational constraint that enables many other
% forms of regulatory delay and capture.
narrative_ontology:affects_constraint(scam_doubt_manufacturing, regulatory_capture_epa).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, climate_change_inaction).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, pfas_regulation_delay).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation
% based on the beneficiary/victim declarations and exit options accurately
% captures the structural dynamics of the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */