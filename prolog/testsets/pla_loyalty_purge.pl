% ============================================================================
% CONSTRAINT STORY: pla_loyalty_purge
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_pla_loyalty_purge, []).

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
 *   constraint_id: pla_loyalty_purge
 *   human_readable: "PLA Loyalty Purge Mechanism"
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the ongoing purge within the People's Liberation Army (PLA)
 *   under Xi Jinping. Framed publicly as an anti-corruption drive, it functions as
 *   a mechanism to enforce absolute personal loyalty, remove potential rivals, and
 *   consolidate control over the military, particularly its nuclear capabilities. The
 *   constraint makes political loyalty the primary metric for survival and advancement,
 *   supplanting military competence or operational readiness.
 *
 * KEY AGENTS (by structural relationship):
 *   - Senior PLA Officials: Primary target (powerful/trapped) — bear extraction via removal, investigation, and loss of status/freedom.
 *   - Xi Jinping & Inner Circle: Primary beneficiary (institutional/arbitrage) — benefits from consolidated power and elimination of perceived threats.
 *   - Central Commission for Discipline Inspection (CCDI): Enforcer (institutional/constrained) — carries out the purge, institutionally bound to the beneficiary's goals.
 *   - Analytical Observer: External analyst (analytical/analytical) — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_loyalty_purge, 0.75). % High extraction of careers, power, and freedom.
domain_priors:suppression_score(pla_loyalty_purge, 0.95).   % Structural property (raw, unscaled). Alternatives (disloyalty, opposition) are completely suppressed.
domain_priors:theater_ratio(pla_loyalty_purge, 0.30).       % Piton detection (>= 0.70). The anti-corruption narrative is theater, but the power consolidation is highly functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_loyalty_purge, extractiveness, 0.75).
narrative_ontology:constraint_metric(pla_loyalty_purge, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(pla_loyalty_purge, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pla_loyalty_purge, tangled_rope).
narrative_ontology:human_readable(pla_loyalty_purge, "PLA Loyalty Purge Mechanism").

% --- Binary flags ---
domain_priors:requires_active_enforcement(pla_loyalty_purge). % Required for Tangled Rope. The CCDI actively investigates and removes officials.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(pla_loyalty_purge, xi_jinping_inner_circle).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(pla_loyalty_purge, senior_pla_officials).
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
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% A senior PLA official is powerful within the military hierarchy but powerless
% against the purge mechanism. Exit is trapped. The system is purely extractive
% and coercive from this viewpoint.
% Engine derives d from: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ ≈ 0.75 * 1.42 * 1.0 (national scope) ≈ 1.065 >= 0.66 (Snare)
constraint_indexing:constraint_classification(pla_loyalty_purge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For Xi Jinping and his core leadership, the purge is a tool of statecraft—a
% mechanism to coordinate loyalty and ensure stability. It is purely functional.
% Engine derives d from: beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% χ ≈ 0.75 * -0.12 * 1.0 ≈ -0.09 (Rope)
constraint_indexing:constraint_classification(pla_loyalty_purge, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An external analyst recognizes both the coordinating function (enforcing loyalty)
% and the severe, asymmetric extraction. This dual nature is the hallmark of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% The structure includes beneficiary, victim, and active enforcement.
constraint_indexing:constraint_classification(pla_loyalty_purge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4: THE ENFORCER (TANGLED ROPE)
% The CCDI is an institutional actor but its role is instrumental and its exit is
% constrained; it must enforce the purge. It experiences the system as a
% high-stakes mechanism that it must operate, acknowledging its extractive nature.
% The directionality 'd' is intermediate, leading to a Tangled Rope classification.
constraint_indexing:constraint_classification(pla_loyalty_purge, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_loyalty_purge_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between the target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(pla_loyalty_purge, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(pla_loyalty_purge, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_structural_check) :-
    % Verify that the necessary structural components for a Tangled Rope classification are present.
    narrative_ontology:constraint_beneficiary(pla_loyalty_purge, _),
    narrative_ontology:constraint_victim(pla_loyalty_purge, _),
    domain_priors:requires_active_enforcement(pla_loyalty_purge).

test(analytical_claim_match) :-
    % The analytical classification must match the declared constraint_claim.
    narrative_ontology:constraint_claim(pla_loyalty_purge, Claim),
    constraint_indexing:constraint_classification(pla_loyalty_purge, Claim, context(agent_power(analytical), _, _, _)).

:- end_tests(pla_loyalty_purge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): This score reflects the severe cost imposed on the victims—loss of career, reputation, freedom, and potentially life. The value extracted is the consolidation of absolute political power for the beneficiary.
 *   - Suppression Score (0.95): The system offers no viable alternatives. Officials cannot opt-out, appeal to an independent judiciary, or organize opposition. The only path is compliance or elimination.
 *   - The analytical classification is Tangled Rope because the constraint possesses both a genuine coordination function (enforcing loyalty across the entire command structure) and a brutal, asymmetric extraction mechanism. It is not a pure Snare, as the "anti-corruption" frame provides a coordination narrative that is functional for the regime, even if it's a pretext.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a targeted general (powerless/trapped), the purge is an arbitrary Snare with no recourse. For the regime's architect (institutional/arbitrage), it's a necessary Rope for ensuring political security and control. The analytical view (Tangled Rope) synthesizes these realities, seeing a system that simultaneously coordinates and extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations `constraint_beneficiary(pla_loyalty_purge, xi_jinping_inner_circle)` and `constraint_victim(pla_loyalty_purge, senior_pla_officials)` are the core drivers of the classification logic. They map directly to the structural power dynamics of the purge. The engine uses this data to derive low 'd' (directionality) for the beneficiary (leading to Rope) and high 'd' for the victim (leading to Snare), quantitatively capturing the perspectival divide.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The CCDI is modeled as an institutional actor with `exit_options(constrained)`. Unlike the ultimate beneficiary who has `arbitrage` exit, the enforcer is bound to its function. This results in a different directionality and reinforces the Tangled Rope classification from its perspective, as it is intimately involved in both the coordinating and extractive aspects of the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This framework correctly identifies the dual nature of the purge, preventing a simplistic mislabeling. An analysis focused only on the victims would call it a pure Snare. An analysis accepting the state's narrative might see it as a harsh Rope. The Tangled Rope classification, derived from structural data, correctly identifies it as a hybrid system where a coordination mandate (anti-corruption/loyalty) is used to power a severe extraction engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pla_purge,
    'Is the purge primarily driven by a genuine (if paranoid) need to root out real corruption that threatens state stability, or is it almost entirely a pretext to eliminate political rivals and enforce personal loyalty?',
    'Access to internal CCDI investigation files and high-level Party deliberations.',
    'If primarily anti-corruption, the coordination function is stronger than modeled. If primarily a loyalty test, the theater_ratio is higher and the constraint is closer in nature to a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pla_loyalty_purge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.75 > 0.46), requiring temporal data.
% The purge has intensified throughout Xi Jinping's tenure (modeled over a 10-year interval).

% Theater ratio over time (stable):
narrative_ontology:measurement(pla_loyalty_purge_tr_t0, pla_loyalty_purge, theater_ratio, 0, 0.30).
narrative_ontology:measurement(pla_loyalty_purge_tr_t5, pla_loyalty_purge, theater_ratio, 5, 0.30).
narrative_ontology:measurement(pla_loyalty_purge_tr_t10, pla_loyalty_purge, theater_ratio, 10, 0.30).

% Extraction over time (increasing as control consolidates):
narrative_ontology:measurement(pla_loyalty_purge_ex_t0, pla_loyalty_purge, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pla_loyalty_purge_ex_t5, pla_loyalty_purge, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(pla_loyalty_purge_ex_t10, pla_loyalty_purge, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The purge acts as a mechanism to enforce a specific state (loyalty).
narrative_ontology:coordination_type(pla_loyalty_purge, enforcement_mechanism).

% Network relationships (structural influence edges)
% The purge directly impacts the operational effectiveness of the military.
narrative_ontology:affects_constraint(pla_loyalty_purge, pla_military_readiness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */