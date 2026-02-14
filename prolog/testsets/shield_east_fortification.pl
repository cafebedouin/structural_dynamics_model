% ============================================================================
% CONSTRAINT STORY: shield_east_fortification
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_shield_east_fortification, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shield_east_fortification
 *   human_readable: "Shield East" Border Fortification Program
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   The "Shield East" program is a Polish national initiative to build a €2.3bn
 *   system of fortifications, surveillance, and anti-drone defenses along its
 *   400km eastern border with Russia and Belarus. The program serves a dual
 *   function: coordinating NATO's eastern flank defense to deter military
 *   aggression, and suppressing the movement of unauthorized migrants, who
 *   are often used in hybrid warfare tactics by Belarus.
 *
 * KEY AGENTS (by structural relationship):
 *   - Polish State & NATO Allies: Primary beneficiaries (institutional/arbitrage) — gain security and deterrence capability.
 *   - Unauthorized Migrants: Primary targets (powerless/trapped) — bear the cost of exclusion and suppressed movement.
 *   - Potential Aggressor States (e.g., Russia): Inter-institutional target (institutional/constrained) — military options are suppressed.
 *   - Polish Taxpayers: Hybrid role (organized/mobile) — provide the funding (victim) for a national security good (beneficiary).
 *   - Defense Contractors: Secondary beneficiaries (powerful/arbitrage) — receive contracts for construction and technology.
 *   - Analytical Observer: Sees the full dual-function structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shield_east_fortification, 0.48).
domain_priors:suppression_score(shield_east_fortification, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(shield_east_fortification, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shield_east_fortification, extractiveness, 0.48).
narrative_ontology:constraint_metric(shield_east_fortification, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(shield_east_fortification, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(shield_east_fortification, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(shield_east_fortification). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(shield_east_fortification, polish_government).
narrative_ontology:constraint_beneficiary(shield_east_fortification, nato_allies).
narrative_ontology:constraint_beneficiary(shield_east_fortification, defense_contractors).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(shield_east_fortification, unauthorized_migrants).
narrative_ontology:constraint_victim(shield_east_fortification, polish_taxpayers).
narrative_ontology:constraint_victim(shield_east_fortification, potential_aggressors).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (UNAUTHORIZED MIGRANTS)
% As victims with trapped exit, engine derives d ≈ 0.95 → f(d) ≈ 1.42.
% High ε (0.48) and suppression (0.85) lead to a Snare classification.
% χ = 0.48 * 1.42 * 1.0 (national) ≈ 0.68, which is ≥ 0.66 (Snare threshold).
constraint_indexing:constraint_classification(shield_east_fortification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (POLISH STATE)
% As a beneficiary with arbitrage exit, engine derives d ≈ 0.05 → f(d) ≈ -0.12.
% Effective extraction is negative, making it a pure coordination mechanism.
% χ = 0.48 * -0.12 * 1.0 (national) ≈ -0.06, qualifying as Rope (χ ≤ 0.35).
constraint_indexing:constraint_classification(shield_east_fortification, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Engine derives canonical d ≈ 0.73 → f(d) ≈ 1.15.
% Scope is continental, σ(S) = 1.1. This reveals the dual-function structure.
% χ = 0.48 * 1.15 * 1.1 (continental) ≈ 0.61. This is a Tangled Rope (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(shield_east_fortification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE ADVERSARY STATE
% As an institutional victim with constrained exit options, its 'd' is high but not maximal.
% Engine derives d ≈ 0.6-0.7 → f(d) ≈ 0.9-1.1.
% χ = 0.48 * ~1.0 * 1.1 (continental) ≈ 0.53. This is also a Tangled Rope.
% It acknowledges the constraint's effectiveness while recognizing the actor's power.
constraint_indexing:constraint_classification(shield_east_fortification, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shield_east_fortification_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the migrant (target) and the state (beneficiary).
    constraint_indexing:constraint_classification(shield_east_fortification, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(shield_east_fortification, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Passed: Migrant sees Snare, State sees Rope.').

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must correctly identify the dual-function nature.
    constraint_indexing:constraint_classification(shield_east_fortification, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(shield_east_fortification, _),
    narrative_ontology:constraint_victim(shield_east_fortification, _),
    domain_priors:requires_active_enforcement(shield_east_fortification).

:- end_tests(shield_east_fortification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): Represents the significant €2.3bn financial cost extracted
 *     from taxpayers and the severe extraction of freedom of movement from migrants.
 *   - Suppression Score (s=0.85): The explicit goal is to suppress military incursions and
 *     unauthorized border crossings. This is the primary function of a fortification.
 *   - Analytical Classification (Tangled Rope): The constraint has both a genuine coordination
 *     function (national/NATO defense) and a highly coercive, asymmetric extraction function
 *     (blocking migrants, costing taxpayers). This dual nature is the hallmark of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Polish state and NATO, this is a Rope—a pure coordination
 *   mechanism to produce a collective security good, where the benefits far outweigh the costs.
 *   For an unauthorized migrant at the border, it is a Snare—a purely coercive, high-suppression
 *   system that extracts their mobility and offers them no benefit, with no viable alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural relationships. The beneficiaries are the Polish
 *   state and its allies, who designed the system and can modify it (arbitrage exit), leading to
 *   a low 'd' value and a Rope classification. The victims are migrants, who are trapped by the
 *   system, and taxpayers, who fund it. The 'trapped' status of migrants gives them the highest
 *   'd' value, pushing their perspective into the Snare category.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   An aggressor state is an institutional-level victim. Unlike a powerless migrant, it has
 *   resources and agency, but its military options are now limited (constrained exit). This
 *   results in a Tangled Rope classification from its perspective: it recognizes the coercive
 *   power of the constraint but is not fully trapped by it in the way a migrant is.
 *
 * MANDATROPHY ANALYSIS:
 *   This model avoids the Mandatrophy error of classifying a critical national security
 *   infrastructure project as a pure Snare. By acknowledging the coordination function valued by
 *   its beneficiaries (the Rope perspective), the system provides a balanced view. However, it
 *   simultaneously uses the `powerless` index to capture the brutal, extractive reality of the
 *   constraint for its most vulnerable targets, correctly identifying it as a Snare from their
 *   viewpoint. The analytical `tangled_rope` classification synthesizes these truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_shield_east_fortification,
    'Is the Shield primarily a functional military deterrent or a piece of performative security theater designed for political signaling?',
    'Analysis of military bypass scenarios, wargaming results, and observed changes in adversary behavior post-completion.',
    'If purely functional, it remains a Tangled Rope. If largely performative and easily bypassed, its theater_ratio would rise over time, causing it to degrade towards a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(shield_east_fortification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The project is slated for 2025-2028. This models the ramp-up from announcement to full operation.
% Extraction (cost) starts low and increases as construction begins.
% Theater is initially high (political announcement) and then decreases as the project becomes functional.

% Theater ratio over time:
narrative_ontology:measurement(shield_east_fortification_tr_t0, shield_east_fortification, theater_ratio, 0, 0.40).
narrative_ontology:measurement(shield_east_fortification_tr_t5, shield_east_fortification, theater_ratio, 5, 0.25).
narrative_ontology:measurement(shield_east_fortification_tr_t10, shield_east_fortification, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(shield_east_fortification_ex_t0, shield_east_fortification, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(shield_east_fortification_ex_t5, shield_east_fortification, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(shield_east_fortification_ex_t10, shield_east_fortification, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is fundamentally a mechanism for enforcing a state of affairs (border integrity).
narrative_ontology:coordination_type(shield_east_fortification, enforcement_mechanism).

% Network relationships (structural influence edges)
% The construction of the shield directly influences and is influenced by broader EU asylum policies.
narrative_ontology:affects_constraint(shield_east_fortification, eu_asylum_policy).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit_options accurately captures the
% structural relationships between the agents and the fortification system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */