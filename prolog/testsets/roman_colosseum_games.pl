% ============================================================================
% CONSTRAINT STORY: roman_colosseum_games
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_roman_colosseum_games, []).

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
 *   constraint_id: roman_colosseum_games
 *   human_readable: The Spectacle of the Roman Colosseum
 *   domain: political/social
 *
 * SUMMARY:
 *   The state-sponsored games of the Roman Colosseum ("bread and circuses")
 *   functioned as a system of social and political control. It provided mass
 *   entertainment to the populace to ensure quiescence, while simultaneously
 *   serving as a brutal mechanism for public execution and a demonstration
 *   of imperial power. The system combined a genuine coordination function
 *   (public order) with extreme asymmetric extraction (the lives of its victims).
 *
 * KEY AGENTS (by structural relationship):
 *   - Condemned & Captives: Primary target (powerless/trapped) — their lives are the extracted resource.
 *   - Roman State & Emperor: Primary beneficiary (institutional/arbitrage) — uses the games to secure political stability and project power.
 *   - Roman Populace: Secondary beneficiary (organized/constrained) — receives entertainment and sustenance, ensuring their compliance.
 *   - Analytical Observer: Sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_colosseum_games, 0.55).
domain_priors:suppression_score(roman_colosseum_games, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(roman_colosseum_games, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_colosseum_games, extractiveness, 0.55).
narrative_ontology:constraint_metric(roman_colosseum_games, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(roman_colosseum_games, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(roman_colosseum_games, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(roman_colosseum_games). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(roman_colosseum_games, roman_state_and_emperor).
narrative_ontology:constraint_beneficiary(roman_colosseum_games, roman_populace).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(roman_colosseum_games, condemned_and_captives).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE CONDEMNED CAPTIVE)
% Experiences the system as pure, inescapable extraction. Engine derives
% d from victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.0 (national scope) ≈ 0.78, which is a Snare (>= 0.66).
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE ROMAN STATE)
% Experiences the system as a pure coordination tool for social stability.
% Engine derives d from beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.0 (national scope) ≈ -0.07, which is a Rope.
constraint_indexing:constraint_classification(roman_colosseum_games, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination and extraction functions simultaneously.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15 for this perspective.
% χ = 0.55 * 1.15 * 1.2 (global scope) ≈ 0.76. This value, combined with the
% high ε and suppression, and the presence of both beneficiary and victim,
% classifies as a Tangled Rope (0.40 <= χ <= 0.90).
constraint_indexing:constraint_classification(roman_colosseum_games, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: SECONDARY BENEFICIARY (THE ROMAN POPULACE)
% Experiences the system as a beneficial coordination mechanism (entertainment).
% As a beneficiary with constrained exit, their derived d is low, resulting in a Rope classification.
constraint_indexing:constraint_classification(roman_colosseum_games, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_colosseum_games_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(roman_colosseum_games, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(roman_colosseum_games, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(roman_colosseum_games, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(roman_colosseum_games, _),
    narrative_ontology:constraint_victim(roman_colosseum_games, _),
    domain_priors:requires_active_enforcement(roman_colosseum_games).

:- end_tests(roman_colosseum_games_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.55) is high, representing the extraction of human life,
 *   the most extreme form of extraction. Suppression (0.90) is near-total, as victims
 *   had no recourse against the power of the Roman state. The Theater Ratio (0.15)
 *   is low because the spectacle WAS the function; it was not a performative layer
 *   hiding a lack of function. The system was brutally effective at its dual goals.
 *   These metrics, combined with the structural facts (beneficiary, victim, enforcement),
 *   make it a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. For the condemned (`powerless`/`trapped`), the games are a
 *   SNAPE: a system of pure, coercive extraction with zero coordination benefit. For
 *   the Roman State (`institutional`/`arbitrage`), the games are a ROPE: a highly
 *   effective, if ruthless, tool for social coordination and pacification. The lives
 *   of victims are viewed as a necessary input cost for maintaining imperial stability.
 *   This diametric opposition in classification is characteristic of highly effective
 *   Tangled Ropes.
 *
 * DIRECTIONALITY LOGIC:
 *   The `roman_state_and_emperor` and `roman_populace` are declared beneficiaries.
 *   The state gains stability and a venue for demonstrating power. The populace gains
 *   food and entertainment, ensuring their quiescence. The `condemned_and_captives`
 *   are the victims, as their lives fuel the entire system. The engine correctly
 *   derives a low/negative directionality (d) for beneficiaries, leading to low/negative
 *   effective extraction (χ) and a Rope classification. It derives a high d for victims,
 *   leading to high χ and a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It does not mistake the
 *   system for a pure Snare, which would ignore its genuine and historically significant
 *   coordination function in managing Roman society. It also does not mistake it for
 *   a pure Rope, which would whitewash the extreme, asymmetric violence required to
 *   make the coordination function work. The Tangled Rope classification captures the
 *   essential, irreducible duality of the Colosseum as both a coordination mechanism
 *   and an extraction engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_colosseum_necessity,
    'Were the games a structurally necessary component for maintaining stability in the Roman political economy, or a purely extractive performance of power that hastened imperial decline?',
    'Comparative analysis of public order and imperial stability in periods/cities with and without large-scale gladiatorial games, controlling for other factors like grain supply and military success.',
    'If structurally necessary, the Tangled Rope classification is robust. If purely a performance of dominance, the Snare classification becomes more accurate for the overall system, with the coordination aspect being more theatrical.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(roman_colosseum_games, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As a high-extraction constraint
% (ε > 0.46), this is required. The model shows a slight increase in both
% extraction and theatricality over the centuries of the games' operation.

% Theater ratio over time (slight increase in spectacle for spectacle's sake):
narrative_ontology:measurement(rcg_tr_t0, roman_colosseum_games, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rcg_tr_t5, roman_colosseum_games, theater_ratio, 5, 0.12).
narrative_ontology:measurement(rcg_tr_t10, roman_colosseum_games, theater_ratio, 10, 0.15).

% Extraction over time (games became more elaborate and deadly):
narrative_ontology:measurement(rcg_ex_t0, roman_colosseum_games, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(rcg_ex_t5, roman_colosseum_games, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(rcg_ex_t10, roman_colosseum_games, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: primarily an enforcement mechanism for social hierarchy and state power.
narrative_ontology:coordination_type(roman_colosseum_games, enforcement_mechanism).

% Network relationships: The games were structurally coupled with military expansion
% (source of captives) and the grain dole (the other half of "panem et circenses").
narrative_ontology:affects_constraint(roman_military_expansion, roman_colosseum_games).
narrative_ontology:affects_constraint(roman_grain_dole, roman_colosseum_games).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on declared beneficiaries, victims, and their respective exit options
% accurately models the structural relationships and produces the correct
% perspectival classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */