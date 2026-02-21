% ============================================================================
% CONSTRAINT STORY: hollywood_four_quadrant_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_hollywood_four_quadrant_model, []).

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
 *   constraint_id: hollywood_four_quadrant_model
 *   human_readable: The Four-Quadrant Blockbuster Model
 *   domain: economic
 *
 * SUMMARY:
 *   The "four-quadrant" model is a Hollywood studio heuristic for greenlighting
 *   and marketing blockbuster films, requiring them to appeal to males and
 *   females both under and over 25. While serving as an internal coordination
 *   mechanism to de-risk massive (~$200M+) investments, it systematically
 *   suppresses mid-budget, artistically ambitious, or non-franchise films,
 *   extracting creative potential from the industry and limiting audience choice.
 *   The recent success of films like "Saltburn" and the fictional "Wuthering Heights"
 *   challenges the model's necessity, revealing its extractive nature.
 *
 * KEY AGENTS (by structural relationship):
 *   - Major Film Studios: Primary beneficiary (institutional/arbitrage) — use the model to coordinate departments and minimize financial risk.
 *   - Mid-budget Filmmakers & Production Companies: Primary target (organized/constrained) — struggle to secure funding and distribution for projects that don't fit the model.
 *   - Diverse Content Audiences: Secondary target (powerless/trapped) — face a homogenized slate of films at the multiplex, with limited access to alternative mainstream content.
 *   - Analytical Observer: Film critic or industry analyst (analytical/analytical) — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hollywood_four_quadrant_model, 0.52).
domain_priors:suppression_score(hollywood_four_quadrant_model, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hollywood_four_quadrant_model, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hollywood_four_quadrant_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(hollywood_four_quadrant_model, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hollywood_four_quadrant_model, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hollywood_four_quadrant_model, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(hollywood_four_quadrant_model). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hollywood_four_quadrant_model, major_film_studios).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hollywood_four_quadrant_model, mid_budget_filmmakers).
narrative_ontology:constraint_victim(hollywood_four_quadrant_model, diverse_content_audiences).

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

% PERSPECTIVE 1: THE AUDIENCE MEMBER (SNARE)
% As an individual moviegoer, the options at the multiplex are highly constrained.
% The model traps them in a feedback loop of franchise films.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.52 * 1.42 * σ(national=1.0) ≈ 0.74.
% With χ ≥ 0.66 and suppression ≥ 0.60, this classifies as a Snare.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MAJOR STUDIO (ROPE)
% For a studio executive, the model is a pure coordination tool to align
% production, marketing, and finance, de-risking enormous capital outlay.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.52 * -0.12 * σ(global=1.2) ≈ -0.07.
% Negative χ indicates a subsidy, classifying it as a Rope.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An industry analyst sees both the coordination function for studios and the
% severe extractive effects on filmmakers and culture.
% Engine derives canonical d for analytical ≈ 0.73 → f(d) ≈ 1.15.
% χ = 0.52 * 1.15 * σ(global=1.2) ≈ 0.72.
% With a clear beneficiary, victim, and active enforcement, the system identifies
% both coordination and extraction, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MID-BUDGET FILMMAKER (TANGLED ROPE)
% For filmmakers, the constraint is coercive, but they are not entirely powerless.
% They are organized (guilds, production companies) and have some exit (indie film, streaming).
% Engine derives d from: victim + organized power + constrained exit → d ≈ 0.60 → f(d) ≈ 0.85.
% χ = 0.52 * 0.85 * σ(national=1.0) ≈ 0.44.
% χ is in the [0.40, 0.90] range, and all structural flags are met, so this is a Tangled Rope.
constraint_indexing:constraint_classification(hollywood_four_quadrant_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hollywood_four_quadrant_model_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the primary victim and beneficiary.
    constraint_indexing:constraint_classification(hollywood_four_quadrant_model, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hollywood_four_quadrant_model, rope, context(agent_power(institutional), _, _, _)),
    format('... Perspectival gap validated: powerless (Snare) vs institutional (Rope)').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(hollywood_four_quadrant_model, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical perspective correctly identifies Tangled Rope structure').

test(tangled_rope_structural_gates) :-
    % Verify that the necessary structural facts for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(hollywood_four_quadrant_model, _),
    narrative_ontology:constraint_victim(hollywood_four_quadrant_model, _),
    domain_priors:requires_active_enforcement(hollywood_four_quadrant_model).

:- end_tests(hollywood_four_quadrant_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents the significant opportunity cost of unmade films and the concentration of cultural/financial capital into a narrow, formulaic genre.
 *   - Suppression Score (0.75): Extremely high. Major studios control global distribution and marketing, making it exceptionally difficult for non-compliant films to achieve wide release and cultural penetration.
 *   - Theater Ratio (0.15): Low. This is not a performative act; it is a core, functional financial and organizational heuristic used by studios.
 *   The combination of a genuine coordination function and high asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a studio (beneficiary), the model is a Rope—an indispensable tool for coordinating global-scale production and mitigating risk, creating value for them (negative χ). For an audience member (victim), it is a Snare—a coercive system that limits choice and traps them in a cycle of sequels and remakes (high positive χ). The filmmaker sees it as a Tangled Rope, recognizing the system's logic while being constrained by its extractive gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `major_film_studios` directly benefit through risk reduction and profit maximization. Their `arbitrage` exit option gives them immense power, driving their directionality `d` close to 0.
 *   - Victims: `mid_budget_filmmakers` and `diverse_content_audiences` bear the costs. Filmmakers lose creative and financial opportunities. Audiences lose cultural diversity. Their `constrained` and `trapped` exit options drive their `d` values close to 1, maximizing the effective extraction they experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not dismiss the model as a pure Snare, which would ignore its genuine, albeit self-serving, coordination function for studios. It also does not accept the studio's "Rope" perspective at face value, which would ignore the immense creative and cultural extraction imposed on other stakeholders. By classifying the overall structure as a Tangled Rope, the system accurately captures the duality of function and harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_hollywood_four_quadrant_model,
    'Is the "Saltburn effect" (increased audience appetite for challenging, non-franchise films) a durable shift in taste or a temporary cultural moment?',
    'Track box office returns and streaming performance of mid-budget, non-franchise, and "arthouse" films over the next 3-5 years. Monitor studio greenlight decisions for changes in strategy.',
    'If durable, the model\'s suppression score will fall as alternatives become viable, potentially loosening the Tangled Rope. If temporary, the model will remain a powerful Snare/Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hollywood_four_quadrant_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model has become more entrenched over the last two decades as film budgets
% have ballooned and the global market has become central.
% This represents extraction_accumulation drift.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(h4qm_tr_t0, hollywood_four_quadrant_model, theater_ratio, 0, 0.15).
narrative_ontology:measurement(h4qm_tr_t5, hollywood_four_quadrant_model, theater_ratio, 5, 0.15).
narrative_ontology:measurement(h4qm_tr_t10, hollywood_four_quadrant_model, theater_ratio, 10, 0.15).

% Extraction over time (increasing as mid-budget films were squeezed out):
narrative_ontology:measurement(h4qm_ex_t0, hollywood_four_quadrant_model, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(h4qm_ex_t5, hollywood_four_quadrant_model, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(h4qm_ex_t10, hollywood_four_quadrant_model, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The model is a heuristic for allocating capital and production resources.
narrative_ontology:coordination_type(hollywood_four_quadrant_model, resource_allocation).

% Network relationships (structural influence edges)
% The logic of the four-quadrant model has been adapted and applied to the
% development of content for major streaming platforms.
narrative_ontology:affects_constraint(hollywood_four_quadrant_model, streaming_content_algorithm).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the declared beneficiary/victim groups and their respective exit
% options (arbitrage vs. constrained/trapped) accurately models the
% structural power dynamics of the Hollywood studio system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */