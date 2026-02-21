% ============================================================================
% CONSTRAINT STORY: monopoly_fp_house_rule
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_monopoly_fp_house_rule, []).

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
 *   constraint_id: monopoly_fp_house_rule
 *   human_readable: Monopoly 'Free Parking' House Rule
 *   domain: social/economic
 *
 * SUMMARY:
 *   This constraint is the common house rule in the game Monopoly where all
 *   fines and taxes are placed in the center of the board and collected by
 *   the player who lands on the "Free Parking" space. This rule, a glitch
 *   that became a feature, introduces a large, random cash windfall,
 *   altering the game's deterministic economic model, prolonging play, and
 *   injecting volatility that can reverse player fortunes.
 *
 * KEY AGENTS (by structural relationship):
 *   - Strategically-dominant players: Primary target (powerless/trapped) — bears extraction by having their skill and accumulated advantage diluted by randomness.
 *   - The Social Group (as an institution): Enforcer/Beneficiary (institutional/arbitrage) — benefits from the social coordination function of the rule.
 *   - Casual players: Primary beneficiary (moderate/constrained) — benefits from a longer, more exciting, and less predictable game experience.
 *   - Analytical observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_fp_house_rule, 0.48).
domain_priors:suppression_score(monopoly_fp_house_rule, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(monopoly_fp_house_rule, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_fp_house_rule, extractiveness, 0.48).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monopoly_fp_house_rule, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(monopoly_fp_house_rule, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(monopoly_fp_house_rule). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(monopoly_fp_house_rule, casual_players).
narrative_ontology:constraint_beneficiary(monopoly_fp_house_rule, financially_disadvantaged_players).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(monopoly_fp_house_rule, strategically_dominant_players).

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

% PERSPECTIVE 1: THE STRATEGIC PLAYER (TARGET)
% This player is treated as powerless against the social convention of the
% house rule. They are trapped in a game whose strategic integrity is
% undermined. The local scope (σ=0.8) dampens the effective extraction,
% preventing a Snare classification and revealing it as a Tangled Rope,
% reflecting that even the victim perceives the social/coordination function.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SOCIAL INSTITUTION (ENFORCER/BENEFICIARY)
% The social group enforcing the rule acts as the institution. They benefit
% from the social coordination function (making the game "fun"). The engine
% derives d from beneficiary status + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12,
% resulting in a clear Rope classification.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE CASUAL PLAYER (BENEFICIARY)
% This player benefits from the extended play and excitement. The rule is
% a pure coordination mechanism for their enjoyment. The engine derives a
% very low directionality (d), resulting in low or negative effective
% extraction (χ), yielding a Rope classification.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The observer sees the full structure. With high base extraction (ε=0.48)
% and the global scope modifier (σ=1.2) amplifying the perceived
% extractiveness, the effective extraction χ crosses the Snare threshold (≥0.66).
% The coordination function is seen as secondary to the rule's primary effect:
% distorting the game's economic model.
constraint_indexing:constraint_classification(monopoly_fp_house_rule, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_fp_house_rule_tests).

test(perspectival_gap_target_institution) :-
    % Verify perspectival gap between target (powerless) and beneficiary (institutional).
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == tangled_rope),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_analytical) :-
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_fp_house_rule, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeTarget == tangled_rope),
    assertion(TypeAnalytical == snare),
    TypeTarget \= TypeAnalytical.

test(tangled_rope_conditions_met) :-
    narrative_ontology:constraint_beneficiary(monopoly_fp_house_rule, _),
    narrative_ontology:constraint_victim(monopoly_fp_house_rule, _),
    domain_priors:requires_active_enforcement(monopoly_fp_house_rule).

:- end_tests(monopoly_fp_house_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High. The rule extracts significant resources (all in-game fines/taxes) from their intended sink (the bank) and redirects them based on luck, fundamentally altering the game's capitalistic accumulation dynamic.
 *   - Suppression (0.65): High. The house rule is a powerful social convention. Arguing to play by the official rules is often seen as being a "spoilsport," effectively suppressing the alternative.
 *   - The combination of a genuine coordination function (making the game more "fun" for casual players) and a high, asymmetric extraction (diluting skill with luck) makes this a canonical Tangled Rope from most perspectives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant.
 *   - The Casual Player & Social Institution (Beneficiaries) see a pure Rope. For them, the rule's only function is to coordinate a better social experience.
 *   - The Strategic Player (Target) sees a Tangled Rope. They are harmed by the extraction but are also part of the social group "benefiting" from the coordination, so they cannot see it as pure extraction. The local scope of their perspective dampens the effective extraction below the Snare threshold.
 *   - The Analytical Observer sees a Snare. From a global, systemic perspective, the coordination function is aesthetic dressing on a mechanism whose primary structural effect is to extract determinism from the system. The global scope modifier (σ=1.2) amplifies the base extraction above the Snare threshold (χ > 0.66), reflecting that the systemic distortion is the most important feature.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `casual_players` benefit from increased excitement and game length. `financially-disadvantaged_players` benefit from the chance of a game-saving windfall. This drives the rule's adoption and persistence.
 *   - Victims: `strategically-dominant_players` are the primary victims. The rule is a direct tax on skill, replacing strategic outcomes with random ones. Their investment in mastering the game's formal system is devalued.
 *
 * MANDATROPHY ANALYSIS:
 *   This case perfectly illustrates how a system can misidentify a constraint without perspectival analysis. A naive analysis might label it a Rope ("it makes the game more fun") or a Snare ("it's an arbitrary tax"). The Deferential Realism framework reveals it to be both simultaneously. The analytical classification as a Snare correctly identifies its ultimate systemic impact (distortion), while the `powerless` classification as a Tangled Rope correctly captures the lived experience of the player who is forced to go along with a "fun" rule that they know is hurting their chances.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_monopoly_fp,
    'Does the "Free Parking" house rule provide a net positive player experience, or merely prolong a flawed and frustrating game?',
    'Large-scale comparative survey of player enjoyment and game-completion rates between groups using the house rule and the official rule.',
    'If net positive, its coordination function is genuine (validating the Tangled Rope view). If net negative, the coordination is illusory, and the constraint is functionally a pure Snare from all non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_monopoly_fp, conceptual, 'Is the rule a net positive experience (coordination) or just a frustrating game-prolonger (extraction)?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(monopoly_fp_house_rule, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% Models the rule's evolution from a simple mistake to an entrenched social convention.
%
% Theater ratio over time (stable, as the function was always social):
narrative_ontology:measurement(monopoly_fp_house_rule_tr_t0, monopoly_fp_house_rule, theater_ratio, 0, 0.10).
narrative_ontology:measurement(monopoly_fp_house_rule_tr_t5, monopoly_fp_house_rule, theater_ratio, 5, 0.25).
narrative_ontology:measurement(monopoly_fp_house_rule_tr_t10, monopoly_fp_house_rule, theater_ratio, 10, 0.30).

% Extraction over time (shows drift from error to core mechanic):
narrative_ontology:measurement(monopoly_fp_house_rule_ex_t0, monopoly_fp_house_rule, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(monopoly_fp_house_rule_ex_t5, monopoly_fp_house_rule, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(monopoly_fp_house_rule_ex_t10, monopoly_fp_house_rule, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The rule coordinates social fun by re-allocating game resources.
narrative_ontology:coordination_type(monopoly_fp_house_rule, resource_allocation).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */