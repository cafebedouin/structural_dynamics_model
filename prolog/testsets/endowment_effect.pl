% ============================================================================
% CONSTRAINT STORY: endowment_effect
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_endowment_effect, []).

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
 *   constraint_id: endowment_effect
 *   human_readable: The Endowment Effect
 *   domain: economic/cognitive
 *
 * SUMMARY:
 *   The endowment effect is a finding in behavioral economics where individuals
 *   ascribe more value to things merely because they own them. This leads to
 *   an asymmetry between the "Willingness to Pay" (WTP) and the "Willingness
 *   to Accept" (WTA), often resulting in market inefficiencies where
 *   beneficial trades do not occur. It functions as a cognitive bias rooted
 *   in loss aversion.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Imprisoned Heir (powerless/constrained): Primary target — bears the extraction of opportunity cost, unable to sell an overvalued asset despite needing liquidity.
 *   - The Car Salesman (institutional/arbitrage): Primary beneficiary — leverages the effect via "try-before-you-buy" tactics to induce ownership and close sales.
 *   - The Behavioral Economist (analytical/trapped): Analytical observer — views the effect as a fixed feature of human cognitive architecture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low. The effect extracts opportunity cost and market liquidity,
% not direct resources. It prevents efficient allocation. The value is below
% the mountain threshold of 0.25.
domain_priors:base_extractiveness(endowment_effect, 0.15).

% Rationale: Very low. The effect is a cognitive default; it doesn't actively
% suppress rational calculation but rather preempts it through emotional weighting.
% The value is below the mountain threshold of 0.05.
domain_priors:suppression_score(endowment_effect, 0.05).

% Rationale: Very low. The effect is a genuine cognitive process, not a
% performative or theatrical one.
domain_priors:theater_ratio(endowment_effect, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endowment_effect, extractiveness, 0.15).
narrative_ontology:constraint_metric(endowment_effect, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(endowment_effect, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: High. Rational valuation is theoretically possible but
% structurally inaccessible due to the cognitive barrier of loss aversion.
narrative_ontology:constraint_metric(endowment_effect, accessibility_collapse, 0.95).
% Resistance: Low. One cannot meaningfully "resist" a cognitive bias in an
% organized way; one can only be trained to recognize and mitigate it.
narrative_ontology:constraint_metric(endowment_effect, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(endowment_effect, mountain).

% --- Emergence flag (required for mountain constraints) ---
% The effect is a feature of human cognitive architecture, not a designed system.
domain_priors:emerges_naturally(endowment_effect).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although the analytical view is Mountain, the effect has beneficiaries and
% victims in practice, which drives the perspectival gaps.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(endowment_effect, existing_owners).
narrative_ontology:constraint_beneficiary(endowment_effect, retailers_offering_trials).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(endowment_effect, potential_asset_purchasers).
narrative_ontology:constraint_victim(endowment_effect, owners_needing_liquidity).

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

% PERSPECTIVE 1: THE IMPRISONED HEIR (SNARE)
% For an heir needing money but unable to sell a family estate, the effect is a
% Snare. Their own psychology strangles their financial future. The value they
% ascribe to the "endowment" prevents them from taking the "exit" that market
% reality offers, extracting their potential wealth and freedom.
constraint_indexing:constraint_classification(endowment_effect, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CAR SALESMAN (ROPE)
% For the institutional seller, the effect is a Rope. It is a functional
% coordination mechanism for closing deals. By encouraging a "test drive" or
% "free home trial," they induce a sense of ownership, pulling the customer
% toward a purchase through the psychological "tether" of the endowment effect.
constraint_indexing:constraint_classification(endowment_effect, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE BEHAVIORAL ECONOMIST (MOUNTAIN)
% To the scientist, this is a Mountain. It is an unchangeable consequence of
% how the human brain processes loss and ownership. You cannot simply "choose"
% not to feel the loss aversion that drives the effect; it is a feature of the
% biological "terrain".
constraint_indexing:constraint_classification(endowment_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endowment_effect_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and observer.
    constraint_indexing:constraint_classification(endowment_effect, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(endowment_effect, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(endowment_effect, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base metrics align with the mountain classification.
    narrative_ontology:constraint_metric(endowment_effect, extractiveness, E),
    narrative_ontology:constraint_metric(endowment_effect, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(endowment_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics (ε=0.15, S=0.05) were set to align with the properties of a
 *   cognitive bias, which is a feature of the natural landscape rather than a
 *   coercive, designed system. This ensures it correctly classifies as a
 *   Mountain from the analytical, civilizational perspective, passing the
 *   natural law certification chain with high accessibility_collapse (0.95)
 *   and low resistance (0.05). The claim is set to 'mountain' to reflect this
 *   foundational view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The analytical observer sees an immutable Mountain of
 *   human psychology. The institutional seller (beneficiary) sees a Rope to
 *   be used as a sales tool (e.g., test drives). The powerless individual
 *   (victim) experiences it as a Snare, where their own mind traps them in
 *   an inefficient financial position, extracting opportunity cost.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'existing_owners' who benefit from inflated valuations, and 'retailers_offering_trials' who leverage the effect to create attachment and drive sales.
 *   - Victims: 'potential_asset_purchasers' who face artificially high prices, and 'owners_needing_liquidity' who are trapped by their own psychological attachment, preventing them from making rational financial decisions.
 *   This structure correctly assigns low directionality (d) to institutional agents with arbitrage and high (d) to powerless agents who are constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the core phenomenon as a Mountain
 *   (a fact of cognitive architecture) while also modeling how this "natural"
 *   feature can be instrumentalized as a Rope (by sellers) or experienced as
 *   a Snare (by trapped owners). This prevents mislabeling the entire phenomenon
 *   as just a Snare, recognizing its non-designed, emergent origin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_endowment_effect,
    'Is the endowment effect an evolved biological heuristic (Mountain) or a learned social norm related to property rights (Rope)?',
    'Comparative cross-cultural studies of societies with differing property concepts.',
    'If Mountain: A fixed barrier to market efficiency. If Rope: Can be de-conditioned or unlearned through cultural shifts.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_endowment_effect, empirical, 'Distinguishing evolved biological origin from learned social norm.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(endowment_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.15) is below the
% 0.46 threshold for mandatory lifecycle drift tracking. The constraint is
% considered stable over human history.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No coordination type or network relationships are applicable for a
% fundamental cognitive bias.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */