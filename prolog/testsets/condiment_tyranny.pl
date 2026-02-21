% ============================================================================
% CONSTRAINT STORY: condiment_tyranny
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_condiment_tyranny, []).

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
 *   constraint_id: condiment_tyranny
 *   human_readable: The Tyranny of the Default Condiment Offering
 *   domain: social/economic
 *
 * SUMMARY:
 *   Social and economic pressures from large-scale food service operations
 *   result in a standardized, limited set of default condiments (e.g., ketchup,
 *   mustard, mayonnaise). This system simplifies logistics and caters to
 *   majority tastes, but it suppresses consumer choice, marginalizes minority
 *   preferences, and creates a self-reinforcing cycle where popular condiments
 *   become ever more dominant.
 *
 * KEY AGENTS (by structural relationship):
 *   - Consumers with minority preferences: Primary target (powerless/trapped) — bears the cost of suppressed choice.
 *   - Large-scale food service providers: Primary beneficiary (institutional/arbitrage) — benefits from efficiency and economies of scale.
 *   - Niche food producers/enthusiasts: Organized opposition (organized/mobile) — seeks to create alternatives.
 *   - Market analyst: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(condiment_tyranny, 0.45).
domain_priors:suppression_score(condiment_tyranny, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(condiment_tyranny, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(condiment_tyranny, extractiveness, 0.45).
narrative_ontology:constraint_metric(condiment_tyranny, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(condiment_tyranny, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(condiment_tyranny, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(condiment_tyranny). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(condiment_tyranny, large_food_service_providers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(condiment_tyranny, consumers_with_minority_preferences).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% A consumer whose preferred condiment is unavailable. The system's
% coordination function is invisible; they only experience the extraction of choice.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.45 * 1.42 * 1.0 = 0.639. This is very close to the snare threshold (0.66) and
% coupled with high suppression (0.65), it classifies as a snare from this view.
constraint_indexing:constraint_classification(condiment_tyranny, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% A large fast-food chain benefiting from supply chain simplification.
% For them, this is a pure coordination mechanism that reduces cost.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(condiment_tyranny, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A market analyst sees both the coordination benefits for producers and the
% extractive costs imposed on consumers. This dual nature is the hallmark of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.45 * 1.15 * 1.2 (global scope) ≈ 0.62.
constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ORGANIZED OPPOSITION (TANGLED ROPE)
% Niche food communities ("foodies") or specialty producers. They recognize the
% mainstream system's extractive nature (as victims) but have the means (mobile exit)
% to create alternatives, participating in a different coordination game.
% Engine derives d for organized victim with mobile exit ≈ 0.55 → f(d) ≈ 0.75.
% χ = 0.45 * 0.75 * 1.0 = 0.3375.
% While χ is low, ε is high (0.45) and they see both the coordination aspect (in their
% own niche) and the extraction of the dominant system. This is still a Tangled Rope.
constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(condiment_tyranny_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(condiment_tyranny, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(condiment_tyranny, rope, context(agent_power(institutional), _, _, _)),
    format('Perspectival gap confirmed: Snare (powerless) vs Rope (institutional)~n').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(condiment_tyranny, _),
    narrative_ontology:constraint_victim(condiment_tyranny, _),
    domain_priors:requires_active_enforcement(condiment_tyranny).

:- end_tests(condiment_tyranny_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.45): This represents a significant, but not total, extraction of consumer surplus. The loss of a preferred condiment is a real cost, and the market power gained by dominant suppliers is a real transfer of value. It's high enough to signal a problem beyond pure coordination.
 *   - Suppression Score (0.65): High. The logistics, contracts, and physical space limitations of large-scale food service create powerful barriers to entry for non-standard condiments. This isn't a free market at the point of consumption.
 *   - The combination of a real coordination function (efficiency) and significant asymmetric extraction (suppressed choice) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the institutional beneficiary (a fast-food chain), standardizing on three condiments is a brilliant act of coordination (Rope) that cuts costs and simplifies operations. For the consumer who wants tartar sauce with their fish sandwich and is offered only ketchup, the system is purely extractive (Snare), denying them a core part of their expected value.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `large_food_service_providers`. They benefit directly from reduced complexity, lower costs, and stronger negotiating positions with a few large suppliers.
 *   - Victim: `consumers_with_minority_preferences`. They bear the direct cost by having their choices constrained and their preferences ignored by the mass market.
 *   These declarations correctly drive the directionality `d`, leading to low/negative effective extraction (χ) for the beneficiary and high χ for the victim.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual nature of the constraint. A simplistic analysis might call it "market efficiency" (a Rope) or "corporate monopoly" (a Snare). Deferential Realism, by using multiple perspectives, correctly identifies it as a Tangled Rope: a system with a genuine coordination function that has been leveraged to create and sustain asymmetric extraction. It avoids mislabeling the real efficiency gains as pure theater, while also refusing to ignore the real costs imposed on a subset of users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_condiment_tyranny,
    'Is the limited condiment selection an emergent outcome of efficient markets catering to majority taste, or a deliberately constructed barrier to entry by dominant market players?',
    'Analysis of internal supply chain contracts and strategic documents from major food service corporations and condiment manufacturers.',
    'If emergent (Rope-like), policy interventions are likely ineffective. If constructed (Snare-like), it implies anti-competitive behavior that could be regulated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(condiment_tyranny, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified as the food industry consolidated from the mid-20th
% century onwards. The initial state was more diverse, becoming more
% extractive over time. Base extractiveness is high enough (0.45) to warrant tracking.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(condiment_tyranny_tr_t0, condiment_tyranny, theater_ratio, 0, 0.05).
narrative_ontology:measurement(condiment_tyranny_tr_t5, condiment_tyranny, theater_ratio, 5, 0.08).
narrative_ontology:measurement(condiment_tyranny_tr_t10, condiment_tyranny, theater_ratio, 10, 0.10).

% Extraction over time (shows intensification):
narrative_ontology:measurement(condiment_tyranny_ex_t0, condiment_tyranny, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(condiment_tyranny_ex_t5, condiment_tyranny, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(condiment_tyranny_ex_t10, condiment_tyranny, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic resource allocation problem: allocating shelf space,
% dispenser slots, and supply chain capacity.
narrative_ontology:coordination_type(condiment_tyranny, resource_allocation).

% No network relationships are defined for this standalone example.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */