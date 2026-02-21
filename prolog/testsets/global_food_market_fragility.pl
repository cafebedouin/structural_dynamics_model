% ============================================================================
% CONSTRAINT STORY: global_food_market_fragility
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_global_food_market_fragility, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: global_food_market_fragility
 *   human_readable: "Global Food Market Fragility to Correlated Crop Failures"
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The global food market is a complex system for distributing staple crops.
 *   While providing a crucial coordination function, its structure also creates
 *   and amplifies fragility. New AI models (like OIST's ACRE) reveal that
 *   climate change induces correlated failures across multiple "breadbasket"
 *   regions. This physical shock is transmitted through the global market,
 *   where price volatility, speculative trading, and unequal purchasing power
 *   asymmetrically harm vulnerable populations and nations while creating
 *   arbitrage opportunities for powerful actors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Food-Import-Dependent Nations: Primary target (powerless/trapped) — bear the costs of price spikes and supply shocks.
 *   - Agricultural Commodity Traders: Primary beneficiary (institutional/arbitrage) — profit from volatility and control logistics.
 *   - International Aid Organizations (e.g., WFP): Secondary actor (institutional/constrained) — attempt to mitigate harm but must operate within the market's constraints.
 *   - OIST Climate Scientists: Analytical observer — model the underlying physical risks and their systemic impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_food_market_fragility, 0.55). % High potential for wealth transfer via price volatility/speculation.
domain_priors:suppression_score(global_food_market_fragility, 0.70).   % Structural property (raw, unscaled). High due to lack of alternatives for import-dependent nations.
domain_priors:theater_ratio(global_food_market_fragility, 0.30).       % Piton detection (>= 0.70). Moderate theater around humanitarian aid vs. core market function.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_food_market_fragility, extractiveness, 0.55).
narrative_ontology:constraint_metric(global_food_market_fragility, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(global_food_market_fragility, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this Tangled Rope.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(global_food_market_fragility, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(global_food_market_fragility). % Required for Tangled Rope. Enforced by trade laws, financial regulations, shipping logistics.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(global_food_market_fragility, agricultural_commodity_traders).
narrative_ontology:constraint_beneficiary(global_food_market_fragility, food_surplus_nations).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(global_food_market_fragility, food_import_dependent_nations).
narrative_ontology:constraint_victim(global_food_market_fragility, low_income_consumers_globally).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met).

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
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This actor experiences the market as purely extractive during a crisis.
constraint_indexing:constraint_classification(global_food_market_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% This actor sees an efficient coordination mechanism for resource allocation.
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function and the
% asymmetric, enforced extraction. This is the basis for the constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for this perspective.
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% An institutional actor that is NOT a primary beneficiary, but is constrained
% by the system's logic. Its `d` value will be higher than the beneficiary's.
%
% Perspective 4: International Aid Organization (e.g., WFP)
% It sees the system's extractive failures, but must leverage its coordination
% function. The `constrained` exit option results in a higher `d` than the
% beneficiary's `arbitrage` exit, revealing the system's dual nature.
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_food_market_fragility_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the powerless target and the institutional beneficiary.
    constraint_indexing:constraint_classification(global_food_market_fragility, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(global_food_market_fragility, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Passed: Target (Snare) and Beneficiary (Rope) perspectives correctly diverge.~n').

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical perspective correctly identifies the mixed nature of the constraint.
    constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Passed: Analytical perspective correctly classifies as Tangled Rope.~n').

test(tangled_rope_structural_requirements) :-
    % Verify that all three structural predicates for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(global_food_market_fragility, _),
    narrative_ontology:constraint_victim(global_food_market_fragility, _),
    domain_priors:requires_active_enforcement(global_food_market_fragility),
    format('Passed: All three structural requirements for Tangled Rope are met.~n').


:- end_tests(global_food_market_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This high value reflects the capacity of the global food market
 *     to generate enormous profits from price volatility, which is a direct wealth transfer from
 *     consumers (especially the most vulnerable) to financial and logistical actors. It's not just
 *     about a simple markup, but about the extractive potential during systemic shocks.
 *   - Suppression (0.70): For an import-dependent nation facing a food crisis, there are no
 *     short-term alternatives to the global market. This lack of exit options is a core feature of
 *     the constraint, justifying the high suppression score.
 *   - Tangled Rope Classification: The system genuinely coordinates the movement of food globally
 *     (a Rope-like function), which is why beneficiaries see it as a Rope. However, this is
 *     inseparable from an extractive structure enforced by trade and financial rules that
 *     asymmetrically harms the vulnerable (a Snare-like function). The presence of both a genuine
 *     coordination function and enforced, asymmetric extraction makes it a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a commodity trader with arbitrage exit, the system is a Rope: an
 *   information-rich environment for coordinating supply and demand for profit. For a food-importing
 *   nation with trapped exit, it is a Snare: a non-negotiable system that can impose catastrophic
 *   price hikes, leading to famine and social unrest. The classification difference (Rope vs. Snare)
 *   is driven by the `d` (directionality) parameter, which the engine derives from their structural
 *   positions as beneficiary vs. victim and their respective exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries (traders, surplus nations) profit from the system's structure, especially its
 *     volatility. They are structurally positioned to extract value. Their low derived `d` gives
 *     a negative effective extraction (χ), reflecting a subsidy.
 *   - Victims (import-dependent nations, poor consumers) bear the costs of this volatility.
 *     They are structurally targeted for extraction. Their high derived `d` results in a very high
 *     positive χ, reflecting the severe costs imposed on them.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not label the entire global
 *   food market as a pure Snare, which would ignore its vital coordination function. Nor does it
 *   accept the beneficiary's view of it as a pure Rope, which would ignore the immense, enforced
 *   extraction it enables. The Tangled Rope classification captures this duality, identifying a
 *   system where the coordination mechanism itself is the vector for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_global_food_market_fragility,
    'Is the extreme market volatility an unfortunate but unavoidable emergent property of a complex system, or a deliberately engineered/maintained feature that facilitates extraction?',
    'Detailed forensic analysis of trading data and regulatory lobbying during past food crises (e.g., 2007-08) to trace the influence of speculative actors vs. fundamental supply/demand signals.',
    'If emergent, the constraint is a "natural" Tangled Rope requiring mitigation. If engineered, it is a "designed" Tangled Rope closer to a Snare, suggesting targeted regulatory intervention is possible.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(global_food_market_fragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint's extractiveness has likely increased over the last few decades
% due to financialization and increased climate volatility.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (humanitarian discourse growing to mask market function):
narrative_ontology:measurement(gfmf_tr_t0, global_food_market_fragility, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gfmf_tr_t5, global_food_market_fragility, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gfmf_tr_t10, global_food_market_fragility, theater_ratio, 10, 0.30).

% Extraction over time (financialization and climate volatility increasing extractive potential):
narrative_ontology:measurement(gfmf_ex_t0, global_food_market_fragility, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(gfmf_ex_t5, global_food_market_fragility, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(gfmf_ex_t10, global_food_market_fragility, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(global_food_market_fragility, resource_allocation).

% --- Network Decomposition (Constraint Families) ---
% This constraint (a socio-economic Tangled Rope) is structurally dependent
% on an underlying physical constraint (a Mountain).

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from the concept of "global food security risk".
% Decomposed because ε differs across observables (ε-invariance principle).
% The physical reality of crop correlation is a Mountain; the market that
% transmits that shock is a Tangled Rope.
% Related stories:
%   - cc_crop_correlation (ε=0.05, Mountain)
%
narrative_ontology:affects_constraint(cc_crop_correlation, global_food_market_fragility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain,
% using beneficiary/victim declarations and distinct exit_options (arbitrage,
% trapped, constrained), accurately models the directionality for all key
% agents without manual adjustment.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */