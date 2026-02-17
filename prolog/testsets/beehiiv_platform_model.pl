% ============================================================================
% CONSTRAINT STORY: beehiiv_platform_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_beehiiv_platform_model, []).

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
 *   constraint_id: beehiiv_platform_model
 *   human_readable: The Beehiiv Newsletter Platform Business Model
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Beehiiv is a newsletter platform that provides creators with tools for
 *   publishing, audience growth, and monetization. The constraint is the
 *   platform's terms of service and tiered subscription model, which offers
 *   genuine coordination benefits (tools, infrastructure) while also
 *   asymmetrically extracting value from creators via monthly fees and
 *   creating soft lock-in through high switching costs.
 *
 * KEY AGENTS (by structural relationship):
 *   - Aspiring Creators: Primary target (powerless/trapped) — new creators with no audience, fully dependent on the platform.
 *   - Established Creators: Secondary target (moderate/constrained) — creators with an audience who bear costs but have some leverage.
 *   - Beehiiv Platform (Company): Primary beneficiary (institutional/arbitrage) — benefits from aggregated creator fees and control over the ecosystem.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beehiiv_platform_model, 0.48).
domain_priors:suppression_score(beehiiv_platform_model, 0.55).   % Structural property (raw, unscaled). Moderated by competitors (Substack, Ghost) but increased by switching costs.
domain_priors:theater_ratio(beehiiv_platform_model, 0.15).       % Piton detection (>= 0.70). Currently low; platform is feature-focused.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beehiiv_platform_model, extractiveness, 0.48).
narrative_ontology:constraint_metric(beehiiv_platform_model, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(beehiiv_platform_model, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(beehiiv_platform_model, tangled_rope).
narrative_ontology:human_readable(beehiiv_platform_model, "The Beehiiv Newsletter Platform Business Model").

% --- Binary flags ---
domain_priors:requires_active_enforcement(beehiiv_platform_model). % Required for Tangled Rope. Enforced via ToS, payment processing, and platform moderation.

% --- Emergence flag (required for mountain constraints) ---
% N/A, this is a human-designed system.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, beehiiv_platform).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(beehiiv_platform_model, independent_creators).

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
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ASPIRING CREATOR (POWERLESS TARGET)
% A new creator with no audience, completely dependent on the platform.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42
% χ ≈ 0.48 * 1.42 * 1.2 (global scope) ≈ 0.82. This is a highly extractive Tangled Rope.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ESTABLISHED CREATOR (MODERATE TARGET)
% Bears the cost of fees and platform lock-in, but has a portable audience.
% Engine derives d from: victim membership + constrained exit -> d ≈ 0.90 -> f(d) ≈ 1.35
% χ ≈ 0.48 * 1.35 * 1.2 (global scope) ≈ 0.78. Still a Tangled Rope, but less extractive.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PLATFORM (PRIMARY BENEFICIARY)
% Experiences the constraint as a pure coordination mechanism for generating revenue.
% Engine derives d from: beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12
% χ ≈ 0.48 * -0.12 * 1.2 (global scope) ≈ -0.07. This is a Rope.
constraint_indexing:constraint_classification(beehiiv_platform_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.73 -> f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66. This confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beehiiv_platform_model_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the powerless target and the institutional beneficiary.
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == tangled_rope),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_gates_pass) :-
    % A Tangled Rope requires all three of these structural facts to be declared.
    narrative_ontology:constraint_beneficiary(beehiiv_platform_model, _),
    narrative_ontology:constraint_victim(beehiiv_platform_model, _),
    domain_priors:requires_active_enforcement(beehiiv_platform_model).

:- end_tests(beehiiv_platform_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): The model is explicitly extractive via paid tiers. The value reflects a significant but not purely predatory cost in exchange for real services. It's high enough to trigger Tangled Rope conditions.
 *   - Suppression (0.55): While alternatives like Substack and Ghost exist, switching costs for established creators (migrating subscribers, SEO, learning curve) are substantial, creating a form of lock-in that suppresses alternatives in practice.
 *   - The combination of a clear coordination function (publishing/growth tools) and significant, asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Platform (Rope) and Creators (Tangled Rope) is fundamental to the platform economy.
 *   - The Platform (beneficiary, arbitrage exit) perceives the system as a tool for coordinating a market, where its revenue is a justified outcome of providing value. The negative effective extraction (χ < 0) reflects this subsidy-like perspective.
 *   - An Aspiring Creator (powerless, trapped) is fully dependent on the platform and experiences a highly extractive Tangled Rope (χ ≈ 0.82).
 *   - An Established Creator (moderate, constrained) has more leverage but still experiences the system as an extractive Tangled Rope (χ ≈ 0.78) due to high switching costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the core business model. Beehiiv (the company) is the explicit `beneficiary`, as its revenue and valuation depend on the successful operation of the constraint. The `independent_creators` are the `victims`, as they are the group from whom value is extracted via subscription fees to access the coordination tools. This structural relationship is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the platform. A naive analysis might label it a Snare (focusing only on lock-in and fees) or a Rope (focusing only on the useful tools). The Tangled Rope classification avoids Mandatrophy by acknowledging both are present: there is a genuine coordination function (`constraint_beneficiary` is declared) AND asymmetric extraction (`constraint_victim` is declared) maintained by active enforcement (`requires_active_enforcement`). This captures the essential tension of the creator economy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_beehiiv_platform_model,
    'Will Beehiiv''s platform incentives shift towards greater extraction over time as it scales and answers to investors, or will market competition keep it aligned with creator interests?',
    'Longitudinal analysis of pricing tiers, revenue splits on new monetization features (e.g., ad network), and creator churn rates over a 5-10 year period.',
    'If extraction increases, the constraint drifts towards a Snare (ε > 0.60). If it remains stable or decreases, it remains a Tangled Rope or could even soften towards a Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(beehiiv_platform_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Since base_extractiveness (0.48) > 0.46,
% this is required. We model a typical startup lifecycle: initial low-extraction
% focus on user acquisition, followed by increasing monetization pressure as the
% platform matures.

% Theater ratio over time (slight increase with marketing scale):
narrative_ontology:measurement(beehiiv_tr_t0, beehiiv_platform_model, theater_ratio, 0, 0.10).
narrative_ontology:measurement(beehiiv_tr_t5, beehiiv_platform_model, theater_ratio, 5, 0.12).
narrative_ontology:measurement(beehiiv_tr_t10, beehiiv_platform_model, theater_ratio, 10, 0.15).

% Extraction over time (increasing monetization focus):
narrative_ontology:measurement(beehiiv_ex_t0, beehiiv_platform_model, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(beehiiv_ex_t5, beehiiv_platform_model, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(beehiiv_ex_t10, beehiiv_platform_model, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It's a vertically integrated stack for creators.
narrative_ontology:coordination_type(beehiiv_platform_model, global_infrastructure).

% Network relationships: This constraint exists in a competitive ecosystem
% with other platform models.
narrative_ontology:affects_constraint(beehiiv_platform_model, substack_platform_model).
narrative_ontology:affects_constraint(beehiiv_platform_model, creator_economy_viability).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics between the platform and its creators.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */