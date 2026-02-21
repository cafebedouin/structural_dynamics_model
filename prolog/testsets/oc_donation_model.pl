% ============================================================================
% CONSTRAINT STORY: oc_donation_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_oc_donation_model, []).

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
 *   constraint_id: oc_donation_model
 *   human_readable: Open Culture's Voluntary Donation-Based Funding Model
 *   domain: economic/social
 *
 * SUMMARY:
 *   Open Culture is a web-based cultural and educational aggregator that provides
 *   free access to a vast repository of high-quality content. The organization
 *   is sustained by a voluntary donation model, where a small fraction of users
 *   provide the financial support for the entire operation. This constraint
 *   is the funding mechanism itself: a system for solving a public goods problem
 *   that relies on asymmetric, non-coercive extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Donors: Primary target (moderate/mobile) — The small subset of users who bear the financial cost of the service for everyone.
 *   - Open Culture Editors: Primary beneficiary (institutional/arbitrage) — The organization that uses the funds to operate and fulfill its mission.
 *   - Global Learners: Secondary beneficiary (powerless/mobile) — The vast majority of users who access the content for free.
 *   - Analytical Observer: Sees the full structure of coordination and asymmetric cost-bearing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oc_donation_model, 0.32).
domain_priors:suppression_score(oc_donation_model, 0.45).   % Structural property (raw, unscaled). The active fundraising suppresses the alternative of a paywall.
domain_priors:theater_ratio(oc_donation_model, 0.10).       % Piton detection (>= 0.70). Fundraising is highly functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oc_donation_model, extractiveness, 0.32).
narrative_ontology:constraint_metric(oc_donation_model, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(oc_donation_model, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(oc_donation_model, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(oc_donation_model). % Required for Tangled Rope. The "enforcement" is the continuous fundraising appeal.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(oc_donation_model, open_culture_editors).
narrative_ontology:constraint_beneficiary(oc_donation_model, global_learners).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(oc_donation_model, donors).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three satisfied).

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

% PERSPECTIVE 1: THE DONOR (PRIMARY TARGET)
% A donor feels the obligation and bears the cost. While exit is easy (mobile),
% the perceived moral weight can create a 'trapped' feeling, bearing the
% cost for the community. The engine derives d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.32 * 1.42 * 1.2(global) ≈ 0.55. This is a Tangled Rope, not a Snare,
% because the donor also values the coordination function.
constraint_indexing:constraint_classification(oc_donation_model, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN CULTURE EDITORS (PRIMARY BENEFICIARY)
% For the organization, the model is a pure coordination mechanism enabling
% its existence. The engine derives d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% This is a classic Rope perspective.
constraint_indexing:constraint_classification(oc_donation_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE NON-DONATING USER (SECONDARY BENEFICIARY)
% This user experiences the system as a pure public good with zero extraction.
% While a beneficiary, their powerless status and mobile exit leads to a
% derived d ≈ 0.15 -> f(d) ≈ -0.01 -> near-zero χ. Also a Rope.
constraint_indexing:constraint_classification(oc_donation_model, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The observer sees both the valuable coordination function and the asymmetric
% extraction from donors. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.32 * 1.15 * 1.2(global) ≈ 0.44. This falls into the Tangled Rope
% category, acknowledging both functions. This matches the constraint_claim.
constraint_indexing:constraint_classification(oc_donation_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oc_donation_model_tests).

test(perspectival_gap_donor_vs_editor, [nondet]) :-
    % Verify the gap between the donor (target) and editor (beneficiary).
    constraint_indexing:constraint_classification(oc_donation_model, tangled_rope, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(oc_donation_model, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_claim(oc_donation_model, tangled_rope),
    constraint_indexing:constraint_classification(oc_donation_model, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(oc_donation_model, _),
    narrative_ontology:constraint_victim(oc_donation_model, _),
    domain_priors:requires_active_enforcement(oc_donation_model).

:- end_tests(oc_donation_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.32): The extraction is real money, but it is voluntary and contributed by a small minority. This value is significant enough to be non-Rope but too low for a Snare, placing it squarely in the Tangled Rope category.
 *   - Suppression (0.45): The model's existence and constant fundraising appeals actively suppress the most obvious alternative: a paywall. This meets the Tangled Rope threshold (>=0.40).
 *   - The combination of a genuine coordination function (funding a public good) and asymmetric extraction (donors pay for all) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the donors and the beneficiaries (editors and non-donating users).
 *   - The Editors see a pure Rope: an elegant, mission-aligned mechanism to fund their work. Effective extraction is negative.
 *   - The Donors see a Tangled Rope: they value the service and the community (the coordination aspect) but also feel the direct financial cost and moral weight of their contribution (the extraction aspect).
 *   - Non-donating users see a pure Rope: a free service with no downside.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `open_culture_editors` and `global_learners`. The funding model directly enables the editors' work, which in turn benefits the global audience of learners. The engine derives a low `d` for these groups, resulting in a low/negative effective extraction (χ).
 *   - Victims: `donors`. This group bears the entire financial cost of the system. The engine assigns a high `d` to members of this group, resulting in a high positive χ, reflecting the felt cost of their contribution.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors.
 *   1. It is not a pure Rope. Calling it a Rope would ignore the real, asymmetric financial burden placed on donors, thus misrepresenting a system with extractive elements as pure coordination.
 *   2. It is not a Snare. The model is non-coercive, and the value provided to the community is immense. Calling it a Snare would falsely pathologize a valuable and voluntary public goods solution. The Tangled Rope classification correctly identifies the hybrid nature of the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_oc_donation_model,
    'Is the voluntary donation model a stable long-term equilibrium for funding digital public goods, or is it inherently prone to collapse from donor fatigue or free-rider saturation?',
    'Longitudinal data tracking donation rates, user growth, and operational costs for Open Culture and similar organizations over a 20+ year period.',
    'If stable, it is a robust Tangled Rope. If unstable, it is a Scaffold that appeared permanent but was always temporary, destined to be replaced by a different model (e.g., paywall, acquisition).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(oc_donation_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% While not a high-extraction constraint (ε < 0.46), modeling its drift is
% useful. We model a stable extraction rate but a slight increase in fundraising
% intensity (theater) over time as costs rise and the user base grows.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(oc_donation_model_tr_t0, oc_donation_model, theater_ratio, 0, 0.05).
narrative_ontology:measurement(oc_donation_model_tr_t5, oc_donation_model, theater_ratio, 5, 0.08).
narrative_ontology:measurement(oc_donation_model_tr_t10, oc_donation_model, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(oc_donation_model_ex_t0, oc_donation_model, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(oc_donation_model_ex_t5, oc_donation_model, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(oc_donation_model_ex_t10, oc_donation_model, base_extractiveness, 10, 0.32).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This model is a mechanism for pooling and allocating financial resources.
narrative_ontology:coordination_type(oc_donation_model, resource_allocation).

% Network relationships (structural influence edges)
% This model exists in an ecosystem with other content funding models.
% Its success or failure influences the viability of others.
narrative_ontology:affects_constraint(oc_donation_model, ad_supported_media_model).
narrative_ontology:affects_constraint(paywalled_content_model, oc_donation_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% from beneficiary/victim declarations and exit options accurately models
% the structural relationships and produces the correct perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */