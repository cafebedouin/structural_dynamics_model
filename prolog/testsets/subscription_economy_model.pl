% ============================================================================
% CONSTRAINT STORY: subscription_economy_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_subscription_economy_model, []).

:- use_module(library(plunit)).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).
:- use_module(config).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: subscription_economy_model
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from one-time
 *   product sales (perpetual licenses) to recurring subscription fees. While
 *   offering a coordination benefit (continuous updates, service access),
 *   it asymmetrically extracts value by creating consumer inertia, making
 *   cancellation difficult (a "hassle tax"), and revoking ownership rights.
 *   The model is actively enforced by DRM and the suppression of non-subscription
 *   alternatives.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Investors/Venture Capital: Secondary beneficiary (institutional/arbitrage) - reward companies with subscription models with higher market valuations.
 *   - Analytical Observer: Sees the full structure as a hybrid of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model, 0.52).
domain_priors:suppression_score(subscription_economy_model, 0.85).   % Structural property (raw, unscaled). High due to phasing out perpetual licenses.
domain_priors:theater_ratio(subscription_economy_model, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(subscription_economy_model, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(subscription_economy_model, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(subscription_economy_model, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(subscription_economy_model). % Required for Tangled Rope (enforced by DRM, ToS)

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(subscription_economy_model, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model, investors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(subscription_economy_model, individual_consumers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculation: χ = 0.52 * f(0.95) * σ(national) ≈ 0.52 * 1.42 * 1.0 ≈ 0.74
% This meets the Snare criteria (χ ≥ 0.66, s ≥ 0.60).
%
% NOTE: Per "Dynamic Coalition" extension, a large enough group of consumers
% could become 'organized', which would shift their power and potentially
% the classification from their perspective.
constraint_indexing:constraint_classification(subscription_economy_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% Calculation: χ = 0.52 * f(0.05) * σ(national) ≈ 0.52 * -0.12 * 1.0 ≈ -0.06
% The negative extraction signifies a subsidy. This is a clear Rope.
constraint_indexing:constraint_classification(subscription_economy_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% Calculation: χ = 0.52 * f(0.72) * σ(global) ≈ 0.52 * 1.15 * 1.2 ≈ 0.72
% Meets Tangled Rope criteria: 0.40 ≤ χ ≤ 0.90, ε ≥ 0.30, s ≥ 0.40.
% The system requires a beneficiary, victim, and enforcement, all of which are declared.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between the consumer and the corporation.
    constraint_indexing:constraint_classification(subscription_economy_model, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(subscription_economy_model, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Verify the analytical classification is Tangled Rope.
    constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that the structural requirements for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(subscription_economy_model, _),
    narrative_ontology:constraint_victim(subscription_economy_model, _),
    domain_priors:requires_active_enforcement(subscription_economy_model).

:- end_tests(subscription_economy_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.52) is high, reflecting the significant value
 *   transfer from consumers (via hassle taxes, lost ownership) to corporations.
 *   Suppression (s=0.85) is also very high, as the primary strategy for
 *   enforcing this model is the systematic elimination of perpetual-license
 *   alternatives, leaving consumers with no other option. Theater is low (t=0.20)
 *   as the services provided are generally functional, even if the business
 *   model is extractive.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: Snare vs. Rope. For corporations and investors
 *   (beneficiaries), the model is a pure Rope: a superior coordination
 *   mechanism for managing revenue, updates, and customer relationships, which
 *   the market rewards. For an individual consumer (victim), the lack of exit
 *   options, opaque cancellation processes, and loss of ownership make it a
 *   Snare that extracts recurring fees with minimal recourse.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The flow of value is from a large,
 *   disorganized group ('individual_consumers') to a small, highly organized
 *   group ('subscription_based_corporations' and 'investors'). The `victim`
 *   and `beneficiary` declarations directly map to this structural reality.
 *   The engine correctly derives a high 'd' value for consumers (especially
 *   when trapped) and a very low 'd' for corporations (with arbitrage exit),
 *   driving the large gap in effective extraction (χ) and thus classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a canonical example of a Tangled Rope, preventing the
 *   misclassification of the system as either a pure Snare or a pure Rope.
 *   A purely anti-corporate analysis might miss the genuine coordination
 *   function (updates, cloud services) and label it a Snare. A purely
 *   pro-business analysis would ignore the massive extraction and suppression
 *   and label it a Rope. The Tangled Rope classification correctly identifies
 *   it as a system that *uses* a coordination function to *enable* asymmetric
 *   extraction, which is its key structural feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_subscription_economy_model,
    'Is the "value" of continuous updates a genuine coordination benefit or primarily a pretext for enforcing the subscription model?',
    'Comparative studies of total cost of ownership (TCO) and consumer surplus between perpetual and subscription models over a 10-year period.',
    'If the value is genuine, the Rope component is strong and the ε might be slightly lower. If it is a pretext, the constraint is closer to a pure Snare and ε is even higher.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(subscription_economy_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model intensified over the last two decades. The data shows
% extraction_accumulation as the model shifted from niche to dominant.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(sem_tr_t0, subscription_economy_model, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sem_tr_t5, subscription_economy_model, theater_ratio, 5, 0.15).
narrative_ontology:measurement(sem_tr_t10, subscription_economy_model, theater_ratio, 10, 0.20).

% Extraction over time (shows significant accumulation):
% T=0 (c. 2005): Early SaaS, niche model.
narrative_ontology:measurement(sem_ex_t0, subscription_economy_model, base_extractiveness, 0, 0.15).
% T=5 (c. 2013): Adobe Creative Cloud transition, model gains momentum.
narrative_ontology:measurement(sem_ex_t5, subscription_economy_model, base_extractiveness, 5, 0.35).
% T=10 (c. 2023): Model is dominant, perpetual licenses are rare.
narrative_ontology:measurement(sem_ex_t10, subscription_economy_model, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This model allocates access to a digital resource based on payment status.
narrative_ontology:coordination_type(subscription_economy_model, resource_allocation).

% Network relationships (structural influence edges)
% The subscription model relies heavily on DRM for enforcement and impacts
% how consumer data is handled.
narrative_ontology:affects_constraint(subscription_economy_model, drm_enforcement).
narrative_ontology:affects_constraint(subscription_economy_model, consumer_data_privacy_erosion).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The automatic derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics between consumers and corporations.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */