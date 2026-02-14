% ============================================================================
% CONSTRAINT STORY: meta_pay_or_okay_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-03
% ============================================================================

:- module(constraint_meta_pay_or_okay_model, []).

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
 *   constraint_id: meta_pay_or_okay_model
 *   human_readable: Meta's "Pay or Okay" Data Monetization Model in the EU
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models Meta's "Pay or Okay" policy for EU users, where
 *   access to services like Facebook and Instagram requires either consent
 *   for personal data processing for targeted ads or payment of a subscription
 *   fee. The Austrian Supreme Court, aligning with the CJEU, ruled this model
 *   illegal under GDPR for not providing a genuine, free, less data-intensive
 *   alternative, effectively making consent coerced. The constraint structures
 *   the user's choice as a trade-off between privacy and money.
 *
 * KEY AGENTS (by structural relationship):
 *   - EU Platform Users: Primary target (powerless/trapped) — bear extraction of data or fees.
 *   - Meta Platforms Inc.: Primary beneficiary (institutional/arbitrage) — extracts value to fund operations and generate profit.
 *   - EU Courts & Regulators: Inter-institutional actor (institutional/constrained) — tasked with enforcing GDPR, acting to dismantle the constraint.
 *   - Privacy Advocacy Groups (e.g., noyb): Organized opposition (organized/mobile) — represents users, leveraging legal system to challenge the constraint.
 *   - Digital Advertisers: Secondary beneficiary (powerful/mobile) — gain access to targeted audiences.
 *   - Analytical Observer: Analytical perspective — sees the full coercive structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_pay_or_okay_model, 0.75).
domain_priors:suppression_score(meta_pay_or_okay_model, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(meta_pay_or_okay_model, 0.15).       % Low theater; this is a core functional mechanism.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_pay_or_okay_model, extractiveness, 0.75).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(meta_pay_or_okay_model, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(meta_pay_or_okay_model). % Enforced via technical access controls.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, meta_platforms).
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, digital_advertisers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(meta_pay_or_okay_model, eu_platform_users).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, privacy_advocacy_groups). % Aligned with users
narrative_ontology:constraint_victim(meta_pay_or_okay_model, eu_courts_and_regulators). % Structurally opposed, bearing enforcement costs

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, ..., continental=1.1, global=1.2.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (EU PLATFORM USERS)
% Individually powerless and trapped by network effects, they face a coercive
% choice. Engine derives d from victim + trapped → d≈0.95 → high f(d) → high χ.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (META)
% The model is a highly efficient mechanism for monetizing its platform.
% Engine derives d from beneficiary + arbitrage → d≈0.05 → negative f(d) → negative χ.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees the structure as a highly effective extractive mechanism that suppresses
% user-centric alternatives. The high ε and suppression lead to a Snare classification.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: EU COURTS & REGULATORS
% An institutional actor whose mandate forces it to oppose the constraint.
% Its exit is constrained by its jurisdiction and legal framework (GDPR).
% By listing it as a 'victim', we model its structural opposition; the engine
% derives a higher d than for Meta, correctly classifying the model as a Snare
% from the regulatory viewpoint.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4B: PRIVACY ADVOCACY GROUPS (e.g., noyb)
% Representing users, these groups form a dynamic coalition. Their power is
% 'organized', not 'powerless'. They have 'mobile' exit as they can shift legal
% and public pressure tactics. They still see a Snare, but their organization
% gives them more agency than a trapped individual.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_pay_or_okay_model_tests).

test(perspectival_gap_user_vs_platform) :-
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypeUser, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypePlatform, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeUser == snare),
    assertion(TypePlatform == rope),
    TypeUser \= TypePlatform.

test(inter_institutional_gap_regulator_vs_platform) :-
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypeRegulator, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypePlatform, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeRegulator == snare),
    assertion(TypePlatform == rope),
    TypeRegulator \= TypePlatform.

test(snare_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(meta_pay_or_okay_model, ExtMetricName, E),
    narrative_ontology:constraint_metric(meta_pay_or_okay_model, SuppMetricName, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(meta_pay_or_okay_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Set high to reflect the immense value of personal
 *     data for targeted advertising, or the direct monetary value of the subscription
 *     fee. The choice architecture is designed to maximize this extraction.
 *   - Suppression (S=0.85): The model's core function is to suppress a third
 *     alternative: free usage with genuine privacy. The Austrian court's ruling
 *     confirms that this suppression is legally significant, making consent
 *     non-viable.
 *   - Analytical Claim (Snare): The combination of high extraction and high
 *     suppression, with a minimal coordination function *in the choice mechanism itself*,
 *     makes this a canonical Snare. While it funds a coordination platform, the
 *     mechanism is pure extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a user, it's a Snare that extracts value (data or money)
 *   coercively. For Meta, it's a Rope—an elegant, efficient coordination mechanism
 *   for funding a global service and satisfying shareholders. This gap is the source
 *   of the entire legal and social conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `meta_platforms` and `digital_advertisers` directly profit from
 *     the data or revenue generated.
 *   - Victims: `eu_platform_users` are the direct targets of extraction. `privacy_advocacy_groups`
 *     are structurally aligned with users. `eu_courts_and_regulators` are included as
 *     victims in a structural sense; they bear the institutional cost and mandate
 *     to oppose and dismantle the extractive mechanism, aligning them with the targets.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model highlights two institutional actors with opposing views driven by
 *   their structural positions. Meta (beneficiary, `arbitrage` exit) sees a Rope
 *   and can deploy capital globally to adapt. The EU Courts (victim-aligned, `constrained`
 *   exit) are bound by their GDPR mandate and see a Snare they are legally
 *   obligated to address. Their different exit options create a measurable
 *   perspectival gap even at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This classification correctly identifies the coercive choice
 *   architecture as a Snare, preventing it from being mislabeled as a neutral or
 *   beneficial Rope ("a way to fund free services"). By isolating the "Pay or Okay"
 *   mechanism from the broader social platform, the analysis avoids conflating the
 *   function of the platform with the extractive nature of its funding model. The high
 *   extraction score is justified by the direct monetary or data value captured, and
 *   the model's existence is predicated on suppressing a genuinely free alternative,
 *   making it a canonical Snare rather than a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_meta_pay_or_okay_model,
    'Can Meta''s services be sustainably funded via a model that is both free for the user and GDPR-compliant (e.g., using only contextual ads, or a genuinely minimal fee for a non-tracking version)?',
    'Empirical evidence from Meta or a large-scale competitor successfully deploying such a model without significant degradation of service.',
    'If True, it confirms this constraint is a pure Snare by demonstrating a viable, suppressed alternative. If False, it suggests the platform has Mountain-like cost structures, making the model a highly extractive Tangled Rope instead of a Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_pay_or_okay_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model represents the culmination of a long-term drift toward explicit
% extraction. The data models this progression.
% Required since base_extractiveness (0.75) > 0.46.

% Theater ratio over time (decline in "connecting the world" narrative vs. pure business model):
narrative_ontology:measurement(meta_pay_or_okay_model_tr_t0, meta_pay_or_okay_model, theater_ratio, 0, 0.40).
narrative_ontology:measurement(meta_pay_or_okay_model_tr_t5, meta_pay_or_okay_model, theater_ratio, 5, 0.25).
narrative_ontology:measurement(meta_pay_or_okay_model_tr_t10, meta_pay_or_okay_model, theater_ratio, 10, 0.15).

% Extraction over time (shift from implicit data collection to explicit Pay-or-Okay model):
narrative_ontology:measurement(meta_pay_or_okay_model_ex_t0, meta_pay_or_okay_model, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(meta_pay_or_okay_model_ex_t5, meta_pay_or_okay_model, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(meta_pay_or_okay_model_ex_t10, meta_pay_or_okay_model, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The constraint itself functions as a mechanism for allocating user attention
% and data as a resource to advertisers.
narrative_ontology:coordination_type(meta_pay_or_okay_model, resource_allocation).

% This constraint is a direct response to, and exists in tension with, the GDPR framework.
narrative_ontology:affects_constraint(gdpr_privacy_framework, meta_pay_or_okay_model).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, digital_advertising_market).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */