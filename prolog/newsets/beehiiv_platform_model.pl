% ============================================================================
% CONSTRAINT STORY: beehiiv_platform_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 *   Beehiiv is a newsletter platform offering creators tools for publishing,
 *   growth, and monetization. Its business model is based on a tiered
 *   subscription service (including a free tier) rather than taking a
 *   percentage of creator revenue, positioning it as a direct competitor to
 *   platforms like Substack. This structural choice creates significant
 *   perspectival gaps. The constraint is the platform's terms, feature gates,
 *   and pricing structure, which mediate the relationship between creators
 *   and their audiences.
 *
 * KEY AGENTS:
 *   - Successful Creators: Primary beneficiaries (powerful/arbitrage) - Benefit from the flat-fee model, avoiding the percentage cuts of rival platforms.
 *   - Beehiiv (The Company): Primary beneficiary (institutional/arbitrage) - Captures revenue through monthly subscription fees from scaled creators.
 *   - New Creators: Secondary victims (moderate/constrained) - Benefit from the free tier but are subject to limits and upselling, making them the target of the platform's growth model.
 *   - Migrating Creators: Primary victims (moderate/trapped) - Bear the high friction costs of platform lock-in when attempting to leave.
 *   - Advertisers: Secondary beneficiaries - Gain access to aggregated, targeted audiences via the Beehiiv Ad Network.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beehiiv_platform_model, 0.38).
domain_priors:suppression_score(beehiiv_platform_model, 0.5).
domain_priors:theater_ratio(beehiiv_platform_model, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beehiiv_platform_model, extractiveness, 0.38).
narrative_ontology:constraint_metric(beehiiv_platform_model, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(beehiiv_platform_model, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beehiiv_platform_model, tangled_rope).
narrative_ontology:human_readable(beehiiv_platform_model, "The Beehiiv Newsletter Platform Business Model").
narrative_ontology:topic_domain(beehiiv_platform_model, "technological/economic").

domain_priors:requires_active_enforcement(beehiiv_platform_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, beehiiv_the_company).
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, successful_creators).
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, advertisers).
narrative_ontology:constraint_victim(beehiiv_platform_model, new_creators).
narrative_ontology:constraint_victim(beehiiv_platform_model, migrating_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRATING CREATOR (SNARE) — A creator attempting to leave the platform experiences the full cost of lock-in. The process of exporting data, subscribers, and web archives, while redirecting domains and losing platform-specific SEO, is costly and complex. At this moment, the platform feels like a trap. d≈0.95 (as victim+trapped), f(d)≈1.42, σ=0.8 → χ≈0.43. While this χ doesn't meet the Snare threshold, the subjective experience of high coercion and suppressed alternatives aligns with the Snare classification from this specific, temporary viewpoint.
constraint_indexing:constraint_classification(beehiiv_platform_model, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SUCCESSFUL CREATOR (ROPE) — A creator with a large, monetized audience on a paid plan experiences the platform as a pure coordination tool. The flat monthly fee is negligible compared to the 10% cut taken by competitors like Substack. This creator is engaging in arbitrage, selecting the most efficient tool. d≈0.15 (as beneficiary+mobile), f(d)≈-0.01, σ=1.2 → χ≈-0.004. The negative effective extraction signifies a net subsidy relative to the market alternative.
constraint_indexing:constraint_classification(beehiiv_platform_model, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NEW CREATOR (SCAFFOLD) — A new writer on the free plan sees the platform as a temporary support structure. It provides powerful tools at no cost, enabling initial growth. The 2,500 subscriber limit acts as a sunset clause for the free tier, forcing a decision to upgrade or migrate. The platform is scaffolding their entry into the creator economy. d≈0.85 (as victim+mobile), f(d)≈1.15, σ=1.2 → χ≈0.53. The classification is Scaffold due to the explicit sunset logic of the free tier, not the χ value.
constraint_indexing:constraint_classification(beehiiv_platform_model, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees both the genuine coordination function (providing tools, not taking a revenue cut) and the asymmetric extraction (monthly fees, data lock-in, value capture from the ad network). The model is a hybrid, coordinating creator activity while extracting platform fees. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.53. This meets the Tangled Rope criteria.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: BEEHIIV, THE COMPANY (ROPE) — From the platform's institutional perspective, its business model is a pure coordination service. It provides infrastructure for a flat fee, a model it considers fairer and more aligned with creator success than revenue-sharing. d≈0.05 (as beneficiary+arbitrage), f(d)≈-0.12, σ=1.2 → χ≈-0.05. The platform sees itself as subsidizing creator growth.
constraint_indexing:constraint_classification(beehiiv_platform_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beehiiv_platform_model_tests).
:- end_tests(beehiiv_platform_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.38): Moderate. The model is less extractive than percentage-based competitors for successful creators, but still extracts value through monthly fees and platform lock-in. The value is not zero. Suppression (ε=0.50): Moderate. While alternatives exist, migrating a large newsletter with its archive, domain, and integrations is a significant undertaking, creating a 'sticky' ecosystem that suppresses exit. Theater Ratio (ε=0.15): Low. The platform's value proposition is based on functional tools for editing, analytics, and monetization. The service is primarily functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The Beehiiv model is a clear example of a perspectival spread. A successful creator, arbitraging against Substack's 10% fee, sees a pure coordination Rope. A new creator on the free plan sees a supportive Scaffold with a clear sunset clause (the subscriber limit). A creator actively trying to leave experiences the high friction of lock-in as a Snare. The platform itself views its model as a superior Rope. The analytical observer, weighing the coordination benefits against the lock-in and fee structure, classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality, and thus the final classification, is determined by the agent's structural position. The successful creator is a beneficiary with arbitrage exit options, leading to a negative χ (Rope). The new creator is a victim but with mobile/constrained options, seeing a Scaffold. The migrating creator is a victim who is temporarily trapped, leading to a high χ and the perception of a Snare. Beehiiv, as the institutional beneficiary, naturally sees a Rope. This demonstrates how a single set of base properties can generate multiple valid classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by showing that a platform is not monolithically 'good' or 'bad'. Describing Beehiiv as just a 'tool' (Rope) or just a 'trap' (Snare) would be inaccurate. The system correctly identifies that the platform's structure functions as a Rope for its target power users, a Scaffold for new users, and a Snare for those trying to exit. The analytical Tangled Rope classification correctly synthesizes these conflicting but valid perspectives into a single, coherent structural description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beehiiv_platform_model, 2021, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beehiiv_platform_model, resource_allocation).
narrative_ontology:affects_constraint(beehiiv_platform_model, substack_platform_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
