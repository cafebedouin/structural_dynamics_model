% ============================================================================
% CONSTRAINT STORY: recipe_scaling_ai
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_recipe_scaling_ai, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: recipe_scaling_ai
 *   human_readable: The NYT Cooking Generative Scaling Constraint
 *   domain: technological/publishing
 *
 * SUMMARY:
 *   The New York Times Cooking generative scaling tool represents a hybrid
 *   constraint combining genuine coordination (standardized recipe scaling
 *   methodology, algorithmic reliability, integration with recipe discovery)
 *   with asymmetric extraction (paywall gatekeeping, lock-in through content
 *   bundling, algorithmic preference for premium users). The constraint
 *   operates at the intersection of culinary knowledge, digital publishing,
 *   and generative AI commoditization. As the tool has matured over its
 *   lifecycle (approximately 3 years in this measurement interval),
 *   extractiveness has risen (0.18 → 0.38) while theater ratio has increased
 *   (0.35 → 0.58), indicating Goodhart drift: the original coordination
 *   function (helping home cooks scale recipes reliably) has become
 *   increasingly wrapped in engagement metrics and paywall incentives. The
 *   suppression mechanism is moderate (0.42): home cooks face cost barriers
 *   (subscription) but retain exit options (manual calculation, other
 *   websites, free tools), unlike total entrapment. The constraint exhibits
 *   all six DR types from different structural positions, making it a
 *   diagnostic case for how publishing platforms create hybrid
 *   extraction-coordination systems.
 *
 * KEY AGENTS:
 *   - New York Times Publishing: Primary beneficiary (institutional/arbitrage) — captures subscriber lock-in through premium feature differentiation and algorithmic promotion of premium recipes
 *   - Home Cooks (Non-Premium): Primary victim (powerless/trapped) — face paywall gatekeeping; lack alternative free tools with comparable reliability and discovery integration
 *   - Independent Recipe Websites: Secondary victim (moderate/constrained) — benefit from NYT content authority establishing recipe culture but face extraction through algorithmic preference for NYT content in search and social feeds
 *   - Open Cooking Data Movement: Organized agents (organized/mobile) — arXiv-equivalent for recipes (RecipeSchema.org, open-source scaling libraries) building alternative pathways with clear sunset logic
 *   - Legacy Print Recipe Books: Institutional reference (institutional/arbitrage) — coordination function (scaling guidance) now replicated digitally but lost to paywall and engagement metrics (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid coordination-extraction structure that is contingent on corporate control, not inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(recipe_scaling_ai, 0.38).
domain_priors:suppression_score(recipe_scaling_ai, 0.42).
domain_priors:theater_ratio(recipe_scaling_ai, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(recipe_scaling_ai, extractiveness, 0.38).
narrative_ontology:constraint_metric(recipe_scaling_ai, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(recipe_scaling_ai, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(recipe_scaling_ai, tangled_rope).
narrative_ontology:human_readable(recipe_scaling_ai, "The NYT Cooking Generative Scaling Constraint").
narrative_ontology:topic_domain(recipe_scaling_ai, "technological/publishing").

domain_priors:requires_active_enforcement(recipe_scaling_ai).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(recipe_scaling_ai, new_york_times_publishing).
narrative_ontology:constraint_beneficiary(recipe_scaling_ai, recipe_discovery_algorithmic_engagement).
narrative_ontology:constraint_victim(recipe_scaling_ai, home_cooks_non_premium_access).
narrative_ontology:constraint_victim(recipe_scaling_ai, independent_recipe_websites).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOME COOK WITHOUT PREMIUM ACCESS (SNARE) — Cannot access the scaling tool without subscription; faces lock-in through recipe discovery dependency and lack of alternative free scaling tools. Trapped by content paywall + network effects (NYT recipes are trusted reference material). d≈0.90, f(d)≈1.35, σ=0.9 → χ≈0.51. Experiences pure extraction: pays subscription or loses convenience feature.
constraint_indexing:constraint_classification(recipe_scaling_ai, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDEPENDENT RECIPE WEBSITE (TANGLED ROPE) — Benefits from NYT's cooking content authority establishing recipe culture and discovery patterns, but faces extraction through algorithmic preference for NYT recipes in search and social feeds. Has modest exit options (own audience building takes years) but can move content or operate independently with constraint. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40. Mixed: coordination function (shared recipe culture) + asymmetric extraction (algorithmic preference).
constraint_indexing:constraint_classification(recipe_scaling_ai, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEW YORK TIMES PUBLISHING (ROPE) — Solves coordination problem: provides standardized recipe format and scaling methodology. Benefits from premium subscriber lock-in through feature differentiation. High exit capacity (can shift feature priority, sunset tool, or move scaling to open access). d≈0.15, f(d)≈0.02, σ=1.2 → χ≈0.01. Near-zero effective extraction from their position; they experience the constraint as a coordination tool that drives subscription value.
constraint_indexing:constraint_classification(recipe_scaling_ai, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN COOKING DATA MOVEMENT (SCAFFOLD) — Organized effort (Open Recipe Standard, RecipeSchema.org advocates, open-source scaling libraries) building alternative verification and scaling pathways. Sees the NYT paywall as a temporary coordination failure with clear sunset: as structured recipe data becomes standardized and open implementations of scaling mature, the proprietary tool loses extraction power. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.13. Low effective extraction because organized coalition has agency and clear transition path (estimated 5-10 years for mature open ecosystem).
constraint_indexing:constraint_classification(recipe_scaling_ai, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY KNOWLEDGE REPOSITORY (PITON) — Print-era recipe books contained scaling guidance as a genuinely valuable coordination function. The digital generative tool replicates this function but wraps it in paywalls and engagement metrics (theater_ratio=0.58). The underlying coordination value persists (cooks need scaling help) but is increasingly lost to performative metrics (feature prominence in apps, subscription upselling). The constraint's original coordination function (teaching home cooks to scale recipes) has atrophied but persists through institutional inertia (NYT's brand authority over cooking).
constraint_indexing:constraint_classification(recipe_scaling_ai, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint exhibits genuine coordination (standardized recipe scaling, accessibility, reliability) AND asymmetric extraction (paywall gatekeeping, algorithmic preference, discovery lock-in). Unlike a natural law, this constraint is contingent on corporate control of recipe archives and lack of open alternatives. Unlike pure coordination, it extracts value from non-premium users. The base metrics (ε=0.38, suppression=0.42, requires_active_enforcement=true, beneficiaries + victims) confirm tangled rope classification from this view. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.30.
constraint_indexing:constraint_classification(recipe_scaling_ai, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(recipe_scaling_ai_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(recipe_scaling_ai, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(recipe_scaling_ai, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(recipe_scaling_ai, TR),
    TR >= 0.70.

:- end_tests(recipe_scaling_ai_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising. Initial extractiveness was low (0.18) when the tool launched as a secondary convenience feature bundled with free recipe access. Extractiveness has risen as NYT has invested in feature prominence, paywalled advanced options (ingredient substitution, dietary constraint scaling), and integrated the tool into premium subscription marketing. The rise from 0.18 to 0.38 over 36 months reflects increasing extraction intensity: the tool's perceived necessity has grown (more users discover it, more recipes reference it), and paywall enforcement has tightened. The current 0.38 value reflects that non-premium users face real lock-in (no easily accessible free alternative of comparable quality) but also retain exit options (manual scaling, other websites, shared accounts). Suppression (0.42): Moderate. Barriers to independent access include: subscription cost barrier (~$60-120/year), lack of prominent free alternatives, algorithmic preference for premium recipes in discovery, and tacit knowledge embedded in NYT's recipe testing. However, suppression is not total: some users maintain shared subscriptions, find free alternatives, or abandon premium features entirely. Theater ratio (0.58): Moderate-high. The tool initially served genuine coordination (teaching scaling methodology in recipe context). Theater has risen as NYT has invested in framing scaling as an exclusive premium benefit through UI prominence, marketing, and engagement metrics. The tool's real coordination value (reliable scaling methodology) is now occluded by subscription gatekeeping and algorithmic upselling. Theater at 0.58 indicates that more than half of the tool's visible prominence is driven by engagement optimization and paywall signaling rather than pure cooking functionality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows markedly different experiences across structural positions. The home cook without premium access experiences lock-in (Snare): they cannot access the tool without subscription and face no easy exit. The independent recipe website operator experiences hybrid extraction with coordination benefits (Tangled Rope): they benefit from NYT recipes establishing recipe culture but lose algorithmic visibility in search and social feeds. The New York Times publishing division experiences the constraint as pure coordination (Rope): they solve the problem of standardizing recipe scaling, benefit from subscriber retention, and have high exit capacity (can sunset, open-source, or reprice the tool). The open cooking data movement experiences a temporary coordination failure with a clear exit path (Scaffold): open standards and libraries will eventually make proprietary scaling tools unnecessary. The legacy knowledge repository sees a degraded function (Piton): the original coordination value persists but is increasingly theatrical and inert. The analytical observer sees a contingent hybrid (Tangled Rope): not a natural law, not pure coordination, but a real structural phenomenon that depends on corporate control and lack of open alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Home cooks without premium access: Victim + trapped → d≈0.90, f(d)≈1.35. Maximum extraction from their perspective. Independent recipe websites: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but with some exit capacity and coordination benefit. New York Times: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Near-zero effective extraction from their position; they have full control and high exit capacity. Open cooking data movement: Organized + mobile → d≈0.35, f(d)≈0.30. Low effective extraction because the coalition has agency and clear path to alternatives. Legacy knowledge repository: Institutional + arbitrage → d≈0.15, f(d)≈0.02. Piton classification comes from high theater (0.58), not from high d or f(d). Analytical observer: analytical → d≈0.50, f(d)≈0.65. Tangled rope classification reflects genuine coordination + asymmetric extraction at the system level.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The tangled rope classification resolves the mandatrophy by establishing that the constraint exhibits BOTH genuine coordination (standardized recipe scaling methodology, reliability, discovery integration) AND asymmetric extraction (paywall gatekeeping, lock-in, algorithmic preference). This is not a false positive for 'is there coordination?' (yes, there is) nor a false positive for 'is there extraction?' (yes, there is). The constraint satisfies all three tangled rope gates: (1) requires_active_enforcement = true (paywall + algorithm + content bundling actively enforce the constraint); (2) beneficiaries array includes two entries (NYT publishing + recipe discovery algorithm); (3) victims array includes two entries (non-premium home cooks + independent recipe sites). The rising theater ratio (0.35 → 0.58) indicates that the performance aspect (framing scaling as exclusive premium feature, engagement optimization) is becoming more prominent relative to core coordination function. This is a real drift, but it does not invalidate the tangled rope classification — it shows how the constraint is evolving from 'mostly coordination with some extraction' (v1) toward 'coordination + extraction, more balanced' (v2). The sunset path via open alternatives (RecipeSchema.org maturation, open-source scaling libraries) indicates that the extraction mechanism will eventually lose force, supporting the future trajectory toward pure coordination or rope classification if open alternatives become competitive. The constraint is not false coordination hiding pure extraction (that would be a snare); nor is it pure coordination with theatrical wrapping (that would be a piton). It is genuinely hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaling_accuracy_vs_engagement,
    'Is the generative scaling tool optimized for cooking accuracy or for engagement metrics and retention?',
    'Compare tool recommendations against culinary expertise databases (professional chef guidance, published ratio tables); A/B test recommendations with and without algorithmic engagement optimization; track user success rates (dish outcomes) vs feature usage metrics',
    'If accuracy-optimized: coordination function is genuine, extractiveness lower (~0.20). If engagement-optimized: tool is primarily a lock-in mechanism, extractiveness higher (~0.55).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaling_accuracy_vs_engagement, empirical, 'Whether tool optimizes for cooking accuracy or engagement metrics').

omega_variable(
    free_alternative_viability,
    'Are open-source or free scaling tools (recipe mathematics libraries, chatbots, community wikis) functionally equivalent to the proprietary NYT tool?',
    'Comparative analysis of scaling accuracy, UX, feature parity, discovery integration; user migration rates if free alternatives are promoted; recipe outcome correlation with each tool''s recommendations',
    'If equivalence reached: extraction power collapses (tangled rope → rope), suppression falls below 0.40. If free alternatives remain inferior: extraction persists, suppression stays ~0.42.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_alternative_viability, empirical, 'Functional equivalence of free alternatives to proprietary scaling tool').

omega_variable(
    recipe_discovery_dependency,
    'How much of user lock-in is attributable to the scaling tool vs to broader NYT recipe content discovery and authority?',
    'Churn analysis: measure subscription retention if scaling tool is made free while keeping paywall on recipe content vs making scaling premium while freeing recipes; survey user reasons for NYT subscription maintenance',
    'If scaling drives 30%+ of stickiness: extraction attribution to scaling tool is high. If scaling drives <10%: tool is secondary beneficiary, real extraction is content gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recipe_discovery_dependency, empirical, 'Attribution of lock-in to scaling tool vs recipe content discovery').

omega_variable(
    generative_quality_collapse,
    'As generative AI scaling tools become commoditized and more accessible, does the NYT tool''s competitive advantage persist or does quality convergence force a shift from extraction to pure service pricing?',
    'Longitudinal comparison of tool feature parity across platforms; price sensitivity analysis; user switching rates as competitors'' tools improve; revenue attribution to tool vs content bundle',
    'If advantage persists: extractiveness stable ~0.38. If advantage erodes: competitive pressure forces feature parity, and extraction shifts to content + data collection rather than tool novelty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_quality_collapse, empirical, 'Persistence of NYT scaling tool competitive advantage under commoditization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(recipe_scaling_ai, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(recipe_scale_tr_t0, recipe_scaling_ai, theater_ratio, 0, 0.35).
narrative_ontology:measurement(recipe_scale_tr_t18, recipe_scaling_ai, theater_ratio, 18, 0.48).
narrative_ontology:measurement(recipe_scale_tr_t36, recipe_scaling_ai, theater_ratio, 36, 0.58).

% Extraction over time
narrative_ontology:measurement(recipe_scale_be_t0, recipe_scaling_ai, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(recipe_scale_be_t18, recipe_scaling_ai, base_extractiveness, 18, 0.28).
narrative_ontology:measurement(recipe_scale_be_t36, recipe_scaling_ai, base_extractiveness, 36, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(recipe_scaling_ai, information_standard).
narrative_ontology:affects_constraint(recipe_scaling_ai, recipe_content_paywall_gatekeeping).
narrative_ontology:affects_constraint(recipe_scaling_ai, food_media_algorithmic_preference).

% DUAL FORMULATION NOTE:
% The recipe scaling tool is downstream of broader NYT content paywall and algorithmic discovery systems. The scaling constraint has its own ε (0.38) reflecting the tool-specific extraction, while the parent constraint (recipe content gatekeeping) has higher ε reflecting stricter paywall enforcement. These are distinct constraints linked by institutional coupling: scaling tool enforcement depends on content paywall success, and algorithmic preference for premium recipes amplifies scaling tool extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
