% ============================================================================
% CONSTRAINT STORY: creator_labor_classification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creator_labor_classification, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creator_labor_classification
 *   human_readable: Creator Labor Classification and Extraction
 *   domain: labor/platform_economics/cultural_production
 *
 * SUMMARY:
 *   Creator labor classification systems used by content platforms (YouTube,
 *   TikTok, Twitch, Instagram, Substack) to categorize creators into
 *   employee/contractor/amateur/professional tiers determine access to
 *   monetization, algorithmic promotion, labor protections, and revenue
 *   shares. These classification systems exhibit the classic pattern of
 *   extraction disguised as coordination: platforms genuinely need
 *   classification to function at scale, but the classification architecture
 *   is designed to route surplus extraction toward platform operators and
 *   away from creators. The constraint demonstrates how institutional power
 *   over category definitions becomes extraction mechanism. Classification
 *   decisions are unilateral (platform chooses category), opaque (algorithms
 *   are proprietary), irreversible (reclassification can destroy creator
 *   income), and asymmetric (creators cannot reclassify platforms).
 *   Extractiveness has risen from 0.32 to 0.58 over the measurement interval
 *   as platforms have tightened classification criteria to exclude marginal
 *   creators from monetization. Theater ratio has risen correspondingly (from
 *   0.35 to 0.68) as platforms add performative classification requirements
 *   (community guidelines, brand safety, engagement metrics) that appear to
 *   govern classification but actually operate as screens for economic
 *   exclusion.
 *
 * KEY AGENTS:
 *   - Individual Creator: Primary victim (powerless/trapped) — dependent on platform classification for visibility and income; no exit option; bears full extraction cost
 *   - Creator Collective/Union: Secondary organized victim (moderate/constrained) — negotiates with platforms over classification standards; faces suppression but has some agency
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — controls classification authority; benefits from both coordination function and extraction surplus; genuinely needs classification but captures all surplus
 *   - Premium/Celebrity Creator: Secondary beneficiary (powerful/mobile) — has negotiating power and mobility options; experiences mixed coordination/extraction; can extract concessions from platforms
 *   - Alternative Platforms/Creator Networks: Organized challengers (organized/constrained) — building decentralized classification systems; sunset clause exists if alternatives reach critical mass
 *   - Legacy Labor Classification Systems: Institutional observer (institutional/arbitrage) — employment law, union frameworks increasingly performative; maintain themselves through theater while missing real power dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creator_labor_classification, 0.58).
domain_priors:suppression_score(creator_labor_classification, 0.65).
domain_priors:theater_ratio(creator_labor_classification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creator_labor_classification, extractiveness, 0.58).
narrative_ontology:constraint_metric(creator_labor_classification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(creator_labor_classification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creator_labor_classification, tangled_rope).
narrative_ontology:human_readable(creator_labor_classification, "Creator Labor Classification and Extraction").
narrative_ontology:topic_domain(creator_labor_classification, "labor/platform_economics/cultural_production").

domain_priors:requires_active_enforcement(creator_labor_classification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creator_labor_classification, platform_operators).
narrative_ontology:constraint_beneficiary(creator_labor_classification, aggregators_and_distributors).
narrative_ontology:constraint_victim(creator_labor_classification, content_creators).
narrative_ontology:constraint_victim(creator_labor_classification, creative_workers).
narrative_ontology:constraint_victim(creator_labor_classification, alternative_labor_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CREATOR (SNARE) — Structurally trapped by platform dependency and algorithmic gatekeeping. Career visibility depends entirely on platform classification decisions; no alternative distribution channels with comparable reach. Suppression mechanisms include algorithmic demotion, sudden classification reclassifications that devastate income, and terms-of-service unilateral modification. Zero degrees of freedom for the creator — classification changes the platform imposes are irreversible and affect livelihood immediately. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(creator_labor_classification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATOR COLLECTIVE/UNION (TANGLED ROPE) — Organized creators benefit from coordination of labor standards, revenue sharing, and classification transparency — genuine coordination function exists. But also face significant suppression: platforms can deactivate, demonetize, or reclassify entire collectives. Exit is possible (creators can attempt platform migration or exclusive contracts) but high-cost and uncertain. Mixed coordination and extraction with constrained options for collective action.
constraint_indexing:constraint_classification(creator_labor_classification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits from creator classification as a coordination mechanism: enables algorithmic curation, recommendation, monetization routing, and content quality thresholds. Experiences the constraint as pure coordination problem — without classification, platform function degrades. No meaningful exit costs — classification is essential to platform operation. Net beneficiary with arbitrage optionality.
constraint_indexing:constraint_classification(creator_labor_classification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PREMIUM/CELEBRITY CREATOR (TANGLED ROPE) — Experiences mixed coordination and extraction. Platform classification enables reaching global audiences (coordination benefit). But also subject to algorithmic governance, content moderation, and revenue-share changes (extraction). Mobile exit option exists — these creators can negotiate exclusive contracts or migrate to competing platforms — but negotiating power is significant without being absolute. Moderate experienced extraction with partial agency.
constraint_indexing:constraint_classification(creator_labor_classification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE PLATFORMS / OPEN CREATOR NETWORKS (SCAFFOLD) — Organized agents (creator co-ops, open-source platforms, blockchain-based networks) see creator classification as a temporary institutional failure with a sunset clause. These alternatives offer decentralized classification, direct revenue sharing, and algorithmic transparency. As decentralized infrastructure matures and network effects shift toward creators, the centralized platform monopoly on classification loses extractive force. Theater ratio is high (alternative platforms often promise utopian governance) but the exit pathway is real — creators can migrate costs are falling as alternatives develop network effects. Estimated sunset: 7-15 years for alternatives to gain sufficient liquidity and network reach.
constraint_indexing:constraint_classification(creator_labor_classification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY LABOR CLASSIFICATION SYSTEMS (PITON) — Traditional employment law, union frameworks, and labor protections are increasingly performative when applied to platform creators. Contractors vs employees, gig worker protections, minimum wage — these categories persist through institutional inertia despite being structurally inapplicable to algorithmic labor allocation. The system maintains itself through regulatory theater (labor boards grapple with classification) while real power dynamics operate through platform algorithms, not legal categories. Theater ratio is high because the regulatory system appears to govern the relationship but actually operates at a remove from where extraction happens.
constraint_indexing:constraint_classification(creator_labor_classification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZING VIEW (MOUNTAIN) — From an analytical/civilizational perspective, platform classification might appear immutable: 'algorithms must categorize content to function,' 'scale requires gatekeeping,' 'classification is inherent to digital infrastructure.' This perspective risks naturalizing what is actually a contingent institutional choice about who controls classification authority and how the surplus generated by classification accrues. The engine's false summit detector should flag this as naturalization of extractive architecture, not discovery of inherent constraint.
constraint_indexing:constraint_classification(creator_labor_classification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creator_labor_classification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creator_labor_classification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creator_labor_classification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creator_labor_classification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creator_labor_classification, TR),
    TR >= 0.70.

:- end_tests(creator_labor_classification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract surplus through unilateral classification authority, but creators retain some agency (can produce content, can move platforms at high cost, some creators negotiate successfully). This is higher than pure coordination (ε ≤ 0.35) but lower than pure extraction (ε ≥ 0.70). The rising trajectory (0.32 → 0.58) reflects accumulation of classification criteria and monetization gatekeeping over time. Suppression (0.65): High. Barriers to exit include network effects (losing platform audience), switching costs (rebuilding followings elsewhere), algorithmic retaliation (demotion for attempting migration), and lack of alternative distribution channels with comparable reach. The suppression is both structural (built into platform economics) and internalized (creators have internalized platform dependence as inevitable). Theater ratio (0.68): High and rising. Platforms impose performative classification requirements (community guidelines adherence, brand safety compliance, engagement metrics, content moderation appeals) that appear to govern categorization but actually operate as rationalization for algorithmic exclusion decisions already made. The theater has increased as platforms tighten monetization gatekeeping while maintaining surface appearance of meritocratic classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal: platform sees rope, creator sees snare, analytical observer is tempted toward mountain. This perspectival divergence is the diagnostic signal that the constraint is genuinely hybrid (tangled rope) — the coordination function is real (creators do benefit from platform distribution), but the extraction asymmetry is also real (platforms capture all of the surplus generated by classification authority). The gap is not resolvable by claiming both perspectives are equally true — instead, the resolution is to recognize that the classification authority itself is the extraction mechanism. Platforms use coordination ('we allocate resources efficiently through classification') as cover for extraction ('we unilaterally decide which creators earn income').
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the classification extraction flow. Platform operators derive d ≈ 0.05 (full beneficiary with arbitrage exit — they control the mechanism and can adapt classification schema without cost). Individual creators derive d ≈ 0.95 (full victim with trapped exit — classification decisions are imposed unilaterally and inescapably). Creator collectives derive d ≈ 0.65 (mixed victim/beneficiary with constrained exit — they benefit from platform reach but face suppression and limited renegotiating power). Premium creators derive d ≈ 0.45 (moderately affected victim/beneficiary with mobile exit — they have negotiating power and can threaten platform migration). The sigmoid function applies to these d values, producing effective extractiveness (χ) modified by agent power and scope. For the individual creator (powerless/trapped/global scope), χ is amplified upward by f(d) ≈ 1.42. For the platform (institutional/arbitrage/global scope), χ is dampened downward by f(d) ≈ -0.12. The scope modifier σ(global) ≈ 1.2 further amplifies extraction at global scale relative to local classification systems.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by identifying classification authority as the extractive mechanism. Platforms cannot claim pure coordination because creators have no voice in how classification categories are defined or applied. Creators cannot claim the constraint is purely extractive because they genuinely benefit from platform distribution reach and algorithmic amplification — the coordination function exists. The tangled rope classification is accurate: the constraint must possess both genuine coordination (asymmetric audience access through algorithmic curation) AND asymmetric extraction (unilateral classification authority and surplus extraction). The rising theater ratio (from 0.35 to 0.68) indicates that performative classification requirements are accumulating — community guidelines, brand safety, engagement metrics, appeals processes — all creating the appearance of meritocratic classification while protecting the underlying extraction mechanism. As theater increases, platforms are manufacturing consent for classification authority through procedural legitimacy theater rather than through demonstrated fairness outcomes. The scaffold perspective's sunset clause depends on whether alternative platforms can achieve sufficient critical mass and network effects to make creator migration viable at scale. This is currently uncertain (omega variable: critical_mass_threshold) but structurally possible — if alternative platforms succeed, the centralized platform monopoly on classification authority dissolves, and extractiveness drops.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_authority_locus,
    'Is creator classification authority necessarily centralized with platform operators, or is decentralization technologically feasible and economically viable?',
    'Empirical testing of decentralized classification systems (blockchain platforms, creator DAOs, federated protocols); measurement of transaction costs, network effects, and capital requirements for alternative architectures',
    'If decentralization is technologically and economically viable: extractiveness drops to ~0.15-0.25 (rope), and scaffold sunset becomes structural. If centralization is inherent to scale: extractiveness remains ~0.55-0.70 (snare/tangled rope), and alternatives remain aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_authority_locus, empirical, 'Whether decentralized classification authority is technologically and economically feasible').

omega_variable(
    creator_coalition_critical_mass,
    'What is the critical mass of creators required to make platform migration collectively viable? Does it vary by content category?',
    'Historical analysis of successful platform migrations (Twitch vs YouTube for streamers, Substack vs Medium for writers); identification of creator concentration thresholds and category-specific dependencies; measurement of network effects by content type',
    'If critical mass threshold is achievable: coordination escape becomes real (scaffold), and exit transitions from trapped to constrained. If threshold is impossibly high: creators remain trapped, and snare classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_coalition_critical_mass, empirical, 'Critical mass threshold for viable creator platform migration').

omega_variable(
    algorithmic_classification_transparency,
    'Can classification algorithms be made sufficiently transparent and contestable that creators have real agency in how they are categorized? Or is opacity inherent to algorithmic governance at scale?',
    'Audit of transparency claims in platforms offering ''explainable AI'' classification; measurement of creator ability to contest classifications; comparison of appeals outcomes before/after transparency interventions',
    'If transparency enables contestation: suppression drops below 0.50, and classification becomes rope rather than snare. If opacity is inherent to scale: suppression persists, and snare classification is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_classification_transparency, empirical, 'Whether algorithmic classification can be made transparent and contestable').

omega_variable(
    creator_surplus_value_attribution,
    'What proportion of platform revenue is generated directly by creator content labor versus platform infrastructure, curation algorithms, and network effects? How does attribution methodology affect classification?',
    'Economic decomposition models (activity-based costing applied to platform revenue); comparison of creator revenue share across platforms with varying service levels; measurement of counterfactual platform value without creator content',
    'If creators generate >60% of value: extraction becomes unambiguous (snare classification strengthens). If creators generate <40% of value: platform coordination role appears more significant (tangled rope is more defensible). Attribution method itself becomes politically contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_surplus_value_attribution, conceptual, 'Attribution of platform revenue to creator labor vs. platform infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creator_labor_classification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creator_tr_t0, creator_labor_classification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(creator_tr_t5, creator_labor_classification, theater_ratio, 5, 0.52).
narrative_ontology:measurement(creator_tr_t10, creator_labor_classification, theater_ratio, 10, 0.68).
narrative_ontology:measurement(creator_tr_t2, creator_labor_classification, theater_ratio, 2, 0.42).

% Extraction over time
narrative_ontology:measurement(creator_be_t0, creator_labor_classification, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(creator_be_t5, creator_labor_classification, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(creator_be_t10, creator_labor_classification, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(creator_be_t2, creator_labor_classification, base_extractiveness, 2, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creator_labor_classification, resource_allocation).
narrative_ontology:boltzmann_floor_override(creator_labor_classification, 0.18).
narrative_ontology:affects_constraint(creator_labor_classification, algorithmic_recommendation_bias).
narrative_ontology:affects_constraint(creator_labor_classification, content_moderation_authority).
narrative_ontology:affects_constraint(creator_labor_classification, platform_labor_law_misclassification).

% DUAL FORMULATION NOTE:
% Creator labor classification is downstream of platform algorithmic architecture and recommendation systems (which determine who benefits from classification authority). It is upstream of labor law misclassification (platforms use creator classification categories to avoid employment law obligations). The three constraints form a family with distinct ε values but causal dependencies: algorithmic systems enable classification authority; classification authority creates labor misclassification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creator_labor_classification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
