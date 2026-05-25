% ============================================================================
% CONSTRAINT STORY: ai_model_moat_network_effects
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_model_moat_network_effects, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_model_moat_network_effects
 *   human_readable: AI Model Moat: Network Effects and Competitive Lock-In
 *   domain: artificial_intelligence/economics/technology
 *
 * SUMMARY:
 *   The AI model moat represents a structural constraint where network
 *   effects create competitive lock-in between proprietary model developers,
 *   downstream application builders, and users seeking reliable access to AI
 *   capabilities. This constraint exhibits the full range of DR
 *   classification across multiple institutional and individual perspectives,
 *   making it a diagnostic exemplar for how technology lock-in operates as a
 *   hybrid coordination-extraction mechanism. The same structural phenomenon
 *   — the concentration of AI capability in dominant models combined with
 *   ecosystem effects (tooling, fine-tuning data, integrations, user base) —
 *   appears as an immutable law of technology markets (mountain perspective),
 *   a legitimate coordination mechanism managing scale (rope perspective), a
 *   temporary coordination problem being solved by open-source alternatives
 *   (scaffold perspective), a degraded competitive narrative (piton
 *   perspective), or pure extraction preventing competitor emergence (snare
 *   perspective), depending on the observer's structural position and power
 *   level. The constraint's extractiveness (0.62) reflects genuine asymmetric
 *   lock-in: once a model achieves critical mass in users and integrations,
 *   competitors cannot match the data feedback loop or ecosystem maturity
 *   without years of development. The theater ratio (0.55) reflects that
 *   network effects narratives increasingly serve to justify pricing and
 *   lock-in rather than describing technical immutability — open-source
 *   alternatives are demonstrating that scale concentration is contingent on
 *   institutional and capital structures, not laws of nature.
 *
 * KEY AGENTS:
 *   - Dominant Model Developer: Primary beneficiary (institutional/arbitrage) — captures network effects, data feedback loop, ecosystem lock-in; experiences constraint as coordination value they provide
 *   - Competing AI Developers: Primary victim (powerless/trapped) — cannot access equivalent training data, compute resources, or user bases; face funding starvation and talent drain; structurally excluded from market
 *   - Application Developers: Secondary victim (moderate/constrained) — benefit from ecosystem tooling and model improvements but face unilateral pricing, API changes, and data capture through usage monitoring
 *   - Open-Source AI Coalition: Organized agents (organized/mobile) — Meta, Mistral, community projects building alternative pathways; have agency and exit trajectory through model open-sourcing and interoperability standards
 *   - Compute Infrastructure Providers: Dual-position (powerful/mobile) — benefit from training concentration but face geopolitical risk and regulatory pressure; can reallocate to open-source or alternative ecosystems
 *   - Information Access Commons: Victims (powerless/trapped) — users and institutions dependent on AI face gated access, pricing extraction, content filtering; no collective exit mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (capital concentration, VC funding structures, regulatory forbearance) as immutable laws of technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_model_moat_network_effects, 0.62).
domain_priors:suppression_score(ai_model_moat_network_effects, 0.68).
domain_priors:theater_ratio(ai_model_moat_network_effects, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_model_moat_network_effects, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_model_moat_network_effects, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_model_moat_network_effects, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_model_moat_network_effects, tangled_rope).
narrative_ontology:human_readable(ai_model_moat_network_effects, "AI Model Moat: Network Effects and Competitive Lock-In").
narrative_ontology:topic_domain(ai_model_moat_network_effects, "artificial_intelligence/economics/technology").

domain_priors:requires_active_enforcement(ai_model_moat_network_effects).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_model_moat_network_effects, dominant_model_developer).
narrative_ontology:constraint_beneficiary(ai_model_moat_network_effects, platform_infrastructure_provider).
narrative_ontology:constraint_victim(ai_model_moat_network_effects, competing_ai_developers).
narrative_ontology:constraint_victim(ai_model_moat_network_effects, downstream_application_builders).
narrative_ontology:constraint_victim(ai_model_moat_network_effects, information_access_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER AI DEVELOPER (SNARE) — Once dominant models achieve network effects (user base, fine-tuning data, integrations, ecosystem tooling), competing developers face insurmountable barriers: users won't switch without massive quality advantage, data collection is asymmetric (dominant model has user feedback loop), and VC funding concentrates on market leaders. Exit is structurally unavailable. Maximum extraction experienced through funding starvation, talent migration to dominant firms, and marginalization in market share.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: APPLICATION DEVELOPER (TANGLED ROPE) — Builds products using dominant model APIs. Genuine coordination benefit: API stability, model improvements, ecosystem growth. But also extraction: pricing power, API changes imposed unilaterally, lock-in via custom fine-tuning, data capture through usage monitoring. Can theoretically switch to alternative models but faces retraining, customer friction, and inferior quality. Moderate extraction with real coordination value.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT MODEL DEVELOPER (ROPE) — Experiences the constraint as pure coordination: managing user base, ecosystem, integrations, and safety standards. The network effects are benefits they provide to users and developers, not extraction imposed on them. They have complete exit optionality (can redirect resources, change pricing, pivot product) and benefit from the very mechanisms (scale, data feedback, ecosystem lock-in) that harm others. Net beneficiary.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE AI COALITION (SCAFFOLD) — Organized agents (Meta, Mistral, community projects) are building open-weight model alternatives with sunset logic: as open models improve toward proprietary quality, lock-in weakens. Distributed fine-tuning, local deployment, and interoperability standards reduce dependency on any single dominant model. Temporary constraint being eroded by alternative coordination pathways. Has agency and exit trajectory.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PLATFORM VENDOR (PITON) — Traditional software vendors (enterprise, cloud providers) maintain AI moat narratives to justify premium pricing, but the functional mechanism is degrading. Open-source models are approaching quality parity. Cost of inference is falling. The moat's extractive power relies increasingly on theater (marketing, narrative of inevitable consolidation, FUD about open-source safety) rather than structural technical advantage. Theater ratio rising as functional differentiation shrinks.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPUTE INFRASTRUCTURE PROVIDER (TANGLED ROPE) — Cloud providers benefit from training concentration (dominant models require massive compute) but also face extraction: regulatory pressure on data residency, security liability, geopolitical dependency on US dominance. Can reallocate resources and pivot to open-source, but structural coupling to whichever ecosystem dominates. Mixed coordination (training infrastructure) and extraction (dependency asymmetry).
constraint_indexing:constraint_classification(ai_model_moat_network_effects, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INFORMATION ACCESS COMMONS (SNARE) — Users and institutions dependent on AI for information access face moat lock-in: proprietary models gate knowledge with usage restrictions, pricing power, and content filtering. Public knowledge commons (academic outputs, open datasets) are increasingly mediated through proprietary APIs. No collective exit option. Maximum extraction: restricted access, pricing extraction, narrative control over what information is retrievable.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects in technology are treated as immutable: whoever achieves scale first wins through positive feedback loops; competition becomes impossible once threshold is crossed. This perspective sees the moat as an inevitable law of tech markets. However, the structural data contradicts this — open-source alternatives are eroding moats at generational timescales; the 'inevitability' narrative serves dominant players' interests and naturalizes what is actually a contingent institutional arrangement vulnerable to coordination.
constraint_indexing:constraint_classification(ai_model_moat_network_effects, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_model_moat_network_effects_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_model_moat_network_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_model_moat_network_effects, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_model_moat_network_effects, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_model_moat_network_effects, TR),
    TR >= 0.70.

:- end_tests(ai_model_moat_network_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high and rising. Initial value (0.38) reflects early-stage competition when multiple capable models existed. Current value (0.62) reflects consolidated market dominance and rising extraction through API pricing, data capture, and competitive exclusion. Trajectory shows accumulation: as dominant models increase market share, their power to extract through pricing and lock-in compounds. The mechanism is not coercive force but structural asymmetry in data feedback and network effects. Suppression (0.68): High. Barriers to competitor emergence include: compute resource concentration ($100M+ training costs), data asymmetry (proprietary models get user feedback loop unavailable to competitors), talent migration to dominant firms, VC funding concentration on market leaders, and switching costs (users reluctant to migrate after integrating with existing ecosystem). But suppression is not total — open-source alternatives are gradually eroding these barriers at generational timescales. Theater ratio (0.55): Moderate. Narratives of 'inevitable winner-take-all' and 'network effects make competition impossible' increasingly serve to justify extraction rather than describing structural inevitability. Open-source models achieving 90%+ of proprietary quality at 10% of cost undermine claims of technical inevitability. Theater is rising as functional differentiation shrinks relative to pricing and lock-in mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The dominant model developer sees rope (coordination) — they experience the constraint as network effects they provide to users and ecosystem partners. Smaller competitors see snare (pure extraction) — they are systematically excluded from feedback loops and capital. Application developers see tangled rope — genuine coordination value plus extraction via pricing and lock-in. The open-source coalition sees scaffold — a temporary coordination problem with visible sunset as open alternatives mature. The compute provider sees tangled rope with geopolitical coupling — benefits from concentration but faces regulatory and dependency risks. The information access commons sees snare — gated access and pricing extraction with no exit mechanism. The legacy vendor sees piton — moat narratives persist through inertia as functional differentiation erodes. The civilizational analytical observer risks seeing mountain (inevitable lock-in) when the structural data reveals contingency: the moat depends on capital concentration, regulatory forbearance, and institutional structures, not immutable technical limits. Open-source alternatives and cost trajectories are demonstrating the contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position: beneficiaries with full arbitrage optionality (dominant developer) experience negative d → negative chi (they benefit from the constraint). Trapped competitors with no exit experience high d → high chi (maximum extraction). Constrained application developers fall in between: they benefit from ecosystem coordination but face pricing and lock-in extraction. The open-source coalition has medium-low d because they have agency and visible exit pathways. Compute providers have dual positioning: they benefit from concentration but face regulatory and geopolitical dependency — their d reflects the coupling cost. The information access commons has highest d: they cannot exit, cannot organize collectively, and bear maximum extraction through gated access and pricing. The piton classification derives not from high extraction but from rising theater ratio: the moat's extractive power increasingly relies on narrative inevitability rather than structural technical advantage. The mountain classification at the analytical context is perspectival and risks naturalizing what is actually a contingent institutional arrangement dependent on capital structures, regulatory forbearance, and VC funding concentration.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how dominance in coordination (providing valuable AI capability at scale) can be weaponized into extraction through lock-in and pricing power. The beneficiary (dominant developer) genuinely provides coordination value — their models are better, their ecosystem is more mature. But the extraction is real: competitors are systematically excluded, users face lock-in, and prices are set by monopoly power. The tangled rope classification is correct: the constraint simultaneously solves coordination problems (integrating AI into products) and imposes asymmetric extraction (pricing, lock-in, data capture). The mandatrophy is resolved not by choosing between 'it's coordination' or 'it's extraction' but by measuring how much of each. The measurement shows rising extractiveness as market concentration increases, which indicates the coordination is becoming progressively more extractive. The scaffold perspective's sunset timeline — roughly 5-10 years for open alternatives to reach quality parity and make lock-in economically irrational — provides a testable hypothesis for whether the current arrangement is temporary or structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_convergence_threshold,
    'At what quality parity level do users switch from proprietary to open-source models despite existing lock-in?',
    'Empirical tracking of model benchmarks (MMLU, coding tasks, reasoning); correlation with adoption rate changes for open alternatives; market share shifts in downstream applications',
    'If threshold < 5% quality gap: network effects moat is weaker than claimed; scaffold perspective dominates; constraint reclassifies to temporary coordination problem. If threshold > 15%: moat is structural; snare classification persists for most alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quality_convergence_threshold, empirical, 'Quality convergence threshold for switching from proprietary to open models').

omega_variable(
    data_feedback_asymmetry_reversibility,
    'Can open-source communities accumulate user-generated fine-tuning data at rates approaching proprietary moat developers despite lacking user base scale?',
    'Analysis of public fine-tuning datasets (HuggingFace, etc.); comparison of data accumulation rates for proprietary vs open models; quality of open-source specialized models vs proprietary equivalents',
    'If reversible (open catches up): data moat is temporary; scaffold perspective valid. If irreversible: proprietary feedback advantage compounds indefinitely; snare classification for competitors stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_feedback_asymmetry_reversibility, empirical, 'Whether data feedback asymmetry is reversible at open-source scale').

omega_variable(
    regulatory_intervention_likelihood,
    'Will regulatory action (interoperability mandates, API portability standards, model access requirements) forcibly erode moats before competitive alternatives maturity?',
    'Legislative tracking in EU (DMA, AI Act), US (antitrust scrutiny), and China; timeline for model interoperability standards; enforcement capacity against tech incumbents',
    'If high probability: moat is contingent on regulatory forbearance; extraction can be terminated exogenously. If low probability: moat persists through institutional inertia and regulatory capture; snare classification stabilizes for smaller developers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_likelihood, preference, 'Likelihood of regulatory intervention forcing moat erosion').

omega_variable(
    inference_cost_trajectory,
    'Do inference costs fall fast enough (Moore''s Law + quantization) to decouple user lock-in from proprietary pricing power?',
    'Quarterly cost tracking ($/inference token, model-agnostic); timeline to inference cost < 1% of proprietary API pricing; adoption of on-device and local models',
    'If costs fall rapidly: lock-in weakens; users can self-host at lower cost, eroding extraction mechanism. If costs plateau: proprietary pricing remains dominant extraction mechanism; tangled rope persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inference_cost_trajectory, empirical, 'Inference cost trajectory and self-hosting viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_model_moat_network_effects, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aimoat_tr_t0, ai_model_moat_network_effects, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aimoat_tr_t2, ai_model_moat_network_effects, theater_ratio, 2, 0.42).
narrative_ontology:measurement(aimoat_tr_t4, ai_model_moat_network_effects, theater_ratio, 4, 0.5).
narrative_ontology:measurement(aimoat_tr_t6, ai_model_moat_network_effects, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(aimoat_be_t0, ai_model_moat_network_effects, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aimoat_be_t2, ai_model_moat_network_effects, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(aimoat_be_t4, ai_model_moat_network_effects, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(aimoat_be_t6, ai_model_moat_network_effects, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_model_moat_network_effects, global_infrastructure).
narrative_ontology:affects_constraint(ai_model_moat_network_effects, ai_training_data_asymmetry).
narrative_ontology:affects_constraint(ai_model_moat_network_effects, compute_resource_concentration).
narrative_ontology:affects_constraint(ai_model_moat_network_effects, api_pricing_extraction).
narrative_ontology:affects_constraint(ai_model_moat_network_effects, ecosystem_switching_costs).

% DUAL FORMULATION NOTE:
% The AI model moat decomposes into four structurally distinct constraints: (1) data asymmetry (training feedback loop concentration), (2) compute resource concentration (capex barriers), (3) API pricing extraction (pricing power), and (4) ecosystem switching costs (integration lock-in). Each has its own epsilon value and classification. The moat constraint represents the aggregated effect when all four operate together. Individual constraints may show different epochs (compute concentration peaked earlier; API pricing extraction rising currently) and different victim populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_model_moat_network_effects, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
