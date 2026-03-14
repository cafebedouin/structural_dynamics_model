% ============================================================================
% CONSTRAINT STORY: open_source_ai_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_ai_accessibility, []).

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
 *   constraint_id: open_source_ai_accessibility
 *   human_readable: Open Source AI Accessibility: Coordination vs. Extraction in Democratization
 *   domain: technology/artificial_intelligence/governance
 *
 * SUMMARY:
 *   Open-source AI democratization is structurally a tangled rope: it solves
 *   genuine coordination problems (avoiding duplicative model training,
 *   leveraging community contributions, accelerating research) while creating
 *   asymmetric resource extraction (computational cost barriers,
 *   infrastructure dependence, governance theater). The constraint exhibits a
 *   perspectival gap that reveals its hybrid nature: beneficiaries (large
 *   cloud providers, well-resourced research institutions) experience
 *   coordination; resource-constrained developers and developing economies
 *   experience snare extraction; intermediate actors experience tangled ropes
 *   with constrained agency. The theater ratio (0.68) reflects that
 *   open-source governance frameworks perform legitimacy while real decisions
 *   remain concentrated: community input shapes marginal features, but model
 *   selection, training data curation, safety constraints, and deployment
 *   incentives flow from corporate strategy. Over the interval,
 *   extractiveness has increased (0.35 → 0.58) as the gap between nominal
 *   access (models released as weights) and functional access (cost of
 *   deployment) has widened with increasing model scale. Theater has also
 *   increased as governance structures have become more elaborate while
 *   decision concentration has persisted.
 *
 * KEY AGENTS:
 *   - Large Cloud Providers (AWS, Google Cloud, Azure): Primary beneficiaries (institutional/arbitrage) — release open-source models at end-of-life, capture ecosystem contributions, lock users into cloud infrastructure for deployment
 *   - Resource-Constrained Developers (Global South, indie researchers, startups): Primary victims (powerless/trapped) — cannot afford GPU infrastructure or cloud-based deployment; cannot access AI development at competitive scale
 *   - Developing Economy Institutions (Universities, research centers, government labs): Secondary victims (powerless/trapped) — structurally locked out by capital and currency barriers; dependent on wealthy-nation infrastructure for any AI research
 *   - AI Developer Communities (Researchers, open-source maintainers): Mixed (organized/constrained) — benefit from shared code and reduced duplication; constrained by infrastructure dependencies and resource asymmetries
 *   - Mid-Tier Technology Companies (Startups, regional tech firms): Mixed (moderate/constrained) — can access models but face constrained deployment options and cost barriers relative to large platforms
 *   - Open-Source Governance Structures (Linux Foundation, open-compute initiatives, model governance boards): Theatrical (institutional/arbitrage) — perform community oversight while real allocation decisions remain concentrated in corporations
 *   - Emerging Public-Compute Infrastructure (OpenCompute, public GPU clusters, open-hardware initiatives): Organized (organized/constrained) — building alternative pathways with sunset logic; constrained but see exit path as alternatives mature
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent economic choices (hardware cost, capital concentration, IP protections) as immutable properties of AI systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_ai_accessibility, 0.58).
domain_priors:suppression_score(open_source_ai_accessibility, 0.62).
domain_priors:theater_ratio(open_source_ai_accessibility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_ai_accessibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(open_source_ai_accessibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(open_source_ai_accessibility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_ai_accessibility, tangled_rope).
narrative_ontology:human_readable(open_source_ai_accessibility, "Open Source AI Accessibility: Coordination vs. Extraction in Democratization").
narrative_ontology:topic_domain(open_source_ai_accessibility, "technology/artificial_intelligence/governance").

domain_priors:requires_active_enforcement(open_source_ai_accessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_ai_accessibility, large_cloud_providers).
narrative_ontology:constraint_beneficiary(open_source_ai_accessibility, research_institutions).
narrative_ontology:constraint_beneficiary(open_source_ai_accessibility, ai_developers).
narrative_ontology:constraint_victim(open_source_ai_accessibility, resource_constrained_developers).
narrative_ontology:constraint_victim(open_source_ai_accessibility, developing_economies).
narrative_ontology:constraint_victim(open_source_ai_accessibility, computational_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED DEVELOPER (SNARE) — Trapped by computational cost barriers. Open-source models require GPU infrastructure, bandwidth, and storage beyond affordable reach. No alternative pathway. Maximum extraction: must use proprietary APIs or abandon AI development entirely. Cannot organize to change the constraint.
constraint_indexing:constraint_classification(open_source_ai_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMY INSTITUTIONS (SNARE) — National research centers, universities, and startups in low-income regions trapped by infrastructure cost and currency barriers. Open-source models nominally 'free' but inaccessible without capital expenditure. Dependency on cloud providers in wealthy nations. Trapped by structural economics, not legal barriers.
constraint_indexing:constraint_classification(open_source_ai_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AI DEVELOPER COMMUNITIES (TANGLED ROPE) — Organized actors (research labs, open-source maintainers) experience both coordination and extraction. Open-source models enable distributed innovation and faster iteration. Genuine coordination function: contributors share code, reduce duplication, accelerate research. BUT also constrained by asymmetric resource control: model weights are 'open' but training infrastructure remains concentrated. Must rely on proprietary clouds for large-scale work, or maintain models with volunteer labor at sub-industrial quality. Mixed experience: access benefits plus resource subordination.
constraint_indexing:constraint_classification(open_source_ai_accessibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE CLOUD AND AI CORPORATIONS (ROPE) — Net beneficiaries (institutional/arbitrage). Releasing open-source models solves genuine coordination problem: reducing replication of basic capabilities, leveraging community contributions, capturing ecosystem data. Extractive outcome: models released at end-of-life (after serving proprietary deployment), constraining developer choices to downstream services. Have arbitrage options — can always close-source if open-source becomes uncompetitive. Experience the constraint as pure coordination with positive spillover.
constraint_indexing:constraint_classification(open_source_ai_accessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MID-TIER TECHNOLOGY COMPANIES (TANGLED ROPE) — Can access open-source models but face constrained deployment options: fine-tuning requires capital investment, serving models at scale requires cloud partnerships with large providers. Genuine coordination benefit: build products on shared foundations. Also extraction: dependence on large-provider infrastructure, API pricing models, data gravitational pull toward proprietary platforms. Cannot easily exit to fully independent stack.
constraint_indexing:constraint_classification(open_source_ai_accessibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN-SOURCE GOVERNANCE THEATER (PITON) — Performative commitment to democratization. Foundation oversight, community governance structures, ethical guidelines — largely theatrical. Real allocation decisions (which models to fund, what capabilities to release, what safety constraints to impose) remain concentrated in corporations. The governance ritual maintains legitimacy narrative while decision power flows toward corporations. Theater ratio: high. Functional content: low. Maintained through institutional inertia and PR value, not substantive coordination.
constraint_indexing:constraint_classification(open_source_ai_accessibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN COMPUTE AND HARDWARE COALITIONS (SCAFFOLD) — Emerging organized actors (OpenCompute Initiative, open-hardware manufacturers, public-compute initiatives) building alternative infrastructure pathways with sunset logic. Genuine coordination: shared infrastructure designs reduce duplication. Constrained but see exit path: distributed inference, open-source accelerators, public GPU clusters promise to reduce cloud provider lock-in. Extraction temporary — as alternative infrastructure matures, dependency on proprietary clouds declines. Sunset estimated 5-15 years as public compute infrastructure develops.
constraint_indexing:constraint_classification(open_source_ai_accessibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, computational cost and expertise barriers are inherent to advanced AI. Training large models fundamentally requires capital and specialized knowledge. Open-source democratizes access compared to proprietary lock-in, but cannot eliminate the underlying scarcity. This perspective risks naturalizing what are contingent economic and regulatory choices (hardware cost, software IP protections, capital concentration) as immutable properties of AI systems.
constraint_indexing:constraint_classification(open_source_ai_accessibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_ai_accessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_source_ai_accessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_source_ai_accessibility, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_source_ai_accessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_source_ai_accessibility, TR),
    TR >= 0.70.

:- end_tests(open_source_ai_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from resource-constrained agents through computational cost barriers and infrastructure dependence. Not maximal (0.70+) because open-source models genuinely reduce costs compared to proprietary-only alternatives and enable some development without cloud dependency. The extraction is real but comparative — less severe than pure proprietary lock-in, more severe than fully distributed infrastructure. Suppression (0.62): Moderate-high. Barriers to independent AI development include: GPU scarcity and cost, cloud provider pricing power, expertise concentration, regulatory uncertainty, capital requirements for fine-tuning. Suppression is not absolute — some development happens in communities with volunteer compute — but it is substantial and rising as model scale increases. Theater ratio (0.68): High and rising. Open-source governance (community councils, ethical guidelines, foundation oversight) is substantially performative. Real decisions (which models to release, what safety constraints to impose, which capabilities to prioritize, how to allocate compute resources) remain concentrated in corporations. Governance ritual provides legitimacy cover for centralized decision-making. Over the interval (0-6 time units), theater has increased as governance structures have become more elaborate (foundation boards, community meetings, published principles) while power concentration has persisted or increased.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Large cloud providers see pure coordination (Rope): open-source solves the problem of duplicative training, enables ecosystem contributions, and provides strategic optionality. Resource-constrained developers see pure extraction (Snare): the models are nominally free but inaccessible without capital expenditure, and all deployment pathways funnel through proprietary clouds. AI developer communities see mixed dynamics (Tangled Rope): genuine coordination of code and research, constrained by infrastructure dependence. Emerging public-compute coalitions see a temporary bottleneck with a clear sunset path (Scaffold): alternative infrastructure is maturing, constrained but viable. The governance structures see themselves as coordinating (Rope) while victims see them as theater (Piton) masking concentrated control. The civilizational analytical observer risks the false summit (Mountain) — treating computational cost as an immutable property of AI rather than a contingent design choice. The perspectival gap reveals that 'open-source AI accessibility' is not a solved problem but a site of active asymmetric resource extraction masked by legitimacy theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position in the extraction flow. Large cloud providers have d ≈ 0.10 (beneficiaries with arbitrage options): they capture benefits without bearing costs; they can exit by closing source if competition threatens. Resource-constrained developers have d ≈ 0.95 (victims trapped): they bear full cost of access barriers with no exit option. Developer communities have d ≈ 0.65 (mixed victims with some agency): they benefit from shared code but are constrained by infrastructure dependence; they can organize and exit is costly but possible. Mid-tier companies have d ≈ 0.55 (victims with moderate constrained options): they can deploy models but only at cost and with platform dependence; exits are possible but expensive. The derived effective extractiveness (χ) scales these raw d values by the sigmoid f(d) and scope modifier σ(S). At global scope (σ = 1.2), the extraction experienced by powerless trapped agents is amplified; for institutional arbitrage agents, it is dampened. The result: resource-constrained developers experience χ ≈ 1.35 (above the raw ε of 0.58), while cloud providers experience χ ≈ 0.02 (near zero). The perspectival gap in χ (1.35 vs 0.02) reflects that the same structural constraint produces radically different extraction experiences based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint's claimed type (tangled_rope) is empirically justified but conceptually contested. Large cloud providers claim the constraint is a Rope: it coordinates the solution to duplicative training and enables ecosystem value creation. Resource-constrained developers claim it is a Snare: it extracts through computational cost barriers with no coordination benefit for them. The constraint is genuinely both — it solves coordination problems for some agents while extracting from others. The mandatrophy is not which classification is correct but whether the asymmetry is justified. Is the extraction a fair price for coordination benefits that the beneficiaries pass downstream? Or is the extraction structural rent-seeking? The divergence between perspectives answers this: if the coordination truly benefited all parties proportionally, resource-constrained developers would perceive Rope rather than Snare. The fact that they experience Snare while beneficiaries experience Rope reveals that the distribution of coordination gains is asymmetric. The constraint's theater ratio (0.68) suggests that some of the legitimacy masking this asymmetry is performative — governance structures provide narrative cover for concentrated resource control. Resolving the mandatrophy would require either: (1) redistributing extraction (subsidizing compute access for resource-constrained agents, which would weaken the snare), or (2) making the coordination genuinely symmetric (transparent shared governance with real power-sharing, which would transform the Piton governance perspective into actual Rope). Currently unresolved; mandatrophy_resolved set to false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_cost_trajectory,
    'Will Moore''s Law or alternative scaling laws reduce computational cost per inference sufficiently to eliminate the hardware access barrier within the next decade?',
    'Empirical tracking of inference cost per token, hardware efficiency gains, and distributed inference feasibility; comparison of historical cost curves with near-future projections',
    'If cost drops dramatically: snare classification weakens, barriers transform from trapped to constrained or mobile. If cost stagnates: snare persists, resource-constrained developers remain trapped indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_cost_trajectory, empirical, 'Whether computational cost reduction will eliminate hardware access barriers').

omega_variable(
    coordination_authenticity,
    'Is the open-source AI governance framework a genuine coordination mechanism or primarily a legitimacy theater masking concentrated control?',
    'Analysis of decision-making authority, funding flows, model selection criteria; measurement of how often community input changes corporate decisions; tracking of divergence between stated governance principles and actual resource allocation',
    'If genuine coordination: piton classification is incorrect, governance is functional (Rope). If theatrical: piton classification confirmed, governance is performative, extraction is hidden. Classification gap determines whether ''open'' AI is actually democratizing or merely distributed surveillance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_authenticity, empirical, 'Whether governance structures represent authentic coordination or theater').

omega_variable(
    hardware_monopoly_irreversibility,
    'Are current patterns of hardware chip manufacturing and design so concentrated that public-compute alternatives face fundamental scaling obstacles that private cloud providers do not?',
    'Analysis of supply chain constraints, fab capacity, design talent concentration, and capital requirements for competitive chip development; comparison of public-compute scaling with private infrastructure; identification of whether the gap is technical or economic',
    'If irreversible: scaffold sunset clause is aspirational — private-cloud dependency is structural, not temporary. Snare classification hardens. If reversible: scaffold is real — alternative infrastructure can mature, supporting the constrained exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_monopoly_irreversibility, empirical, 'Whether hardware concentration creates irreversible cloud provider advantage').

omega_variable(
    data_extraction_asymmetry,
    'How much of the ''open-source'' value derives from proprietary data collection from users of open models via cloud services? Is the extractiveness of the computational cost barrier offset by data extraction that cloud providers capture?',
    'Transparency reports on data collection practices; analysis of how user interactions with open models deployed on proprietary platforms flow back to model operators; measurement of data asymmetry between users and platform operators',
    'If data extraction is substantial: the constraint is structurally a two-layer snare — computational access for data extraction trade-off. Suppressiveness increases. If minimal: computat. Cost barrier is the primary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_extraction_asymmetry, empirical, 'Whether data extraction is embedded in open-source deployment').

omega_variable(
    regulatory_path_dependency,
    'Will future AI regulation (safety, capability control, export restrictions) further concentrate control in large platforms that can afford compliance infrastructure, or will it enable smaller actors to certify safety independently?',
    'Analysis of emerging regulatory frameworks; tracking of compliance cost structures; identification of whether regulations create barriers to entry (concentrating power) or enable distributed verification (decentralizing power)',
    'If concentrating: regulation layers additional extraction mechanisms onto the computational cost barrier. Snare hardens. If enabling: regulation could reduce asymmetry, supporting constrained-to-mobile transitions for smaller actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_path_dependency, empirical, 'Whether AI regulation will concentrate or distribute control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_ai_accessibility, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(osai_tr_t0, open_source_ai_accessibility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(osai_tr_t3, open_source_ai_accessibility, theater_ratio, 3, 0.62).
narrative_ontology:measurement(osai_tr_t6, open_source_ai_accessibility, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(osai_be_t0, open_source_ai_accessibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(osai_be_t3, open_source_ai_accessibility, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(osai_be_t6, open_source_ai_accessibility, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_ai_accessibility, global_infrastructure).
narrative_ontology:affects_constraint(open_source_ai_accessibility, ai_compute_concentration).
narrative_ontology:affects_constraint(open_source_ai_accessibility, developing_economy_digital_exclusion).
narrative_ontology:affects_constraint(open_source_ai_accessibility, open_source_sustainability_crisis).

% DUAL FORMULATION NOTE:
% Open-source AI accessibility is downstream of hardware concentration and capital requirements (constraint: ai_compute_concentration), which in turn affect broader digital exclusion patterns (constraint: developing_economy_digital_exclusion). The sustainability of volunteer-maintained open-source infrastructure is also constrained by economic dynamics (constraint: open_source_sustainability_crisis). These three constraints form a family: the accessibility bottleneck cannot be resolved without addressing upstream compute monopolies and downstream sustainability pressure on maintainers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_source_ai_accessibility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
