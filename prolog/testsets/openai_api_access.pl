% ============================================================================
% CONSTRAINT STORY: openai_api_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_api_access, []).

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
 *   constraint_id: openai_api_access
 *   human_readable: OpenAI API Access Controls
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's API access controls create a structural constraint on the
 *   ecosystem of developers, researchers, and companies building applications
 *   on large language models. The constraint operates simultaneously as a
 *   coordination mechanism (rate limiting prevents service collapse, pricing
 *   tiers allocate scarce GPU compute) and as an extraction mechanism
 *   (pricing power concentration, lock-in effects, unilateral policy
 *   changes). Different actors experience the constraint differently based on
 *   their structural position: OpenAI experiences it as efficient resource
 *   allocation (Rope); bootstrapped companies dependent on the API experience
 *   it as inescapable (Snare); enterprise customers with negotiating power
 *   experience mixed coordination and extraction (Tangled Rope); open-source
 *   communities see an alternative pathway emerging (Scaffold). The
 *   constraint's extractiveness has increased over the measurement interval
 *   (0.38→0.52) as demand exceeded supply and pricing pressures intensified.
 *   The theater ratio has also increased (0.32→0.48), reflecting growing
 *   performativity in compliance enforcement and terms-of-service
 *   justifications.
 *
 * KEY AGENTS:
 *   - OpenAI Corporation: Primary beneficiary (institutional/arbitrage) — controls API infrastructure and captures pricing power during period of scarce capacity
 *   - Dependent Startups: Primary victims (powerless/trapped) — companies whose core products are built on OpenAI API with no viable alternatives; subject to unilateral constraint changes
 *   - Enterprise Customers: Secondary beneficiary-victim (organized/constrained) — benefit from standardized API but face extraction through tiered pricing and usage monitoring; have some negotiating power
 *   - Open Source Communities: Secondary victim (powerless/trapped) — depend on API for benchmarking and capability comparison; face pricing and access restrictions with limited alternatives
 *   - Open Model Initiative: Emerging alternative (organized/mobile) — Llama, Mistral, and other projects building exit pathways; organized actors with agency to create competitive alternatives
 *   - Individual Researchers: Secondary victims (moderate/constrained) — benefit from API access for rapid experimentation but face quotas and pricing disproportionately affecting low-budget work
 *   - Compliance and Terms Theater: Institutional actor (institutional/arbitrage) — maintains published policies that are increasingly performative as enforcement scales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_api_access, 0.52).
domain_priors:suppression_score(openai_api_access, 0.58).
domain_priors:theater_ratio(openai_api_access, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_api_access, extractiveness, 0.52).
narrative_ontology:constraint_metric(openai_api_access, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(openai_api_access, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_api_access, tangled_rope).
narrative_ontology:human_readable(openai_api_access, "OpenAI API Access Controls").
narrative_ontology:topic_domain(openai_api_access, "technological/economic").

domain_priors:requires_active_enforcement(openai_api_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_api_access, openai_corporation).
narrative_ontology:constraint_beneficiary(openai_api_access, api_ecosystem_developers).
narrative_ontology:constraint_victim(openai_api_access, dependent_application_providers).
narrative_ontology:constraint_victim(openai_api_access, cost_sensitive_startups).
narrative_ontology:constraint_victim(openai_api_access, open_source_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT STARTUP (SNARE) — A bootstrapped company whose core product depends entirely on the OpenAI API. No alternative provider offers equivalent capability-to-cost ratio. Switching requires months of reengineering and carries reputational/customer risk. Rate limits, pricing changes, and terms-of-service modifications are non-negotiable constraints. Maximum extraction experienced.
constraint_indexing:constraint_classification(openai_api_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE CUSTOMER (TANGLED ROPE) — Large organizations benefit from standardized API access (coordination function: enables rapid scaling and feature deployment) but face extraction through pricing tiers, usage monitoring, and SLA enforcement. Can negotiate contracts and have some exit optionality (in-house models, competitor APIs), but switching costs remain substantial. Mixed coordination and extraction.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: OPENAI CORPORATION (ROPE) — Controls the API and experiences access restrictions as a pure coordination mechanism: rate limiting prevents system overload, tier pricing allocates scarce computational resources efficiently, and usage monitoring enables billing. The constraint solves a genuine collective action problem (preventing tragedy of the commons on GPU capacity). Net beneficiary — extraction flows toward this actor.
constraint_indexing:constraint_classification(openai_api_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COMMUNITIES (SNARE) — Projects like LLaMA-based derivatives depend on OpenAI's API for baseline capability comparisons, benchmarking, and integration prototyping. Pricing and access restrictions limit their ability to build alternatives. Cannot exit to competitor APIs without abandoning their specific feature dependencies. No negotiating power; subject to unilateral policy changes.
constraint_indexing:constraint_classification(openai_api_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIVIDUAL DEVELOPERS (TANGLED ROPE) — Researchers benefit from API access for rapid experimentation (coordination) but face extraction through usage quotas, rate limits, and pricing that disproportionately affect low-budget users. Can theoretically switch to open models but face switching friction for specific GPT capabilities. Constrained exit with partial benefits.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TERMS AND COMPLIANCE THEATER (PITON) — OpenAI's published API terms (content policy enforcement, rate limit justifications, acceptable use clauses) are increasingly performative as enforcement scales. Automated content filtering and quota management are the functional constraints; the explicit policy documentation serves symbolic/legal purposes. Theater ratio high (0.48→0.62 over interval) as terms multiply without proportional functional change.
constraint_indexing:constraint_classification(openai_api_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN MODEL INITIATIVE (SCAFFOLD) — Organized actors (Meta/Llama, Mistral, open-weight projects) are building alternative pathways to capable language models. The OpenAI API constraint is structured as temporary from this perspective — as open models mature and lower-cost alternatives proliferate, API dependency decays. Estimated sunset: 3-7 years for parity in key use cases. Organized agents see exit paths.
constraint_indexing:constraint_classification(openai_api_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systemic perspective, the API access constraint simultaneously solves a coordination problem (GPU allocation, service stability) and extracts economic rent through pricing power concentration. OpenAI's control derives both from genuine scarcity (compute) and from intellectual property leverage (model weights, training data). The constraint cannot be classified as pure coordination (rope) because extraction is explicit and asymmetric; cannot be classified as pure extraction (snare) because coordination function is non-trivial.
constraint_indexing:constraint_classification(openai_api_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_api_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_api_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_api_access, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_api_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openai_api_access, TR),
    TR >= 0.70.

:- end_tests(openai_api_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. At launch (0.38), the API was primarily coordinative—scarce GPU capacity genuinely required allocation mechanisms. Over 12 months, extractiveness increased to 0.52 as demand exceeded supply and OpenAI captured pricing power. The $0.002-0.06 per-token pricing spans a 30x range, creating effective extraction through tier stratification. However, extractiveness is not at snare levels (≥0.66) because: (1) open-source alternatives exist and are maturing, (2) some customers can negotiate enterprise contracts, (3) GPU scarcity is real (not entirely artificial), and (4) the coordination function remains genuine. Suppression (0.58): Moderate-high. Rate limits, usage quotas, tier restrictions, and content policy enforcement create significant barriers to free access. But suppression is not total—paid access is available at all scales, and the technical justification for rate limits (system stability) is partly legitimate. Theaters (0.48): Moderate and rising. The published acceptable use policies and rate limit justifications are increasingly performative—automated enforcement is the functional mechanism; policy text is largely symbolic. Growth from 0.32→0.48 reflects multiplication of terms-of-service clauses without proportional functional change.
 *
 * PERSPECTIVAL GAP:
 *   All six constraint types are structurally justified from different observational positions. This is NOT a case of indeterminate classification—it is a case where the constraint legitimately exhibits different structural signatures depending on the agent's position. The snare (dependent startup) and rope (OpenAI) perspectives are not contradictory; they are dual descriptions of the same constraint from opposite ends of the extraction flow. The scaffold (open models maturing) is a structural feature—it is NOT aspirational but rather a measurable exit path with empirical timeline. The piton (compliance theater) is also justified—published terms are increasingly divorced from functional enforcement. The tangled rope (analytical observer) is the most complete perspective because it captures the simultaneous coordination and extraction without naturalizing either as the only truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position: beneficiaries with arbitrage options (OpenAI) experience low d (derived ~0.05 from arbitrage exit); victims with trapped exits experience high d (derived ~0.95 from trapped exit + victim status). Enterprise customers and researchers occupy the middle (d ~0.50-0.65) because they have partial exit optionality and partial benefits. The engine's sigmoid f(d) maps these to experienced extractiveness: OpenAI sees f(d)≈-0.12 (net subsidy to them); dependent startups see f(d)≈1.42 (maximum extraction); enterprise customers see f(d)≈0.75 (high extraction with some agency). Scope modifier σ(S) applies at global scale (σ=1.2), amplifying effective extractiveness across all perspectives because API access transcends regional boundaries and switching costs are globally uniform.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution for this constraint requires recognizing that 'API access control' conflates two structurally distinct constraints: (1) GPU scarcity allocation (genuine coordination problem, creates rope-like necessity), and (2) OpenAI pricing power and lock-in (economic rent extraction, creates snare-like extraction). The high-level claim 'OpenAI controls API access' combines both. Decomposition: the GPU scarcity problem is real and creates legitimate coordination. The pricing power problem is rent-based and extractive. Both exist simultaneously. The tangled rope classification captures this synthesis. Mandatrophy is resolved by acknowledging that neither pure-coordination nor pure-extraction framing is complete. The constraint simultaneously solves a real problem (GPU allocation) and enables economic rent extraction (pricing concentration). The mandatrophy trap would be claiming the constraint is ONLY coordination (ignoring extraction) or ONLY extraction (ignoring real scarcity coordination). The tangled rope classification rejects this false choice and identifies the constraint as a genuine hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_model_capability_parity,
    'Will open-source language models reach capability parity with OpenAI GPT-4 in cost-critical applications within 3 years?',
    'Benchmark comparison (MMLU, GSM8K, code generation) across open models vs GPT-4; cost-per-token parity analysis for specific application classes; developer migration tracking',
    'If yes: scaffold timeline is accurate, snare perspective is temporary, constraint transforms to piton. If no: snare perspective persists, open model initiative does not materialize as exit path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_model_capability_parity, empirical, 'Capability parity timeline for open versus proprietary models').

omega_variable(
    api_pricing_sustainability,
    'Is OpenAI''s current API pricing model extractive (above cost) or competitive (at cost)?',
    'Cost accounting analysis: infrastructure costs (GPU, bandwidth, training amortization) vs published pricing; comparison with alternative providers (Anthropic, Google, Azure OpenAI); margin transparency from financial disclosures if available',
    'If extractive: tangled rope classification confirmed for all perspectives; snare perspective reflects genuine overcharging. If competitive: classification shifts toward rope for most perspectives; extraction claim softens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(api_pricing_sustainability, empirical, 'Whether API pricing reflects cost or economic rent').

omega_variable(
    locked_in_customers_proportion,
    'What fraction of OpenAI API customers face switching costs > 50% of their deployment value?',
    'Survey of API customers regarding switching cost estimates; analysis of customer retention after price increases; longitudinal tracking of customers migrating to alternatives',
    'If > 60%: snare classification is accurate for majority. If < 30%: trap perspective overstates constraint severity; rope classification more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locked_in_customers_proportion, empirical, 'Proportion of customers with high switching costs').

omega_variable(
    regulatory_intervention_likelihood,
    'Will antitrust or API regulation materially change OpenAI''s access control enforcement within 5 years?',
    'Regulatory filing analysis (DOJ, FTC, EU Digital Markets Act); precedent from prior API monopolies (AWS, Apple App Store); enforcement action timeline',
    'If yes: constraint transforms to snare with regulatory ceiling, or scaffold with regulatory sunset. If no: current tangled rope classification persists unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_likelihood, empirical, 'Likelihood of regulatory intervention on API access').

omega_variable(
    rate_limit_functional_necessity,
    'Are OpenAI''s published rate limits technically necessary for system stability or primarily revenue-driven pricing differentiation?',
    'Technical analysis: compare OpenAI''s published utilization rates vs theoretical maximum capacity; examine whether tier limits scale with actual demand vs published tiers; analysis of competitor rate limit strategies',
    'If necessary: suppression is justified (not coercive), classification leans toward rope. If revenue-driven: suppression is rent-extracting, classification confirms snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rate_limit_functional_necessity, empirical, 'Whether rate limits serve technical necessity or pricing strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_api_access, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oaa_tr_t0, openai_api_access, theater_ratio, 0, 0.32).
narrative_ontology:measurement(oaa_tr_t6, openai_api_access, theater_ratio, 6, 0.4).
narrative_ontology:measurement(oaa_tr_t12, openai_api_access, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(oaa_be_t0, openai_api_access, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(oaa_be_t6, openai_api_access, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(oaa_be_t12, openai_api_access, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_api_access, resource_allocation).
narrative_ontology:affects_constraint(openai_api_access, ai_model_training_data_access).
narrative_ontology:affects_constraint(openai_api_access, large_language_model_deployment_costs).

% DUAL FORMULATION NOTE:
% The OpenAI API access constraint is downstream of GPU scarcity and model training data constraints. The API layer creates its own constraint through access control and pricing power. These are linked: GPU scarcity justifies coordination mechanisms (rate limits), which OpenAI leverages to extract economic rent through tiered pricing. Upstream constraint: GPU supply. This constraint: API access and pricing. Downstream constraints: application-level service availability and cost structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openai_api_access, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
