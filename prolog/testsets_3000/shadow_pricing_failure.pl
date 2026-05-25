% ============================================================================
% CONSTRAINT STORY: shadow_pricing_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shadow_pricing_failure, []).

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
 *   constraint_id: shadow_pricing_failure
 *   human_readable: The Invisible Market Collapse: Shadow Pricing Failure
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Shadow pricing failure occurs when institutional frameworks
 *   systematically suppress or ignore the true economic value of resources
 *   that lack explicit market prices. This constraint operates across three
 *   major domains: personal data in digital platforms, carbon and
 *   environmental externalities, and attention in cognitive and social
 *   spaces. The suppression is not accidental — it is maintained through
 *   specific enforcement mechanisms including regulatory capture (lobbying
 *   that prevents carbon pricing), network lock-in (switching costs that trap
 *   users), accounting standards (that permit carbon-blind valuations), and
 *   ideological frameworks (that treat shadow-priced resources as
 *   'externalities' rather than stolen commons). The constraint exhibits both
 *   genuine coordination function (free-at-point-of-use digital services
 *   reduce friction) and asymmetric extraction (platforms capture data value;
 *   producers avoid carbon costs; attention extractors monetize cognitive
 *   surplus). The theater ratio reflects the gap between stated institutional
 *   commitments to 'market efficiency' and 'sustainable development' versus
 *   actual pricing mechanisms that permit shadow suppression. The
 *   constraint's extractiveness has grown over two decades as digital
 *   platforms consolidated power, climate impacts accumulated, and attention
 *   became the primary scarce resource in information economies.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary victims (powerless/trapped) — individuals systematically underpriced or priced at zero in digital ecosystems
 *   - Climate Commons: Primary victim (powerless/trapped) — environmental degradation costs externalized and borne by future generations
 *   - Cognitive Ecosystem: Primary victim (powerless/trapped) — attention harvested and monetized without compensation or consent
 *   - Platform Ecosystems: Primary beneficiaries (institutional/arbitrage) — capture data value, avoid carbon costs, monetize attention arbitrage
 *   - Carbon Extractors: Primary beneficiaries (powerful/mobile) — energy producers, transportation, industrial processes externalize climate costs
 *   - Consumer-Participants: Secondary victim (moderate/constrained) — receive service benefit but lose data, bear climate risk through individual choices constrained by unshadowed prices
 *   - Regulatory Authorities: Powerful/mixed (powerful/mobile) — could implement shadow pricing reforms but captured by industry pressure and institutional complexity
 *   - ESG and Sustainability Movements: Organized reformers (organized/constrained) — building alternative pricing scaffolds (carbon markets, data trusts, attention accounting) with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shadow_pricing_failure, 0.58).
domain_priors:suppression_score(shadow_pricing_failure, 0.68).
domain_priors:theater_ratio(shadow_pricing_failure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shadow_pricing_failure, extractiveness, 0.58).
narrative_ontology:constraint_metric(shadow_pricing_failure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shadow_pricing_failure, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shadow_pricing_failure, tangled_rope).
narrative_ontology:human_readable(shadow_pricing_failure, "The Invisible Market Collapse: Shadow Pricing Failure").
narrative_ontology:topic_domain(shadow_pricing_failure, "economic/technological").

domain_priors:requires_active_enforcement(shadow_pricing_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shadow_pricing_failure, data_extracting_platforms).
narrative_ontology:constraint_beneficiary(shadow_pricing_failure, carbon_externality_producers).
narrative_ontology:constraint_beneficiary(shadow_pricing_failure, attention_monetizing_firms).
narrative_ontology:constraint_victim(shadow_pricing_failure, data_subjects).
narrative_ontology:constraint_victim(shadow_pricing_failure, climate_commons).
narrative_ontology:constraint_victim(shadow_pricing_failure, cognitive_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individuals trapped in digital ecosystems where their data is systematically underpriced or priced at zero. No real exit option: participation in modern life requires data disclosure. The shadow price of their attention, behavioral patterns, and identity is extracted continuously while institutional accounting frames it as 'free service.' Maximum experienced extraction with zero alternatives.
constraint_indexing:constraint_classification(shadow_pricing_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE COMMONS (SNARE) — The shadow price of carbon and environmental degradation is institutionally set to zero or near-zero in market transactions. Extractive producers capture value while the commons bears costs that accumulate across generations. No exit mechanism for future generations. Extraction mechanism: institutional suppression of shadow carbon pricing through regulatory capture and accounting standards that permit carbon-blind valuations.
constraint_indexing:constraint_classification(shadow_pricing_failure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSUMER-PARTICIPANT (TANGLED ROPE) — Moderate power through collective action (unions, consumer advocacy, regulation). Experiences both coordination benefit (digital services enable communication, commerce) and extraction (data harvesting, attention capture). Exit is constrained by network effects and switching costs, but some agency exists through regulation, data portability rights, and consumer pressure. Mixed extraction and benefit.
constraint_indexing:constraint_classification(shadow_pricing_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM ECOSYSTEM (ROPE) — Digital platforms experience shadow pricing failure as a coordination solution: the suppression of explicit data pricing enables network scaling. Free-at-point-of-use models create liquidity and reduce transaction costs. Beneficiaries experience the constraint as pure coordination — markets for data would be smaller and more friction-laden. Arbitrage option: can redeploy capital to alternative business models if shadow pricing mechanisms lose enforcement.
constraint_indexing:constraint_classification(shadow_pricing_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Powerful but constrained by institutional capture and technical complexity. Sees shadow pricing failure as both coordination problem (markets need common accounting standards) and extraction opportunity (regulation creates arbitrage rents). Mobility: can implement carbon pricing, data privacy regimes, or attention taxation, but faces industry pressure and regulatory arbitrage (capital flight). Mixed enforcement and benefit.
constraint_indexing:constraint_classification(shadow_pricing_failure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ESG AND SUSTAINABILITY MOVEMENT (SCAFFOLD) — Organized actors (NGOs, impact funds, carbon accounting standards) are building alternative pricing mechanisms with a sunset: carbon pricing markets, ESG disclosure regimes, data valuation frameworks. These represent temporary scaffolding that would dissolve if shadow pricing mechanisms are internalized into market-clearing mechanisms. High suppression is tolerated because agents see an exit path through norm change and institutional reform. Theater: much ESG scoring is performative, but genuine price discovery mechanisms are emerging (carbon credits, data trusts).
constraint_indexing:constraint_classification(shadow_pricing_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CLASSICAL ECONOMICS FRAMEWORK (PITON) — The neoclassical assumption that 'all resources have prices' and that market clearing solves allocation problems is maintained as institutional theater despite substantial evidence of systematic shadow price suppression. Economics departments continue teaching equilibrium models; policy frameworks cite 'market efficiency' while implementing mechanisms that prevent shadow price discovery. The framework persists through academic inertia and ideological commitment, not functional verification. Theater ratio high because the performative content (equilibrium diagrams, efficiency proofs) remains decoupled from empirical market structure.
constraint_indexing:constraint_classification(shadow_pricing_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal/analytical perspective, information, energy, and attention are scarce resources with real thermodynamic costs. Any economic system that suppresses the pricing of scarce resources will misallocate them — this is as immutable as energy conservation. The constraint appears as a natural law: pricing failure is inherent to systems that permit shadow price suppression. However, this risks naturalizing what is actually a political/institutional choice: shadow pricing is not inevitable, it is maintained through specific enforcement mechanisms (regulatory capture, accounting standards, network lock-in). The mountain perspective obscures the contingent institutional arrangements that sustain the constraint.
constraint_indexing:constraint_classification(shadow_pricing_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shadow_pricing_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shadow_pricing_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shadow_pricing_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shadow_pricing_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shadow_pricing_failure, TR),
    TR >= 0.70.

:- end_tests(shadow_pricing_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint involves systematic suppression of prices for three major resource classes. The beneficiaries (platforms, carbon producers, attention monetizers) capture significant value by avoiding explicit payments for resources they use. The victims (data subjects, climate commons, attention ecosystem) bear costs that are externalized and often invisible. The value transferred is substantial but not maximal — some coordination benefits are real (free digital services), some consumer surplus remains (goods and services are cheaper than they would be if shadow prices were internalized), and some reform mechanisms are emerging (GDPR, carbon pricing, attention tokens). If shadow prices were fully internalized, extractiveness would rise toward 0.75+ and convert to snare across all victims. Suppression (0.68): High. Multiple enforcement mechanisms maintain shadow price invisibility: regulatory capture prevents carbon and data pricing; network effects trap users in unshadowed digital platforms; accounting standards permit carbon-blind valuations; ideological frameworks treat externalities as natural rather than contingent. But suppression is not absolute — some jurisdictions (EU with GDPR, carbon-taxing nations) have reduced suppression; some platforms offer data access controls; some attention-aware alternatives exist. Theater ratio (0.65): Moderate-high. Institutional claims about market efficiency, ESG commitment, and sustainable development are substantially performative. Companies report 'net zero' while lobbying against carbon pricing; platforms pledge 'data privacy' while harvesting behavior; economists teach equilibrium models divorced from shadow price suppression. The gap between stated frameworks and actual pricing mechanisms reflects theater accumulation over time.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The trapped victim (data subject, climate commons) sees pure extraction (Snare) with no real alternatives. The consumer-participant sees mixed extraction and coordination benefit (Tangled Rope) — they receive services but lose data and bear climate costs. The platform beneficiary sees pure coordination (Rope) — shadow price suppression enables network scaling and reduces friction. The regulatory authority sees a mixed landscape (Tangled Rope or Scaffold) — they could reform but face capture and complexity. The ESG movement sees a temporary problem with emerging solutions (Scaffold) — carbon markets and data trusts are building alternative pathways. The classical economics framework sees none of this as a problem (Piton) — equilibrium models assume all resources are priced; the performative ritual of economics teaching persists despite systematic shadow suppression. The analytical observer risks naturalizing the suppression as inherent to economies of scale and information asymmetry (Mountain), when in fact it is maintained through contingent enforcement mechanisms. The perspectival gaps reflect real differences in power, exit options, and beneficiary/victim status — they are not measurement artifacts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Data subjects have d → 1.0 (trapped powerless victims): they bear full cost of data extraction with zero exit options. The climate commons has d → 1.0 (trapped powerless victim): bears cost of carbon externalization across generations with no exit mechanism. Platform beneficiaries have d → 0.05 (institutional arbitrage): they extract value and can redeploy capital if shadow pricing enforcement changes, experiencing minimal effective extraction. Carbon producers have d → 0.15 (powerful arbitrage): capture value through externalization but retain mobility. Consumer-participants have d → 0.55 (moderate constrained): moderate power through regulation and collective action, but constrained by switching costs and network effects. The regulatory authority has d → 0.45 (powerful mobile): could implement shadow pricing reforms but face capture pressures and technical complexity, giving constrained mobility. The ESG movement has d → 0.60 (organized constrained): organized but constrained by incumbent resistance and slow norm change. Each perspective's experienced extractiveness χ is modulated by these directionality values through the sigmoid function, producing the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   SHADOW PRICING FAILURE RESOLVES MANDATROPHY as follows: The constraint is fundamentally Tangled Rope (moderate-high extractiveness, genuine coordination function, asymmetric victim/beneficiary structure, requires enforcement). However, from specific victim perspectives (data subjects, climate commons), it appears as pure Snare — the coordination benefits are captured by beneficiaries, and victims experience only extraction. From beneficiary perspectives, it appears as pure Rope — the shadow suppression is experienced as coordination mechanism. The mandatrophy is resolved by recognizing that BENEFICIARY/VICTIM ASYMMETRY is the defining structural feature: the same institutional arrangement (shadow price suppression) coordinates efficient allocation for beneficiaries while extracting from victims. This is the definition of Tangled Rope. The classification is not ambiguous; it is perspectival. The analytical observer who claims the constraint is 'really' a Snare or 'really' a Rope is making a normative choice about whose perspective counts. The deferential realism framework resolves this by including all perspectives and showing the structural gap. The piton classification (classical economics framework) represents institutional theater: the performative commitment to equilibrium and efficiency masks the systematic shadow suppression that generates the constraint. The mountain classification (thermodynamic inevitability) naturalizes what is actually political choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shadow_price_quantification,
    'What is the true shadow price of personal data, and does it vary systematically by population segment, behavioral profile, or use case?',
    'Privacy-preserving data valuation models; market experiments with opt-in personal data sales; comparison of implicit valuations across platforms and geographies',
    'If data shadow prices are high and unequal across segments: extraction is severe and regressive. If data shadow prices are low or uniform: current platform models are closer to efficient equilibrium, and the constraint is coordination rather than snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shadow_price_quantification, empirical, 'Quantification and heterogeneity of personal data shadow prices').

omega_variable(
    carbon_pricing_sufficiency,
    'Do existing carbon pricing mechanisms (ETS, voluntary markets, implicit regulatory costs) adequately capture the true shadow price of atmospheric carbon, or is suppression still systematic?',
    'Comparison of observed carbon prices to climate damage estimates; analysis of carbon pricing pass-through to end-consumer prices; longitudinal tracking of allocation distortions',
    'If pricing is sufficient: carbon commons extraction is moving toward snare→tangled_rope transition. If pricing is still suppressed: carbon extraction remains snare-class with slow institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_pricing_sufficiency, empirical, 'Adequacy of carbon pricing mechanisms relative to atmospheric damage').

omega_variable(
    attention_economy_price_discovery,
    'Is the attention economy capable of price discovery without third-party intervention, or is attention intrinsically difficult to price because it is not fungible across contexts?',
    'Analysis of attention markets (e.g., creator economies, attention tokens); comparison of market-clearing rates to downstream behavioral metrics; experiments with explicit attention pricing mechanisms',
    'If attention can be priced: shadow pricing failure is contingent on policy choice (snare can become rope). If attention is inherently difficult to price: the constraint has fundamental coordination aspects that market mechanisms alone cannot solve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attention_economy_price_discovery, conceptual, 'Whether attention is intrinsically difficult to price in markets').

omega_variable(
    regulatory_capture_reversibility,
    'Is the suppression of shadow pricing enforced through deliberate regulatory capture that could be reversed through political reform, or is it an equilibrium outcome of rational actors with misaligned incentives?',
    'Comparative analysis of jurisdictions with different regulatory postures (GDPR vs. US laissez-faire, carbon-taxing vs. carbon-free economies); longitudinal analysis of policy change and industry response',
    'If capture is reversible: the constraint is fundamentally a tangled_rope with political exit (reform). If it is equilibrium: the constraint has snare aspects that persist even with regulatory intention to change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_reversibility, empirical, 'Whether shadow price suppression is reversible through policy reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shadow_pricing_failure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shadow_tr_t0, shadow_pricing_failure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(shadow_tr_t10, shadow_pricing_failure, theater_ratio, 10, 0.62).
narrative_ontology:measurement(shadow_tr_t20, shadow_pricing_failure, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(shadow_be_t0, shadow_pricing_failure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shadow_be_t10, shadow_pricing_failure, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(shadow_be_t20, shadow_pricing_failure, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shadow_pricing_failure, resource_allocation).
narrative_ontology:affects_constraint(shadow_pricing_failure, regulatory_capture).
narrative_ontology:affects_constraint(shadow_pricing_failure, attention_extraction).
narrative_ontology:affects_constraint(shadow_pricing_failure, carbon_externality).
narrative_ontology:affects_constraint(shadow_pricing_failure, data_asymmetry).

% DUAL FORMULATION NOTE:
% Shadow pricing failure is a constraint family with three structurally distinct components: data underpricing (platform extraction, information asymmetry), carbon externality (environmental commons destruction, intergenerational transfer), and attention monetization (cognitive resource depletion, behavioral manipulation). Each component has its own ε, enforcement mechanism, and victim structure. They are linked through a common causal mechanism (institutional suppression of market-clearing prices) and through institutional actors that profit from multiple forms of shadow suppression (digital platforms extract data AND attention; energy companies externalize carbon AND lobby against pricing). The family is treated as a single constraint because the same institutional frameworks (regulatory capture, accounting standards, ideological narratives) maintain all three forms of suppression simultaneously. Separate constraint stories for each component would duplicate the institutional analysis; the family view captures the multiplier effect of bundled suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shadow_pricing_failure, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
