% ============================================================================
% CONSTRAINT STORY: platform_search_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_search_cost_asymmetry, []).

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
 *   constraint_id: platform_search_cost_asymmetry
 *   human_readable: Platform Search Cost Asymmetry
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   Platform search cost asymmetry describes the structural tension between
 *   the genuine coordination function that platform search provides (matching
 *   buyers to sellers at scale) and the extractive mechanisms platforms
 *   deploy through ranking opacity and mandatory paid placement. Users and
 *   small merchants depend on platform search for customer discovery with no
 *   viable alternatives; platforms exploit this dependency to extract value
 *   through advertising revenue and merchant fees. The constraint exhibits
 *   genuine coordination value—platforms do solve the matching problem more
 *   efficiently than pre-digital alternatives—while simultaneously enabling
 *   high-margin extraction through algorithmic opacity and forced paid
 *   visibility. The theater ratio (0.55) reflects that platforms maintain
 *   performative commitments to 'fair ranking' and 'relevance optimization'
 *   while obscuring the ranking factors that determine visibility, allowing
 *   them to capture value while appearing neutral.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — depend on platform search for product discovery; no coordination benefit; bear full cost of ranking bias through poor recommendations and higher prices
 *   - Small Merchants: Primary victims (powerless/trapped) — cannot afford visibility without paid placement; trapped by dependency on platform traffic; systematic algorithmic deprioritization forces advertising spending
 *   - Premium Merchants: Primary beneficiaries (institutional/arbitrage) — can afford search visibility; experience platform as pure coordination mechanism; have options to negotiate or switch platforms
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures revenue from search ranking opacity and mandatory placement fees; maintains high exit optionality; sees constraint as coordination solution
 *   - Merchant Coalition: Organized secondary agent (organized/constrained) — experiences mixed coordination and extraction; has collective power but trapped by platform dependency and high exit costs
 *   - Regulatory Authority: Powerful but constrained agent (powerful/constrained) — can mandate transparency but faces institutional friction; sees genuine coordination value alongside asymmetric extraction
 *   - Alternative Platform Coalition: Nascent escape path (organized/constrained) — decentralized marketplaces and niche platforms building parallel infrastructure; constrained by network effects but offering sunset mechanism
 *   - Search Transparency Norms: Degraded institutional standard (institutional/arbitrage) — early transparency norms have atrophied; now performative ritual rather than functional constraint on platform behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_search_cost_asymmetry, 0.58).
domain_priors:suppression_score(platform_search_cost_asymmetry, 0.62).
domain_priors:theater_ratio(platform_search_cost_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_search_cost_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_search_cost_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(platform_search_cost_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_search_cost_asymmetry, tangled_rope).
narrative_ontology:human_readable(platform_search_cost_asymmetry, "Platform Search Cost Asymmetry").
narrative_ontology:topic_domain(platform_search_cost_asymmetry, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(platform_search_cost_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_search_cost_asymmetry, platform_operator).
narrative_ontology:constraint_beneficiary(platform_search_cost_asymmetry, premium_merchants).
narrative_ontology:constraint_victim(platform_search_cost_asymmetry, end_users).
narrative_ontology:constraint_victim(platform_search_cost_asymmetry, small_merchants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Structurally dependent on platform search for product discovery. No viable alternative; constrained by information asymmetry and switching costs. Platform manipulates search ranking opacity to extract value through ad-driven commerce. User bears full cost of degraded discovery while platform captures conversion surplus. No coordination benefit experienced.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MERCHANT (SNARE) — Cannot afford search visibility without paid placement. Organic search algorithm systematically deprioritizes non-paying merchants. Trapped by dependency on platform traffic; no alternative distribution channel viable. Extraction mechanism: forced advertising spend. Suppression enforced through algorithmic opacity and platform's exclusive gating of customer access.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MERCHANT COALITION (TANGLED ROPE) — Organized merchant groups experience the constraint as both coordination (platform traffic enables their business) and extraction (mandatory ad spending). They have collective power to negotiate but face high exit costs (losing platform access). The constraint solves a genuine coordination problem (matching buyers and sellers) while extracting rent through algorithmic favoritism and paid placement requirements.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PREMIUM MERCHANT (ROPE) — High-volume merchants who can afford search placement fees experience the constraint as pure coordination. Platform search mechanisms and paid visibility pathways enable efficient buyer-seller matching. Net beneficiary — extraction flows away from this agent through arbitrage options (can switch platforms, negotiate placement terms, or build owned-channel alternatives).
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATOR (ROPE) — Experiences constraint as pure coordination mechanism. Platform search solves genuine collective action problem (connecting millions of buyers and sellers). Captures advertising revenue through search ranking opacity and mandatory placement fees. High exit optionality due to market power and alternative monetization paths.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AUTHORITY (TANGLED ROPE) — Powerful actor but constrained by technical complexity and economic dependency on platform. Sees constraint as both coordination requirement (platforms do enable commerce) and asymmetric extraction (opaque algorithms, mandatory fees). Can enforce limits on search ranking opacity or paid placement requirements, but faces institutional friction and political pressure from platform incumbents and premium merchants.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SEARCH TRANSPARENCY NORMS (PITON) — Early e-commerce platforms published ranking criteria and visibility rules. This norm has degraded through inertia: transparency theater (vague 'relevance' statements) replaces actual algorithmic disclosure. Theater ratio 0.55 reflects ongoing performative commitments to fairness without structural change. The norm persists institutionally but has lost functional verification capacity.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ALTERNATIVE PLATFORM COALITION (SCAFFOLD) — Decentralized marketplaces, open-protocol commerce networks, and competing platforms (niche vertical markets) represent a sunset mechanism for the search cost asymmetry. As alternative distribution channels mature, users and small merchants have increasing exit options. Extraction mechanism loses force as organized agents develop non-platform-dependent business models. Sunset estimated at 8-15 years as infrastructure matures and coordination standards improve.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, information asymmetry between buyer and seller is inherent to commerce; platforms reduce this asymmetry more than pre-digital markets. Search cost cannot be zero; someone must bear the computation. This perspective risks naturalizing what is contingent: the decision to concentrate search ranking power in the platform operator is institutional choice, not physical law. Engine will flag this as a false summit — the apparent immutability masks distributional choice.
constraint_indexing:constraint_classification(platform_search_cost_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_search_cost_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_search_cost_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_search_cost_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_search_cost_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_search_cost_asymmetry, TR),
    TR >= 0.70.

:- end_tests(platform_search_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Platforms have progressively increased reliance on paid placement and algorithmic opacity as primary revenue mechanisms. The base extractiveness reflects that genuine coordination value exists—users and merchants do benefit from efficient matching—but asymmetric extraction has grown as platforms refined monetization. The interval trajectory (0.35 → 0.58) shows extraction accumulation as competition consolidated and alternative platforms failed to scale. Suppression (0.62): Moderate-high. Barriers include algorithmic opacity preventing user/merchant understanding of ranking factors, lack of viable alternatives, switching costs (ported merchant reviews, customer relationships), and informational advantage concentrated in platform. Theater ratio (0.55): Moderate. Platforms publish fairness commitments and algorithmic descriptions ('relevance,' 'customer satisfaction') but these statements lack specificity about how paid placement influences ranking. Theater has remained relatively stable because platforms maintain plausible deniability—they do optimize for stated metrics alongside paid placement, but the weighting is opaque.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence driven by exit options and beneficiary/victim status. The primary gap is between powerless/trapped agents (Snare perspective) and institutional/arbitrage agents (Rope perspective). Trapped agents perceive only extraction because they cannot exit and receive no coordination benefit (their transaction costs would be lower with transparent, competitive search). Beneficiaries perceive pure coordination because the system enables their preferred outcome (premium placement, transaction fees) with no exit pressure. Organized merchants occupy the middle (Tangled Rope) because they both benefit from platform traffic and pay for visibility—they experience the constraint as mixed. The regulatory observer perceives Tangled Rope at national scope because regulatory intervention is real but institution-bound. Alternative platforms perceive Scaffold because they are building a sunset mechanism. The mountain view (natural law) is a false summit—it naturalizes the choice to centralize ranking power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. End users as powerless/trapped agents experience maximum d (≈0.95), yielding high f(d)→high χ; they have no exit and bear costs. Small merchants as powerless/trapped also experience high d and maximum experienced extraction. Premium merchants and platform operator as institutional/arbitrage agents experience low d (≈0.15), yielding negative f(d); they are beneficiaries with escape routes. Organized merchants with constrained exit experience moderate d (≈0.55), reflecting that they benefit from platform access but are locked into spending. Regulatory authority with institutional power but constrained exit in this specific domain experiences d≈0.50, reflecting symmetric exposure to both coordination value and extraction costs. The chi formula χ=ε×f(d)×σ(S) with global scope modifier σ(global)=1.2 amplifies effective extraction for powerless agents—the platform's reach (global connectivity) increases the cost of exit for trapped users and merchants.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival multiplicity. The tension between 'platform search is essential infrastructure enabling efficient commerce (Rope)' and 'platform search is extractive gatekeeping (Snare)' is not resolvable at a single perspective—it IS resolvable by noting that different agents experience different constraints. From the platform's view, search is coordination (Rope). From the trapped user's view, it is extraction (Snare). Both are true. The mandatrophy is dissolved by indexical classification: the constraint IS Rope for institutional actors, IS Tangled Rope for organized merchants, IS Snare for powerless agents. The analytical observer must avoid the mountain fallacy—that there is an underlying 'true' constraint independent of perspective. The presheaf over the indexed observation positions IS the complete description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_ranking_opacity_necessity,
    'Is algorithmic ranking opacity technically necessary for platform search function, or is it a choice that serves extraction?',
    'Comparative analysis of platforms with transparent vs opaque ranking; measurement of user conversion and merchant success rates under each regime; technical feasibility studies of explainable ranking systems',
    'If technically necessary: constraint is more coordinative than extractive (shifts classification toward Rope). If optional: opacity is pure extraction mechanism, and architecture enables Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_ranking_opacity_necessity, empirical, 'Whether algorithmic ranking opacity is technically necessary or extractive choice').

omega_variable(
    paid_placement_market_efficiency,
    'Does paid placement improve or degrade search result relevance for users?',
    'A/B testing of search results with/without paid ranking bias; user satisfaction metrics and conversion rates; merchant revenue outcomes under different placement regimes',
    'If improves: coordination benefit is real; tangled rope classification appropriate. If degrades: paid placement is pure extraction; Snare classification for user perspective more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paid_placement_market_efficiency, empirical, 'Whether paid placement improves search result relevance').

omega_variable(
    small_merchant_margin_viability,
    'Below what threshold of organic visibility do small merchants become unviable without paid search investment?',
    'Longitudinal tracking of merchant profitability as organic search ranking declines; measurement of breakeven point for paid placement investment relative to merchant margin structure',
    'If threshold is low: organic visibility sustainable; merchants have genuine choice. If threshold is high: forced advertising spend functions as coercion; suppression measure understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_merchant_margin_viability, empirical, 'Merchant viability threshold under organic search visibility').

omega_variable(
    alternative_platform_network_effects,
    'Can decentralized or competing platforms achieve search coordination efficiency comparable to dominant platform at meaningful scale?',
    'Measurement of user base growth and merchant success rates on decentralized alternatives; analysis of network effects and switching cost barriers',
    'If achievable: scaffold sunset mechanism is realistic; exit options will materialize. If not: trapped agent status persists; escape routes are illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_platform_network_effects, empirical, 'Whether alternative platforms can provide comparable search coordination').

omega_variable(
    regulatory_intervention_effectiveness,
    'Can algorithmic transparency mandates or ranking fairness regulations eliminate the search cost asymmetry, or do they create new compliance theater?',
    'Post-regulation analysis of platforms subject to transparency requirements; measurement of merchant outcome changes; assessment of compliance depth vs performative transparency',
    'If effective: regulatory pathway becomes primary constraint type (shifts toward supervised Rope). If performative: Piton mechanism (degraded norms) persists despite regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_effectiveness, empirical, 'Effectiveness of algorithmic transparency and fairness regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_search_cost_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psca_tr_t0, platform_search_cost_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(psca_tr_t3, platform_search_cost_asymmetry, theater_ratio, 3, 0.46).
narrative_ontology:measurement(psca_tr_t6, platform_search_cost_asymmetry, theater_ratio, 6, 0.52).
narrative_ontology:measurement(psca_tr_t10, platform_search_cost_asymmetry, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(psca_be_t0, platform_search_cost_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psca_be_t3, platform_search_cost_asymmetry, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(psca_be_t6, platform_search_cost_asymmetry, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(psca_be_t10, platform_search_cost_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_search_cost_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(platform_search_cost_asymmetry, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(platform_search_cost_asymmetry, small_business_digital_dependence).
narrative_ontology:affects_constraint(platform_search_cost_asymmetry, monopolistic_platform_consolidation).

% DUAL FORMULATION NOTE:
% Platform search cost asymmetry is downstream of platform consolidation and upstream of merchant viability constraints. Decomposed into three structurally distinct constraints: (1) algorithmic ranking opacity (ε≈0.45, Tangled Rope) — coordination value in matching + extraction via opacity; (2) paid placement requirement (ε≈0.68, Snare) — forced advertising spend with minimal coordination benefit; (3) search transparency norms (ε≈0.35, Piton) — degraded institutional standard. Each story tracks distinct observables and failure modes. All three are linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_search_cost_asymmetry, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
