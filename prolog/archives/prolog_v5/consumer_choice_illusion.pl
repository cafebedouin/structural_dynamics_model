% ============================================================================
% CONSTRAINT STORY: consumer_choice_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_choice_illusion, []).

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
 *   constraint_id: consumer_choice_illusion
 *   human_readable: Consumer Choice Illusion: Coordination and Extraction in Market Selection
 *   domain: economic/cognitive
 *
 * SUMMARY:
 *   The consumer choice illusion represents a structural constraint that
 *   conflates coordination (connecting buyers and sellers, solving matching
 *   problems) with extraction (controlling information visibility,
 *   prioritizing platform-favored products, harvesting consumer attention and
 *   data). Platforms present themselves as neutral infrastructure for
 *   consumer choice while simultaneously architecting choice through
 *   algorithmic ranking, recommendation systems, and interface design. The
 *   constraint exhibits a perspectival spectrum: consumers see themselves as
 *   autonomous agents exercising choice (but lack visibility into the
 *   algorithmic filters determining their options); sellers see platforms as
 *   necessary intermediaries (but cannot compete without algorithmic favor);
 *   platforms see themselves as coordinators solving transaction problems
 *   (while extracting rents through commissions, data, and attention);
 *   regulators impose choice-protecting requirements (but the theater of
 *   compliance masks underlying asymmetries); alternative platforms
 *   (cooperative marketplaces, open APIs, decentralized review) offer sunset
 *   logic by creating competitive pressure. The theater ratio (0.68) reflects
 *   that platform choice architecture — filters, reviews, comparisons,
 *   personalization — creates the appearance of agency while algorithmic
 *   ranking determines actual outcomes. The rising extractiveness over time
 *   reflects platform evolution toward greater algorithmic control and data
 *   exploitation as competitive moats deepen.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary victims (powerless/trapped) — cannot opt out of market participation; selection options determined by algorithmic ranking beyond consumer visibility or control
 *   - Consumer Autonomy (Collective): Secondary victim (powerless/trapped) — abstract collective good; market segmentation and preference manipulation erode the informational basis for autonomous choice
 *   - Small Independent Sellers: Secondary victims (moderate/constrained) — dependent on platform reach but bear algorithmic de-prioritization and commission extraction
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture rents through commissions, data harvesting, preferential placement, and attention extraction without bearing selection costs
 *   - Incumbent Brands: Secondary beneficiaries (powerful/arbitrage) — receive algorithmic preference and have resources to optimize for platform ranking
 *   - Regulatory Agencies: Institutional actors (organized/constrained) — impose performative compliance requirements that preserve choice theater while leaving extraction mechanisms intact
 *   - Alternative Market Coalition: Organized agents (organized/mobile) — building transparent marketplaces, open-source platforms, cooperative buyer networks that offer genuine exit pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent platform architecture as inevitable feature of scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_choice_illusion, 0.58).
domain_priors:suppression_score(consumer_choice_illusion, 0.62).
domain_priors:theater_ratio(consumer_choice_illusion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_choice_illusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(consumer_choice_illusion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(consumer_choice_illusion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_choice_illusion, tangled_rope).
narrative_ontology:human_readable(consumer_choice_illusion, "Consumer Choice Illusion: Coordination and Extraction in Market Selection").
narrative_ontology:topic_domain(consumer_choice_illusion, "economic/cognitive").

domain_priors:requires_active_enforcement(consumer_choice_illusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_choice_illusion, platform_operators).
narrative_ontology:constraint_beneficiary(consumer_choice_illusion, incumbent_sellers).
narrative_ontology:constraint_victim(consumer_choice_illusion, consumer_autonomy).
narrative_ontology:constraint_victim(consumer_choice_illusion, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER AS EPISTEMIC PRISONER (SNARE) — Consumers face a paradox: genuine choice requires information about alternatives, quality, and prices, but the platforms that enable choice simultaneously control information visibility through algorithms, ranking, search results, and recommendation systems. Exit is structurally trapped — consumers cannot opt out of market participation without abandoning economic participation. The apparatus of choice (sorting, filtering, comparison) is the apparatus of constraint. Maximum extraction with minimal perceived coercion because choice theater masks the constraint.
constraint_indexing:constraint_classification(consumer_choice_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL INDEPENDENT SELLER (TANGLED ROPE) — Must use platform intermediaries to access consumers; genuine coordination function (matching buyers and sellers) exists, but platform controls visibility through algorithmic ranking and pays itself through commissions/placement fees. Seller bears both extraction (commissions, data harvesting, algorithmic de-prioritization of non-favored products) and coordination benefits (reach, logistics infrastructure, payment processing). Exit is constrained by the platform's dominance — seller could leave but would lose market access. Asymmetric extraction across both axes.
constraint_indexing:constraint_classification(consumer_choice_illusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: aggregating buyers and sellers, solving matching and trust problems, enabling transactions at scale. Benefits flow to the platform through commission structures, data harvesting, preferential treatment for aligned sellers, and attention extraction (ad placements). The operator has arbitrage options — can shift to different markets or introduce new features without bearing costs. Sees coordination function as paramount; extraction is legitimate transaction fee.
constraint_indexing:constraint_classification(consumer_choice_illusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE MARKET COALITION (SCAFFOLD) — Organized actors (cooperatives, open-source platforms, regulatory bodies) are building alternative matching mechanisms: open API marketplaces, data-portable standards, transparent ranking algorithms, decentralized review systems. These alternatives have a sunset logic — if adoption grows, the incumbents' extraction mechanism loses force. Constraints experienced as temporary, with genuine exit pathways emerging. Theater declining as alternatives reduce reliance on centralized choice curation.
constraint_indexing:constraint_classification(consumer_choice_illusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — Consumer protection regulations (disclosure requirements, choice architecture guidelines, algorithm transparency mandates) impose performative compliance rather than structural change. Platforms add choice-enabling features (filters, reviews, comparisons) that preserve the appearance of agency while algorithmic ranking and recommendation still determine actual selection. The regulatory apparatus persists through institutional inertia despite low functional bite — platforms meet letter-of-law compliance while preserving extraction mechanisms. Theater ratio dominates because compliance creates the appearance of choice without structural competition.
constraint_indexing:constraint_classification(consumer_choice_illusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, information asymmetry between sellers and buyers is inherent to commerce — no matching system can fully eliminate information gaps, and all intermediaries possess positional power to curate selection. This perspective sees the choice constraint as an immutable feature of transaction costs and bounded rationality. The engine's false summit detector will identify this as naturalization of what is actually a contingent institutional arrangement: the specific degree of information asymmetry and the algorithmic curation mechanisms are engineered, not inevitable.
constraint_indexing:constraint_classification(consumer_choice_illusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_choice_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_choice_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_choice_illusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_choice_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_choice_illusion, TR),
    TR >= 0.70.

:- end_tests(consumer_choice_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform captures significant economic surplus through commissions, data, and attention, but extraction is legitimately tied to genuine coordination services — the platform does solve real matching and trust problems. The extraction value reflects the asymmetry between platform control over selection visibility and consumer inability to access alternative ranking signals. The value increased over the interval as algorithmic control deepened and platforms consolidated competitive advantage. Suppression (0.62): Moderate-high. Significant barriers to authentic choice include: algorithmic opacity (consumers cannot see ranking logic), data lock-in (switching costs to alternative platforms), network effects (platforms offer more sellers/products), cognitive load (information overload makes algorithmic curation attractive), and information asymmetry (platforms know more about products and consumers than consumers know). Suppression has remained relatively stable, reflecting that these barriers are structural to the platform business model rather than degrading over time. Theater ratio (0.68): High. Choice-enabling features (product reviews, comparison tools, filtering options, personalized recommendations) create the perception of agency while algorithmic ranking and recommendation systems silently determine actual selection pathways. The theater ratio increased over the interval as platforms invested in choice aesthetics while deepening algorithmic control. Compliance with consumer protection regulations (disclosure requirements, algorithm auditing, choice architecture guidelines) adds performative legitimacy without structural change. The constraint exhibits Goodhart drift: the more choice infrastructure platforms provide, the less authentic the choice becomes.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates agents who experience the platform as a coordination solution from those who experience it as an extraction mechanism. The beneficiary (platform operator) sees Rope — genuine matching service. The consumer sees Snare — illusion of choice masking algorithmic control. The seller sees Tangled Rope — necessity and asymmetry mixed. The alternative coalition sees Scaffold — temporary problem with structural exit paths. The regulatory authority sees Piton — theater persisting through institutional habit. The analytical observer risks false summit (Mountain) — naturalizing contingent architecture. The gaps are measurable: beneficiaries' experienced χ approaches 0 (negative extraction, pure coordination); powerless consumers' experienced χ approaches maximum (pure extraction, minimal perceived coordination). The perspectival range from rope to snare is the diagnostic signature that the constraint is authentically tangled rope — it has both genuine coordination and genuine extraction, and different observers see predominantly one or the other depending on their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) flows from the agent's structural position relative to the extraction mechanism. Consumers are structurally trapped — they cannot participate in markets without submitting to platform selection architecture, and network effects prevent exit to competitors. Their d approaches 1.0 (full target). Platform operators are beneficiaries with arbitrage — they can shift business models, launch new products, or exit markets without losing market access. Their d approaches 0.0 (full beneficiary). Sellers occupy a middle position: constrained exit (could build direct sales channels but lose platform reach) places their d in the 0.55–0.70 range. Regulators represent organized institutional power (d ~0.40) with the capacity to impose constraints but without perfect enforcement. Alternative platforms represent organized agents with mobile exit (d ~0.45) because they have strategic optionality. The constraint's overall extractiveness (0.58) reflects the weighted average of these positions — high extraction experienced by powerless consumers, moderate extraction experienced by constrained sellers, negative extraction (coordination benefit) experienced by institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The consumer choice illusion conflates three structurally distinct claims with different ε values. The manifesto should split this into a constraint family: (1) Platform Information Asymmetry (ε=0.35, Rope) — the coordination service itself; (2) Algorithmic Ranking Opacity (ε=0.62, Tangled Rope) — the hidden selection mechanism; (3) Network Effects Barrier (ε=0.71, Snare) — the competitive moat. Each has its own empirical footprint, remediation pathways, and perspectival range. The unified 'consumer choice illusion' story obscures these distinctions. However, because the prompt treats this as a single constraint, the analysis treats it as Tangled Rope with strong snare characteristics: genuine coordination function (information asymmetry reduction) paired with substantial extraction (algorithmic control and data harvesting). The mandatrophy is resolved by recognizing that platforms ARE solving matching problems (coordination is real) while SIMULTANEOUSLY controlling information visibility (extraction is real). The theater ratio rising over time signals Goodhart drift: as platforms invest in choice features, the paradox deepens — more choice infrastructure makes the underlying algorithmic control more invisible. The constraint's classification as Tangled Rope rather than pure Snare depends on whether the coordination benefits to consumers (product discovery, price comparison, transaction security) are genuine or merely cover stories. The omegas address whether platforms could provide equivalent coordination with lower extraction (transparent algorithms, competitive marketplaces, data portability standards). If yes, extraction is engineered. If no, it approaches structural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentic_vs_simulated_choice,
    'Does the existence of multiple platform options create genuine competitive selection pressure, or does network effects and switching costs render the platforms functionally equivalent from the consumer''s epistemic perspective?',
    'Comparative analysis of consumer decision-making on different platforms; measurement of actual switching rates vs. stated preference for alternatives; analysis of whether platform-switching reduces effective extraction',
    'If platforms create genuine competition: extraction is Tangled Rope (mixed coordination-extraction). If network effects render switching prohibitively costly: extraction becomes pure Snare (trapped competitors reinforce platform power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authentic_vs_simulated_choice, empirical, 'Whether multi-platform availability creates authentic competition or illusory choice').

omega_variable(
    algorithmic_ranking_determinism,
    'To what degree do algorithmic ranking and recommendation systems deterministically constrain consumer choice outcomes versus providing navigational aid for genuinely autonomous selection?',
    'Measurement of algorithm-driven selection concentration vs. alternative-search rates; comparison of outcomes when algorithms are transparent/auditable vs. opaque; analysis of whether algorithm changes shift selection distribution',
    'If algorithms are primarily deterministic: constraint is Snare (illusion of choice masks algorithmic control). If algorithms primarily reduce information load without determining outcomes: constraint is Rope (genuine coordination mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_ranking_determinism, empirical, 'Whether algorithmic ranking determines or merely structures consumer choice').

omega_variable(
    extraction_opacity_mechanism,
    'Is the suppression mechanism primarily structural (consumers genuinely unable to access alternatives) or internalized (consumers believe they are choosing freely despite algorithmic constraints)?',
    'Post-algorithm-transparency studies: do consumers exposed to ranking logic change selection behavior? Survey of consumer beliefs about platform neutrality vs. experimental measurement of algorithmic bias; longitudinal tracking of behavior change after awareness interventions',
    'If suppression is primarily internalized: constraint exhibits identity_locked dynamics (consumers identify with ''smart shopping'' on the platform). If structural: constraint is purely trapped. Mixed suppression affects whether constraint is snare vs. tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_opacity_mechanism, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    platform_heterogeneity_constraint,
    'Do different platforms (Amazon, eBay, specialty marketplaces, direct seller sites) create sufficiently heterogeneous choice ecologies that the constraint is local (per-platform) rather than global, or does network effect consolidation make the constraint essentially platform-invariant?',
    'Mapping of product availability, pricing, and recommendation patterns across major platforms; measurement of consumer awareness of cross-platform options; analysis of transaction flow concentration',
    'If platforms are heterogeneous: consumers can escape constraints by switching platforms (mobile exit). If consolidated: escape routes are illusory, and the constraint is truly trapped (global).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_heterogeneity_constraint, empirical, 'Whether platforms are heterogeneous enough to provide genuine exit alternatives').

omega_variable(
    historical_counterfactual_transparency,
    'Would consumer selection outcomes differ substantially if ranking algorithms were fully transparent and auditable, or is information asymmetry an inevitable feature of matching problems at scale?',
    'Natural experiments with transparent-ranking platforms (DuckDuckGo, specialty marketplaces); comparison of consumer search behavior and outcome satisfaction; measurement of competitive dynamics under transparency vs. opacity',
    'If transparent algorithms reduce extraction: constraint is engineered suppression (Tangled Rope/Snare). If transparency has minimal effect: constraint approximates Mountain (information asymmetry is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_counterfactual_transparency, empirical, 'Whether algorithm transparency reduces extraction or is inherent to scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_choice_illusion, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cci_tr_t0, consumer_choice_illusion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cci_tr_t4, consumer_choice_illusion, theater_ratio, 4, 0.55).
narrative_ontology:measurement(cci_tr_t8, consumer_choice_illusion, theater_ratio, 8, 0.68).
narrative_ontology:measurement(cci_tr_t12, consumer_choice_illusion, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(cci_be_t0, consumer_choice_illusion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cci_be_t4, consumer_choice_illusion, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(cci_be_t8, consumer_choice_illusion, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(cci_be_t12, consumer_choice_illusion, base_extractiveness, 12, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_choice_illusion, information_standard).
narrative_ontology:affects_constraint(consumer_choice_illusion, attention_extraction_mechanism).
narrative_ontology:affects_constraint(consumer_choice_illusion, data_harvesting_asymmetry).
narrative_ontology:affects_constraint(consumer_choice_illusion, seller_algorithmic_dependency).

% DUAL FORMULATION NOTE:
% Consumer choice illusion decomposes into three constraint stories if ε-invariance principle is applied strictly: (1) Platform information asymmetry (low ε, Rope) — genuine coordination function; (2) Algorithmic ranking opacity (medium ε, Tangled Rope) — mixed coordination/extraction; (3) Network effects barrier (high ε, Snare) — pure extraction via switching costs. The unified story treats all three as a single tangled rope constraint. The network links map the decomposition: information asymmetry feeds algorithmic dependency, which creates network effects that enable data harvesting extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumer_choice_illusion, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
