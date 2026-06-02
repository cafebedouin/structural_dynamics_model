% ============================================================================
% CONSTRAINT STORY: two_sided_market_power_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_two_sided_market_power_asymmetry, []).

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
 *   constraint_id: two_sided_market_power_asymmetry
 *   human_readable: Two-Sided Market Power Asymmetry
 *   domain: platform_economics/market_structure
 *
 * SUMMARY:
 *   Two-sided markets create structural conditions where a central platform
 *   operator intermediates between two distinct user populations — producers
 *   and consumers — whose participation creates network effects that benefit
 *   both but also create mutual dependence on the platform. The platform
 *   operator's power asymmetry emerges from this intermediation role: as
 *   network effects accumulate, switching costs rise for both sides, enabling
 *   the operator to extract through commissions, data access, algorithmic
 *   preference, and policy unilateralism. The constraint exhibits the full
 *   spectrum of DR classifications because the same structural mechanism
 *   generates genuine coordination benefits (matching at scale, transaction
 *   cost reduction) alongside systematic extraction (asymmetric surplus
 *   capture, lock-in, suppression of alternatives). The rising extractiveness
 *   trajectory (0.35→0.58) reflects the historical pattern: early platforms
 *   justified high fees through genuine coordination scarcity; as scale
 *   increases and network liquidity deepens, the operator increasingly
 *   extracts through algorithmic opacity, data harvesting, and first-party
 *   product preference while claiming neutrality. The theater ratio
 *   trajectory (0.32→0.48) shows increasing performative framing: platforms
 *   increasingly deploy neutrality narratives, algorithmic fairness rhetoric,
 *   and 'ecosystem' language to occlude explicit extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures asymmetric surplus through commissions, data access, and algorithmic control; high exit optionality through pivot capacity
 *   - Dependent Producers: Primary victims (powerless/trapped) — locked into platform dependence through reputation systems, network effects, and switching costs; zero exit options below platform thresholds
 *   - Consumers: Secondary victims (powerless/trapped) — experience extraction through privacy loss, algorithmic curation, and lock-in effects; trapped in network externality dynamics
 *   - Alternative Platform Operators: Secondary actors (moderate/constrained) — face incumbent advantage from scale and data but benefit from operator's extraction pricing; high-cost exit through capital requirements
 *   - Regulatory Coalition: Organized agents (organized/constrained) — intervening through interoperability mandates, labor classification, and antitrust enforcement; designing sunset through technical standards maturation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes structural hybrid of genuine coordination and systematic extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(two_sided_market_power_asymmetry, 0.58).
domain_priors:suppression_score(two_sided_market_power_asymmetry, 0.65).
domain_priors:theater_ratio(two_sided_market_power_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(two_sided_market_power_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(two_sided_market_power_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(two_sided_market_power_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(two_sided_market_power_asymmetry, tangled_rope).
narrative_ontology:human_readable(two_sided_market_power_asymmetry, "Two-Sided Market Power Asymmetry").
narrative_ontology:topic_domain(two_sided_market_power_asymmetry, "platform_economics/market_structure").

domain_priors:requires_active_enforcement(two_sided_market_power_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(two_sided_market_power_asymmetry, platform_operator).
narrative_ontology:constraint_victim(two_sided_market_power_asymmetry, consumer_side_users).
narrative_ontology:constraint_victim(two_sided_market_power_asymmetry, producer_side_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT PRODUCER (SNARE) — Small sellers (merchants, drivers, creators) on Amazon, Uber, or TikTok are locked into platform dependence. Exit barriers include: algorithmic suppression if they reduce sales through the platform, network effects that make alternatives non-viable, switching costs from reputation/rating systems, and absence of substitute markets of comparable scale. Maximal experienced extraction — the producer perceives no genuine exit option and bears asymmetric extraction through commission rates, algorithm manipulation, and unilateral policy changes.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSUMER NETWORK EXTERNALITY VICTIM (SNARE) — Consumers experience extraction through: reduced privacy (data harvesting and surveillance), diminished choice (algorithm curation limiting visibility of alternatives), and switching costs (reputation, purchase history, payment methods locked to platform). Though consumers benefit from convenient access, they perceive the network as monopolistic and face suppression of exit through lock-in effects and degraded experience on competing platforms. Trapped in the network due to coordination dynamics that make leaving costly.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALTERNATIVE PLATFORM OPERATOR (TANGLED ROPE) — Competing platforms (e.g., Shopify competing with Amazon, Lyft with Uber) benefit from the dominant platform's network effects-driven prices and restrictions — they can recruit participants with lower fees and better terms. But they are also constrained by the incumbent's scale and data advantage, requiring significant capital to build network liquidity. This is genuine coordination (competition as price/feature signal) mixed with extraction (incumbent's dominance suppresses alternatives' growth). Exit is possible at high cost.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — The platform operator (Amazon, Uber, TikTok) experiences the constraint as pure coordination: matching supply and demand, facilitating transactions, and capturing network effects. From the operator's position, the two-sided market is a genuine coordination mechanism that produces mutual benefits — without it, neither side can access the other at scale. The extraction (commission rates, data access, algorithmic control) is experienced as legitimate coordination cost and surplus capture. High exit optionality — the operator can shift business models, pivot to adjacent markets, or leverage existing infrastructure for new platforms.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (SCAFFOLD) — Organized regulatory actors (EU Digital Markets Act, US FTC antitrust actions, platform labor advocacy groups) view the constraint as extractive and temporary. The coalition's intervention (interoperability mandates, labor classification, commission caps, data portability) is designed with a sunset: as technical and regulatory infrastructure mature, the dependency relationships weaken. Interoperability standards, mandatory data portability, and algorithmic transparency reduce lock-in. Theater is modest because regulation aims at structural change, not performative compliance. This is coordination with explicit termination logic.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PLATFORM-AS-UTILITY MYTH (PITON) — The narrative that platforms are 'neutral infrastructure' or 'technology-driven solutions to matching problems' persists despite degraded explanatory power. Data-driven ranking, algorithmic opacity, and unilateral policy changes are performatively presented as objective or natural, while actual governance decisions (favoring first-party products, suppressing competitor visibility, enforcing terms of service) operate covertly. The myth persists through institutional inertia — platforms continue to claim neutrality despite systematic evidence of preferential treatment — but the underlying function (coordination of supply and demand) has atrophied beneath extractive mechanisms. Theater ratio high; actual coordination function occluded.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, the two-sided market constraint exhibits genuine mixed coordination and extraction. Coordination function: matching at scale that reduces transaction costs, enabling market access for both sides. Extraction function: asymmetric power allowing the platform to capture surplus above coordination cost, suppress wages/prices on both sides, and enforce lock-in through technical and policy mechanisms. Both functions are structural, not incidental. The constraint cannot be dissolved without losing coordination benefits, nor can extraction be eliminated without platform intervention. This is the structural definition of Tangled Rope: hybrid.
constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(two_sided_market_power_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(two_sided_market_power_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(two_sided_market_power_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(two_sided_market_power_asymmetry, TR),
    TR >= 0.70.

:- end_tests(two_sided_market_power_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The platform captures significant surplus above coordination cost through: commissions typically 15-30% on producer side, unlimited data harvesting on consumer side, algorithmic preference for first-party products, and policy unilateralism. However, extractiveness is not maximal (0.70+) because coordination function is genuine — without the platform, neither side accesses the other at scale, and some surplus capture is fair compensation for this service. The rising trajectory reflects historical pattern: early platforms (2005-2012) justified high fees through genuine scarcity of matching technology; current platforms (2020+) rely increasingly on algorithmic control and data leverage rather than matching innovation, indicating extraction accumulation above coordination cost. Suppression (0.65): High. Barriers to exit include: network effects reducing alternative platform viability, reputation/rating systems (non-transferable across platforms), algorithmic ranking suppressing visibility of alternatives, payment integration friction, and switching costs borne by users. Suppression is high but not total (0.80+) because exit is possible at cost — alternative platforms exist (Etsy, DuckDuckGo, Mastodon) but require coordination overcoming. Theater ratio (0.48): Moderate. The constraint exhibits mixed performative and functional content. Genuine coordination happens (matching, transaction facilitation). But extraction is increasingly occluded by neutrality myths, algorithmic fairness rhetoric, and data privacy theater. The theater ratio has risen from 0.32 to 0.48 as platforms increasingly use language-based legitimation to cover explicit extraction mechanisms. Theater is higher than early-stage Tangled Rope (0.35) but lower than Piton (0.70+) because actual mechanisms are still visible — the constraint is extractive but not yet purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between dependent producer and platform operator is maximal. The producer sees Snare (trapped, maximal extraction, no alternatives) while the operator sees Rope (coordination mechanism, mutual benefit, fair surplus capture). This gap is not a difference in preferences or ideology but in structural position: the operator has exit optionality that producers lack, enabling the operator to experience the same mechanism as coordination while producers experience it as extraction. The alternative platform operator's Tangled Rope perspective bridges this gap — they see both coordination and extraction because they can act on both: they benefit from incumbent's extraction pricing (creating competitive opportunity) while being constrained by incumbent's network effect dominance (creating exit barriers). The regulatory coalition's Scaffold perspective reveals this gap as institutional and thus modifiable: sunset logic applies because the lock-in is technical (network effects, reputation transfer friction) and regulatory (interoperability mandates can weaken both). The piton perspective (platform-as-utility myth) shows how the operator uses performative neutrality to occlude the perspectival gap from outside observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and exit capacity. Producers with trapped exit and victim status experience d ≈ 0.92 (near full target): they bear maximal extraction. Consumers with trapped exit and victim status experience d ≈ 0.90. Alternative platforms with constrained exit and beneficiary status (benefit from incumbent's pricing) experience d ≈ 0.40 (moderate). The platform operator with arbitrage exit and beneficiary status experiences d ≈ 0.05 (near full beneficiary). The regulatory coalition with constrained exit but power to enforce standards experiences d ≈ 0.35 (moderate target of current extraction, but with capacity to reshape). These d values feed the sigmoid f(d) function: high d agents experience f(d) ≈ 1.40 (amplified extraction), while low d agents experience f(d) ≈ -0.10 (negative experienced extraction / subsidy effect). When combined with scope modifier σ(S)=1.2 for global scope, high-d agents experience χ ≈ 0.58 × 1.40 × 1.2 ≈ 0.98, while low-d agents experience χ ≈ 0.58 × (-0.10) × 1.2 ≈ -0.07.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition. This constraint is NOT uniformly one type — it is genuinely Tangled Rope when viewed from the analytical perspective (coordination + extraction both structural) but Snare from the dependent producer perspective (extraction maximal, coordination invisible) and Rope from the platform operator perspective (coordination visible, extraction invisible). The resolution: the constraint's classification is not the question; the question is which perspective are you measuring from? A regulator who asks 'what type is this constraint?' without specifying the observation position will misclassify it as Rope (if they adopt the operator's framing) or Snare (if they adopt the producer's framing). The correct answer is: it is all six types from different positions, and the presheaf structure reveals that the operator's claim of pure coordination and the producer's claim of pure extraction are both partial truths filtered through structural position. Mandatrophy resolution requires recognizing that the analytical observer's Tangled Rope classification is the least position-relative — it identifies both coordination and extraction as structural, requiring no agent to be a liar. The Scaffold perspective adds sunset logic: regulatory intervention (interoperability, labor classification, data portability) can weaken lock-in mechanisms enough to reduce the extraction component while preserving coordination, enabling the constraint to transition from Tangled Rope toward pure Rope over generational timescale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_commission_threshold,
    'What commission rate or data access level represents fair coordination cost vs extractive rent-seeking?',
    'Comparative analysis: benchmark two-sided platforms against transaction cost accounting models; identify commission levels that correlate with reduced innovation or participant exit',
    'If threshold is high (>35% commission): most platforms classified as Snare from producer side. If threshold is low (>15%): many platforms remain Tangled Rope. Sets the boundary between coordination cost and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_commission_threshold, empirical, 'Fair level of platform commission and data extraction').

omega_variable(
    interoperability_network_recovery,
    'Can interoperability standards and data portability mandates actually reduce lock-in at scale, or do they create new coordination problems?',
    'Post-implementation analysis of EU Digital Markets Act interoperability requirements; measurement of participant migration and lock-in persistence after data portability mandates',
    'If interoperability reduces lock-in substantially: Scaffold classification validated, sunset logic real. If coordination problems persist or increase: regulation is performative (Piton from regulator perspective), and constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_network_recovery, empirical, 'Whether interoperability mandates can reduce platform lock-in').

omega_variable(
    algorithmic_opacity_necessity,
    'Is algorithmic opacity functionally necessary for platform operation or is it maintained for strategic information asymmetry?',
    'Analysis of platforms that voluntarily increase algorithmic transparency; measurement of operational efficiency, participant satisfaction, and platform profitability with and without opacity',
    'If transparency reduces efficiency: opacity is coordination cost. If transparency has minimal operational impact: opacity is pure extraction mechanism. Changes the platform operator perspective from Rope to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether algorithmic opacity is operationally necessary').

omega_variable(
    natural_monopoly_status,
    'Are two-sided platforms inherent natural monopolies due to network effects, or is monopoly dominance a result of extractive strategies and regulatory capture?',
    'Historical analysis of platform market competition; identification of markets where multiple platforms coexist at comparable scale; economic modeling of network effect thresholds vs competitive sustainability',
    'If natural monopoly: platform power is inherent constraint (Mountain perspective). If dominance is strategic: it is institutionally contingent (Tangled Rope). Affects classification of regulatory coalition perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_monopoly_status, conceptual, 'Whether two-sided markets are inherent natural monopolies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(two_sided_market_power_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsm_tr_t0, two_sided_market_power_asymmetry, theater_ratio, 0, 0.32).
narrative_ontology:measurement(tsm_tr_t5, two_sided_market_power_asymmetry, theater_ratio, 5, 0.42).
narrative_ontology:measurement(tsm_tr_t10, two_sided_market_power_asymmetry, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(tsm_be_t0, two_sided_market_power_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tsm_be_t5, two_sided_market_power_asymmetry, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tsm_be_t10, two_sided_market_power_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(two_sided_market_power_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(two_sided_market_power_asymmetry, platform_labor_classification).
narrative_ontology:affects_constraint(two_sided_market_power_asymmetry, algorithmic_opacity_and_ranking).
narrative_ontology:affects_constraint(two_sided_market_power_asymmetry, data_portability_and_lock_in).
narrative_ontology:affects_constraint(two_sided_market_power_asymmetry, network_effects_natural_monopoly).

% DUAL FORMULATION NOTE:
% The two-sided market power asymmetry decomposes into three structurally distinct constraints: (1) platform_labor_classification (whether gig workers are employees or contractors) affects producer-side extraction but not consumer-side dynamics, (2) algorithmic_opacity_and_ranking (whether ranking is transparent or opaque) affects both sides through information asymmetry, (3) data_portability_and_lock_in (whether user data transfers between platforms) affects both sides through switching cost mechanisms. Each has its own ε value and perspectival structure. This story represents the aggregated constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
