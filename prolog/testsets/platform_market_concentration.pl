% ============================================================================
% CONSTRAINT STORY: platform_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_market_concentration, []).

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
 *   constraint_id: platform_market_concentration
 *   human_readable: Platform Market Concentration and Digital Gatekeeping
 *   domain: economic/technology/regulatory
 *
 * SUMMARY:
 *   Platform market concentration represents a structural constraint where
 *   dominant digital intermediaries (Amazon, Apple, Google, Meta, Alibaba,
 *   TikTok) have captured disproportionate value from digital commerce and
 *   services by leveraging network effects and switching costs. Small
 *   sellers, app developers, competing platforms, and merchants are locked
 *   into the ecosystem by the absence of viable alternatives. The constraint
 *   exhibits high extractiveness (0.68) driven by platform fee structures,
 *   algorithmic suppression, mandatory use of platform payment systems,
 *   forced bundling, and data asymmetries. Suppression is structural (network
 *   lock-in, switching costs, lack of alternatives) rather than purely
 *   regulatory. The constraint shows distinct classification types across
 *   different observer positions: sellers and developers see a Snare
 *   (trapped, high extraction); emerging competitors see a Snare (constrained
 *   by scale asymmetries); regulatory coalitions see a Scaffold with sunset
 *   clause (interoperability mandates designed to degrade extraction); the
 *   platform operator sees Rope (coordination mechanism); large merchants see
 *   Tangled Rope (mixed coordination and extraction with exit options); and
 *   the civilizational view risks a Piton (naturalizing contingent dominance
 *   as infrastructure inevitability). Theater ratio (0.55) reflects that
 *   platform activities are genuinely coordinative (payment processing,
 *   curation, dispute resolution) while others are pure gatekeeping
 *   performance (algorithmic opacity, exclusive dealing, acquisition of
 *   competitors).
 *
 * KEY AGENTS:
 *   - Dominant Platform Operators: Primary beneficiary (institutional/arbitrage) — capture disproportionate value through network effects and switching costs; experience constraint as coordination mechanism
 *   - Small Sellers and Merchants: Primary victim (powerless/trapped) — locked into platform dependency by lack of alternatives; face arbitrary fee changes, algorithmic suppression, and forced terms. Also constrained/moderate large merchants with some exit options
 *   - App Developers: Secondary victim (powerless/trapped) — dependent on app store distribution; face mandatory terms, revenue extraction through forced payment systems, and visibility control
 *   - Emerging Competitor Platforms: Secondary victim (moderate/constrained) — face network effects, scale disadvantages, and predatory practices; exit theoretically possible but prohibitively expensive
 *   - Regulatory Coalition: Organized actor (organized/mobile) — antitrust authorities, consumer protection agencies, interoperability advocates building exit pathways through enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — can identify the structural extraction mechanisms underlying platform dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_market_concentration, 0.68).
domain_priors:suppression_score(platform_market_concentration, 0.65).
domain_priors:theater_ratio(platform_market_concentration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_market_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(platform_market_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_market_concentration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_market_concentration, snare).
narrative_ontology:human_readable(platform_market_concentration, "Platform Market Concentration and Digital Gatekeeping").
narrative_ontology:topic_domain(platform_market_concentration, "economic/technology/regulatory").

domain_priors:requires_active_enforcement(platform_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_market_concentration, dominant_platform_operators).
narrative_ontology:constraint_victim(platform_market_concentration, small_business_sellers).
narrative_ontology:constraint_victim(platform_market_concentration, competitor_platforms).
narrative_ontology:constraint_victim(platform_market_concentration, consumer_choice).
narrative_ontology:constraint_victim(platform_market_concentration, app_developer_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL SELLER (SNARE) — Sellers dependent on platform reach have no viable exit. Alternative marketplaces lack network effects; building direct customer channels requires capital and time they cannot afford. Platform can unilaterally change terms, algorithms, fees, or remove seller with minimal recourse. Suppression is structural: network lock-in, switching costs, and lack of alternative distribution channels. Maximum experienced extraction.
constraint_indexing:constraint_classification(platform_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: APP DEVELOPER (SNARE) — Developer access to platform app store is effectively a monopoly distribution channel. Platform can unilaterally change terms, reject apps, suppress visibility, or extract revenue through forced use of platform payment systems. Developer trapped by: network effects (users expect app on dominant platform), switching costs (rebuilding user base on alternative platform is prohibitive), and lack of alternatives. High suppression, high extraction.
constraint_indexing:constraint_classification(platform_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EMERGING COMPETITOR PLATFORM (SNARE) — New entrant platforms face suppression through: network effects (lock-in to incumbent), scale asymmetries (incumbent's data and algorithmic advantages), and predatory practices (underpricing, exclusive dealing, acquisition of potential rivals). Exit is theoretically possible but prohibitively expensive. Constrained rather than trapped because regulatory intervention or investor capital could change this, but the structural costs are so high that exit is effectively unavailable to most competitors.
constraint_indexing:constraint_classification(platform_market_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Antitrust authorities, consumer protection agencies, and interoperability advocates see market concentration as a temporary regulatory problem with a sunset clause. Digital Markets Acts (EU), executive orders on interoperability, and antitrust enforcement are intentionally designed to degrade the extraction mechanism: forced interoperability, API access mandates, and data portability rules lower switching costs and reduce network lock-in. Sunset logic: as these regulations mature and are enforced, the dominant platform's structural advantages should weaken. Theater ratio is moderate because enforcement theater exists alongside real structural changes. Exit path is clear for organized actors — the constraint is intentionally being dismantled.
constraint_indexing:constraint_classification(platform_market_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DOMINANT PLATFORM OPERATOR (ROPE) — Platform experiences market concentration as coordination mechanism: network effects that connect buyers and sellers, standards that enable interoperability within the platform, and curation that manages quality and trust. Platform benefits from scale and switching costs. Extraction is experienced as legitimate coordination benefit — the platform provides real value (distribution, payment processing, fraud prevention, discovery). From this perspective, suppression of competitors is not extraction but natural market outcome of superior efficiency. Arbitrage exit (could become a seller/developer on their own platform, but chooses not to) makes effective extraction low or negative.
constraint_indexing:constraint_classification(platform_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE MERCHANT (TANGLED ROPE) — Large sellers (major brands with significant market share) have mobile exit options — they can build direct-to-consumer channels, use multiple platforms simultaneously, or leverage brand power to negotiate favorable terms. However, they still benefit from and depend on platform access for market reach. They experience the constraint as mixed coordination (platform enables reach, payment processing, logistics) and extraction (high fees, algorithmic suppression, data access asymmetries, forced bundling). Mobile exit options reduce experienced extraction relative to trapped sellers, creating a perspectival gap between large and small merchants.
constraint_indexing:constraint_classification(platform_market_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: E-COMMERCE INFRASTRUCTURE (PITON) — From a civilizational view, market concentration in digital platforms mirrors historical concentration in railroads, telecommunications, and utilities — network effects create natural monopoly structures that are difficult to dislodge. From this perspective, the current platform dominance is inertial: the structure persists because alternatives haven't fully replaced it (no viable competing infrastructure with equal network effects), not because extraction is currently maximized. Theater ratio (0.55) reflects that some platform activities are genuinely coordinative while others are pure gatekeeping theater. This perspective risks naturalizing contingent institutional arrangements.
constraint_indexing:constraint_classification(platform_market_concentration, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From the analytical/civilizational perspective, market concentration exhibits all the structural hallmarks of a snare: high base extractiveness (0.68 — platforms extract through fees, data, algorithmic visibility control, forced bundling), high suppression (0.65 — network lock-in and switching costs prevent exit for most users and sellers), high effective extraction chi (platforms capture disproportionate value relative to their coordination function), and minimal theater (the extraction mechanisms are transparent once examined). The analytical observer sees the constraint as structural extraction made possible by network effects, not as natural monopoly or inevitable economic outcome.
constraint_indexing:constraint_classification(platform_market_concentration, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_market_concentration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_market_concentration, TR),
    TR >= 0.70.

:- end_tests(platform_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Platforms extract through multiple overlapping mechanisms: commissions (15-45% on marketplace transactions), forced use of payment systems (2-3.5% + fees), data monetization, algorithmic visibility control, and exclusive dealing requirements. The value extracted is disproportionate to coordination costs. Measurement trajectory shows growth from 0.48 to 0.68 over 15 years as platforms expanded commission structures and bundled services. Suppression (0.65): High. Structural barriers prevent exit: network effects make competing platforms unviable (users expect to find all merchants on dominant platform), switching costs are high (merchants must rebuild customer acquisition), alternative distribution channels require capital and time most sellers lack, and platform can unilaterally change terms with minimal recourse. Regulatory action (data portability, interoperability mandates) is beginning to reduce suppression, but effect lags. Theater ratio (0.55): Moderate. Platforms perform genuine coordination functions (payment processing, merchant curation, dispute resolution, fraud prevention, logistics integration) but also maintain theater around algorithmic decisions, recommendation systems, and quality control that obscures extraction mechanisms. Theater has increased slightly (0.42 to 0.55) as platforms rely more on algorithmic opacity to justify visibility decisions.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates beneficiaries (platform operators perceiving Rope) from victims (small sellers perceiving Snare). The gap is driven by directionality: beneficiaries have arbitrage exit and derive value from network effects; victims have trapped exit and face suppression. The scaffold perspective (regulatory coalition) adds a temporal dimension — the constraint is temporary and being intentionally dismantled through enforcement. The piton perspective adds a risk: naturalizing market dominance as infrastructure necessity when it is actually extractive institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit capacity. Platform operators as beneficiaries with arbitrage exit have low d (≈0.10), producing negative χ — extraction runs toward them. Small sellers as victims with trapped exit have high d (≈0.95), producing maximum f(d) → high χ experienced by them. Large merchants as victims with mobile exit have moderate d (≈0.55), producing moderate χ relative to small sellers. Emerging competitors as victims with mobile but prohibitively expensive exit have d (≈0.75), producing high χ despite nominal exit option. Regulatory agents as organized/mobile have d (≈0.50), producing moderate χ reflecting their agency in reshaping the constraint. The directionality derives from beneficiary/victim declarations and exit capacity — no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by examining whether platform dominance is a natural monopoly coordination mechanism or extractive gatekeeping. If coordination: should classify as Rope (pure coordination with minimal extraction). If extraction: should classify as Snare (high extraction with coordination as cover story). The structural data supports the Snare classification: (1) High extractiveness independent of coordination value — platforms extract fees far exceeding measured coordination costs. (2) High suppression creating trapped condition for merchants and developers — network effects and switching costs prevent exit. (3) Multiple victim classes (sellers, developers, competitors, consumer choice) with no exit options — indicates asymmetric extraction, not coordination. (4) Theater (algorithmic opacity, quality control framing) obscures extraction mechanisms. (5) Regulatory response (DMA, Executive Orders, antitrust enforcement) specifically targets extraction, not coordination failure. The constraint is accurately classified as Snare. The piton risk is that dominance naturalizes as infrastructure — this is a false summit that the analytical observer should reject.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_vs_switching_costs,
    'Is the dominant platform''s durability driven by genuine network effects (platforms become more valuable as more users join) or by sunk switching costs (users stay because switching is expensive, not because value increases)?',
    'Historical analysis of user retention across regulatory interventions (interoperability mandates, data portability rules, API access); measurement of user migration when switching costs decline. If users migrate readily when switching costs drop, network effects are weaker than claimed.',
    'If network effects dominate: market concentration is stable absent major technological disruption (Piton classification more accurate). If switching costs dominate: reduced switching costs should enable competition and reduce extraction (Scaffold sunset logic is valid). Classification could shift toward Tangled Rope or Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_vs_switching_costs, empirical, 'Decomposition of network effects from switching costs in platform persistence').

omega_variable(
    extraction_vs_coordination_boundary,
    'What portion of platform value capture represents coordination cost (legitimate payment for infrastructure, curation, dispute resolution) versus extractive overhead (rents from gatekeeper position)?',
    'Comparison of platform fee structures against measured coordination costs in competing models (direct-to-consumer, wholesale, cooperative platforms). Analysis of fee changes when regulatory pressure increases (should decrease if extractive, persist if coordination cost).',
    'If coordination costs are high (>40% of fees): platform is closer to Rope, extraction lower. If coordination costs are low (<20% of fees): platform is Snare, extraction higher. This shifts χ calculation and potentially changes classification from Snare toward Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Boundary between legitimate coordination costs and extractive gatekeeping').

omega_variable(
    regulatory_enforcement_capacity,
    'Can antitrust and interoperability regulations actually reduce market concentration, or does platform dominance persist despite enforcement efforts?',
    'Post-enforcement measurement: DMA interoperability mandates (EU), executive order implementation (US), and DSA enforcement outcomes. Track: new platform entrant market share, merchant exit rates, app developer switching, consumer switching. If suppression and extractiveness decline measurably within 3-5 years, enforcement is working. If metrics stagnate, enforcement theater exceeds real impact.',
    'If enforcement succeeds: Scaffold perspective is validated, sunset clause is real, constraint will degrade to Rope or weaken substantially. If enforcement fails: constraint may shift toward Piton (theater dominates function) or remain Snare (extraction persists despite regulatory performance). Could invalidate mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Effectiveness of regulatory intervention in reducing platform market concentration').

omega_variable(
    trapped_vs_constrained_boundary,
    'For small sellers and developers, is exit truly impossible (trapped) or merely prohibitively expensive (constrained)?',
    'Economic analysis: cost and time required to build direct customer channels, use competing platforms, or form cooperative alternatives. Survey of merchant perception: do sellers see viable alternatives or perceive exit as impossible? Historical case studies: have any market segments or regions escaped dominant platform dependency?',
    'If trapped: powerless agent perspectives and Snare classification are correct. If constrained: effective extraction χ should be lower; classification might shift toward Tangled Rope in some contexts (if sellers perceive some agency). Could affect directionality derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_vs_constrained_boundary, empirical, 'Distinction between structural impossibility of exit versus high cost of exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_market_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_tr_t0, platform_market_concentration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(plat_tr_t5, platform_market_concentration, theater_ratio, 5, 0.48).
narrative_ontology:measurement(plat_tr_t10, platform_market_concentration, theater_ratio, 10, 0.55).
narrative_ontology:measurement(plat_tr_t15, platform_market_concentration, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(plat_be_t0, platform_market_concentration, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(plat_be_t5, platform_market_concentration, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(plat_be_t10, platform_market_concentration, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(plat_be_t15, platform_market_concentration, base_extractiveness, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_market_concentration, resource_allocation).
narrative_ontology:affects_constraint(platform_market_concentration, small_business_access_digital_markets).
narrative_ontology:affects_constraint(platform_market_concentration, algorithmic_transparency_in_content_distribution).
narrative_ontology:affects_constraint(platform_market_concentration, data_asymmetry_in_consumer_platforms).
narrative_ontology:affects_constraint(platform_market_concentration, payment_system_interoperability).

% DUAL FORMULATION NOTE:
% Platform market concentration is upstream of specific extraction mechanisms (algorithmic suppression, forced bundling, data access asymmetries, payment system lock-in). Each downstream constraint has its own ε reflecting domain-specific extractiveness. The parent constraint models the structural concentration enabling all downstream mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
