% ============================================================================
% CONSTRAINT STORY: platform_lock_in_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_lock_in_extraction, []).

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
 *   constraint_id: platform_lock_in_extraction
 *   human_readable: Platform Lock-In Extraction in Digital Marketplaces
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   Platform lock-in extraction represents a structural phenomenon that
 *   exhibits all six constraint types depending on the observer's position
 *   within the digital economy ecosystem. A single platform architecture
 *   (Uber, Amazon, Shopify, TikTok) simultaneously appears as pure
 *   coordination from the operator's perspective, pure extraction from
 *   trapped merchants, mixed coordination-extraction from mid-market sellers,
 *   regulatory theater from policy authorities, a solvable temporary problem
 *   from reform coalitions, and a natural law from civilizational analysts.
 *   The constraint's extractiveness has increased over the measurement
 *   interval (0.38 → 0.58) as platforms have consolidated market power, while
 *   suppression requirements and theater ratios have both risen, indicating
 *   both harder enforcement and more performative regulation. The mechanism
 *   is threefold: (1) network effects create genuine coordination value
 *   (matched buyers and sellers, reduced search friction, standardized
 *   payments); (2) data aggregation and algorithmic opacity create switching
 *   costs that exceed the value of the coordination services alone; (3)
 *   unilateral control over terms (fee changes, algorithmic ranking, payment
 *   holds) allows continuous extraction without explicit coercion. The
 *   constraint is tangled_rope from the analytical perspective because
 *   genuine coordination (required for the platform to function) is
 *   inseparable from asymmetric extraction (the mechanism by which the
 *   operator captures disproportionate value).
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures value through network effects, data aggregation, algorithmic opacity, and lock-in mechanisms
 *   - Trapped Small Merchant: Primary victim (powerless/trapped) — revenue concentration creates irreversible dependency; reputational capital is platform-specific; exit requires 18-36 months zero-revenue restructuring
 *   - Mid-Market Seller: Secondary victim (moderate/constrained) — has exit options at high cost; experiences real coordination benefit paired with real extraction
 *   - Diversified Multi-Platform Seller: Mobile actor (moderate/mobile) — low lock-in due to platform diversification; experiences constraint as pure coordination
 *   - Price-Constrained Consumer: Identity-locked victim (powerless/identity_locked) — structurally mobile but cognitively fused with platform's interface, recommendation logic, and seller reputation history
 *   - Regulatory Authority: Theater performer (institutional/constrained) — conducts audits and levies fines, but enforcement is symbolic; fines are minor cost of business; compliance reporting is opaque
 *   - Reform Coalition: Organized reformer (organized/constrained) — advocates for data portability, interoperability mandates, and app store gatekeeping restrictions; sees sunset pathway through regulation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (lock-in, opacity) as immutable properties of network effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_lock_in_extraction, 0.58).
domain_priors:suppression_score(platform_lock_in_extraction, 0.68).
domain_priors:theater_ratio(platform_lock_in_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_lock_in_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_lock_in_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_lock_in_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_lock_in_extraction, tangled_rope).
narrative_ontology:human_readable(platform_lock_in_extraction, "Platform Lock-In Extraction in Digital Marketplaces").
narrative_ontology:topic_domain(platform_lock_in_extraction, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(platform_lock_in_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_lock_in_extraction, platform_operator).
narrative_ontology:constraint_victim(platform_lock_in_extraction, trapped_merchants).
narrative_ontology:constraint_victim(platform_lock_in_extraction, price_constrained_consumers).
narrative_ontology:constraint_victim(platform_lock_in_extraction, competitive_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED SMALL MERCHANT (SNARE) — A seller with 40-60% revenue flow through a single platform has no exit. Reputational capital is platform-specific; algorithmic downranking for non-compliance is immediate; customer list is platform property. Restructuring sales infrastructure to exit takes 18-36 months with zero revenue. Suppression is total: coercive terms, opaque algorithm changes, mandatory fee escalation. The trapped merchant experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(platform_lock_in_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET SELLER (TANGLED ROPE) — A seller with 20-35% platform revenue has meaningful but costly exit options: building owned channels, listing on competing platforms, integrating fulfillment. The platform genuinely provides coordination services (customer acquisition, logistics infrastructure, payment processing, dispute resolution). But the coordination is paired with asymmetric extraction: algorithmic opacity, unilateral fee changes, promotional preferencing. Real agency exists but at high cost; real benefit exists but paired with real extraction.
constraint_indexing:constraint_classification(platform_lock_in_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIVERSIFIED MULTI-PLATFORM SELLER (ROPE) — A seller with balanced revenue across 4+ platforms and owned channels (15-20% per platform) has structural mobility. Exit cost from any single platform is material but not existential. The platform relationship becomes pure coordination: market access, customer aggregation, standardized payment processing. No individual platform can extract via lock-in because the seller can reallocate traffic. Experiences the constraint as coordination problem, not extraction.
constraint_indexing:constraint_classification(platform_lock_in_extraction, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination. Network effects are real: seller value increases with buyer count; buyer value increases with seller variety. The platform solves genuine matching problems and reduces transaction friction. From the operator's perspective, scale and lock-in are the platform's core function — they enable coordination at marginal cost. The operator benefits from the lock-in, but the primary mechanism is coordination, not coercion (from their own internal model).
constraint_indexing:constraint_classification(platform_lock_in_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER-AS-HABIT (TANGLED ROPE, IDENTITY_LOCKED) — Price-conscious consumers benefit from platform aggregation (low search cost, diverse sellers, convenient payment). But exit is blocked by internalized habit, algorithmic dependency (personalization becomes the frame through which they discover products), and sunk investment in seller relationships and account history. Structurally mobile — could switch platforms or buy direct — but identity fused with the platform's interface and recommendation logic. The lock-in is cognitive rather than legal, but equally binding at biographical time horizon.
constraint_indexing:constraint_classification(platform_lock_in_extraction, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGULATORY THEATER (PITON) — Competition authorities and consumer protection agencies conduct audits, issue guidance, and occasionally levy fines for algorithmic opacity and unfair terms. But the regulatory machinery is substantially performative: enforcement requires proving intentional harm; fines are calculated as small fractions of revenue (cost of business, not deterrent); compliance reporting is opaque. The regulation persists through institutional pressure but lacks functional teeth. Theater ratio is high because inspection replaces structural change.
constraint_indexing:constraint_classification(platform_lock_in_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY REFORM COALITION (SCAFFOLD) — EU Digital Markets Act, potential US legislation, and interoperability advocates see platform lock-in as a temporary institutional failure with a policy sunset. Data portability, forced interoperability, and app store gatekeeping reforms create alternative pathways. If enacted and enforced (big if), these policies reduce lock-in extraction by structural design — merchants can migrate data, consumers can switch without losing history. This perspective sees the constraint as solvable via sunset mechanisms (regulatory reform with enforcement teeth).
constraint_indexing:constraint_classification(platform_lock_in_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, network effects and data aggregation are immutable properties of digital platforms. Two-sided markets inherently have lock-in dynamics; algorithms necessarily embed opacity; switching costs are structural, not contingent. The platform operator frames lock-in as a natural law: 'This is just how network effects work.' The mountain classification flags this as a false summit candidate — the structural data shows identifiable beneficiaries (platform operator) and victims (trapped merchants), suggesting the 'immutability' is naturalization of a contingent institutional arrangement rather than discovery of a physical law.
constraint_indexing:constraint_classification(platform_lock_in_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_lock_in_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_lock_in_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_lock_in_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_lock_in_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_lock_in_extraction, TR),
    TR >= 0.70.

:- end_tests(platform_lock_in_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The platform operator captures surplus through three mechanisms: (1) genuine coordination value (network effects, transaction friction reduction) — real and non-extractive; (2) data and algorithmic advantages that exceed coordination benefits — partially extractive; (3) unilateral control over terms enabling continuous rent extraction — fully extractive. The 0.58 value reflects that the constraint bundles all three, with the coordination component reducing the pure-extraction value. If extraction were attempted without coordination services, ε would exceed 0.75 (snare territory). Suppression (0.68): High. Multiple enforcement mechanisms exist: algorithmic downranking of non-compliant sellers; account suspension; payment hold threats; terms-of-service unilaterality that makes exit costly and public complaint difficult (forced arbitration, NDA clauses). For trapped merchants, suppression approaches 0.90. For mobile sellers, suppression approaches 0.40. The 0.68 average reflects the weighted distribution of merchant power across the ecosystem. Theater ratio (0.65): High. Regulatory compliance reporting, transparency dashboards, algorithmic fairness commitments, and merchant appeal processes are substantially performative: audits are post-hoc and rule-focused rather than outcome-focused; fines are minor fractions of revenue; algorithm changes continue regardless of merchant complaints; 'appeals' are routed to the same algorithmic ranking system that generated the original decision. The theater has increased over the interval as regulatory pressure has mounted, incentivizing performative compliance signaling without structural change.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across institutional and individual perspectives. The platform operator sees rope (network effects, genuine coordination function). The trapped small merchant sees snare (irreversible extraction, zero agency). The mid-market seller sees tangled_rope (real benefit paired with real extraction). The diversified seller sees rope (coordination without lock-in). The consumer may see either tangled_rope (real benefit from aggregation, constrained by habit) or identity_locked rope (habit plus identity fusion). The regulator sees piton (performative compliance, degraded enforcement). The reform coalition sees scaffold (solvable via sunset reforms). The civilizational analyst risks mountain (naturalizing lock-in as inherent to network effects). These gaps are not measurement errors — they reflect genuine structural differences in how the agents experience the same constraint. The constraint's presence is universal; its classification is observer-dependent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from beneficiary/victim status combined with exit options. Platform operator: beneficiary + arbitrage → d ≈ 0.10 → f(d) ≈ -0.05 → negative effective extraction (experiences subsidy). Trapped merchant: victim + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → high effective extraction. Mid-market seller: victim + constrained → d ≈ 0.70 → f(d) ≈ 1.05 → moderate-high effective extraction. Diversified seller: neither clearly (mobile exit option) → d ≈ 0.50 → f(d) ≈ 0.65 → moderate effective extraction. Consumer identity_locked: victim + identity_locked → d ≈ 0.85 → f(d) ≈ 1.15 → high effective extraction (adjusted for internal binding rather than external barriers). Regulator: observer (institutional) → d ≈ 0.65 → f(d) ≈ 1.00 → moderate effective extraction (sees extraction but filtered through institutional constraints). These derived d values confirm the perspectival gap: trapped merchants and identity_locked consumers experience the highest χ (effective extraction); the operator experiences negative χ (benefit from the constraint); mobile and regulated actors experience moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy through perspectival relativism. From the operator's view, the constraint is pure coordination (Rope) — no extraction is visible because the operator is structurally exempt from the lock-in mechanism. From the trapped merchant's view, the constraint is pure extraction (Snare) — no coordination benefit is legible because the merchant has no alternative. From the mid-market view, the constraint is hybrid (Tangled Rope) — both coordination and extraction are simultaneously true. The mandatrophy is not 'which type is correct?' but 'which structural position are you measuring from?' All six types are perspectivally valid. The constraint structure itself — the bundling of genuine coordination with asymmetric extraction — IS the answer. Unifying the perspectives requires recognizing that platform architecture enables both functions simultaneously, and the question 'can we separate them?' is not a classification question but a policy question (resolvable via regulatory reform — see scaffold perspective).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_necessity,
    'Is algorithmic opacity necessary for platform function or a deliberate rent-extraction mechanism?',
    'Comparative analysis: platforms that transparently publish ranking factors vs. proprietary platforms; user outcomes under transparency vs. opacity; patent evidence of deliberate suppression vs. necessary technical constraints',
    'If necessary: suppression value justified by coordination function; reclassify as lower-suppression tangled_rope. If deliberate: confirms extraction mechanism; suppression value indicates snare-level coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether algorithmic opacity is technically necessary or instrumentally coercive').

omega_variable(
    lock_in_magnitude_threshold,
    'At what revenue concentration does a seller transition from constrained exit to trapped exit?',
    'Longitudinal merchant data: time-to-rebrand after platform delisting or demotion; financial survival rates; secondary market valuations for platform-dependent businesses',
    'If threshold < 25% platform revenue: most sellers are trapped; constraint is snare-dominant. If threshold > 60%: most sellers remain constrained; constraint is tangled_rope-dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_magnitude_threshold, empirical, 'Revenue concentration threshold distinguishing trapped from constrained merchant exit').

omega_variable(
    consumer_exit_cost_structure,
    'Is consumer lock-in primarily identity-locked (cognitive habit) or structurally trapped (data/network dependency)?',
    'Consumer surveys on exit motivation; A/B testing of interface changes that highlight alternatives; measurement of search behavior when competitor platforms are prominently advertised; tracking of habit persistence after forced platform interruption',
    'If identity_locked dominant: constraint appears as tangled_rope for consumers; narratives about ''digital natives'' having natural platform affinity are validated. If trapped dominant: consumers shift to snare classification; reveals structural rather than cognitive binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_exit_cost_structure, conceptual, 'Whether consumer lock-in is identity-fused or structurally trapped').

omega_variable(
    interoperability_feasibility,
    'Can data portability and interoperability protocols eliminate lock-in extraction without collapsing network effects?',
    'Technical feasibility studies; pilot programs (EU data portability, app store sideloading); market outcomes after forced interoperability mandates; measurement of switching costs post-portability',
    'If feasible with preserved effects: scaffold sunset is real; regulatory reform can eliminate constraint. If effects collapse: lock-in is functionally inseparable from network benefits; constraint persists even under reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_feasibility, empirical, 'Whether interoperability mandates can break lock-in while preserving coordination benefits').

omega_variable(
    platform_coordination_necessity,
    'Could the coordination functions (matching, payment, reputation, logistics) be provided through competitive alternatives without lock-in?',
    'Analysis of public utility models, cooperative platforms, API-based marketplaces; historical cases of unbundling (e.g., payment processing moving to Stripe, fulfillment to third parties); comparative outcomes in regulated verticals',
    'If alternatives exist: lock-in is extractive overlay on optional coordination; constraint is snare-ish. If coordination is inseparable from lock-in: extraction is tied to genuinely valuable service; constraint is tangled_rope-ish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_coordination_necessity, empirical, 'Whether platform coordination requires lock-in or is bundled with it contingently').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_lock_in_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plockex_tr_t0, platform_lock_in_extraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(plockex_tr_t5, platform_lock_in_extraction, theater_ratio, 5, 0.55).
narrative_ontology:measurement(plockex_tr_t10, platform_lock_in_extraction, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(plockex_be_t0, platform_lock_in_extraction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(plockex_be_t5, platform_lock_in_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(plockex_be_t10, platform_lock_in_extraction, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(plockex_su_t0, platform_lock_in_extraction, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(plockex_su_t5, platform_lock_in_extraction, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(plockex_su_t10, platform_lock_in_extraction, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_lock_in_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_lock_in_extraction, 0.18).
narrative_ontology:affects_constraint(platform_lock_in_extraction, data_portability_regulation).
narrative_ontology:affects_constraint(platform_lock_in_extraction, algorithmic_fairness_standards).
narrative_ontology:affects_constraint(platform_lock_in_extraction, digital_markets_act_enforcement).
narrative_ontology:affects_constraint(platform_lock_in_extraction, merchant_power_asymmetry).

% DUAL FORMULATION NOTE:
% Platform lock-in is a single structural constraint that decomposes into distinct stories when analyzed by specific mechanism: (1) network effect coordination (ε=0.12, pure rope), (2) algorithmic opacity extraction (ε=0.48, tangled_rope), (3) data lock-in mechanism (ε=0.55, tangled_rope), (4) fee escalation extraction (ε=0.52, tangled_rope), (5) merchant power asymmetry (ε=0.62, snare for trapped merchants). The constraint as described (platform lock-in broadly) bundles all five mechanisms into a single 0.58 ε value. Network decomposition would separate these, but the constraint story models them as a unified phenomenon because they operate interdependently — separating the mechanisms reduces analytical fidelity. The downstream affects_constraints array links to domain-specific regulatory and structural responses: data portability regulation attacks lock-in directly; algorithmic fairness standards constrain opacity; DMA enforcement attempts structural reform; merchant power asymmetry is the field-level outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_lock_in_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
