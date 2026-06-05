% ============================================================================
% CONSTRAINT STORY: indian_ai_licensing_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_ai_licensing_regime, []).

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
 *   constraint_id: indian_ai_licensing_regime
 *   human_readable: India's Sovereign AI Licensing and Data Localization Mandate
 *   domain: technological/political/regulatory
 *
 * SUMMARY:
 *   India's AI licensing and data localization mandate, enacted following the
 *   2026 Delhi AI Expo, represents a strategic attempt to build domestic AI
 *   capacity while maintaining regulatory sovereignty over cross-border data
 *   flows and algorithmic governance. The regime requires foreign AI vendors
 *   to obtain licensing approval, localize training data within India, and
 *   operate through subsidiary structures subject to regulatory oversight.
 *   This constraint exhibits the characteristic tension of national
 *   technology policy: it simultaneously functions as coordination
 *   (technology transfer, infrastructure sovereignty) and extraction
 *   (compliance burden, market barriers). The extractiveness of 0.52 reflects
 *   that the regime imposes real costs on non-compliant vendors (model
 *   retraining, subsidiary establishment, licensing uncertainty) while
 *   providing genuine benefits to domestic startups (protected market access,
 *   state compute infrastructure, technology transfer). The suppression of
 *   0.65 indicates significant barriers to exit (abandoning 1.4B-person
 *   market is costly) and to alternatives (open-source models are improving
 *   but not yet fully substitutive). The theater ratio of 0.58 indicates that
 *   compliance demonstrations are partially performative — data residency
 *   rules are routinely circumvented through subsidiary structures and
 *   contractual obfuscation — but enforcement is inconsistent rather than
 *   purely theatrical.
 *
 * KEY AGENTS:
 *   - Indian Tech Ministry / Government: Primary beneficiary (institutional/arbitrage) — achieves technology transfer, compute sovereignty, regulatory leverage; sees constraint as solution to coordination problem
 *   - Global Model Developers (Non-Compliant Path): Primary victim (powerless/trapped) — cannot exit Indian market; must comply with opaque licensing criteria; bears full cost of model retraining and subsidiary establishment
 *   - Multinational AI Vendors (Subsidiary Strategy): Secondary victim (organized/constrained) — have resources to establish subsidiaries but face compliance costs, data transfer restrictions, and regulatory uncertainty
 *   - Indian AI Startups: Secondary beneficiary with constraints (moderate/constrained) — benefit from protected domestic market and state infrastructure but constrained by licensing bureaucracy and training data access limitations
 *   - Global Open-Source AI Community: Organized agents (organized/mobile) — building alternative pathways through open-weight models and decentralized training; see licensing regime as temporary scaffold with 5-10 year sunset
 *   - Data Residency Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative compliance demonstrations; sees own enforcement as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing national technology sovereignty as immutable law rather than contingent political arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_ai_licensing_regime, 0.52).
domain_priors:suppression_score(indian_ai_licensing_regime, 0.65).
domain_priors:theater_ratio(indian_ai_licensing_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_ai_licensing_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_ai_licensing_regime, tangled_rope).
narrative_ontology:human_readable(indian_ai_licensing_regime, "India's Sovereign AI Licensing and Data Localization Mandate").
narrative_ontology:topic_domain(indian_ai_licensing_regime, "technological/political/regulatory").

domain_priors:requires_active_enforcement(indian_ai_licensing_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, indian_ai_startups).
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, state_compute_infrastructure).
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, domestic_tech_champions).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, multinational_ai_vendors).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, cross_border_data_flows).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, global_model_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL MODEL DEVELOPERS FORCED COMPLIANCE (SNARE) — Cannot exit Indian market without abandoning 1.4B user base and regulatory legitimacy. Must localize data, retrain models on Indian compute, obtain licensing approval from bureaucratic apparatus with opaque criteria. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72. Extraction is severe: compliance costs imposed without reciprocal standards access.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MULTINATIONAL VENDORS WITH ORGANIZATIONAL CAPACITY (TANGLED ROPE) — Have resources to establish Indian subsidiaries, negotiate licensing terms, and invest in local infrastructure. Experience constraint as mixed: extraction via compliance costs and data sovereignty requirements, but also coordination benefit through market access and regulatory clarity (once obtained). d≈0.60, f(d)≈0.75, σ=1.0 → χ≈0.39. Constrained exit but not trapped — can establish in-country operations.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIAN TECH MINISTRY / GOVERNMENT (ROPE) — Primary beneficiary. Achieves coordination objectives: technology transfer to domestic vendors, compute infrastructure sovereignty, regulatory leverage over global platforms. Licensing regime enables data residency compliance, export control monitoring, and capability-building for Indian AI startups. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; sees constraint as solution to coordination problem.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDIAN AI STARTUPS (TANGLED ROPE) — Benefit from protected domestic market, preferential licensing terms, and access to state compute infrastructure. But also constrained by licensing bureaucracy, data residency requirements that increase operational costs, and limited access to pre-trained models from non-compliant vendors. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Mixed: protective barrier is coordination benefit, but enforcement costs and training data bottlenecks impose extraction.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL STANDARDIZATION BODIES / OPEN-STACK MOVEMENT (SCAFFOLD) — See the licensing regime as temporary. Open-source AI models (Llama, Mistral, others) and decentralized training infrastructure reduce dependence on proprietary models requiring licensing compliance. Export of open-weight models bypasses data localization requirements. d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.28. This perspective sees a sunset: within 5-10 years, sufficient open-source alternatives will reduce the extraction force of the licensing mandate.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: DATA RESIDENCY ENFORCEMENT APPARATUS (PITON) — Licensing and data localization rules are substantially theatrical. Actual enforcement is inconsistent; data routes through VPNs, subsidiary structures, and contractual obfuscation routinely circumvent rules. Theater ratio (0.58) reflects that compliance demonstrations are often performative rather than functional — servers technically 'in India' may be management-controlled from Singapore; 'encrypted local storage' masks cloud federation. d≈0.12, f(d)≈0.02, σ=1.0 → χ≈0.01. Low functional extraction; constraint persists through inertia and regulatory theater, not through effective enforcement.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT RISK (MOUNTAIN) — From a global analytical perspective, data sovereignty and compute independence might appear as immutable constraints inherent to the logic of nation-states and information control. However, the structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts this framing. The licensing regime is a contingent political-regulatory arrangement, not a natural law. Open-source alternatives, regulatory arbitrage through subsidiaries, and decentralizing technologies are erosive. This perspective risks naturalizing extraction as inevitable.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_ai_licensing_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_ai_licensing_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_ai_licensing_regime, TR),
    TR >= 0.70.

:- end_tests(indian_ai_licensing_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The regime imposes real costs on foreign vendors — model retraining to comply with localization, licensing approval delays, subsidiary establishment, and ongoing compliance overhead. However, extractiveness is not as severe as pure monopolistic extraction (0.70+) because the regime does provide coordination benefits: vendors gain market access to 1.4B users, regulatory clarity (once licensing is obtained), and predictable operating terms. The extraction is justified by India as technology transfer and sovereignty-building, which provides partial legitimacy. Suppression (0.65): Moderate-high. Multiple barriers to exit and alternatives: abandoning Indian market is costly (market size, growth trajectory, regulatory importance); open-source alternatives exist but are not yet fully substitutive for cutting-edge proprietary models; VPN-based circumvention faces legal and operational risks. However, suppression is not total (0.80+) because subsidiaries provide a partial exit path, and open-source models are improving. Theater ratio (0.58): Moderate. Compliance demonstrations are partially performative — data residency rules are circumvented through subsidiary structures where management and compute are externally located; 'licensed' vendors sometimes operate through complex contractual arrangements that obscure non-compliance. However, enforcement is inconsistent rather than purely theatrical; some vendors have been denied licenses or faced audit pressure. The theater ratio has risen from 0.35 at regime inception (when enforcement was stricter) to 0.58 (as vendors have learned to circumvent and enforcement capacity has been tested).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a significant perspectival gap between the vendor and beneficiary perspectives. The Indian Tech Ministry sees Rope: a coordination mechanism for achieving technology transfer and sovereignty. Global model developers see Snare: extraction without exit. Multinational vendors with subsidiary capacity see Tangled Rope: mixed coordination and extraction. Indian startups see Tangled Rope: protective benefits and compliance constraints. Open-source advocates see Scaffold: a temporary regulatory problem being solved by alternative technology. Enforcement apparatus see Piton: degraded, theater-heavy regulation persisting through inertia. The analytical observer risks seeing Mountain: naturalizing national AI sovereignty as an immutable constraint. The perspectival gap reveals that the constraint's character depends entirely on the agent's structural position relative to licensing regime—their exit options, their market access, and their technological alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Indian Tech Ministry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Global model developers (non-compliant): Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Multinational vendors (subsidiary strategy): Victim + constrained → d≈0.60, f(d)≈0.75. Significant but not maximal extraction; constrained exit slightly reduces pressure. Indian startups: Beneficiary + constrained → d≈0.35, f(d)≈0.35. Low to moderate extraction; they benefit from protection but face constraints on training data access. Open-source community: Organized + mobile → d≈0.45, f(d)≈0.48. Mobile exit reduces effective extraction. Enforcement apparatus: Institutional + arbitrage → d≈0.12, f(d)≈0.02. Low directionality; they are not extracted from but see their own process as degraded (Piton classification comes from theater gate). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective risks naturalizing contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the Indian AI licensing regime is NOT FULLY RESOLVED (mandatrophy_resolved: false). The tension is real: the regime genuinely coordinates technology transfer and sovereignty (rope function) while genuinely extracting from non-compliant vendors (snare function). Both are structurally present. The extraction cannot be dismissed as epiphenomenal coordination overhead; vendors with subsidiary capacity face real costs. The coordination cannot be dismissed as cover for pure extraction; domestic startups genuinely benefit from market protection. The resolution pathway depends on two empirical unknowns: (1) whether domestic vendor capability reaches threshold to justify protective barriers (would stabilize Tangled Rope as permanent arrangement), and (2) whether open-source alternatives reach substitutivity (would degrade extraction, moving constraint toward Piton as open-source sunset logic matures). Currently, the constraint is authentically hybrid: simultaneous coordination function and asymmetric extraction. The classification as Tangled Rope is structurally correct, not a compromise between two competing types. Mandatrophy will be RESOLVED if either empirical pathway resolves: threshold achievement → permanent Tangled Rope stability; substitution → temporary Scaffold with clear sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_enforcement_capacity,
    'Can India''s regulatory apparatus effectively enforce data localization and licensing terms against determined multinational vendors using subsidiary structures and regulatory arbitrage?',
    'Audit of actual enforcement: compliance violations detected vs. violations circumvented; pattern analysis of data routing through subsidiaries; effectiveness of license denial in blocking market access',
    'If effective enforcement: suppression and extractiveness scores justified; Snare classification for non-compliant vendors stable. If enforcement weak: theater_ratio rises above 0.70, constraint downgrades to Piton; effective extraction is lower than declared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_enforcement_capacity, empirical, 'Effective enforcement capacity of data localization and licensing regimes').

omega_variable(
    domestic_vendor_capability_threshold,
    'Will Indian AI startups achieve sufficient capability within the licensing regime''s timeline to justify the protective barriers, or will the market become dependent on foreign vendors through subsidiary licensing?',
    'Longitudinal tracking of Indian AI startup performance: model quality benchmarks, compute infrastructure utilization, patent generation, export capacity; comparison with pre-licensing trends',
    'If domestic capability threshold met: licensing regime transitions to permanent coordination mechanism (Tangled Rope stability). If not met: regime becomes pure extraction apparatus masquerading as development policy (Snare classification throughout).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_vendor_capability_threshold, empirical, 'Whether Indian AI startups achieve justifying capability within licensing regime').

omega_variable(
    open_source_substitution_velocity,
    'How quickly will open-source AI models and decentralized training infrastructure make proprietary licensing regimes obsolete?',
    'Model adoption tracking: percentage of new Indian AI systems built on open-source vs. proprietary; effectiveness of open-weight models on downstream tasks; availability of compute infrastructure for local training',
    'If substitution rapid (3-5 years): licensing regime''s extraction mechanism degrades faster than anticipated; Scaffold sunset confirmed. If slow (10+ years): Snare and Tangled Rope classifications remain structurally relevant; extraction window extends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_substitution_velocity, empirical, 'Velocity of open-source substitution for proprietary AI licensing').

omega_variable(
    reciprocal_constraint_asymmetry,
    'Does India face equivalent constraints on its own AI vendors exporting to Europe, USA, or other jurisdictions, or is the licensing regime asymmetrically extractive by global standards?',
    'Comparative regulatory analysis: licensing requirements imposed on Indian vendors by other major markets; analysis of symmetry/asymmetry in data localization demands; trade negotiation outcomes',
    'If asymmetric in India''s favor: regime is coordination for India but extraction for foreigners — Tangled Rope stability. If globally reciprocal: all jurisdictions impose similar regimes — Rope classification across all perspectives (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocal_constraint_asymmetry, empirical, 'Whether licensing regime is asymmetric or reciprocal across global jurisdictions').

omega_variable(
    underground_market_emergence,
    'Will a shadow ecosystem of non-compliant, unlicensed AI services emerge in India, reducing the effective suppression and extraction of the licensing regime?',
    'Market surveillance: detection of unlicensed model deployment; analysis of criminal enforcement costs vs. detection capacity; tracking of VPN-based access to blocked models',
    'If underground economy significant: effective suppression drops below 0.60; regime transitions from Snare toward Piton (theatrical enforcement). If suppression maintained: regime retains extraction capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(underground_market_emergence, empirical, 'Emergence of unlicensed AI services bypassing licensing regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_ai_licensing_regime, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indai_tr_t0, indian_ai_licensing_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indai_tr_t2, indian_ai_licensing_regime, theater_ratio, 2, 0.5).
narrative_ontology:measurement(indai_tr_t4, indian_ai_licensing_regime, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(indai_be_t0, indian_ai_licensing_regime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(indai_be_t2, indian_ai_licensing_regime, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(indai_be_t4, indian_ai_licensing_regime, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_ai_licensing_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, global_ai_model_availability).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, semiconductor_supply_chain_sovereignty).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, cross_border_data_transfer_governance).

% DUAL FORMULATION NOTE:
% The Indian AI licensing regime downstream of broader technology sovereignty and data localization movements globally. Upstream constraints include semiconductor supply chain control and export governance frameworks; downstream constraints include specific model deployment barriers and cross-border compute arbitrage. The licensing regime has ε=0.52 reflecting its hybrid coordination-extraction character. Related but distinct constraints in the network have different ε values reflecting their structural character: semiconductor sovereignty (ε≈0.48, Tangled Rope), global model availability (ε≈0.35, Rope), and data transfer governance (ε≈0.55, Snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_ai_licensing_regime, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
