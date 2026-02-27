% ============================================================================
% CONSTRAINT STORY: commercial_data_brokerage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commercial_data_brokerage, []).

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
 *   constraint_id: commercial_data_brokerage
 *   human_readable: The Commercial Data Brokerage Ecosystem
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The commercial data brokerage ecosystem represents one of the largest
 *   systematic extractions of value from unwitting individuals in the modern
 *   economy. Data brokers aggregate information from hundreds of sources —
 *   public records, purchase histories, browsing behavior, location tracking,
 *   financial data, health information — and sell profiles to advertisers,
 *   financial firms, insurance companies, employers, and law enforcement. The
 *   extraction mechanism is structural: individuals cannot meaningfully
 *   consent because they lack information about what is collected, how it is
 *   aggregated, or who accesses it. The suppression is extreme because
 *   collection occurs without their knowledge, opt-out mechanisms are
 *   deliberately obscured, and enforcement against brokers has been minimal.
 *   Over the past 20 years, the ecosystem has evolved from fragmented
 *   regional brokers to global data giants (Experian, Equifax, TransUnion,
 *   Acxiom, Oracle Data Cloud) with near-universal coverage and automated
 *   real-time data flows. Theater has increased as privacy policies and
 *   regulatory compliance theater have proliferated without meaningfully
 *   constraining extraction.
 *
 * KEY AGENTS:
 *   - Data Subjects (individuals): Primary victims (powerless/trapped) — cannot exit data collection; extraction occurs without knowledge or meaningful consent
 *   - Data Brokers (corporate): Primary beneficiaries (institutional/arbitrage) — monetize aggregated profiles; have full agency to shift business models if pressured
 *   - Advertising Platforms (Google, Meta, etc.): Secondary beneficiaries (institutional/arbitrage) — consume brokered data to improve targeting; benefit from suppressed competition
 *   - Financial Services Firms (lenders, insurers, employers): Secondary beneficiaries (institutional/arbitrage) — use brokered data for risk assessment and targeting
 *   - Regulated Consumers (GDPR/CCPA jurisdictions): Secondary victims (moderate/mobile) — have formal exit options (opt-out, data access) but friction remains high
 *   - Privacy Regulation Coalition (regulators, advocates, privacy engineers): Organized agents (organized/constrained) — building alternative architectures (privacy-by-design, federated learning, data cooperatives) with sunset logic
 *   - Analytical Observer: Civilizational context (analytical/analytical) — recognizes brokerage as contingent institutional choice, not natural law of information systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commercial_data_brokerage, 0.58).
domain_priors:suppression_score(commercial_data_brokerage, 0.72).
domain_priors:theater_ratio(commercial_data_brokerage, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commercial_data_brokerage, extractiveness, 0.58).
narrative_ontology:constraint_metric(commercial_data_brokerage, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(commercial_data_brokerage, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commercial_data_brokerage, snare).
narrative_ontology:human_readable(commercial_data_brokerage, "The Commercial Data Brokerage Ecosystem").
narrative_ontology:topic_domain(commercial_data_brokerage, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commercial_data_brokerage, data_brokers).
narrative_ontology:constraint_beneficiary(commercial_data_brokerage, advertising_platforms).
narrative_ontology:constraint_beneficiary(commercial_data_brokerage, financial_services_firms).
narrative_ontology:constraint_victim(commercial_data_brokerage, data_subjects).
narrative_ontology:constraint_victim(commercial_data_brokerage, privacy_expectations).
narrative_ontology:constraint_victim(commercial_data_brokerage, individual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individuals cannot realistically exit data collection. Even with privacy settings and opt-out requests, data is already aggregated and sold. No meaningful alternative paths exist; suppression is near-total because collection occurs without knowledge and consent is not genuinely voluntary. Maximum extraction experienced.
constraint_indexing:constraint_classification(commercial_data_brokerage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DATA BROKER (ROPE) — Experiences the ecosystem as coordination: aggregating fragmented data sources into marketable profiles solves the buyer's information problem. Benefits substantially from the constraint through recurring revenue. Suppression is low for this agent; they have full agency and can exit if regulatory pressure mounts. For them, this is not extraction but coordination benefit.
constraint_indexing:constraint_classification(commercial_data_brokerage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATED CONSUMER (TANGLED ROPE) — In jurisdictions with privacy law (GDPR, CCPA), consumers have formal exit options: opt-out requests, data access rights. These are mobile rather than trapped, but friction remains high. Consumers benefit from some data-driven services (targeted ads, credit scoring) while bearing extraction costs through privacy loss. Moderate extraction and moderate coordination.
constraint_indexing:constraint_classification(commercial_data_brokerage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVACY REGULATION COALITION (SCAFFOLD) — Organized agents (regulators, advocacy groups, privacy engineers) see the brokerage ecosystem as a temporary coordination failure with structural sunset. Privacy-by-design mandates, federated learning, differential privacy, and decentralized data models represent alternative pathways that bypass centralized brokerage. Sunset clause: as technical architectures mature and enforcement strengthens, the data brokerage model loses extractive force. Theater remains moderate because regulations produce performative compliance while brokers shift to shadow brokerage.
constraint_indexing:constraint_classification(commercial_data_brokerage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY PRIVACY THEATER (PITON) — Privacy policies, opt-out mechanisms, and 'transparent' data practices are substantially performative. Most users cannot or do not read privacy policies; opt-out requests disappear into administrative black holes; transparency statements obscure rather than clarify. The theater persists through institutional inertia — companies maintain privacy rituals while collecting aggressively. Theater ratio 0.68 reflects that much of the compliance infrastructure is ritual without functional constraint.
constraint_indexing:constraint_classification(commercial_data_brokerage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/universal view, the structural reality is that data brokerage systematically extracts information asymmetry from powerless agents. No amount of 'transparency' changes this: knowing you are surveilled does not restore autonomy. The constraint is snare, not mountain — it is not an immutable law of information systems but a contingent institutional choice to centralize and commercialize personal data. Alternative architectures (federated, decentralized, data cooperatives) demonstrate that this is not natural or necessary.
constraint_indexing:constraint_classification(commercial_data_brokerage, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commercial_data_brokerage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commercial_data_brokerage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commercial_data_brokerage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commercial_data_brokerage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commercial_data_brokerage, TR),
    TR >= 0.70.

:- end_tests(commercial_data_brokerage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Data brokers extract substantial value from data subjects: profiles are sold repeatedly, generating recurring revenue. The extraction is sustained because individuals cannot exit or negotiate. Extractiveness has increased from 0.32 to 0.58 over the interval as data brokers have expanded coverage (from 100M to 500M+ profiles), integrated new data sources (location, behavioral, health), and automated aggregation. However, extractiveness is not maximal (0.70+) because regulatory friction (GDPR, CCPA) has created some friction, and privacy-conscious segments have some exit options through VPNs, data brokers, and privacy-by-design services. Suppression (0.72): Very high. Structural suppression is near-total: data collection occurs without knowledge, consent mechanisms are deliberately obscured through dark patterns in privacy policies, and brokers have no incentive to surface alternatives. Regulatory suppression is lower — enforcement against brokers exists but is slow and inconsistent. Theater ratio (0.68): Moderate-high. Privacy policies proliferate but are unreadable (average 73,000 words per year per person). Opt-out mechanisms exist but are administratively burdensome and often ineffective. GDPR/CCPA enforcement produces theater of compliance without preventing aggregation or resale. Data deletion requests disappear into opaque processes. The theater has increased from 0.42 to 0.68 as regulatory requirements have created compliance infrastructure that obscures rather than constrains extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap between trapped and arbitrage-positioned agents. The data subject sees snare: no exit, no knowledge, maximum suppression. The data broker sees rope: solving a coordination problem (matching buyers with profiles), enjoying arbitrage benefits, experiencing zero suppression. A regulated consumer in GDPR jurisdiction sees tangled_rope: formal exit options but significant friction, mixed benefits from data-driven services, moderate suppression. The privacy coalition sees scaffold: technical alternatives exist and are maturing; sunset is real if adoption accelerates. The institutional observer sees piton: privacy mechanisms are performative rituals that don't actually constrain the core extraction. The analytical observer sees snare: information asymmetry is fundamental — knowing you are profiled does not restore autonomy. The perspectival gap reveals that the same constraint is snare/rope/tangled_rope/scaffold/piton/snare depending on the observer's structural position and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position. Data subjects are victims with trapped exits: their d approaches 1.0, maximizing experienced extraction chi. Data brokers are beneficiaries with arbitrage options: their d is near 0.0, creating negative chi (extraction runs toward them, not away). Regulated consumers have mobile exits (opt-out, VPN, privacy-conscious alternatives) and are both beneficiaries (data-driven services) and victims (privacy loss): their d is moderate (~0.50-0.65), classifying them as tangled_rope. Privacy regulation coalition has organized power and constrained (not trapped) exits: their d is moderate (~0.55), with exit pathways visible through technical alternatives. The piton perspective reflects institutional inertia: privacy theater persists not because it constrains extraction but because regulatory requirements mandate it; the theater itself becomes the mechanism of false legitimation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids conflating coordination with extraction. The snare classification (which dominates from the data subject's perspective) does not depend on arguing that all data aggregation is inherently extractive. Rather, it depends on the structural facts: (1) subjects cannot exit, (2) suppression is extreme (no meaningful knowledge or consent), (3) value asymmetry is large (brokers extract 100+ times the individual subject's share), (4) coordination benefits are minimal for subjects and not distributed. If data brokerage were a genuine coordination mechanism, we would expect to see beneficiaries paying for the coordination benefit, exit options being real, and suppression being low. Instead, we observe the opposite. The tangled_rope perspective (regulated consumers with exit options) is legitimate in GDPR/CCPA jurisdictions where subjects have formal rights, but even there, friction prevents many from exercising them. The rope perspective (data brokers) is accurate from their structural position but does not dominate globally — it is the view from the extracted-to position, not from the extracted-from position. The scaffold perspective is real but aspirational: privacy-by-design and data cooperatives are possible futures, not current structures. The piton perspective reveals that privacy theater (policies, opt-outs, transparency statements) performs regulatory legitimacy while the core extraction mechanism continues unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_validity_threshold,
    'At what level of friction and information asymmetry does ''consent'' cease to be meaningful in data collection agreements?',
    'Behavioral studies comparing actual vs stated understanding of data practices; correlation between consent rates and policy readability metrics; analysis of regulatory enforcement outcomes',
    'If threshold is high (current ~95% click-through rates): consent framework is largely fiction. If threshold is lower (e.g., active opt-in with 20%+ read-through): consent becomes operational gatekeeper. Changes classification from snare to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_validity_threshold, empirical, 'Threshold for meaningful consent in data brokerage').

omega_variable(
    shadow_brokerage_scale,
    'How much of the data brokerage value flows through shadow markets (unregulated brokers, illicit data sales, internal company flows) vs regulated brokers?',
    'Forensic tracing of data flows; purchase pattern analysis from regulatory filings; undercover acquisition testing; dark web market monitoring',
    'If shadow markets > 30% of value: regulation impacts only a portion of extraction. Snare persistence despite regulation. If < 10%: regulation concentrates brokerage and may enable monitoring. Scaffold timeline shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shadow_brokerage_scale, empirical, 'Proportion of data brokerage through shadow markets').

omega_variable(
    individual_data_cooperative_viability,
    'Can decentralized data cooperatives (individuals pooling data as collective bargaining units) achieve sufficient scale to reduce extraction relative to centralized brokers?',
    'Case studies of existing cooperatives (Swapcard, Solid, DATUM); adoption curves; comparative revenue analysis; exit rates relative to brokerage firms',
    'If viable: scaffold sunset is real and timeline is 10-15 years. If unviable at scale: centralized brokerage is structurally necessary and snare persists. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_data_cooperative_viability, empirical, 'Technical and economic viability of data cooperatives').

omega_variable(
    extraction_asymmetry_measurability,
    'Can the actual value extracted from individual data subjects be measured and compared to payments received by brokers?',
    'Regression analysis of data value: model correlations between data attributes and broker margins; consumer surplus calculation for data-driven services; audit of data valuation methodologies',
    'If ratio > 100:1 (brokers capture 100x value of individual subject): snare classification confirmed. If < 10:1: possible rope or tangled_rope reframing. Affects directionality and chi computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_asymmetry_measurability, empirical, 'Measurability of value extraction asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commercial_data_brokerage, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cdb_tr_t0, commercial_data_brokerage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cdb_tr_t10, commercial_data_brokerage, theater_ratio, 10, 0.58).
narrative_ontology:measurement(cdb_tr_t20, commercial_data_brokerage, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(cdb_be_t0, commercial_data_brokerage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cdb_be_t10, commercial_data_brokerage, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cdb_be_t20, commercial_data_brokerage, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commercial_data_brokerage, information_standard).
narrative_ontology:affects_constraint(commercial_data_brokerage, digital_advertising_surveillance).
narrative_ontology:affects_constraint(commercial_data_brokerage, financial_scoring_systems).
narrative_ontology:affects_constraint(commercial_data_brokerage, employment_data_extraction).

% DUAL FORMULATION NOTE:
% Commercial data brokerage is downstream of data collection and aggregation technology but represents a distinct structural constraint: the systemic monetization of personal information. Upstream constraints (tracking technology, platform data collection) enable brokerage but have their own extractiveness values. Downstream constraints (credit scoring, insurance pricing, targeted hiring) consume brokered data and have their own extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commercial_data_brokerage, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
