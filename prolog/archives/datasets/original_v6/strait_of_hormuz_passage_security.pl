% ============================================================================
% CONSTRAINT STORY: strait_of_hormuz_passage_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strait_of_hormuz_passage_security, []).

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
 *   constraint_id: strait_of_hormuz_passage_security
 *   human_readable: Strait of Hormuz Passage Security
 *   domain: geopolitical/maritime_security
 *
 * SUMMARY:
 *   The Strait of Hormuz represents approximately 21 miles of geographic
 *   necessity through which 35% of the world's seaborne traded oil passes.
 *   This chokepoint creates a structural constraint where passage security is
 *   managed by a regional hegemonic power in coordination with international
 *   shipping and energy infrastructure, but asymmetrically extracted from
 *   fuel-dependent developing economies with no alternative access. The
 *   constraint exhibits a classic tangled rope structure: genuine
 *   coordination function (shipping and energy markets require predictable
 *   passage, naval presence provides anti-piracy and traffic management),
 *   active enforcement mechanism (naval deployments, surveillance, selective
 *   blockade capacity), asymmetric extraction (transit leverage, intelligence
 *   collection, forced geopolitical alignment), and both beneficiaries and
 *   victims with minimal exit options. The theater_ratio (0.58) reflects the
 *   performative elements: naval presence functions partly as deterrent (real
 *   coordination value) and partly as visible assertion of control
 *   (extractive display). Alternative infrastructure (LNG terminals, pipeline
 *   networks, shipping diversification) is under development with 20-30 year
 *   maturation horizons, creating a scaffold perspective where the constraint
 *   appears temporary. The analytical observer risks naturalizing what is
 *   geopolitically contingent as geographically inevitable — the Strait's
 *   narrow width is indeed a fact, but it is political choice (naval
 *   deployment, blockade capacity, fee extraction) that transforms geography
 *   into constraint.
 *
 * KEY AGENTS:
 *   - Incumbent Regional Security Hegemon: Primary beneficiary (powerful/mobile) — captures transit intelligence, strategic leverage, maintenance fees; extracts geopolitical alignment through passage control
 *   - Fuel-Dependent Developing Economies: Primary victim (powerless/trapped) — cannot avoid passage; no military capacity for alternative security; bears full cost of supply disruption and price volatility
 *   - Smaller Regional States: Secondary victim (moderate/constrained) — face military inferiority; economically integrated; exposed to blockade threat and forced alignment
 *   - International Shipping Consortia: Beneficiary (institutional/arbitrage) — access to coordinated passage security, insurance pools, predictable routing; can shift to alternative routes if extraction excessive
 *   - Oil Trading Infrastructure: Beneficiary (institutional/arbitrage) — benefits from price stability and transaction volume enabled by passage security; global arbitrage capacity reduces exposure
 *   - International Maritime Regulation System: Institutional observer (institutional/constrained) — UNCLOS and maritime law provide nominal governance; actual enforcement bypassed; maintains theater through institutional inertia
 *   - Emerging Alternative Infrastructure Coalition: Organized victim-beneficiary (organized/constrained) — investing in LNG networks, pipelines, shipping routes; sees constraint as temporary with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing geopolitical arrangements as geographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strait_of_hormuz_passage_security, 0.58).
domain_priors:suppression_score(strait_of_hormuz_passage_security, 0.62).
domain_priors:theater_ratio(strait_of_hormuz_passage_security, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strait_of_hormuz_passage_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(strait_of_hormuz_passage_security, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(strait_of_hormuz_passage_security, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strait_of_hormuz_passage_security, tangled_rope).
narrative_ontology:human_readable(strait_of_hormuz_passage_security, "Strait of Hormuz Passage Security").
narrative_ontology:topic_domain(strait_of_hormuz_passage_security, "geopolitical/maritime_security").

domain_priors:requires_active_enforcement(strait_of_hormuz_passage_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strait_of_hormuz_passage_security, regional_security_hegemon).
narrative_ontology:constraint_beneficiary(strait_of_hormuz_passage_security, international_shipping_consortia).
narrative_ontology:constraint_beneficiary(strait_of_hormuz_passage_security, oil_trading_infrastructure).
narrative_ontology:constraint_victim(strait_of_hormuz_passage_security, smaller_regional_states).
narrative_ontology:constraint_victim(strait_of_hormuz_passage_security, non_aligned_shipping_operators).
narrative_ontology:constraint_victim(strait_of_hormuz_passage_security, developing_economies_fuel_dependent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUEL-DEPENDENT DEVELOPING ECONOMIES (SNARE) — Cannot avoid Hormuz passage; no alternative energy sources; no capacity for military response. Bears full cost of transit disruption, supply shock, price volatility. Trapped by geographic necessity and economic dependence. Maximum experienced extraction.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALLER REGIONAL STATES (TANGLED ROPE) — Constrained by military inferiority and economic integration. Benefits from shipping access and regional stability coordination but exposed to extraction through blockade threat, tolls, and forced alignment. Asymmetric coordination with high coercion cost.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL SHIPPING CONSORTIA (ROPE) — Benefits from predictable passage security; insurance costs and routing fees are coordination overhead, not extraction. Can arbitrage to alternative routes or insurance pools if extraction becomes excessive. Net beneficiary experiencing constraint as coordination mechanism.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OIL TRADING INFRASTRUCTURE (ROPE) — Benefits from price stability and transaction volume enabled by passage security. Can arbitrage to spot markets or alternative suppliers if constraints tighten. Experiences constraint as coordination mechanism for global energy markets.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENT REGIONAL SECURITY HEGEMON (TANGLED ROPE) — Primary beneficiary extracting transit leverage, naval maintenance fees, intelligence value. But also benefits from coordination of regional shipping — too-severe extraction would collapse the system. Mobile exit option (can shift enforcement mechanisms) but chooses to maintain current structure for extraction. Coordination + asymmetric extraction.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL MARITIME REGULATION (PITON) — UNCLOS and international maritime law provide nominal governance, but enforcement is theatrical — actual passage security is determined by regional hegemon, not by international convention. Regulatory system persists through institutional inertia despite being bypassed. Theater ratio high; functional governance low.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EMERGING ALTERNATIVE INFRASTRUCTURE COALITION (SCAFFOLD) — New LNG terminal networks, pipeline diversification, shipping route development are building alternatives to Hormuz dependence. These infrastructure investments have sunset logic: as alternatives mature, Hormuz passage extraction mechanisms lose force. Organized actors (energy exporters, shipping lines) investing in parallel systems see the constraint as temporary with 20-30 year sunset horizon.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / GEOGRAPHIC INEVITABILITY (MOUNTAIN) — From civilizational scale, the Strait's 21-mile width and 35% of seaborne oil transit constitute a natural geographic chokepoint immune to political will. This perspective sees passage security constraints as emerging naturally from topology. However, the structural data reveals this as false summit: actual constraints derive from political choice (naval deployment, blockade capacity), not from geography alone. Geography creates vulnerability; politics creates extraction.
constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strait_of_hormuz_passage_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strait_of_hormuz_passage_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strait_of_hormuz_passage_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strait_of_hormuz_passage_security, TR),
    TR >= 0.70.

:- end_tests(strait_of_hormuz_passage_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regional hegemon captures measurable benefits — transit intelligence, strategic leverage, maintenance fees for naval operations, geopolitical alignment through passage control. However, extraction is constrained by the requirement to maintain passage functionality; too-severe extraction would trigger alternative infrastructure investment and organized coalition response. The value reflects asymmetric but not total extraction — coordination benefits still flow to all parties, but concentration benefits flow primarily to the hegemon. Suppression (0.62): Moderate-high. Fuel-dependent economies face geographic necessity (cannot avoid the Strait), military inferiority (cannot challenge naval control), and economic integration (cannot redirect trade flows quickly). Smaller regional states face military asymmetry and economic vulnerability. However, suppression is not total — alternative infrastructure development, coalition coordination, and technological disruption pathways exist with sufficient credibility that actors are investing in them. Theater ratio (0.58): Moderate. Naval presence functions partly as genuine coordination (anti-piracy, traffic management, deterrence of regional conflict escalation) and partly as visible assertion of hegemonic control (naval exercises, passage denial demonstrations, intelligence collection). The performative component has increased as alternative infrastructure development has reduced the functional necessity of dense naval presence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the critical relationship between beneficiary status and classification type. The hegemon (beneficiary/mobile) classifies as Tangled Rope — they genuinely coordinate shipping security (rope component) while extracting strategic leverage (snare component). Developing economies (victim/trapped) classify as Snare — they experience coordination benefits (ability to trade at all) but the asymmetric extraction dominates their experiential reality. Shipping consortia (beneficiary/arbitrage) classify as Rope — they experience coordination mechanism without meaningful extraction because they have exit options. The regional state (victim/constrained) classifies as Tangled Rope — genuinely coordinated regional security with embedded extraction. The maritime regulation system (institutional/constrained, bypassed) classifies as Piton — nominally governing but functionally degraded, maintained through institutional inertia. The alternative infrastructure coalition (organized/constrained, building exits) classifies as Scaffold — sees the constraint as temporary with sunset logic. The analytical observer (civilizational perspective) risks classifying as Mountain — naturalizing geopolitical choice as geographic necessity — but the structural data contradicts this: geography creates vulnerability, politics creates constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation follows from beneficiary/victim declarations and exit options. The fuel-dependent developing economy (victim/trapped) derives d ≈ 0.95, experiencing maximum effective extraction f(d) ≈ 1.42. The smaller regional state (victim/constrained) derives d ≈ 0.75, experiencing high extraction f(d) ≈ 1.15. The shipping consortium (beneficiary/arbitrage) derives d ≈ 0.15, experiencing low or negative extraction f(d) ≈ -0.01 — they see the constraint as providing value through coordination. The regional hegemon (beneficiary/mobile) derives d ≈ 0.35, but with mobile exit capacity, they could shift to alternative enforcement mechanisms, creating a special case: they benefit from the current structure but have agency to change it, making them structurally powerful even though they bear costs of maintaining passage. Scope modifier σ(global) = 1.2 amplifies these values, reflecting that Hormuz passage security affects global energy markets, making verification and alternative response more difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing geographic necessity from geopolitical extraction. The Strait's 21-mile width and 35% of oil transit are facts. The constraint structure — passage security managed asymmetrically by a hegemonic power extracting strategic leverage — is choice. The mandatrophy is resolved by noting that every perspective (except the false-summit mountain view) recognizes both the coordination function (passage requires security) and the extraction mechanism (security is provided asymmetrically). The tangled rope classification is correct across the most informed perspectives because the constraint genuinely coordinates and genuinely extracts. Attempts to reduce it to pure coordination (rope) or pure extraction (snare) would misclassify the constraint — it is genuinely hybrid. The emerging scaffold perspective (alternative infrastructure maturation) will eventually reduce extraction capacity, but only if alternative infrastructure succeeds. The piton classification (regulatory system) reflects that international law has become decorative — it coordinates nominal coverage while actual coordination is performed by hegemonic naval deployment. This is not a misuse of piton; it is precisely the constraint type for institutions maintained by inertia despite functional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_versus_coordination_boundary,
    'What level of transit fees, intelligence collection, or military positioning constitutes coordination overhead versus extractive rent-seeking?',
    'Comparative analysis across eras: pre-hegemon period vs current; comparison to alternative chokepoint passage costs (Suez, Panama); economic modeling of coordination costs vs observed fees',
    'If observed costs > 1.3x alternative chokepoints: classified as extraction-dominant (Snare/Tangled Rope from more positions). If costs align with alternatives: coordination-dominant (Rope from more positions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_coordination_boundary, empirical, 'Boundary between coordination overhead and extractive rent').

omega_variable(
    alternative_infrastructure_credibility,
    'Will LNG terminal networks, pipeline diversification, and shipping route alternatives materialize at scale before geopolitical crisis forces coercive closure?',
    'Infrastructure investment tracking; timeline correlation with actual capacity additions; historical precedent from other chokepoint diversification efforts',
    'If alternatives mature: scaffold perspective confirmed, sunset logic real, constraint degrades from Snare/Tangled Rope to Rope over 20-30 years. If alternatives stall: scaffold is aspirational, constraint persists unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_credibility, empirical, 'Whether alternative energy/shipping infrastructure will reduce Hormuz dependence').

omega_variable(
    hegemonic_enforcement_threshold,
    'What level of blocking or fee escalation would trigger organized coalition response and/or alternative route investment that undermines hegemon''s extraction capacity?',
    'Historical case analysis of hegemonic overreach (Suez 1956, OPEC embargoes); game-theoretic modeling of coalition formation thresholds; observation of actual response to recent Hormuz incidents',
    'If threshold is high: hegemon has wide extraction range without destabilizing (Rope from hegemon perspective). If threshold is low: small escalation triggers coalition response (Snare becomes unstable, transition to Scaffold visible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegemonic_enforcement_threshold, empirical, 'Threshold for coalition response to hegemonic extraction').

omega_variable(
    technological_disruption_timeline,
    'Will autonomous shipping, undersea pipeline infrastructure, or satellite-based routing reduce physical vulnerability to surface-level naval control within the next 15 years?',
    'Technology development tracking; deployment of autonomous convoy systems; investment in undersea transport; satellite navigation independence milestones',
    'If yes: constraint mechanism itself becomes obsolete (Piton escalation or architectural shift). If no: current power dynamics persist, extraction mechanisms remain high-confidence for indefinite horizon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_disruption_timeline, empirical, 'Whether autonomous/alternative transport tech will bypass surface-level naval control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strait_of_hormuz_passage_security, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hormuz_tr_t0, strait_of_hormuz_passage_security, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hormuz_tr_t5, strait_of_hormuz_passage_security, theater_ratio, 5, 0.52).
narrative_ontology:measurement(hormuz_tr_t10, strait_of_hormuz_passage_security, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hormuz_be_t0, strait_of_hormuz_passage_security, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hormuz_be_t5, strait_of_hormuz_passage_security, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(hormuz_be_t10, strait_of_hormuz_passage_security, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strait_of_hormuz_passage_security, enforcement_mechanism).
narrative_ontology:affects_constraint(strait_of_hormuz_passage_security, opec_production_extraction_asymmetry).
narrative_ontology:affects_constraint(strait_of_hormuz_passage_security, lng_infrastructure_development).
narrative_ontology:affects_constraint(strait_of_hormuz_passage_security, naval_hegemony_southeast_asia).

% DUAL FORMULATION NOTE:
% Hormuz passage security is downstream of regional geopolitical power asymmetry but represents a structurally distinct constraint. The upstream constraint (regional hegemonic capacity) determines enforcement capability; Hormuz passage security has its own extraction and suppression dynamics reflecting the specific chokepoint geography and energy dependency structure. Alternative infrastructure development represents a parallel constraint family exploring escape routes from Hormuz dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strait_of_hormuz_passage_security, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
