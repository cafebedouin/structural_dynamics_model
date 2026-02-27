% ============================================================================
% CONSTRAINT STORY: cn_tech_decoupling_security_software
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cn_tech_decoupling_security_software, []).

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
 *   constraint_id: cn_tech_decoupling_security_software
 *   human_readable: Mandate for Chinese SOEs to replace US security software
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   China's mandate for state-owned enterprises to replace US-origin network
 *   security software with domestic alternatives represents a constraint that
 *   combines coordination objectives (reducing critical infrastructure
 *   dependency on foreign vendors) with significant extraction mechanisms
 *   (vendor lock-in, operational disruption costs, supply chain
 *   fragmentation). The constraint exhibits conflicting classifications
 *   across perspectives: beneficiaries (state apparatus, domestic vendors)
 *   experience coordination benefits; victims (SOEs, US vendors, global
 *   supply chain efficiency) experience pure extraction or forced transition
 *   costs; organized international coalitions experience both coordinated
 *   standard-setting and tit-for-tat rent-seeking. The theater ratio (0.55)
 *   reflects that security justifications coexist with economic protectionism
 *   — the security improvements are real but modest relative to the
 *   extraction costs, suggesting theater is significant but not dominant. The
 *   extractiveness trajectory (0.35→0.62 over six years) reveals how mandate
 *   implementation accumulates both switching costs and vendor consolidation
 *   rents, pushing the constraint toward higher-extraction classification
 *   over time.
 *
 * KEY AGENTS:
 *   - Chinese State Regulatory Apparatus: Primary beneficiary (institutional/arbitrage) — achieves critical infrastructure control, vendor consolidation, technology transfer leverage
 *   - Chinese SOEs: Primary victim (powerless/trapped) — mandatory compliance, sunk license costs, transition disruption, performance gaps during maturation period
 *   - Domestic Chinese Security Vendors: Secondary beneficiary (moderate/constrained) — guaranteed market capture, state purchase mandates, but locked into state-directed development pace
 *   - US Software Vendors: Secondary victim (moderate/constrained) — forced exit from largest Asian customer base, revenue loss, cannot appeal through commercial channels
 *   - Global Cybersecurity Supply Chain: Tertiary victim (analytical/analytical) — fragmentation reduces interoperability, increases duplicate R&D, slows vulnerability coordination
 *   - International Regulatory Coalition: Organized responder (organized/mobile) — governments develop reciprocal mandates, standards, export controls; experiences both coordination and tit-for-tat extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cn_tech_decoupling_security_software, 0.58).
domain_priors:suppression_score(cn_tech_decoupling_security_software, 0.72).
domain_priors:theater_ratio(cn_tech_decoupling_security_software, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, extractiveness, 0.58).
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cn_tech_decoupling_security_software, tangled_rope).
narrative_ontology:human_readable(cn_tech_decoupling_security_software, "Mandate for Chinese SOEs to replace US security software").
narrative_ontology:topic_domain(cn_tech_decoupling_security_software, "technological/geopolitical").

domain_priors:requires_active_enforcement(cn_tech_decoupling_security_software).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, chinese_domestic_security_vendors).
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, state_regulatory_apparatus).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, chinese_soe_operational_efficiency).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, us_software_vendors).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, global_cybersecurity_supply_chain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPLIANT SOE (SNARE) — Chinese SOEs cannot refuse mandate compliance without risking state penalties, funding withdrawal, or administrative sanctions. Trapped exit: replacement is mandatory, non-compliance is not tolerated. Bears full cost of transition: software license sunk costs, retraining expenses, operational disruption during migration, and persistent performance gaps if domestic alternatives are immature. No exit mechanism; maximum experienced extraction.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC SECURITY VENDOR (TANGLED ROPE) — Benefits from guaranteed market capture and state purchase mandates. Also constrained by pressure to match foreign software maturity within compressed timelines, quality expectations from SOE deployments, and reliance on state specification compliance. Experiences both coordination (solving domestic supply dependency) and extraction (forced to develop at state-directed pace with guaranteed but limited margins). Constrained exit: vendor is locked into domestic market; international expansion requires CCP approval.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY APPARATUS (ROPE) — Primary beneficiary. Mandate achieves coordination objective (reduce foreign software dependency, control critical infrastructure security stack) while capturing significant extraction rent: vendor consolidation under state auspices, technology transfer obligations, surveillance capability integration into domestic software. Experiences constraint as coordination mechanism with net benefit flow toward the state. Arbitrage exit: state can modulate mandate stringency, timeline, exceptions.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US SOFTWARE VENDOR (SNARE) — Excluded from Chinese SOE market through mandate. Loses installed base, revenue streams, and future upgrade contracts. Constrained exit: cannot appeal mandate through commercial channels; Chinese market access requires state permission. Cannot shift to alternative markets without losing the largest single customer base in Asia. Extraction via forced exit from market; suppression via regulatory prohibition on alternatives.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL CYBERSECURITY SUPPLY CHAIN (PITON) — Mandate fragments what was a unified global supply chain into regional silos (Chinese domestic, US-allied, others). The fragmentation is theatrically justified as security but operationally creates supply chain vulnerabilities through reduced interoperability, knowledge sharing bottlenecks, and reduced incentives for coordinated vulnerability disclosure. Theater ratio high: the security rationale (defense against foreign espionage backdoors) coexists with extraction logic (market capture, vendor lock-in). The performative aspect: replacing foreign software does not intrinsically improve security unless domestic alternatives are genuinely superior — often they are not initially. Piton because the fragmentation itself becomes institutionally persistent even as technical superiority arguments weaken.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL REGULATORY COALITION (TANGLED ROPE) — Governments and vendors in US-allied ecosystems experience the mandate as both coordination challenge and extraction threat. Coordinated response (export controls, reciprocal mandates, standards-setting) generates cooperation benefits but also locks in tit-for-tat extraction: US mandates domestic alternatives, EU mandates vendor diversity, others follow. Mobile exit exists for large vendors and states (develop parallel stacks, invest in alternatives), but full decoupling is expensive. Mixed coordination (solving fragmentation through standards) and extraction (each player capturing regional rents).
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cn_tech_decoupling_security_software_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cn_tech_decoupling_security_software, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cn_tech_decoupling_security_software, TR),
    TR >= 0.70.

:- end_tests(cn_tech_decoupling_security_software_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting significant forced transition costs for SOEs, vendor lock-in mechanisms, and supply chain fragmentation, but not maximal because legitimate security coordination benefits exist. Initial extractiveness (0.35) reflects primarily coordination framing; trajectory to 0.62 by year 6 captures the accumulation of switching costs and vendor consolidation as implementation deepens. Suppression (0.72): High. SOEs cannot refuse compliance without state sanctions (regulatory, financial). Domestic vendors face compressed timelines and state-specified feature requirements (not voluntary specifications). US vendors cannot appeal mandate or access market. Global supply chain has no negotiating power over fragmentation. Suppression reflects coercive enforcement and lack of alternatives. Theater ratio (0.55): Moderate. The security rationale (defense against backdoors in foreign software) is partially genuine — foreign vendors do pose espionage risks. But the ratio of security improvement to economic cost is unclear, and the extraction mechanism (vendor consolidation, market capture) is substantial. Theater increases slightly over time as mandate becomes routine rather than justified by crisis.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows strong perspectival fragmentation. The state regulator sees Rope (coordination + manageable benefit flow). Chinese domestic vendors see Tangled Rope (guaranteed market but constrained autonomy). SOEs see Snare (no exit, full cost). US vendors see Snare (forced exit). The global supply chain sees Piton (fragmentation justified by security but increasingly performative). The international regulatory coalition sees Tangled Rope (coordination needs but tit-for-tat extraction). No single type captures the full structure — the constraint is genuinely hybrid, with coordination benefits (reduced foreign dependency) coexisting with significant extraction (vendor lock-in, SOE costs, supply chain fragmentation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) map structural position to extraction experience. State apparatus: d ≈ 0.05 (full beneficiary), derives low/negative chi. SOEs: d ≈ 0.95 (trapped victims), derives high chi. Domestic vendors: d ≈ 0.55 (constrained beneficiaries with modest extraction overhead), derives moderate chi. US vendors: d ≈ 0.85 (constrained targets), derives high chi. Global supply chain: d ≈ 0.70 (analytical observer bearing costs), derives high chi. International coalition: d ≈ 0.60 (organized responders with exit options), derives moderate-high chi. The engine derives d from beneficiary/victim declarations (state is beneficiary, SOEs and US vendors are victims) and exit options (state has arbitrage, SOEs trapped, international coalition mobile). The Tangled Rope classification emerges from the synthesis: active enforcement required, beneficiaries exist, victims exist, but coordination function (reducing foreign dependency) is genuine alongside extraction (vendor lock-in).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate resolves mandatrophy by admitting genuine coordination function alongside extraction. The temptation is to classify as pure Snare (state extracts from SOEs and US vendors). But the beneficiary analysis reveals: the state genuinely solves a coordination problem (critical infrastructure vendor diversification), not merely rent-seeking. Domestic vendors are not passive recipients of state favor — they have real technical development obligations. The international regulatory response is not pure retaliation but coordination around fragmented supply chain realities. The Tangled Rope classification prevents false naturalization of extraction as inevitable law-of-supply-chains, while also preventing false idealization of the mandate as pure coordination. Theater ratio (0.55) reflects the hybrid: security framing is real but imperfect. The constraint is authentically mixed — coordination benefits are genuine but modest; extraction is real but not maximal; suppression is high but not total (domestic vendors have some autonomy within state direction). The mandatrophy is resolved by accepting the hybrid structure rather than forcing it into a pure category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_software_maturity_gap,
    'What is the actual technical maturity gap between Chinese domestic security software and US equivalents at mandate start, and how quickly does it close?',
    'Independent security audits, vulnerability disclosure rates, penetration test results comparing Chinese domestic alternatives to US baselines over 3-5 year windows',
    'If gap is large and slow-closing: mandate imposes genuine operational cost (extractive). If gap is small or closing rapidly: mandate is primarily a coordination mechanism with real security benefits (rope-ish). If gap is artificial (marketing): reveals pure extraction dressed as security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_software_maturity_gap, empirical, 'Technical maturity gap between Chinese domestic and US security software').

omega_variable(
    espionage_baseline_risk,
    'What is the empirical risk of backdoored foreign security software serving as espionage vector, versus domestic software?',
    'Analysis of documented supply chain attacks, vendor relationship with foreign intelligence, capabilities of Chinese domestic vendors to resist state pressure versus US vendors, historical data on backdoor prevalence',
    'If foreign risk is substantially higher: mandate is legitimate coordination (Rope). If risks are symmetric: mandate is pure protection theater (Piton/Snare). If domestic risk is higher: mandate is counter-productive (false summit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(espionage_baseline_risk, empirical, 'Comparative espionage risk: foreign vs domestic software').

omega_variable(
    vendor_lock_in_persistence,
    'Does the domestic vendor market consolidate into durable lock-in, or does competition remain sufficient to keep vendors disciplined?',
    'Market structure analysis: number of competitors, pricing trends, switching costs for SOEs, government procurement flexibility over 10-year horizon',
    'If lock-in is durable: extraction mechanism persists (high-extraction Snare or Tangled Rope). If competition survives: coordination function dominates (Rope-like). If lock-in breaks down and SOEs revert to foreign software: mandate fails structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_lock_in_persistence, empirical, 'Durability of vendor lock-in in Chinese domestic market').

omega_variable(
    supply_chain_fragmentation_cost,
    'What is the total economic cost of global cybersecurity supply chain fragmentation relative to unified supply chain efficiency?',
    'Input-output analysis of security software costs, integration expenses, vulnerability disclosure delays, duplicate R&D investment across regional stacks',
    'If fragmentation cost is moderate: constraint is primarily distributional (Tangled Rope). If fragmentation cost is very high: reveals constraint as net-negative for all parties including beneficiaries (false classification), suggesting mandate persistence is purely political theater (Piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_fragmentation_cost, empirical, 'Economic cost of global cybersecurity supply chain fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cn_tech_decoupling_security_software, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cntds_tr_t0, cn_tech_decoupling_security_software, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cntds_tr_t3, cn_tech_decoupling_security_software, theater_ratio, 3, 0.52).
narrative_ontology:measurement(cntds_tr_t6, cn_tech_decoupling_security_software, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(cntds_be_t0, cn_tech_decoupling_security_software, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cntds_be_t3, cn_tech_decoupling_security_software, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(cntds_be_t6, cn_tech_decoupling_security_software, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cn_tech_decoupling_security_software, enforcement_mechanism).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, semiconductor_supply_chain_resilience).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, us_china_tech_export_controls).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, domestic_cloud_infrastructure_mandate).

% DUAL FORMULATION NOTE:
% This constraint is part of a broader technology decoupling cluster. Upstream constraints (export controls, semiconductor supply chain) establish the security/geopolitical rationale. This constraint represents the operational implementation phase. Downstream constraints (domestic cloud, domestic payment systems) extend the same logic to other critical infrastructure layers. Each constraint in the family has distinct ε values and beneficiary/victim structures but shares the coordination-vs-extraction tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
