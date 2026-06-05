% ============================================================================
% CONSTRAINT STORY: epcis_interoperability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epcis_interoperability, []).

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
 *   constraint_id: epcis_interoperability
 *   human_readable: EPCIS Interoperability Standard Lock-In
 *   domain: supply_chain/standards/technology
 *
 * SUMMARY:
 *   EPCIS (Electronic Product Code Information Services) standardization
 *   creates a structural tension between genuine multi-party supply chain
 *   coordination needs and extractive lock-in by the standard-setting
 *   authority and early adopters. The constraint exhibits characteristics of
 *   all six classification types depending on observational position. For
 *   dependent suppliers, EPCIS is an inescapable snare that mandates
 *   compliance with no exit option. For mid-market implementers, it is
 *   tangled rope — enabling supply chain visibility while extracting
 *   switching costs and compliance overhead. For the standards body and first
 *   movers, it is pure coordination rope — solving the genuine problem of
 *   heterogeneous supply chain partners needing shared language. For
 *   competing standards bodies, EPCIS creates extraction through incumbency
 *   lock-in and first-mover advantage. The traditional EDI infrastructure
 *   persists as degraded piton — maintained through institutional inertia
 *   despite EPCIS standardization. Emerging blockchain and decentralized
 *   identity frameworks represent scaffold structures with sunset logic:
 *   alternative protocols could displace EPCIS within 10-15 years if network
 *   effects reverse. The extractiveness metric (0.52) reflects that EPCIS
 *   lock-in has deepened over the past decade as enterprise adoption
 *   increased adoption barriers for switching. The theater ratio (0.55)
 *   indicates moderate performative compliance — many enterprises maintain
 *   parallel EPCIS and legacy EDI stacks rather than full migration, creating
 *   the appearance of standardization without true interoperability.
 *
 * KEY AGENTS:
 *   - EPC Global Standards Body: Primary beneficiary (institutional/arbitrage) — controls standard evolution, captures value from vendor ecosystem built on EPCIS
 *   - Early Adopter Enterprises (Walmart, Amazon, Coca-Cola): Co-beneficiaries (institutional/arbitrage) — designed standard to suit their infrastructure, first-mover advantage in supply chain optimization
 *   - Dependent Supply Chain Participants (small suppliers, 3PLs): Primary victims (powerless/trapped) — mandatory EPCIS compliance with no exit option; absorb compliance costs
 *   - Mid-Market Implementers: Secondary victims (moderate/constrained) — face significant switching costs and retooling expenses; some coordination benefits but asymmetric extraction
 *   - Competing Standards Bodies (ISO, GS1, industry-specific consortia): Secondary victims (organized/constrained) — market share erosion due to EPCIS incumbent advantage; constrained ability to promote alternatives
 *   - Legacy EDI Infrastructure Operators: Institutional actors maintaining dual systems (institutional/arbitrage) — persist through inertia; extract value from enterprises maintaining parallel stacks
 *   - Blockchain/DLT Innovation Communities: Emerging organized agents (organized/mobile) — building alternative interoperability pathways with escape velocity; positioned to erode EPCIS lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epcis_interoperability, 0.52).
domain_priors:suppression_score(epcis_interoperability, 0.48).
domain_priors:theater_ratio(epcis_interoperability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epcis_interoperability, extractiveness, 0.52).
narrative_ontology:constraint_metric(epcis_interoperability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(epcis_interoperability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epcis_interoperability, tangled_rope).
narrative_ontology:human_readable(epcis_interoperability, "EPCIS Interoperability Standard Lock-In").
narrative_ontology:topic_domain(epcis_interoperability, "supply_chain/standards/technology").

domain_priors:requires_active_enforcement(epcis_interoperability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epcis_interoperability, epc_global_standards_body).
narrative_ontology:constraint_beneficiary(epcis_interoperability, early_adopter_enterprises).
narrative_ontology:constraint_victim(epcis_interoperability, supply_chain_participants).
narrative_ontology:constraint_victim(epcis_interoperability, competing_standard_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SUPPLY CHAIN PARTICIPANT (SNARE) — Small suppliers and logistics operators face mandatory EPCIS compliance to participate in global supply chains. Exit is structurally impossible: non-compliance means exclusion from major retailers' networks. Maximum extraction with minimal coordination benefit for this agent.
constraint_indexing:constraint_classification(epcis_interoperability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET IMPLEMENTER (TANGLED ROPE) — Medium-sized enterprises benefit from supply chain visibility and coordination with partners but face significant compliance costs and switching barriers. Exit is possible at high cost (retooling systems, losing market access). Hybrid: both genuine coordination gains and asymmetric extraction.
constraint_indexing:constraint_classification(epcis_interoperability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STANDARDS BODY AND EARLY ADOPTERS (ROPE) — EPC Global and early-adopting enterprises (Walmart, Amazon, Coca-Cola) designed the standard to solve genuine multi-party coordination: shared visibility across supply chains requires common language. These agents experience the constraint as enabling their coordination goals. Net beneficiaries with full exit options (can modify standard or exit to proprietary systems).
constraint_indexing:constraint_classification(epcis_interoperability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING STANDARDS COALITION (TANGLED ROPE) — ISO, industry-specific standards bodies, and proprietary alternative protocols (GS1, EDIFACT variants) face lock-in effects. They can theoretically build alternatives, but EPCIS network effects create switching costs. Organized agents with partial agency: can coordinate alternatives but face extraction from incumbent advantage.
constraint_indexing:constraint_classification(epcis_interoperability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY EDI INFRASTRUCTURE (PITON) — Traditional Electronic Data Interchange (EDI) and proprietary integration protocols persist despite EPCIS standardization efforts. They continue through institutional inertia: enterprises maintain dual systems rather than full migration. Theater ratio high because many firms maintain parallel EDI and EPCIS stacks simultaneously, creating performative compliance rather than true interoperability.
constraint_indexing:constraint_classification(epcis_interoperability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING BLOCKCHAIN INTEROPERABILITY LAYER (SCAFFOLD) — Distributed ledger technologies (blockchain, DLT) and decentralized identity frameworks (W3C Verifiable Credentials) are building alternative verification and coordination pathways that could reduce EPCIS centralization over 10-15 years. Organized agents see sunset logic: blockchain enables peer-to-peer provenance without EPCIS gatekeeping. Scaffold classification reflects temporary coordination role with visible exit path.
constraint_indexing:constraint_classification(epcis_interoperability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, supply chain interoperability is presented as an immutable requirement: complex global logistics cannot function without standardized tracking. This perspective naturalizes EPCIS as inevitable. However, the structural data contradicts this: multiple competing standards exist (GS1, ISO 9735, blockchain), and 'need for coordination' does not entail 'only EPCIS coordination possible.' The mountain classification is a false summit — institutionalized as natural when it is contingent.
constraint_indexing:constraint_classification(epcis_interoperability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epcis_interoperability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epcis_interoperability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epcis_interoperability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epcis_interoperability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epcis_interoperability, TR),
    TR >= 0.70.

:- end_tests(epcis_interoperability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing. Initial EPCIS adoption (0.28) was genuinely coordinating — solving heterogeneous EDI integration problems for early adopters. As adoption becomes mandatory for supply chain participation, extractiveness increased as lock-in deepened and switching costs accumulated. Current value reflects that EPCIS controls now extract through incumbent advantage, not just coordination function. The trend line upward (0.28 → 0.40 → 0.52 over 15 years) shows rent-seeking accumulation: early coordination function is preserved while extraction layered on top. Suppression (0.48): Moderate. Network effects create high switching costs (technical retraining, system replacement, supply chain partner coordination). But suppression is not total — viable alternatives exist (GS1, ISO standards, blockchain), and coordinated industry migration is theoretically possible at high cost. Theater ratio (0.55): Moderate, increasing. Increasing proportion of EPCIS activity is performative: dual EDI/EPCIS stacks, compliance theater without deep interoperability, standardization that masks underlying heterogeneity. Theater rise suggests constraint is degrading from genuine coordination toward piton-like institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal because EPCIS creates genuine coordination benefits while simultaneously extracting through lock-in. The standards body genuinely solved the problem of heterogeneous EDI integration — this is not naturalized extraction but real coordination function. However, that coordination function is now 'covered' by extraction: switching to GS1 or blockchain would solve the same coordination problem, but network effects and incumbent advantage make switching prohibitively expensive. The analytical observer's false summit is the claim that 'supply chains need standards' entails 'all supply chains must use EPCIS.' The first is coordination (Rope), the second is incumbency (Snare for non-beneficiaries). The perspectival gap reveals this distinction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) combines their base structural relationship (d) with the constraint's measured properties (ε, f(d), scope). Standards body beneficiaries with arbitrage exit: d ≈ 0.15, f(d) ≈ -0.01, scope=global (σ=1.2) → χ ≈ -0.006 (negative, enabling). Dependent suppliers with trapped exit: d ≈ 0.92, f(d) ≈ 1.40, scope=global → χ ≈ 0.72 (high extraction). Mid-market with constrained exit: d ≈ 0.62, f(d) ≈ 0.92, scope=national (σ=1.0) → χ ≈ 0.48 (moderate extraction). The tuples correctly differentiate structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that 'coordination function' and 'extractive lock-in' are not mutually exclusive — the same mechanism can coordinate for some agents (early adopters, standards body) while extracting from others (dependent suppliers, competing standards). The classical mandatrophy asks: 'Is this coordination (Rope) or extraction (Snare)?' The answer is Tangled Rope — genuine coordination with asymmetric extraction. The constraint earns Tangled Rope classification because (1) it solves a real multi-party coordination problem (supply chain visibility), (2) it requires active enforcement (standards compliance, network effects), (3) it produces asymmetric extraction (lock-in favors early adopters), and (4) it has identifiable beneficiaries and victims. The false summit risk is naturalizing EPCIS as the only possible solution — the 'need for coordination' is real, but 'EPCIS dominance' is contingent on network effects, not necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_reversibility,
    'Are EPCIS network effects irreversible, or could alternative standards achieve critical mass if first-mover advantage were overcome?',
    'Historical analysis of standard transitions (EDI→XML, IPv4→IPv6); cost-benefit analysis of migration to alternative standards with equivalent functionality',
    'If reversible: extractiveness should be lower (constrained exit), classification shifts toward Rope. If irreversible: current high extractiveness justified, lock-in is real structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_reversibility, empirical, 'Whether EPCIS network effects are reversible or path-dependent').

omega_variable(
    genuine_coordination_vs_incumbency,
    'Does EPCIS solve supply chain coordination problems better than available alternatives, or does its dominance derive primarily from incumbency and network lock-in?',
    'Comparative performance analysis: EPCIS latency/cost vs GS1/ISO alternatives; counterfactual analysis of supply chain efficiency if alternative standards had achieved critical mass',
    'If genuine technical superiority: Tangled Rope classification correct (coordination function + extraction). If incumbency-driven: reclassify toward Snare (extraction with minimal coordination benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_coordination_vs_incumbency, empirical, 'Whether EPCIS dominance reflects technical superiority or incumbency advantage').

omega_variable(
    interoperability_bridge_feasibility,
    'Can transparent interoperability bridges (cross-standard translators, blockchain-based proxies) reduce EPCIS lock-in without requiring wholesale migration?',
    'Technical evaluation of bridge protocol feasibility; case studies of successful multi-standard environments (financial settlement systems, healthcare data standards)',
    'If feasible: scaffold sunset becomes realistic (10-15 year path to multi-standard interoperability). If infeasible: lock-in deepens, extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_bridge_feasibility, empirical, 'Whether cross-standard interoperability bridges can reduce lock-in').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.48) primarily structural (technical switching costs, network effects) or behavioral (internalized legitimacy of ''industry standards'')?',
    'Post-standard-choice analysis: survey enterprises that migrated away from EPCIS to measure whether suppression persists; longitudinal tracking of ''standards education'' as normalization mechanism',
    'If structural: suppression persists even if alternatives become viable. If behavioral: suppression declines rapidly once legitimate alternatives achieve scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs behavioral mechanisms of suppression in EPCIS lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epcis_interoperability, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcis_tr_t0, epcis_interoperability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(epcis_tr_t5, epcis_interoperability, theater_ratio, 5, 0.45).
narrative_ontology:measurement(epcis_tr_t10, epcis_interoperability, theater_ratio, 10, 0.55).
narrative_ontology:measurement(epcis_tr_t15, epcis_interoperability, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(epcis_be_t0, epcis_interoperability, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(epcis_be_t5, epcis_interoperability, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(epcis_be_t10, epcis_interoperability, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(epcis_be_t15, epcis_interoperability, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epcis_interoperability, resource_allocation).
narrative_ontology:boltzmann_floor_override(epcis_interoperability, 0.18).
narrative_ontology:affects_constraint(epcis_interoperability, supply_chain_visibility_coupling).
narrative_ontology:affects_constraint(epcis_interoperability, iot_device_interoperability).
narrative_ontology:affects_constraint(epcis_interoperability, blockchain_supply_chain_proofs).

% DUAL FORMULATION NOTE:
% EPCIS interoperability is downstream of the genuine supply chain coordination problem (which has ε≈0.15, pure Rope) but represents the specific institutional arrangement that solves it. The upstream constraint is 'supply chain visibility coordination' (ε=0.15, Rope); EPCIS is the incumbent solution that adds extraction through lock-in. Decomposition: supply_chain_visibility_coordination (upstream, low ε) → epcis_interoperability (incumbent solution, moderate ε) → blockchain_supply_chain_proofs (emerging alternative, moderate ε). All three are linked because competing solutions address the same coordination need.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epcis_interoperability, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
