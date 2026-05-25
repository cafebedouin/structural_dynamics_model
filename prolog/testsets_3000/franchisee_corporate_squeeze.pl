% ============================================================================
% CONSTRAINT STORY: franchisee_corporate_squeeze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_franchisee_corporate_squeeze, []).

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
 *   constraint_id: franchisee_corporate_squeeze
 *   human_readable: Franchise Agreement Squeeze
 *   domain: economic/franchise_systems
 *
 * SUMMARY:
 *   Franchise agreements represent a hybrid structure: corporations seeking
 *   rapid expansion without capital risk, and entrepreneurs seeking branded
 *   business models with lower startup risk than independent ventures.
 *   However, once franchisees are locked into agreements with sunk costs and
 *   limited exit options, franchisors gain asymmetric power to extract wealth
 *   through royalty increases, mandatory product purchases, marketing fee
 *   inflation, technology fee imposition, and unfavorable renewal terms. The
 *   constraint exhibits characteristics of both coordination (brand
 *   standardization, operational support, economies of scale) and pure
 *   extraction (fee escalation, supply chain markups, unilateral contract
 *   modification). From the franchisee's perspective, especially individual
 *   owner-operators with trapped exit options, the agreement functions as a
 *   snare. From the franchisor's perspective, the system is coordination
 *   infrastructure. The analytical observer risks naturalizing the asymmetry
 *   as inherent to capital ownership, when in fact it reflects specific
 *   contractual design choices that could be reformed through regulation.
 *
 * KEY AGENTS:
 *   - Corporate Franchisor: Primary beneficiary (institutional/arbitrage) — controls fee structures, supply chains, brand standards; multiple revenue streams with low cost of capital; can exit or modify terms
 *   - Individual Franchisees: Primary victims (powerless/trapped) — sunk costs in property, buildout, and working capital; long-term agreements with high exit penalties; subject to unilateral fee increases
 *   - Franchisee Network Organizations: Secondary actors (moderate/constrained) — franchise associations and litigation groups provide limited collective leverage; can pressure but not exit
 *   - Regulatory Agencies: Reform coalition (organized/constrained) — state franchise boards and consumer protection agencies pushing for fee transparency and renewal protections; building sunset mechanisms through legal reform
 *   - Legacy Franchise Disclosure System: Institutional theater (institutional/arbitrage) — required FDD compliance creates appearance of franchisee protection but limited real bite
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent contractual asymmetries as immutable laws of capitalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(franchisee_corporate_squeeze, 0.58).
domain_priors:suppression_score(franchisee_corporate_squeeze, 0.65).
domain_priors:theater_ratio(franchisee_corporate_squeeze, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, extractiveness, 0.58).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(franchisee_corporate_squeeze, tangled_rope).
narrative_ontology:human_readable(franchisee_corporate_squeeze, "Franchise Agreement Squeeze").
narrative_ontology:topic_domain(franchisee_corporate_squeeze, "economic/franchise_systems").

domain_priors:requires_active_enforcement(franchisee_corporate_squeeze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(franchisee_corporate_squeeze, corporate_franchisor).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, individual_franchisees).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, franchisee_profitability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL FRANCHISEE (SNARE) — Trapped in long-term franchise agreement with high exit costs (non-refundable initial fee, property lease obligations, buildout sunk costs). Franchisor controls pricing, supply chain, marketing fees, technology fees, and renewal terms. Franchisee bears operational risk but franchisor extracts through multiple revenue streams (royalties, marketing fees, product markups, technology fees). Limited alternatives and high switching costs create a snare structure.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRANCHISEE NETWORK (TANGLED ROPE) — Moderate power when franchisees organize (franchise associations, litigation coordination). Benefits from brand standardization, shared training, economies of scale in supply purchasing. Also bears extraction through collective fee negotiations often tilted toward franchisor. Coalition-building provides constrained exit (ability to pressure franchisor through collective action) but not mobility. Mix of genuine coordination benefits and asymmetric extraction.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORPORATE FRANCHISOR (ROPE) — Experiences the franchise system as coordination: standardized operations, quality control, brand consistency across locations. Benefits from network effects (more franchisees = stronger brand = more franchise applications). Extracts through multiple fee streams but also invests in brand development, training infrastructure, and marketing that benefits all franchisees. Experiences the constraint primarily as a coordination mechanism. High exit optionality (can increase fees, change terms, open company locations).
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — State legislatures, franchisee advocacy groups, and consumer protection agencies pushing for franchise disclosure laws, fee transparency requirements, and non-renewal protections. Sees the squeeze as a temporary problem with policy sunset: better regulation of franchise agreements can reduce extractive pressure while preserving coordination benefits. Extraction is constrained by emerging legal frameworks limiting franchisor unilateral fee increases and requiring good-faith renewal negotiations.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY FRANCHISE THEATER (PITON) — The franchise disclosure document (FDD) and compliance framework are largely performative. Required to reveal fee structures and historical franchisee earnings, but: (a) historical earnings data is voluntarily provided and often selective, (b) actual franchisee profitability varies wildly due to operational skill and market conditions, (c) new franchisees ignore warnings in FDD if brand reputation is strong. Theater ratio (0.48) reflects moderate performativity — disclosure exists but has limited real bite. Maintained through regulatory requirement, not because it effectively protects franchisees.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the asymmetry in franchise agreements appears as an immutable natural law: capital owners always hold structural advantage over small-business operators; information asymmetry between franchisor (100+ franchisees of data) and individual franchisee (single location data) is inherent to the business model; and exit costs are inherent to asset-specific investment (property, buildout). However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the asymmetries are contingent on legal and contractual design choices, not natural laws.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(franchisee_corporate_squeeze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(franchisee_corporate_squeeze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(franchisee_corporate_squeeze, TR),
    TR >= 0.70.

:- end_tests(franchisee_corporate_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting multiple extraction mechanisms (royalties typically 5-8%, marketing fees 2-5%, mandatory product markups 15-30%, technology fees 1-3%) and escalating fees over contract term. However, not as severe as pure predatory lending (0.75+) because franchisor reinvests some revenues in brand development and franchisees do retain operational autonomy. The 10-year trajectory shows 0.38→0.58, indicating extraction has worsened as franchisors optimize fee structures and consolidate power. Suppression (0.65): High. Exit barriers include non-refundable initial fees (typically $25K-$100K+), property lease obligations (often 10+ years), training sunk costs, and brand-specific buildout (equipment, signage). Career risk of failure (franchisees report 20-30% failure rates in competitive sectors). Limited transparency on actual franchisee earnings. Information asymmetry heavily favors franchisor. Theater ratio (0.48): Moderate, declining over interval. FDD disclosure creates appearance of protection, but effectiveness is limited because (a) historical earnings are optional and selective, (b) new franchisees often ignore warnings if brand is strong, (c) franchisor controls most earnings variables. Theater has declined slightly as regulatory enforcement and franchisee litigation have increased real consequences, reducing pure performativity.
 *
 * PERSPECTIVAL GAP:
 *   The franchisor sees a working coordination system (Rope): brand standardization, training infrastructure, quality control, and network effects justify the fee structure. The franchisee sees extraction under the guise of partnership (Snare/Tangled Rope): fees escalate, alternatives are eliminated, and franchisor controls pricing power. The franchisee network sees mixed benefits and extraction costs (Tangled Rope): collective bargaining can extract some concessions, but information asymmetry and individual competition prevent full closure of the gap. The regulatory coalition sees a temporary market failure (Scaffold) with a sunset: fee transparency, renewal protections, and good-faith negotiation requirements can rebalance the relationship while preserving legitimate coordination benefits. The legacy FDD system sees itself as working (Piton): disclosure requirement is maintained through regulatory habit despite limited real enforcement, creating theater of franchisee protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation tracks how structural position determines experienced extractiveness. Individual franchisees: trapped exit + victim status → d ≈ 0.90 → f(d) ≈ 1.40 → χ high. Franchisee network: constrained exit + organized power → d ≈ 0.55 → f(d) ≈ 0.75 → χ moderate. Franchisor: arbitrage exit + beneficiary status → d ≈ 0.05 → f(d) ≈ -0.12 → χ negative (extraction flows toward franchisor). The pipeline automatically derives these from the declarations of beneficiaries (franchisor), victims (franchisees), power levels, and exit options. No overrides needed — structural data clearly maps to directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three tangled rope gates: (1) Genuine coordination function exists — brand standardization, training, operational support, economies of scale are real and franchisees do benefit. (2) Asymmetric extraction is documented — multiple fee streams, supply chain markups, unilateral term modifications, and unfavorable renewal negotiations create directional extraction from franchisee to franchisor. (3) Active enforcement is required — franchisor must actively monitor, audit, and enforce compliance with brand standards, supply contracts, and fee obligations. The mandatrophy (confusion between coordination and extraction) is resolved by recognizing that BOTH properties are structural. This is not 'is it coordination or extraction?' but 'it is coordination WITH extraction.' The regulatory coalition's scaffold perspective shows that policy reform can separate legitimate coordination (reduced extraction levels) from rent-seeking (franchisor fee excess). The snare perspective (from trapped franchisees) reflects that from their position, the extraction overwhelms the coordination benefits — they experience it as pure extraction even though coordination infrastructure exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchisee_profitability_data_reliability,
    'What fraction of franchisee profit variations are due to franchisor extraction mechanisms versus operator skill, market conditions, and local competition?',
    'Longitudinal data on franchisee earnings across same brand in different markets; cohort analysis comparing franchisees entering during high vs low franchisor fee regimes; exit analysis of why franchisees leave',
    'If extraction accounts for >40% of variance: snare classification dominates. If <20%: rope classification strengthens. If mixture is large (25-35%): tangled rope is correct classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchisee_profitability_data_reliability, empirical, 'Whether franchisee profit variance reflects extraction or operator skill').

omega_variable(
    franchisor_reinvestment_rates,
    'What fraction of franchisor revenues from royalties and fees are reinvested in brand development, franchisee support, and training versus captured as corporate profit?',
    'Franchisor financial disclosure analysis; franchisee surveys on quality and timeliness of support; benchmarking against other franchise systems',
    'If reinvestment >50%: coordination narrative strengthens (rope interpretation valid). If <30%: extraction narrative strengthens (snare interpretation valid). Impact on claimed_type classification if findings shift significantly from tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(franchisor_reinvestment_rates, empirical, 'Whether franchisor fees fund coordination services or pure extraction').

omega_variable(
    regulatory_enforcement_effectiveness,
    'Do state franchise laws (FDD requirements, non-renewal protections, good-faith negotiation duties) actually reduce extraction rates or merely create compliance theater?',
    'Comparison of franchisee profitability and fee structures in heavily regulated states (California, New York) versus lightly regulated states; analysis of regulatory enforcement actions and penalties',
    'If effective: scaffold sunset is real, extraction rates declining over time. If theater: regulatory framework is piton-like, theater_ratio should be higher. Affects measurement trajectory and interval sunset assumptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_effectiveness, empirical, 'Whether franchise regulation reduces extraction or creates compliance theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(franchisee_corporate_squeeze, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcs_tr_t0, franchisee_corporate_squeeze, theater_ratio, 0, 0.52).
narrative_ontology:measurement(fcs_tr_t5, franchisee_corporate_squeeze, theater_ratio, 5, 0.5).
narrative_ontology:measurement(fcs_tr_t10, franchisee_corporate_squeeze, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fcs_be_t0, franchisee_corporate_squeeze, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fcs_be_t5, franchisee_corporate_squeeze, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fcs_be_t10, franchisee_corporate_squeeze, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(franchisee_corporate_squeeze, resource_allocation).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, small_business_capital_access).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, supply_chain_monopoly_power).

% DUAL FORMULATION NOTE:
% The franchise squeeze is downstream of capital allocation structures in small business financing (why entrepreneurs need franchises) and upstream of supply chain concentration (franchisor-controlled suppliers capture additional rents). The three constraints form a causal chain: capital access → franchise dependence → supply chain vulnerability. Each has distinct extractiveness values reflecting their specific structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(franchisee_corporate_squeeze, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
