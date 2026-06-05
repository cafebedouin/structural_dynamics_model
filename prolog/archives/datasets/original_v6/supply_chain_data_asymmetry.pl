% ============================================================================
% CONSTRAINT STORY: supply_chain_data_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supply_chain_data_asymmetry, []).

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
 *   constraint_id: supply_chain_data_asymmetry
 *   human_readable: Supply Chain Data Asymmetry
 *   domain: economic/logistics/information_asymmetry
 *
 * SUMMARY:
 *   Supply chain data asymmetry represents a structural tension between the
 *   legitimate need for coordination (lead firms require visibility to manage
 *   complex networks) and extractive information control (suppliers surrender
 *   operational autonomy and strategic information without reciprocal
 *   visibility). The constraint exhibits all major types from different
 *   structural positions. From the perspective of trapped tier suppliers,
 *   data asymmetry is pure extraction (Snare): they are forced to disclose
 *   production capacity, costs, and logistics while receiving minimal
 *   visibility into demand forecasts or pricing decisions. From the lead
 *   firm's perspective, asymmetry is coordination (Rope): real-time supplier
 *   visibility enables just-in-time production and risk mitigation. From the
 *   perspective of final consumers, the constraint is mixed (Tangled Rope):
 *   asymmetry enables supply chain coordination that lowers costs and
 *   improves availability, but prevents discovery of quality defects and
 *   hidden externalities. The constraint's extractiveness has increased from
 *   0.35 to 0.58 over the ten-year interval as platform operators have
 *   deepened data collection and enforcement mechanisms while alternative
 *   transparency standards remain nascent. Theater ratio has increased
 *   correspondingly, reflecting that compliance with data-sharing contractual
 *   obligations often performs transparency without enabling genuine
 *   visibility.
 *
 * KEY AGENTS:
 *   - Tier Suppliers: Primary victim (powerless/trapped) — forced to disclose operational data without reciprocal visibility; no realistic exit option
 *   - Lead Firms: Primary beneficiary (institutional/arbitrage) — capture coordination benefits; can arbitrage to alternative suppliers if data access threatened
 *   - Platform Operators: Secondary beneficiary (institutional/arbitrage) — aggregate data across networks; enable matching and risk assessment
 *   - Final Consumers: Secondary victim (moderate/constrained) — benefit from coordination-enabled supply chain efficiency but cannot exit or access hidden defect/externality information
 *   - Regulatory Coalition: Mixed actor (organized/constrained) — attempt to mandate transparency for sustainability compliance but enforcement often asymmetric; constrained by supplier resistance
 *   - Transparency Standard Coalition: Organized agents (organized/constrained) — create alternative pathways (open standards, consortia) with mutual transparency; building exit option for suppliers
 *   - Legacy EDI Operators: Institutional incumbents (institutional/arbitrage) — maintain degraded data exchange standards through switching costs and backward compatibility requirements
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional power asymmetry as inherent supply chain necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supply_chain_data_asymmetry, 0.58).
domain_priors:suppression_score(supply_chain_data_asymmetry, 0.65).
domain_priors:theater_ratio(supply_chain_data_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supply_chain_data_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(supply_chain_data_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supply_chain_data_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supply_chain_data_asymmetry, tangled_rope).
narrative_ontology:human_readable(supply_chain_data_asymmetry, "Supply Chain Data Asymmetry").
narrative_ontology:topic_domain(supply_chain_data_asymmetry, "economic/logistics/information_asymmetry").

domain_priors:requires_active_enforcement(supply_chain_data_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supply_chain_data_asymmetry, lead_firms).
narrative_ontology:constraint_beneficiary(supply_chain_data_asymmetry, platform_operators).
narrative_ontology:constraint_victim(supply_chain_data_asymmetry, tier_suppliers).
narrative_ontology:constraint_victim(supply_chain_data_asymmetry, final_consumers).
narrative_ontology:constraint_victim(supply_chain_data_asymmetry, supply_chain_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TIER SUPPLIER (SNARE) — Trapped in asymmetric data relationship with lead firms. Forced to provide detailed production data, inventory, pricing, and logistics information while receiving minimal visibility into demand forecasts, pricing decisions, or contract terms. No exit option: supplier cannot refuse without losing contract. Suppression is severe — information barriers are contractually enforced through NDAs and platform restrictions. Maximum extraction experienced.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FINAL CONSUMER (TANGLED ROPE) — Benefits from supply chain coordination that lowers costs and improves product availability. Simultaneously victimized by hidden defects, counterfeit products, and environmental externalities that asymmetric data prevents from surfacing. Exit is costly (switching brands/suppliers) but possible. Experiences both coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEAD FIRM (ROPE) — Experiences data asymmetry as pure coordination mechanism. Real-time supplier visibility enables just-in-time production, demand-responsive procurement, and risk mitigation. Information flows upstream (supplier → lead firm) enabling efficiency. Lead firm can arbitrage to alternative suppliers if data access is threatened. Net beneficiary.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences asymmetry as coordination infrastructure. Aggregating supplier data enables matching, visibility, and risk assessment across networks. Arbitrage option: can shift to alternative data-collection business models. Suppression is low — information disclosure rules are contractual rather than coercive.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (TANGLED ROPE) — Sustainability regulations (EU due diligence directives, SEC climate disclosure) mandate supply chain visibility while remaining incomplete. Regulators benefit from data aggregation for enforcement; suppliers are constrained by compliance costs and incomplete guidance. Active enforcement through audits and penalties. Some coordination function (driving transparency) alongside extraction (compliance burden falls primarily on suppliers).
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSPARENCY STANDARD COALITION (SCAFFOLD) — Industry consortia, open standards (GS1, OpenDEP), and blockchain initiatives create alternative data-sharing pathways with mutual transparency rather than asymmetric extraction. Theater is lower (real data flow vs performative compliance). Sunset logic: as common standards mature and regulatory enforcement strengthens, lock-in to proprietary platform asymmetry becomes less tenable. Estimated sunset: 5-10 years as interoperable standards capture market share.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY EDI SYSTEM (PITON) — Electronic Data Interchange standards (ANSI X.12, EDIFACT) persist despite higher-cost alternatives (APIs, real-time data clouds). Theater ratio is high — systems maintain data exchange ritual through incumbent infrastructure and switching costs rather than functional superiority. Vendors maintain backward compatibility to preserve customer lock-in. Degraded institutional form persisting through inertia.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information asymmetry is inherent to complex supply chains: not all data can be perfectly transparent without destroying strategic advantage and competitive viability. This perspective risks naturalizing what is actually a contingent institutional arrangement — the specific scope and enforcement mechanisms of data asymmetry are policy choices, not natural limits.
constraint_indexing:constraint_classification(supply_chain_data_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supply_chain_data_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supply_chain_data_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supply_chain_data_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supply_chain_data_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supply_chain_data_asymmetry, TR),
    TR >= 0.70.

:- end_tests(supply_chain_data_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from suppliers through information control, enabling lead firms to negotiate unfavorable terms and access pricing/capacity data that shifts bargaining power. However, extraction is not maximal because: (1) some suppliers can negotiate better terms via countervailing power or diversification; (2) regulatory pressure is increasing disclosure requirements; (3) open standards are creating alternative pathways. The trajectory from 0.35 to 0.58 reflects deepening platform data collection and enforcement, but the rate of increase is slowing as regulatory and competitive alternatives emerge. Suppression (0.65): High. Contractual confidentiality clauses, platform lock-in, switching costs, and market concentration create formidable barriers to supplier exit or information symmetry. Suppliers cannot disclose their own data, cannot access lead firm data, and cannot easily switch to competing platforms. Theater ratio (0.68): High. Compliance with data-sharing obligations often creates appearance of transparency without genuine visibility — suppliers provide data that is immediately proprietary-locked, compressed into aggregates, or used for benchmarking without feedback loop. Real-time APIs create higher theater than actual supplier empowerment. The theater has increased as platforms have sophisticated data collection/usage tracking without corresponding transparency to suppliers.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence across eight distinct structural positions. The tier supplier and lead firm experience opposite data flows and opposite extraction direction — supplier sees Snare, lead firm sees Rope. The consumer sees Tangled Rope because supply chain coordination benefits them while hidden externalities extract value without consent. The platform operator sees Rope because data aggregation is a genuine coordination function from their perspective. The regulatory coalition sees Tangled Rope because regulations mandate transparency (coordination function) while enforcement burden falls asymmetrically on suppliers (extraction). The transparency coalition sees Scaffold because open standards represent a sunset mechanism — once mutual transparency standards mature, proprietary asymmetry becomes less competitive. The legacy EDI system sees its own function as degraded (Piton) — persists through inertia and switching costs despite technological obsolescence. The civilizational analytical observer risks seeing Mountain (information asymmetry is inherent to supply chain complexity) but structural data reveals this as naturalization of contingent power arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship of each agent to the asymmetric data flow. Tier suppliers occupy the position of full targets (d ≈ 0.89–0.95): trapped, providing information without reciprocal access, no exit option. Lead firms occupy the beneficiary position (d ≈ 0.10–0.20): arbitrage-enabled exit, capture coordination benefits, face minimal information disclosure burden. Consumers occupy an intermediate position (d ≈ 0.65–0.75): constrained exit (switching brands is costly but possible), experience both benefits (supply chain efficiency) and costs (hidden externalities). Platform operators operate from an institutional beneficiary perspective (d ≈ 0.15–0.25): arbitrage-enabled, capture value from data aggregation. The regulatory coalition occupies a contested middle (d ≈ 0.50–0.60): ostensibly neutral but enforcement asymmetry means suppliers bear higher compliance cost, making them closer to victims. The transparency coalition occupies a constrained organized position (d ≈ 0.40–0.55): pushing for symmetry but facing entrenched incumbent power. The analytical observer's natural law reading derives maximum d from the universal/civilizational context (d ≈ 0.72), treating information asymmetry as inherent constraint — but the structural data reveals this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The constraint resolves the coordination-vs-extraction mandate through perspectival multiplicity. The beneficiary (lead firm) experiences genuine coordination — asymmetric data flow solves real problems (just-in-time production, risk mitigation). The victim (tier supplier) experiences genuine extraction — information asymmetry shifts bargaining power and prevents exit. Both classifications are structurally correct from their respective positions. The constraint is NOT a disguised snare (false rope) because lead firms genuinely solve a coordination problem. It is NOT a disguised rope (false snare) because suppliers genuinely experience extraction without exit. The resolution: the constraint IS a tangled rope, with the coordination and extraction functions distributed asymmetrically across agents. The analytical observer's mountain risk — that some information asymmetry is 'inherent to supply chains' — is a false summit detection case: the specific enforcement mechanisms (confidentiality clauses, platform lock-in, data aggregation depth, regulatory asymmetry) are policy-contingent, not natural. The constraint could be restructured toward mutual transparency (open standards model) or toward explicit negotiated asymmetry (negotiated data-sharing contracts) without violating the coordination function. The existence of viable Scaffold alternatives proves the mountain framing is incorrect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetry_threshold_definition,
    'What level of data asymmetry is legitimate coordination overhead vs extractive withholding?',
    'Empirical comparison: correlation between supplier data withholding and supplier profitability across industries; comparison of industries with symmetric vs asymmetric data flows on cost pass-through and innovation rates',
    'If threshold is low: most current arrangements classify as snare. If threshold is high: arrangements shift toward rope/tangled rope. Classification of entire supply chains pivots on this parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_threshold_definition, empirical, 'Threshold distinguishing coordination overhead from extractive asymmetry').

omega_variable(
    countervailing_power_sufficiency,
    'Do large suppliers have sufficient countervailing power to negotiate symmetric data arrangements, or is asymmetry enforced through market concentration?',
    'Empirical analysis: proportion of suppliers by tier and firm size who have negotiated data-symmetric contracts; correlation between supplier size/diversification and data access rights; market concentration indices for lead firms',
    'If countervailing power is high: suppliers have real constrained (not trapped) exit; classification shifts toward tangled rope. If power is concentrated: suppliers are truly trapped; snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(countervailing_power_sufficiency, empirical, 'Supplier countervailing power and negotiation asymmetry').

omega_variable(
    externality_capture_mechanism,
    'Does the data asymmetry actively hide environmental or labor externalities, or does it merely fail to reveal them?',
    'Historical analysis: cases where lead firms withheld data and later-discovered externalities emerged; comparison of audit findings in opaque vs transparent supply chains; mechanism analysis of whether asymmetry is maintained specifically to prevent externality discovery',
    'If asymmetry actively hides externalities: extraction component is higher; snare classification strengthened. If passive opacity: classification may shift toward rope (coordination without intentional suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_mechanism, empirical, 'Whether data asymmetry actively conceals or merely reveals externalities').

omega_variable(
    standard_interoperability_timeline,
    'At what adoption rate do open standards (GS1, OpenDEP, blockchain supply chain) achieve sufficient critical mass to break proprietary platform lock-in?',
    'Adoption curve analysis for open standards; tracking of supplier defection from proprietary platforms to interoperable alternatives; cost-benefit analysis of migration as standard maturity increases',
    'If timeline is < 5 years: scaffold sunset is credible. If > 15 years: scaffold is aspirational; constraint may persist as tangled rope or snare long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_interoperability_timeline, empirical, 'Adoption timeline for open supply chain data standards').

omega_variable(
    regulatory_enforcement_asymmetry,
    'Are sustainability regulations enforced symmetrically (equal compliance burden) or asymmetrically (enforcement focuses on suppliers, not lead firms)?',
    'Audit of SEC/EU enforcement actions: proportion of enforcement against lead firms vs suppliers; cost allocation analysis in compliance regulations; interview data on perceived burden by firm tier',
    'If asymmetric enforcement: regulatory perspective shifts from tangled rope toward snare. Regulatory coalition becomes extractive rather than coordinating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_asymmetry, empirical, 'Asymmetry in regulatory compliance burden by firm tier').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supply_chain_data_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scda_tr_t0, supply_chain_data_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(scda_tr_t5, supply_chain_data_asymmetry, theater_ratio, 5, 0.58).
narrative_ontology:measurement(scda_tr_t10, supply_chain_data_asymmetry, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(scda_be_t0, supply_chain_data_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scda_be_t5, supply_chain_data_asymmetry, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(scda_be_t10, supply_chain_data_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supply_chain_data_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(supply_chain_data_asymmetry, supply_chain_pricing_power).
narrative_ontology:affects_constraint(supply_chain_data_asymmetry, supplier_financial_distress).
narrative_ontology:affects_constraint(supply_chain_data_asymmetry, product_recall_coordination).

% DUAL FORMULATION NOTE:
% Supply chain data asymmetry is upstream of multiple derived constraints. Pricing power asymmetry (lead firms extract through information-enabled price negotiation) is a secondary extraction mechanism enabled by data asymmetry. Supplier financial distress (inability to access demand forecasts forces higher safety stock and working capital requirements) flows from information barriers. Product recall coordination failure (inability of lead firms to access full supplier defect data delays recalls) is a functional consequence. The upstream constraint (this story) affects all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supply_chain_data_asymmetry, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
